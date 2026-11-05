# ---------------------------------------------------------------------------
# questionnaire.R -- read a Survey Solutions Designer questionnaire
# (document.json, a .zip containing it, an unpacked folder, or a PDF printout)
# into a normalised object of class "suso_questionnaire".
# ---------------------------------------------------------------------------

.qtype_by_class <- c(
  SingleQuestion = "single", MultyOptionsQuestion = "multi", MultiOptionsQuestion = "multi",
  NumericQuestion = "numeric", TextQuestion = "text", DateTimeQuestion = "date",
  GpsCoordinateQuestion = "gps", TextListQuestion = "list", QRBarcodeQuestion = "barcode",
  MultimediaQuestion = "picture", AudioQuestion = "audio", AreaQuestion = "area")
.qtype_by_int <- c("0" = "single", "3" = "multi", "4" = "numeric", "5" = "date", "6" = "gps",
                   "7" = "text", "9" = "list", "10" = "barcode", "11" = "picture", "12" = "area",
                   "13" = "audio")

#' Read a Survey Solutions questionnaire
#'
#' @param path Path to a Designer `document.json`, a `.zip` containing it (Designer
#'   package or HQ export archive), a folder, or a questionnaire PDF (experimental).
#' @param pdf_engine `"pdftools"` (default) or `"markitdown"` (needs the
#'   `markitdown` CLI on PATH) for PDF input.
#' @param verbose Print progress.
#' @return Object of class `suso_questionnaire`. When the input is an HQ export
#'   (`<Q>_<version>_<FORMAT>_All.zip` or its folder), the questionnaire version
#'   is detected from `export__info.json` or the archive name (`$hq_version`).
#' @export
suso_read_questionnaire <- function(path, pdf_engine = c("pdftools", "markitdown"), verbose = TRUE) {
  check_dependencies()
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  cat_dir <- NULL
  if (dir.exists(path)) {
    json <- find_document_json(path); cat_dir <- path
  } else {
    ext <- tolower(tools::file_ext(path))
    if (ext == "zip") {
      td <- tempfile("suso_q_"); dir.create(td)
      utils::unzip(path, exdir = td)
      json <- find_document_json(td); cat_dir <- td
    } else if (ext == "json") {
      json <- path; cat_dir <- dirname(path)
    } else if (ext == "pdf") {
      return(suso_read_pdf(path, engine = match.arg(pdf_engine), verbose = verbose))
    } else stop("Unsupported input type: .", ext, call. = FALSE)
  }
  sm_msg(verbose, "Reading questionnaire: ", basename(json))
  txt <- read_text_utf8(json)
  doc <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                  error = function(e) stop("Could not parse ", basename(json), ": ", conditionMessage(e), call. = FALSE))
  qn <- qn_from_document(doc, cat_dir = cat_dir, verbose = verbose,
                         raw_json = txt)
  qn$hq_version <- detect_hq_version(path, cat_dir)
  if (!is.na(qn$hq_version)) sm_msg(verbose, "  questionnaire version: ", qn$hq_version)
  qn
}

# HQ questionnaire version: export__info.json ("<id>$<version>"), else the
# archive/folder name "<Q>_<version>_<FORMAT>_All[.zip]".
detect_hq_version <- function(path, dir = NULL) {
  info <- if (!is.null(dir)) list.files(dir, "^export__info\\.json$", recursive = TRUE, full.names = TRUE) else character()
  if (length(info)) {
    id <- tryCatch(jsonlite::fromJSON(info[1])$QuestionnaireId, error = function(e) NULL)
    v <- sub("^.*\\$", "", id %||% "")
    if (grepl("^[0-9]+$", v)) return(as.integer(v))
  }
  m <- regmatches(basename(path), regexec("_([0-9]+)_[A-Za-z]+_All(\\.zip)?$", basename(path)))[[1]]
  if (length(m)) as.integer(m[2]) else NA_integer_
}

find_document_json <- function(dir) {
  f <- list.files(dir, pattern = "\\.json$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  f <- f[!grepl("export__info", basename(f))]
  if (!length(f)) {
    # HQ export archives embed the questionnaire as Questionnaire/content.zip
    inner <- list.files(dir, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    for (z in inner) utils::unzip(z, exdir = file.path(dirname(z), sub("\\.zip$", "", basename(z))))
    f <- list.files(dir, pattern = "\\.json$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    f <- f[!grepl("export__info", basename(f))]
  }
  if (!length(f)) stop("No .json questionnaire found in ", dir, call. = FALSE)
  pref <- f[tolower(basename(f)) == "document.json"]
  if (length(pref)) return(pref[1])
  f[which.max(file.size(f))]
}

parse_macros <- function(m) {
  if (!length(m)) return(character())
  items <- Filter(is.list, if (!is.null(names(m)) && all(nzchar(names(m)))) unname(m) else m)   # HQ 26.x adds a "$type" entry
  nm <- vapply(items, function(x) x$Name %||% "", ""); ct <- vapply(items, function(x) x$Content %||% "", "")
  stats::setNames(ct[nzchar(nm)], nm[nzchar(nm)])
}

# Lookup tables: "Lookup Tables/<id>.txt" (tab-separated, first column rowcode),
# used in expressions as table[rowcode].column
load_lookup_tables <- function(lt, dir, warn) {
  out <- list(); raw <- list()
  lt <- Filter(is.list, lt %||% list())
  if (!length(lt)) return(out)
  ids <- names(lt) %||% vapply(lt, function(x) x$Id %||% "", "")
  files <- if (is.null(dir)) character() else list.files(dir, recursive = TRUE, full.names = TRUE)
  for (k in seq_along(lt)) {
    nm <- lt[[k]]$TableName %||% ""; idn <- tolower(gsub("-", "", ids[k]))
    hit <- files[tolower(tools::file_path_sans_ext(basename(files))) == idn |
                 basename(files) == (lt[[k]]$FileName %||% "\\0")]
    df <- if (length(hit)) tryCatch(utils::read.delim(text = read_text_utf8(hit[1]), stringsAsFactors = FALSE,
                                                       check.names = FALSE), error = function(e) NULL)
    if (is.null(df) || !nrow(df) || tolower(names(df)[1]) != "rowcode") {
      warn("Lookup table '", nm, "' not found: expressions using it fall back to random draws"); next
    }
    names(df)[1] <- "rowcode"
    out[[nm]] <- df
    raw[[basename(hit[1])]] <- readBin(hit[1], "raw", file.size(hit[1]))   # re-exported in Questionnaire/content.zip
  }
  attr(out, "raw") <- raw
  out
}

# Reusable categories: Categories/<id>.xlsx|.tsv|.txt|.csv inside the package
load_categories <- function(cats, dir) {
  out <- list(); raw <- list()
  if (!length(cats) || is.null(dir)) return(out)
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  for (c in cats) {
    id <- c$Id %||% c$CategoriesId %||% ""
    idn <- tolower(gsub("-", "", id))
    hit <- files[grepl(idn, tolower(gsub("-", "", basename(files))), fixed = TRUE)]
    if (!length(hit)) next
    f <- hit[1]; ext <- tolower(tools::file_ext(f))
    df <- tryCatch({
      sanitize_df(if (ext %in% c("xlsx", "xls")) { check_dependencies(xlsx = TRUE); as.data.frame(readxl::read_excel(f, trim_ws = FALSE)) }
      else utils::read.delim(text = read_text_utf8(f), sep = if (ext == "csv") "," else "\t", stringsAsFactors = FALSE, encoding = "UTF-8"))
    }, error = function(e) NULL)
    if (is.null(df) || !nrow(df)) next
    raw[[basename(f)]] <- readBin(f, "raw", file.size(f))   # re-exported in Questionnaire/content.zip
    nmz <- tolower(names(df))
    pick <- function(cands) { j <- which(nmz %in% cands); if (length(j)) df[[j[1]]] else NULL }
    out[[tolower(id)]] <- data.frame(
      code = as.numeric(pick(c("id", "value", "code"))),
      label = as.character(pick(c("text", "title", "label"))),
      parent = as_num_or_na(pick(c("parentid", "parentvalue", "parent"))) %||% NA_real_,
      stringsAsFactors = FALSE)
  }
  attr(out, "raw") <- raw
  out
}

options_df <- function(answers) {
  if (!length(answers)) return(data.frame(code = numeric(0), label = character(0), parent = numeric(0)))
  data.frame(
    code = vapply(answers, function(a) as_num_or_na(a$AnswerValue %||% a$AnswerCode %||% a$Value), 0),
    label = vapply(answers, function(a) as.character(a$AnswerText %||% a$Title %||% ""), ""),
    parent = vapply(answers, function(a) as_num_or_na(a$ParentValue %||% a$ParentCode), 0),
    stringsAsFactors = FALSE)
}

scope_code <- function(x) {
  if (is.null(x)) return(0L)
  if (is.numeric(x)) return(as.integer(x))
  switch(tolower(as.character(x)), interviewer = 0L, supervisor = 1L, headquarter = , headquarters = 2L,
         hidden = 3L, 0L)
}

severity_code <- function(x) {
  if (is.null(x)) return(0L)
  if (is.numeric(x)) return(as.integer(x))
  if (tolower(x) == "warning") 1L else 0L
}

var_type_code <- function(x) {
  if (is.null(x)) return(2L)
  if (is.numeric(x)) return(as.integer(x))
  switch(tolower(x), longinteger = 1L, double = 2L, boolean = 3L, datetime = 4L, string = 5L, 2L)
}

qn_from_document <- function(doc, cat_dir = NULL, verbose = TRUE, raw_json = NULL) {
  doc <- sanitize_list(doc)                         # every string valid UTF-8, control characters dropped
  qn <- new.env()
  qn$title   <- doc$Title %||% "questionnaire"
  qn$varname <- doc$VariableName %||% gsub("[^A-Za-z0-9_]", "_", fold(qn$title))
  qn$id      <- doc$PublicKey %||% doc$Id %||% new_guid()
  qn$revision <- doc$Revision %||% NA
  qn$hq_version <- NA_integer_
  qn$document_json <- raw_json %||% as.character(jsonlite::toJSON(doc, auto_unbox = TRUE, null = "null", digits = NA))
  qn$macros  <- parse_macros(doc$Macros)
  qn$categories <- load_categories(doc$Categories, cat_dir)
  qn$category_names <- stats::setNames(lapply(doc$Categories %||% list(), function(c) c$Name %||% ""),
                                       vapply(doc$Categories %||% list(), function(c) tolower(c$Id %||% ""), ""))
  qn$category_raw <- attr(qn$categories, "raw") %||% list()
  qn$nodes   <- new.env(hash = TRUE)
  qn$by_name <- new.env(hash = TRUE)
  qn$order   <- character()
  qn$levels  <- list("main" = list(key = "", file = qn$varname, id_col = NULL, depth = 0L, parent = NA))
  qn$warnings <- character()
  warn <- function(...) qn$warnings <- c(qn$warnings, paste0(...))
  qn$lookup <- load_lookup_tables(doc$LookupTables, cat_dir, warn)
  qn$lookup_raw <- attr(qn$lookup, "raw") %||% list()

  walk <- function(items, parent, path) {
    ids <- character()
    for (it in items) {
      cls <- it$`$type` %||% ""
      id <- tolower(it$PublicKey %||% it$Id %||% new_guid())
      node <- list(id = id, parent = parent, path = path, cond_src = it$ConditionExpression %||% "")
      if (cls == "Group") {
        is_roster <- isTRUE(it$IsRoster)
        node$varname <- it$VariableName %||% ""
        node$text <- it$Title %||% ""
        if (is_roster) {
          node$kind <- "roster"
          src <- it$RosterSizeSource %||% 0
          fixed <- identical(src, 1L) || identical(src, 1) || identical(tolower(as.character(src)), "fixedtitles")
          ft <- it$FixedRosterTitles %||% list()
          node$roster <- if (fixed) {
            list(type = "fixed", qid = NULL, titles = data.frame(
              code = vapply(seq_along(ft), function(i) as_num_or_na(ft[[i]]$Value %||% (i - 1)), 0),
              label = vapply(ft, function(x) as.character(x$Title %||% ""), ""), stringsAsFactors = FALSE))
          } else list(type = "question", qid = tolower(it$RosterSizeQuestionId %||% ""))
          # level key encodes the full chain: rosters share a level (and export file)
          # only when trigger AND nesting path coincide
          lk <- paste0(if (length(path)) path[length(path)] else "", ">",
                       if (fixed) paste0("fixed:", id) else paste0("q:", node$roster$qid))
          node$path <- c(path, lk)
          if (is.null(qn$levels[[lk]])) {
            if (!nzchar(node$varname)) node$varname <- paste0("roster_", substr(id, 1, 6))
            qn$levels[[lk]] <- list(key = lk, file = node$varname, id_col = paste0(node$varname, "__id"),
                                    depth = length(node$path), parent = if (length(path)) path[length(path)] else "")
          }
        } else node$kind <- "group"
        qn$nodes[[id]] <- node; qn$order <- c(qn$order, id)
        kids <- walk(it$Children %||% list(), id, node$path)
        node$children <- kids
        qn$nodes[[id]] <- node
      } else if (cls == "Variable") {
        node$kind <- "variable"
        node$varname <- it$Name %||% it$VariableName
        node$text <- it$Label %||% node$varname
        node$label <- node$text
        node$expression <- it$Expression %||% ""
        node$vtype <- var_type_code(it$Type)
        node$do_not_export <- isTRUE(it$DoNotExport)
        qn$nodes[[id]] <- node; qn$order <- c(qn$order, id)
      } else if (cls == "StaticText") {
        node$kind <- "static"; node$varname <- ""; node$text <- it$Text %||% ""
        qn$nodes[[id]] <- node; qn$order <- c(qn$order, id)
      } else {
        qt <- .qtype_by_class[cls]
        if (is.na(qt)) qt <- .qtype_by_int[as.character(it$QuestionType %||% -1)]
        if (is.na(qt)) { warn("Skipped unknown item type '", cls, "'"); next }
        node$kind <- "question"; node$qtype <- unname(qt)
        node$varname <- it$VariableName %||% it$StataExportCaption
        node$text <- it$QuestionText %||% node$varname
        node$var_label <- it$VariableLabel %||% ""
        node$label <- if (nzchar(node$var_label)) node$var_label else node$text
        node$fold <- fold(paste(node$varname, strip_html(node$text), node$var_label))   # for keyword heuristics
        node$validations <- lapply(it$ValidationConditions %||% list(), function(v)
          list(src = v$Expression %||% "", msg = v$Message %||% "", severity = severity_code(v$Severity)))
        node$options <- options_df(it$Answers)
        cid <- it$CategoriesId %||% it$Properties$CategoriesId
        if (!is.null(cid)) {
          node$categories_id <- tolower(cid)
          node$categories_name <- qn$category_names[[tolower(cid)]] %||% ""
          cats <- qn$categories[[tolower(cid)]]
          if (!is.null(cats)) node$options <- cats
          else { warn("Reusable categories ", cid, " not found for '", node$varname, "': using placeholder codes 1-5")
                 node$options <- data.frame(code = 1:5, label = paste("Option", 1:5), parent = NA_real_) }
        }
        node$is_integer <- isTRUE(it$IsInteger)
        node$decimals <- it$CountOfDecimalPlaces %||% NA
        node$scope <- scope_code(it$QuestionScope)
        node$featured <- isTRUE(it$Featured) || isTRUE(it$Identifying)
        node$linked_qid <- tolower(it$LinkedToQuestionId %||% "") %||% NULL
        node$linked_rid <- tolower(it$LinkedToRosterId %||% "") %||% NULL
        node$linked_filter <- it$LinkedFilterExpression %||% ""
        node$cascade_qid <- tolower(it$CascadeFromQuestionId %||% "")
        node$filter <- it$Properties$OptionsFilterExpression %||% it$OptionsFilterExpression %||% ""
        node$max_answers <- as_num_or_na(it$MaxAllowedAnswers %||% it$MaxAnswerCount)
        node$yesno <- isTRUE(it$YesNoView)
        node$combo <- isTRUE(it$IsFilteredCombobox)
        node$ordered <- isTRUE(it$AreAnswersOrdered)
        node$is_timestamp <- isTRUE(it$IsTimestamp)
        node$mask <- it$Mask %||% ""
        if (node$qtype == "area") {                       # GeometryType 0 polygon, 1 polyline, 2 point, 3 multipoint
          node$geometry <- as.integer(it$Properties$GeometryType %||% 0L)
          node$geo_auto <- identical(as.integer(it$Properties$GeometryInputMode %||% 0L), 1L)   # automatic (walking) mode
        }
        qn$nodes[[id]] <- node; qn$order <- c(qn$order, id)
      }
      ids <- c(ids, id)
    }
    ids
  }
  qn$root <- walk(doc$Children %||% list(), "", character())
  qn$lang <- detect_language(c(qn$title, vapply(qn$order, function(i) strip_html(qn$nodes[[i]]$text %||% ""), "")))
  finalize_questionnaire(qn, verbose)
}

finalize_questionnaire <- function(qn, verbose = TRUE) {
  warn <- function(...) qn$warnings <- c(qn$warnings, paste0(...))
  # name index
  for (id in qn$order) {
    nd <- qn$nodes[[id]]
    if (nd$kind %in% c("question", "variable", "roster") && nzchar(nd$varname %||% "")) {
      if (!is.null(env_get(qn$by_name, nd$varname))) warn("Duplicate variable name '", nd$varname, "'")
      qn$by_name[[nd$varname]] <- nd
    }
  }
  upd <- function(nd) { qn$nodes[[nd$id]] <- nd; if (nzchar(nd$varname %||% "")) qn$by_name[[nd$varname]] <- nd }
  # roster sources, linked/cascade resolution, roster triggers
  for (id in qn$order) {
    nd <- qn$nodes[[id]]
    if (nd$kind == "roster" && nd$roster$type == "question") {
      tq <- qn$nodes[[nd$roster$qid]]
      if (is.null(tq)) stop("Roster '", nd$varname, "' size question not found", call. = FALSE)
      nd$roster$type <- switch(tq$qtype, numeric = "numeric", list = "list",
                               multi = if (isTRUE(tq$yesno)) "yesno" else "multi",
                               stop("Unsupported roster source type: ", tq$qtype))
      nd$roster$trigger <- tq$varname
      upd(nd)
      tq <- qn$nodes[[tq$id]]; tq$is_trigger <- TRUE; upd(tq)
    }
  }
  # level metadata: id labels (fixed titles / multi options) and list-trigger column
  for (id in qn$order) {
    nd <- qn$nodes[[id]]
    if (nd$kind != "roster") next
    lk <- nd$path[length(nd$path)]
    lv <- qn$levels[[lk]]
    if (!is.null(lv$type)) next
    lv$type <- nd$roster$type
    if (nd$roster$type == "fixed") lv$id_labels <- stats::setNames(nd$roster$titles$code, nd$roster$titles$label)
    if (nd$roster$type %in% c("multi", "yesno")) {
      tq <- env_get(qn$by_name, nd$roster$trigger)
      # SuSo labels the id only for inline options, not reusable categories
      if (is.null(tq$categories_id)) lv$id_labels <- stats::setNames(tq$options$code, tq$options$label)
    }
    if (nd$roster$type == "list") lv$list_trigger <- nd$roster$trigger
    if (nd$roster$type != "fixed") lv$trigger <- nd$roster$trigger
    qn$levels[[lk]] <- lv
  }
  for (id in qn$order) {
    nd <- qn$nodes[[id]]
    if (nd$kind != "question") next
    if (nzchar(nd$linked_qid %||% "")) {
      t <- qn$nodes[[nd$linked_qid]]
      if (is.null(t)) { warn("Linked source missing for ", nd$varname); nd$lnk <- NULL }
      else nd$lnk <- list(var = t$varname, path = t$path, roster = FALSE)
    } else if (nzchar(nd$linked_rid %||% "")) {
      t <- qn$nodes[[nd$linked_rid]]
      nd$lnk <- if (is.null(t)) NULL else list(var = t$varname, path = t$path, roster = TRUE)
    }
    if (nzchar(nd$cascade_qid %||% "")) nd$cascade_from <- qn$nodes[[nd$cascade_qid]]$varname
    upd(nd)
  }
  compile_all(qn)
  n_q <- sum(vapply(qn$order, function(i) qn$nodes[[i]]$kind == "question", TRUE))
  sm_msg(verbose, sprintf("  %d questions, %d roster levels, %d sections", n_q, length(qn$levels) - 1, length(qn$root)))
  if (length(qn$warnings)) sm_msg(verbose, paste0("  ! ", unique(qn$warnings), collapse = "\n"))
  class(qn) <- c("suso_questionnaire", class(qn))
  qn
}

compile_all <- function(qn) {
  targets <- list(); nulls <- character()
  unsup <- character()
  comp <- function(src, owner, what) {
    if (!nzchar(trimws(src %||% ""))) return(NULL)
    ce <- suso_compile_expression(src, qn$macros)
    if (!is.null(ce$err)) unsup <<- c(unsup, sprintf("%s [%s]: %s -> %s", owner, what, src, ce$err))
    ce
  }
  for (id in qn$order) {
    nd <- qn$nodes[[id]]
    own <- if (nzchar(nd$varname %||% "")) nd$varname else id
    nd$enc <- comp(nd$cond_src, own, "condition")
    nd$memo_refs <- unique(Filter(nzchar, c("__rows__", nd$enc$refs, nd$varname %||% "")))
    nd$clock_dep <- grepl("DateTime\\s*\\.\\s*(Now|Today|UtcNow)", nd$cond_src)
    if (!is.null(nd$enc$ast)) {
      tg <- ast_targets(nd$enc$ast, nd$varname)
      for (v in names(tg)) targets[[v]] <- unique(c(targets[[v]], tg[[v]]))
      nulls <- c(nulls, attr(tg, "nulls"))
    }
    if (nd$kind == "question") {
      nd$valc <- lapply(nd$validations, function(v) {
        ce <- comp(v$src, own, "validation"); if (is.null(ce)) NULL else c(ce, list(msg = v$msg, severity = v$severity))
      })
      nd$valc <- Filter(Negate(is.null), nd$valc)
      b <- c(NA_real_, NA_real_)
      for (v in nd$valc) if (!is.null(v$ast) && v$severity == 0) {
        bb <- ast_bounds(v$ast, nd$varname)
        b[1] <- suppressWarnings(max(b[1], bb[1], na.rm = TRUE)); b[2] <- suppressWarnings(min(b[2], bb[2], na.rm = TRUE))
      }
      b[!is.finite(b)] <- NA
      nd$bounds <- b
      nd$filterc <- comp(nd$filter, own, "option filter")
      nd$lfilterc <- comp(nd$linked_filter, own, "linked filter")
    }
    if (nd$kind == "variable") nd$exprc <- comp(nd$expression, own, "expression")
    qn$nodes[[id]] <- nd
    if (nzchar(nd$varname %||% "")) qn$by_name[[nd$varname]] <- nd
  }
  # A numeric question that an option filter uses to pick a slice of a coded list (e.g.
  # household_id selecting its members from a preloaded name list, @optioncode between
  # household_id*100+1 and household_id*100+hhsize) is drawn from that list's parent codes,
  # so the filter keeps some options.
  # ponytail: assumes parent codes carry the id; a filter evaluator per candidate if lists vary
  for (id in qn$order) {
    nd <- qn$nodes[[id]]
    if (nd$kind != "question" || is.null(nd$filterc) || !NROW(nd$options)) next
    par <- unique(stats::na.omit(nd$options$parent))
    if (!length(par)) next
    for (v in nd$filterc$refs) {
      x <- env_get(qn$by_name, v)
      if (is.null(x) || x$kind != "question" || !identical(x$qtype, "numeric") || isTRUE(x$is_trigger)) next
      x$domain <- par; qn$nodes[[x$id]] <- x; qn$by_name[[v]] <- x
    }
  }
  # Gatekeepers: a single-select whose `var == code` opens much of the questionnaire
  # (household found, consent) takes that code most of the time, as in real fieldwork.
  # Weight of an item = 1 + items inside it + what the item itself opens.
  size <- list(); par_of <- function(nd) nd$parent %||% ""
  for (id in qn$order) {
    p <- par_of(qn$nodes[[id]])
    while (nzchar(p) && !is.null(qn$nodes[[p]])) { size[[p]] <- (size[[p]] %||% 0) + 1; p <- par_of(qn$nodes[[p]]) }
  }
  eqs <- function(a) {                                   # var == const pairs in an AST
    if (!is.list(a) || is.null(a$k)) return(NULL)
    if (a$k == "bin" && a$op == "==" && a$l$k == "id" && a$r$k == "num") return(list(c(a$l$name, a$r$v)))
    unlist(lapply(a[c("l", "r", "x", "cond", "yes", "no")], eqs), recursive = FALSE)
  }
  opens <- list(); gate <- list()
  for (pass in 1:3) {
    for (id in rev(qn$order)) {
      nd <- qn$nodes[[id]]
      w <- 1 + (size[[id]] %||% 0) + (if (nzchar(nd$varname %||% "")) opens[[nd$varname]] %||% 0 else 0)
      for (e in eqs(nd$enc$ast)) {
        g <- gate[[e[1]]] %||% list(); g[[e[2]]] <- max(g[[e[2]]] %||% 0, w); gate[[e[1]]] <- g
      }
    }
    for (v in names(gate)) opens[[v]] <- max(unlist(gate[[v]]))
  }
  for (v in names(gate)) {
    nd <- env_get(qn$by_name, v); g <- unlist(gate[[v]])
    g2 <- sort(g, decreasing = TRUE)[2]                  # clear winner only: chained alternatives stay uniform
    if (is.null(nd) || nd$kind != "question" || !identical(nd$qtype, "single") || !is.null(nd$lnk) ||
        max(g) < 10 || (!is.na(g2) && max(g) < 1.5 * g2)) next
    nd$gate <- as.numeric(names(g)[which.max(g)]); qn$nodes[[nd$id]] <- nd; qn$by_name[[v]] <- nd
  }
  for (v in names(targets)) {
    nd <- env_get(qn$by_name, v)
    if (is.null(nd) || nd$kind != "question") next
    nd$targets <- targets[[v]]
    qn$nodes[[nd$id]] <- nd; qn$by_name[[v]] <- nd
  }
  for (v in unique(nulls)) {
    nd <- env_get(qn$by_name, v)
    if (is.null(nd) || nd$kind != "question") next
    nd$null_target <- TRUE
    qn$nodes[[nd$id]] <- nd; qn$by_name[[v]] <- nd
  }
  qn$unsupported_parse <- unsup
  invisible(qn)
}

#' @export
print.suso_questionnaire <- function(x, ...) {
  kinds <- vapply(x$order, function(i) x$nodes[[i]]$kind, "")
  qt <- vapply(x$order[kinds == "question"], function(i) x$nodes[[i]]$qtype, "")
  cat(sprintf("<suso_questionnaire> %s (%s)\n", x$title, x$varname))
  cat(sprintf("  Sections: %d | Questions: %d | Variables: %d | Rosters: %d | Levels: %d\n",
              length(x$root), sum(kinds == "question"), sum(kinds == "variable"),
              sum(kinds == "roster"), length(x$levels) - 1))
  cat("  Types:", paste(names(table(qt)), table(qt), sep = "=", collapse = ", "), "\n")
  nc <- sum(vapply(x$order, function(i) !is.null(x$nodes[[i]]$enc), TRUE))
  cat(sprintf("  Conditions: %d | Untranslatable expressions: %d\n", nc, length(x$unsupported_parse)))
  invisible(x)
}

# Accessors used by the interactive wizard
qn_questions <- function(qn, filter = NULL) {
  ids <- qn$order[vapply(qn$order, function(i) qn$nodes[[i]]$kind == "question", TRUE)]
  nodes <- lapply(ids, function(i) qn$nodes[[i]])
  if (!is.null(filter)) nodes <- Filter(filter, nodes)
  nodes
}
