# ---------------------------------------------------------------------------
# app.R -- Shiny interface (bslib), modelled on the DIME iehfc workflow:
#   1 Questionnaire -> 2 Sample design -> 3 Settings -> 4 Generate & review -> 5 Export
# Settings can be saved/loaded as JSON; the app also writes template R code
# reproducing the run and a Stata do-file that loads and merges the output.
# ---------------------------------------------------------------------------

.app_pkgs <- c("shiny", "bslib", "DT")

ui_hint <- function(...) shiny::tags$small(class = "text-muted d-block mb-2", ...)

#' Launch the SuSoSimulate Shiny app
#'
#' Point-and-click interface for the whole workflow: upload a questionnaire,
#' set the sample design and generation settings, generate, review coverage
#' and data, and download the Stata export, paradata, reports, settings and
#' template code.
#'
#' @param launch.browser Open the app in the default browser.
#' @param port Port (default: random).
#' @param ... Further arguments to [shiny::runApp()].
#' @return Called for its side effect (runs the app).
#' @examples
#' if (interactive()) suso_app()
#' @export
suso_app <- function(launch.browser = TRUE, port = getOption("shiny.port"), ...) {
  miss <- .app_pkgs[!vapply(.app_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss))
    stop("The app needs: ", paste(miss, collapse = ", "), ". Install with install.packages(c(",
         paste0('"', miss, '"', collapse = ", "), "))", call. = FALSE)
  shiny::runApp(suso_app_object(), launch.browser = launch.browser, port = port, ...)
}

#' Shiny app object (for deployment, e.g. Posit Connect / shinyapps.io)
#' @return A `shiny.appobj`.
#' @export
suso_app_object <- function() {
  www <- system.file("app", "www", package = "SuSoSimulate")
  if (nzchar(www)) shiny::addResourcePath("susosim", www)
  shiny::shinyApp(app_ui(), app_server)
}

# --------------------------------------------------------------------------- UI
app_ui <- function() {
  num <- function(id, label, value, min = NA, max = NA, step = NA)
    shiny::numericInput(id, label, value = value, min = min, max = max, step = step)
  theme <- bslib::bs_theme(version = 5, primary = "#0E7C86", secondary = "#F4A259",
                           base_font = bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif"))
  bslib::page_navbar(
    id = "nav", theme = theme, window_title = "SuSoSimulate", bg = "#16324F", inverse = TRUE, fillable = FALSE,
    title = shiny::tags$span(
      shiny::tags$img(src = "susosim/logo.png", height = "34px", style = "margin-right:8px;", alt = ""),
      "SuSoSimulate"),
    header = shiny::tags$style(shiny::HTML("
      .status-covered{color:#1b7f3b;font-weight:600}.status-partial{color:#b26b00;font-weight:600}
      .status-never{color:#a61b1b;font-weight:600} .bslib-value-box .value-box-value{font-size:1.6rem}
      pre.codebox{max-height:420px;overflow:auto;background:#f6f8fa;padding:10px;border-radius:6px}")),

    # 1 ---------------------------------------------------------------------
    bslib::nav_panel("1. Questionnaire", value = "q",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(width = 340,
          shiny::h5("Load a questionnaire"),
          shiny::fileInput("qfile", NULL, accept = c(".json", ".zip", ".pdf", ".xlsx", ".tsv", ".txt"), multiple = TRUE,
                           placeholder = "HQ export .zip, content.zip, document.json (+ category files)"),
          ui_hint("Best: any Headquarters export zip, or Questionnaire/content.zip from one (exact questionnaire + categories). You can also select document.json together with the category files. PDF input is experimental and loses detail."),
          shiny::actionButton("use_demo", "Use the demo questionnaire", class = "btn-outline-secondary w-100"),
          shiny::hr(),
          shiny::uiOutput("q_status")),
        bslib::layout_columns(col_widths = c(3, 3, 3, 3),
          bslib::value_box("Questions", shiny::textOutput("vb_q"), theme = "primary"),
          bslib::value_box("Roster levels", shiny::textOutput("vb_r"), theme = "secondary"),
          bslib::value_box("Conditions", shiny::textOutput("vb_c"), theme = "primary"),
          bslib::value_box("Untranslatable", shiny::textOutput("vb_u"), theme = "light")),
        bslib::card(bslib::card_header("Export files (one per roster level)"), DT::DTOutput("tbl_levels")),
        bslib::card(bslib::card_header("Questions"), DT::DTOutput("tbl_questions")),
        bslib::card(bslib::card_header("Parser messages"), shiny::verbatimTextOutput("q_msgs")))),

    # 2 ---------------------------------------------------------------------
    bslib::nav_panel("2. Sample design", value = "d",
      bslib::layout_columns(col_widths = c(4, 4, 4),
        bslib::card(bslib::card_header("Sample"),
          num("n_psu", "Primary sampling units (PSUs)", 10, 1),
          num("ssu_per_psu", "Interviews per PSU", 10, 1),
          shiny::selectInput("strata_var", "Variable receiving the stratum", c("(none)" = "")),
          num("n_strata", "Number of strata (if not categorical)", 1, 1),
          shiny::selectInput("psu_var", "Variable receiving the PSU id", c("(none)" = "")),
          shiny::selectInput("ssu_var", "Variable receiving the household / SSU id", c("(none)" = "")),
          ui_hint("Selectors list identifying (cover) questions first, then other main-level questions."),
          shiny::uiOutput("n_total")),
        bslib::card(bslib::card_header("Field teams & calendar"),
          num("n_interviewers", "Interviewers", 5, 1),
          num("ipsup", "Interviewers per supervisor", 5, 1),
          shiny::dateInput("start_date", "Fieldwork start", value = Sys.Date()),
          num("ipd", "Interviews per interviewer per day", 4, 1),
          shiny::selectInput("assignment_per", "Assignments", c("One per interview" = "interview", "One per PSU" = "psu")),
          shiny::textInput("tz_offset", "Local UTC offset", "+03:00")),
        bslib::card(bslib::card_header("GPS area & preload frame"),
          ui_hint("Bounding box (decimal degrees) for PSU centroids."),
          bslib::layout_column_wrap(width = 1 / 2, fill = FALSE,
            num("xmin", "xmin (lon)", 29.5, -180, 180), num("xmax", "xmax (lon)", 35, -180, 180),
            num("ymin", "ymin (lat)", -1.5, -90, 90), num("ymax", "ymax (lat)", 4.2, -90, 90)),
          shiny::fileInput("frame_file", "Optional preload frame (.csv)", accept = ".csv"),
          ui_hint("One row per interview; columns named after questionnaire variables are preloaded. Optional .psu / .stratum columns. Overrides the sample settings."),
          shiny::uiOutput("frame_status")))),

    # 3 ---------------------------------------------------------------------
    bslib::nav_panel("3. Settings", value = "s",
      bslib::layout_columns(col_widths = c(6, 6),
        bslib::card(bslib::card_header("Roster sizes"),
          ui_hint("Rows generated for each numeric / list roster trigger. Validation bounds are respected."),
          num("roster_mean", "Default mean rows (other rosters)", 4, 0),
          shiny::uiOutput("roster_ui")),
        bslib::card(bslib::card_header("Data realism & coverage"),
          shiny::sliderInput("item_nonresponse", "Item non-response", 0, 0.5, 0, step = 0.01),
          shiny::sliderInput("error_rate", "Validation-error injection", 0, 0.5, 0, step = 0.01),
          shiny::sliderInput("special_value_rate", "Numeric special values (e.g. -98)", 0, 0.3, 0.03, step = 0.01),
          shiny::sliderInput("target_prob", "Branch steering strength", 0, 1, 0.5, step = 0.05),
          bslib::input_switch("coverage_repair", "Repair coverage (every reachable branch)", TRUE),
          num("repair_budget", "Repair budget (candidate interviews)", 400, 0),
          num("repair_time", "Repair time limit (seconds)", 120, 0),
          bslib::input_switch("allow_augment", "Append (rather than replace) repair interviews", FALSE),
          shiny::h6("Final interview status shares"),
          bslib::layout_column_wrap(width = 1 / 3, fill = FALSE,
            num("st100", "Completed", 0.2, 0, 1, 0.05), num("st120", "Approved (sup.)", 0.3, 0, 1, 0.05),
            num("st130", "Approved (HQ)", 0.5, 0, 1, 0.05), num("st65", "Rejected (sup.)", 0, 0, 1, 0.05),
            num("st125", "Rejected (HQ)", 0, 0, 1, 0.05)),
          num("seed", "Random seed", 12345, 1)))),

    # 4 ---------------------------------------------------------------------
    bslib::nav_panel("4. Generate & review", value = "g",
      bslib::layout_columns(col_widths = c(3, 9),
        bslib::card(
          shiny::actionButton("go", "Generate mock data", class = "btn-primary btn-lg w-100"),
          shiny::uiOutput("run_status")),
        bslib::layout_columns(col_widths = c(3, 3, 3, 3),
          bslib::value_box("Interviews", shiny::textOutput("vb_n"), theme = "primary"),
          bslib::value_box("Branch coverage", shiny::textOutput("vb_bc"), theme = "secondary"),
          bslib::value_box("Option coverage", shiny::textOutput("vb_oc"), theme = "primary"),
          bslib::value_box("Paradata events", shiny::textOutput("vb_pe"), theme = "light"))),
      bslib::navset_card_tab(
        bslib::nav_panel("Branch coverage",
          ui_hint("Each enabling condition: interviews where it was TRUE / FALSE. 'unreachable' notes mean the questionnaire's own validations rule the outcome out."),
          DT::DTOutput("tbl_cov")),
        bslib::nav_panel("Option coverage", DT::DTOutput("tbl_opt")),
        bslib::nav_panel("Data browser",
          shiny::selectInput("data_file", "File", choices = NULL), DT::DTOutput("tbl_data")),
        bslib::nav_panel("Paradata", DT::DTOutput("tbl_para")),
        bslib::nav_panel("System files",
          shiny::selectInput("sys_file", "File", choices = NULL), DT::DTOutput("tbl_sys")))),

    # 5 ---------------------------------------------------------------------
    bslib::nav_panel("5. Export", value = "x",
      bslib::layout_columns(col_widths = c(4, 4, 4),
        bslib::card(bslib::card_header("Mock export"),
          ui_hint("Folder <Q>_SuSoMock_<timestamp> with <Q>_<version>_STATA_All.zip and <Q>_<version>_Paradata_All.zip (as downloaded from HQ), coverage reports, sample frame and settings."),
          shiny::numericInput("version", "Questionnaire version (HQ)", value = 1, min = 1, step = 1),
          shiny::downloadButton("dl_zip", "Download everything (.zip)", class = "btn-primary w-100 mb-3"),
          shiny::textInput("save_dir", "Or save to a folder on this computer", value = getwd()),
          shiny::actionButton("save_btn", "Save export to folder", class = "w-100"),
          shiny::uiOutput("save_status")),
        bslib::card(bslib::card_header("Settings"),
          ui_hint("Save the current settings and re-load them later (or share them with your team)."),
          shiny::downloadButton("dl_params", "Download settings (.json)", class = "w-100 mb-2"),
          shiny::fileInput("ul_params", "Load settings (.json)", accept = ".json")),
        bslib::card(bslib::card_header("Template code"),
          shiny::downloadButton("dl_r", "R script reproducing this run", class = "w-100 mb-2"),
          shiny::downloadButton("dl_do", "Stata do-file: load & merge output", class = "w-100"))),
      bslib::card(bslib::card_header("Preview: R code"), shiny::tags$pre(class = "codebox", shiny::textOutput("code_r"))),
      bslib::card(bslib::card_header("Preview: Stata do-file"), shiny::tags$pre(class = "codebox", shiny::textOutput("code_do")))),

    bslib::nav_spacer(),
    bslib::nav_item(shiny::tags$a(href = "https://github.com/emawbgit/SuSoSimulate", target = "_blank", "GitHub"))
  )
}

# ----------------------------------------------------------------------- server
app_server <- function(input, output, session) {
  rv <- shiny::reactiveValues(qn = NULL, qpath = NULL, qname = NULL, res = NULL, frame = NULL, err = NULL, log = NULL, saved = NULL)
  pct <- function(x) sprintf("%.1f%%", 100 * x)
  dt <- function(df, ...) DT::datatable(df, rownames = FALSE, filter = "none",
                                        options = list(pageLength = 15, scrollX = TRUE, ...))

  # ---- 1 questionnaire
  load_q <- function(path, name) {
    msgs <- character()
    qn <- tryCatch(withCallingHandlers(suso_read_questionnaire(path, verbose = TRUE),
                     message = function(m) { msgs <<- c(msgs, trimws(conditionMessage(m))); invokeRestart("muffleMessage") }),
                   error = function(e) e)
    if (inherits(qn, "error")) {
      rv$qn <- NULL; rv$err <- conditionMessage(qn)
      shiny::showNotification(paste("Could not read questionnaire:", rv$err), type = "error", duration = 10)
      return(invisible())
    }
    rv$qn <- qn; rv$qpath <- path; rv$qname <- name; rv$err <- NULL; rv$res <- NULL; rv$log <- msgs
    if (!is.na(qn$hq_version)) shiny::updateNumericInput(session, "version", value = qn$hq_version)
    shiny::showNotification(sprintf("Loaded '%s'", qn$title), type = "message")
  }
  shiny::observeEvent(input$qfile, {
    f <- input$qfile
    if (length(f$datapath) == 1) return(load_q(f$datapath, f$name))
    td <- tempfile("q_upload_"); dir.create(td)          # document.json + Categories files -> one folder
    file.copy(f$datapath, file.path(td, f$name))
    load_q(td, paste(f$name, collapse = ", "))
  })
  shiny::observeEvent(input$use_demo, load_q(system.file("extdata", "demo_household.json", package = "SuSoSimulate"),
                                             "demo_household.json"))

  qstats <- shiny::reactive({
    qn <- rv$qn; shiny::req(qn)
    nodes <- lapply(qn$order, function(i) qn$nodes[[i]])
    list(nq = sum(vapply(nodes, function(n) n$kind == "question", TRUE)),
         nl = length(qn$levels) - 1,
         nc = sum(vapply(nodes, function(n) !is.null(n$enc), TRUE)),
         nu = length(qn$unsupported_parse))
  })
  qs_or_dash <- function(k) shiny::renderText(if (is.null(rv$qn)) "-" else qstats()[[k]])
  output$vb_q <- qs_or_dash("nq")
  output$vb_r <- qs_or_dash("nl")
  output$vb_c <- qs_or_dash("nc")
  output$vb_u <- qs_or_dash("nu")
  output$q_status <- shiny::renderUI({
    if (!is.null(rv$err)) return(shiny::div(class = "alert alert-danger", rv$err))
    if (is.null(rv$qn)) return(shiny::div(class = "alert alert-info", "No questionnaire loaded yet."))
    shiny::div(class = "alert alert-success", shiny::strong(rv$qn$title), shiny::br(),
               sprintf("Export name: %s | source: %s", rv$qn$varname, rv$qname))
  })
  output$tbl_levels <- DT::renderDT({
    qn <- rv$qn; shiny::req(qn)
    lv <- qn$levels
    df <- data.frame(file = vapply(lv, `[[`, "", "file"),
                     type = vapply(lv, function(l) l$type %||% "main", ""),
                     depth = vapply(lv, function(l) as.integer(l$depth), 1L),
                     trigger = vapply(lv, function(l) l$trigger %||% "", ""),
                     questions = vapply(names(lv), function(k) length(level_nodes(qn, if (k == "main") "" else k)), 1L),
                     row.names = NULL, stringsAsFactors = FALSE)
    dt(df, dom = "t", pageLength = 50)
  })
  output$tbl_questions <- DT::renderDT({
    qn <- rv$qn; shiny::req(qn)
    qs <- qn_questions(qn)
    df <- data.frame(variable = vapply(qs, `[[`, "", "varname"), type = vapply(qs, `[[`, "", "qtype"),
                     level = vapply(qs, function(n) if (length(n$path)) qn$levels[[n$path[length(n$path)]]]$file else qn$varname, ""),
                     identifying = vapply(qs, function(n) isTRUE(n$featured), TRUE),
                     condition = vapply(qs, function(n) gsub("\\s+", " ", n$cond_src %||% ""), ""),
                     text = vapply(qs, function(n) substr(strip_html(n$text), 1, 90), ""), stringsAsFactors = FALSE)
    dt(df)
  })
  output$q_msgs <- shiny::renderText({
    qn <- rv$qn; shiny::req(qn)
    paste(c(rv$log, if (length(qn$warnings)) paste("!", unique(qn$warnings)),
            if (length(qn$unsupported_parse)) c("Untranslatable expressions (fall back to random outcomes):", qn$unsupported_parse)),
          collapse = "\n")
  })

  # ---- 2 design: selectors from questionnaire
  shiny::observeEvent(rv$qn, {
    qn <- rv$qn
    main <- qn_questions(qn, function(n) !length(n$path))
    feat <- vapply(Filter(function(n) isTRUE(n$featured), main), `[[`, "", "varname")
    other <- setdiff(vapply(main, `[[`, "", "varname"), feat)
    ch <- c("(none)" = "", if (length(feat)) list(Identifying = stats::setNames(feat, feat)),
            if (length(other)) list(Other = stats::setNames(other, other)))
    guess <- function(rx) { g <- grep(rx, feat, value = TRUE, ignore.case = TRUE); if (length(g)) g[1] else "" }
    shiny::updateSelectInput(session, "strata_var", choices = ch, selected = guess("region|strat|province|district"))
    shiny::updateSelectInput(session, "psu_var", choices = ch, selected = guess("^ea|psu|cluster"))
    shiny::updateSelectInput(session, "ssu_var", choices = ch, selected = guess("hh_?id|hhid|household|ssu"))
  })
  shiny::observeEvent(input$frame_file, {
    fr <- tryCatch(utils::read.csv(input$frame_file$datapath, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) e)
    if (inherits(fr, "error") || !nrow(fr)) {
      rv$frame <- NULL; shiny::showNotification("Could not read the frame CSV.", type = "error")
    } else rv$frame <- fr
  })
  output$frame_status <- shiny::renderUI({
    if (is.null(rv$frame)) return(NULL)
    known <- if (!is.null(rv$qn)) names(rv$frame)[vapply(names(rv$frame), function(v) !is.null(env_get(rv$qn$by_name, v)), TRUE)] else character()
    shiny::div(class = "alert alert-secondary", sprintf("Frame: %d rows. Preloaded variables: %s", nrow(rv$frame),
                                                        if (length(known)) paste(known, collapse = ", ") else "none matched"))
  })
  output$n_total <- shiny::renderUI({
    n <- if (!is.null(rv$frame)) nrow(rv$frame) else (input$n_psu %||% 0) * (input$ssu_per_psu %||% 0)
    shiny::div(class = "alert alert-info", sprintf("Total interviews: %s", format(n, big.mark = ",")))
  })

  # ---- 3 roster size inputs per trigger
  triggers <- shiny::reactive({
    qn <- rv$qn; shiny::req(qn)
    qn_questions(qn, function(n) isTRUE(n$is_trigger) && n$qtype %in% c("numeric", "list"))
  })
  output$roster_ui <- shiny::renderUI({
    tq <- triggers()
    if (!length(tq)) return(ui_hint("No numeric or list roster triggers in this questionnaire."))
    lapply(tq, function(n) {
      b <- n$bounds
      lo <- if (is.na(b[1])) 0 else max(0, ceiling(b[1]))
      hi <- min(c(if (is.na(b[2])) 30 else b[2], if (n$qtype == "list" && !is.na(n$max_answers)) n$max_answers, 60), na.rm = TRUE)
      hi <- max(hi, lo + 1)
      shiny::sliderInput(paste0("rs_", n$varname), sprintf("%s (%s)", n$varname, n$qtype),
                         min = lo, max = hi, value = c(max(lo, 1), min(hi, 8)), step = 1)
    })
  })

  # ---- settings collection
  settings <- shiny::reactive({
    rs <- list()
    for (n in tryCatch(triggers(), error = function(e) list())) {
      v <- input[[paste0("rs_", n$varname)]]
      if (length(v) == 2) rs[[n$varname]] <- as.integer(v)
    }
    sm <- c("100" = input$st100, "120" = input$st120, "130" = input$st130, "65" = input$st65, "125" = input$st125)
    sm <- sm[!is.na(sm) & sm > 0]
    nz <- function(x) if (is.null(x) || !nzchar(x)) NULL else x
    list(
      design = list(n_psu = input$n_psu, ssu_per_psu = input$ssu_per_psu, strata_var = nz(input$strata_var),
                    n_strata = input$n_strata, psu_var = nz(input$psu_var), ssu_var = nz(input$ssu_var),
                    n_interviewers = input$n_interviewers, interviewers_per_supervisor = input$ipsup,
                    start_date = as.character(input$start_date), interviews_per_day = input$ipd,
                    bbox = c(input$xmin, input$ymin, input$xmax, input$ymax), assignment_per = input$assignment_per),
      config = list(item_nonresponse = input$item_nonresponse, error_rate = input$error_rate,
                    special_value_rate = input$special_value_rate, target_prob = input$target_prob,
                    roster_mean = input$roster_mean, roster_sizes = rs, status_mix = sm, tz_offset = input$tz_offset,
                    coverage_repair = isTRUE(input$coverage_repair), repair_budget = input$repair_budget, repair_time = input$repair_time %||% 120,
                    allow_augment = isTRUE(input$allow_augment)),
      seed = input$seed, version = input$version)
  })

  build_args <- function(s) {
    d <- s$design
    des <- do.call(suso_design, c(d[setdiff(names(d), "start_date")], list(start_date = as.Date(d$start_date), frame = rv$frame)))
    cfg <- do.call(suso_mock_config, s$config)
    list(design = des, config = cfg)
  }

  # ---- 4 generate
  shiny::observeEvent(input$go, {
    if (is.null(rv$qn)) { shiny::showNotification("Load a questionnaire first (tab 1).", type = "warning"); return() }
    s <- settings()
    args <- tryCatch(build_args(s), error = function(e) e)
    if (inherits(args, "error")) { shiny::showNotification(conditionMessage(args), type = "error", duration = 10); return() }
    warns <- character()
    res <- shiny::withProgress(message = "Generating", value = 0, {
      tryCatch(withCallingHandlers(
        suso_mock(rv$qn, design = args$design, config = args$config, out_dir = NULL, seed = s$seed, verbose = FALSE,
                  progress = function(v, detail) shiny::setProgress(value = v, detail = detail)),
        warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }),
        error = function(e) e)
    })
    if (inherits(res, "error")) {
      rv$res <- NULL
      shiny::showNotification(paste("Generation failed:", conditionMessage(res)), type = "error", duration = 15)
      return()
    }
    rv$res <- res
    for (w in unique(warns)) shiny::showNotification(w, type = "warning", duration = 10)
    shiny::updateSelectInput(session, "data_file", choices = names(res$data))
    shiny::updateSelectInput(session, "sys_file", choices = names(res$system))
    bslib::nav_select("nav", "g", session = session)
  })
  output$run_status <- shiny::renderUI({
    r <- rv$res
    if (is.null(r)) return(ui_hint("Settings from tabs 2-3 are used. Large or complex questionnaires can take a few minutes."))
    shiny::div(class = "alert alert-success mt-3", sprintf("Generated %d interviews into %d files.", nrow(r$data[[1]]), length(r$data)))
  })
  sm <- shiny::reactive({ shiny::req(rv$res); rv$res$coverage$summary })
  dash <- function(expr) if (is.null(rv$res)) "-" else expr
  output$vb_n <- shiny::renderText(dash(format(sm()[["interviews"]], big.mark = ",")))
  output$vb_bc <- shiny::renderText(dash(pct(sm()[["branch_coverage"]])))
  output$vb_oc <- shiny::renderText(dash(pct(sm()[["option_coverage"]])))
  output$vb_pe <- shiny::renderText(dash(format(nrow(rv$res$paradata), big.mark = ",")))
  output$tbl_cov <- DT::renderDT({
    shiny::req(rv$res)
    b <- rv$res$coverage$branches
    if (!nrow(b)) return(dt(data.frame(message = "No enabling conditions in this questionnaire.")))
    b <- b[order(b$status == "covered", b$item), ]
    DT::formatStyle(dt(b), "status", color = DT::styleEqual(c("covered", "TRUE only", "FALSE only", "never evaluated"),
                                                           c("#1b7f3b", "#b26b00", "#b26b00", "#a61b1b")), fontWeight = "bold")
  })
  output$tbl_opt <- DT::renderDT({ shiny::req(rv$res); dt(rv$res$coverage$options) })
  output$tbl_data <- DT::renderDT({
    shiny::req(rv$res, input$data_file)
    d <- rv$res$data[[input$data_file]]; shiny::req(d)
    d <- as.data.frame(lapply(d, function(x) if (inherits(x, "haven_labelled")) as.vector(x) else x), check.names = FALSE)
    dt(utils::head(d, 5000))
  })
  output$tbl_para <- DT::renderDT({ shiny::req(rv$res); dt(utils::head(rv$res$paradata, 5000)) })
  output$tbl_sys <- DT::renderDT({
    shiny::req(rv$res, input$sys_file)
    d <- rv$res$system[[input$sys_file]]; shiny::req(d)
    dt(as.data.frame(lapply(d, function(x) if (inherits(x, "haven_labelled")) as.vector(x) else x), check.names = FALSE))
  })

  # ---- 5 export
  export_to <- function(dir, ts) {
    root <- suppressWarnings(write_suso_mock(rv$res, dir, version = input$version, timestamp = ts, verbose = FALSE))
    writeLines(app_r_code(settings(), rv$qname, !is.null(rv$frame)), file.path(root, "reproduce_mock.R"))
    writeLines(app_do_code(rv$res$questionnaire, input$version), file.path(root, "load_mock.do"))
    root
  }
  dl_ts <- shiny::reactiveVal(Sys.time())
  output$dl_zip <- shiny::downloadHandler(
    filename = function() { dl_ts(Sys.time())
      sprintf("%s_SuSoMock_%s.zip", rv$res$questionnaire$varname %||% "suso", format(dl_ts(), "%Y%m%d_%H%M%S")) },
    content = function(file) {
      shiny::req(rv$res)
      td <- tempfile("susosim_"); dir.create(td)
      root <- export_to(td, dl_ts())
      zip::zipr(file, root)
    })
  shiny::observeEvent(input$save_btn, {
    if (is.null(rv$res)) { shiny::showNotification("Generate data first (tab 4).", type = "warning"); return() }
    root <- tryCatch(export_to(input$save_dir, Sys.time()), error = function(e) e)
    if (inherits(root, "error")) shiny::showNotification(paste("Save failed:", conditionMessage(root)), type = "error", duration = 10)
    else rv$saved <- root
  })
  output$save_status <- shiny::renderUI(if (!is.null(rv$saved)) shiny::div(class = "alert alert-success mt-2 small", "Saved to ", shiny::code(rv$saved)))
  output$dl_params <- shiny::downloadHandler(
    filename = function() "susosimulate_settings.json",
    content = function(file) jsonlite::write_json(settings(), file, auto_unbox = TRUE, pretty = TRUE, null = "null"))
  shiny::observeEvent(input$ul_params, {
    p <- tryCatch(jsonlite::fromJSON(input$ul_params$datapath, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(p) || is.null(p$design) || is.null(p$config)) { shiny::showNotification("Not a SuSoSimulate settings file.", type = "error"); return() }
    d <- p$design; cf <- p$config
    upd_num <- function(id, v) if (!is.null(v)) shiny::updateNumericInput(session, id, value = v)
    upd_num("n_psu", d$n_psu); upd_num("ssu_per_psu", d$ssu_per_psu); upd_num("n_strata", d$n_strata)
    upd_num("n_interviewers", d$n_interviewers); upd_num("ipsup", d$interviewers_per_supervisor); upd_num("ipd", d$interviews_per_day)
    if (length(d$bbox) == 4) { upd_num("xmin", d$bbox[1]); upd_num("ymin", d$bbox[2]); upd_num("xmax", d$bbox[3]); upd_num("ymax", d$bbox[4]) }
    if (!is.null(d$start_date)) shiny::updateDateInput(session, "start_date", value = as.Date(d$start_date))
    for (v in c("strata_var", "psu_var", "ssu_var")) shiny::updateSelectInput(session, v, selected = d[[v]] %||% "")
    if (!is.null(d$assignment_per)) shiny::updateSelectInput(session, "assignment_per", selected = d$assignment_per)
    for (v in c("item_nonresponse", "error_rate", "special_value_rate", "target_prob"))
      if (!is.null(cf[[v]])) shiny::updateSliderInput(session, v, value = cf[[v]])
    upd_num("roster_mean", cf$roster_mean); upd_num("repair_budget", cf$repair_budget); upd_num("repair_time", cf$repair_time); upd_num("seed", p$seed); upd_num("version", p$version)
    if (!is.null(cf$tz_offset)) shiny::updateTextInput(session, "tz_offset", value = cf$tz_offset)
    if (!is.null(cf$coverage_repair)) bslib::update_switch("coverage_repair", value = isTRUE(cf$coverage_repair), session = session)
    if (!is.null(cf$allow_augment)) bslib::update_switch("allow_augment", value = isTRUE(cf$allow_augment), session = session)
    stm <- unlist(cf$status_mix)
    for (k in c("100", "120", "130", "65", "125")) upd_num(paste0("st", k), if (k %in% names(stm)) stm[[k]] else 0)
    for (v in names(cf$roster_sizes)) shiny::updateSliderInput(session, paste0("rs_", v), value = unlist(cf$roster_sizes[[v]]))
    shiny::showNotification("Settings loaded.", type = "message")
  })
  output$dl_r <- shiny::downloadHandler(filename = function() "reproduce_mock.R",
    content = function(file) writeLines(app_r_code(settings(), rv$qname, !is.null(rv$frame)), file))
  output$dl_do <- shiny::downloadHandler(filename = function() "load_mock.do",
    content = function(file) { shiny::req(rv$qn); writeLines(app_do_code(rv$qn, input$version), file) })
  output$code_r <- shiny::renderText(paste(app_r_code(settings(), rv$qname, !is.null(rv$frame)), collapse = "\n"))
  output$code_do <- shiny::renderText({ shiny::req(rv$qn); paste(app_do_code(rv$qn, input$version), collapse = "\n") })
}

# ---------------------------------------------------------------- code templates
r_lit <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.list(x)) {
    if (!length(x)) return("list()")
    return(paste0("list(", paste(names(x), vapply(x, r_lit, ""), sep = " = ", collapse = ", "), ")"))
  }
  if (is.character(x)) v <- paste0('"', x, '"') else if (is.logical(x)) v <- ifelse(x, "TRUE", "FALSE") else v <- fmt_num(x)
  if (!is.null(names(x))) v <- paste0('"', names(x), '" = ', v)
  if (length(x) == 1 && is.null(names(x))) v else paste0("c(", paste(v, collapse = ", "), ")")
}

app_r_code <- function(s, qname = NULL, has_frame = FALSE) {
  d <- s$design; cf <- s$config
  dd <- d[!vapply(d, is.null, TRUE)]
  if (has_frame) dd$frame <- quote(frame)
  dd$start_date <- NULL
  arg <- function(l) paste0("    ", names(l), " = ", vapply(l, function(v) if (is.name(v)) as.character(v) else r_lit(v), ""), collapse = ",\n")
  c("# Reproduce this SuSoSimulate run",
    "# install.packages(\"remotes\"); remotes::install_github(\"emawbgit/SuSoSimulate\")",
    "library(SuSoSimulate)", "",
    sprintf('questionnaire <- "%s"   # path to document.json / HQ export .zip', qname %||% "document.json"),
    if (has_frame) 'frame <- read.csv("frame.csv", stringsAsFactors = FALSE, check.names = FALSE)',
    "",
    "res <- suso_mock(",
    "  questionnaire,",
    "  design = suso_design(",
    paste0(arg(dd), ",\n    start_date = as.Date(\"", d$start_date, "\")),"),
    "  config = suso_mock_config(",
    paste0(arg(cf), "),"),
    sprintf('  out_dir = ".", version = %s, seed = %s)   # writes <Q>_SuSoMock_<timestamp>/', fmt_num(s$version %||% 1), fmt_num(s$seed %||% 1)),
    "", "res                         # coverage summary", "res$coverage$branches       # per-condition detail")
}

app_do_code <- function(qn, version = 1) {
  main <- qn$varname
  stem <- sprintf("%s_%d", main, as.integer(version %||% 1))
  out <- c("* Load and merge SuSoSimulate output (same layout as a Survey Solutions Stata export)",
           sprintf("* cd into the %s_SuSoMock_<timestamp> folder, then run.", main),
           sprintf('global mock "%s_STATA_All"', stem), "",
           sprintf('use "$mock/%s.dta", clear', main), "describe, short", "tab interview__status", "")
  lv <- qn$levels[setdiff(names(qn$levels), "main")]
  for (k in names(lv)) {
    l <- lv[[k]]
    chain <- character(); cur <- l$parent %||% ""
    while (nzchar(cur)) { chain <- c(qn$levels[[cur]]$id_col, chain); cur <- qn$levels[[cur]]$parent %||% "" }
    pfile <- if (nzchar(l$parent %||% "")) qn$levels[[l$parent]]$file else main
    out <- c(out, sprintf("* %s: one row per %s (level %d)", l$file, l$id_col, l$depth),
             sprintf('use "$mock/%s.dta", clear', l$file),
             sprintf('merge m:1 interview__key %s using "$mock/%s.dta", assert(match using) keep(match) nogenerate keepusing(interview__id)',
                     paste(chain, collapse = " "), pfile),
             sprintf("isid interview__key %s", paste(c(chain, l$id_col), collapse = " ")), "")
  }
  c(out, "* Paradata", sprintf('* cd "%s_Paradata_All"', stem), "* do paradata.do")
}
