# ---------------------------------------------------------------------------
# tabular.R -- "Tabular" export (tab-separated .tab + Stata .do per file),
# as produced by Survey Solutions 24.x:
#   * UTF-8 with BOM, CRLF, tab-separated, header row
#   * disabled -> empty cell; enabled but unanswered -> -999999999 / ##N/A##
#   * <file>.do: insheet + value/variable labels (reusable categories are
#     defined once at the top under the category name)
# ---------------------------------------------------------------------------

.tab_na <- "-999999999"

tab_cell <- function(x) {
  if (inherits(x, "haven_labelled")) x <- unclass(x)
  if (is.numeric(x)) {
    out <- formatC(x, digits = 15, format = "fg", flag = "", decimal.mark = ".", big.mark = "")   # independent of options(OutDec)
    out <- trimws(out)
    out[is.na(x)] <- ""
    out[haven::is_tagged_na(x, "a")] <- .tab_na
    return(out)
  }
  x <- as.character(x); x[is.na(x)] <- ""
  gsub("[\t\r\n]+", " ", enc2utf8(x))
}

write_tab <- function(df, path) {
  lines <- c(paste(names(df), collapse = "\t"),
             if (nrow(df)) do.call(paste, c(lapply(df, tab_cell), sep = "\t")))
  con <- file(path, open = "wb"); on.exit(close(con))
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con)
  invisible(path)
}

html_unescape <- function(x) gsub("&amp;", "&", gsub("&gt;", ">", gsub("&lt;", "<", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)
do_quote <- function(s) paste0("`\"", gsub("[\r\n]+", "", s), "\"'")
do_define <- function(name, vl) {
  paste0("label define ", name, " ", paste0(fmt_num(unname(vl)), " ", vapply(html_unescape(gsub("'", "", names(vl), fixed = TRUE)), do_quote, ""), " ", collapse = ""))
}

# kind: "main", "roster" (data files) or "system". Layout as HQ writes it: "clear" + CRLF,
# then LF lines; data files separate every variable block with an empty line.
write_do <- function(df, file, kind = "system") {
  blocks <- list()
  cats <- list()
  for (c in names(df)) { cn <- attr(df[[c]], "suso_cat") %||% ""; if (nzchar(cn) && is.null(cats[[cn]])) cats[[cn]] <- attr(df[[c]], "labels") }
  for (cn in names(cats)) blocks[[length(blocks) + 1]] <- do_define(cn, cats[[cn]])
  cols <- names(df)
  if (kind == "main") {
    first <- intersect(c("interview__id", "interview__key", "assignment__id", "sssys_irnd", "has__errors", "interview__status"), cols)
    cols <- c(first, setdiff(cols, first))
  } else if (kind == "roster") {
    ids <- grep("__id$", setdiff(cols, "interview__id"), value = TRUE)
    own <- ids[length(ids)]
    lt <- names(df)[vapply(df, function(x) identical(attr(x, "label"), "Roster list question"), TRUE)]
    cols <- c(own, "interview__key", setdiff(cols, c(ids, "interview__key", "interview__id", lt)), lt, "interview__id",
              setdiff(ids, own))
  }
  for (c in cols) {
    x <- df[[c]]; vl <- attr(x, "labels"); cn <- attr(x, "suso_cat") %||% ""
    if (file == "interview__comments" && c == "role") vl <- NULL      # HQ's Tabular do-file leaves it unlabelled
    b <- character()
    if (length(vl)) {
      ln <- if (nzchar(cn)) cn else c
      if (!nzchar(cn)) b <- do_define(ln, vl)
      b <- c(b, sprintf("label values %s %s", c, ln))
    }
    pre <- if (kind == "system" && grepl("^id[0-9]$", c)) "capture label variable" else "label variable"
    blocks[[length(blocks) + 1]] <- c(b, sprintf("%s %s %s", pre, c, do_quote(attr(x, "suso_tlabel") %||% attr(x, "label") %||% "")))
  }
  body <- if (kind == "system") paste0("\n", paste0(unlist(blocks), "\n", collapse = ""))
          else paste0(vapply(blocks, function(b) paste0("\n", paste0(b, "\n", collapse = "")), ""), collapse = "")
  paste0("clear\r\n", sprintf('insheet using "%s.tab", tab case names\n', file), body)
}

write_text_raw <- function(txt, path) {
  con <- file(path, open = "wb"); on.exit(close(con))
  writeBin(charToRaw(enc2utf8(txt)), con)
}

# text files as HQ writes them: UTF-8 (no BOM), LF; independent of the session's native encoding
write_lines_utf8 <- function(lines, path) write_text_raw(paste0(paste(as_utf8(lines), collapse = "\n"), "\n"), path)

write_lines_crlf <- function(lines, path) {
  con <- file(path, open = "wb"); on.exit(close(con))
  writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con)
}
