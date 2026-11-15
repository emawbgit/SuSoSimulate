# Installer for SuSoSimulate that needs base R only (no remotes, no Rtools, no admin
# rights). Written for locked-down machines (e.g. World Bank laptops):
#   * installs into your personal library when the system library is read-only;
#   * uses binary packages from CRAN (or the repository your IT configured) - no compiler;
#   * retries downloads with the Windows internet settings (proxy) if the default fails;
#   * installs the optional packages (readxl, shiny, bslib, DT, pdftools, testthat) one by
#     one and keeps going if one is blocked;
#   * installs SuSoSimulate itself from the public GitHub archive (no API, no token), or from
#     a file you downloaded: a .tar.gz, the GitHub "Download ZIP", or an unpacked folder.
#
# Usage, in R or RStudio:
#   source("https://raw.githubusercontent.com/emawbgit/SuSoSimulate/main/install_SuSoSimulate.R")
#   install_susosimulate()                                   # from GitHub
#   source("install_SuSoSimulate.R"); install_susosimulate("C:/Users/me/Downloads/SuSoSimulate-main.zip")
# From a terminal:
#   Rscript install_SuSoSimulate.R [path-to-zip-or-tar.gz-or-folder]

install_susosimulate <- function(source = NULL, optional = c("readxl", "shiny", "bslib", "DT", "pdftools"),
                                 repos = NULL, lib = NULL, ref = "main") {
  say <- function(...) message(sprintf(...))
  # 1. a library we can write to
  if (is.null(lib)) {
    writable <- function(p) {                                        # file.access() is unreliable on Windows
      f <- file.path(p, sprintf(".write_test_%d", Sys.getpid()))
      ok <- suppressWarnings(file.create(f, showWarnings = FALSE)); if (ok) unlink(f); ok
    }
    ok <- Filter(writable, .libPaths())
    lib <- if (length(ok)) ok[1] else {
      p <- Sys.getenv("R_LIBS_USER"); p <- path.expand(strsplit(p, .Platform$path.sep)[[1]][1])
      dir.create(p, recursive = TRUE, showWarnings = FALSE)
      if (!writable(p)) stop("No writable R library. Set R_LIBS_USER to a folder you own and retry.", call. = FALSE)
      p
    }
  }
  .libPaths(unique(c(lib, .libPaths())))
  say("Library: %s", lib)
  # 2. repository: keep a configured one (internal mirror), else CRAN
  if (is.null(repos)) {
    repos <- getOption("repos")
    if (!length(repos) || any(repos %in% c("@CRAN@", ""))) repos <- c(CRAN = "https://cloud.r-project.org")
  }
  options(repos = repos, install.packages.compile.from.source = "never", timeout = max(300, getOption("timeout")))
  bin <- if (.Platform$OS.type == "windows" || Sys.info()[["sysname"]] == "Darwin") "binary" else "source"
  # 3. downloads: default method, then the Windows internet settings (proxy / certificates)
  retry <- function(f) {
    r <- tryCatch(f(), error = function(e) e, warning = function(w) w)
    if (!inherits(r, "condition")) return(r)
    if (.Platform$OS.type != "windows") stop(conditionMessage(r), call. = FALSE)
    say("  download failed (%s); retrying with the Windows internet settings", conditionMessage(r))
    old <- options(download.file.method = "wininet"); on.exit(options(old))
    f()
  }
  inst <- function(pk) {
    if (requireNamespace(pk, quietly = TRUE)) return(TRUE)
    say("Installing %s ...", pk)
    ok <- tryCatch({
      retry(function() utils::install.packages(pk, lib = lib, type = bin, quiet = TRUE))
      requireNamespace(pk, quietly = TRUE)
    }, error = function(e) { say("  %s", conditionMessage(e)); FALSE })
    if (!ok && bin == "binary") ok <- tryCatch({                    # no binary for this R version
      utils::install.packages(pk, lib = lib, type = "source", quiet = TRUE); requireNamespace(pk, quietly = TRUE)
    }, error = function(e) FALSE)
    ok
  }
  req <- c("jsonlite", "haven", "zip")
  miss <- req[!vapply(req, inst, TRUE)]
  if (length(miss)) stop("Could not install required packages: ", paste(miss, collapse = ", "),
                         ". Ask IT to allow downloads from ", paste(repos, collapse = ", "),
                         " or install them from your internal repository, then rerun.", call. = FALSE)
  opt <- vapply(optional, inst, TRUE)
  if (any(!opt)) say("Optional packages not installed (SuSoSimulate works without them): %s.%s",
                     paste(optional[!opt], collapse = ", "),
                     if (any(c("shiny", "bslib", "DT") %in% optional[!opt])) " The Shiny app needs shiny, bslib and DT; suso_mock() and suso_mock_interactive() do not." else "")
  # 4. the package
  src <- source
  if (is.null(src)) {
    # public archive on github.com: no API call, no token (a stale GITHUB_PAT breaks remotes)
    url <- sprintf("https://github.com/emawbgit/SuSoSimulate/archive/%s.tar.gz", ref)
    src <- tempfile(fileext = ".tar.gz")
    say("Downloading SuSoSimulate (%s) from GitHub ...", ref)
    got <- tryCatch({ retry(function() utils::download.file(url, src, mode = "wb", quiet = TRUE)); TRUE },
                    error = function(e) { say("  %s", conditionMessage(e)); FALSE },
                    warning = function(w) { say("  %s", conditionMessage(w)); FALSE })
    if (!got || !file.exists(src) || file.size(src) < 1000)
      stop("Could not download SuSoSimulate from GitHub (blocked by the network, or no branch/tag '", ref, "').\n",
           "  Download https://github.com/emawbgit/SuSoSimulate > Code > Download ZIP in the browser, then ",
           "install_susosimulate(\"<path to SuSoSimulate-main.zip>\").", call. = FALSE)
  }
  src <- normalizePath(src, mustWork = TRUE)
  if (grepl("\\.(zip|tar\\.gz|tgz)$", src, ignore.case = TRUE)) {
    ex <- tempfile("susosim"); dir.create(ex)                       # GitHub archives hold one top folder
    if (grepl("\\.zip$", src, ignore.case = TRUE)) utils::unzip(src, exdir = ex) else utils::untar(src, exdir = ex)
    src <- dirname(list.files(ex, "^DESCRIPTION$", recursive = TRUE, full.names = TRUE)[1])
  }
  say("Installing SuSoSimulate from %s ...", src)
  utils::install.packages(src, lib = lib, repos = NULL, type = "source", quiet = TRUE)
  if (!requireNamespace("SuSoSimulate", quietly = TRUE)) stop("SuSoSimulate did not install; see the messages above.", call. = FALSE)
  say("Done: SuSoSimulate %s. Try: library(SuSoSimulate); suso_app()  (or suso_mock_interactive())",
      as.character(utils::packageVersion("SuSoSimulate", lib.loc = lib)))
  invisible(TRUE)
}

if (!interactive() && sys.nframe() == 0L) {                         # Rscript install_SuSoSimulate.R [path]
  a <- commandArgs(trailingOnly = TRUE)
  install_susosimulate(if (length(a)) a[1] else NULL)
} else message("Run install_susosimulate() (from GitHub) or install_susosimulate(\"<downloaded zip / tar.gz / folder>\").")
