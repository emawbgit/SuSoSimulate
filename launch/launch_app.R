# Started by SuSoSimulate_app.bat (Windows) / SuSoSimulate_app.command (macOS): installs what
# is missing (see ../install_SuSoSimulate.R), then opens the app in the browser.
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
here <- if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[1]))) else getwd()
root <- dirname(here)
need <- c("SuSoSimulate", "shiny", "bslib", "DT")
if (!all(vapply(need, requireNamespace, TRUE, quietly = TRUE))) {
  source(file.path(root, "install_SuSoSimulate.R"))
  install_susosimulate(if (file.exists(file.path(root, "DESCRIPTION"))) root else NULL,
                       optional = c("readxl", "shiny", "bslib", "DT"))
}
SuSoSimulate::suso_app(launch.browser = TRUE)
