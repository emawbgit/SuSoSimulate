#!/bin/bash
# Double-click (macOS) to open the SuSoSimulate app. First time: right-click > Open, or
# run `chmod +x SuSoSimulate_app.command` if macOS says it cannot be opened.
cd "$(dirname "$0")" || exit 1
RS="$(command -v Rscript)"
for p in /usr/local/bin/Rscript /opt/homebrew/bin/Rscript /Library/Frameworks/R.framework/Resources/bin/Rscript; do
  [ -z "$RS" ] && [ -x "$p" ] && RS="$p"
done
if [ -z "$RS" ]; then echo "R was not found. Install it from https://cloud.r-project.org (or Self Service)."; read -r -p "Press Enter to close"; exit 1; fi
"$RS" launch_app.R || read -r -p "Press Enter to close"
