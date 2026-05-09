# Generiert:
#   1. Fortschreibung_E+V_BA_GR_n5000_template.R
#        - liest lauf_nr als Kommandozeilenargument
#        - seed = as.integer(stichtag) + lauf_nr  (individuell je Lauf)
#        - Dateinamen: ..._GR_BA_n5000_lauf{NNNN}.csv / .txt
#   2. run_BA_GR_n5000_5000runs.sh
#        - n_ref=5.000 Laeufe in 100 Batches a 50 parallel
#        - Aufruf: Rscript template.R <lauf_nr>

SCRIPT_DIR <- "c:/Users/tmamantova/Desktop/DAK Sozialhilfe Modell/Bachelorarbeit/WATT Scripts"
SOURCE     <- file.path(SCRIPT_DIR, "Fortschreibung E+V_BA_GR_optimiert_lauf01.R")
TEMPLATE   <- file.path(SCRIPT_DIR, "Fortschreibung_E+V_BA_GR_n5000_template.R")
SHELL      <- file.path(SCRIPT_DIR, "run_BA_GR_n5000_5000runs.sh")

TOTAL_RUNS <- 5000L  # n_ref: Anzahl der durchzuführenden Modellläufe
BATCH_SIZE <- 50L

# ── 1. Quell-Script einlesen ──────────────────────────────────────────────────
code <- paste(readLines(SOURCE, encoding = "UTF-8", warn = FALSE), collapse = "\n")

# ── 2. Ersetzungen ────────────────────────────────────────────────────────────

# 2a. lauf_nr + lauf_tag nach ergebnis_pfad-Zeile einfügen
inject <- 'lauf_nr <- as.integer(commandArgs(trailingOnly = TRUE)[1])\nlauf_tag <- sprintf("%04d", lauf_nr)\n'
code <- gsub(
  'ergebnis_pfad <- "/home/.samba/homes/tmamontova/Bachelorarbeit/Ergebnisse/"\n',
  paste0('ergebnis_pfad <- "/home/.samba/homes/tmamontova/Bachelorarbeit/Ergebnisse/"\n', inject),
  code, fixed = TRUE
)

# 2b. sink – Output-Dateiname mit lauf_tag
code <- gsub(
  'sink(paste0(ergebnis_pfad, "output_GR_BA_optimiert_lauf01.txt"), split = TRUE)',
  'sink(paste0(ergebnis_pfad, "output_GR_BA_n5000_lauf", lauf_tag, ".txt"), split = TRUE)',
  code, fixed = TRUE
)

# 2c. set.seed mit lauf_nr-Offset
code <- gsub(
  'set.seed(as.integer(stichtag) + 0)  # Lauf 01: reproduzierbarer Seed',
  'set.seed(as.integer(stichtag) + lauf_nr)  # Seed variiert per Lauf und Stichtag',
  code, fixed = TRUE
)

# 2d. write_csv Detailergebnisse
code <- gsub(
  'write_csv(results, paste0(ergebnis_pfad, "simulation_ergebnisse_", format(stichtag, "%Y%m%d"), "_GR_BA_optimiert_lauf01.csv"))',
  'write_csv(results, paste0(ergebnis_pfad, "simulation_ergebnisse_", format(stichtag, "%Y%m%d"), "_GR_BA_n5000_lauf", lauf_tag, ".csv"))',
  code, fixed = TRUE
)

# 2e. cat-Bestätigung je Stichtag
code <- gsub(
  'cat("Ergebnisse gespeichert in:", paste0("simulation_ergebnisse_", format(stichtag, "%Y%m%d"), "_GR_BA_optimiert_lauf01.csv\\n"))',
  'cat("Ergebnisse gespeichert in:", paste0("simulation_ergebnisse_", format(stichtag, "%Y%m%d"), "_GR_BA_n5000_lauf", lauf_tag, ".csv\\n"))',
  code, fixed = TRUE
)

# 2f. Zusammenfassung Export
code <- gsub(
  'write_csv(alle_ergebnisse, paste0(ergebnis_pfad, "zusammenfassung_GR_BA_optimiert_lauf01.csv"))',
  'write_csv(alle_ergebnisse, paste0(ergebnis_pfad, "zusammenfassung_GR_BA_n5000_lauf", lauf_tag, ".csv"))',
  code, fixed = TRUE
)
code <- gsub(
  'cat("Zusammenfassung gespeichert in: zusammenfassung_GR_BA_optimiert_lauf01.csv\\n")',
  'cat("Zusammenfassung gespeichert in: zusammenfassung_GR_BA_n5000_lauf", lauf_tag, ".csv\\n")',
  code, fixed = TRUE
)

# 2g. Per-Stichtag Detail-Export-Loop
code <- gsub(
  'filename <- paste0(ergebnis_pfad, "simulation_ergebnisse_", gsub("-", "", stichtag_name), "_GR_BA_optimiert_lauf01.csv")',
  'filename <- paste0(ergebnis_pfad, "simulation_ergebnisse_", gsub("-", "", stichtag_name), "_GR_BA_n5000_lauf", lauf_tag, ".csv")',
  code, fixed = TRUE
)

# ── 3. Prüfen ob noch "optimiert_lauf01" übrig ───────────────────────────────
remaining <- which(grepl("optimiert_lauf01", strsplit(code, "\n")[[1L]]))
if (length(remaining) > 0L) {
  cat(sprintf("WARNUNG: 'optimiert_lauf01' noch in Zeilen %s\n", paste(remaining, collapse = ", ")))
} else {
  cat("OK: Alle 'optimiert_lauf01'-Vorkommen ersetzt.\n")
}

# ── 4. Template schreiben ─────────────────────────────────────────────────────
writeLines(code, TEMPLATE)
cat(sprintf("Template geschrieben: %s\n", TEMPLATE))
cat(sprintf("  Zeilen: %d\n", length(strsplit(code, "\n")[[1L]])))

# ── 5. Shell-Script schreiben ─────────────────────────────────────────────────
shell_lines <- c(
  "#!/bin/bash",
  'SCRIPTDIR="/home/.samba/homes/tmamontova/Bachelorarbeit/Input/R runs"',
  'TEMPLATE="Fortschreibung_E+V_BA_GR_n5000_template.R"',
  'cd "$SCRIPTDIR"',
  "",
  sprintf("TOTAL=%d", TOTAL_RUNS),
  sprintf("BATCH_SIZE=%d", BATCH_SIZE),
  "count=0",
  "",
  sprintf("for lauf in $(seq 1 %d); do", TOTAL_RUNS),
  '  Rscript "$TEMPLATE" $lauf &',
  "  count=$((count + 1))",
  "  if [ $count -ge $BATCH_SIZE ]; then",
  "    wait",
  "    count=0",
  sprintf('    echo "Batch abgeschlossen, Lauf $lauf / %d"', TOTAL_RUNS),
  "  fi",
  "done",
  "",
  "wait",
  sprintf('echo "Alle %d Laeufe abgeschlossen."', TOTAL_RUNS)
)

writeLines(shell_lines, SHELL)
cat(sprintf("Shell-Script geschrieben: %s\n", SHELL))
cat(sprintf("\nStruktur:\n  n_ref=%d Laeufe total, %d Batches a %d parallel\n",
            TOTAL_RUNS, TOTAL_RUNS %/% BATCH_SIZE, BATCH_SIZE))
cat(sprintf("  Seeds: set.seed(as.integer(stichtag) + lauf_nr)  [lauf_nr = 1 .. %d]\n", TOTAL_RUNS))
cat(sprintf("  Dateinamen: zusammenfassung_GR_BA_n5000_lauf%s.csv .. lauf%04d.csv\n",
            "0001", TOTAL_RUNS))
