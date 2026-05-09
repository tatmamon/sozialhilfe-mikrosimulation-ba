# Generiert 600 BA-Scripts für Stichprobengrößen-Sensitivitätsanalyse:
#   - N ∈ {1.000, 5.000, 10.000} (Stichprobengröße je Lauf)
#   - je m=200 Läufe
#   - Stichtage 2024-2035 (ohne 2021-2023)
#   - Seed laufspezifisch: set.seed(as.integer(stichtag) + offset)

BASE    <- "c:/Users/tmamantova/Desktop/DAK Sozialhilfe Modell/10x10K_WATT_Scripts/Fortschreibung E+V_GeltendesRecht_v4_lauf01.R"
OUT_DIR <- "c:/Users/tmamantova/Desktop/DAK Sozialhilfe Modell/Bachelorarbeit/WATT Scripts"

base  <- paste(readLines(BASE, encoding = "UTF-8", warn = FALSE), collapse = "\n")
count <- 0L

for (N in c(1000L, 5000L, 10000L)) {
  for (lauf in 1:200) {
    s      <- sprintf("%03d", lauf)
    offset <- (lauf - 1L) * 100L
    code   <- base

    # 1. Personen-Export entfernen
    code <- gsub(
      'write_csv\\(personen, paste0\\(ergebnis_pfad, "personen_gesamt_lauf01\\.csv"\\)\\)\ncat\\("Personen-Tabelle gespeichert in: personen_gesamt_lauf01\\.csv\\\\n\\\\n"\\)\n',
      "", code, perl = TRUE
    )

    # 2. Zweiten Write-Loop entfernen (DOTALL: (?s))
    code <- gsub(
      paste0("(?s)# Detaillierte Ergebnisse für jeden Stichtag\n",
             "for \\(stichtag_name in names\\(alle_results_detail\\)\\) \\{.*?\\}\n",
             'cat\\("Detaillierte Ergebnisse für jeden Stichtag gespeichert\\.\\\\n"\\)\n'),
      "# Detaillierte Ergebnisse wurden bereits pro Stichtag gespeichert (kein doppelter Export)\n",
      code, perl = TRUE
    )

    # 3. Stichtage 2021-2023 entfernen
    code <- gsub(
      'stichtage <- as.Date(c("2021-07-01", "2022-07-01", "2023-07-01", "2024-07-01",',
      'stichtage <- as.Date(c("2024-07-01",',
      code, fixed = TRUE
    )

    # 4. Kommentare aktualisieren
    code <- gsub("# HAUPTSCHLEIFE: 15 STICHTAGE (2021-2035)",
                 "# HAUPTSCHLEIFE: 12 STICHTAGE (2024-2035)", code, fixed = TRUE)
    code <- gsub("Geltendes Recht v4: Stichtage 2021-2035, Lauf 01",
                 sprintf("Geltendes Recht v4 BA (N=%d): Stichtage 2024-2035, Lauf %s", N, s),
                 code, fixed = TRUE)
    code <- gsub("# ZUSAMMENFASSUNG ALLER STICHTAGE (2022-2027)",
                 "# ZUSAMMENFASSUNG ALLER STICHTAGE (2024-2035)", code, fixed = TRUE)
    code <- gsub("# Zielgröße: 50.000 Personen (großer Lauf)",
                 sprintf("# Zielgröße: %s Personen (Bootstrap aus N=5.000 SOEP-Stichprobe)",
                         format(N, big.mark = ".", scientific = FALSE)),
                 code, fixed = TRUE)

    # 5. Pfade auf Bachelorarbeit-Ordner
    code <- gsub('basepfad <- "/home/.samba/homes/tmamontova/Input/"',
                 'basepfad <- "/home/.samba/homes/tmamontova/Bachelorarbeit/Input/"',
                 code, fixed = TRUE)
    code <- gsub('ergebnis_pfad <- "/home/.samba/homes/tmamontova/Ergebnisse/"',
                 'ergebnis_pfad <- "/home/.samba/homes/tmamontova/Bachelorarbeit/Ergebnisse/"',
                 code, fixed = TRUE)

    # 6. Stichprobengröße N setzen
    code <- gsub("ziel_n     <- 10000",
                 sprintf("ziel_n     <- %d", N), code, fixed = TRUE)

    # 7. Seed laufspezifisch
    code <- gsub(
      "set.seed(as.integer(stichtag) + 0)  # Gleicher Seed pro Stichtag → identische Population über alle Szenarien",
      sprintf("set.seed(as.integer(stichtag) + %dL)  # Lauf %s: reproduzierbarer, laufspezifischer Seed", offset, s),
      code, fixed = TRUE
    )

    # 8. Ausgabedateinamen
    code <- gsub("GeltendesRecht_v4_lauf01",
                 sprintf("GR_BA_n%d_lauf%s", N, s), code, fixed = TRUE)

    # 9. Zeitmessung nach sink() einfügen
    sink_line <- sprintf('sink(paste0(ergebnis_pfad, "output_GR_BA_n%d_lauf%s.txt"), split = TRUE)', N, s)
    code <- gsub(sink_line,
                 paste0(sink_line,
                        '\nstart_time <- Sys.time()\ncat("=== Start:", format(start_time, "%Y-%m-%d %H:%M:%S"), "===\\n\\n")\n'),
                 code, fixed = TRUE)

    # 10. Laufzeit vor sink() am Ende
    code <- gsub(
      'cat("\\n=== ALLE SIMULATIONEN BEENDET ===\\n")\nsink()',
      paste0('cat("\\n=== ALLE SIMULATIONEN BEENDET ===\\n")\n',
             'cat(sprintf("=== Laufzeit: %.1f Minuten ===\\n", as.numeric(difftime(Sys.time(), start_time, units = "mins"))))\n',
             'sink()'),
      code, fixed = TRUE
    )

    out <- file.path(OUT_DIR, sprintf("Fortschreibung E+V_BA_GR_n%d_lauf%s.R", N, s))
    writeLines(code, out)

    count <- count + 1L
    if (count %% 100L == 0L) cat(sprintf("  %d/600 Scripts generiert ...\n", count))
  }
}

cat(sprintf("\nFertig: 600 Scripts in %s\n", OUT_DIR))

# ── Shell-Script ─────────────────────────────────────────────────────────────
shell_lines <- c(
  "#!/bin/bash",
  'SCRIPTDIR="/home/.samba/homes/tmamontova/Bachelorarbeit/Input/R runs"',
  'ERGEBNIS="/home/.samba/homes/tmamontova/Bachelorarbeit/Ergebnisse"',
  'cd "$SCRIPTDIR"',
  "",
  "BATCH_SIZE=50",
  "count=0",
  "",
  "for n_var in 1000 5000 10000; do",
  "  for lauf in $(seq -w 001 200); do",
  '    SCRIPT="Fortschreibung E+V_BA_GR_n${n_var}_lauf${lauf}.R"',
  '    if [ -f "$SCRIPT" ]; then',
  '      Rscript "$SCRIPT" &',
  "      count=$((count + 1))",
  "      if [ $count -ge $BATCH_SIZE ]; then",
  "        wait",
  "        count=0",
  "      fi",
  "    fi",
  "  done",
  "done",
  "",
  "wait",
  'echo "Alle 600 BA GR Laeufe abgeschlossen."'
)

shell_path <- file.path(OUT_DIR, "run_BA_GR_600.sh")
writeLines(shell_lines, shell_path)
cat(sprintf("Shell-Script gespeichert: %s\n", shell_path))
cat(sprintf("\nStruktur: m=200 Laeufe je N, 3 Stichprobengroessen -> 600 Laeufe total\n"))
cat(sprintf("Seeds: set.seed(as.integer(stichtag) + offset)  [offset = 0, 100, ..., 19900]\n"))
