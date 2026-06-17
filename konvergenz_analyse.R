# Konvergenzanalyse der HzP-Quote – Perzentil-Stabilität & SNV-basiertes KI
# Empirisches KI: [q2.5; q97.5] über kumulative Läufe
# SNV-basiertes KI: mean ± 1.96 × SD  (θ̂ ± C × √V(θ̂), McClelland)
# Kriterium: |Δ| < 0,1 PP UND gleicher Ganzzahlanteil in % für K=5 Folge-Prüfpunkte (Hoad et al. 2010)
# Prüfpunkte: alle 50 Läufe bis n=5000

library(tidyverse)
library(writexl)

DATA_DIR  <- paste0("C:/Users/tmamantova/Desktop/DAK Sozialhilfe Modell/",
                    "Bachelorarbeit/Modellergebnisse/5000 runs with 5k")
OUT_DIR   <- paste0("C:/Users/tmamantova/Desktop/DAK Sozialhilfe Modell/",
                    "Bachelorarbeit/Modellergebnisse/Konvergenzanalyse_5k")

STICHTAGE   <- paste0(2024:2035, "-07-01")
SCHRITT     <- 50L
DELTA_THR   <- 0.001  # Schwellenwert: |Δ| < 0,001 (= 0,1 PP als Proportion)
K_CONFIRM   <- 5L     # Bestätigungsfenster (Hoad et al. 2010)

# ── Daten einlesen ────────────────────────────────────────────────────────────
files <- sort(list.files(DATA_DIR,
                         pattern = "output_GR_BA_n5000_lauf\\d+\\.txt",
                         full.names = TRUE))
cat(sprintf("%d Dateien gefunden\n", length(files)))

records  <- vector("list", length(files) * length(STICHTAGE))
laufzeit <- numeric(length(files))   # Minuten je Lauf
idx <- 0L

for (i in seq_along(files)) {
  fpath <- files[[i]]
  lauf  <- as.integer(sub(".*lauf(\\d+).*", "\\1", basename(fpath)))
  lines <- readLines(fpath, encoding = "UTF-8", warn = FALSE)

  # Laufzeit extrahieren
  lz_hit <- grep("=== Laufzeit:\\s+[0-9.]+\\s+Minuten ===", lines, value = TRUE)
  laufzeit[[i]] <- if (length(lz_hit) > 0L)
    as.numeric(regmatches(lz_hit[1L], regexpr("[0-9]+\\.?[0-9]*", lz_hit[1L])))
  else NA_real_

  st_idx  <- grep("=== Sozialhilfe \\(HzP\\) am Stichtag \\d{4}-\\d{2}-\\d{2} ===",
                  lines)
  st_vals <- regmatches(lines[st_idx],
                        regexpr("\\d{4}-\\d{2}-\\d{2}", lines[st_idx]))

  for (k in seq_along(st_idx)) {
    st <- st_vals[k]
    if (!st %in% STICHTAGE) next
    end_idx   <- if (k < length(st_idx)) st_idx[k + 1L] - 1L else length(lines)
    block     <- lines[(st_idx[k] + 1L):end_idx]
    hit <- grep(">>> HzP-Quote:\\s+[0-9.]+\\s*%", block, value = TRUE, perl = TRUE)
    if (length(hit) == 0L) next
    val <- as.numeric(regmatches(hit[1L], regexpr("[0-9]+\\.?[0-9]*", hit[1L])))
    idx <- idx + 1L
    records[[idx]] <- list(lauf = lauf, stichtag = st, hzp = val / 100)
  }
}

# Laufzeiten nach Laufnummer sortiert (Files sind bereits sortiert)
lauf_nrs <- as.integer(sub(".*lauf(\\d+).*", "\\1", basename(files)))
lz_df <- tibble(lauf = lauf_nrs, laufzeit_min = laufzeit) |>
  arrange(lauf)

df <- bind_rows(records[seq_len(idx)]) |> arrange(lauf, stichtag)
n_max <- max(df$lauf)
cat(sprintf("Läufe eingelesen: 1 bis %d\n\n", n_max))

# ── Kumulative Statistiken an Prüfpunkten ─────────────────────────────────────
checkpoints <- seq(SCHRITT, n_max, by = SCHRITT)

perc <- map_dfr(checkpoints, function(n) {
  df |>
    filter(lauf <= n) |>
    group_by(stichtag) |>
    summarise(n      = n,
              p025   = quantile(hzp, 0.025),
              p975   = quantile(hzp, 0.975),
              mw     = mean(hzp),
              sd_hzp = sd(hzp),
              .groups = "drop") |>
    mutate(snv_lo = mw - 1.96 * sd_hzp,
           snv_hi = mw + 1.96 * sd_hzp)
})

# ── Wanduhrzeit je Prüfpunkt (50 Läufe parallel, Batch-Max) ──────────────────
BATCH_SIZE <- 50L

wanduhrzeit <- map_dbl(checkpoints, function(n) {
  laeufe_bis_n <- lz_df |> filter(lauf <= n)
  batch_id     <- ceiling(laeufe_bis_n$lauf / BATCH_SIZE)
  laeufe_bis_n |>
    mutate(batch = batch_id) |>
    group_by(batch) |>
    summarise(batch_max = max(laufzeit_min, na.rm = TRUE), .groups = "drop") |>
    pull(batch_max) |>
    sum()
})

wanduhr_df <- tibble(n = checkpoints, wanduhrzeit_h = round(wanduhrzeit / 60, 2))

# ── Tabelle: Zeilen = Prüfpunkte, Spalten = Stichtag × Perzentil ─────────────
tabelle_lo <- perc |>
  select(n, stichtag, p025) |>
  pivot_wider(names_from = stichtag, values_from = p025,
              names_prefix = "lo_")

tabelle_hi <- perc |>
  select(n, stichtag, p975) |>
  pivot_wider(names_from = stichtag, values_from = p975,
              names_prefix = "hi_")

tabelle <- left_join(tabelle_lo, tabelle_hi, by = "n") |>
  left_join(wanduhr_df, by = "n")

# Spalten sortieren: n, Wanduhrzeit, dann je Stichtag lo + hi
st_cols <- as.vector(rbind(paste0("lo_", STICHTAGE), paste0("hi_", STICHTAGE)))
tabelle  <- tabelle |>
  select(n, wanduhrzeit_h, all_of(st_cols)) |>
  mutate(wanduhrzeit_h = sprintf("%.2f", wanduhrzeit_h),
         across(all_of(st_cols), \(x) sprintf("%.6f", x)))

cat("HzP-Konvergenztabelle (p025 = lo_, p975 = hi_, Dezimal):\n\n")
print(tabelle, n = Inf)


# ── Stabilitätsfunktion: Fenster-vom-Referenzpunkt (Hoad et al. 2010) ────────
# n_stab = erstes n_v[i], für das gilt: ALLE K nachfolgenden Prüfpunkte haben
# denselben Ganzzahlanteil in % UND weichen um < DELTA_THR ab (lo UND hi)
stab_run <- function(df, lo_col, hi_col) {
  map_dfr(unique(df$stichtag), function(st) {
    sub  <- df |> filter(stichtag == st) |> arrange(n)
    lo_v <- sub[[lo_col]]
    hi_v <- sub[[hi_col]]
    n_v  <- sub$n
    m    <- length(n_v)
    result <- NA_integer_
    for (i in seq_len(max(0L, m - K_CONFIRM))) {
      refs <- (i + 1L):(i + K_CONFIRM)   # K Folge-Indizes nach Referenz i
      if (all(abs(lo_v[refs] - lo_v[i]) < DELTA_THR) &&
          all(floor(lo_v[refs] * 100) == floor(lo_v[i] * 100)) &&
          all(abs(hi_v[refs] - hi_v[i]) < DELTA_THR) &&
          all(floor(hi_v[refs] * 100) == floor(hi_v[i] * 100))) {
        result <- n_v[i]; break
      }
    }
    tibble(stichtag = st,
           n_stabil = result,
           lo_n5000 = sprintf("%.3f", lo_v[m]),
           hi_n5000 = sprintf("%.3f", hi_v[m]))
  })
}

cat("\n── Stabilität Perzentil (|Δ| < 0,001, K=5 Bestätigungen) ──\n")
stab <- stab_run(perc, "p025", "p975")
print(stab)


# ── SNV-basiertes KI: mean ± 1.96 × SD  (θ̂ ± C × √V(θ̂)) ───────────────────
tabelle_snv_lo <- perc |>
  select(n, stichtag, snv_lo) |>
  pivot_wider(names_from = stichtag, values_from = snv_lo,
              names_prefix = "lo_")

tabelle_snv_hi <- perc |>
  select(n, stichtag, snv_hi) |>
  pivot_wider(names_from = stichtag, values_from = snv_hi,
              names_prefix = "hi_")

tabelle_snv <- left_join(tabelle_snv_lo, tabelle_snv_hi, by = "n") |>
  left_join(wanduhr_df, by = "n") |>
  select(n, wanduhrzeit_h, all_of(st_cols)) |>
  mutate(wanduhrzeit_h = sprintf("%.2f", wanduhrzeit_h),
         across(all_of(st_cols), \(x) sprintf("%.6f", x)))

cat("\nSNV-Konvergenztabelle (mean ± 1.96·SD, lo_ / hi_, Dezimal):\n\n")
print(tabelle_snv, n = Inf)

# ── SNV-Stabilitätsprüfung ────────────────────────────────────────────────────
cat("\n── Stabilität SNV (Δ ≤ 0,001, K=5 Bestätigungen) ──\n")
stab_snv <- stab_run(perc, "snv_lo", "snv_hi")
print(stab_snv)


# ── Excel speichern (vier Sheets in einer Datei) ──────────────────────────────
xlsx_path <- file.path(OUT_DIR, "hzp_konvergenz_50er.xlsx")
tryCatch(
  {
    # Mittelwert-Sheet: n, Wanduhrzeit, mw_ je Stichtag
    tabelle_mw <- perc |>
      select(n, stichtag, mw) |>
      pivot_wider(names_from = stichtag, values_from = mw,
                  names_prefix = "mw_") |>
      left_join(wanduhr_df, by = "n")
    mw_cols <- paste0("mw_", STICHTAGE)
    tabelle_mw <- tabelle_mw |>
      select(n, wanduhrzeit_h, all_of(mw_cols)) |>
      mutate(wanduhrzeit_h = sprintf("%.2f", wanduhrzeit_h),
             across(all_of(mw_cols), \(x) sprintf("%.6f", x)))

    write_xlsx(
      list(
        Konvergenztabelle     = tabelle,
        Stabilitaet           = stab,
        Konvergenztabelle_SNV = tabelle_snv,
        Stabilitaet_SNV       = stab_snv,
        Mittelwerte           = tabelle_mw
      ),
      path = xlsx_path
    )
    cat(sprintf("\nExcel gespeichert: %s\n", xlsx_path))
  },
  error = function(e) cat(sprintf(
    "\nExcel NICHT gespeichert (Datei evtl. noch in Excel geöffnet): %s\n",
    conditionMessage(e)))
)


# ── Konvergenzplot: alle Stichjahre, n ≤ 1000 ────────────────────────────────
library(ggtext)

lbl_perc <- "Perzentil [θ̂<sub>2,5</sub>; θ̂<sub>97,5</sub>]"
lbl_snv  <- "auf der SNV basierte (θ̂ ± 1,96·SD)"
lbl_mw   <- "Mittelwert"

plot_data <- perc |>
  filter(n <= 1000) |>
  mutate(stichjahr = substr(stichtag, 1, 4))

plot_long <- bind_rows(
  plot_data |> transmute(n, stichjahr, methode = lbl_perc, lo = p025, hi = p975),
  plot_data |> transmute(n, stichjahr, methode = lbl_snv,  lo = snv_lo, hi = snv_hi)
)

mw_data <- plot_data |>
  select(n, stichjahr, mw) |>
  mutate(methode = lbl_mw)

vline_perc <- stab     |> mutate(stichjahr = substr(stichtag, 1, 4))
vline_snv  <- stab_snv |> mutate(stichjahr = substr(stichtag, 1, 4))

farben <- setNames(c("#2c5f8a", "#e07b39"), c(lbl_perc, lbl_snv))

caption_txt <- paste0(
  "Vertikale Linien: <i>n_stab</i> für auf der SNV basierte ",
  "(gepunktet) und Perzentil-Konfidenzintervalle (gestrichelt)"
)

p <- ggplot(plot_long, aes(x = n)) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = methode), alpha = 0.25) +
  geom_line(aes(y = lo, color = methode), linewidth = 0.6) +
  geom_line(aes(y = hi, color = methode), linewidth = 0.6) +
  geom_line(data = mw_data, aes(y = mw, linetype = methode),
            color = "black", linewidth = 0.5) +
  geom_vline(data = vline_snv,  aes(xintercept = n_stabil),
             linetype = "dotted", color = "#e07b39", linewidth = 0.6) +
  geom_vline(data = vline_perc, aes(xintercept = n_stabil),
             linetype = "dashed", color = "#2c5f8a", linewidth = 0.6) +
  facet_wrap(~stichjahr, scales = "free_y", ncol = 4) +
  scale_x_continuous(breaks = c(200, 600, 1000)) +
  scale_y_continuous(labels = \(x) paste0(round(x * 100, 1), " %")) +
  scale_fill_manual(values  = farben, name = "Konfidenzintervalle") +
  scale_color_manual(values = farben, name = "Konfidenzintervalle") +
  scale_linetype_manual(values = setNames("solid", lbl_mw), name = NULL) +
  guides(
    linetype = guide_legend(order = 1),
    fill     = guide_legend(order = 2),
    color    = guide_legend(order = 2)
  ) +
  labs(
    x       = "Anzahl Simulationsläufe (<i>n</i>)",
    y       = "HzP-Quote",
    caption = caption_txt
  ) +
  theme_minimal(base_size = 9, base_family = "Arial") +
  theme(
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold", family = "Arial"),
    plot.caption     = element_markdown(size = 7, color = "gray40", family = "Arial"),
    axis.title.x     = element_markdown(family = "Arial"),
    legend.text      = element_markdown(family = "Arial"),
    legend.title     = element_text(family = "Arial"),
    axis.text        = element_text(family = "Arial", size = 7),
    axis.title.y     = element_text(family = "Arial"),
    panel.spacing    = unit(0.4, "cm")
  )

plot_path <- file.path(OUT_DIR, "hzp_konvergenz_plot.png")
ggsave(plot_path, plot = p, width = 22, height = 18, units = "cm", dpi = 300)
cat(sprintf("Plot gespeichert: %s\n", plot_path))

# Daten für separate Plots speichern
saveRDS(list(perc = perc, stab = stab, stab_snv = stab_snv),
        file.path(OUT_DIR, "konvergenz_daten.rds"))
cat(sprintf("RDS gespeichert: %s\n", file.path(OUT_DIR, "konvergenz_daten.rds")))


# ── Reduzierte Konvergenztabelle ──────────────────────────────────────────────
n_sel_red <- c(seq(50, 650, 50), 700, 750, 1000, 2000, 3000, 4000, 5000)

tbl_red <- perc |>
  filter(n %in% n_sel_red, stichtag %in% STICHTAGE) |>
  mutate(
    jahr   = substr(stichtag, 1, 4),
    snv_s  = gsub("\\.", ",", sprintf("[%.2f; %.2f]", snv_lo * 100, snv_hi * 100)),
    perc_s = gsub("\\.", ",", sprintf("[%.2f; %.2f]", p025   * 100, p975   * 100))
  ) |>
  select(n, jahr, snv_s, perc_s) |>
  pivot_wider(names_from  = jahr,
              values_from = c(snv_s, perc_s),
              names_glue  = "{.value}_{jahr}") |>
  left_join(wanduhr_df |> rename(laufzeit_h = wanduhrzeit_h), by = "n") |>
  select(n, laufzeit_h,
         all_of(as.vector(rbind(paste0("snv_s_",  2024:2035),
                                paste0("perc_s_", 2024:2035)))))

tbl_red_snv <- tbl_red |>
  select(n, laufzeit_h, all_of(paste0("snv_s_",  2024:2035))) |>
  rename_with(~ sub("^snv_s_",  "SNV-KI ",       .x), starts_with("snv_s_")) |>
  rename("Laufzeit (h)" = laufzeit_h)

tbl_red_perc <- tbl_red |>
  select(n, laufzeit_h, all_of(paste0("perc_s_", 2024:2035))) |>
  rename_with(~ sub("^perc_s_", "Perzentil-KI ", .x), starts_with("perc_s_")) |>
  rename("Laufzeit (h)" = laufzeit_h)

# KI bei jahresspezifischem n_stab
ki_per_year <- function(stab_df, lo_col, hi_col) {
  stab_df |>
    select(stichtag, n_stabil) |>
    left_join(perc |> select(stichtag, n, all_of(c(lo_col, hi_col))),
              by = c("stichtag", "n_stabil" = "n")) |>
    mutate(
      jahr = substr(stichtag, 1, 4),
      ki_s = gsub("\\.", ",", sprintf("[%.2f; %.2f]",
                                     .data[[lo_col]] * 100,
                                     .data[[hi_col]] * 100))
    ) |>
    select(jahr, n_stabil, ki_s)
}

tbl_nstab <- ki_per_year(stab_snv, "snv_lo", "snv_hi") |>
  rename("n_stab (SNV)" = n_stabil, "SNV-KI" = ki_s) |>
  left_join(
    ki_per_year(stab, "p025", "p975") |>
      rename("n_stab (Perc)" = n_stabil, "Perzentil-KI" = ki_s),
    by = "jahr"
  ) |>
  rename(Jahr = jahr)

red_path <- file.path(OUT_DIR, "hzp_konvergenz_tabelle_reduziert.xlsx")
tryCatch(
  {
    write_xlsx(list(SNV       = tbl_red_snv,
                    Perzentil = tbl_red_perc,
                    nstab_KI  = tbl_nstab),
               path = red_path)
    cat(sprintf("Reduzierte Tabelle gespeichert: %s\n", red_path))
  },
  error = function(e) cat(sprintf(
    "\nReduzierte Tabelle NICHT gespeichert (Datei evtl. geöffnet): %s\n",
    conditionMessage(e)))
)


# ── Finale Tabelle: Perzentil-KI & Mittelwert bei einheitlichem n_stab=500 ───
# (konservativster Stichtag-spezifischer n_stab-Wert aus tbl_nstab)
N_STAB_FIX <- 500L
KALIB       <- 35.64 / 38.13   # Kalibrierungsfaktor (Ist-Quote / Simulations-MW 2024)

tbl_final <- perc |>
  filter(n == N_STAB_FIX) |>
  mutate(
    Jahr      = substr(stichtag, 1, 4),
    # erst auf 4 NKS runden, dann kalibrieren
    mw_r      = round(mw,   4),
    lo_r      = round(p025, 4),
    hi_r      = round(p975, 4),
    Mittelwert = round(mw_r * KALIB, 4),
    lo_k       = round(lo_r * KALIB, 4),
    hi_k       = round(hi_r * KALIB, 4),
    KI_unten   = round(Mittelwert - lo_k, 4),   # Abstand nach unten
    KI_oben    = round(hi_k - Mittelwert, 4)    # Abstand nach oben
  ) |>
  arrange(Jahr) |>
  select(Jahr, Mittelwert, KI_unten, KI_oben)

hinweis <- data.frame(
  Jahr       = "Hinweis:",
  Mittelwert = sprintf("Kalibrierungsfaktor = 35,64 / 38,13 = %.6f", KALIB),
  KI_unten   = "Abstand Mittelwert - KI_lo (kalibriert, in PP)",
  KI_oben    = "Abstand KI_hi - Mittelwert (kalibriert, in PP)"
)

final_path <- file.path(OUT_DIR, "hzp_perc_ki_mittelwerte.xlsx")
tryCatch(
  {
    library(openxlsx)
    wb <- createWorkbook()
    sname <- sprintf("n_stab = %d", N_STAB_FIX)
    addWorksheet(wb, sname)
    # Hinweis in Zeile 1
    writeData(wb, sname, as.data.frame(t(unlist(hinweis))), startRow = 1, colNames = FALSE)
    # Daten ab Zeile 2 mit Spaltennamen
    writeData(wb, sname, as.data.frame(tbl_final), startRow = 2, colNames = TRUE)
    saveWorkbook(wb, final_path, overwrite = TRUE)
    cat(sprintf("\nFinale Tabelle (n_stab=%d) gespeichert: %s\n", N_STAB_FIX, final_path))
  },
  error = function(e) cat(sprintf(
    "\nFinale Tabelle NICHT gespeichert (Datei evtl. geöffnet): %s\n",
    conditionMessage(e)))
)
library(haven)
library(arrow)
library(dplyr)
library(fs)
library(sjlabelled)
library(sjmisc)
library(stringr)
library(readr)

#Datasets einlesen
#Funktion: einmalige Konvertierung CSV → Parquet
convert_csv <- function(infile, outdir,
                        sep = ",",
                        dec = ".",
                        encoding = "Latin1") {
  
  basename <- path_ext_remove(path_file(infile))
  outfile  <- file.path(outdir, paste0(basename, ".parquet"))
  
  if (file_exists(outfile)) {
    message("Schon vorhanden: ", outfile)
    return(outfile)
  }
  
  message("Konvertiere: ", infile)
  
  df <- read.csv(
    infile,
    sep = sep,
    dec = dec,
    header = TRUE,
    stringsAsFactors = FALSE,
    fileEncoding = encoding
  )
  
  write_parquet(df, outfile)
  rm(df); gc()
  
  return(outfile)
}

#Ordner definieren
raw_dir     <- "C:/Users/tmamantova/Desktop/DAK Sozialhilfe Modell/SOEP v40/soepdata"
parquet_dir <- file.path(raw_dir, "parquet")

dir_create(parquet_dir)

#Liste aller Dateien
datasets_csv <- c(
  "pequiv.csv",
  "hbrutto.csv",
  "ppathl.csv",
  "hl_2023.csv",
  "hwealth.csv",
  "pwealth.csv",
  "hgen.csv"
)

#Vollständige .csv-Pfade
csv_paths <- file.path(raw_dir, datasets_csv)

# Konvertieren: CSV → Parquet
parquet_paths <- sapply(csv_paths, convert_csv, outdir = parquet_dir)

#Arrow-Datasets bereitstellen
Input_pequiv  <- open_dataset(file.path(parquet_dir, "pequiv.parquet"))
Input_hbrutto <- open_dataset(file.path(parquet_dir, "hbrutto.parquet"))
Input_ppathl  <- open_dataset(file.path(parquet_dir, "ppathl.parquet"))
Input_hl_2023 <- open_dataset(file.path(parquet_dir, "hl_2023.parquet"))
Input_hwealth     <- open_dataset(file.path(parquet_dir, "hwealth.parquet"))
Input_pwealth     <- open_dataset(file.path(parquet_dir, "pwealth.parquet"))
Input_hgen        <- open_dataset(file.path(parquet_dir, "hgen.parquet"))

#Jahr 2023 filtern
pequiv_2023c <- Input_pequiv  %>%
  filter(syear == 2023) %>%
  collect()

#Variablennamen ändern
pequiv_2023c <- pequiv_2023c %>%
  rename(Geschlecht = d11102ll,
         Alter = d11101,
         Beziehung_zum_Haushaltsvorstand = d11105,
         Personen_im_Haushalt = d11106
  )

#Bundesland hinzufügen
bula <- Input_hbrutto %>%
  select (bula_v2) %>%
  collect()
hid <- Input_hbrutto %>%
  select (hid) %>%
  collect()
syear <- Input_hbrutto %>%
  select (syear) %>%
  collect()
bula <- cbind(bula, hid, syear)
bula <- as.data.frame(bula)
bula <- bula %>%
  rename (bula = bula_v2)
pequiv_2023c <- left_join(pequiv_2023c, bula, by = c("hid","syear"))
pequiv_2023c$bula <- set_labels(pequiv_2023c$bula, labels = c("SH" = 1, "HH" = 2, "NI" = 3, "HB" = 4, "NW" = 5, "HE" = 6, "RP" = 7, "BW" = 8, "BY" = 9, "SL" = 10, "BE" = 11, "BB" = 12, "MV" = 13, "SN" = 14, "ST" = 15, "TH" = 16))
rm(bula)
rm(hid)
rm(syear)

#Geburtsjahr hinzufügen
Geburtsjahr <- Input_ppathl %>%
  select (gebjahr) %>%
  collect()
pid <- Input_ppathl %>%
  select (pid) %>%
  collect()
syear <- Input_ppathl %>%
  select (syear) %>%
  collect()
Geburtsjahr <- cbind(Geburtsjahr, pid, syear)
Geburtsjahr <- as.data.frame(Geburtsjahr)
pequiv_2023c <- left_join(pequiv_2023c, Geburtsjahr, by = c("pid","syear"))
pequiv_2023c <- pequiv_2023c %>% move_columns(gebjahr, .after = "syear")
rm(Geburtsjahr)
rm(pid)
rm(syear)

# Sicherstellen, dass das Geburtsjahr nicht in der Zukunft liegt
pequiv_2023c <- pequiv_2023c %>%
  filter(gebjahr < 2024)


# -1 als NA für relevante demografische Variablen (Alter, Geschlecht, Geburtsjahr) definieren
pequiv_2023c <- pequiv_2023c %>%
  mutate(
    Alter = na_if(Alter, -1),
    Geschlecht = na_if(Geschlecht, -1),
    gebjahr = na_if(gebjahr, -1)
  )

#Altersintervalle hinzufügen
pequiv_2023c <- pequiv_2023c %>% 
  mutate(Altersintervalle = cut(Alter, breaks = c(-Inf, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf), labels = c("unter 5", "5 bis unter 10", "10 bis unter 15", "15 bis unter 20", "20 bis unter 25", "25 bis unter 30", "30 bis unter 35", "35 bis unter 40", "40 bis unter 45", "45 bis unter 50", "50 bis unter 55", "55 bis unter 60", "60 bis unter 65", "65 bis unter 70", "70 bis unter 75", "75 bis unter 80", "80 bis unter 85", "85 bis unter 90","90 und älter")))
pequiv_2023c <- pequiv_2023c %>% 
  move_columns(Altersintervalle, .after = "Alter")

#Region hinzufügen
pequiv_2023c <- pequiv_2023c %>% 
  mutate(Region = ifelse(bula == 1, "Norden", ifelse(bula == 2, "Norden", ifelse(bula == 3, "Norden", ifelse(bula == 4, "Norden", ifelse(bula == 5, "NRW", ifelse(bula == 6, "Mitteldeutschland", ifelse(bula == 7, "Mitteldeutschland", ifelse(bula == 8, "Süden", ifelse(bula == 9, "Süden", ifelse(bula == 10, "Mitteldeutschland", ifelse(bula == 11, "Osten", ifelse(bula == 12, "Osten", ifelse(bula == 13, "Osten", ifelse(bula == 14, "Osten", ifelse(bula == 15, "Osten", ifelse(bula == 16, "Osten", "Rest")))))))))))))))))
pequiv_2023c <- pequiv_2023c %>% 
  mutate(Region_2 = ifelse(bula == 1, "SH_HH", ifelse(bula == 2, "SH_HH", ifelse(bula == 3, "NI_HB", ifelse(bula == 4, "NI_HB", ifelse(bula == 5, "NRW", ifelse(bula == 6, "HE", ifelse(bula == 7, "RP_SL", ifelse(bula == 8, "BW", ifelse(bula == 9, "BY", ifelse(bula == 10, "RP_SL", ifelse(bula == 11, "BB_BE", ifelse(bula == 12, "BB_BE", ifelse(bula == 13, "MV", ifelse(bula == 14, "SN", ifelse(bula == 15, "ST", ifelse(bula == 16, "TH", "Rest")))))))))))))))))
pequiv_2023c <- pequiv_2023c %>% 
  mutate(Region_4 = ifelse(bula == 1, "SH_HH", ifelse(bula == 2, "SH_HH", ifelse(bula == 3, "NI_HB", ifelse(bula == 4, "NI_HB", ifelse(bula == 5, "NRW", ifelse(bula == 6, "HE", ifelse(bula == 7, "RP_SL", ifelse(bula == 8, "BW", ifelse(bula == 9, "BY", ifelse(bula == 10, "RP_SL", ifelse(bula == 11, "BB_BE", ifelse(bula == 12, "BB_BE", ifelse(bula == 13, "Osten", ifelse(bula == 14, "Osten", ifelse(bula == 15, "Osten", ifelse(bula == 16, "Osten", "Rest")))))))))))))))))
pequiv_2023c <- pequiv_2023c %>% 
  mutate(Region_3 = ifelse(bula == 1, "Norden", ifelse(bula == 2, "Norden", ifelse(bula == 3, "Norden", ifelse(bula == 4, "Norden", ifelse(bula == 5, "NRW", ifelse(bula == 6, "Mitteldeutschland", ifelse(bula == 7, "Mitteldeutschland", ifelse(bula == 8, "BW", ifelse(bula == 9, "BY", ifelse(bula == 10, "Mitteldeutschland", ifelse(bula == 11, "Osten", ifelse(bula == 12, "Osten", ifelse(bula == 13, "Osten", ifelse(bula == 14, "Osten", ifelse(bula == 15, "Osten", ifelse(bula == 16, "Osten", "Rest")))))))))))))))))

#Heimeintrittsrisiko hinzufügen 
pequiv_2023c <- pequiv_2023c %>% 
  mutate(Heimeintrittsrisiko = case_when((bula == 1 & Geschlecht == 1 & Alter > 64 & Alter < 80) ~ 0.01717, (bula == 2 & Geschlecht == 1 & Alter > 64 & Alter < 80) ~ 0.01540, (bula == 3 & Geschlecht == 1 & Alter > 64 & Alter < 80) ~ 0.01593, (bula == 4 & Geschlecht == 1 & Alter > 64 & Alter < 80) ~ 0.01375, (bula == 5 & Geschlecht == 1 & Alter > 64 & Alter < 80) ~ 0.01273, (bula == 6 & Geschlecht == 1 & Alter > 64 & Alter < 80) ~ 0.01192, (bula == 7 & Geschlecht == 1 & Alter > 64 & Alter < 80) ~ 0.01154, (bula == 8 & Geschlecht == 1 & Alter > 64 & Alter < 80) ~ 0.01262, (bula == 9 & Geschlecht == 1 & Alter > 64 & Alter < 80) ~ 0.01317, (bula == 10 & Geschlecht == 1 & Alter > 64 & Alter < 80) ~ 0.01581, (bula == 11 & Geschlecht == 1 & Alter > 64 & Alter < 80) ~ 0.01441, (bula == 12 & Geschlecht == 1 & Alter > 64 & Alter < 80) ~ 0.01137, (bula == 13 & Geschlecht == 1 & Alter > 64 & Alter < 80) ~ 0.01430, (bula == 14 & Geschlecht == 1 & Alter > 64 & Alter < 80) ~ 0.01336, (bula == 15 & Geschlecht == 1 & Alter > 64 & Alter < 80) ~ 0.01482, (bula == 16 & Geschlecht == 1 & Alter > 64 & Alter < 80) ~ 0.01311, (bula == 1 & Geschlecht == 2 & Alter > 64 & Alter < 80) ~ 0.01919, (bula == 2 & Geschlecht == 2 & Alter > 64 & Alter < 80) ~ 0.01670, (bula == 3 & Geschlecht == 2 & Alter > 64 & Alter < 80) ~ 0.01780, (bula == 4 & Geschlecht == 2 & Alter > 64 & Alter < 80) ~ 0.01541, (bula == 5 & Geschlecht == 2 & Alter > 64 & Alter < 80) ~ 0.01527, (bula == 6 & Geschlecht == 2 & Alter > 64 & Alter < 80) ~ 0.01436, (bula == 7 & Geschlecht == 2 & Alter > 64 & Alter < 80) ~ 0.01419, (bula == 8 & Geschlecht == 2 & Alter > 64 & Alter < 80) ~ 0.01404, (bula == 9 & Geschlecht == 2 & Alter > 64 & Alter < 80) ~ 0.01429, (bula == 10 & Geschlecht == 2 & Alter > 64 & Alter < 80) ~ 0.01770, (bula == 11 & Geschlecht == 2 & Alter > 64 & Alter < 80) ~ 0.01445, (bula == 12 & Geschlecht == 2 & Alter > 64 & Alter < 80) ~ 0.01337, (bula == 13 & Geschlecht == 2 & Alter > 64 & Alter < 80) ~ 0.01518, (bula == 14 & Geschlecht == 2 & Alter > 64 & Alter < 80) ~ 0.01459, (bula == 15 & Geschlecht == 2 & Alter > 64 & Alter < 80) ~ 0.01675, (bula == 16 & Geschlecht == 2 & Alter > 64 & Alter < 80) ~ 0.01569, (bula == 1 & Geschlecht == 1 & Alter > 79) ~ 0.06106, (bula == 2 & Geschlecht == 1 & Alter > 79) ~ 0.05983, (bula == 3 & Geschlecht == 1 & Alter > 79) ~ 0.06711, (bula == 4 & Geschlecht == 1 & Alter > 79) ~ 0.04844, (bula == 5 & Geschlecht == 1 & Alter > 79) ~ 0.05245, (bula == 6 & Geschlecht == 1 & Alter > 79) ~ 0.05174, (bula == 7 & Geschlecht == 1 & Alter > 79) ~ 0.05124, (bula == 8 & Geschlecht == 1 & Alter > 79) ~ 0.05019, (bula == 9 & Geschlecht == 1 & Alter > 79) ~ 0.05428, (bula == 10 & Geschlecht == 1 & Alter > 79) ~ 0.05295, (bula == 11 & Geschlecht == 1 & Alter > 79) ~ 0.05308, (bula == 12 & Geschlecht == 1 & Alter > 79) ~ 0.05005, (bula == 13 & Geschlecht == 1 & Alter > 79) ~ 0.05686, (bula == 14 & Geschlecht == 1 & Alter > 79) ~ 0.05959, (bula == 15 & Geschlecht == 1 & Alter > 79) ~ 0.06447, (bula == 16 & Geschlecht == 1 & Alter > 79) ~ 0.06073, (bula == 1 & Geschlecht == 2 & Alter > 79) ~ 0.13817, (bula == 2 & Geschlecht == 2 & Alter > 79) ~ 0.12827, (bula == 3 & Geschlecht == 2 & Alter > 79) ~ 0.14727, (bula == 4 & Geschlecht == 2 & Alter > 79) ~ 0.10189, (bula == 5 & Geschlecht == 2 & Alter > 79) ~ 0.12004, (bula == 6 & Geschlecht == 2 & Alter > 79) ~ 0.12123, (bula == 7 & Geschlecht == 2 & Alter > 79) ~ 0.11777, (bula == 8 & Geschlecht == 2 & Alter > 79) ~ 0.11359, (bula == 9 & Geschlecht == 2 & Alter > 79) ~ 0.12096, (bula == 10 & Geschlecht == 2 & Alter > 79) ~ 0.11729, (bula == 11 & Geschlecht == 2 & Alter > 79) ~ 0.11452, (bula == 12 & Geschlecht == 2 & Alter > 79) ~ 0.10905, (bula == 13 & Geschlecht == 2 & Alter > 79) ~ 0.11836, (bula == 14 & Geschlecht == 2 & Alter > 79) ~ 0.12945, (bula == 15 & Geschlecht == 2 & Alter > 79) ~ 0.13693, (bula == 16 & Geschlecht == 2 & Alter > 79) ~ 0.12957))                                                                    
                                                                    
#Fehlende Werte mit NA kennzeichnen
pequiv_2023c <- pequiv_2023c %>%  
  mutate_all(~replace(., . == -2, 0))

#Sample hinzufügen
Sample <- Input_hbrutto %>% 
  select(hid, syear, sample1) %>%
  collect()
pequiv_2023c <- left_join(pequiv_2023c, Sample, by = c("hid", "syear"))
pequiv_2023c <- pequiv_2023c %>% move_columns(sample1, .after = "syear")
rm(Sample)

    #Samples 30, 31 und 34 entfernen (fehlende und unvollständige Einkommensdaten)
    pequiv_2023c <- pequiv_2023c[pequiv_2023c$sample1 != 30, ]
    pequiv_2023c <- pequiv_2023c[pequiv_2023c$sample1 != 31, ]
    pequiv_2023c <- pequiv_2023c[pequiv_2023c$sample1 != 34, ]

#Einkommensvariablen gruppieren

    #Renten
    pequiv_2023c <- pequiv_2023c %>% 
      mutate(Rente = rowSums(select(.,igrv1, iciv1, iguv1, ivbl1, icom1, iprv1, ilib1, iaus1, ison1, igrv2, iciv2, iguv2, ivbl2, icom2, iprv2, iaus2, ilib2, ison2), na.rm = TRUE),
             Rente_monatlich = round(Rente / 12, digits = 0),
             GRente = rowSums(select(.,igrv1, igrv2, ivbl1, ivbl2, iaus1, iaus2), na.rm = TRUE),
             GRente_monatlich = round(GRente / 12, digits = 0),
             BRente = rowSums(select(.,icom1, icom2), na.rm = TRUE),
             BRente_monatlich = round(BRente / 12, digits = 0))
    
    #Arbeitseinkommen
    pequiv_2023c <- pequiv_2023c %>% 
      mutate(Arbeitseinkommen = rowSums(select(.,ijob1, ijob2, iself, i13ly, i14ly, ixmas, iholy, igray, iothy, itray), na.rm = TRUE),
             Arbeitseinkommen_monatlich = round(Arbeitseinkommen / 12, digits = 0),
             Arbeitseinkommen_ab = rowSums(select(.,ijob1, ijob2, isick, i13ly, i14ly, ixmas, iholy, igray, iothy), na.rm = TRUE),
             Arbeitseinkommen_ab_monatlich = round(Arbeitseinkommen_ab / 12, digits = 0),
             Arbeitseinkommen_sf = rowSums(select(.,ijob1, ijob2, iself), na.rm = TRUE),
             Arbeitseinkommen_sf_monatlich = round(Arbeitseinkommen_ab / 12, digits = 0))
    
    #SV-Leistungen und Transfers
    pequiv_2023c <- pequiv_2023c %>% 
      mutate(SVLeistungen_Transfers = rowSums(select(., iunby,	istuy,	ielse), na.rm = TRUE),
             SVLeistungen_Transfers_monatlich = round(SVLeistungen_Transfers / 12, digits = 0))
    
    #Summe der individuellen Einkommen
    pequiv_2023c <- pequiv_2023c %>% 
      mutate(Indiv.Einkommen_Summe = rowSums(select(.,Rente), na.rm = TRUE),
             Indiv.Einkommen_Summe_monatlich = round(Indiv.Einkommen_Summe / 12, digits = 0))
    
    #Haushaltseinkommen
    pequiv_2023c <- pequiv_2023c %>% 
      mutate(renty_bereinigt = renty - opery - lossr)
    pequiv_2023c <- pequiv_2023c %>% 
      move_columns(renty_bereinigt, .after = "renty")
    pequiv_2023c$renty_bereinigt[pequiv_2023c$renty_bereinigt < 0] <- 0
    pequiv_2023c$renty_bereinigt <- round(pequiv_2023c$renty_bereinigt,digit=0)
    
    pequiv_2023c <- pequiv_2023c %>% 
      mutate(divdy_bereinigt = divdy - lossc)
    pequiv_2023c <- pequiv_2023c %>% 
      move_columns(divdy_bereinigt, .after = "divdy")
    pequiv_2023c$divdy_bereinigt[pequiv_2023c$divdy_bereinigt < 0] <- 0
    pequiv_2023c$divdy_bereinigt <- round(pequiv_2023c$divdy_bereinigt,digit=0)
    
    pequiv_2023c <- pequiv_2023c %>% 
      mutate(Haushaltseinkommen = renty_bereinigt + divdy_bereinigt)                                                                              
    pequiv_2023c$Haushaltseinkommen[pequiv_2023c$Haushaltseinkommen < 0] <- 0
    pequiv_2023c <- pequiv_2023c %>% 
      rowwise () %>% mutate(Haushaltseinkommen_monatlich = Haushaltseinkommen/12)
    pequiv_2023c$Haushaltseinkommen_monatlich <- round(pequiv_2023c$Haushaltseinkommen_monatlich,digit=0)

# Haushaltseinkommen aufteilen

Haushaltseinkommen_aufgeteilt <- pequiv_2023c %>% 
  select(hid, pid, Alter, Haushaltseinkommen)

# komplette Daten ohne Kinder Filtern

ohne_kinder <- Haushaltseinkommen_aufgeteilt %>%
  filter(Alter > 18) %>%
  group_by(hid) %>% # Die Daten nach der Haushalts-ID gruppieren
  mutate(
    # distinct(... Haushaltseinkommen) → erster vorhandener Wert
    haushaltseinkommen_einheitlich =
      first(Haushaltseinkommen[!is.na(Haushaltseinkommen)]),
    
    anzahl_erwachsene = n(), # Die Anzahl der Erwachsenen (n()) pro Haushalt (hid) berechnen
    
    Haushaltseinkommen =
      haushaltseinkommen_einheitlich / anzahl_erwachsene # Das ursprüngliche Haushaltseinkommen durch die Anzahl der Erwachsenen teilen
  ) %>%
  ungroup() %>% # Die Gruppierung beenden
  select(hid, pid, Alter, Haushaltseinkommen)

# komplette Daten mit Kindern filtern
kinder <- Haushaltseinkommen_aufgeteilt %>%
  filter(Alter <= 18) %>%
  mutate(Haushaltseinkommen = 0) #  Einkommen aller Kinder auf 0 setzen

# Aufgeteiltes Haushaltseinkommen hinzufügen
Haushaltseinkommen_aufgeteilt <- bind_rows(ohne_kinder, kinder) %>%
  rename(Haushaltseinkommen_aufgeteilt = Haushaltseinkommen) %>%
  mutate(
    Haushaltseinkommen_aufgeteilt =
      round(Haushaltseinkommen_aufgeteilt, 0)
  )
# Join
pequiv_2023c <- left_join(
  pequiv_2023c,
  Haushaltseinkommen_aufgeteilt,
  by = c("pid", "hid", "Alter")
)

# Monatswert
pequiv_2023c <- pequiv_2023c %>%
  mutate(
    Haushaltseinkommen_aufgeteilt_monatlich =
      round(Haushaltseinkommen_aufgeteilt / 12, 0)
  )

rm(ohne_kinder, kinder, Haushaltseinkommen_aufgeteilt)

    # Gesamteinkommen (selbst) wird erst in Fortschreibung E+V aus Einzelkomponenten berechnet:
    # Logik: Rente_monatlich + Haushaltseinkommen_aufgeteilt_monatlich

#Verweigerer entfernen
pequiv_2023c <- pequiv_2023c[!is.na(pequiv_2023c$Alter), ]  
  
#Personen mit fehlendem Geschlecht entfernen
pequiv_2023c <- pequiv_2023c[!is.na(pequiv_2023c$Geschlecht), ]     

#Personen, die 2018 nicht dabei waren, entfernen
pequiv_2018c_1 <- read_csv("C:/Users/tmamantova/Desktop/DAK Sozialhilfe Modell/pequiv_2018c.csv")
# Personen aus 2018 extrahieren
pids_2018 <- unique(pequiv_2018c_1$pid)
# 2023 nur auf diese PIDs filtern
pequiv_2023c <- pequiv_2023c %>%
  filter(pid %in% pids_2018)
rm(pequiv_2018c_1)
rm(pids_2018)

#Wohnkosten

#Daten für Mieter
Mieter_2023 <- Input_hgen %>% 
  filter(syear == 2023) %>%
  select(hid, syear, hgowner, hghinc, hgelectr, hgelectrinfo, hgutil, hgutilinfo, hgheat, hgheatinfo, hgrent, hgrentinfo, hgf2rent) %>%
  collect %>%
  filter (hgowner > 1)  

Sample <- Input_hbrutto %>% 
  select(hid, syear, sample1) %>%
  collect()
Mieter_2023 <- left_join(Mieter_2023, Sample, by = c("hid", "syear"))
Mieter_2023 <- Mieter_2023 %>% 
  move_columns(sample1, .after = "syear")
rm(Sample)
Mieter_2023 <- Mieter_2023[Mieter_2023$sample1 != 30, ] 
Mieter_2023 <- Mieter_2023[Mieter_2023$sample1 != 31, ] 
Mieter_2023 <- Mieter_2023[Mieter_2023$sample1 != 34, ]
Mieter_2023$hgelectr[Mieter_2023$hgelectrinfo == 3] <- 0
Mieter_2023$hgelectr[Mieter_2023$hgrentinfo == 3] <- 0
Mieter_2023$hgutil[Mieter_2023$hgutilinfo == 3] <- 0
Mieter_2023$hgutil[Mieter_2023$hgrentinfo == 3] <- 0
Mieter_2023$hgheat[Mieter_2023$hgheatinfo == 3] <- 0
Mieter_2023$hgheat[Mieter_2023$hgrentinfo == 3] <- 0
Mieter_2023$hgrent[Mieter_2023$hgrentinfo == 3] <- 0
Mieter_2023$hgrent[Mieter_2023$hgrentinfo == 2] <- 0
Mieter_2023$hgelectr[Mieter_2023$hgelectr == -1] <- 0 #oder imputieren? handelt sich um 5er = Heimbewohner
Mieter_2023$hgutil[Mieter_2023$hgutil == -1] <- 0 #oder imputieren? handelt sich um 5er = Heimbewohner
Mieter_2023$hgheat[Mieter_2023$hgheat == -1] <- 0 #oder imputieren? handelt sich um 5er = Heimbewohner
Mieter_2023$hgrent[Mieter_2023$hgrent == -1] <- 0 #oder imputieren? handelt sich um 5er = Heimbewohner
Personen_im_Haushalt <- pequiv_2023c %>% 
  select(hid, syear, Personen_im_Haushalt)
Personen_im_Haushalt <- Personen_im_Haushalt[!duplicated(Personen_im_Haushalt$hid), ]
Mieter_2023 <- left_join(Mieter_2023, Personen_im_Haushalt, by = c("hid", "syear"))
Mieter_2023 <- Mieter_2023 %>% 
  mutate(hgelectr = ifelse(Personen_im_Haushalt > 2, hgelectr, hgelectr * 0.65)) ### Eventuell anpassen!
Mieter_2023 <- Mieter_2023 %>% 
  mutate(hgheat = ifelse(Personen_im_Haushalt > 2, hgheat, hgheat * 0.65)) ### Eventuell anpassen!
Mieter_2023 <- Mieter_2023 %>% 
  mutate(Wohnkosten = rowSums(select(.,hgelectr, hgutil, hgheat, hgrent), na.rm = TRUE))
Mieter_2023 <- Mieter_2023 %>% 
  mutate(Wohnkosten = ifelse(Personen_im_Haushalt > 2, Wohnkosten/(Personen_im_Haushalt-1), Wohnkosten))
Mieter_2023$Wohnkosten <- round(Mieter_2023$Wohnkosten,digit=0)

#Daten für Eigentümer
Eigentümer_2023 <- Input_hl_2023 %>% 
  select(hid, syear, hlf0001_h, hlf0087_h, hlf0088_h, hlf0599, hlf0600, hlf0601, hlf0602, hlf0069_h, hlf0603, hlf0078, hlf0604, hlf0081_h, hlf0605) %>%
  collect() %>%
  filter(hlf0001_h == 3)
  
Sample <- Input_hbrutto %>% 
  select(hid, syear, sample1) %>%
  collect()
Eigentümer_2023 <- left_join(Eigentümer_2023, Sample, by = c("hid", "syear"))
Eigentümer_2023 <- Eigentümer_2023 %>% 
  move_columns(sample1, .after = "syear")
rm(Sample)
Haushaltseinkommen <- Input_hgen %>% 
  select(hid, syear, hghinc) %>%
  collect()
Eigentümer_2023 <- left_join(Eigentümer_2023, Haushaltseinkommen, by = c("hid", "syear"))
Eigentümer_2023 <- Eigentümer_2023 %>% 
  move_columns(hghinc, .after = "hgowner")
rm(Haushaltseinkommen)
Eigentümer_2023$hghinc <- as.numeric(Eigentümer_2023$hghinc)
Eigentümer_2023 <- Eigentümer_2023[Eigentümer_2023$sample1 != 30, ] #um -5er zu entfernen, entspricht nur 2 Personen
Eigentümer_2023 <- Eigentümer_2023[Eigentümer_2023$sample1 != 31, ] #um -5er zu entfernen, entspricht nur 1 Person
Eigentümer_2023 <- Eigentümer_2023 %>%
  rename(hgutil = hlf0081_h)
Eigentümer_2023 <- Eigentümer_2023 %>%
  rename(hgelectr = hlf0078)
Eigentümer_2023 <- Eigentümer_2023 %>%
  rename(Grundsteuer = hlf0601)
Eigentümer_2023 <- Eigentümer_2023 %>%
  rename(hgheat = hlf0069_h)
Eigentümer_2023 <- Eigentümer_2023 %>%
  rename(hgowner = hlf0001_h)
Eigentümer_2023 <- Eigentümer_2023 %>%
  rename(Zins_Tilgung = hlf0088_h)
Eigentümer_2023 <- Eigentümer_2023 %>%
  rename(Instandhaltung_Modernisierung = hlf0600)
Eigentümer_2023$hgelectr[Eigentümer_2023$hlf0604 == 1] <- 0
Eigentümer_2023$Grundsteuer[Eigentümer_2023$hlf0602 == 1] <- 0
Eigentümer_2023$hgheat[Eigentümer_2023$hlf0603 == 1] <- 0
Eigentümer_2023$hgutil[Eigentümer_2023$hlf0605 == 1] <- 0
Eigentümer_2023$Zins_Tilgung[Eigentümer_2023$hlf0087_h == 2] <- 0
Eigentümer_2023$Instandhaltung_Modernisierung[Eigentümer_2023$hlf0599 == 2] <- 0
Eigentümer_2023$hgowner[Eigentümer_2023$hgowner == 3] <- 1
Eigentümer_2023 <- Eigentümer_2023 %>% 
  mutate(across(where(is.numeric),~ ifelse(. < 0, NA, .)))
Eigentümer_2023<- Eigentümer_2023 %>% 
  mutate(Instandhaltung_Modernisierung = (Instandhaltung_Modernisierung / 12*0.5))
Eigentümer_2023$Instandhaltung_Modernisierung <- round(Eigentümer_2023$Instandhaltung_Modernisierung,digit=0)
Eigentümer_2023 <- Eigentümer_2023 %>% 
  mutate(Grundsteuer = Grundsteuer / 12)
Eigentümer_2023$Grundsteuer <- round(Eigentümer_2023$Grundsteuer,digit=0)
Eigentümer_2023 <- Eigentümer_2023 %>% 
  mutate(hgheat = (hgheat * 0.65)) 
Eigentümer_2023$hgheat <- round(Eigentümer_2023$hgheat,digit=0)
Eigentümer_2023 <- Eigentümer_2023 %>% 
  mutate(hgelectr = (hgelectr * 0.65))
Eigentümer_2023$hgelectr  <- round(Eigentümer_2023$hgelectr,digit=0)

#Imputation mit Mittelwert
Eigentümer_2023_Imputation <- Eigentümer_2023 %>% 
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .))

#Aufteilung Wohnkosten 
Eigentümer_2023_Imputation <- Eigentümer_2023_Imputation %>% 
  mutate(Wohnkosten = rowSums(select(., Grundsteuer, Zins_Tilgung, Instandhaltung_Modernisierung, hgelectr, hgutil, hgheat), na.rm = TRUE))
Personen_im_Haushalt <- pequiv_2023c %>% 
  select(hid, syear, Personen_im_Haushalt)
Personen_im_Haushalt <- Personen_im_Haushalt[!duplicated(Personen_im_Haushalt$hid), ]
Eigentümer_2023_Imputation <- left_join(Eigentümer_2023_Imputation, Personen_im_Haushalt, by = c("hid", "syear"))
Eigentümer_2023_Imputation <- Eigentümer_2023_Imputation %>% 
  mutate(Wohnkosten = ifelse(Personen_im_Haushalt > 2, Wohnkosten/(Personen_im_Haushalt-1), Wohnkosten))
Eigentümer_2023_Imputation$Wohnkosten <- round(Eigentümer_2023_Imputation$Wohnkosten,digit=0)

#Daten für Mieter und Eigentümer zusammenfügen
Mieter_2023_Join <- Mieter_2023 %>% 
  select(hid, syear, Wohnkosten)
Eigentümer_2023_Join <- Eigentümer_2023_Imputation %>% 
  select(hid, syear, Wohnkosten)
Wohnkosten_2023 <- rbind(Mieter_2023_Join, Eigentümer_2023_Join)
rm(Eigentümer_2023)
rm(Eigentümer_2023_Imputation)
rm(Eigentümer_2023_Join)
rm(Mieter_2023)
rm(Mieter_2023_Join)
rm(Personen_im_Haushalt)

#Join mit pequiv
pequiv_2023c <- left_join(pequiv_2023c, Wohnkosten_2023, by = c("hid","syear"))
pequiv_2023c$Wohnkosten <- round(pequiv_2023c$Wohnkosten, digit=0)
rm(Wohnkosten_2023)


#Altersintervalle_Alte
pequiv_2023c <- pequiv_2023c %>% 
  mutate(Altersintervalle_Alte = cut(Alter, breaks = c(-Inf, 79, Inf), labels = c( "65 bis unter 80", "80 und älter")))
pequiv_2023c <- pequiv_2023c %>% 
  move_columns(Altersintervalle_Alte, .after = "Alter")


#Alle nötigen Variablen selektieren
pequiv_2023c_Export <- pequiv_2023c %>%
  select(pid, hid, bula, syear, Geschlecht, Alter, Altersintervalle_Alte,
         Beziehung_zum_Haushaltsvorstand, w11105, e11102, d11104, iciv1,
         Rente_monatlich, Haushaltseinkommen_aufgeteilt_monatlich,
         SVLeistungen_Transfers_monatlich,
         GRente_monatlich, BRente_monatlich,
         Arbeitseinkommen_monatlich,
         Wohnkosten,
         Arbeitseinkommen_ab_monatlich, Arbeitseinkommen_sf_monatlich)
write.csv(pequiv_2023c_Export ,"C:/Users/tmamantova/Desktop/DAK Sozialhilfe Modell/pequiv_2023c.csv", row.names = FALSE, fileEncoding = "UTF-8")

rm(pequiv_2023c_Export) 
