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
  Mittelwert = gsub("\\.", ",", sprintf("Kalibrierungsfaktor = 35,64 / 38,13 = %.6f", KALIB)),
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
