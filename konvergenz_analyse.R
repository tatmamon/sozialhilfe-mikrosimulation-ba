# Konvergenzanalyse der HzP-Quote – Perzentil-Stabilität & SNV-basiertes KI
# Empirisches KI: [q2.5; q97.5] über kumulative Läufe
# SNV-basiertes KI: mean ± 1.96 × SD  (θ̂ ± C × √V(θ̂), McClelland)
# Kriterium: max. Änderung ≤ 0,001 zwischen aufeinanderfolgenden Prüfpunkten
# Prüfpunkte: alle 50 Läufe bis n=5000

library(tidyverse)
library(writexl)

DATA_DIR  <- paste0("C:/Users/tmamantova/Desktop/DAK Sozialhilfe Modell/",
                    "Bachelorarbeit/Modellergebnisse/5000 runs with 5k")
OUT_DIR   <- paste0("C:/Users/tmamantova/Desktop/DAK Sozialhilfe Modell/",
                    "Bachelorarbeit/Modellergebnisse/Konvergenzanalyse_5k")

STICHTAGE <- paste0(2024:2035, "-07-01")
SCHRITT   <- 50L

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


# ── Stabilitätsprüfung: ab welchem n ist die Änderung ≤ 0,001 (3. NK)? ────────
# Kriterium: für alle aufeinanderfolgenden Prüfpunkte ab n* gilt
#   |p025(n+50) - p025(n)| ≤ 0,001  UND  |p975(n+50) - p975(n)| ≤ 0,001
cat("\n── Stabilität (max. Änderung ≤ 0,001 zwischen Prüfpunkten) ──\n")

stab <- perc |>
  group_by(stichtag) |>
  arrange(n) |>
  mutate(
    delta_lo = abs(p025 - lag(p025)),
    delta_hi = abs(p975 - lag(p975)),
    ok = (delta_lo <= 0.001) & (delta_hi <= 0.001)
  ) |>
  filter(!is.na(ok)) |>
  summarise(
    n_stabil = {
      last_bad <- max(c(0L, n[!ok]))
      n[n > last_bad][1L]
    },
    lo_n5000 = sprintf("%.3f", p025[n == max(n)]),
    hi_n5000 = sprintf("%.3f", p975[n == max(n)]),
    .groups = "drop"
  )

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
cat("\n── SNV-Stabilität (max. Änderung ≤ 0,001 zwischen Prüfpunkten) ──\n")

stab_snv <- perc |>
  group_by(stichtag) |>
  arrange(n) |>
  mutate(
    delta_lo = abs(snv_lo - lag(snv_lo)),
    delta_hi = abs(snv_hi - lag(snv_hi)),
    ok = (delta_lo <= 0.001) & (delta_hi <= 0.001)
  ) |>
  filter(!is.na(ok)) |>
  summarise(
    n_stabil = {
      last_bad <- max(c(0L, n[!ok]))
      n[n > last_bad][1L]
    },
    lo_n5000 = sprintf("%.3f", snv_lo[n == max(n)]),
    hi_n5000 = sprintf("%.3f", snv_hi[n == max(n)]),
    .groups = "drop"
  )

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


# ── Konvergenzplot: Stichtage 2024 und 2035, n ≤ 1000 ────────────────────────
library(ggtext)

# Legende-Labels (HTML für ggtext)
lbl_perc <- "Perzentil [θ̂<sub>2,5</sub>; θ̂<sub>97,5</sub>]"
lbl_snv  <- "auf der SNV basierte (θ̂ ± 1,96·SD)"

plot_data <- perc |>
  filter(n <= 1000, stichtag %in% c("2024-07-01", "2035-07-01")) |>
  mutate(stichtag_label = ifelse(stichtag == "2024-07-01",
                                 "Stichjahr 2024", "Stichjahr 2035"))

plot_long <- bind_rows(
  plot_data |> transmute(n, stichtag_label, methode = lbl_perc,
                         lo = p025, hi = p975),
  plot_data |> transmute(n, stichtag_label, methode = lbl_snv,
                         lo = snv_lo, hi = snv_hi)
)

mw_data  <- plot_data |> select(n, stichtag_label, mw)
mw_label <- mw_data |> filter(n == min(n))
n_stab_perc <- max(stab$n_stabil,     na.rm = TRUE)
n_stab_snv  <- max(stab_snv$n_stabil, na.rm = TRUE)

farben <- setNames(c("#2c5f8a", "#e07b39"), c(lbl_perc, lbl_snv))

caption_txt <- paste0(
  "Vertikale Linien: <i>n_stab</i> für auf der SNV basierte ",
  "(gepunktet) und Perzentil-Konfidenzintervalle (gestrichelt)"
)

p <- ggplot(plot_long, aes(x = n)) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = methode), alpha = 0.25) +
  geom_line(aes(y = lo, color = methode), linewidth = 0.7) +
  geom_line(aes(y = hi, color = methode), linewidth = 0.7) +
  geom_line(data = mw_data, aes(y = mw), color = "black",
            linewidth = 0.6) +
  geom_text(data = mw_label, aes(x = n, y = mw, label = "MW"),
            hjust = 1.2, vjust = -0.3, size = 2.8,
            family = "Arial", color = "black") +
  geom_vline(xintercept = n_stab_snv,  linetype = "dotted",
             color = "#e07b39", linewidth = 0.7) +
  geom_vline(xintercept = n_stab_perc, linetype = "dashed",
             color = "#2c5f8a", linewidth = 0.7) +
  facet_wrap(~stichtag_label, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 1000, 200)) +
  scale_y_continuous(labels = \(x) paste0(round(x * 100, 1), " %")) +
  scale_fill_manual(values  = farben) +
  scale_color_manual(values = farben) +
  labs(
    x       = "Anzahl Simulationsläufe (<i>n</i>)",
    y       = "HzP-Quote",
    fill    = "Konfidenzintervalle",
    color   = "Konfidenzintervalle",
    caption = caption_txt
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 11, base_family = "Arial") +
  theme(
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold", family = "Arial"),
    plot.caption     = element_markdown(size = 8, color = "gray40",
                                        family = "Arial"),
    axis.title.x     = element_markdown(family = "Arial"),
    legend.text      = element_markdown(family = "Arial"),
    legend.title     = element_text(family = "Arial"),
    axis.text        = element_text(family = "Arial"),
    axis.title.y     = element_text(family = "Arial")
  )

plot_path <- file.path(OUT_DIR, "hzp_konvergenz_plot.png")
ggsave(plot_path, plot = p, width = 16, height = 8, units = "cm", dpi = 300)
cat(sprintf("Plot gespeichert: %s\n", plot_path))

# Daten für separate Plots speichern
saveRDS(list(perc = perc, stab = stab, stab_snv = stab_snv),
        file.path(OUT_DIR, "konvergenz_daten.rds"))
cat(sprintf("RDS gespeichert: %s\n", file.path(OUT_DIR, "konvergenz_daten.rds")))
