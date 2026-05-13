# Monte Carlo Error (MCE) nach Koehler, Brown & Haneuse (2009)
# MCE = SD der HzP-Quote über m=200 unabhängige Läufe je Stichtag,
# gemittelt über alle 12 Stichtage (2024–2035)
# N = Stichprobengröße (Personen je Lauf): 1.000, 5.000, 10.000

library(tidyverse)

DATA_DIR <- "C:/Users/tmamantova/Desktop/DAK Sozialhilfe Modell/Bachelorarbeit/Modellergebnisse/200 runs with 1k, 5k, 10k"

STICHTAGE <- paste0(2024:2035, "-07-01")
N_SIZES   <- c(1000L, 5000L, 10000L)

# ── Daten einlesen ────────────────────────────────────────────────────────────
records <- list()

for (N in N_SIZES) {
  files <- sort(list.files(DATA_DIR,
                           pattern = sprintf("output_GR_BA_n%d_lauf.*\\.txt", N),
                           full.names = TRUE))
  cat(sprintf("N=%6d: %d Dateien\n", N, length(files)))

  for (fpath in files) {
    lauf  <- as.integer(sub(".*lauf(\\d+).*", "\\1", basename(fpath)))
    lines <- readLines(fpath, encoding = "UTF-8", warn = FALSE)

    st_idx  <- grep("=== Sozialhilfe \\(HzP\\) am Stichtag \\d{4}-\\d{2}-\\d{2} ===", lines)
    st_vals <- regmatches(lines[st_idx], regexpr("\\d{4}-\\d{2}-\\d{2}", lines[st_idx]))

    for (k in seq_along(st_idx)) {
      st <- st_vals[k]
      if (!st %in% STICHTAGE) next

      end_idx   <- if (k < length(st_idx)) st_idx[k + 1L] - 1L else length(lines)
      block     <- lines[(st_idx[k] + 1L):end_idx]
      quote_hit <- grep(">>> HzP-Quote:\\s+[0-9.]+\\s*%", block, value = TRUE, perl = TRUE)
      if (length(quote_hit) == 0L) next

      quote_val <- as.numeric(regmatches(quote_hit[1L],
                                         regexpr("[0-9]+\\.?[0-9]*", quote_hit[1L])))
      records[[length(records) + 1L]] <- list(N = N, lauf = lauf,
                                               stichtag = st, hzp_quote = quote_val)
    }
  }
}

df <- bind_rows(records)
cat(sprintf("\nGesamt: %d Beobachtungen\n", nrow(df)))
print(df |> group_by(N) |> summarise(m_Laeufe = n_distinct(lauf), .groups = "drop"))

# ── MCE je N und Stichtag, gemittelt über alle Stichtage ─────────────────────
mce_df <- df |>
  group_by(N, stichtag) |>
  summarise(m = n(), MCE = sd(hzp_quote), .groups = "drop") |>
  filter(m > 1L)

mce_mean <- mce_df |>
  group_by(N) |>
  summarise(m_Laeufe = mean(m) |> round() |> as.integer(),
            MCE_min  = min(MCE),
            MCE_mean = mean(MCE),
            MCE_max  = max(MCE),
            .groups  = "drop")

cat("\nMCE je N (Min / Mittelwert / Max über alle Stichtage):\n")
print(mce_mean |> mutate(across(starts_with("MCE"), \(x) round(x, 3))))
