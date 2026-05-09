# ---- Schritt 1: Vorbereitung der Datensätze ----

### SOEP-Datensätze einlesen
library(haven)
library(arrow)
library(dplyr)
library(fs)
library(sjlabelled)
library(sjmisc)
library(stringr)

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
raw_dir     <- "/home/.samba/homes/tmamontova/Input/"
parquet_dir <- file.path(raw_dir, "parquet")

dir_create(parquet_dir)

#Liste aller Dateien
datasets_csv <- c(
  "pl_2018.csv",
  "pl_2023.csv",
  "pgen.csv"
)

#Vollständige .csv-Pfade
csv_paths <- file.path(raw_dir, datasets_csv)

#Konvertieren: CSV → Parquet
parquet_paths <- sapply(csv_paths, convert_csv, outdir = parquet_dir)

#Arrow-Datasets bereitstellen
Input_pl_2018 <- open_dataset(file.path(parquet_dir, "pl_2018.parquet"))
Input_pl_2023 <- open_dataset(file.path(parquet_dir, "pl_2023.parquet"))
Input_pgen        <- open_dataset(file.path(parquet_dir, "pgen.parquet"))
rm(csv_paths, datasets_csv, parquet_dir, parquet_paths, raw_dir, convert_csv)

### Personendatensatz einlesen
library(readr)
personen <- read_csv("/home/.samba/homes/tmamontova/Input/pequiv_2018c.csv")
names(personen) <- tolower(names(personen))

#Stichprobengröße festlegen
ziel_total <- 781923

#Mapping bula -> bundesland (Reihenfolge aus dem SOEP)
bula_map <- data.frame(
  bula = 1:16,
  bundesland = c(
    "Schleswig-Holstein", "Hamburg", "Niedersachsen", "Bremen",
    "Nordrhein-Westfalen", "Hessen", "Rheinland-Pfalz", "Baden-Württemberg",
    "Bayern", "Saarland", "Berlin", "Brandenburg",
    "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Thüringen"
  ), stringsAsFactors = FALSE
)
personen <- merge(personen, bula_map, by = "bula", all.x = TRUE)
rm(bula_map)

# ---- Schritt 2: Einkommens- und Vermögenswerte auf 2022 projizieren ----

### Die Tabelle mit den Inflationsraten einlesen
#Quelle: https://www-genesis.destatis.de/genesis/online?operation=result&code=61111-0001
#Achtung: Die Tabelle selbst muss herauskopiert werden, da die csv-Datei zusammen mit den Hinweisen runtergeladen wird 
inflation_csv <- "/home/.samba/homes/tmamontova/Input/Allgemeine Inflationsrate zum Einlesen.csv"
library(readr)
library(dplyr)
inflation <- read_csv2(inflation_csv, trim_ws = TRUE)
colnames(inflation) <- c(              # Spaltennamen angepasst 
  "jahr",
  "verbraucherpreisindex",
  "veränderung"
)
inflation <- inflation[-1,]            # Die erste Zeile gelöscht (bereits in den neuen Spaltennamen enthalten)
inflation <- inflation |>              # Komma zu Punkt, Zahlenformat
  mutate(
    jahr = as.numeric(jahr),
    veränderung = as.numeric(gsub(",", ".", veränderung)) / 100
  )
rm(inflation_csv)

### Die Tabelle mit den Rentenanpassungen einlesen
#Quelle: https://www.deutsche-rentenversicherung.de/DRV/DE/Ueber-uns-und-Presse/Presse/Meldungen/2025/250306-rentenanpassung-2025.html#_b20pxq3yi
#Achtung: Die Tabelle muss bei Bedarf herauskopiert werden, da es keine Version zum Herunterladen vorhanden ist
rente_csv <- "/home/.samba/homes/tmamontova/Input/Rentenanpassungen.csv"
library(readr)
rente <- read_csv2(rente_csv, trim_ws = TRUE)
rente <- rente %>%
  mutate(
    jahr = as.numeric(gsub("[^0-9]", "", `Rentenanpassung zum 01.07.`)), # Spaltennamen angepasst + Komma zu Punkt, Zahlenformat
    west = as.numeric(gsub(",", ".",`West (in Prozent)`)) / 100,
    ost  = as.numeric(gsub(",", ".", `Ost (in Prozent)`)) / 100
  ) %>%
  select(jahr, west, ost)
rm(rente_csv)

### Die Tabelle mit den Häuserpreisindexen einlesen
#Quelle: https://www-genesis.destatis.de/genesis/online?sequenz=statistikTabellen&selectionname=61262#abreadcrumb (Häuserpreisindex, Preisindex für Bauland: Deutschland, Jahre)
#Aktualisiert mit der Schätzung für 2025
#Achtung: Die Tabelle selbst muss herauskopiert werden, da die csv-Datei zusammen mit den Hinweisen runtergeladen wird 
library(readr)
library(dplyr)
library(tidyr)
haus_csv <- "/home/.samba/homes/tmamontova/Input/Häuserpreisindex zum Einlesen.csv"
haus_raw <- read_csv2(haus_csv, trim_ws = TRUE, col_types = cols(.default = "c")) # Alles als Text einlesen, um Typkonflikte zu vermeiden

#Die Zeilen bereinigen und irrelevante Zeilen löschen
haus <- haus_raw %>%
  filter(grepl("Häuserpreisindex", .[[1]], ignore.case = TRUE)) %>%
  select(-1, -2)  # Meta-Spalten ("Häuserpreisindex", "2015=100") löschen

#Komma zu Punkt in allen Spalten ersetzen und numerisch machen
haus <- haus %>%
  mutate(across(everything(), ~ as.numeric(gsub(",", ".", .x))))

#Spaltennamen bereinigen und numerisch umwandeln ins Longformat (Jahr in den Zeilen und nicht in den Spalten)
haus <- haus %>%
  pivot_longer(
    cols = everything(),
    names_to = "jahr",
    values_to = "hausindex"
  ) %>%
  mutate(
    jahr = as.numeric(jahr),
    hausindex = as.numeric(gsub(",", ".", hausindex))
  ) %>%
  arrange(jahr)
rm(haus_raw, haus_csv)

### Die Tabelle mit den vorausberechteten Rentenanpassungen einlesen
#Quelle: https://www.bmas.de/DE/Service/Presse/Pressemitteilungen/2023/bundeskabinett-beschliesst-rentenversicherungsbericht-2023.html (S.46)
#Achtung: Die Daten müssen händisch herauskopiert werden, weil sie aus einer pdf-Datei stammen
rentezukunft_csv <- "/home/.samba/homes/tmamontova/Input/Rentenanpassungen prospektiv.csv"
library(readr)
rentezukunft <- read_csv2(rentezukunft_csv, trim_ws = TRUE)
names(rentezukunft) <- tolower(names(rentezukunft)) # Einheitlich klein geschrieben
rentezukunft <- rentezukunft %>%
  mutate(
    jahr = as.numeric(jahr),
    anpassungssatz = as.numeric(gsub(",", ".", anpassungssatz))/100 # Komma zu Punkt, Zahlenformat
  ) %>%
  arrange(jahr)
rm(rentezukunft_csv)

### Die Tabelle mit den prognostizierten Inflationsraten einlesen
#Quelle: https://www.bundesbank.de/de/presse/pressenotizen/deutschland-prognose-der-bundesbank-wirtschaft-erholt-sich-allmaehlich-wieder-936568
#Achtung: Die Tabelle selbst muss herauskopiert werden, da die zusammen mit den Hinweisen runtergeladen wird
inflationzukunft_csv <- "/home/.samba/homes/tmamontova/Input/Inflation prospektiv.csv"
library(readr)
library(dplyr)
library(tidyr)
inflationzukunft_raw <- read_csv2(inflationzukunft_csv, trim_ws = TRUE)

#Die Zeilen bereinigen und irrelevante Zeilen löschen
inflationzukunft <- inflationzukunft_raw %>%
  filter(grepl("Harmonisierter Verbraucherpreisindex", .[[1]], ignore.case = TRUE)) %>% 
  select(-1)  # Meta-Spalte ("Harmonisierter Verbraucherpreisindex") löschen

#Spaltennamen bereinigen und numerisch umwandeln ins Longformat (Jahr in den Zeilen und nicht in den Spalten)
inflationzukunft <- inflationzukunft %>%
  pivot_longer(
    cols = everything(),
    names_to = "jahr",
    values_to = "verbraucherpreisindex"
  ) %>%
  mutate(
    jahr = as.numeric(jahr),
    verbraucherpreisindex = as.numeric(gsub("[^0-9.-]", "", gsub(",", ".", verbraucherpreisindex)))/100
  ) %>%
  arrange(jahr)
rm(inflationzukunft_raw, inflationzukunft_csv)

#Lohnentwicklung einlesen (für BBG-Fortschreibung ab 2027)
lohnentwicklung <- read.csv2("/home/.samba/homes/tmamontova/Input/Lohnentwicklung.csv", stringsAsFactors = FALSE)
lohnentwicklung <- lohnentwicklung %>%
  mutate(
    jahr = as.numeric(jahr),
    lohnentwicklung = as.numeric(gsub(",", ".", lohnentwicklung)) / 100
  )

#Kumulative Faktoren 2017→2022 berechnen
kum_rente_west <- cumprod(1 + rente$west[rente$jahr %in% 2018:2022])[length(cumprod(1 + rente$west[rente$jahr %in% 2018:2022]))]
kum_rente_ost  <- cumprod(1 + rente$ost[rente$jahr %in% 2018:2022])[length(cumprod(1 + rente$ost[rente$jahr %in% 2018:2022]))]
kum_inflation  <- cumprod(1 + inflation$veränderung[inflation$jahr %in% 2018:2022])[length(cumprod(1 + inflation$veränderung[inflation$jahr %in% 2018:2022]))]
kum_haus       <- haus$hausindex[haus$jahr == 2022] / haus$hausindex[haus$jahr == 2017]

#Originaldaten mit dem Jahr verzeichnen
personen <- personen %>%
rename_with(~ paste0(.x, "_2017"),
            .cols = c("rente_monatlich", "grente_monatlich",
                      "brente_monatlich",
                      "arbeitseinkommen_monatlich",    
                      "wohnkosten",                     
                      "arbeitseinkommen_ab_monatlich", "arbeitseinkommen_sf_monatlich",
                      "gesamt_vermögen_ohne_anteiliges_wohneigentum",
                      "sonstiges_wohneigentum_gesamter_haushalt", "selbstgenutztes_wohneigentum_gesamter_haushalt",
                      "wohneigentum_gesamter_haushalt",
                      "haushaltseinkommen_aufgeteilt_monatlich", "svleistungen_transfers_monatlich"))

#Einkommen/Vermögen fortschreiben
personen <- personen %>%
  mutate(
    #_2017 → _2022 (Rentenkomponenten mit Rentenanpassung)
    rente_monatlich_2022 = rente_monatlich_2017 *
      ifelse(bula >= 11, kum_rente_ost, kum_rente_west),
    grente_monatlich_2022 = grente_monatlich_2017 *
      ifelse(bula >= 11, kum_rente_ost, kum_rente_west),
    brente_monatlich_2022 = brente_monatlich_2017 *
      ifelse(bula >= 11, kum_rente_ost, kum_rente_west),

    #Inflationsbasierte Variablen
    arbeitseinkommen_monatlich_2022 = arbeitseinkommen_monatlich_2017 * kum_inflation,  
    wohnkosten_2022 = wohnkosten_2017 * kum_inflation,  
    arbeitseinkommen_ab_monatlich_2022 = arbeitseinkommen_ab_monatlich_2017 * kum_inflation,
    arbeitseinkommen_sf_monatlich_2022 = arbeitseinkommen_sf_monatlich_2017 * kum_inflation,
    haushaltseinkommen_aufgeteilt_monatlich_2022 = haushaltseinkommen_aufgeteilt_monatlich_2017 * kum_inflation,
    svleistungen_transfers_monatlich_2022 = svleistungen_transfers_monatlich_2017 * kum_inflation,

    gesamt_vermögen_ohne_anteiliges_wohneigentum_2022 =
      gesamt_vermögen_ohne_anteiliges_wohneigentum_2017 * kum_inflation,

    wohneigentum_gesamter_haushalt_2022 =
      wohneigentum_gesamter_haushalt_2017 * kum_haus,

    selbstgenutztes_wohneigentum_gesamter_haushalt_2022 =
      selbstgenutztes_wohneigentum_gesamter_haushalt_2017 * kum_haus,

    sonstiges_wohneigentum_gesamter_haushalt_2022 =
      sonstiges_wohneigentum_gesamter_haushalt_2017 * kum_haus
  )
rm(kum_haus, kum_inflation, kum_rente_ost, kum_rente_west)

# ---- Schritt 3: Berechnete Werte womöglich mit den tatsächlichen Einkommensdaten aus 2022 ersetzen ----

#Aufbereiteren Datensatz einlesen
pequiv_2023c_1 <- read_csv("/home/.samba/homes/tmamontova/Input/pequiv_2023c.csv")
names(pequiv_2023c_1) <- tolower(names(pequiv_2023c_1))

#2023-Werte priorisieren + Alter +5 für Fehlende
personen <- personen %>%
  left_join(
    pequiv_2023c_1 %>%
      # ZUERST pid sichern, DANACH alles umbenennen
      select(pid, everything()) %>%
      rename_with(~ paste0(.x, "_2023"), -pid),  # ALLES außer pid
    by = "pid"
  ) %>%
  mutate(
    #2023-Werte priorisieren (coalesce)
    syear = coalesce(syear_2023, syear),
    bula = coalesce(bula_2023, bula),
    alter = coalesce(alter_2023, alter + 5),
    altersintervalle_alte = case_when(
      !is.na(altersintervalle_alte_2023) ~ altersintervalle_alte_2023,
      alter >= 80 ~ "80 und älter",
      alter >= 65 ~ "65 bis unter 80",
      TRUE ~ NA_character_
    ),
    beziehung_zum_haushaltsvorstand = coalesce(beziehung_zum_haushaltsvorstand_2023, beziehung_zum_haushaltsvorstand),
    iciv1 = coalesce(iciv1_2023, iciv1),
    e11102 = coalesce(e11102_2023, e11102),
    d11104 = coalesce(d11104_2023, d11104),
    
    #Einkommen/Vermögen: 2023 priorisieren
    rente_monatlich_2022 = coalesce(rente_monatlich_2023, rente_monatlich_2022),
    grente_monatlich_2022 = coalesce(grente_monatlich_2023, grente_monatlich_2022),
    brente_monatlich_2022 = coalesce(brente_monatlich_2023, brente_monatlich_2022),
    haushaltseinkommen_aufgeteilt_monatlich_2022 = coalesce(haushaltseinkommen_aufgeteilt_monatlich_2023, haushaltseinkommen_aufgeteilt_monatlich_2022),
    svleistungen_transfers_monatlich_2022 = coalesce(svleistungen_transfers_monatlich_2023, svleistungen_transfers_monatlich_2022),
    arbeitseinkommen_monatlich_2022 = coalesce(arbeitseinkommen_monatlich_2023, arbeitseinkommen_monatlich_2022),  # NEU: statt partnereinkommen_o_wk
    wohnkosten_2022 = coalesce(wohnkosten_2023, wohnkosten_2022),  # NEU
    arbeitseinkommen_ab_monatlich_2022 = coalesce(arbeitseinkommen_ab_monatlich_2023, arbeitseinkommen_ab_monatlich_2022),
    arbeitseinkommen_sf_monatlich_2022 = coalesce(arbeitseinkommen_sf_monatlich_2023, arbeitseinkommen_sf_monatlich_2022)
  ) %>%
  select(-ends_with("_2023"))  # Hilfsspalten löschen

#Gesamteinkommen aus fortgeschriebenen Komponenten berechnen
#Eigenes Gesamteinkommen: rente + haushaltseinkommen
#Gesamteinkommen für Partner: rente + arbeitseinkommen + svleistungen + haushaltseinkommen - wohnkosten
#Wohngeldeinkommen: (rente + haushaltseinkommen) * 0.9
personen <- personen %>%
  mutate(
    #Eigenes Gesamteinkommen (für die Person selbst)
    gesamteinkommen_monatlich_2022 = round(
      coalesce(rente_monatlich_2022, 0) +
      coalesce(haushaltseinkommen_aufgeteilt_monatlich_2022, 0), 0),

    #Wohngeldeinkommen aus Komponenten berechnen
    wohngeldeinkommen_netto_2022 = round(
      (coalesce(rente_monatlich_2022, 0) +
       coalesce(haushaltseinkommen_aufgeteilt_monatlich_2022, 0)) * 0.9, 0),

    #Gesamteinkommen das dem Partner zugeordnet wird
    #(Einkommen dieser Person, das für den Partner relevant ist)
    gesamteinkommen_fuer_partner_2022 = round(
      coalesce(rente_monatlich_2022, 0) +
      coalesce(arbeitseinkommen_monatlich_2022, 0) +
      coalesce(svleistungen_transfers_monatlich_2022, 0) +
      coalesce(haushaltseinkommen_aufgeteilt_monatlich_2022, 0) -
      coalesce(wohnkosten_2022, 0), 0)
  )

#Partner-Zuordnung nach d11104
#basierend auf d11104 (1 = Partner vorhanden)
#- Personen mit d11104 == 1 haben einen Partner
#- Im Haushalt (hid) die andere Person mit d11104 == 1 finden
#- Deren gesamteinkommen_fuer_partner übernehmen
#- Bei Haushalten ohne genau 2 Personen mit d11104 == 1: Partnereinkommen = 0
partner_einkommen <- personen %>%
  filter(d11104 == 1) %>%
  group_by(hid) %>%
  filter(n() == 2) %>%  # Nur echte Paare (genau 2 Personen mit d11104 == 1)
  mutate(
    partner_eink_temp = sum(gesamteinkommen_fuer_partner_2022, na.rm = TRUE) -
      gesamteinkommen_fuer_partner_2022
  ) %>%
  ungroup() %>%
  select(pid, partner_eink_temp)

personen <- personen %>%
  left_join(partner_einkommen, by = "pid") %>%
  mutate(
    gesamteinkommen_partner_2022 = coalesce(partner_eink_temp, 0)
  ) %>%
  select(-partner_eink_temp, -gesamteinkommen_fuer_partner_2022)

rm(partner_einkommen)

# ---- Schritt 4: Einsetzungsfähiges Einkommen generieren ----

#Krankenversicherungsbeiträge hinzufügen
### Die Tabelle mit den Beitragssätzen einlesen
#Quelle: https://www.sozialpolitik-aktuell.de/files/sozialpolitik-aktuell/_Politikfelder/Finanzierung/Datensammlung/PDF-Dateien/tabII6.pdf
#Achtung: Die PDF-Datei muss erst in die Excel Tabelle umgewandelt werden, die Spalten AV und RV können gelöscht werden
gkv_csv <- "/home/.samba/homes/tmamontova/Input/Beitragssätze Sozialversicherung.csv"
library(readr)
library(tidyr)
gkv <- read_csv2(gkv_csv, trim_ws = TRUE)
gkv <- gkv %>%
  mutate(
    jahr   = as.numeric(Jahr),
    gkv    = as.numeric(gsub(",", ".", Krankenversicherung)), # Spaltennamen angepasst + Komma zu Punkt, Zahlenformat
    gkv_z  = as.numeric(gsub(",", ".", Zusatzbeitrag_GKV)),
    pv     = as.numeric(gsub(",", ".", Pflegeversicherung)),
    pv_z   = as.numeric(gsub(",", ".", Kinderlosenzuschlag_PV))
  ) %>%
  mutate(across(gkv:pv_z, ~ .x / 100)) %>% # Zahlen in Prozent
  mutate(
    across(c(gkv, gkv_z, pv, pv_z),
           ~ replace_na(.x, 0)) # NA durch 0 ersetzt
  ) %>%
  bind_rows(
    data.frame(jahr = 2001:2004)  # leere Zeilen für Lücke
  ) %>%
  arrange(jahr) %>%
  fill(gkv, gkv_z, pv, pv_z, .direction = "down") %>% 
  select(jahr, gkv, gkv_z, pv, pv_z)

#Beitragssätze für Berufsgruppen berechnen
sv_satz <- gkv %>%
  mutate(
    satz_ab   = ((gkv + gkv_z) / 2 + pv / 2), # Arbeitnehmer + Beamte
    satz_sf   = ( gkv + gkv_z + pv), # Selbständige + Freiberufler
    satz_rent = ((gkv + gkv_z) / 2 + pv), # Rentner
    satz_betr = ( gkv + gkv_z + pv)) # Betriebsrente
rm(gkv, gkv_csv)

#Höchstbeiträge berechnen
### Die Tabelle mit den BBGs einlesen
#Quelle: https://de.statista.com/statistik/daten/studie/2930/umfrage/gesetzliche-krankenversicherung-beitragsbemessungsgrenze-seit-1998/
#Achtung: Die Tabelle selbst muss herauskopiert werden (West/Ost-Unterscheidung seit 2001 irrelevant)
bbg_csv <- "/home/.samba/homes/tmamontova/Input/BBG GKV.csv"
library(dplyr)
library(readr)
bbg <- read_csv2(bbg_csv, trim_ws = TRUE)
colnames(bbg) <- c(              # Spaltennamen angepasst 
  "jahr",
  "bbg_monat"
)
bbg <- bbg %>%
  mutate(
    jahr = as.numeric(gsub("[^0-9]", "", jahr)),  # Jahr extrahieren
    bbg_monat = as.numeric(gsub(",", ".", bbg_monat))
  ) %>%
  arrange(jahr) %>%  # aufsteigend sortieren
  select(jahr, bbg_monat)

#Annahme: BBG mit Lohnentwicklung fortschreiben ab 2027
for (y in 2027:2035) {
  if (!(y %in% bbg$jahr)) {
    lohn_rate <- lohnentwicklung$lohnentwicklung[lohnentwicklung$jahr == y]
    if (length(lohn_rate) == 0) lohn_rate <- tail(lohnentwicklung$lohnentwicklung, 1)
    prev_val <- bbg$bbg_monat[bbg$jahr == y - 1]
    bbg <- rbind(bbg, data.frame(jahr = y, bbg_monat = prev_val * (1 + lohn_rate)))
  }
}

#Die Tabelle mit den Beitragssätzen mit den Höchstbeiträgen ergänzen
sv_satz <- bbg %>%
  left_join(sv_satz, by = "jahr") %>%
  arrange(jahr) %>%
  fill(gkv, gkv_z, pv, pv_z, satz_ab, satz_sf, satz_rent, satz_betr, .direction = "down") %>%
  mutate(
    #Höchstbeiträge je Gruppe (BBG * Gruppen-spezifischer Satz)
    hoechst_ab   = bbg_monat * satz_ab,   # Arbeitnehmer + Beamte
    hoechst_sf   = bbg_monat * satz_sf,   # Freiberufler + Selbstständige
    hoechst_rent = bbg_monat * satz_rent, # Rentner
    hoechst_betr = bbg_monat * satz_betr  # Betriebsrente
  ) %>%
  select(jahr, gkv, gkv_z, pv, pv_z, bbg_monat, satz_ab, satz_sf, satz_rent, satz_betr,
         hoechst_ab, hoechst_sf, hoechst_rent, hoechst_betr)
rm(bbg, bbg_csv)

#Die Tabelle sv-satz mit den Beitragssätzen mit den Mindestbemessungsgrenzen für Sebstständige, 
#Familienversicherungsgrenzen für Rentner und Freibeträgen für Betriebsrenten ergänzen
### Die Tabelle einlesen
#1) Mindestbemessungsgrenze § 240 Abs. 2 SGB V (SVG-VO und Versichertenentlastungsgesetz)
#2) Familienversicherungsgrenze § 10 Abs. 1 Nr. 4 SGB V (Änderungen von SGB V)
#3) Freibeträge § 229 SGB V (BSG-Rechtsprechung, GKV-BRG)
grenzwerte_csv <- "/home/.samba/homes/tmamontova/Input/Grenzwerte GKV.csv"
library(dplyr)
library(readr)
grenzwerte <- read_csv (grenzwerte_csv, trim_ws = TRUE)

#Annahme: Grenzwerte GKV mit Inflation fortschreiben ab 2027
for (y in 2027:2035) {
  if (!(y %in% grenzwerte$jahr)) {
    infl <- inflationzukunft$verbraucherpreisindex[inflationzukunft$jahr == y]
    if (length(infl) == 0) infl <- tail(inflationzukunft$verbraucherpreisindex, 1)
    prev <- grenzwerte[grenzwerte$jahr == max(grenzwerte$jahr[grenzwerte$jahr < y]), ]
    grenzwerte <- rbind(grenzwerte, data.frame(
      jahr = y,
      fam_rent = prev$fam_rent * (1 + infl),
      frei_betr = prev$frei_betr * (1 + infl),
      min_sf = prev$min_sf * (1 + infl)
    ))
  }
}

#sv-satz mit den drei Punkten ergänzen
sv_satz <- sv_satz %>%
  left_join(grenzwerte, by = "jahr") %>%
  #Neue Spalten hinzufügen
  mutate(
    fam_rent = fam_rent,    # Familienversicherungsgrenze Rentner
    frei_betr = frei_betr,  # Freibetrag Betriebsrente
    min_sf = min_sf*satz_sf # Mindestbeitrag hauptberuflich Selbstständige
  ) %>%
  select(jahr, gkv, gkv_z, pv, pv_z, bbg_monat, satz_ab, satz_sf, satz_rent, satz_betr,
         hoechst_ab, hoechst_sf, hoechst_rent, hoechst_betr,fam_rent, frei_betr, min_sf)
rm(grenzwerte, grenzwerte_csv)

### Die Tabelle mit Barbeträgen, Regelsätzen und Schonvermögenswerten einlesen
#1) Barbeträge § 27b SGB XII
#2) Regelsätze § 28 SGB XII
#3) Schonvermögen §90, §66a SGB XII
schonwerte_csv <- "/home/.samba/homes/tmamontova/Input/SGBXII Werte.csv"
library(dplyr)
library(readr)
schonwerte <- read_csv (schonwerte_csv, trim_ws = TRUE)
schonwerte <- schonwerte %>%
  rename (barbeträge = barbetraege)
rm(schonwerte_csv)

#Annahme: SGBXII Werte mit Inflation fortschreiben ab 2027
for (y in 2027:2035) {
  if (!(y %in% schonwerte$jahr)) {
    infl <- inflationzukunft$verbraucherpreisindex[inflationzukunft$jahr == y]
    if (length(infl) == 0) infl <- tail(inflationzukunft$verbraucherpreisindex, 1)
    prev <- schonwerte[schonwerte$jahr == max(schonwerte$jahr[schonwerte$jahr < y]), ]
    schonwerte <- rbind(schonwerte, data.frame(
      jahr = y,
      barbeträge = prev$barbeträge * (1 + infl),
      regelsatz = prev$regelsatz * (1 + infl),
      schon_bund = 10000
    ))
  }
}

#KV-Beiträge berechnen
#Beschäftigungsstatus ergänzen
#Daten für 2018 und 2023 extrahieren
pgstib_2023 <- Input_pgen %>%
  select(pid, syear, pgstib) %>%
  filter(syear == 2023) %>%
  collect()

pgstib_2018 <- Input_pgen %>%
  select(pid, syear, pgstib) %>%
  filter(syear == 2018) %>%
  collect()

#2018 als Basis nehmen und womöglich mit 2023 ergänzen
pgstib_combo <- pgstib_2018 %>%
  select(pid, pgstib_2018 = pgstib)

pgstib_combo <- pgstib_combo %>%
  full_join(
    pgstib_2023 %>% select(pid, pgstib_2023 = pgstib),
    by = "pid"
  ) %>%
  mutate(
    pgstib = coalesce(pgstib_2023, pgstib_2018)
  ) %>%
  select(pid, pgstib)

#In "personen" mergen
personen <- personen %>%
  left_join(pgstib_combo, by = "pid") %>%
  move_columns(pgstib, .after = "iciv1")
rm(pgstib_2018, pgstib_2023, pgstib_combo)

#1. PKV-Beiträge
#Die Daten aus 2018 und 2023 extrahieren
kv_beitrag_pkv_2018 <- Input_pl_2018 %>%
  select(pid, syear, ple0136_h) %>%
  rename(kv_beitrag_pkv_2018 = ple0136_h) %>%
  mutate(kv_beitrag_pkv_2018 = ifelse(kv_beitrag_pkv_2018 < 0, 0, kv_beitrag_pkv_2018)) %>%
  collect()

kv_beitrag_pkv_2023 <- Input_pl_2023 %>%
  select(pid, syear, ple0136_h) %>%
  rename(kv_beitrag_pkv_2023 = ple0136_h) %>%
  mutate(kv_beitrag_pkv_2023 = ifelse(kv_beitrag_pkv_2023 < 0, 0, kv_beitrag_pkv_2023)) %>%
  collect()

#2018 als Basis nehmen und womöglich mit 2023 ergänzen
kv_beitrag_pkv <- kv_beitrag_pkv_2018 %>%
  select(pid, kv_beitrag_pkv_2018) %>%
  full_join(
    kv_beitrag_pkv_2023 %>% select(pid, kv_beitrag_pkv_2023),
    by = "pid"
  ) %>%
  mutate(
    kv_beitrag_pkv = coalesce(kv_beitrag_pkv_2023, kv_beitrag_pkv_2018),
    kv_beitrag_pkv = coalesce(kv_beitrag_pkv, 0)
  ) %>%
  select(pid, kv_beitrag_pkv)

#In "personen" mergen
personen <- personen %>%
  left_join(kv_beitrag_pkv, by = "pid")%>% 
  mutate(kv_beitrag_pkv = coalesce(kv_beitrag_pkv, 0))
rm(kv_beitrag_pkv, kv_beitrag_pkv_2018, kv_beitrag_pkv_2023)

#KV-Beiträge berechnen MIT lookup aus sv_satz[2022, ]
satz_ab_2022     <- sv_satz$satz_ab[sv_satz$jahr == 2022]
satz_sf_2022     <- sv_satz$satz_sf[sv_satz$jahr == 2022]
satz_rent_2022   <- sv_satz$satz_rent[sv_satz$jahr == 2022]
satz_betr_2022   <- sv_satz$satz_betr[sv_satz$jahr == 2022]
hoechst_ab_2022  <- sv_satz$hoechst_ab[sv_satz$jahr == 2022]
hoechst_sf_2022  <- sv_satz$hoechst_sf[sv_satz$jahr == 2022]
hoechst_rent_2022<- sv_satz$hoechst_rent[sv_satz$jahr == 2022]
hoechst_betr_2022<- sv_satz$hoechst_betr[sv_satz$jahr == 2022]
fam_rent_2022    <- sv_satz$fam_rent[sv_satz$jahr == 2022]
min_sf_2022      <- sv_satz$min_sf[sv_satz$jahr == 2022]
frei_betr_2022   <- sv_satz$frei_betr[sv_satz$jahr == 2022]

#2. GKV AN/Beamte (pgstib 210-250, 510-550, 610-640) UND Selbstständige/Freiberufler (pgstib 410-413, 430-433, 421-423)
personen <- personen %>%
  mutate(
    kv_pgstib = case_when(
      # AN/Beamte
      pgstib %in% c(210:250, 510:550, 610:640) ~
        pmin(arbeitseinkommen_ab_monatlich_2022 * satz_ab_2022, hoechst_ab_2022),
      
      # Selbstständige/Freiberufler
      pgstib %in% c(410:413, 430:433, 421:423) ~
        pmin(pmax(arbeitseinkommen_sf_monatlich_2022, min_sf_2022) * satz_sf_2022, hoechst_sf_2022),
      
      TRUE ~ 0
    )
  )

#3. GKV Rentner (Gesamteinkommen > fam_rent)
personen <- personen %>%
  mutate(
    kv_rentner = ifelse(
      gesamteinkommen_monatlich_2022 > fam_rent_2022,
      pmin(grente_monatlich_2022 * satz_rent_2022, hoechst_rent_2022),
      0
    )
  )

#4. GKV Betriebsrente (brente_monatlich - frei_betr)
personen <- personen %>%
  mutate(
    kv_betriebsrente = pmax(
      0,
      pmin(
        pmax(brente_monatlich_2022 - frei_betr_2022, 0) * satz_betr_2022,
        hoechst_betr_2022
      )
    )
  )

#5. Endgültige KV_Beitrag
personen <- personen %>%
  mutate(
    kv_gesamt = rowSums(across(c(kv_pgstib, kv_rentner, kv_betriebsrente)), na.rm = TRUE),
    kv_beitrag = ifelse(
      kv_beitrag_pkv > 0,
      kv_beitrag_pkv,
      pmax(0, round(kv_gesamt, 0))
    )
  ) %>%
  select(-c(kv_pgstib, kv_rentner, kv_betriebsrente, kv_gesamt))
rm(satz_ab_2022, satz_sf_2022, satz_rent_2022, satz_betr_2022, hoechst_ab_2022, hoechst_sf_2022, hoechst_rent_2022, hoechst_betr_2022, fam_rent_2022, min_sf_2022, frei_betr_2022)

#Einsetzungsfähiges Einkommen und Vermögen berechnen
#Schonwerte 2022 extrahieren
regelsatz_2022 <- schonwerte$regelsatz[schonwerte$jahr == 2022]
barbetrag_2022 <- schonwerte$barbeträge[schonwerte$jahr == 2022]
schon_bund_2022 <- schonwerte$schon_bund[schonwerte$jahr == 2022]

#1. Einsetzungsfähiges Partnereinkommen
#Verwendet das zugeordnete gesamteinkommen_partner (aus Partner-Zuordnung)
personen <- personen %>%
  mutate(
    einsetzungsfähiges_partnereinkommen_2022 = pmax(
      coalesce(gesamteinkommen_partner_2022, 0) - kv_beitrag - regelsatz_2022, 0
    )
  )

#2. Einsetzungsfähiges individuelle Einkommen
personen <- personen %>%
  mutate(
    einsetzungsfähiges_individualeinkommen_2022 = pmax(
      gesamteinkommen_monatlich_2022 - kv_beitrag - 50 - barbetrag_2022, 0
    )
  )

#3. Einsetzungsfähiges Einkommen insgesamt
personen <- personen %>%
  mutate(
    einsetzungsfähiges_einkommen_insgesamt_2022 = pmax(
      round(einsetzungsfähiges_partnereinkommen_2022 +
              einsetzungsfähiges_individualeinkommen_2022), 0
    )
  )

#4. Einsetzbares Vermögen
#Schonbetrag abziehen (Paare 2x)
personen <- personen %>%
  group_by(hid) %>%
  mutate(
    #Ist Partner vorhanden? (d11104 == 1)
    partner_vorhanden = (d11104 == 1),
    schonbetrag_pro_person = ifelse(partner_vorhanden, 2 * schon_bund_2022, schon_bund_2022)
  ) %>%
  ungroup()

personen <- personen %>%
  mutate(
    einsetzbares_vermögen_ohne_anteiliges_wohneigentum_2022 = 
      pmax(gesamt_vermögen_ohne_anteiliges_wohneigentum_2022 - schonbetrag_pro_person, 0)) %>%
  select(-c(schonbetrag_pro_person))
rm(barbetrag_2022, schon_bund_2022)

#############################################Pflegewohngeld##########################################################
#Einsetzungsfähiges Einkommen und Vermögen für Wohngeld NRW und SH berechnen
### Die Tabellen mit Schonwerten für Einkommen (SH) und Vermögen (NRW und SH) einlesen
#Quelle: Jeweiliges Gesetz
#NRW: APG NRW § 14 (Fn 8), SH: LPflegeGVO § 8
schonwerte_nrw_csv <- "/home/.samba/homes/tmamontova/Input/Schonwerte NRW.csv"
schonwerte_sh_csv <- "/home/.samba/homes/tmamontova/Input/Schonwerte SH.csv"
library(dplyr)
library(readr)
schonwerte_nrw <- read_csv2 (schonwerte_nrw_csv, trim_ws = TRUE)
schonwerte_sh <- read_csv2 (schonwerte_sh_csv, trim_ws = TRUE)
rm(schonwerte_nrw_csv, schonwerte_sh_csv)

#Schonwerte 2022 extrahieren
schon_nrw_ind_2022 <- schonwerte_nrw$schon_vermoegen_ind[schonwerte_nrw$jahr == 2022]
schon_nrw_part_2022 <- schonwerte_nrw$schon_vermoegen_part[schonwerte_nrw$jahr == 2022]
schon_sh_e_ind_2022 <- schonwerte_sh$schon_einkommen_ind[schonwerte_sh$jahr == 2022]
schon_sh_e_part_2022 <- schonwerte_sh$schon_einkommen_part[schonwerte_sh$jahr == 2022]
schon_sh_v_ind_2022 <- schonwerte_sh$schon_vermoegen_ind[schonwerte_sh$jahr == 2022]
schon_sh_v_part_2022 <- schonwerte_sh$schon_vermoegen_part[schonwerte_sh$jahr == 2022]

#1. Einsetzungsfähiges individuelle Einkommen Wohngeld NRW
personen <- personen %>%
  mutate(
    einsetzungsfähiges_individualeinkommen_wg_nrw_2022 = pmax(
      gesamteinkommen_monatlich_2022 - kv_beitrag - 50, 0
    )
  )

#2. Einsetzungsfähiges Einkommen insgesamt Wohngeld NRW
personen <- personen %>%
  mutate(
    einsetzungsfähiges_einkommen_insgesamt_wg_nrw_2022 = pmax(
      round(einsetzungsfähiges_partnereinkommen_2022 + 
              einsetzungsfähiges_individualeinkommen_wg_nrw_2022 - 
              regelsatz_2022), 0
    )
  )

#3. Einsetzungsfähiges individuelle Einkommen Wohngeld SH
personen <- personen %>%
  mutate(
    einsetzungsfähiges_individualeinkommen_wg_sh_2022 =
      pmax(
        gesamteinkommen_monatlich_2022 -
          ifelse(d11104 == 1, schon_sh_e_part_2022, schon_sh_e_ind_2022),
        0
      )
  )

#4. Einsetzungsfähiges Einkommen insgesamt Wohngeld SH
personen <- personen %>%
  mutate(
    einsetzungsfähiges_einkommen_insgesamt_wg_sh_2022 = pmax(
      round(einsetzungsfähiges_partnereinkommen_2022 + 
              einsetzungsfähiges_individualeinkommen_wg_sh_2022 - 
              regelsatz_2022), 0
    )
  )

#5. Einsetzbares Vermögen Wohngeld NRW
#Schonbetrag abziehen für NRW (Alleinstehend vs Paare) 
personen <- personen %>%
  mutate(
einsetzbares_vermögen_ohne_anteiliges_wohneigentum_wg_nrw_2022 =
  pmax(
    round(
      gesamt_vermögen_ohne_anteiliges_wohneigentum_2022 -
        ifelse(partner_vorhanden,
               schon_nrw_part_2022,
               schon_nrw_ind_2022)
    ),0),

#6. Einsetzbares Vermögen Wohngeld SH   
#Schonbetrag abziehen für SH (Alleinstehend vs Paare) 
einsetzbares_vermögen_ohne_anteiliges_wohneigentum_wg_sh_2022 =
  pmax(
    round(
      gesamt_vermögen_ohne_anteiliges_wohneigentum_2022 -
        ifelse(partner_vorhanden,
               schon_sh_v_part_2022,
               schon_sh_v_ind_2022)
    ),0)) %>%
  select(-c(partner_vorhanden, kv_beitrag))
rm(regelsatz_2022, schon_nrw_ind_2022, schon_nrw_part_2022, 
   schon_sh_e_ind_2022, schon_sh_e_part_2022, schon_sh_v_ind_2022, schon_sh_v_part_2022)
#############################################Pflegewohngeld##########################################################

#Filter: NA-Vermögen ausschließen
personen <- personen %>%
  filter(!is.na(einsetzbares_vermögen_ohne_anteiliges_wohneigentum_2022))%>%
  filter(!is.na(einsetzbares_vermögen_ohne_anteiliges_wohneigentum_wg_nrw_2022))%>%
  filter(!is.na(einsetzbares_vermögen_ohne_anteiliges_wohneigentum_wg_sh_2022))

#Variablenblöcke (für fortschreibe_rohwerte_fast + extract_basis_vals)
renten_vars <- c(
  "rente_monatlich",
  "grente_monatlich",
  "brente_monatlich"
)

#Erweitert um Einkommenskomponenten für separate Fortschreibung
inflation_vars <- c(
  "arbeitseinkommen_monatlich",
  "wohnkosten",
  "arbeitseinkommen_ab_monatlich",
  "arbeitseinkommen_sf_monatlich",
  "gesamt_vermögen_ohne_anteiliges_wohneigentum",
  "haushaltseinkommen_aufgeteilt_monatlich",
  "svleistungen_transfers_monatlich"
)

haus_vars <- c(
  "wohneigentum_gesamter_haushalt",
  "selbstgenutztes_wohneigentum_gesamter_haushalt",
  "sonstiges_wohneigentum_gesamter_haushalt"
)

einsetz_vars <- c(
  "einsetzungsfähiges_partnereinkommen",
  "einsetzungsfähiges_individualeinkommen",
  "einsetzungsfähiges_einkommen_insgesamt",
  "einsetzbares_vermögen_ohne_anteiliges_wohneigentum",
  "einsetzungsfähiges_individualeinkommen_wg_nrw",
  "einsetzungsfähiges_einkommen_insgesamt_wg_nrw",
  "einsetzungsfähiges_individualeinkommen_wg_sh",
  "einsetzungsfähiges_einkommen_insgesamt_wg_sh",
  "einsetzbares_vermögen_ohne_anteiliges_wohneigentum_wg_nrw",
  "einsetzbares_vermögen_ohne_anteiliges_wohneigentum_wg_sh"
)


###Filter 
#Alter
personen <- personen %>% 
  filter(alter >= 65)

#Erwerbstätigkeit
personen <- personen[personen$e11102 != 1, ]

#Gewicht = 0
personen <- personen[personen$w11105 != 0, ]

#Anteil Pensionäre gewichtet (absolut und Prozent)
n_pensionaere <- sum(personen$w11105[personen$iciv1 > 0], na.rm = TRUE)
n_gesamt <- sum(personen$w11105, na.rm = TRUE)

cat("Pensionäre (gewichtet):", format(round(n_pensionaere), big.mark = ","), 
    "(", round(n_pensionaere / n_gesamt * 100, 2), "%)\n")

# PKV-Anteil berechnen (gewichtet)
n_pkv <- sum(personen$w11105[personen$kv_beitrag_pkv > 0], na.rm = TRUE)
n_gesamt <- sum(personen$w11105, na.rm = TRUE)

cat("PKV-Versicherte (gewichtet):", format(round(n_pkv), big.mark = " "), 
    "(", round(n_pkv / n_gesamt * 100, 2), "%)\n")

#Gewichtsanpassung: Pensionäre auf 4% der 65+ Bevölkerung
#Pensionäre (iciv1 > 0) sollen nur 4% ausmachen
n_total <- sum(personen$w11105, na.rm = TRUE)
n_pensionaere <- sum(personen$w11105[personen$iciv1 > 0], na.rm = TRUE)
anteil_pensionaere_aktuell <- n_pensionaere / n_total

ziel_anteil <- 0.04

#Faktoren berechnen (Gesamtsumme bleibt gleich)
faktor_pensionaere <- ziel_anteil / anteil_pensionaere_aktuell
faktor_rest <- (1 - ziel_anteil) / (1 - anteil_pensionaere_aktuell)

#Gewichte anpassen
personen$w11105 <- ifelse(personen$iciv1 > 0,
                          personen$w11105 * faktor_pensionaere,
                          personen$w11105 * faktor_rest)

cat("Gewichtsanpassung Pensionäre:\n")
cat("  Anteil vorher:", round(anteil_pensionaere_aktuell * 100, 2), "%\n")
cat("  Anteil nachher:", round(ziel_anteil * 100, 2), "%\n")
cat("  Faktor Pensionäre:", round(faktor_pensionaere, 4), "\n")
cat("  Faktor Rest:", round(faktor_rest, 4), "\n")
cat("  Gesamtgewicht (Kontrolle):", format(round(sum(personen$w11105, na.rm = TRUE)), big.mark = " "), "\n\n")

#Basispfad
basepfad <- "/home/.samba/homes/tmamontova/Bachelorarbeit/Input/"
ergebnis_pfad <- "/home/.samba/homes/tmamontova/Bachelorarbeit/Ergebnisse/"
lauf_nr <- as.integer(commandArgs(trailingOnly = TRUE)[1])
lauf_tag <- sprintf("%04d", lauf_nr)
sink(paste0(ergebnis_pfad, "output_GR_BA_n5000_lauf", lauf_tag, ".txt"), split = TRUE)
start_time <- Sys.time()
cat("=== Start:", format(start_time, "%Y-%m-%d %H:%M:%S"), "===\n\n")

################################Mikrosimulation###################################
#Zielgröße (N): 5.000 Personen
N     <- 5000

# Pflegewohngeld-Höchstbeträge
pfwg_max_sh <- 466.95  # Schleswig-Holstein: fester Höchstbetrag

library(dplyr)
library(readr)
library(lubridate)
library(readxl)

### Hilfsdateien einlesen
#Regelsätze Grundsicherung (Mindesteinkommen)
regelsaetze <- read_csv(paste0(basepfad, "regelsaetze_grundsicherung.csv"), show_col_types = FALSE)

#Annahme: Regelsätze mit Inflation fortschreiben
#2026 = 2025-Werte (keine Erhöhung bekannt)
if (!(2026 %in% regelsaetze$jahr)) {
  prev <- regelsaetze[regelsaetze$jahr == 2025, ]
  regelsaetze <- rbind(regelsaetze, data.frame(
    jahr = 2026,
    regelsatz_grundsicherung = prev$regelsatz_grundsicherung,
    pauschale_unterkunft = prev$pauschale_unterkunft,
    mindesteinkommen = prev$mindesteinkommen
  ))
}
#Ab 2027: jährliche Inflation anwenden
for (y in 2027:2035) {
  if (!(y %in% regelsaetze$jahr)) {
    infl <- inflationzukunft$verbraucherpreisindex[inflationzukunft$jahr == y]
    if (length(infl) == 0) infl <- tail(inflationzukunft$verbraucherpreisindex, 1)
    prev <- regelsaetze[regelsaetze$jahr == y - 1, ]
    regelsaetze <- rbind(regelsaetze, data.frame(
      jahr = y,
      regelsatz_grundsicherung = prev$regelsatz_grundsicherung * (1 + infl),
      pauschale_unterkunft = prev$pauschale_unterkunft * (1 + infl),
      mindesteinkommen = prev$mindesteinkommen * (1 + infl)
    ))
  }
}

#Leistungszuschläge § 43c SGB XI
zuschlaege <- read_csv2(paste0(basepfad, "leistungszuschlaege_43c.csv"), show_col_types = FALSE)
zuschlaege$zuschlag_prozent <- as.numeric(zuschlaege$zuschlag_prozent)

#Pflegestatistik 2023 Zielverteilung (mit Altersgruppen)
pflege_ziel <- read_csv(paste0(basepfad, "pflegestatistik_2023_zielverteilung.csv"), show_col_types = FALSE)

#PV-Leistungen nach Pflegegrad (§ 43 SGB XI)
pv_leistungen <- read_csv2(paste0(basepfad, "pv_leistungen_43.csv"), show_col_types = FALSE)

#PV-Leistungen vor GR-Dynamisierung speichern
#Damit delta_pv = PV_GR - PV_unveraendert berechnet werden kann
pv_null_werte <- pv_leistungen  # Kopie der Original-Daten

#Geltendes Recht: §43 für 2028 einmalig kumulierte Inflation (§30 SGB XI)
#Inflationsraten zusammenführen: historisch (inflation) + prospektiv (inflationzukunft)
alle_inflationsraten <- c(
  setNames(inflation$veränderung, inflation$jahr),
  setNames(inflationzukunft$verbraucherpreisindex, inflationzukunft$jahr)
)
#Anpassungsjahr: 2028 (kumulierte Inflation der letzten 3 Jahre)
anpassungsjahre <- c(2028) 
basis_jahr <- max(pv_leistungen$jahr)  # 2027
basis_werte <- pv_leistungen[pv_leistungen$jahr == basis_jahr, ]

for (y in 2028:2035) {
  if (y %in% anpassungsjahre) {
    #Kumulierte Inflation der letzten 3 Kalenderjahre
    infl_jahre <- (y-3):(y-1)  # 2028 -> 2025,2026,2027
    kum_faktor <- prod(1 + sapply(infl_jahre, function(j) {
      r <- alle_inflationsraten[as.character(j)]
      if (is.na(r)) 0.015 else r  # Fallback 1.5%
    }))
    basis_werte <- data.frame(
      jahr = y,
      pflegegrad_1 = basis_werte$pflegegrad_1 * kum_faktor,
      pflegegrad_2 = basis_werte$pflegegrad_2 * kum_faktor,
      pflegegrad_3 = basis_werte$pflegegrad_3 * kum_faktor,
      pflegegrad_4 = basis_werte$pflegegrad_4 * kum_faktor,
      pflegegrad_5 = basis_werte$pflegegrad_5 * kum_faktor
    )
    pv_leistungen <- rbind(pv_leistungen, basis_werte)
  } else {
    #Zwischen Anpassungsjahren: Werte bleiben konstant
    pv_leistungen <- rbind(pv_leistungen, data.frame(
      jahr = y,
      pflegegrad_1 = basis_werte$pflegegrad_1,
      pflegegrad_2 = basis_werte$pflegegrad_2,
      pflegegrad_3 = basis_werte$pflegegrad_3,
      pflegegrad_4 = basis_werte$pflegegrad_4,
      pflegegrad_5 = basis_werte$pflegegrad_5
    ))
  }
}

#PKV-Beitragssteigerungen (3% p.a. ab 2001)
#Quelle: https://www.wip-pkv.de/veroeffentlichungen/detail/entwicklung-der-praemien-und-beitragseinnahmen-in-pkv-und-gkv-aktualisierung-20242025.html
pkv_steigerung <- read_csv(paste0(basepfad, "PKV Beitragssteigerungen.csv"), show_col_types = FALSE)

#Wohngeld-Höchstbeträge (inkl. Klimakomponente) nach Mietstufe
wohngeld_hoechstbetraege <- read.csv(paste0(basepfad, "wohngeld_hoechstbetraege_klimakomponente.csv"))

#Annahme: Wohngeld-Höchstbeträge mit Inflation fortschreiben
#2026 = 2025-Werte (keine Erhöhung bekannt)
for (y in 2026:2035) {
  spalte_neu <- paste0("betrag_", y)
  if (!(spalte_neu %in% names(wohngeld_hoechstbetraege))) {
    if (y == 2026) {
      wohngeld_hoechstbetraege[[spalte_neu]] <- wohngeld_hoechstbetraege$betrag_2025
    } else {
      basis_spalte <- paste0("betrag_", y - 1)
      infl <- inflationzukunft$verbraucherpreisindex[inflationzukunft$jahr == y]
      if (length(infl) == 0) infl <- tail(inflationzukunft$verbraucherpreisindex, 1)
      wohngeld_hoechstbetraege[[spalte_neu]] <- wohngeld_hoechstbetraege[[basis_spalte]] * (1 + infl)
    }
  }
}

#Lookup-Funktion: Mietstufe (römisch) → Höchstbetrag
get_wohngeld_hoechstbetrag <- function(mietstufe_roman, jahr = 2023) {
  spalte <- paste0("betrag_", jahr)
  
  #Fallback: Wenn Spalte nicht existiert, letztes verfügbares Jahr verwenden
  if (!(spalte %in% names(wohngeld_hoechstbetraege))) {
    verfuegbare_jahre <- as.numeric(gsub("betrag_", "", 
                                         names(wohngeld_hoechstbetraege)[grepl("^betrag_", names(wohngeld_hoechstbetraege))]))
    max_jahr <- max(verfuegbare_jahre, na.rm = TRUE)
    spalte <- paste0("betrag_", max_jahr)
  }
  
  idx <- match(mietstufe_roman, wohngeld_hoechstbetraege$mietstufe_roman)
  return(wohngeld_hoechstbetraege[[spalte]][idx])
}

#Pflegegrad-Verteilung aus Pflegestatistik 2023 (vollstationäre Dauerpflege, 65+)
#Berechnet aus Absolutzahlen der Altersgruppen
#Gruppe m_65_80: männlich, 65-80 Jahre (Summe aus 65-70, 70-75, 75-80)
#Gruppe m_80plus: männlich, 80+ Jahre (Summe aus 80-85, 85-90, 90+)
#Gruppe f_65_80: weiblich, 65-80 Jahre
#Gruppe f_80plus: weiblich, 80+ Jahre
#Werte aus pflegestatistik_2023_pflegegrad.csv (Zeilen 12-16, Spalten für M/W und Altersgruppen)

#Absolutzahlen aus der Statistik:
#m_65_80: PG1=557, PG2=11340, PG3=29161, PG4=21574, PG5=12464 => Summe=75096
#m_80plus: PG1=675, PG2=19366, PG3=48512, PG4=44140, PG5=16319 => Summe=129012
#f_65_80: PG1=347, PG2=7520, PG3=18363, PG4=16858, PG5=9090 => Summe=52178
#f_80plus: PG1=1436, PG2=65779, PG3=145256, PG4=126371, PG5=61575 => Summe=400417

pflegegrad_verteilung <- data.frame(
  gruppe = c("m_65_80", "m_80plus", "f_65_80", "f_80plus"),
  pg1 = c(557/75096, 675/129012, 347/52178, 1436/400417),
  pg2 = c(11340/75096, 19366/129012, 7520/52178, 65779/400417),
  pg3 = c(29161/75096, 48512/129012, 18363/52178, 145256/400417),
  pg4 = c(21574/75096, 44140/129012, 16858/52178, 126371/400417),
  pg5 = c(12464/75096, 16319/129012, 9090/52178, 61575/400417)
)

cat("Hilfsdateien geladen.\n\n")

# ---- Schritt 1: Heimkosten-Zeitreihe einlesen ----

cat("=== Heimkosten-Zeitreihe einlesen ===\n")

heime_raw <- read_excel(paste0(basepfad, "2026.03.09 DAK_Heimkosten_Zeitreihe (EEE 10 _, AK 10 _, 2035) korrigiert.xlsx"),
                        sheet = "Komponenten_Zeitreihe")

#Spaltennamen in Kleinbuchstaben
names(heime_raw) <- tolower(names(heime_raw))

#Relevante Spalten auswählen (mit Kostenkomponenten UV, Inv, EEE)
heime <- heime_raw %>%
  select(
    heim_id = matching_schluessel,
    name,
    plz,
    bundesland,
    platzzahl,
    wohngeldstufe,
    matches("^q[1-4]_\\d{4}_(uv|inv|eee|ak)$", ignore.case = TRUE)
  )

#Platzzahl in numerisch konvertieren
heime$platzzahl <- as.numeric(heime$platzzahl)

#Alle Quartalsspalten in numerisch konvertieren
quartal_cols <- names(heime)[grepl("^q[1-4]_", names(heime))]
for (col in quartal_cols) {
  heime[[col]] <- as.numeric(heime[[col]])
}

#Nur Heime mit gültiger Platzzahl behalten
heime <- heime %>%
  filter(!is.na(platzzahl) & platzzahl > 0)

#Prüfung auf fehlende Kostenkomponenten
komponenten_cols <- names(heime)[grepl("^q[1-4]_\\d{4}_(uv|inv|eee)$", names(heime))]
na_counts <- sapply(heime[komponenten_cols], function(x) sum(is.na(x)))
if (any(na_counts > 0)) {
  cat("WARNUNG: Fehlende Werte in Kostenkomponenten gefunden:\n")
  print(na_counts[na_counts > 0])
  cat("Anzahl Heime gesamt:", nrow(heime), "\n")
}

#Bundesland-Code hinzufügen
if (!exists("bula_map")) {
  bula_map <- data.frame(
    bula = 1:16,
    bundesland = c(
      "Schleswig-Holstein", "Hamburg", "Niedersachsen", "Bremen",
      "Nordrhein-Westfalen", "Hessen", "Rheinland-Pfalz", "Baden-Württemberg",
      "Bayern", "Saarland", "Berlin", "Brandenburg",
      "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Thüringen"
    ), stringsAsFactors = FALSE
  )
}

heime <- heime %>%
  left_join(bula_map, by = "bundesland")

cat("Anzahl Heime:", nrow(heime), "\n\n")

# ---- Schritt 2: Zielverteilung skalieren ----

cat("=== Zielverteilung skalieren ===\n")

gesamt_n <- sum(pflege_ziel$total)

pflege_ziel <- pflege_ziel %>%
  mutate(
    target_m_65_80 = round(male_65_80 / gesamt_n * N),
    target_f_65_80 = round(female_65_80 / gesamt_n * N),
    target_m_80plus = round(male_80plus / gesamt_n * N),
    target_f_80plus = round(female_80plus / gesamt_n * N)
  )

sum_targets <- sum(pflege_ziel$target_m_65_80, pflege_ziel$target_f_65_80,
                   pflege_ziel$target_m_80plus, pflege_ziel$target_f_80plus)
cat("Soll-Summe:", sum_targets, "(Ziel:", N, ")\n\n")

# ============================================================
# PERFORMANCE-OPTIMIERUNG: Caches und Lookup-Tabellen vorbauen
# ============================================================
cat("=== Performance-Caches aufbauen ===\n")

#---- A1: Kumulative Faktortabellen (Rente, Inflation, Haus) ----
#Ermöglicht O(1)-Lookup statt O(M)-Schleife in fortschreibe_rohwerte
alle_monate <- seq(as.Date("2001-01-01"), as.Date("2036-12-01"), by = "month")
n_monate <- length(alle_monate)
monat_keys <- format(alle_monate, "%Y-%m")

#Rente-Faktoren: kum_before[k] = kumulativer Faktor VOR Verarbeitung von Monat k
#So dass factor(A->B) = kum[B] / kum[A] (A = Startmonat, B = Zielmonat)
kum_rente_west_v <- numeric(n_monate)
kum_rente_ost_v <- numeric(n_monate)
kum_rente_west_v[1] <- 1; kum_rente_ost_v[1] <- 1

for (k in 2:n_monate) {
  #Adjustment des VORHERIGEN Monats (k-1)
  y_prev <- year(alle_monate[k - 1])
  m_prev <- month(alle_monate[k - 1])

  if (m_prev == 7) {
    if (y_prev %in% rente$jahr) {
      idx_r <- match(y_prev, rente$jahr)
      fw <- 1 + rente$west[idx_r]; fo <- 1 + rente$ost[idx_r]
    } else if (y_prev %in% rentezukunft$jahr) {
      idx_r <- match(y_prev, rentezukunft$jahr)
      fw <- 1 + rentezukunft$anpassungssatz[idx_r]; fo <- fw
    } else {
      idx_r <- which.max(rentezukunft$jahr)
      fw <- 1 + rentezukunft$anpassungssatz[idx_r]; fo <- fw
    }
  } else {
    fw <- 1; fo <- 1
  }
  kum_rente_west_v[k] <- kum_rente_west_v[k - 1] * fw
  kum_rente_ost_v[k] <- kum_rente_ost_v[k - 1] * fo
}
names(kum_rente_west_v) <- monat_keys
names(kum_rente_ost_v) <- monat_keys

#Inflation-Faktoren: monatlich (1 + jahresrate)^(1/12)
kum_infl_v <- numeric(n_monate)
kum_infl_v[1] <- 1
max_infl_jahr <- max(inflation$jahr, na.rm = TRUE)

for (k in 2:n_monate) {
  y_prev <- year(alle_monate[k - 1])

  if (y_prev <= max_infl_jahr) {
    idx_i <- match(y_prev, inflation$jahr)
    infl_rate <- if (!is.na(idx_i)) inflation$veränderung[idx_i] else 0
  } else {
    idx_i <- match(y_prev, inflationzukunft$jahr)
    if (is.na(idx_i)) idx_i <- which.max(inflationzukunft$jahr)
    infl_rate <- inflationzukunft$verbraucherpreisindex[idx_i]
  }
  kum_infl_v[k] <- kum_infl_v[k - 1] * (1 + infl_rate)^(1 / 12)
}
names(kum_infl_v) <- monat_keys

#Häuserpreisindex-Faktoren: monatlich (jahresindex / vorjahresindex)^(1/12)
kum_haus_v <- numeric(n_monate)
kum_haus_v[1] <- 1
max_haus_jahr <- max(haus$jahr)

for (k in 2:n_monate) {
  y_prev <- year(alle_monate[k - 1])

  idx_y <- match(min(y_prev, max_haus_jahr), haus$jahr)
  idx_y1 <- match(min(y_prev, max_haus_jahr) - 1, haus$jahr)

  if (!is.na(idx_y) && !is.na(idx_y1)) {
    hf <- (haus$hausindex[idx_y] / haus$hausindex[idx_y1])^(1 / 12)
  } else {
    max_idx <- which.max(haus$jahr)
    hf <- (haus$hausindex[max_idx] / haus$hausindex[max_idx - 1])^(1 / 12)
  }
  kum_haus_v[k] <- kum_haus_v[k - 1] * hf
}
names(kum_haus_v) <- monat_keys

cat("  Kumulative Faktortabellen gebaut:", n_monate, "Monate\n")

# ---- A2: Schnelle Fortschreibungsfunktion ----
fortschreibe_rohwerte_fast <- function(person_basis_vals, ist_ost, basis_jahr, ziel_key, ziel_jahr) {
  if (basis_jahr >= ziel_jahr) {
    return(person_basis_vals)
  }

  basis_key <- paste0(basis_jahr, "-07")

  r_basis <- if (ist_ost) kum_rente_ost_v[[basis_key]] else kum_rente_west_v[[basis_key]]
  r_ziel <- if (ist_ost) kum_rente_ost_v[[ziel_key]] else kum_rente_west_v[[ziel_key]]
  rente_faktor <- r_ziel / r_basis

  infl_faktor <- kum_infl_v[[ziel_key]] / kum_infl_v[[basis_key]]
  haus_faktor <- kum_haus_v[[ziel_key]] / kum_haus_v[[basis_key]]

  result <- person_basis_vals  # Shallow copy (list)
  for (v in renten_vars) result[[v]] <- result[[v]] * rente_faktor
  for (v in inflation_vars) result[[v]] <- result[[v]] * infl_faktor
  for (v in haus_vars) result[[v]] <- result[[v]] * haus_faktor
  return(result)
}

# ---- A3: Heimkosten-Cache vorbauen ----
#Verschachtelte Liste: kosten_cache[[heim_id]][[qkey]] -> list(uv, inv, eee, ak, gesamt)
kosten_cache <- list()
quartal_uv_cols <- names(heime)[grepl("^q[1-4]_\\d{4}_uv$", names(heime))]

for (h in seq_len(nrow(heime))) {
  hid_str <- as.character(heime$heim_id[h])
  hcache <- list()
  for (uv_col in quartal_uv_cols) {
    qkey <- sub("_uv$", "", uv_col)
    uv_val <- as.numeric(heime[[paste0(qkey, "_uv")]][h])
    inv_val <- as.numeric(heime[[paste0(qkey, "_inv")]][h])
    eee_val <- as.numeric(heime[[paste0(qkey, "_eee")]][h])
    ak_col_name <- paste0(qkey, "_ak")
    ak_val <- if (ak_col_name %in% names(heime)) as.numeric(heime[[ak_col_name]][h]) else NA

    g <- sum(c(uv_val, inv_val, eee_val, ak_val), na.rm = TRUE)
    hcache[[qkey]] <- list(
      uv = uv_val, inv = inv_val, eee = eee_val, ak = ak_val,
      gesamt = if (g == 0) NA else g
    )
  }
  kosten_cache[[hid_str]] <- hcache
}

get_kosten_fast <- function(heim_id_val, datum) {
  qkey <- paste0("q", ceiling(month(datum) / 3), "_", year(datum))
  kosten_cache[[as.character(heim_id_val)]][[qkey]]
}

cat("  Heimkosten-Cache:", length(kosten_cache), "Heime x", length(quartal_uv_cols), "Quartale\n")

# ---- A4: Partner-Index vorbauen ----
#partner_cache[[pid]] -> Partner-Zeile aus personen (oder NULL)
partner_cache <- list()
paare_idx <- which(personen$d11104 == 1)
paare_df <- personen[paare_idx, ]
for (hid_val in unique(paare_df$hid)) {
  paar <- paare_df[paare_df$hid == hid_val, ]
  if (nrow(paar) == 2) {
    partner_cache[[as.character(paar$pid[1])]] <- paar[2, ]
    partner_cache[[as.character(paar$pid[2])]] <- paar[1, ]
  }
}
rm(paare_idx, paare_df)
cat("  Partner-Cache:", length(partner_cache), "Paare\n")

# ---- A5: Jahresparameter-Caches vorbauen ----
#Mindesteinkommen (mit Fallback auf max(jahr))
mek_cache <- setNames(regelsaetze$mindesteinkommen, as.character(regelsaetze$jahr))
mek_max_jahr <- as.character(max(regelsaetze$jahr))
get_mek_fast <- function(j) {
  v <- mek_cache[as.character(j)]
  if (is.na(v)) mek_cache[mek_max_jahr] else v
}

#Barbetrag (mit Fallback auf max(jahr))
bb_cache <- setNames(schonwerte$barbeträge, as.character(schonwerte$jahr))
bb_max_jahr <- as.character(max(schonwerte$jahr))
get_bb_fast <- function(j) {
  v <- bb_cache[as.character(j)]
  if (is.na(v)) bb_cache[bb_max_jahr] else v
}

#PV-Pauschale (Liste nach Jahr -> benannter Vektor nach Pflegegrad)
pv_cache <- list()
pv_max_jahr <- as.character(max(pv_leistungen$jahr))
for (r in seq_len(nrow(pv_leistungen))) {
  j <- as.character(pv_leistungen$jahr[r])
  pv_cache[[j]] <- c(
    pv_leistungen$pflegegrad_1[r], pv_leistungen$pflegegrad_2[r],
    pv_leistungen$pflegegrad_3[r], pv_leistungen$pflegegrad_4[r],
    pv_leistungen$pflegegrad_5[r]
  )
}
get_pv_fast <- function(pflegegrad, j) {
  jc <- as.character(j)
  pv <- pv_cache[[jc]]
  if (is.null(pv)) pv <- pv_cache[[pv_max_jahr]]
  pv[pflegegrad]
}

# === PV-Cache vor GR-Anpassung (fuer Delta-Berechnung: EEE_GR = EEE_basis - delta_pv) ===
pv_null_cache <- list()
pv_null_max_jahr <- as.character(max(pv_null_werte$jahr))
for (r in seq_len(nrow(pv_null_werte))) {
  j <- as.character(pv_null_werte$jahr[r])
  pv_null_cache[[j]] <- c(
    pv_null_werte$pflegegrad_1[r], pv_null_werte$pflegegrad_2[r],
    pv_null_werte$pflegegrad_3[r], pv_null_werte$pflegegrad_4[r],
    pv_null_werte$pflegegrad_5[r]
  )
}
get_pv_null_fast <- function(pflegegrad, j) {
  jc <- as.character(j)
  pv <- pv_null_cache[[jc]]
  if (is.null(pv)) pv <- pv_null_cache[[pv_null_max_jahr]]
  pv[pflegegrad]
}

#Leistungszuschlag-Cache (Liste nach Jahr -> Funktion verweilmonat -> satz)
lz_cache <- list()
lz_max_jahr <- as.character(max(zuschlaege$jahr))
for (j in unique(zuschlaege$jahr)) {
  rows_j <- zuschlaege[zuschlaege$jahr == j, ]
  von <- rows_j$von_monat
  bis <- rows_j$bis_monat
  satz <- rows_j$zuschlag_prozent
  #Erstelle environment mit Vektoren
  lz_cache[[as.character(j)]] <- list(von = von, bis = bis, satz = satz)
}
get_lz_fast <- function(verweilmonat, aktuelles_jahr) {
  if (aktuelles_jahr < 2022) return(0)
  jc <- as.character(aktuelles_jahr)
  lz <- lz_cache[[jc]]
  if (is.null(lz)) lz <- lz_cache[[lz_max_jahr]]
  idx <- which(lz$von <= verweilmonat & lz$bis >= verweilmonat)
  if (length(idx) == 0) return(0)
  return(lz$satz[idx[1]])
}

#SV-Satz Cache (Liste nach Jahr -> Liste der Parameter)
sv_cache <- list()
sv_max_jahr_full <- max(sv_satz$jahr[!is.na(sv_satz$fam_rent)])
for (j in unique(sv_satz$jahr)) {
  sv_cache[[as.character(j)]] <- as.list(sv_satz[sv_satz$jahr == j, ])
}
get_sv_fast <- function(j) {
  jc <- as.character(j)
  params <- sv_cache[[jc]]
  if (is.null(params) || is.null(params$fam_rent) || is.na(params$fam_rent)) {
    params <- sv_cache[[as.character(sv_max_jahr_full)]]
  }
  params
}

#Schonwerte Cache (Liste nach Jahr -> Liste der Parameter)
schon_cache <- list()
schon_max_jahr <- as.character(max(schonwerte$jahr))
for (j in unique(schonwerte$jahr)) {
  schon_cache[[as.character(j)]] <- as.list(schonwerte[schonwerte$jahr == j, ])
}
get_schon_fast <- function(j) {
  jc <- as.character(j)
  params <- schon_cache[[jc]]
  if (is.null(params)) params <- schon_cache[[schon_max_jahr]]
  params
}

#Schonwerte NRW/SH Cache
nrw_cache <- list()
nrw_max_jahr <- as.character(max(schonwerte_nrw$jahr))
for (j in unique(schonwerte_nrw$jahr)) {
  nrw_cache[[as.character(j)]] <- as.list(schonwerte_nrw[schonwerte_nrw$jahr == j, ])
}
get_nrw_fast <- function(j) {
  jc <- as.character(j)
  params <- nrw_cache[[jc]]
  if (is.null(params)) params <- nrw_cache[[nrw_max_jahr]]
  params
}

sh_cache <- list()
sh_max_jahr <- as.character(max(schonwerte_sh$jahr))
for (j in unique(schonwerte_sh$jahr)) {
  sh_cache[[as.character(j)]] <- as.list(schonwerte_sh[schonwerte_sh$jahr == j, ])
}
get_sh_fast <- function(j) {
  jc <- as.character(j)
  params <- sh_cache[[jc]]
  if (is.null(params)) params <- sh_cache[[sh_max_jahr]]
  params
}

#Wohngeld-Höchstbeträge Cache (Mietstufe -> Jahr -> Betrag)
wg_hb_cache <- list()
wg_hb_spalten <- names(wohngeld_hoechstbetraege)[grepl("^betrag_", names(wohngeld_hoechstbetraege))]
wg_hb_jahre <- as.numeric(gsub("betrag_", "", wg_hb_spalten))
wg_hb_max_jahr <- max(wg_hb_jahre)
for (r in seq_len(nrow(wohngeld_hoechstbetraege))) {
  ms <- as.character(wohngeld_hoechstbetraege$mietstufe_roman[r])
  wg_hb_cache[[ms]] <- list()
  for (sp in wg_hb_spalten) {
    j <- gsub("betrag_", "", sp)
    wg_hb_cache[[ms]][[j]] <- wohngeld_hoechstbetraege[[sp]][r]
  }
}
get_wg_hb_fast <- function(mietstufe_roman, jahr) {
  ms <- as.character(mietstufe_roman)
  jc <- as.character(jahr)
  hb <- wg_hb_cache[[ms]]
  if (is.null(hb)) return(NA)
  v <- hb[[jc]]
  if (is.null(v)) v <- hb[[as.character(wg_hb_max_jahr)]]
  return(v)
}

# ---- A6: Schnelle berechne_einsetzbare_werte ----
berechne_einsetzbare_werte_fast <- function(rohwerte, partner_rohwerte, person_d11104,
                                             person_pgstib, person_kv_beitrag_pkv,
                                             person_bula, person_we_geschuetzt, jahr) {
  params <- get_schon_fast(jahr)
  sv_params <- get_sv_fast(jahr)

  partner_vorhanden <- (!is.na(person_d11104) && person_d11104 == 1)

  gesamteinkommen_monatlich <- round(
    rohwerte$rente_monatlich + rohwerte$haushaltseinkommen_aufgeteilt_monatlich, 0)

  kv_beitrag_pkv <- if (is.na(person_kv_beitrag_pkv)) 0 else person_kv_beitrag_pkv
  pgstib <- if (is.na(person_pgstib)) 0 else person_pgstib

  kv_pgstib <- 0
  if (pgstib %in% c(210:250, 510:550, 610:640)) {
    kv_pgstib <- min(rohwerte$arbeitseinkommen_ab_monatlich * sv_params$satz_ab, sv_params$hoechst_ab)
  } else if (pgstib %in% c(410:413, 430:433, 421:423)) {
    kv_pgstib <- min(max(rohwerte$arbeitseinkommen_sf_monatlich, sv_params$min_sf) * sv_params$satz_sf, sv_params$hoechst_sf)
  }

  kv_rentner <- 0
  if (gesamteinkommen_monatlich > sv_params$fam_rent) {
    kv_rentner <- min(rohwerte$grente_monatlich * sv_params$satz_rent, sv_params$hoechst_rent)
  }

  kv_betriebsrente <- max(0, min(max(rohwerte$brente_monatlich - sv_params$frei_betr, 0) * sv_params$satz_betr, sv_params$hoechst_betr))
  kv_beitrag <- if (kv_beitrag_pkv > 0) kv_beitrag_pkv else max(0, round(kv_pgstib + kv_rentner + kv_betriebsrente, 0))

  schon_bund <- if (partner_vorhanden) 2 * params$schon_bund else params$schon_bund

  if (!is.null(partner_rohwerte)) {
    gesamteinkommen_partner <- round(
      partner_rohwerte$rente_monatlich + partner_rohwerte$arbeitseinkommen_monatlich +
        partner_rohwerte$svleistungen_transfers_monatlich +
        partner_rohwerte$haushaltseinkommen_aufgeteilt_monatlich -
        partner_rohwerte$wohnkosten, 0)
  } else {
    gesamteinkommen_partner <- 0
  }

  einsetzungsfaehiges_partnereinkommen <- max(gesamteinkommen_partner - kv_beitrag - params$regelsatz, 0)
  einsetzungsfaehiges_individualeinkommen <- max(gesamteinkommen_monatlich - kv_beitrag - 50 - params$barbeträge, 0)
  einsetzungsfaehiges_einkommen_insgesamt <- max(round(einsetzungsfaehiges_partnereinkommen + einsetzungsfaehiges_individualeinkommen, 0), 0)
  einsetzbares_vermoegen <- max(rohwerte$gesamt_vermögen_ohne_anteiliges_wohneigentum - schon_bund, 0)
  gesamtvermoegen <- rohwerte$gesamt_vermögen_ohne_anteiliges_wohneigentum

  wohneigentum_gesamt <- rohwerte$wohneigentum_gesamter_haushalt
  wohneigentum_sonstig <- rohwerte$sonstiges_wohneigentum_gesamter_haushalt
  wohneigentum_verzehrbar <- if (person_we_geschuetzt) wohneigentum_sonstig else wohneigentum_gesamt

  list(
    einsetzungsfaehiges_einkommen_insgesamt = einsetzungsfaehiges_einkommen_insgesamt,
    einsetzungsfaehiges_individualeinkommen = einsetzungsfaehiges_individualeinkommen,
    einsetzungsfaehiges_partnereinkommen = einsetzungsfaehiges_partnereinkommen,
    einsetzbares_vermoegen = einsetzbares_vermoegen,
    gesamtvermoegen = gesamtvermoegen,
    wohneigentum = wohneigentum_verzehrbar,
    wohneigentum_gesamt = wohneigentum_gesamt,
    wohneigentum_selbstgenutzt = rohwerte$selbstgenutztes_wohneigentum_gesamter_haushalt,
    wohneigentum_sonstig = wohneigentum_sonstig,
    kv_beitrag = kv_beitrag,
    gesamteinkommen_monatlich = gesamteinkommen_monatlich
  )
}

# ---- A7: Basiswerte-Extraktion Hilfsfunktion ----
extract_basis_vals <- function(person_row, basis_jahr) {
  result <- list()
  for (v in renten_vars) {
    var_name <- paste0(v, "_", basis_jahr)
    val <- if (var_name %in% names(person_row)) as.numeric(person_row[[var_name]]) else 0
    result[[v]] <- if (is.na(val)) 0 else val
  }
  for (v in inflation_vars) {
    var_name <- paste0(v, "_", basis_jahr)
    val <- if (var_name %in% names(person_row)) as.numeric(person_row[[var_name]]) else 0
    result[[v]] <- if (is.na(val)) 0 else val
  }
  for (v in haus_vars) {
    var_name <- paste0(v, "_", basis_jahr)
    val <- if (var_name %in% names(person_row)) as.numeric(person_row[[var_name]]) else 0
    result[[v]] <- if (is.na(val)) 0 else val
  }
  return(result)
}

cat("  Alle Parameter-Caches gebaut.\n")
cat("Performance-Caches fertig.\n\n")

# ============================================================
# HAUPTSCHLEIFE: 12 STICHTAGE (2024-2035)
# ============================================================
stichtage <- as.Date(c("2024-07-01", "2025-07-01", "2026-07-01", "2027-07-01", "2028-07-01", "2029-07-01", "2030-07-01", "2031-07-01", "2032-07-01", "2033-07-01", "2034-07-01", "2035-07-01"))

cat("\n")
cat("=====================================================\n")
cat("  Mikrosimulation - Pflegebedingte Sozialhilfe\n")
cat(paste0("  Geltendes Recht BA: Stichtage 2024-2035, Lauf ", lauf_tag, "\n"))
cat("=====================================================\n")
cat("Stichtage:", paste(stichtage, collapse=", "), "\n")
cat("Zielgröße:", N, "Personen\n\n")

#Wohngeld-Zuweisungsdaten (alle 24 Monate)
WOHNGELD_ZUWEISUNGSDATEN <- as.Date(c("2023-01-01", "2025-01-01", "2027-01-01",
                                      "2029-01-01", "2031-01-01", "2033-01-01", "2035-01-01"))

#Ergebnis-Speicher für alle Jahre
alle_ergebnisse <- data.frame(
  stichtag = as.Date(character()),
  n_simuliert = integer(),
  n_beamte = integer(),
  beamte_anteil = numeric(),
  hzp_quote = numeric(),
  hzp_empfaenger_sim = integer(),
  hzp_empfaenger_hoch = numeric(),
  hzp_ausgaben_pro_empf = numeric(),
  hzp_ausgaben_mrd = numeric(),
  wohngeld_berechtigt_quota = integer(),
  wohngeld_berechtigt_stichtag = integer(),
  wohngeld_bezogen_sim = integer(),
  wohngeld_bezogen_hoch = numeric(),
  wohngeld_betrag_avg = numeric(),
  wohngeld_ausgaben_mrd = numeric(),
  pfwg_quote = numeric(),
  pfwg_empfaenger_hoch = numeric(),
  pfwg_ausgaben_mrd = numeric(),
  privat_mrd = numeric(),
  pv_mrd = numeric(),
  eigenanteil_q1_avg = numeric(),
  eee_q1_avg = numeric(),
  ak_q1_avg = numeric(),
  uv_q1_avg = numeric(),
  ik_q1_avg = numeric(),
  zuschlag_q1_avg = numeric(),
  eigenanteil_stichtag_avg = numeric(),
  eee_q3_avg = numeric(),
  ak_q3_avg = numeric(),
  uv_q3_avg = numeric(),
  ik_q3_avg = numeric(),
  zuschlag_q3_avg = numeric(),
  stringsAsFactors = FALSE
)

#Detaillierte Ergebnisse pro Stichtag
alle_results_detail <- list()

for (stichtag_idx in seq_along(stichtage)) {

  stichtag <- stichtage[stichtag_idx]
  set.seed(as.integer(stichtag) + lauf_nr)  # Seed variiert per Lauf und Stichtag

  cat("\n\n")
  cat("################################################################\n")
  cat("###  STICHTAG", stichtag_idx, "von", length(stichtage), ":", as.character(stichtag), "  ###\n")
  cat("################################################################\n\n")

  #Relevante Wohngeld-Zuweisungen für diesen Stichtag
  relevante_zuweisungen <- WOHNGELD_ZUWEISUNGSDATEN[WOHNGELD_ZUWEISUNGSDATEN <= stichtag]
  cat("Wohngeld-Runden:", length(relevante_zuweisungen), "\n")
  if (length(relevante_zuweisungen) > 0) {
    cat("Zuweisungsdaten:", paste(relevante_zuweisungen, collapse = ", "), "\n")
  }
  cat("\n")

# ---- Schritt 3: Heimpopulation ziehen (Bootstrapping) ----

cat("=== Heimpopulation ziehen ===\n")

#Personendatensatz vorbereiten
personen$geschlechtF <- ifelse(personen$geschlecht == 1, "male", "female")
personen$altersgruppe <- ifelse(personen$alter >= 80, "80plus", "65_80")
personen <- subset(personen, !is.na(w11105) & w11105 > 0)

  #Kein fester Seed: echtes Bootstrapping bei jedem Lauf

  #---- Immobilienbesitz-Korrektur nach Rothgang et al. (2008: 46) ----
  #Immobilienbesitz verringert Heimeintrittswahrscheinlichkeit um 58%.
  #Ziehungsgewichte von Personen mit WE entsprechend reduzieren.
  personen$ziehungsgewicht <- personen$w11105
  hat_we <- personen$wohneigentum_gesamter_haushalt_2022 > 0
  hat_we[is.na(hat_we)] <- FALSE
  personen$ziehungsgewicht[hat_we] <- personen$w11105[hat_we] * 0.42

gezogene <- list()

for (i in seq_len(nrow(pflege_ziel))) {
  b <- pflege_ziel$bula[i]
  
  #Männlich 65-80
  grp <- subset(personen, bula == b & geschlechtF == "male" & altersgruppe == "65_80")
  n_ziel <- pflege_ziel$target_m_65_80[i]
  if (nrow(grp) > 0 && n_ziel > 0) {
    idx <- sample(seq_len(nrow(grp)), size = n_ziel, replace = TRUE, prob = grp$ziehungsgewicht)
    gezogene[[paste0("m_65_80_", b)]] <- grp[idx, ]
  }
  
  #Weiblich 65-80
  grp <- subset(personen, bula == b & geschlechtF == "female" & altersgruppe == "65_80")
  n_ziel <- pflege_ziel$target_f_65_80[i]
  if (nrow(grp) > 0 && n_ziel > 0) {
    idx <- sample(seq_len(nrow(grp)), size = n_ziel, replace = TRUE, prob = grp$ziehungsgewicht)
    gezogene[[paste0("f_65_80_", b)]] <- grp[idx, ]
  }
  
  #Männlich 80+
  grp <- subset(personen, bula == b & geschlechtF == "male" & altersgruppe == "80plus")
  n_ziel <- pflege_ziel$target_m_80plus[i]
  if (nrow(grp) > 0 && n_ziel > 0) {
    idx <- sample(seq_len(nrow(grp)), size = n_ziel, replace = TRUE, prob = grp$ziehungsgewicht)
    gezogene[[paste0("m_80plus_", b)]] <- grp[idx, ]
  }
  
  #Weiblich 80+
  grp <- subset(personen, bula == b & geschlechtF == "female" & altersgruppe == "80plus")
  n_ziel <- pflege_ziel$target_f_80plus[i]
  if (nrow(grp) > 0 && n_ziel > 0) {
    idx <- sample(seq_len(nrow(grp)), size = n_ziel, replace = TRUE, prob = grp$ziehungsgewicht)
    gezogene[[paste0("f_80plus_", b)]] <- grp[idx, ]
  }
}

heimpop <- bind_rows(gezogene)
heimpop$sim_id <- seq_len(nrow(heimpop))

#Pflegegrad zuweisen basierend auf Alters-/Geschlechtsgruppe
zuweise_pflegegrad <- function(alter, geschlecht) {
  gruppe <- case_when(
    geschlecht == 1 & alter >= 65 & alter < 80 ~ "m_65_80",
    geschlecht == 1 & alter >= 80 ~ "m_80plus",
    geschlecht == 2 & alter >= 65 & alter < 80 ~ "f_65_80",
    geschlecht == 2 & alter >= 80 ~ "f_80plus",
    TRUE ~ "m_80plus"
  )

  anteile <- pflegegrad_verteilung[pflegegrad_verteilung$gruppe == gruppe, ]
  probs <- c(anteile$pg1, anteile$pg2, anteile$pg3, anteile$pg4, anteile$pg5)

  sample(1:5, 1, prob = probs)
}

heimpop$pflegegrad <- mapply(zuweise_pflegegrad, heimpop$alter, heimpop$geschlecht)

cat("Gezogene Gesamtzahl:", nrow(heimpop), "\n")
cat("Pflegegrad-Verteilung:\n")
print(table(heimpop$pflegegrad))
cat("\n")

# ---- Schritt 4: Heimplätze zuordnen ----

cat("=== Heimplätze zuordnen ===\n")

heimpop$heim_id <- NA

for (b in unique(heimpop$bula)) {
  heime_b <- heime %>% filter(bula == b)
  if (nrow(heime_b) == 0) next
  
  pers_idx <- which(heimpop$bula == b)
  n_pers <- length(pers_idx)
  
  probs <- heime_b$platzzahl / sum(heime_b$platzzahl)
  
  heimpop$heim_id[pers_idx] <- sample(
    heime_b$heim_id,
    size = n_pers,
    replace = TRUE,
    prob = probs
  )
}

cat("Zuweisung abgeschlossen.\n\n")

#Wohngeldstufe aus Heimdaten übernehmen (römisch, z.B. "III")
heimpop <- heimpop %>%
  left_join(heime %>% select(heim_id, wohngeldstufe), by = "heim_id")

# ---- Schritt 5: Einzugsdaten zuordnen ----

cat("=== Einzugsdaten zuordnen ===\n")

verweildauer_pfad <- paste0(basepfad, "Dataset Verweildauer.csv")

#Verweildauer-Daten einlesen (kein Fallback - Datei muss vorhanden sein)
stay_raw <- read.csv2(verweildauer_pfad, dec = ",")
names(stay_raw) <- c("x_raw", "F_raw")
stay_raw$F_raw <- stay_raw$F_raw / 100

x_min <- min(stay_raw$x_raw)
x_max <- max(stay_raw$x_raw)
stay_raw$Monat <- (stay_raw$x_raw - x_min) / (x_max - x_min) * 297
stay_raw$S <- 1 - stay_raw$F_raw
stay_raw$S[1] <- 1

stay_full <- data.frame(Monat = 0:297) %>%
  mutate(S = approx(stay_raw$Monat, stay_raw$S, xout = Monat, rule = 2)$y)

#Verweildauer-Begrenzung laut Bericht:
#- Maximal 240 Monate vor dem Stichtag (frühestes Einzugsdatum)
#- Minimal im 6. Monat (Juni) des Stichtag-Jahres (spätestes Einzugsdatum)
max_monate <- 240
max_einzugsdatum <- as.Date(paste0(year(stichtag), "-06-01"))  # spätestens Juni des Stichtag-Jahres
cat("Maximale Verweildauer:", max_monate, "Monate\n")
cat("Spätestes Einzugsdatum:", as.character(max_einzugsdatum), "\n")

stay_cut <- stay_full %>% filter(Monat <= max_monate)
S_cut <- stay_cut$S
p_cut <- c(S_cut[-length(S_cut)] - S_cut[-1], S_cut[length(S_cut)])
p_cut[p_cut < 0] <- 0
p_cut <- p_cut / sum(p_cut)

verweildauer <- sample(0:(length(p_cut)-1), size = nrow(heimpop), replace = TRUE, prob = p_cut)

heimpop$verweildauer_monate <- verweildauer
heimpop$einzugsdatum <- stichtag %m-% months(verweildauer)
heimpop$einzugsdatum <- pmin(heimpop$einzugsdatum, max_einzugsdatum)  # nicht später als Juni des Stichtag-Jahres
heimpop$verweildauer_monate <- as.numeric(interval(heimpop$einzugsdatum, stichtag) %/% months(1))

cat("Einzugsdaten zugeordnet.\n\n")

# ---- Wohngeld Plus: Initialisierung ----

#Konstanten
WOHNGELD_STARTDATUM <- as.Date("2023-01-01")
WOHNGELD_MAX_MONATE <- 24

#Neue Variablen pro Person
heimpop$wohngeld_monate_bezogen <- 0L
heimpop$wohngeld_betrag_aktuell <- 0
heimpop$wohngeld_zugewiesen <- FALSE
heimpop$einkommen_roh_dez2022 <- 0
heimpop$vermoegen_dez2022 <- 0
heimpop$gesamteinkommen_dez2022 <- 0
heimpop$uv_kosten_dez2022 <- 0
heimpop$heimkosten_effektiv_dez2022 <- 0
heimpop$gesamtvermoegen_dez2022 <- 0

#State-Speicher für Zwei-Phasen-Simulation
phase1_ende <- as.Date("2022-12-01")
heimpop$vermoegen_end_phase1 <- 0
heimpop$gesamtvermoegen_end_phase1 <- 0
heimpop$vermoegen_pfwg_end_phase1 <- 0
heimpop$wohneigentum_end_phase1 <- 0
heimpop$verweilmonat_end_phase1 <- 1L

#State-Speicher für Runden-Übergabe (wird nach Phase 1 auf end_phase1 initialisiert,
#dann nach jeder Wohngeld-Runde aktualisiert, damit Runde 2+ nahtlos weiterlaufen)
heimpop$vermoegen_end_prev_runde <- 0
heimpop$gesamtvermoegen_end_prev_runde <- 0
heimpop$vermoegen_pfwg_end_prev_runde <- 0
heimpop$wohneigentum_end_prev_runde <- 0
heimpop$verweilmonat_end_prev_runde <- 1L
#Für WG-Berechtigungsprüfung: Einkommen und Kosten am Runden-Ende
heimpop$gesamteinkommen_end_prev_runde <- 0
heimpop$heimkosten_effektiv_end_prev_runde <- 0
heimpop$gesamtvermoegen_end_prev_runde_wg <- 0

# ---- Schritt 6: Simulation ----

cat("=== Phase 1: Simulation bis Dezember 2022 ===\n\n")

#Investitionskosten-Korrekturfaktoren
#Grund: Heimkostendaten sind inkonsistent (ungewichtete Zusammenrechnung)
INV_FAKTOR_QUOTE <- 1.0      # GR: Keine IK-Korrektur
INV_FAKTOR_AUSGABEN <- 1.0   # GR: Keine IK-Korrektur

n_sim <- nrow(heimpop)

# ---- WE-Schutz nach § 90 Abs. 2 Nr. 8 SGB XII ----
#Laut Kantar (2019: 220) leben nur 12% der Heimbewohner in einer Partnerschaft.
#Bei diesen wird das selbstgenutzte Wohneigentum geschützt (Partner wohnt darin).
#Bei den übrigen 88% wird das gesamte WE (inkl. selbstgenutztes) verzehrt.
heimpop$we_geschuetzt <- sample(c(TRUE, FALSE), n_sim, replace = TRUE, prob = c(0.12, 0.88))


# ============================================================
# PRE-COMPUTATION: Einmalig pro Person (nicht pro Stichtag)
# Optimierung: extract_basis_vals + partner_cache nur 1x pro Person
# ============================================================
cat("Pre-computing person-level caches...\n")
hp_einzug        <- as.Date(heimpop$einzugsdatum)
hp_einzugsjahr   <- as.integer(format(hp_einzug, "%Y"))
hp_basis_jahr    <- ifelse(hp_einzugsjahr <= 2019L, 2017L, 2022L)
hp_bula          <- heimpop$bula
hp_heim_id       <- heimpop$heim_id
hp_pid_str       <- as.character(heimpop$pid)
hp_d11104        <- heimpop$d11104
hp_pgstib        <- as.numeric(heimpop$pgstib)
hp_kv_pkv        <- as.numeric(heimpop$kv_beitrag_pkv)
hp_we            <- heimpop$we_geschuetzt
hp_wohngeldstufe <- heimpop$wohngeldstufe
hp_pflegegrad    <- heimpop$pflegegrad
hp_is_nrw        <- hp_bula == 5L
hp_is_sh         <- hp_bula == 1L
hp_is_sonstig    <- !hp_is_nrw & !hp_is_sh

#Partner-Cache: einmal nachschlagen
hp_partner <- lapply(hp_pid_str, function(pid) partner_cache[[pid]])

#extract_basis_vals: einmal pro Person statt einmal pro Person x Stichtag
#WICHTIG: hp_partner_basis[[.i]] <- NULL würde das Element löschen (R-Verhalten)!
#Daher: nur zuweisen wenn Partner vorhanden, sonst initial-NULL behalten.
hp_person_basis  <- vector("list", n_sim)
hp_partner_basis <- vector("list", n_sim)
for (.i in seq_len(n_sim)) {
  hp_person_basis[[.i]] <- extract_basis_vals(heimpop[.i, ], hp_basis_jahr[.i])
  if (!is.null(hp_partner[[.i]])) {
    hp_partner_basis[[.i]] <- extract_basis_vals(hp_partner[[.i]], hp_basis_jahr[.i])
  }
}
cat("Pre-computation abgeschlossen.\n\n")

results <- data.frame(
  sim_id = heimpop$sim_id,
  pid = heimpop$pid,
  bula = heimpop$bula,
  einzugsdatum = heimpop$einzugsdatum,
  verweildauer_monate = heimpop$verweildauer_monate,
  pflegegrad = heimpop$pflegegrad,
  ist_beamter = heimpop$iciv1 > 0,
  hzp_bedarf = FALSE,
  hzp_betrag = 0,
  pfwg_bedarf = FALSE,
  pfwg_betrag = 0,
  vermoegen_rest = 0,
  wohneigentum_rest = 0,
  #Neue Spalten für erweiterte Endpunkte
  heimkosten_brutto = 0,         # Gesamtheimkosten vor Zuschlag
  eigenanteil_nach_zuschlag = 0, # Eigenanteil nach § 43c Zuschlag
  zuschlag_betrag = 0,           # Leistungszuschlag § 43c
  pv_pauschale = 0,              # PV-Pauschale nach Pflegegrad (§ 43 SGB XI)
  pv_leistung = 0,               # PV-Ausgaben gesamt (Pauschale + Zuschlag)
  privat_getragen = 0,           # Selbst getragener Anteil
  uv_kosten = 0,                 # Unterkunft & Verpflegung
  inv_kosten = 0,                # Investitionskosten
  eee_kosten = 0,                # Einrichtungseinheitlicher Eigenanteil (pflegebed. EA)
  ak_kosten = 0,                 # Ausbildungskosten
  einkommen_monat = 0,           # Einsetzungsfähiges Einkommen im Stichtagsmonat
  wohngeld_bedarf = FALSE,       # Wohngeld Plus bezogen am Stichtag
  wohngeld_betrag = 0,           # Wohngeld-Betrag am Stichtag (EUR/Monat)
  bezieht_mindesteinkommen_2022 = FALSE, # Marker: wurde Ende 2022 auf Mindesteinkommen aufgestockt?
  bezieht_grundsicherung = FALSE,        # Grundsicherung/Lebensunterhalt am Stichtag?
  wohngeld_berechtigt_stichtag = FALSE, # Wohngeldberechtigt am Stichtag
  wohngeld_zugewiesen = FALSE,   # Per Quote zugewiesen?
  heimkosten_brutto_q1 = NA_real_,  # Q1-Werte (1.1.JAHR) – NA wenn Bewohner noch nicht eingezogen
  eigenanteil_nach_zuschlag_q1 = NA_real_,
  zuschlag_betrag_q1 = NA_real_,
  uv_kosten_q1 = NA_real_,
  inv_kosten_q1 = NA_real_,
  eee_kosten_q1 = NA_real_,
  ak_kosten_q1 = NA_real_,
  mek_aufstockung = 0,          # MEK-Aufstockungsbetrag am Q3-Stichtag (EUR/Monat)
  mek_aufstockung_q1 = NA_real_ # MEK-Aufstockungsbetrag am Q1-Stichtag (EUR/Monat)
)

for (i in seq_len(n_sim)) {
  
  #Vorab extrahierte Vektoren
  einzug       <- hp_einzug[i]
  einzugsjahr  <- hp_einzugsjahr[i]
  bula         <- hp_bula[i]
  heim_id_val  <- hp_heim_id[i]
  basis_jahr   <- hp_basis_jahr[i]

  #Partner + Basiswerte: vorberechnet (nicht stichtagsabhängig)
  partner_row       <- hp_partner[[i]]
  ist_ost           <- (bula >= 11)
  person_basis_vals <- hp_person_basis[[i]]
  partner_basis_vals <- hp_partner_basis[[i]]
  person_d11104 <- hp_d11104[i]
  person_pgstib <- hp_pgstib[i]
  person_kv_pkv        <- hp_kv_pkv[i]
  person_we            <- hp_we[i]
  person_wohngeldstufe <- hp_wohngeldstufe[i]
  person_pflegegrad    <- hp_pflegegrad[i]

  #Vermögensverzehr-Tracker (startet bei Einzug)
  rohwerte_einzug <- fortschreibe_rohwerte_fast(person_basis_vals, ist_ost, basis_jahr, format(einzug, "%Y-%m"), einzugsjahr)
  partner_rohwerte_einzug <- if (!is.null(partner_basis_vals)) fortschreibe_rohwerte_fast(partner_basis_vals, ist_ost, basis_jahr, format(einzug, "%Y-%m"), einzugsjahr) else NULL
  einsetz_einzug <- berechne_einsetzbare_werte_fast(rohwerte_einzug, partner_rohwerte_einzug, person_d11104, person_pgstib, person_kv_pkv, bula, person_we, einzugsjahr)
  
  #---- Vermögen bei Einzug ----
  #Kein Vorverzehr-Abschlag: Vermögenseffekte häuslicher Pflege werden
  #ausschließlich über die Einzugswahrscheinlichkeiten abgebildet.
  vermoegen_aktuell <- einsetz_einzug$einsetzbares_vermoegen
  gesamtvermoegen_aktuell <- einsetz_einzug$gesamtvermoegen
  wohneigentum_aktuell <- einsetz_einzug$wohneigentum

  # ---- PfWG-Vermögen für NRW/SH (höhere Schongrenzen) ----
  #In NRW/SH gibt es zweistufigen Verzehr mit unterschiedlichen Schongrenzen
  vermoegen_pfwg_aktuell <- 0
  if (bula == 5) {
    #NRW: Schonvermögen 10.000/15.000
    nrw_p <- get_nrw_fast(einzugsjahr)
    schon_pfwg <- if (person_d11104 == 1) nrw_p$schon_vermoegen_part else nrw_p$schon_vermoegen_ind
    vermoegen_pfwg_aktuell <- max(einsetz_einzug$gesamtvermoegen - schon_pfwg, 0)
  } else if (bula == 1) {
    #SH: Schonvermögen 6.900/11.900
    sh_p <- get_sh_fast(einzugsjahr)
    schon_pfwg <- if (person_d11104 == 1) sh_p$schon_vermoegen_part else sh_p$schon_vermoegen_ind
    vermoegen_pfwg_aktuell <- max(einsetz_einzug$gesamtvermoegen - schon_pfwg, 0)
  }

  #Simuliere Monat für Monat
  current_date <- einzug
  verweilmonat <- 1
  hzp_bedarf_monat <- FALSE
  hzp_betrag_monat <- 0
  pfwg_bedarf_monat <- FALSE
  pfwg_betrag_monat <- 0
  wohngeld_betrag_monat <- 0
  wg_zugewiesen_i <- heimpop$wohngeld_zugewiesen[i]
  wg_monate_i <- heimpop$wohngeld_monate_bezogen[i]

  phase1_ziel <- min(phase1_ende, stichtag)

  #Datum-Sequenz vorberechnen (ersetzt %m+% months(1) - Hauptoptimierung)
  #Guard: leere Sequenz wenn einzug > phase1_ziel (Einzug nach Dez 2022)
  .dates_p1  <- if (einzug <= phase1_ziel) seq.Date(from = einzug, to = phase1_ziel, by = "month") else as.Date(character(0))
  .jahre_p1  <- year(.dates_p1)
  .monate_p1 <- month(.dates_p1)
  .is_wg_p1  <- .dates_p1 >= WOHNGELD_STARTDATUM
  .yymm_p1  <- format(.dates_p1, "%Y-%m")       # Pre-compute format keys
  .qkeys_p1 <- paste0("q", ceiling(.monate_p1 / 3L), "_", .jahre_p1)  # Heimkosten-Keys

  for (.k in seq_along(.dates_p1)) {
    current_date  <- .dates_p1[.k]
    current_jahr  <- .jahre_p1[.k]
    current_monat <- .monate_p1[.k]
    
    #Fortschreibung der Rohwerte auf aktuellen Monat (Person UND Partner)
    rohwerte_aktuell <- fortschreibe_rohwerte_fast(person_basis_vals, ist_ost, basis_jahr, .yymm_p1[.k], current_jahr)
    partner_rohwerte_aktuell <- if (!is.null(partner_basis_vals)) fortschreibe_rohwerte_fast(partner_basis_vals, ist_ost, basis_jahr, .yymm_p1[.k], current_jahr) else NULL

    #Einsetzbare Werte nach aktueller Gesetzeslage berechnen
    einsetz_aktuell <- berechne_einsetzbare_werte_fast(rohwerte_aktuell, partner_rohwerte_aktuell, person_d11104, person_pgstib, person_kv_pkv, bula, person_we, current_jahr)

    #Einkommen VOR Aufstockung speichern
    einkommen_roh <- einsetz_aktuell$einsetzungsfaehiges_einkommen_insgesamt

    #Mindesteinkommen prüfen (auf Individualeinkommen, nicht Gesamt)
    mindesteinkommen <- get_mek_fast(current_jahr)
    einsetzf_ind <- einsetz_aktuell$einsetzungsfaehiges_individualeinkommen
    einsetzf_partner <- einsetz_aktuell$einsetzungsfaehiges_partnereinkommen
    einsetzf_ind_effektiv <- max(einsetzf_ind, mindesteinkommen)
    einkommen <- einsetzf_ind_effektiv + einsetzf_partner

    #Grundsicherung-Marker (für Stichtag-Speicherung)
    grundsicherung_bezogen <- (einsetzf_ind < mindesteinkommen)
    mek_aufstockung_val    <- max(0, mindesteinkommen - einsetzf_ind)

    #Marker setzen am Ende 2022: Wurde auf Mindesteinkommen aufgestockt?
    if (current_jahr == 2022L && current_monat == 12L) {
      results$bezieht_mindesteinkommen_2022[i] <- (einkommen_roh < mindesteinkommen)
      heimpop$einkommen_roh_dez2022[i] <- einkommen_roh
      heimpop$vermoegen_dez2022[i] <- vermoegen_aktuell
      heimpop$gesamtvermoegen_dez2022[i] <- gesamtvermoegen_aktuell
      heimpop$gesamteinkommen_dez2022[i] <- einsetz_aktuell$gesamteinkommen_monatlich
    }
    
    #Kostenkomponenten (individuell aus Datensatz!)
    kosten <- kosten_cache[[as.character(heim_id_val)]][[.qkeys_p1[.k]]]

    if (is.na(kosten$gesamt)) {
      stop(paste("Keine Heimkosten für Person", i, "Heim", heim_id_val, "Datum", current_date))
    }

    #Kostenkomponenten auslesen
    pflegebedingter_ea <- if (is.na(kosten$eee)) 0 else kosten$eee
    investitionskosten_raw <- if (is.na(kosten$inv)) 0 else kosten$inv
    ausbildungskosten <- if (is.na(kosten$ak)) 0 else kosten$ak

    #Ab 2028: EEE anpassen um PV-Delta (Reform erhoeht PV -> senkt EEE -> senkt EA)
    if (current_jahr >= 2028) {
      delta_pv <- get_pv_fast(person_pflegegrad, current_jahr) - get_pv_null_fast(person_pflegegrad, current_jahr)
      pflegebedingter_ea <- max(0, pflegebedingter_ea - delta_pv)
    }

    #IK-Varianten für unterschiedliche Berechnungen
    investitionskosten_quote <- investitionskosten_raw * INV_FAKTOR_QUOTE
    investitionskosten_ausgaben <- investitionskosten_raw * INV_FAKTOR_AUSGABEN

    #Leistungszuschlag § 43c SGB XI (basiert auf EEE + AK)
    zuschlag_satz <- get_lz_fast(verweilmonat, current_jahr)
    zuschlag_betrag <- (pflegebedingter_ea + ausbildungskosten) * zuschlag_satz

    #Heimkosten: vor 2028 identisch zu Null (kosten$eee direkt), ab 2028 mit Reform-EA
    if (current_jahr >= 2028) {
      heimkosten_gesamt_quote <- pflegebedingter_ea + kosten$uv + investitionskosten_quote + ausbildungskosten
      heimkosten_gesamt_ausgaben <- pflegebedingter_ea + kosten$uv + investitionskosten_ausgaben + ausbildungskosten
    } else {
      heimkosten_gesamt_quote <- kosten$eee + kosten$uv + investitionskosten_quote + ausbildungskosten
      heimkosten_gesamt_ausgaben <- kosten$eee + kosten$uv + investitionskosten_ausgaben + ausbildungskosten
    }
    heimkosten_effektiv <- heimkosten_gesamt_quote - zuschlag_betrag
    heimkosten_effektiv_ausgaben <- heimkosten_gesamt_ausgaben - zuschlag_betrag

    #Kosten Ende 2022 speichern (für Wohngeld-Quote-Berechnung)
    if (current_jahr == 2022L && current_monat == 12L) {
      heimpop$uv_kosten_dez2022[i] <- kosten$uv
      heimpop$heimkosten_effektiv_dez2022[i] <- heimkosten_effektiv
    }

    #Deckungslücke (NA-sicher) - basierend auf Quote-Heimkosten (bestimmt HzP-Bedarf)
    deckungsluecke <- as.numeric(heimkosten_effektiv - einkommen)[1]
    if (is.na(deckungsluecke)) deckungsluecke <- 0

    #Deckungslücke OHNE Mindesteinkommen (für Wohngeld-Prüfung ab 2023)
    #Wohngeld soll Mindesteinkommen/Grundsicherung ERSETZEN, nicht ergänzen
    deckungsluecke_ohne_mek <- as.numeric(heimkosten_effektiv - einkommen_roh)[1]
    if (is.na(deckungsluecke_ohne_mek)) deckungsluecke_ohne_mek <- 0

    #Deckungslücke für Ausgaben-Berechnung (bestimmt HzP-Betrag)
    deckungsluecke_ausgaben <- as.numeric(heimkosten_effektiv_ausgaben - einkommen)[1]
    if (is.na(deckungsluecke_ausgaben)) deckungsluecke_ausgaben <- 0

    hzp_bedarf_monat <- FALSE
    hzp_betrag_monat <- 0
    pfwg_bedarf_monat <- FALSE
    pfwg_betrag_monat <- 0

    #Werte für Endpunkt-Berechnung speichern (werden am Ende des Loops aktualisiert)
    heimkosten_brutto_monat <- heimkosten_gesamt_quote
    eigenanteil_nach_zuschlag_monat <- heimkosten_effektiv
    zuschlag_betrag_monat <- zuschlag_betrag
    uv_kosten_monat <- kosten$uv
    inv_kosten_monat <- investitionskosten_raw
    eee_kosten_monat <- pflegebedingter_ea
    ak_kosten_monat <- ausbildungskosten
    einkommen_monat_val <- einkommen

    #Q1-Werte speichern (1.1. des Stichjahres)
    q1_datum <- as.Date(paste0(year(stichtag), "-01-01"))
    if (current_date == q1_datum) {
      results$heimkosten_brutto_q1[i] <- heimkosten_gesamt_quote
      results$eigenanteil_nach_zuschlag_q1[i] <- heimkosten_effektiv
      results$zuschlag_betrag_q1[i] <- zuschlag_betrag
      results$uv_kosten_q1[i] <- kosten$uv
      results$inv_kosten_q1[i] <- investitionskosten_raw
      results$eee_kosten_q1[i] <- pflegebedingter_ea
      results$ak_kosten_q1[i] <- ausbildungskosten
    }

    #Ab 2023: Prüfung mit Deckungslücke OHNE Mindesteinkommen
    #(Wohngeld soll Mindesteinkommen ersetzen, daher muss ohne geprüft werden)
    deckungsluecke_check <- if (.is_wg_p1[.k]) {
      deckungsluecke_ohne_mek
    } else {
      deckungsluecke
    }

    if (deckungsluecke_check > 0) {

      #Vermögensverzehr - NUR für Nicht-NRW/SH
      #In NRW/SH erfolgt der Verzehr zweistufig in der NRW/SH-Logik unten
      if (!(bula %in% c(1, 5))) {
        #Vermögensverzehr (Vermögen wird über die Monate aufgebraucht)
        if (vermoegen_aktuell > 0) {
          verzehr <- min(deckungsluecke, vermoegen_aktuell)
          vermoegen_aktuell <- vermoegen_aktuell - verzehr
          gesamtvermoegen_aktuell <- gesamtvermoegen_aktuell - verzehr
          deckungsluecke <- deckungsluecke - verzehr
          #Parallel für Ausgaben-Berechnung (gleicher Verzehr)
          deckungsluecke_ausgaben <- max(0, deckungsluecke_ausgaben - verzehr)
        }

        #Wohneigentum-Schonregelung nach § 90 Abs. 2 Nr. 8 SGB XII implementiert:
        #Bei Partner im Eigenheim (d11104 == 1) wird nur sonstiges Wohneigentum verzehrt.
        #Das selbstgenutzte Eigenheim bleibt geschützt.
        #Die Unterscheidung erfolgt bereits in berechne_einsetzbare_werte().
        if (deckungsluecke > 0 && wohneigentum_aktuell > 0) {
          verzehr <- min(deckungsluecke, wohneigentum_aktuell)
          wohneigentum_aktuell <- wohneigentum_aktuell - verzehr
          deckungsluecke <- deckungsluecke - verzehr
          deckungsluecke_ausgaben <- max(0, deckungsluecke_ausgaben - verzehr)
        }
      }

      # ---- Wohngeld Plus Prüfung (ab 2023) ----
      wohngeld_betrag_monat <- 0
      wohngeld_berechtigt <- FALSE

      if (.is_wg_p1[.k] &&
          !(bula %in% c(1, 5)) &&
          wg_monate_i < WOHNGELD_MAX_MONATE &&
          gesamtvermoegen_aktuell <= 60000) {

        #Wohngeldeinkommen berechnen
        #KEIN Mindesteinkommen-Abzug - das ist Grundsicherung/Sozialhilfe, die wir vermeiden wollen
        barbetrag <- get_bb_fast(current_jahr)
        gesamteinkommen_aktuell <- einsetz_aktuell$gesamteinkommen_monatlich
        wohngeldeinkommen <- gesamteinkommen_aktuell * 0.90 - barbetrag - 50

        #Lücke = Gesamte Heimkosten minus Wohngeldeinkommen
        #(Wohngeld vermeidet nur SH wenn ALLE Kosten gedeckt)
        wohngeld_luecke <- heimkosten_effektiv - wohngeldeinkommen

        if (wohngeld_luecke > 0) {
          #Maximaler Wohngeld-Betrag
          wg_hoechstbetrag <- get_wg_hb_fast(
            person_wohngeldstufe, current_jahr)
          max_wohngeld <- min(wohngeld_luecke, wg_hoechstbetrag)

          #Berechtigt wenn Lücke > 0 (Kombination mit HzP jetzt möglich)
          wohngeld_berechtigt <- TRUE

          #Tatsächlich Wohngeld nur wenn per Quote zugewiesen
          if (wg_zugewiesen_i == TRUE) {
            wohngeld_betrag_monat <- max_wohngeld
            wg_monate_i <- wg_monate_i + 1

            #Wohngeld-Empfänger: KEINE Mindesteinkommen-Aufstockung
            einkommen <- einkommen_roh
          }
        }
      }
      heimpop$wohngeld_betrag_aktuell[i] <- wohngeld_betrag_monat

      #Deckungslücke neu berechnen NUR wenn Wohngeld gezahlt wird
      #Sonst bleibt die bereits durch Vermögensverzehr reduzierte Deckungslücke bestehen
      if (wohngeld_betrag_monat > 0) {
        deckungsluecke <- max(0,
          as.numeric(heimkosten_effektiv - einkommen - wohngeld_betrag_monat)[1])
        if (is.na(deckungsluecke)) deckungsluecke <- 0
        deckungsluecke_ausgaben <- max(0,
          as.numeric(heimkosten_effektiv_ausgaben - einkommen - wohngeld_betrag_monat)[1])
        if (is.na(deckungsluecke_ausgaben)) deckungsluecke_ausgaben <- 0
      }

      #Grundsicherung bei Deckungslücke (nur ab 2023: nach Wohngeld, vor HzP)
      #Vor 2023: Mindesteinkommen ist bereits in 'einkommen' enthalten
      #Ab 2023: Bei Wohngeld-Empfängern wurde einkommen auf einkommen_roh zurückgesetzt,
      #daher muss Mindesteinkommen hier nochmals berücksichtigt werden
      if (.is_wg_p1[.k] &&
          deckungsluecke > 0 && vermoegen_aktuell <= 0) {
        grundsicherung_betrag <- get_mek_fast(current_jahr)
        deckungsluecke <- max(0, deckungsluecke - grundsicherung_betrag)
        deckungsluecke_ausgaben <- max(0, deckungsluecke_ausgaben - grundsicherung_betrag)
      }

      #Sozialhilfe / Pflegewohngeld (nach Wohngeld + Grundsicherung)
      if (bula == 5) {
        # === NRW: Zweistufiger Verzehr ===
        #Sonstige Kosten = EEE + UV + AK (alles außer IK)
        sonstige_kosten <- pflegebedingter_ea + kosten$uv + ausbildungskosten - zuschlag_betrag

        # STUFE 1: Einkommen für sonstige Kosten
        rest_nach_sonstige <- einkommen - sonstige_kosten

        if (rest_nach_sonstige < 0) {
          #Einkommen reicht nicht für sonstige Kosten
          #→ Vermögensverzehr nach HzP-Schongrenzen
          fehlbetrag_sonstige <- abs(rest_nach_sonstige)
          if (vermoegen_aktuell > 0) {
            verzehr <- min(fehlbetrag_sonstige, vermoegen_aktuell)
            vermoegen_aktuell <- vermoegen_aktuell - verzehr
            vermoegen_pfwg_aktuell <- max(0, vermoegen_pfwg_aktuell - verzehr)
            gesamtvermoegen_aktuell <- gesamtvermoegen_aktuell - verzehr
            fehlbetrag_sonstige <- fehlbetrag_sonstige - verzehr
          }
          #Wohneigentum-Verzehr (nach Geldvermögen, vor HzP)
          if (fehlbetrag_sonstige > 0 && wohneigentum_aktuell > 0) {
            verzehr <- min(fehlbetrag_sonstige, wohneigentum_aktuell)
            wohneigentum_aktuell <- wohneigentum_aktuell - verzehr
            fehlbetrag_sonstige <- fehlbetrag_sonstige - verzehr
          }
          if (fehlbetrag_sonstige > 0) {
            #HzP für sonstige Kosten
            hzp_bedarf_monat <- TRUE
            hzp_betrag_monat <- fehlbetrag_sonstige
          }
          rest_nach_sonstige <- 0  # Kein Rest-Einkommen für IK
        }

        # STUFE 2: Rest-Einkommen für IK
        fehlbetrag_ik <- investitionskosten_raw - rest_nach_sonstige
        if (fehlbetrag_ik > 0) {
          #Rest-Einkommen reicht nicht für IK
          #→ Vermögensverzehr nach PfWG-Schongrenzen
          if (vermoegen_pfwg_aktuell > 0) {
            verzehr <- min(fehlbetrag_ik, vermoegen_pfwg_aktuell)
            vermoegen_pfwg_aktuell <- vermoegen_pfwg_aktuell - verzehr
            gesamtvermoegen_aktuell <- gesamtvermoegen_aktuell - verzehr
            fehlbetrag_ik <- fehlbetrag_ik - verzehr
          }
          if (fehlbetrag_ik > 0) {
            #PfWG zahlt restliche IK
            pfwg_bedarf_monat <- TRUE
            pfwg_betrag_monat <- investitionskosten_raw  # PfWG = volle IK in NRW
          }
        }

      } else if (bula == 1) {
        # === SH: Zweistufiger Verzehr (analog NRW, aber PfWG max 466,95€) ===
        sonstige_kosten <- pflegebedingter_ea + kosten$uv + ausbildungskosten - zuschlag_betrag

        # STUFE 1: identisch wie NRW
        rest_nach_sonstige <- einkommen - sonstige_kosten

        if (rest_nach_sonstige < 0) {
          fehlbetrag_sonstige <- abs(rest_nach_sonstige)
          if (vermoegen_aktuell > 0) {
            verzehr <- min(fehlbetrag_sonstige, vermoegen_aktuell)
            vermoegen_aktuell <- vermoegen_aktuell - verzehr
            vermoegen_pfwg_aktuell <- max(0, vermoegen_pfwg_aktuell - verzehr)
            gesamtvermoegen_aktuell <- gesamtvermoegen_aktuell - verzehr
            fehlbetrag_sonstige <- fehlbetrag_sonstige - verzehr
          }
          #Wohneigentum-Verzehr (nach Geldvermögen, vor HzP)
          if (fehlbetrag_sonstige > 0 && wohneigentum_aktuell > 0) {
            verzehr <- min(fehlbetrag_sonstige, wohneigentum_aktuell)
            wohneigentum_aktuell <- wohneigentum_aktuell - verzehr
            fehlbetrag_sonstige <- fehlbetrag_sonstige - verzehr
          }
          if (fehlbetrag_sonstige > 0) {
            hzp_bedarf_monat <- TRUE
            hzp_betrag_monat <- fehlbetrag_sonstige
          }
          rest_nach_sonstige <- 0
        }

        # STUFE 2: IK (PfWG max 466,95€ in SH)
        fehlbetrag_ik <- investitionskosten_raw - rest_nach_sonstige
        if (fehlbetrag_ik > 0) {
          if (vermoegen_pfwg_aktuell > 0) {
            verzehr <- min(fehlbetrag_ik, vermoegen_pfwg_aktuell)
            vermoegen_pfwg_aktuell <- vermoegen_pfwg_aktuell - verzehr
            gesamtvermoegen_aktuell <- gesamtvermoegen_aktuell - verzehr
            fehlbetrag_ik <- fehlbetrag_ik - verzehr
          }
          if (fehlbetrag_ik > 0) {
            pfwg_bedarf_monat <- TRUE
            pfwg_betrag_monat <- min(investitionskosten_raw, pfwg_max_sh)
            #Rest-IK über PfWG-Maximum → HzP
            if (investitionskosten_raw > pfwg_max_sh) {
              rest_ueber_pfwg <- investitionskosten_raw - pfwg_max_sh
              hzp_bedarf_monat <- TRUE
              hzp_betrag_monat <- hzp_betrag_monat + rest_ueber_pfwg
            }
          }
        }

      } else if (deckungsluecke > 0) {
        #Alle anderen Bundesländer: HzP nach Abzug Wohngeld
        hzp_betrag_monat <- max(0, deckungsluecke_ausgaben)
        if (hzp_betrag_monat > 0) {
          hzp_bedarf_monat <- TRUE
        }
      }
    }
    
    #Nächster Monat
    verweilmonat <- verweilmonat + 1
  }

  #Wohngeld-Monate zurückschreiben
  heimpop$wohngeld_monate_bezogen[i] <- wg_monate_i

  #State am Ende Phase 1 speichern (für Phase 2)
  heimpop$vermoegen_end_phase1[i] <- vermoegen_aktuell
  heimpop$gesamtvermoegen_end_phase1[i] <- gesamtvermoegen_aktuell
  heimpop$vermoegen_pfwg_end_phase1[i] <- vermoegen_pfwg_aktuell
  heimpop$wohneigentum_end_phase1[i] <- wohneigentum_aktuell
  heimpop$verweilmonat_end_phase1[i] <- verweilmonat

  #Ergebnis speichern nur wenn Stichtag in Phase 1 liegt
  if (stichtag <= phase1_ende) {
  #Ergebnis speichern (letzter Monat = Stichtag)
  results$hzp_bedarf[i] <- hzp_bedarf_monat
  results$hzp_betrag[i] <- hzp_betrag_monat
  results$pfwg_bedarf[i] <- pfwg_bedarf_monat
  results$pfwg_betrag[i] <- pfwg_betrag_monat
  results$vermoegen_rest[i] <- vermoegen_aktuell
  results$wohneigentum_rest[i] <- wohneigentum_aktuell

  #Neue Werte für erweiterte Endpunkte speichern
  results$heimkosten_brutto[i] <- heimkosten_brutto_monat
  results$eigenanteil_nach_zuschlag[i] <- eigenanteil_nach_zuschlag_monat
  results$zuschlag_betrag[i] <- zuschlag_betrag_monat
  results$uv_kosten[i] <- uv_kosten_monat
  results$inv_kosten[i] <- inv_kosten_monat
  results$eee_kosten[i] <- eee_kosten_monat
  results$ak_kosten[i] <- ak_kosten_monat
  results$einkommen_monat[i] <- einkommen_monat_val

  #PV-Leistung: Pauschale nach Pflegegrad (§ 43 SGB XI) + Leistungszuschlag (§ 43c SGB XI)
  pv_pauschale_monat <- get_pv_fast(person_pflegegrad, year(stichtag))
  results$pv_pauschale[i] <- pv_pauschale_monat
  results$pv_leistung[i] <- pv_pauschale_monat + zuschlag_betrag_monat

  #Wohngeld Plus
  results$wohngeld_bedarf[i] <- wohngeld_betrag_monat > 0
  results$wohngeld_betrag[i] <- wohngeld_betrag_monat

  #Grundsicherung/Lebensunterhalt
  results$bezieht_grundsicherung[i] <- grundsicherung_bezogen
  results$mek_aufstockung[i]        <- mek_aufstockung_val

  #Privat getragen = Eigenanteil - HzP - PfWG - Wohngeld
  results$privat_getragen[i] <- max(0, eigenanteil_nach_zuschlag_monat - hzp_betrag_monat - pfwg_betrag_monat - wohngeld_betrag_monat)
  } #Ende: if (stichtag <= phase1_ende)

}


# ============================================================
# ZWISCHEN-PHASE: Wohngeld-Quote Berechnung (nach Phase 1)
# ============================================================

n_berechtigt_quota <- 0  # Berechtigt am Zuweisungsdatum (wird in Quota-Berechnung gesetzt)

if (stichtag > phase1_ende) {

  #Initialisierung: Runden-State mit Phase-1-Endwerten befüllen
  heimpop$vermoegen_end_prev_runde <- heimpop$vermoegen_end_phase1
  heimpop$gesamtvermoegen_end_prev_runde <- heimpop$gesamtvermoegen_end_phase1
  heimpop$vermoegen_pfwg_end_prev_runde <- heimpop$vermoegen_pfwg_end_phase1
  heimpop$wohneigentum_end_prev_runde <- heimpop$wohneigentum_end_phase1
  heimpop$verweilmonat_end_prev_runde <- heimpop$verweilmonat_end_phase1
  #WG-relevante Werte: Für Runde 1 sind die Dez-2022-Werte korrekt
  heimpop$gesamteinkommen_end_prev_runde <- heimpop$gesamteinkommen_dez2022
  heimpop$heimkosten_effektiv_end_prev_runde <- heimpop$heimkosten_effektiv_dez2022
  heimpop$gesamtvermoegen_end_prev_runde_wg <- heimpop$gesamtvermoegen_dez2022

  #Für jede Wohngeld-Runde
  for (runde_idx in seq_along(relevante_zuweisungen)) {

    zuweisung_datum <- relevante_zuweisungen[runde_idx]

    cat("\n\n=== Wohngeld-Runde", runde_idx, "- Zuweisung am", as.character(zuweisung_datum), "===\n")

    #Reset: Alle verlieren Wohngeld (außer erste Runde)
    if (runde_idx > 1) {
      heimpop$wohngeld_zugewiesen <- FALSE
      heimpop$wohngeld_monate_bezogen <- 0L
    }

    #Ende dieser Runde bestimmen
    if (runde_idx < length(relevante_zuweisungen)) {
      runde_ende <- relevante_zuweisungen[runde_idx + 1] - days(1)
    } else {
      runde_ende <- stichtag
    }

  cat("\n\n=== Wohngeld-Quote Berechnung ===\n")

  zuweisung_jahr <- year(zuweisung_datum)
  mindesteinkommen_zuw <- get_mek_fast(zuweisung_jahr)
  barbetrag_zuw <- get_bb_fast(zuweisung_jahr)

  #Berechtigung vektorisiert prüfen
  einzug_ok <- heimpop$einzugsdatum <= zuweisung_datum
  bula_ok <- !(heimpop$bula %in% c(1, 5))
  verm_ok <- heimpop$gesamtvermoegen_end_prev_runde_wg <= 60000

  wohngeldeinkommen_v <- heimpop$gesamteinkommen_end_prev_runde * 0.90 - barbetrag_zuw - 50
  wohngeld_luecke_v <- heimpop$heimkosten_effektiv_end_prev_runde - wohngeldeinkommen_v
  luecke_ok <- wohngeld_luecke_v > 0

  results$wohngeld_berechtigt_q1_2023 <- einzug_ok & bula_ok & verm_ok & luecke_ok

  #Anzahl Berechtigte ermitteln
  n_wohngeld_berechtigt_q1 <- sum(results$wohngeld_berechtigt_q1_2023, na.rm = TRUE)

  #Quote berechnen (auf Basis hochgerechneter Berechtigter)
  GESAMT_HEIMBEWOHNER <- 778693
  WOHNGELD_ZIEL <- 119455  # 15,34% - konstant für alle Jahre
  n_berechtigt_hochgerechnet <- n_wohngeld_berechtigt_q1 * (GESAMT_HEIMBEWOHNER / nrow(heimpop))
  wohngeld_quote_val <- WOHNGELD_ZIEL / n_berechtigt_hochgerechnet

  n_berechtigt_quota <- n_wohngeld_berechtigt_q1  # Für Output: Berechtigt am Zuweisungsdatum

  cat("  Berechtigte am", as.character(zuweisung_datum), ":", n_wohngeld_berechtigt_q1, "\n")
  cat("  Ziel-Anzahl:", WOHNGELD_ZIEL, "\n")
  cat("  Quote:", round(wohngeld_quote_val, 4), "\n")

  #Zufällige Zuweisung vektorisiert (nur für Berechtigte)
  heimpop$wohngeld_zugewiesen <- results$wohngeld_berechtigt_q1_2023 & (runif(nrow(heimpop)) < wohngeld_quote_val)
  results$wohngeld_zugewiesen <- heimpop$wohngeld_zugewiesen

  n_wohngeld_zugewiesen <- sum(heimpop$wohngeld_zugewiesen, na.rm = TRUE)
  cat("  Zugewiesen (per Quote):", n_wohngeld_zugewiesen, "\n\n")


  # ============================================================
  # PHASE 2: Simulation ab Zuweisungsdatum bis Runde-Ende
  # ============================================================

  cat("=== Phase 2: Simulation ab", as.character(zuweisung_datum), "bis", as.character(runde_ende), "===\n\n")

  for (i in seq_len(n_sim)) {

    #Vorab extrahierte Vektoren
    einzug       <- hp_einzug[i]
    einzugsjahr  <- hp_einzugsjahr[i]
    bula         <- hp_bula[i]
    is_nrw       <- hp_is_nrw[i]
    is_sh        <- hp_is_sh[i]
    is_sonstig   <- hp_is_sonstig[i]
    heim_id_val  <- hp_heim_id[i]
    basis_jahr   <- hp_basis_jahr[i]

    #Partner + Basiswerte: vorberechnet
    partner_row        <- hp_partner[[i]]
    ist_ost            <- (bula >= 11)
    person_basis_vals  <- hp_person_basis[[i]]
    partner_basis_vals <- hp_partner_basis[[i]]
    person_d11104 <- hp_d11104[i]
    person_pgstib <- hp_pgstib[i]
    person_kv_pkv        <- hp_kv_pkv[i]
    person_we            <- hp_we[i]
    person_wohngeldstufe <- hp_wohngeldstufe[i]
    person_pflegegrad    <- hp_pflegegrad[i]

    #State aus vorheriger Runde wiederherstellen (Runde 1: = Phase 1, Runde 2+: = Ende vorherige Runde)
    vermoegen_aktuell <- heimpop$vermoegen_end_prev_runde[i]
    gesamtvermoegen_aktuell <- heimpop$gesamtvermoegen_end_prev_runde[i]
    vermoegen_pfwg_aktuell <- heimpop$vermoegen_pfwg_end_prev_runde[i]
    wohneigentum_aktuell <- heimpop$wohneigentum_end_prev_runde[i]
    verweilmonat <- heimpop$verweilmonat_end_prev_runde[i]

    #Startdatum für diese Runde
    if (einzug > zuweisung_datum) {
      current_date <- einzug  # Person zieht erst nach Zuweisungsdatum ein
    } else {
      current_date <- zuweisung_datum
    }

    #Initialisiere Monatswerte
    hzp_bedarf_monat <- FALSE
    hzp_betrag_monat <- 0
    pfwg_bedarf_monat <- FALSE
    pfwg_betrag_monat <- 0
    wohngeld_betrag_monat <- 0
    wg_zugewiesen_i <- heimpop$wohngeld_zugewiesen[i]
    wg_monate_i <- heimpop$wohngeld_monate_bezogen[i]
    #Tracking für WG-Runden-Übergabe (bleiben bei prev_runde wenn while-Schleife nicht läuft)
    gesamteinkommen_last <- heimpop$gesamteinkommen_end_prev_runde[i]
    heimkosten_effektiv_last <- heimpop$heimkosten_effektiv_end_prev_runde[i]
    gesamtvermoegen_last <- heimpop$gesamtvermoegen_end_prev_runde_wg[i]

    #Datum-Sequenz vorberechnen (ersetzt %m+% months(1))
    #Guard: leere Sequenz wenn current_date > runde_ende
    .dates_p2  <- if (current_date <= runde_ende) seq.Date(from = current_date, to = runde_ende, by = "month") else as.Date(character(0))
    .jahre_p2  <- year(.dates_p2)
    .monate_p2 <- month(.dates_p2)
    .is_wg_p2  <- .dates_p2 >= WOHNGELD_STARTDATUM
    .yymm_p2  <- format(.dates_p2, "%Y-%m")
    .qkeys_p2 <- paste0("q", ceiling(.monate_p2 / 3L), "_", .jahre_p2)

    for (.k2 in seq_along(.dates_p2)) {
      current_date  <- .dates_p2[.k2]
      current_jahr  <- .jahre_p2[.k2]
      current_monat <- .monate_p2[.k2]

      #Fortschreibung der Rohwerte auf aktuellen Monat (Person UND Partner)
      rohwerte_aktuell <- fortschreibe_rohwerte_fast(person_basis_vals, ist_ost, basis_jahr, .yymm_p2[.k2], current_jahr)
      partner_rohwerte_aktuell <- if (!is.null(partner_basis_vals)) fortschreibe_rohwerte_fast(partner_basis_vals, ist_ost, basis_jahr, .yymm_p2[.k2], current_jahr) else NULL

      #Einsetzbare Werte nach aktueller Gesetzeslage berechnen
      einsetz_aktuell <- berechne_einsetzbare_werte_fast(rohwerte_aktuell, partner_rohwerte_aktuell, person_d11104, person_pgstib, person_kv_pkv, bula, person_we, current_jahr)

      #Einkommen VOR Aufstockung speichern
      einkommen_roh <- einsetz_aktuell$einsetzungsfaehiges_einkommen_insgesamt

      #Mindesteinkommen prüfen (auf Individualeinkommen, nicht Gesamt)
      mindesteinkommen <- get_mek_fast(current_jahr)
      einsetzf_ind <- einsetz_aktuell$einsetzungsfaehiges_individualeinkommen
      einsetzf_partner <- einsetz_aktuell$einsetzungsfaehiges_partnereinkommen
      einsetzf_ind_effektiv <- max(einsetzf_ind, mindesteinkommen)
      einkommen <- einsetzf_ind_effektiv + einsetzf_partner

      #Grundsicherung-Marker (für Stichtag-Speicherung)
      grundsicherung_bezogen    <- (einsetzf_ind < mindesteinkommen)
      mek_aufstockung_q1_val    <- max(0, mindesteinkommen - einsetzf_ind)

      #Kostenkomponenten (individuell aus Datensatz!)
      kosten <- kosten_cache[[as.character(heim_id_val)]][[.qkeys_p2[.k2]]]

      if (is.na(kosten$gesamt)) {
        stop(paste("Keine Heimkosten für Person", i, "Heim", heim_id_val, "Datum", current_date))
      }

      #Kostenkomponenten auslesen
      pflegebedingter_ea <- if (is.na(kosten$eee)) 0 else kosten$eee
      investitionskosten_raw <- if (is.na(kosten$inv)) 0 else kosten$inv
      ausbildungskosten <- if (is.na(kosten$ak)) 0 else kosten$ak

      #Ab 2028: EEE anpassen um PV-Delta (GR-Dynamisierung erhoeht PV -> senkt EEE -> senkt EA)
      if (current_jahr >= 2028) {
        delta_pv <- get_pv_fast(person_pflegegrad, current_jahr) - get_pv_null_fast(person_pflegegrad, current_jahr)
        pflegebedingter_ea <- max(0, pflegebedingter_ea - delta_pv)
      }

      #IK-Varianten für unterschiedliche Berechnungen
      investitionskosten_quote <- investitionskosten_raw * INV_FAKTOR_QUOTE
      investitionskosten_ausgaben <- investitionskosten_raw * INV_FAKTOR_AUSGABEN

      #Leistungszuschlag § 43c SGB XI (basiert auf EEE + AK)
      zuschlag_satz <- get_lz_fast(verweilmonat, current_jahr)
      zuschlag_betrag <- (pflegebedingter_ea + ausbildungskosten) * zuschlag_satz

      #Heimkosten: vor 2028 ohne Dynamisierung (kosten$eee direkt), ab 2028 mit dynamisiertem EEE
      if (current_jahr >= 2028) {
        heimkosten_gesamt_quote <- pflegebedingter_ea + kosten$uv + investitionskosten_quote + ausbildungskosten
        heimkosten_gesamt_ausgaben <- pflegebedingter_ea + kosten$uv + investitionskosten_ausgaben + ausbildungskosten
      } else {
        heimkosten_gesamt_quote <- kosten$eee + kosten$uv + investitionskosten_quote + ausbildungskosten
        heimkosten_gesamt_ausgaben <- kosten$eee + kosten$uv + investitionskosten_ausgaben + ausbildungskosten
      }
      heimkosten_effektiv <- heimkosten_gesamt_quote - zuschlag_betrag
      heimkosten_effektiv_ausgaben <- heimkosten_gesamt_ausgaben - zuschlag_betrag

      #Tracking für WG-Runden-Übergabe (immer aktuell halten)
      gesamteinkommen_last <- einsetz_aktuell$gesamteinkommen_monatlich
      heimkosten_effektiv_last <- heimkosten_effektiv
      gesamtvermoegen_last <- gesamtvermoegen_aktuell

      #Deckungslücke (NA-sicher) - basierend auf Quote-Heimkosten (bestimmt HzP-Bedarf)
      deckungsluecke <- as.numeric(heimkosten_effektiv - einkommen)[1]
      if (is.na(deckungsluecke)) deckungsluecke <- 0

      #Deckungslücke OHNE Mindesteinkommen (für Wohngeld-Prüfung)
      #Wohngeld soll Mindesteinkommen/Grundsicherung ERSETZEN, nicht ergänzen
      deckungsluecke_ohne_mek <- as.numeric(heimkosten_effektiv - einkommen_roh)[1]
      if (is.na(deckungsluecke_ohne_mek)) deckungsluecke_ohne_mek <- 0

      #Deckungslücke für Ausgaben-Berechnung (bestimmt HzP-Betrag)
      deckungsluecke_ausgaben <- as.numeric(heimkosten_effektiv_ausgaben - einkommen)[1]
      if (is.na(deckungsluecke_ausgaben)) deckungsluecke_ausgaben <- 0

      hzp_bedarf_monat <- FALSE
      hzp_betrag_monat <- 0
      pfwg_bedarf_monat <- FALSE
      pfwg_betrag_monat <- 0

      #Werte für Endpunkt-Berechnung speichern
      heimkosten_brutto_monat <- heimkosten_gesamt_quote
      eigenanteil_nach_zuschlag_monat <- heimkosten_effektiv
      zuschlag_betrag_monat <- zuschlag_betrag
      uv_kosten_monat <- kosten$uv
      inv_kosten_monat <- investitionskosten_raw
      eee_kosten_monat <- pflegebedingter_ea
      ak_kosten_monat <- ausbildungskosten
      einkommen_monat_val <- einkommen

      #Q1-Werte speichern (1.1. des Stichjahres)
      q1_datum <- as.Date(paste0(year(stichtag), "-01-01"))
      if (current_date == q1_datum) {
        results$heimkosten_brutto_q1[i] <- heimkosten_gesamt_quote
        results$eigenanteil_nach_zuschlag_q1[i] <- heimkosten_effektiv
        results$zuschlag_betrag_q1[i] <- zuschlag_betrag
        results$uv_kosten_q1[i] <- kosten$uv
        results$inv_kosten_q1[i] <- investitionskosten_raw
        results$eee_kosten_q1[i] <- pflegebedingter_ea
        results$ak_kosten_q1[i] <- ausbildungskosten
      }
      #Q3-Wert: bei Stichtag (01.07.) einfrieren
      if (current_date == stichtag) {
        mek_aufstockung_val <- max(0, mindesteinkommen - einsetzf_ind)
      }

      # ---- 1. Wohngeld Plus: Einmal bewilligt = 24 Monate Bezug ----
      #Wohngeld wird VOR der HzP-Prüfung berechnet und gezahlt.
      #Zugewiesene Empfänger behalten ihren Anspruch für die volle Laufzeit
      #(keine monatliche Neuprüfung - wie in der Realität).
      wohngeld_betrag_monat <- 0
      wohngeld_berechtigt <- FALSE

      #A) ZUGEWIESENE: Bewilligt = 24 Monate Bezug, KEINE monatliche Neuprüfung
      #Einmal bewilligt → Zahlung für volle Laufzeit, unabhängig von Vermögen/Lücke
      if (wg_zugewiesen_i == TRUE &&
          wg_monate_i < WOHNGELD_MAX_MONATE) {

        barbetrag <- get_bb_fast(current_jahr)
        gesamteinkommen_aktuell <- einsetz_aktuell$gesamteinkommen_monatlich
        wohngeldeinkommen <- gesamteinkommen_aktuell * 0.90 - barbetrag - 50
        wohngeld_luecke <- heimkosten_effektiv - wohngeldeinkommen

        wg_hoechstbetrag <- get_wg_hb_fast(
          person_wohngeldstufe, current_jahr)
        wohngeld_betrag_monat <- min(max(wohngeld_luecke, 0), wg_hoechstbetrag)
        wg_monate_i <- wg_monate_i + 1

        #Wohngeld-Empfänger: KEINE Mindesteinkommen-Aufstockung
        einkommen <- einkommen_roh
        wohngeld_berechtigt <- TRUE

      #B) NICHT-ZUGEWIESENE: Berechtigungsprüfung (nur für Statistik/Quote)
      } else if (.is_wg_p2[.k2] &&
                 !(bula %in% c(1, 5)) &&
                 gesamtvermoegen_aktuell <= 60000) {

        barbetrag <- get_bb_fast(current_jahr)
        gesamteinkommen_aktuell <- einsetz_aktuell$gesamteinkommen_monatlich
        wohngeldeinkommen <- gesamteinkommen_aktuell * 0.90 - barbetrag - 50
        wohngeld_luecke <- heimkosten_effektiv - wohngeldeinkommen

        if (wohngeld_luecke > 0) {
          wohngeld_berechtigt <- TRUE
        }
      }
      heimpop$wohngeld_betrag_aktuell[i] <- wohngeld_betrag_monat

      #Deckungslücken mit Wohngeld-Betrag neu berechnen
      deckungsluecke <- max(0,
        as.numeric(heimkosten_effektiv - einkommen - wohngeld_betrag_monat)[1])
      if (is.na(deckungsluecke)) deckungsluecke <- 0
      deckungsluecke_ohne_mek <- max(0,
        as.numeric(heimkosten_effektiv - einkommen_roh - wohngeld_betrag_monat)[1])
      if (is.na(deckungsluecke_ohne_mek)) deckungsluecke_ohne_mek <- 0
      deckungsluecke_ausgaben <- max(0,
        as.numeric(heimkosten_effektiv_ausgaben - einkommen - wohngeld_betrag_monat)[1])
      if (is.na(deckungsluecke_ausgaben)) deckungsluecke_ausgaben <- 0

      #Phase 2: HzP-Prüfung (NACH Wohngeld)
      if (deckungsluecke_ohne_mek > 0) {

        # ---- 2. Vermögensverzehr (NACH Wohngeld) - NUR für Nicht-NRW/SH ----
        #In NRW/SH erfolgt der Verzehr zweistufig in der NRW/SH-Logik unten
        if (!(bula %in% c(1, 5))) {
          if (deckungsluecke > 0 && vermoegen_aktuell > 0) {
            verzehr <- min(deckungsluecke, vermoegen_aktuell)
            vermoegen_aktuell <- vermoegen_aktuell - verzehr
            gesamtvermoegen_aktuell <- gesamtvermoegen_aktuell - verzehr
            deckungsluecke <- deckungsluecke - verzehr
            deckungsluecke_ausgaben <- max(0, deckungsluecke_ausgaben - verzehr)
          }

          #Wohneigentum-Verzehr
          if (deckungsluecke > 0 && wohneigentum_aktuell > 0) {
            verzehr <- min(deckungsluecke, wohneigentum_aktuell)
            wohneigentum_aktuell <- wohneigentum_aktuell - verzehr
            deckungsluecke <- deckungsluecke - verzehr
            deckungsluecke_ausgaben <- max(0, deckungsluecke_ausgaben - verzehr)
          }
        }

        # ---- 3. Sozialhilfe / Pflegewohngeld (nach Wohngeld) ----
        #NRW/SH: einkommen_effektiv = einkommen + wohngeld
        einkommen_effektiv <- einkommen + wohngeld_betrag_monat

        if (bula == 5) {
          # === NRW: Zweistufiger Verzehr ===
          sonstige_kosten <- pflegebedingter_ea + kosten$uv + ausbildungskosten - zuschlag_betrag

          # STUFE 1: Einkommen für sonstige Kosten
          rest_nach_sonstige <- einkommen_effektiv - sonstige_kosten

          if (rest_nach_sonstige < 0) {
            fehlbetrag_sonstige <- abs(rest_nach_sonstige)
            if (vermoegen_aktuell > 0) {
              verzehr <- min(fehlbetrag_sonstige, vermoegen_aktuell)
              vermoegen_aktuell <- vermoegen_aktuell - verzehr
              vermoegen_pfwg_aktuell <- max(0, vermoegen_pfwg_aktuell - verzehr)
              gesamtvermoegen_aktuell <- gesamtvermoegen_aktuell - verzehr
              fehlbetrag_sonstige <- fehlbetrag_sonstige - verzehr
            }
            #Wohneigentum-Verzehr (nach Geldvermögen, vor HzP)
            if (fehlbetrag_sonstige > 0 && wohneigentum_aktuell > 0) {
              verzehr <- min(fehlbetrag_sonstige, wohneigentum_aktuell)
              wohneigentum_aktuell <- wohneigentum_aktuell - verzehr
              fehlbetrag_sonstige <- fehlbetrag_sonstige - verzehr
            }
            if (fehlbetrag_sonstige > 0) {
              hzp_bedarf_monat <- TRUE
              hzp_betrag_monat <- fehlbetrag_sonstige
            }
            rest_nach_sonstige <- 0
          }

          # STUFE 2: Rest-Einkommen für IK
          fehlbetrag_ik <- investitionskosten_raw - rest_nach_sonstige
          if (fehlbetrag_ik > 0) {
            if (vermoegen_pfwg_aktuell > 0) {
              verzehr <- min(fehlbetrag_ik, vermoegen_pfwg_aktuell)
              vermoegen_pfwg_aktuell <- vermoegen_pfwg_aktuell - verzehr
              gesamtvermoegen_aktuell <- gesamtvermoegen_aktuell - verzehr
              fehlbetrag_ik <- fehlbetrag_ik - verzehr
            }
            if (fehlbetrag_ik > 0) {
              pfwg_bedarf_monat <- TRUE
              pfwg_betrag_monat <- investitionskosten_raw
            }
          }

        } else if (bula == 1) {
          # === SH: Zweistufiger Verzehr (analog NRW, aber PfWG max 466,95€) ===
          sonstige_kosten <- pflegebedingter_ea + kosten$uv + ausbildungskosten - zuschlag_betrag

          # STUFE 1
          rest_nach_sonstige <- einkommen_effektiv - sonstige_kosten

          if (rest_nach_sonstige < 0) {
            fehlbetrag_sonstige <- abs(rest_nach_sonstige)
            if (vermoegen_aktuell > 0) {
              verzehr <- min(fehlbetrag_sonstige, vermoegen_aktuell)
              vermoegen_aktuell <- vermoegen_aktuell - verzehr
              vermoegen_pfwg_aktuell <- max(0, vermoegen_pfwg_aktuell - verzehr)
              gesamtvermoegen_aktuell <- gesamtvermoegen_aktuell - verzehr
              fehlbetrag_sonstige <- fehlbetrag_sonstige - verzehr
            }
            #Wohneigentum-Verzehr (nach Geldvermögen, vor HzP)
            if (fehlbetrag_sonstige > 0 && wohneigentum_aktuell > 0) {
              verzehr <- min(fehlbetrag_sonstige, wohneigentum_aktuell)
              wohneigentum_aktuell <- wohneigentum_aktuell - verzehr
              fehlbetrag_sonstige <- fehlbetrag_sonstige - verzehr
            }
            if (fehlbetrag_sonstige > 0) {
              hzp_bedarf_monat <- TRUE
              hzp_betrag_monat <- fehlbetrag_sonstige
            }
            rest_nach_sonstige <- 0
          }

          # STUFE 2: IK (PfWG max 466,95€)
          fehlbetrag_ik <- investitionskosten_raw - rest_nach_sonstige
          if (fehlbetrag_ik > 0) {
            if (vermoegen_pfwg_aktuell > 0) {
              verzehr <- min(fehlbetrag_ik, vermoegen_pfwg_aktuell)
              vermoegen_pfwg_aktuell <- vermoegen_pfwg_aktuell - verzehr
              gesamtvermoegen_aktuell <- gesamtvermoegen_aktuell - verzehr
              fehlbetrag_ik <- fehlbetrag_ik - verzehr
            }
            if (fehlbetrag_ik > 0) {
              pfwg_bedarf_monat <- TRUE
              pfwg_betrag_monat <- min(investitionskosten_raw, pfwg_max_sh)
              if (investitionskosten_raw > pfwg_max_sh) {
                rest_ueber_pfwg <- investitionskosten_raw - pfwg_max_sh
                hzp_bedarf_monat <- TRUE
                hzp_betrag_monat <- hzp_betrag_monat + rest_ueber_pfwg
              }
            }
          }

        } else if (deckungsluecke > 0) {
          #Alle anderen Bundesländer: HzP nach Abzug Wohngeld
          hzp_betrag_monat <- max(0, deckungsluecke_ausgaben)
          if (hzp_betrag_monat > 0) {
            hzp_bedarf_monat <- TRUE
          }
        }
      }

      #Nächster Monat
      verweilmonat <- verweilmonat + 1
    }

    #Ergebnis speichern (letzter Monat = Stichtag)
    results$hzp_bedarf[i] <- hzp_bedarf_monat
    results$hzp_betrag[i] <- hzp_betrag_monat
    results$pfwg_bedarf[i] <- pfwg_bedarf_monat
    results$pfwg_betrag[i] <- pfwg_betrag_monat
    results$vermoegen_rest[i] <- vermoegen_aktuell
    results$wohneigentum_rest[i] <- wohneigentum_aktuell

    #Neue Werte für erweiterte Endpunkte speichern
    results$heimkosten_brutto[i] <- heimkosten_brutto_monat
    results$eigenanteil_nach_zuschlag[i] <- eigenanteil_nach_zuschlag_monat
    results$zuschlag_betrag[i] <- zuschlag_betrag_monat
    results$uv_kosten[i] <- uv_kosten_monat
    results$inv_kosten[i] <- inv_kosten_monat
    results$eee_kosten[i] <- eee_kosten_monat
    results$ak_kosten[i] <- ak_kosten_monat
    results$einkommen_monat[i] <- einkommen_monat_val

    #PV-Leistung: Pauschale nach Pflegegrad (§ 43 SGB XI) + Leistungszuschlag (§ 43c SGB XI)
    pv_pauschale_monat <- get_pv_fast(person_pflegegrad, year(stichtag))
    results$pv_pauschale[i] <- pv_pauschale_monat
    results$pv_leistung[i] <- pv_pauschale_monat + zuschlag_betrag_monat

    #Wohngeld Plus: Zählung basiert auf Zuweisung (nicht Betrag), da Bewilligung = 24 Monate
    results$wohngeld_bedarf[i] <- wg_zugewiesen_i
    results$wohngeld_betrag[i] <- wohngeld_betrag_monat
    results$wohngeld_berechtigt_stichtag[i] <- wohngeld_berechtigt
    
    #Grundsicherung/Lebensunterhalt
    results$bezieht_grundsicherung[i] <- grundsicherung_bezogen
    results$mek_aufstockung[i]        <- mek_aufstockung_val
    results$mek_aufstockung_q1[i]     <- mek_aufstockung_q1_val

    #Privat getragen = Eigenanteil - HzP - PfWG - Wohngeld
    results$privat_getragen[i] <- max(0, eigenanteil_nach_zuschlag_monat - hzp_betrag_monat - pfwg_betrag_monat - wohngeld_betrag_monat)

    #Wohngeld-Monate zurückschreiben
    heimpop$wohngeld_monate_bezogen[i] <- wg_monate_i

    #State am Ende dieser Runde speichern (für nächste Runde)
    heimpop$vermoegen_end_prev_runde[i] <- vermoegen_aktuell
    heimpop$gesamtvermoegen_end_prev_runde[i] <- gesamtvermoegen_aktuell
    heimpop$vermoegen_pfwg_end_prev_runde[i] <- vermoegen_pfwg_aktuell
    heimpop$wohneigentum_end_prev_runde[i] <- wohneigentum_aktuell
    heimpop$verweilmonat_end_prev_runde[i] <- verweilmonat
    #WG-relevante Werte für nächste Runde
    heimpop$gesamteinkommen_end_prev_runde[i] <- gesamteinkommen_last
    heimpop$heimkosten_effektiv_end_prev_runde[i] <- heimkosten_effektiv_last
    heimpop$gesamtvermoegen_end_prev_runde_wg[i] <- gesamtvermoegen_last

  }


  }  # ===== ENDE DER WOHNGELD-RUNDEN-SCHLEIFE =====

} #Ende: if (stichtag > phase1_ende)

cat("\n\nSimulation abgeschlossen.\n\n")

# ---- Schritt 7: Ergebnisauswertung ----

cat("=== Ergebnisse ===\n\n")

n_gesamt <- nrow(results)
n_beamte <- sum(results$ist_beamter, na.rm = TRUE)

#Beamte werden nie HzP-abhängig, sind aber im Nenner
results$hzp_effektiv <- ifelse(results$ist_beamter, FALSE, results$hzp_bedarf)

n_hzp <- sum(results$hzp_effektiv, na.rm = TRUE)
hzp_quote <- n_hzp / n_gesamt * 100
hzp_ausgaben_pro_empfaenger <- mean(results$hzp_betrag[results$hzp_effektiv], na.rm = TRUE)

n_pfwg <- sum(results$pfwg_bedarf, na.rm = TRUE)

#Grundsicherung/Lebensunterhalt
n_grundsicherung <- sum(results$bezieht_grundsicherung, na.rm = TRUE)
grundsicherung_quote <- n_grundsicherung / n_gesamt * 100

cat("Gesamtanzahl simulierte Personen:", n_gesamt, "\n")
cat("  davon Beamte (nie SH-abhängig):", n_beamte, sprintf("(%.1f%%)\n", n_beamte/n_gesamt*100))
cat("\n")

cat("=== Sozialhilfe (HzP) am Stichtag", as.character(stichtag), "===\n")
cat("HzP-Empfänger:", n_hzp, "\n")
cat(">>> HzP-Quote:", round(hzp_quote, 2), "% <<<\n")
cat(">>> HzP-Ausgaben pro Empfänger:", round(hzp_ausgaben_pro_empfaenger, 2), "EUR/Monat <<<\n")
cat("\n")

cat("=== Pflegewohngeld (NRW/SH) ===\n")
cat("PfWG-Empfänger:", n_pfwg, "\n")
cat("\n")

cat("=== Grundsicherung/Lebensunterhalt am Stichtag", as.character(stichtag), "===\n")
cat("Empfänger (simuliert):", n_grundsicherung, "\n")
cat(">>> Quote:", round(grundsicherung_quote, 2), "% <<<\n")
cat("\n")

# ---- Erweiterte Endpunkte ----

#Hochrechnungsfaktor: Simulation auf Gesamtpopulation hochrechnen
#Wert aus HzP-Statistik 2023 (Blatt "Übersicht", Zeile "Heimbewohner")
gesamt_heimbewohner <- 778693
hochrechnungsfaktor <- gesamt_heimbewohner / n_gesamt

cat("=== HOCHRECHNUNG AUF GESAMTPOPULATION ===\n")
cat("Simulierte Personen:", n_gesamt, "\n")
cat("Gesamtpopulation (fixiert):", gesamt_heimbewohner, "\n")
cat("Hochrechnungsfaktor:", round(hochrechnungsfaktor, 2), "\n\n")

# ============================================================
# ERGEBNISSE - Berechnungen
# ============================================================

hzp_ausgaben_mrd <- sum(results$hzp_betrag[results$hzp_effektiv], na.rm = TRUE) *
                    12 * hochrechnungsfaktor / 1e9
n_hzp_hochgerechnet <- n_hzp * hochrechnungsfaktor
privat_eigenanteil_mrd <- sum(results$privat_getragen, na.rm = TRUE) *
                          12 * hochrechnungsfaktor / 1e9
pv_ausgaben_mrd <- sum(results$pv_leistung, na.rm = TRUE) *
                   12 * hochrechnungsfaktor / 1e9
n_wohngeld <- sum(results$wohngeld_bedarf, na.rm = TRUE)
wohngeld_quote <- n_wohngeld / n_gesamt * 100
wohngeld_ausgaben_mrd <- sum(results$wohngeld_betrag[results$wohngeld_bedarf], na.rm = TRUE) *
                         12 * hochrechnungsfaktor / 1e9
n_wohngeld_hochgerechnet <- n_wohngeld * hochrechnungsfaktor
pfwg_quote <- n_pfwg / n_gesamt * 100
pfwg_ausgaben_mrd <- sum(results$pfwg_betrag, na.rm = TRUE) *
                     12 * hochrechnungsfaktor / 1e9
n_pfwg_hochgerechnet <- n_pfwg * hochrechnungsfaktor

#Grundsicherung hochgerechnet
n_grundsicherung_hochgerechnet <- n_grundsicherung * hochrechnungsfaktor

# ============================================================
# 1. ERGEBNISTABELLE (Tab. 4)
# ============================================================

cat("=== ERGEBNISTABELLE (Tab. 4) ===\n\n")

ergebnis_tabelle <- data.frame(
  Kennzahl = c(
    "HzP-Quote (%)",
    "HzP-Empfänger (hochgerechnet)",
    "HzP-Ausgaben (Mrd. EUR/Jahr)",
    "Eigenanteile privat getragen (Mrd. EUR/Jahr)",
    "PV-Ausgaben gesamt (Mrd. EUR/Jahr)"
  ),
  Wert = c(
    round(hzp_quote, 2),
    round(n_hzp_hochgerechnet, 0),
    round(hzp_ausgaben_mrd, 3),
    round(privat_eigenanteil_mrd, 3),
    round(pv_ausgaben_mrd, 3)
  )
)

print(ergebnis_tabelle, row.names = FALSE)
cat("\n")

# ============================================================
# 2. EIGENANTEILE Q1 vs Q3 (Abb. 4)
# ============================================================

cat("=== EIGENANTEILE Q1 vs Q3 (Abb. 4) ===\n\n")

cat("Q1 (1.1.", year(stichtag), "):\n", sep = "")
cat("  Ø EEE:           ", round(mean(results$eee_kosten_q1, na.rm = TRUE), 2), " EUR\n")
cat("  Ø UV:            ", round(mean(results$uv_kosten_q1, na.rm = TRUE), 2), " EUR\n")
cat("  Ø Inv:           ", round(mean(results$inv_kosten_q1, na.rm = TRUE), 2), " EUR\n")
cat("  Ø AK:            ", round(mean(results$ak_kosten_q1, na.rm = TRUE), 2), " EUR\n")
cat("  Ø Zuschlag §43c: ", round(mean(results$zuschlag_betrag_q1, na.rm = TRUE), 2), " EUR\n")
cat("  Ø Eigenanteil:   ", round(mean(results$eigenanteil_nach_zuschlag_q1, na.rm = TRUE), 2), " EUR\n\n")

cat("Q3 (1.7.", year(stichtag), "):\n", sep = "")
cat("  Ø EEE:           ", round(mean(results$eee_kosten, na.rm = TRUE), 2), " EUR\n")
cat("  Ø UV:            ", round(mean(results$uv_kosten, na.rm = TRUE), 2), " EUR\n")
cat("  Ø Inv:           ", round(mean(results$inv_kosten, na.rm = TRUE), 2), " EUR\n")
cat("  Ø AK:            ", round(mean(results$ak_kosten, na.rm = TRUE), 2), " EUR\n")
cat("  Ø Zuschlag §43c: ", round(mean(results$zuschlag_betrag, na.rm = TRUE), 2), " EUR\n")
cat("  Ø Eigenanteil:   ", round(mean(results$eigenanteil_nach_zuschlag, na.rm = TRUE), 2), " EUR\n\n")

# ============================================================
# 3. EA NACH VERWEILDAUER (Abb. 5)
# ============================================================

cat("=== EA NACH VERWEILDAUER (Abb. 5) ===\n\n")

results$verweildauer_kat <- cut(results$verweildauer_monate,
                                breaks = c(0, 12, 24, 36, Inf),
                                labels = c("<12 Mon (15%)", "12-24 Mon (30%)",
                                           "24-36 Mon (50%)", "36+ Mon (75%)"),
                                include.lowest = TRUE)

eigenanteil_nach_verweildauer <- results %>%
  group_by(verweildauer_kat) %>%
  summarise(
    n = n(),
    mean_eee = mean(eee_kosten, na.rm = TRUE),
    mean_eee_nach_zuschlag = mean(eee_kosten - zuschlag_betrag, na.rm = TRUE),
    mean_zuschlag = mean(zuschlag_betrag, na.rm = TRUE),
    .groups = "drop"
  )

for (i in seq_len(nrow(eigenanteil_nach_verweildauer))) {
  row <- eigenanteil_nach_verweildauer[i, ]
  cat(sprintf("  %s: n=%d, Ø EEE=%.2f, Ø Zuschlag=%.2f, Ø EEE netto=%.2f EUR\n",
              row$verweildauer_kat, row$n, row$mean_eee, row$mean_zuschlag, row$mean_eee_nach_zuschlag))
}
cat("\n")

# ============================================================
# 4. AUSGABEN NACH KOSTENTRÄGER (Abb. 7)
# ============================================================

cat("=== AUSGABEN NACH KOSTENTRÄGER (Abb. 7) ===\n\n")

avg_privat <- mean(results$privat_getragen, na.rm = TRUE)
avg_pv <- mean(results$pv_leistung, na.rm = TRUE)
avg_hzp <- mean(results$hzp_betrag, na.rm = TRUE)
avg_pfwg <- mean(results$pfwg_betrag, na.rm = TRUE)
avg_wohngeld <- mean(results$wohngeld_betrag, na.rm = TRUE)

kostentraeger_monatlich <- data.frame(
  Kostentraeger = c(
    "Privat getragen",
    "PV-Leistungen (§43 + §43c)",
    "Hilfe zur Pflege",
    "Pflegewohngeld (NRW/SH)",
    "Wohngeld (§3 WoGG)"
  ),
  EUR_pro_Monat = c(avg_privat, avg_pv, avg_hzp, avg_pfwg, avg_wohngeld)
)

kostentraeger_monatlich$Anteil_Pct <-
  kostentraeger_monatlich$EUR_pro_Monat /
  sum(kostentraeger_monatlich$EUR_pro_Monat) * 100

cat("Durchschnittliche monatliche Ausgaben pro Heimbewohner:\n\n")
for (i in seq_len(nrow(kostentraeger_monatlich))) {
  row <- kostentraeger_monatlich[i, ]
  cat(sprintf("  %-30s %8.2f EUR (%5.1f%%)\n",
              row$Kostentraeger, row$EUR_pro_Monat, row$Anteil_Pct))
}
cat(sprintf("  %-30s %8.2f EUR\n",
            "SUMME:", sum(kostentraeger_monatlich$EUR_pro_Monat)))
cat("\n")

# ============================================================
# 5. WEITERE KENNZAHLEN
# ============================================================

cat("=== WOHNGELD-ERGEBNISSE ===\n\n")

# Berechtigt am Zuweisungsdatum (aus Quota-Berechnung, letzte Runde)
n_berechtigt_quota_hochgerechnet <- n_berechtigt_quota * hochrechnungsfaktor

# Berechtigt am Stichtag (aktuelle Phase-2-Werte)
n_berechtigt_stichtag <- sum(results$wohngeld_berechtigt_stichtag, na.rm = TRUE)
n_berechtigt_stichtag_hochgerechnet <- n_berechtigt_stichtag * hochrechnungsfaktor

# Tatsächlich bezogen am Stichtag
n_wohngeld_bezogen <- sum(results$wohngeld_bedarf, na.rm = TRUE)
n_wohngeld_bezogen_hochgerechnet <- n_wohngeld_bezogen * hochrechnungsfaktor

cat("1) Berechtigt am Zuweisungsdatum (Quota-Basis):\n")
cat("  Simuliert:", n_berechtigt_quota, "\n")
cat("  Hochgerechnet:", round(n_berechtigt_quota_hochgerechnet, 0), "\n\n")

cat("2) Empfänger (zugewiesen, letzte Runde):\n")
cat("  Simuliert:", n_wohngeld_bezogen, "\n")
cat("  Hochgerechnet:", round(n_wohngeld_bezogen_hochgerechnet, 0), "\n")
if (exists("WOHNGELD_ZIEL")) {
  cat("  Ziel:", WOHNGELD_ZIEL, sprintf("(=%.2f%%)\n", WOHNGELD_ZIEL / 778693 * 100))
} else {
  cat("  Ziel: (kein Wohngeld in dieser Runde)\n")
}
cat("  Anteil Stichprobe:", round(n_wohngeld_bezogen / n_gesamt * 100, 2), "%\n")
cat("  Ø Betrag:", round(mean(results$wohngeld_betrag[results$wohngeld_bedarf],
               na.rm = TRUE), 2), "EUR/Monat\n\n")

cat("3) Berechtigt am Stichtag", as.character(stichtag), ":\n")
cat("  Simuliert:", n_berechtigt_stichtag, "\n")
cat("  Hochgerechnet:", round(n_berechtigt_stichtag_hochgerechnet, 0), "\n\n")

cat("Pflegewohngeld (NRW/SH):\n")
cat("  Quote:", round(pfwg_quote, 2), "%\n")
cat("  Empfänger:", round(n_pfwg_hochgerechnet, 0), "Personen\n")
cat("  Ausgaben:", round(pfwg_ausgaben_mrd, 3), "Mrd. EUR/Jahr\n\n")

# ============================================================
# SPEICHERN
# ============================================================

write_csv(results, paste0(ergebnis_pfad, "simulation_ergebnisse_", format(stichtag, "%Y%m%d"), "_GR_BA_n5000_lauf", lauf_tag, ".csv"))
cat("Ergebnisse gespeichert in:", paste0("simulation_ergebnisse_", format(stichtag, "%Y%m%d"), "_GR_BA_n5000_lauf", lauf_tag, ".csv\n"))

  # ============================================================
  # ERGEBNISSE FÜR DIESEN STICHTAG SAMMELN
  # ============================================================

  # Zeile zu alle_ergebnisse hinzufügen
  alle_ergebnisse <- rbind(alle_ergebnisse, data.frame(
    stichtag = stichtag,
    n_simuliert = n_gesamt,
    n_beamte = n_beamte,
    beamte_anteil = n_beamte / n_gesamt * 100,
    hzp_quote = hzp_quote,
    hzp_empfaenger_sim = n_hzp,
    hzp_empfaenger_hoch = n_hzp_hochgerechnet,
    hzp_ausgaben_pro_empf = hzp_ausgaben_pro_empfaenger,
    hzp_ausgaben_mrd = hzp_ausgaben_mrd,
    wohngeld_berechtigt_quota = n_berechtigt_quota,
    wohngeld_berechtigt_stichtag = n_berechtigt_stichtag,
    wohngeld_bezogen_sim = n_wohngeld,
    wohngeld_bezogen_hoch = n_wohngeld_hochgerechnet,
    wohngeld_betrag_avg = mean(results$wohngeld_betrag[results$wohngeld_bedarf], na.rm = TRUE),
    wohngeld_ausgaben_mrd = wohngeld_ausgaben_mrd,
    pfwg_quote = pfwg_quote,
    pfwg_empfaenger_hoch = n_pfwg_hochgerechnet,
    pfwg_ausgaben_mrd = pfwg_ausgaben_mrd,
    privat_mrd = privat_eigenanteil_mrd,
    pv_mrd = pv_ausgaben_mrd,
    eigenanteil_q1_avg = mean(results$eigenanteil_nach_zuschlag_q1, na.rm = TRUE),
    eee_q1_avg        = mean(results$eee_kosten_q1,      na.rm = TRUE),
    ak_q1_avg         = mean(results$ak_kosten_q1,       na.rm = TRUE),
    uv_q1_avg         = mean(results$uv_kosten_q1,       na.rm = TRUE),
    ik_q1_avg         = mean(results$inv_kosten_q1,      na.rm = TRUE),
    zuschlag_q1_avg   = mean(results$zuschlag_betrag_q1, na.rm = TRUE),
    eigenanteil_stichtag_avg = mean(results$eigenanteil_nach_zuschlag, na.rm = TRUE),
    eee_q3_avg        = mean(results$eee_kosten,         na.rm = TRUE),
    ak_q3_avg         = mean(results$ak_kosten,          na.rm = TRUE),
    uv_q3_avg         = mean(results$uv_kosten,          na.rm = TRUE),
    ik_q3_avg         = mean(results$inv_kosten,         na.rm = TRUE),
    zuschlag_q3_avg   = mean(results$zuschlag_betrag,    na.rm = TRUE),
    stringsAsFactors = FALSE
  ))

  # Detaillierte Ergebnisse speichern
  results$stichtag <- stichtag
  alle_results_detail[[as.character(stichtag)]] <- results

  cat("\n--- Stichtag", as.character(stichtag), "abgeschlossen ---\n")

}  # ===== ENDE DER HAUPTSCHLEIFE =====

# ============================================================
# ZUSAMMENFASSUNG ALLER STICHTAGE (2024-2035)
# ============================================================

cat("\n\n")
cat("################################################################\n")
cat("###         ZUSAMMENFASSUNG ALLER STICHTAGE                 ###\n")
cat("################################################################\n\n")

#Eine übersichtliche Tabelle mit allen wichtigen Ergebnissen
zusammenfassung <- data.frame(
  Stichtag = format(alle_ergebnisse$stichtag, "%Y-%m-%d"),
  `HzP_Quote_%` = round(alle_ergebnisse$hzp_quote, 1),
  `HzP_Empf_Tsd` = round(alle_ergebnisse$hzp_empfaenger_hoch / 1000, 0),
  `HzP_Mrd` = round(alle_ergebnisse$hzp_ausgaben_mrd, 2),
  `WG_Empf_Tsd` = round(alle_ergebnisse$wohngeld_bezogen_hoch / 1000, 0),
  `WG_Mrd` = round(alle_ergebnisse$wohngeld_ausgaben_mrd, 2),
  `PfWG_Quote_%` = round(alle_ergebnisse$pfwg_quote, 1),
  `PfWG_Mrd` = round(alle_ergebnisse$pfwg_ausgaben_mrd, 2),
  `Privat_Mrd` = round(alle_ergebnisse$privat_mrd, 2),
  `PV_Mrd` = round(alle_ergebnisse$pv_mrd, 2),
  `EA_Q1_EUR` = round(alle_ergebnisse$eigenanteil_q1_avg, 0),
  `EA_Stichtag_EUR` = round(alle_ergebnisse$eigenanteil_stichtag_avg, 0),
  check.names = FALSE
)

print(zusammenfassung, row.names = FALSE)
cat("\n")

cat("Legende:\n")
cat("  HzP = Hilfe zur Pflege\n")
cat("  WG = Wohngeld\n")
cat("  PfWG = Pflegewohngeld (NRW/SH)\n")
cat("  EA = Eigenanteil\n")
cat("  Tsd = Tausend, Mrd = Milliarden EUR/Jahr\n\n")

# ============================================================
# EXPORT
# ============================================================

write_csv(alle_ergebnisse, paste0(ergebnis_pfad, "zusammenfassung_GR_BA_n5000_lauf", lauf_tag, ".csv"))
cat("Zusammenfassung gespeichert in: zusammenfassung_GR_BA_n5000_lauf", lauf_tag, ".csv\n")

# Detaillierte Ergebnisse für jeden Stichtag
for (stichtag_name in names(alle_results_detail)) {
  filename <- paste0(ergebnis_pfad, "simulation_ergebnisse_", gsub("-", "", stichtag_name), "_GR_BA_n5000_lauf", lauf_tag, ".csv")
  write_csv(alle_results_detail[[stichtag_name]], filename)
}
cat("Detaillierte Ergebnisse für jeden Stichtag gespeichert.\n")

cat("\n=== ALLE SIMULATIONEN BEENDET ===\n")
cat(sprintf("=== Laufzeit: %.1f Minuten ===\n", as.numeric(difftime(Sys.time(), start_time, units = "mins"))))
sink()
