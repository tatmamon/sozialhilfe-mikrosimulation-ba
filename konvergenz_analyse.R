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
