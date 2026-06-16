# sozialhilfe-mikrosimulation-ba
Eine Sammlung von R-Codes, die in der Bachelorarbeit zum Thema "Modell zur Abschätzung der Sozialhilfequote in deutschen Pflegeheimen" genutzt wurden.
# Modell zur Abschätzung der Sozialhilfequote in deutschen Pflegeheimen

**Tatiana Mamontova** — Bachelorarbeit  

Dieses Repository enthält den R-Kerncode der stochastischen Mikrosimulation, die der Bachelorarbeit zugrunde liegt, sowie weitere R-Codes. Das Modell schätzt die Quote der Hilfe zur Pflege (HzP) unter Bewohnerinnen und Bewohnern stationärer Pflegeeinrichtungen in Deutschland für den Zeitraum 2024–2035.

---

## Modell

- **Datengrundlage:** SOEP (Einkommens- und Vermögensdaten) sowie vdek (Heimentgelte)
- **Methode:** Stratifiziertes Bootstrap-Sampling mit Zurücklegen (Bundesland × Geschlecht × Altersgruppe), dann Monte Carlo Simulation
- **Szenario:** Geltendes Recht — einmalige Dynamisierung der PV-Leistungen 2028 gemäß § 30 SGB XI
- **Stichtage:** 1. Juli 2024 bis 1. Juli 2035 (12 Stichtage)

---

## Struktur

```
├── Mikrosimulation_BA_GR_n5000_template für BA.R   # Kern-Simulationsscript (ein Lauf)
│
├── WATT Scripts/
│   ├── generate_BA_scripts.R       # Erzeugt 600 Scripts + run_BA_GR_600.sh
│   ├── run_BA_GR_600.sh            # Bash-Script: m=200 Läufe je N parallel (WATT-Cluster)
│   ├── generate_n5000_5000runs.R   # Erzeugt Template + Bash-Script für n_ref=5.000 Referenzläufe
│   └── run_BA_GR_n5000_5000runs.sh # Bash-Script: n_ref=5.000 Referenzläufe parallel (WATT-Cluster)
│ 
├── mce_analyse.R                   # MCE-Berechnung nach Koehler et al. (2009)
└── konvergenz_analyse.R            # Konvergenzanalyse der HzP-Quote über Simulationsläufe
```

---

## Workflow

**Schritt 1 — Stichprobengrößenbestimmung (MCE-Analyse)**

```r
source("WATT Scripts/generate_BA_scripts.R")  # erzeugt 600 R-Scripts + run_BA_GR_600.sh
# → run_BA_GR_600.sh auf dem Cluster ausführen (m=200 Läufe je N ∈ {1.000, 5.000, 10.000})
source("mce_analyse.R")                       # MCE je N berechnen und ausgeben
```

**Schritt 2 — Hauptsimulation (n_ref=5.000 Modellläufe, N=5.000)**

```r
source("WATT Scripts/generate_n5000_5000runs.R")  # erzeugt Template + Bash-Script
# → run_BA_GR_n5000_5000runs.sh auf dem Cluster ausführen (100 Batches à 50 parallel)
```
**Schritt 3 — Konvergenzanalyse der HzP-Quote**

```r
source("konvergenz_analyse.R")  # erzeugt Template + Bash-Script
# → run_BA_GR_n5000_5000runs.sh auf dem Cluster ausführen (100 Batches à 50 parallel)
```
---

## Voraussetzungen

R-Pakete:

```r
install.packages(c("tidyverse", "lubridate", "arrow"))
```

---

## Reproduzierbarkeit

Seeds werden stichtag- und laufspezifisch gesetzt:

```r
set.seed(as.integer(stichtag) + lauf_nr)
```

Damit sind alle Ergebnisse bei gegebenen Eingabedaten vollständig reproduzierbar.

---

## Referenz

Koehler, E., Brown, E. & Haneuse, S. J.-P. A. (2009). On the Assessment of Monte Carlo Error in Simulation-Based Statistical Analyses. *The American Statistician*, 63(2), 155–162.
