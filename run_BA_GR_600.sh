#!/bin/bash
SCRIPTDIR="/home/.samba/homes/tmamontova/Bachelorarbeit/Input/R runs"
ERGEBNIS="/home/.samba/homes/tmamontova/Bachelorarbeit/Ergebnisse"
cd "$SCRIPTDIR"

BATCH_SIZE=50
count=0

for n_var in 1000 5000 10000; do
  for lauf in $(seq -w 001 200); do
    SCRIPT="Fortschreibung E+V_BA_GR_n${n_var}_lauf${lauf}.R"
    if [ -f "$SCRIPT" ]; then
      Rscript "$SCRIPT" &
      count=$((count + 1))
      if [ $count -ge $BATCH_SIZE ]; then
        wait
        count=0
      fi
    fi
  done
done

wait
echo "Alle 600 BA GR Laeufe abgeschlossen."
