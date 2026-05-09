#!/bin/bash
SCRIPTDIR="/home/.samba/homes/tmamontova/Bachelorarbeit/Input/R runs"
TEMPLATE="${SCRIPTDIR}/Fortschreibung_E+V_BA_GR_n5000_template.R"
cd "$SCRIPTDIR"

TOTAL=5000
BATCH_SIZE=50
count=0

for lauf in $(seq 1 5000); do
  Rscript "${TEMPLATE}" $lauf &
  count=$((count + 1))
  if [ $count -ge $BATCH_SIZE ]; then
    wait
    count=0
    echo "Batch abgeschlossen, Lauf $lauf / 5000"
  fi
done

wait
echo "Alle 5000 Laeufe abgeschlossen."
