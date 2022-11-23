#!/bin/bash
# Découpe le fichier txt passé en paramètre

[[ $# -ne 1 || ! -r $1 ]] && {
    echo "syntax: $0 fichier.TXT" >&2
    exit 1
}

# Fichier passé en paramètre
TXT_FILE="$1"

# Répertoire de sortie: "out" sous le répertoire conteanant le fichier TXT
OUT_DIR="$(dirname "$TXT_FILE")/out"
# Crée le répertoire out si besoin
mkdir -p "$OUT_DIR"

# Extraction de la partie du nom du fichier après la date et "_qI_" et avant ".TXT"
# exemple, si TXT_FILE=2022_09_12_qI_T2H_HL.TXT alors TIME=T2H_HL
TIME=$(basename "$TXT_FILE" | perl -wne 'print $1 if /^\d{4}_\d\d_\d\d_qI_([^.]+)\.TXT/i')
# Vérification du format du nom du fichier
[ -z "$TIME" ] && {
    echo "Le format du nom du fichier doit être: YYYY_MM_DD_qI_............TXT" 
    exit 1
}

# Entête se trouvant sur la 3ème ligne sans le \r final éventuel
HEADER=$(sed -n '3s/\r//;3p' "$TXT_FILE")

# Vérification du format de l'entête
echo "$HEADER" | grep -q 'Area \d'
[ $? -ne 0 ] && {
    echo "Un entête contenant des \"Area XX\" doit être présent sur la 3ème ligne"
    exit 1
}

# Construction de la liste des sélections de colonnes
COLUMN_SEL=$(echo -n "$HEADER" |
    perl -wpe 's/[^\w\t]/_/g; s/\t/\n/g' | # remplace les caractères non alphahnum ou tabulation par _ puis découpe sur les tabulations
        grep -v '^$' | # supprime la ligne vide au début (à cause de l'absence de nom de colonne)
            nl | # numérote les lignes
                egrep -v '^\s*\d+\s+Area_' # exclu les lignes Area
)


# Sélectionne les colonnes avec xsv
# Input:
#   START_COL
#   STOP_COL
#   OUT_FILE_NAME
function xsv_select {
    local START_COL=$1
    local STOP_COL=$2
    local OUT_FILE_NAME="$3"
    sed 1,2d $TXT_FILE | # Enlève les 2 1ères lignes du fichier (entête inutile et incompatible avec xsv)
        xsv select -d '\t' 1,${START_COL}-${STOP_COL} | # selection de la colonne 1 et de l'intervale START STOP
            xsv fmt -t '\t' > "$OUT_FILE_NAME" # formatage de sortie avec des tabulations 
}


# Crée les fichiers TXT sur la sélection des colonnes avec xsv
PREV_LINE_NB=0
PREV_LINE_NAME=''
while read LINE_NB LINE_NAME; do
    START_COL=$PREV_LINE_NB
    STOP_COL=$LINE_NB
    if [ $START_COL -ne 0 ]; then
        OUT_FILE_NAME="$OUT_DIR/${TIME}_$PREV_LINE_NAME.TXT"
        echo "$OUT_FILE_NAME $START_COL $STOP_COL"
        xsv_select $START_COL "$STOP_COL" "$OUT_FILE_NAME"
    fi
    PREV_LINE_NB=$(( $LINE_NB + 1 ))
    PREV_LINE_NAME=$LINE_NAME
done < <( echo "$COLUMN_SEL" )

# Traite la dernière lignée
START_COL=$PREV_LINE_NB
STOP_COL=''
OUT_FILE_NAME="$OUT_DIR/${TIME}_$PREV_LINE_NAME.TXT"
echo "$OUT_FILE_NAME $START_COL $STOP_COL"
xsv_select $START_COL "$STOP_COL" "$OUT_FILE_NAME"

