#!/bin/bash
#-----------------------------------------------------------
# # Ce script extrait les données Fv et FM contenu dans les fichiers de sortie du fluorcam.
# Il ne prend pas de paramètres. Il doit être exécuté dans le répertoire contenant les fichiers
# à analyser et seulement eux.
# Il teste la présence de fichiers avec l'extension définie dans FILE_EXTENSION puis pour chaque
# fichier extrait le temps du point de mesure et le nom de lignée (format de nom de fichier : Temps_Lignée.TXT),
# les lignes Fv et Fm et fait le calcul pour Fv/Fm pour chaque réplicat. 
# Les données de tous les fichiers sont concaténées dans un seul fichier de sortie.
# La syntaxe est : compute_fv_fm_ratio.sh > ressultat.txt
#-----------------------------------------------------------

FILE_EXTENSION='TXT'

# Vérifications
[ "$(ls -1 *.$FILE_EXTENSION 2> /dev/null | wc -l)" -eq 0 ] && {
    echo "Aucun fichier *.$FILE_EXTENSION dans le répertoire" >&2
    exit 1
}

# Traitement
for F in *.$FILE_EXTENSION; do
    perl -F'\t' -wane '
        if ($F[0] =~ /^Fv$/) { @Fv = @F[1..$#F] }
        if ($F[0] =~ /^Fm$/) { @Fm = @F[1..$#F] }

        END {
            $filename = $ARGV;
            $filename =~ s/\.txt$//i;
            ($time, $line) = split /_/, $filename;

            # Affiche les 2 premières colonnes à partir du nom du fichier
            print join("\t", $time, $line), "\t";

            # Calcul de Fv / Fm
            for $i (0..$#Fv) {
                push @ratio, $Fv[$i] / $Fm[$i];
            }
            print join("\t", @ratio), "\n";
        }
        ' $F;
done

