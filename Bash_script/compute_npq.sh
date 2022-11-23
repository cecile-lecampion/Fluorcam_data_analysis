#!/bin/bash
#-----------------------------------------------------------
# This script extract Fm and Fm' data from the fluorcam output files.
# It does not take parameters. It must be executed in the repertory that contains the files to process.
# It checks the extension of the files in the repertory, then for each file
# it extracts the time point of the mesure and the name of the line contained in the file's name(pattern for file's # name : Time-point_Line.TXT),
# It extracts Fm and  Fm' data and compute (Fm-Fm’)/Fm’ for each réplicat. 
# Data from all files are concatenated in one single output file.
# Synthax: compute_npq.sh > ressultat.txt
#-----------------------------------------------------------

FILE_EXTENSION='TXT'

# Vérifications
[ "$(ls -1 *.$FILE_EXTENSION 2> /dev/null | wc -l)" -eq 0 ] && {
    echo "Aucun fichier *.$FILE_EXTENSION dans le répertoire" >&2
    exit 1
}

# Traitement
for F in *.$FILE_EXTENSION; do
    perl -wne '
        # Nettoie la fin de la ligne (enlève les \r\n)
        s/\r?\r?\n$//;

        # Découpe la ligne sur les tabulations
        @F = split "\t";

        # Sauvegarde de la ligne Fm
        # On obtient un tableau (à une dimension) du type:
        # [ 718.01, 700.17, 733.21, 675.83 ]

        if ($F[0] eq "Fm") { 
			@Fm = @F[1..$#F];
        }

        # Sauvegarde des lignes Fm_.... dans un tableau @Fmprime de tableau de valeurs
        # Les valeurs étant: Lx ou Dx, suivit des colonne 2 jusqu à la fin
        # On obtient un tableau de tableaux (tableau à deux dimension) du type:
        # 
        #    [
        #        [ L1, 327.86, 310.14, 326.03, 293.72 ],
        #        [ L2, 238.37, 230.81, 239.82, 216.77 ],
        #        [ L3, 207.84, 202.09, 211.54, 190.84 ]
        #    ]

        if ($F[0] =~ /^Fm_([LD]\d+)$/) { 
            push @Fmprime, [$1, @F[1..$#F]];
        }

        END {
            # Extraction depuis le nom du fichier des valeur Tx et lignée
            # Exemple: avec le fichier "T0_H1.TXT" on récupère:
            # $time = "T0" et $line = "H1"

            $filename = $ARGV;
            $filename =~ s/\.txt$//i;
            ($time, $line) = split /_/, $filename;

            # Calcul de (Fm-Fmprime)/Fmprime
            # Pour chaque sous-tableau du tableau Fmprime
            for $prime (@Fmprime) {

                # On enlève le code au début du sous-tableau
                # Si dans @{$prime} on a [ L1, 327.86, 310.14, 326.03, 293.72 ]
                # On obtient "L1" dans $code et [ 327.86, 310.14, 326.03, 293.72 ] dans @{$prime}
                $code = shift @{$prime};

                $i = 0;
                # Initialisation de tableau de NQP résultat avec en 1er le $code
                @resultNpq = ($code);

                for $primeValue (@{$prime}) {
                    # Récuprèe la valeur Fm correspondant à la primeValue
                    $fm = $Fm[$i];
                    # Calcul du npq
                    $npq = ($fm - $primeValue) / $primeValue;
                    # On ajoute la valeur de NPQ au tableau de résultat
                    push @resultNpq, $npq;
                    $i += 1;
                }

                # Affiche les 2 premières colonnes à partir du nom du fichier
                # suivi du calcul Npq pour chaque réplicat
                print join("\t", $time, $line, @resultNpq), "\n";
            }
        }
        ' $F;
done
