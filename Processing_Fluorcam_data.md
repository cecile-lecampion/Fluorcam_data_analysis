#  Processing Fluorcam data

# Data preparation

Fluorcam datas are exported as TSV files with `.TXT` extension. Alla parameters are inside the files, ie 78 rows.

When working on whole plants, one file per time point and per line is generated, but when working with detached leaf, one file per time point per many lines is generated.

## Pre process detached leaf files

Detached leaf files look like this :

```
FluorCam7 Numeric Avg Export File
-----------------------------------
	wt	Area 2	Area 3	Area 4	Area 5	Area 6	Area 7	Area 8	OX:RSH3.1	Area 10	Area 11	Area 12	Area 13	Area 14	Area 15	Area 16	ch1	Area 18	Area 19	Area 20	Area 21	Area 22	Area 23	Area 24	OX:RSH3.1 ch1	Area 26	Area 27	Area 28	Area 29	Area 30	Area 31	Area 32	glk1-2	Area 34	Area 35	Area 36	Area 37	Area 38	Area 39	Area 40
Size [pixels]	1195	1245	1123	1400	1995	1854	1448	1197	1759	2316	2154	2490	2458	2129	2451	1775	624	745	924	971	818	800	605	473	361	586	912	991	1095	1912	1139	986	521	807	558	1533	805	973	858	949
Fo	81.22	88.27	87.07	90.98	94.36	87.69	75.15	70.05	113.90	122.69	124.79	223.04	127.89	122.85	121.62	101.26	64.70	74.19	79.15	84.47	72.20	74.83	69.22	62.25	53.05	60.32	69.82	67.79	94.70	105.32	99.14	71.94	46.40	51.25	53.19	69.46	57.53	58.06	56.22	49.52
Fm	198.56	207.46	208.97	227.45	228.43	221.01	201.82	176.49	254.02	273.28	276.82	457.61	288.31	271.02	286.26	239.93	135.94	141.39	151.12	147.31	141.66	143.70	136.45	114.97	104.83	106.93	130.91	123.05	149.42	178.86	145.72	124.72	90.45	109.60	114.47	165.00	118.37	125.61	117.04	127.00
```

The script `split_fluorcam_txt.sh` can split the file to create a file per timepoint per line.

### Prerequisite

In the file, each replicate is named `Area#`, the line’s name replace the first replicate of each line (as in the exemple above)

The file should be named according to the following pattern : YYYY_MM_DD_qI_.....TXT

If your files don’t follow those prescriptions, you can easily change them.

#### Add the line’s name to first replicate for each line

In the repertory contening the files :

```bash
sed 's/Area1/wt/' File_to_modify.txt > mofidied_file.txt
```

To change more than one field at the same time, just add another command separated by a `;` :

```bash
sed 's/Area1/wt/ ; s/Area9/OX:RSH3.1/' File_to_modify.txt > mofidied_file.txt
```

###  Split the file 

#### Install xsv : A fast CSV command line toolkit written in Rust.

Follow instruction on : https://github.com/BurntSushi/xsv

#### Execute `split_fluorcam_txt.sh`

- First copy the script to your tools repertory (In your path)

- Make it executable : 

    ```bash
    chmod +x split_fluorcam_txt.sh
    ```

- Execute 

    ```bash
    split_fluorcam_txt.sh File_to_split.TXT
    ```

    A new repertory `out` is created to save the results. the script extract what is after the `qI` in the file name and add the line’s name

    If your file is :`2022_09_12_qI_T0.TXT` and contains 3 lines `A`, `B`, `C` you will get 3 files in the `out repertory`:

    `T0_A.TXT`, `T0_B.TXT`, `T0_C.TXT`  

## Extract Fv/Fm and npq data from the raw files

To extract the data from the raw files exported from the fluorcam as `.TXT` , 2 scripts are available : `compute_fv_fm_ratio.sh` and `compute_npq.sh` 

First copy the script to a repertory in your PATH and make them executable :

```bash
chmod +x compute_fv_fm_ratio.sh				# to make the script executable
chmod +x compute_npq.sh
```

### Fv/Fm data

The script `compute_fv_fm_ratio.sh`does not take any parameter, it will process all the file with the `.TXT` extension (it is case sensitive) that are in the repertory where the script is executed.

This extension may be change if necessary : open the script in a text editor, change the value of the variable FILE_EXTENSION (line 13) without changing any other thing. Save the new script.

It collects the Fv and Fm value and compute the ratio. It extract the time point and the line’s name from the file’s name. For that purpose the file’s name must follow this pattern : Time-point_Line.TXT

```bash
#!/bin/bash
#-----------------------------------------------------------
# This script extract the Fv and Fm data from a file exported from the fluorcam.
# It does not take parameters. It must be executed in the repertory that contains the files to process.
# It checks the extension of the files in the repertory, then for each file
# it extracts the time point of the mesure and the name of the line contained in the file's name(pattern for file's # name : Time-point_Line.TXT),
# It extracts rows Fv and  Fm and compute Fv/Fm for each réplicat. 
# Data from all files are concatenated in one single output file.
# Synthax: compute_fv_fm_ratio.sh > ressultat.txt
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

```

To execute the script :

```bash
cd /PATH/TO/Fluorcam-output-DIR/
compute_fv_fm_ratio.sh > results_FvFm.txt
```

### npq Data

The npq data can be directly extract from the fluorcam output file but, when differences between raw values are very small, the round numbers generated by the fluorcam can all be equal which leads to diffculties for statistical analysis. For this reason, the script `compute_npq.sh` collect Fm and Fm’ rows and compute the npq for each mesure : (Fm-Fm’)/Fm’

The script is used in the same way as the `compute_fv_fm_ratio.sh`  script : no parameters, same file nam pattern...

```
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

```

To execute the script :

```bash
cd /PATH/TO/Fluorcam-output-DIR/
compute_npq.sh > results_npq.txt
```

 # Data analysis

Five R scripts are availlable to perform data analysis.

- For detached leaf

fvfm_detached_leaf_ratio.R

- For Fv/Fm analysis

fvfm_V2.r

fvfm_multiple_manip.r

- For NPQ analysis

npq2_3separate_graph.R

npq2_3separate_graph_multiple_manip.R

For all the scripts, only the first part entitled “Define Variables” has to be modified to fit with your data. Once those variables have been defined , you can run the script in one click on `Source`, in the upper left panel. You can also execute line by line clicking on `Run`

### Details variables

- For all the scripts

WORKDIR : The path to your working directory. It must be written between “” 

​		exemple :

```R
# Working directory
WORKDIR <- "~/MyData/fluorcam"
```

target_order : The order in which your lines will appear on the plot. If not or wrongly define R use alphabetical order. 

​						It is a vector of your lines names.

:warning: R is case sensitive, be sure to use exactly the same notation in your target order and in your data

​		exemple :

```R
# Order in wich the lines will appear on the plot
target_order <- c("WT1", "h1", "npq1", "npq4", "h1npq1", "h1npq4")
```

- For single assy scripts

DATA : the name of the file to analyse. If this file is in the working directory, only the name is necessary, if the file is somewhere else, then the whole path to the file is necessary.

​		exemple :

```R
# File to analyse
DATA <- "results_fvfm.txt"
```

- For FvFm scripts

RefLine : your reference line against which all the statistical tests will be performed. Significance will be determine compare to this line.

​		exemple :

```R
##Reference line : the line againt which the statistical tests will be performed
RefLine <- "WT"  #a string between ""
```

- For npq scripts

CODE : A file in TSV format (Tabulated, Separated, Values) , linking the L1, L2... in fluorcam output file to the time of mesure 

code	secondes
L1	15
L2	30
L3	45
L4	60
L5	90
L6	120

```R
# File to convert code in the fluorcam output file into length of mesure
CODE <- "temps_mesure_NPQ.txt"
```

- For detached leaf only

time_order : The order in which your data willa ppear on plots. It is a vector of your time point.

​		exemple :

```R
## Order for the mesuring time point in the graph
time_order <- c("T0", "T2H-HL", "T0.5H-R", "T2H-R", "T4H-R")
```

