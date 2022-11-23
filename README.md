# Fluorcam_data_analysis
This repository contains many tools to analyse data exported from a fluorcam. The use of all tools is describe in Processing_Fluorcam_ data.md

In the bash_script repertory you will find command line script to pre process the data : 

- Split a large file into one file per line : split_fluorcam_txt.sh
- Extract the Fv and Fm datas, compute the ratio and concatenate all files in a single one : compute_fv_fm_ratio.sh
- Extract the data and compute the npq : compute_npq.sh 



In the R_script repertory you will find R script to perform analysis

- For detached leaf  
fvfm_detached_leaf_ratio.R
- For Fv/Fm analysis  
  fvfm_V2.r   
fvfm_multiple_manip.r  
- For NPQ analysis  
  npq2_3separate_graph.R  
npq2_3separate_graph_multiple_manip.R  



In the R_markdown repertory you will find the same scripts in R markdown to produce beautiful repport of your analysis
