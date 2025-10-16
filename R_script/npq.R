#Script for NPQ data analysis with statistical analysis
#==============================================================================

# Define Variables

# Working directory
WORKDIR <- "~/MyData/fluorcam"

# Order in wich the lines will appear on the plot
target_order <- c("WT1", "h1")

# File to analyse
DATA <- "results_npq.txt"

# File to convert code in the fluorcam output file into length of mesure
CODE <- "temps_mesure_NPQ.txt"

#==============================================================================
#Installation des packages
if (!require(tidyr)) { install.packages("tidyr") }
if (!require(dplyr)) { install.packages("dplyr") }
if (!require(rcompanion)) { install.packages("rcompanion") }
if (!require(ggplot2)) { install.packages("ggplot2") }

library(dplyr)
library(rcompanion)
library(ggplot2)

################################################################################################################################

# Fonction utilisée dans le script

#=======================================================================================================================================
#Check data normality. Get out of th loop if at least one of the group of data does not follow a normal law.
# return TRUE if data follow a normal law
#=======================================================================================================================================
check_normality <- function(shapiro_df) {
  # The normality is assumed to be true
  flag_normal <- TRUE
  
  for (i in 1 : nrow(shapiro_df)) {
    if(shapiro_df[i, 4] > 0.05) {
      # print(paste0("les données ",shapiro_df$grouping_factor[i],"-", 
      # shapiro_df$plant_line[i], " suivent une loi normale"), quote = FALSE)
      
    } else {
      # print(paste0("les données ",shapiro_df$grouping_factor[i],"-", 
      #          shapiro_df$plant_line[i], " ne suivent pas une loi normale"), quote = FALSE)
      
      # En fait les données ne sont pas normales, pas besoin d'aller plus loin
      flag_normal <- FALSE
      break
    }
  }
  return(flag_normal)
}

#=======================================================================================================================================
# Perform analysis
# Return the plot
#=======================================================================================================================================
f_analyseNPQ <- function(df){
  #Determining data normality status
  
  shapiro_df <- df %>%
    dplyr::group_by(line, secondes) %>%
    summarise(statistic = shapiro.test(value)$statistic,
              p.value = shapiro.test(value)$p.value)
  
  flag_normal <- check_normality(shapiro_df)
  
  # Data treatement according to normality status
  if(flag_normal == TRUE) {
    print("Datas follow a normal law")
    
    # Summary
    if (!require(Rmisc)) {install.packages("Rmisc")}
    library(plyr) # dépendence de rmisc
    library(Rmisc) # pour la commande summarySE
    my_summary <- summarySE(df, measurevar="value", groupvars=c("line", "secondes"))
    
    detach(package:Rmisc)
    detach(package:plyr)
    
    # Plot
    
    p<-my_summary %>%
      ggplot(aes(x=secondes, y=value, group = line))+
      geom_ribbon(aes(ymin=value-ci, ymax=value+ci, fill=line),alpha=0.3, linetype=0)+
      scale_fill_manual(values=my_colours)+
      geom_point(aes(x=secondes, y=value, color=line))+
      geom_line(aes(x=secondes, y=value, color=line), size=1)+
      scale_colour_manual(values=my_colours) +
      theme_classic()+
      theme(legend.position= "none")+
      ggtitle(levels(as.factor(df$time)))+
      theme(plot.title=element_text(hjust = 1))
    
    
    
    
  } else {
    print("Datas don't follow a normal law")
    
    # Summary
    conf_int <- groupwiseMedian(data = df,
                                var = "value",
                                group = c("line", "secondes"),
                                conf       = 0.95,
                                R          = 5000,
                                percentile = TRUE,
                                bca        = FALSE,
                                digits     = 3)
    
    
    # Plot
    p<-conf_int %>%   
      ggplot(aes(x=secondes, y=Median, group = line))+
      geom_ribbon(aes(ymin=Percentile.lower, ymax=Percentile.upper, fill=line),alpha=0.3, linetype=0)+
      scale_fill_manual(values=my_colours)+
      geom_point(aes(x=secondes, y=Median, color=line))+
      geom_line(aes(x=secondes, y=Median, color=line), size=1)+
      scale_colour_manual(values=my_colours) +
      theme_classic()+
      theme(legend.position= "none")+
      ggtitle(levels(as.factor(df$time)))+
      theme(plot.title=element_text(hjust = 1))
    
    
  }
  return(plot = p)
}


#==============================================================================
#Define Working directory
setwd(WORKDIR)

#Load the data
df_data <- read.table(DATA, header = FALSE, sep = "\t")
colnames(df_data) <- c("time", "line", "secondes", (paste("rep", c(1:(ncol(df_data)-3)), sep = '')))

#Convert to long format
library(tidyr)

#https://tidyr.tidyverse.org/reference/pivot_longer.html
df_data_long <- df_data %>% pivot_longer(cols = starts_with("rep"), names_to = "replicat")

# Load Code for length of mesurement
code <- read.table(CODE, header = TRUE, sep = '\t')

# Replace value in column "secondes" according to the code
df_data_long$secondes <- code$secondes[match(df_data_long$secondes, code$code)]

# Convert line's name into factor to force the order in the plot  
df_data_long$line <- as.factor(df_data_long$line)


# Split the data frame into a list of data frame, one per line
df_list <- split(df_data_long, f = df_data_long$time)

# Define color
levels_count <- length(levels(as.factor(df_data$line)))

# Exemple avec deux couleurs différentes
my_colours <- c("chartreuse3", "blue4")[1:levels_count]



# Perform analysis
lapply(df_list, f_analyseNPQ)

