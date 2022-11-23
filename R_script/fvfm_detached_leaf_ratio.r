#Script for Fv/Fm data analysis on detached leaf, with statistical analysis
# and recovery ratio computation
#==============================================================================

#Define Variables
##Reference line : the line againt which the statistical tests will be performed
RefLine <- "WT"  #a string between ""

## Order in wich the lines will appear on the plot
target_order <- c("WT", "H1", "npq1", "npq4", "H1npq1", "H1npq4")   # your lines names

## Order for the mesuring time point in the graph
time_order <- c("T0", "T2H-HL", "T0.5H-R", "T2H-R", "T4H-R")     # your time points

# Working directory : Where the files are and results will be saved
WORKDIR <- "~/MyData/fluorcam"

# File to analyse
DATA <- "your_file.txt"

#==============================================================================
#Installation des packages
if (!require(tidyr)) { install.packages("tidyr") }
if (!require(dplyr)) { install.packages("dplyr") }
if (!require(tidyverse)) { install.packages("tidyverse") }
if (!require(devtools)) { install.packages("devtools") }
if (!require(rstatix)) { install.packages("rstatix", repos = "https://cloud.r-project.org") }
if (!require(ggbeeswarm)) { install.packages("ggbeeswarm") }
if (!require(RColorBrewer)) { install.packages("RColorBrewer") }
if (!require(rcompanion)) { install.packages("rcompanion") }

################################################################################################################################

# Fonction utilisée dans le script

#=======================================================================================================================================
# Check data normality. Get out of th loop if at least one of the group of data does not follow a normal law.
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
      
      # If data don't follow a normal law, don't go further
      flag_normal <- FALSE
      break
    }
  }
  return(flag_normal)
}

#=======================================================================================================================================
# Take the results of Anova test
# Return TRUE if at least one mean is significantly different 
#=======================================================================================================================================
check_anova <- function(anova_results) {
  flag_anova <- FALSE
  
  for (i in 1 : nrow(anova_results)) {
    if (anova_results$p[i] < 0.05) {
      print (paste0("Anova test compares the mean, pvalue for the group ", anova_results$time[i] , " is < 0.05 this means that at least one of the medians is significantly different from the others, a post hoc Tukey test is performed"))
      flag_anova <- TRUE
    } else {
      print (paste0("Anova test compares the mean, pvalue for the group ", anova_results$time[i] , " is > 0.05 this means that there is no significant difference between means"))
    }
  }    
  
  return(flag_anova)
}

#=======================================================================================================================================
# Perform Kruskal-Wallis test
# Return TRUE if one of the median is significantly different
#=======================================================================================================================================
check_kruskal <- function(kruskal_pval) {
  flag_kruskal <- FALSE
  
  for (i in 1 : nrow(kruskal_pval)) {
    if (kruskal_pval$p[i] < 0.05) {
      print (paste0("Kruskall Wallis test compares medians, pvalue for the group ", kruskal_pval$time[i] , " is < 0.05 this means that at least one of the medians is significantly different from the others, a post hoc Dunn test is performed"))
      flag_kruskal <- TRUE
    } else {
      print (paste0("Kruskall Wallis test compares medians, pvalue for the group ", kruskal_pval$time[i] , " is > 0.05 this means that there is no significant difference between medians"))
    }
  }    
  
  return(flag_kruskal)
}

#=======================================================================================================================================
# Perform Dunn test
# Return the pvalue in a dataframe
#=======================================================================================================================================
test_dunn <- function() {
  pval <- as.data.frame(df_data_long%>%
                          mutate(line = fct_relevel(line, target_order)) %>% 
                          group_by(time) %>% dunn_test(value ~ line, p.adjust.method = "BH"))
  #print(df_data_long %>% group_by(time) %>% dunn_test(value ~ line, p.adjust.method = "BH"))
  return(pval)
}

#==============================================================================
#Define Working directory
setwd(WORKDIR)

#Load the data
df_data <- read.table(DATA, header = FALSE, sep = "\t")
colnames(df_data) <- c("time", "line", (paste("rep", c(1:(ncol(df_data)-2)), sep = '')))

#Convert to long format
library(tidyr)
library(tidyverse)
#https://tidyr.tidyverse.org/reference/pivot_longer.html
df_data_long <- df_data %>% pivot_longer(cols = starts_with("rep"), names_to = "replicat")

#___________________________________________________________________________________________________________________________________
# Statistical analysis
#___________________________________________________________________________________________________________________________________

library(ggbeeswarm) # for funcnction geom_quasirandom
library(RColorBrewer) # to define colors
library(rstatix)  # for statistical test
library(rcompanion) # to compute confidence interval for non parametric data
library(forcats)


# Convert line's name into factor to force the order in the plot 
df_data_long$line <- as.factor(df_data_long$line)



# Determining data normality status
shapiro_df <- df_data_long %>%
  dplyr::group_by(time, line) %>%
  summarise(statistic = shapiro.test(value)$statistic, 
            p.value = shapiro.test(value)$p.value)

flag_normal <- check_normality(shapiro_df)

# Data treatement according to normality status
if(flag_normal == TRUE) {
  print("Datas follow a normal law")
  
  # Summary
  if (!require(Rmisc)) {install.packages("Rmisc")}
  library(plyr) # rmisc dependance
  library(Rmisc) # for the command summarySE
  my_summary <- summarySE(df_data_long, measurevar="value", groupvars=c("time", "line"))
  
  # The 2 packages must be detach because they can prevent the goup_by function to work properly
  detach(package:Rmisc)
  detach(package:plyr)
  
  
  # Stats
  anova_results <- df_data_long %>% group_by(time) %>% anova_test(value ~ line)
  flag_anova <- check_anova(anova_results)
  if (flag_anova == TRUE) {
    tukey_results <- as.data.frame(df_data_long %>% mutate(line = fct_relevel(line, target_order)) %>%
                                     group_by(time) %>%  tukey_hsd(value ~ line))
  }
  
  # Save the files
  write.table(my_summary, file = "Summary.txt", 
              quote = FALSE, row.names = FALSE, sep = '\t')
  write.table(anova_results, file = "Anova.txt", 
              quote = FALSE, row.names = FALSE, sep = '\t')
  if (flag_anova == TRUE) {
    write.table(tukey_results[, c(1,3,4, 9, 10)], file = "Tukey.txt", 
                quote = FALSE, row.names = FALSE, sep = '\t')
  } 
  
  # Prepare data to add stars on error bar
  if(flag_anova == TRUE) {
    df <- dplyr::filter(tukey_results, grepl(RefLine, group1))
    
    for (i in levels(as.factor(df$time))) {
      x <- c(i, "line", RefLine, RefLine, rep(x="", 6))
      df <- rbind(x, df)
    }
    
    df <- df[ with(df, order(time, group2)),]
    
    df2 <- cbind(my_summary, df$p.adj.signif)
    names(df2)[names(df2) == "df$p.adj.signif"] <- "Labels"
    
    # Plot
    p<-df2 %>% mutate(line = fct_relevel(line, target_order)) %>%  
      ggplot(aes(x=time, y=value, group = line))+
      geom_col(aes(x=time, y=value), color="dark grey", fill="white")+
      scale_x_discrete(limits=time_order)+
      scale_y_continuous(breaks=seq(0,1,0.2))+
      geom_quasirandom(data = df_data_long, aes(x = time, y = value, color = line), 
                       color="green", width = 0.3, alpha = 0.6)+
      geom_errorbar(aes(x = as.factor(time), ymin=value-ci, ymax=value+ci), 
                    width=.2, position=position_dodge(0.9), color = "black") +
      geom_text(aes(x = as.factor(time), y= value+ci + 0.018 , 
                    label = Labels),
                size = 3, position = position_dodge(0.9), inherit.aes = TRUE)+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
      facet_wrap(~line, nrow=1, scales = "free_x")
    
    print(p)
    
    ggsave("fvfm_plot.svg", width=8, height=3)
    
  }else {
    print("Datas are not significantly different, the Tukey test was not performed.")
  }
  
  
} else {
  print("Datas don't follow a normal law")
  
  # Summary
  conf_int <- groupwiseMedian(data = df_data_long,
                              var = "value",
                              group = c("time", "line"),
                              conf       = 0.95,
                              R          = 5000,
                              percentile = TRUE,
                              bca        = FALSE,
                              digits     = 3)
  
  
  
  # Stats
  kruskal_pval <- (df_data_long %>% group_by(time)%>%kruskal_test(value ~ line)) %>% select(time, p)
  
  flag_kruskal <- check_kruskal(kruskal_pval)
  if (flag_kruskal == TRUE) { pval_dunn <- test_dunn() }
  
  # Save the files
  write.table(conf_int, file = "Summary.txt", 
              quote = FALSE, row.names = FALSE, sep = '\t')
  write.table(kruskal_pval, file = "Kruskal.txt", 
              quote = FALSE, row.names = FALSE, sep = '\t')
  
  if (flag_kruskal == TRUE) {
    write.table(pval_dunn[, c(1, 3, 4, 8, 9, 10)], file = "Dunn.txt", 
                quote = FALSE, row.names = FALSE, sep = '\t')
  } 
  
  # Prepare data to add stars on error bar
  if(flag_kruskal == TRUE) {
    df <- dplyr::filter(pval_dunn, grepl(RefLine, group1))
    
    for (i in levels(as.factor(df$time))) {
      x <- c(i, "line", RefLine, RefLine, rep(x="", 6))
      df <- rbind(x, df)
    }
    
    df <- df[ with(df, order(time, group2)),]
    
    df2 <- cbind(conf_int, df$p.adj.signif)
    names(df2)[names(df2) == "df$p.adj.signif"] <- "Labels"
    
    # Plot
    p<-df2 %>% mutate(line = fct_relevel(line, target_order)) %>%  
      ggplot(aes(x=time, y=Median, group = line))+
      geom_col(aes(x=time, y=Median), color="dark grey", fill="white")+
      scale_x_discrete(limits=time_order)+
      scale_y_continuous(breaks=seq(0,1,0.2))+
      geom_quasirandom(data = df_data_long, aes(x = time, y = value, color = line), 
                       color="green", width = 0.3, alpha = 0.6)+
      geom_errorbar(aes(x = as.factor(time), ymin=Percentile.lower, ymax=Percentile.upper), 
                    width=.2, position=position_dodge(0.9), color = "black") +
      geom_text(aes(x = as.factor(time), y= Percentile.upper + 0.018 , 
                    label = Labels),
                size = 3, position = position_dodge(0.9), inherit.aes = TRUE)+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
      facet_wrap(~line, nrow=1, scales = "free_x")
      
    
    print(p)
    
    ggsave("fvfm_plot.svg", width=8, height=3)
    
  }else {
    print("Datas are not significantly different, the Dunn test was not performed.")
  }
}

# Compute the ratio T4-R/T0
# Compute mean and SD
summary <- df_data_long %>% dplyr::group_by(time, line) %>%
  summarise(mean = mean(value, na.rm = TRUE), sd= sd(value, na.rm = TRUE))

# Compute recovery
# Split the data frame in a list of data frame, one for each time point
df_list <- split(summary, f = summary$time)

recovery <- data.frame(Line= df_list[["T4H-R"]]$line)

recovery$Ratio <- df_list[["T4H-R"]]$mean/df_list[["T0"]]$mean

# Convert line's name into factor to force the order in the plot  
recovery$Line <- as.factor(recovery$Line)



#bar plot
p2 <- recovery %>% mutate(Line = fct_relevel(Line, target_order)) %>%  
  ggplot(aes(x=Line, y=Ratio))+
  geom_bar(stat="identity", position = position_dodge(width = 0.9), width = 0.8, fill = "chartreuse3")+
  labs(title="", 
       x="", y = "Recovery ratio")+
  ylim(0, 1)+
  theme_classic()
  
p2
