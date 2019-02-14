#  ####################################################################### 
#       File-Name:      Graph_ViolenceData_Analysis_170223.R
#       Version:        R 3.3.2
#       Date:           Feb 23, 2017
#       Author:         MM
#       Purpose:        Graph violence data that accompanies statistical
#                       analyses when applying three basic algorithms:
#                       OLS, logistic regression and random forest
#       Input Files:    AllViolenceData_170216.csv (cleaned data on violence)
#       Output Files:   NONE
#       Data Output:    NONE
#       Previous files: NONE
#       Dependencies:   Gather_ViolenceData_170216.R
#       Required by:    NONE 
#       Status:         IN PROGRESS
#       Machine:        Mac laptop
#  ####################################################################### 


library(readr)          # easier reading of flat files
library(ggplot2)        # pretty graphs made easy
library(reshape2)       # nice correlation plots
library(magrittr)       # ceci n'est pas une pipe
library(here)           # enabling relative paths 

# ::::::: SOME USEFUL DEFINITIONS :::::::::::::::::::::::::::::::::::::::::::::  


# define additional paths for files you will use. In each case, determine
# appropriate additions to the path

inFileName1   <- here("data", "processed", "AllViolenceData_170216.csv")   # cleaned data on violence
outGraphName1  <- here("graphs", "CorrelationPlot.pdf")                     # output file name
outGraphName2  <- here("graphs", "OrganizedCrimeDeaths.pdf")                # output file name
outGraphName3  <- here("graphs", "DeathsByCategory.pdf")                    # output file name
outGraphName4  <- here("graphs", "Wounded_Casualties")                      # output file name


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
# :::::::::::::::::::::: LOADS DATA :::::::::::::::::::::::::::::::::::::::::::  


# ::::::: LOADING RAW DATA
# note that read_csv() guesses column types, so that date is read as a date 
# very useful for plotting time series

AllData <- read_csv(inFileName1) 
    
# rough validations that data was correctly loaded
names(AllData)
nrow(AllData)
summary(AllData)


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
# :::::::::::::::::::::: SOME INITIAL GRAPHING ::::::::::::::::::::::::::::::::  

# a simple correlation plot among all variables in the model to assess
# structure and facilitate model diagnosis and interpretation 

# to create the correlation plot, we need to:
# first define the subset of variables to those variables that we are focusing on

col_vector <- c("organized_crime_dead", "organized_crime_wounded", "afi", "army",
          "navy", "federal_police", "long_guns_seized", "small_arms_seized",
          "clips_seized", "cartridge_sezied")

# then create a dataframe that creates the combinations from the correlation 
# matrix
correlations <- AllData %>% 
  select_(.dots = col_vector) %>%
  cor(.) %>%
  round(2) %>%
  melt()

# and then we graph
ggplot(correlations, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  theme_minimal() +
  scale_x_discrete("") +
  scale_y_discrete("") +
  theme(axis.text.x = element_text(angle = 30, 
                                   vjust = 1, 
                                   hjust = 1)) +
  scale_fill_gradient2(low = "blue", 
                       high = "red", 
                       mid = "white", 
                       midpoint = 0, 
                       limit = c(-1,1), 
                       space = "Lab", 
                       name="Pearson\nCorrelation") +
  ggsave(file = outGraphName1, 
       width = 300, height = 180, units = "mm", dpi = 300)



# WHAT PERCENTAGE OF ALL DEATHS HAPPENED IN EVENTS OF "PERFECT LETHALITY"?

print(paste0(
    round((sum(AllData$organized_crime_dead[AllData$perfect_lethality == 1])/
               sum(AllData$organized_crime_dead))*100, digits = 2), 
    " percent of organized crime deaths happened in events of perfect lethality" 
))

# building a nice graph to show this

ggplot(data = AllData) +
    geom_bar(aes(x=factor(perfect_lethality), 
                 y = (..count..)/sum(..count..)), 
             fill = "orangered2") + 
    scale_y_continuous("", labels = scales::percent) +
    scale_x_discrete("", labels = element_blank()) +
    annotate("text", x = factor(0), y = .60, 
             label = "dead > 0 \n wounded = 0", fontface = 2, 
             color = "white", size = 8) +
    annotate("text", x = factor(1), y = .18, 
             label = "dead > 0 \n wounded > 0", fontface = 2, 
             color = "white", size = 8) +
    theme_minimal() +
    theme(axis.text.y = element_text(size=16)) +
    ggsave(file = outGraphName2, 
       width = 300, height = 180, units = "mm", dpi = 300)

# what are we really talking about in terms of data

# first, to sort the graph we need to make "category"
# a factor variable with the ordering of levels as required
AllData$category <- factor(AllData$category, 
                           levels = c("no_dead_wounded",
                                      "perfect_lethality",
                                      "dead_wounded",
                                      "just_wounded")) 

# since we modified the base data, we need to reset the data
# in ggplot for this graph

ggplot(data = AllData) +
    geom_bar(aes(x = category, 
                 y = (..count..)/sum(..count..)), 
             fill = "brown4") +
    scale_y_continuous("", labels = scales::percent) +
    scale_x_discrete("", 
                     labels = c(
        "no_dead_wounded" = "neither dead \n nor wounded", 
        "perfect_lethality" = "perfect \n lethality",
        "dead_wounded" = "both dead \n and wounded", 
        "just_wounded" = "just wounded")
        ) +
    annotate("text", x = "no_dead_wounded", y = .50, 
             label = "dead = 0 \n wounded = 0", fontface = 2, 
             color = "white", size = 8) +
    annotate("text", x = "perfect_lethality", y = .20, 
             label = "dead > 0 \n wounded = 0", fontface = 2, 
             color = "white", size = 8) +
    annotate("text", x = "dead_wounded", y = .04, 
             label = "dead > 0 \n wounded > 0", fontface = 2, 
             color = "white", size = 8) +
    annotate("text", x = "just_wounded", y = .04, 
             label = "dead = 0 \n wounded > 0", fontface = 2, 
             color = "white", size = 8) +
    theme_minimal() +
    theme(axis.text.y = element_text(size=16),
          axis.text.x = element_text(size=12)) +
    ggsave(file = outGraphName3, 
       width = 300, height = 180, units = "mm", dpi = 300)




# not terribly informative, what of we compute the difference and plot it
ggplot(data = AllData,
       aes(date, organized_crime_lethality_diff)) + 
    geom_point(alpha = 1/2, size = 3) +
    theme_minimal() +
    geom_hline(yintercept = 0, color = "gray", linetype = 2) +
    annotate("text", 
             x = as.Date("2007-06-20"), 
             y =  28, 
             size = 6,
             label = "casualties > wounded", 
             fontface = 2, 
             color = "gray52") +
    annotate("text", 
             x = as.Date("2007-06-20"), 
             y = -19, 
             size = 6,
             label = "wounded > casualties", 
             fontface = 2, 
             color = "gray52") +
    scale_x_date("", 
                 date_breaks = "1 year", 
                 date_labels = "%Y")  +
    scale_y_continuous("") +
    theme(axis.text.y = element_text(size=16),
          axis.text.x = element_text(size=12)) +
    ggsave(file = outGraphName4, 
       width = 300, height = 180, units = "mm", dpi = 300)

