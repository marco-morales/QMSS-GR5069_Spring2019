#  #######################################################################
#       File-Name:      Gather_ConfrontationsData_171220.R
#       Version:        R 3.4.3
#       Date:           Dec 20, 2017
#       Author:         MM
#       Purpose:        Extract and transform raw database made public by
#                       the Program on Drug Policies (PDD) that compiles
#                       violent confrontations and aggression between armed
#                       forces and organized crime in MX between 2007-2011.
#                       ADDS agressions data and consolidates into a single
#                       database.
#                       ADDS table specifying federal force involved in
#                       Confrontation event
#       Input Files:    A-E.xlsx    (raw data on confrontations)
#                       A-A.xlsx    (raw data on aggressions)
#                       ARCH535.csv (name equivalence tables)
#                       tabla9-A-E.xlsx (federal forces involved in confrontations)
#                       tabla9-A-A.xlsx (federal forces involved in aggressions)
#                       LookupAuthorityNames.csv (conversion tables for federal forces codes)
#       Output Files:   NONE
#       Data Output:    AllViolenceData_170216.csv
#       Previous files: NONE
#       Dependencies:   LibrayInstaller.R (installs all needed packages)
#                       DefineFunctions.R (defines functions needed in the script)
#       Required by:    NONE
#       Status:         COMPLETED
#       Machine:        Mac laptop
#  #######################################################################


# SET THE ROOT OF THE PROJECT AS YOUT WORKING DIRECTORY, THAT WILL ENABLE
# THE REST OF THE PROJECT TO RUN

source("src/data/LibraryInstaller.R")  # installs all needed packages
source("src/functions/Functions.R")   # loads custom-made functions  

library(readr)          # easier reading of flat files
library(readxl)         # easier reading of excel files
library(dplyr)          # data manipulation functions
library(tidyr)          # tools for tidy datasets
library(magrittr)       # ceci n'est pas un pipe
library(lubridate)      # easier manipulation of time objects
library(stringr)        # easier manipulation of strings
library(reshape2)       # a much more flexible reshaping for our purpose here
library(here)           # easy path location and setting

# or, you could just load the tidyverse
# library(tidyverse)

# ::::::: SOME USEFUL DEFINITIONS :::::::::::::::::::::::::::::::::::::::::::::

# define additional paths for files you will use. In each case, determine
# appropriate additions to the path

inFileName1  <- here::here("data", "raw", "A-E.xlsx")                       # raw data on confrontations
inFileName2  <- here::here("data", "external", "ARCH535.csv")               # name equivalence tables
inFileName3  <- here::here("data", "raw", "tabla9-A-E.xlsx")                # id of federal forces involved in confrontation event
inFileName4  <- here::here("data", "external", "LookupAuthorityNames.csv")  # name of federal forces involved
inFileName5  <- here::here("data", "raw", "A-A.xlsx")                       # raw data on agressions
inFileName6  <- here::here("data", "raw", "tabla9-A-A.xlsx")                # id of federal forces involved in agression event

outFileName1 <- here::here("data", "processed" , "AllViolenceData_171220.csv") # output file name


# ::::::: APPLY INITIAL DEFINITIONS :::::::::::::::::::::::::::::::::::::::::::


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::: LOADS DATA :::::::::::::::::::::::::::::::::::::::::::


# ::::::: LOADING RAW DATA FOR CONFRONTATIONS
# the original file uses 9999 as a sentinel value for missing values changing
# back to null upon loading

Confrontations <- read_excel(inFileName1,
                             col_types = "numeric", # forcing all columns to numeric
                             sheet = 1,
                             na = "9999"   # converting sentinel value to null
)

# rough validations that data was correctly loaded
names(Confrontations)
nrow(Confrontations)
summary(Confrontations)


# :::::: LOADING NAME CONVERSION TABLE
# the original file treats numeric codes as strings, must convert to integers
# upon loading. Also, names of municipalities are in Spanish, so must specify
# the encoding as the file is read

NameTable <- read_csv(inFileName2,
                      col_types = cols(
                          CVE_ENT = col_integer(),    # must convert to integer
                          NOM_ENT = col_character(),
                          NOM_ABR = col_character(),
                          CVE_MUN = col_integer(),    # must convert to integer
                          NOM_MUN = col_character()
                          ),
                      locale = locale(encoding = "ISO-8859-1") # to read accents properly
                      )

# rough validations that data was correctly loaded
names(NameTable)
nrow(NameTable)
summary(NameTable)


# :::::: LOADING TABLE OF FEDERAL FORCES INVOLVED IN CONFRONTATIONS
# the original file uses 9999 as a sentinel value for missing values changing
# back to null upon loading

ForcesTable_Confrontations <- read_excel(inFileName3,
                             sheet = 1,
                             na = "9999"   # converting sentinel value to null
)

# rough validations that data was correctly loaded
names(ForcesTable_Confrontations)
nrow(ForcesTable_Confrontations)
summary(ForcesTable_Confrontations)

# :::::: LOADING NAME CONVERSION TABLE FOR FEDERAL FORCES NAMES
# the original file treats numeric codes as strings, must convert to integers
# upon loading. Also, names of municipalities are in Spanish, so must specify
# the encoding as the file is read

ForcesNameLookup <- read_csv(inFileName4)

# rough validations that data was correctly loaded
names(ForcesNameLookup)
nrow(ForcesNameLookup)
summary(ForcesNameLookup)


# ::::::: LOADING RAW DATA FOR AGGRESSIONS
# the original file uses 9999 as a sentinel value for missing values changing
# back to null upon loading

Aggressions <- read_excel(inFileName5,
                             col_types = "numeric", # forcing all columns to numeric
                             sheet = 1,
                             na = "9999"   # converting sentinel value to null
)

# rough validations that data was correctly loaded
names(Aggressions)
nrow(Aggressions)
summary(Aggressions)


# :::::: LOADING TABLE OF FEDERAL FORCES INVOLVED IN AGGRESSIONS
# the original file uses 9999 as a sentinel value for missing values changing
# back to null upon loading

ForcesTable_Aggressions <- read_excel(inFileName6,
                                         sheet = 1,
                                         na = "9999"   # converting sentinel value to null
)

# rough validations that data was correctly loaded
names(ForcesTable_Aggressions)
nrow(ForcesTable_Aggressions)
summary(ForcesTable_Aggressions)



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::: SOME DATA PROCESSING ::::::::::::::::::::::::::::::::::

# munges table identifying which armed forces participated on each event
Forces_Confrontations <- wrangle_table(ForcesTable_Confrontations, ForcesNameLookup)

Forces_Aggressions    <- wrangle_table(ForcesTable_Aggressions, ForcesNameLookup)


# processing all confrontations data
Confrontations_processed <- munge_data(Confrontations, NameTable,
                                       Forces_Confrontations, "confrontations")

# processing all aggressions data
Aggressions_processed <- munge_data(Aggressions, NameTable,
                                       Forces_Aggressions, "aggressions")

# binds all dataframes into a single one
AllData <- Confrontations_processed %>%
    bind_rows(., Aggressions_processed)

# creates new features
AllData %<>%
    mutate(.,
           # computes lethality indices as defined by Chevigny: dead/wounded
           organized_crime_lethality = organized_crime_dead/organized_crime_wounded,
           army_lethality = military_dead / military_wounded,
           navy_lethality = navy_dead / navy_wounded,
           federal_police_lethality = federal_police_dead / federal_police_wounded,

           # computes an alternate measure of the difference between dead and wounded
           organized_crime_lethality_diff = organized_crime_dead-organized_crime_wounded,
           army_lethality_diff = military_dead - military_wounded,
           navy_lethality_diff = navy_dead - navy_wounded,
           federal_police_lethality_diff = federal_police_dead - federal_police_wounded,

           # computes new index
           organized_crime_NewIndex =
               (organized_crime_dead-organized_crime_wounded) /
               (organized_crime_dead+organized_crime_wounded),
           army_NewIndex = (military_dead - military_wounded)/
               (military_dead + military_wounded),
           navy_NewIndex = (navy_dead - navy_wounded)/(navy_dead + navy_wounded),
           federal_police_NewIndex = (federal_police_dead - federal_police_wounded) /
               (federal_police_dead + federal_police_wounded),

           # adds perfect lethality indicator
           perfect_lethality = ifelse(is.infinite(organized_crime_lethality) , 1,0),
           # adss category
           category = ifelse(is.infinite(organized_crime_lethality), "perfect_lethality",
                             ifelse(is.nan(organized_crime_lethality), "no_dead_wounded",
                                    ifelse(organized_crime_lethality == 0, "just_wounded",
                                           "dead_wounded"))),

           # global id indicator, since ID is only unique by data source
           global_id = 1:nrow(AllData)
    )



# :::::::::::::::::::::: SAVING PROCESSED FILE ::::::::::::::::::::::::::::::::

write_csv(AllData, outFileName1)
