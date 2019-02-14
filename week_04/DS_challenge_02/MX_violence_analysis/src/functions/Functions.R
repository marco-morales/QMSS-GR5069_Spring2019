
# :::::::::::::::::::::: DEFINE SOME FUNCTIONS ::::::::::::::::::::::::::::::::

# note that the functions are over-commented for you to be able to follow
# each step precisely


munge_data <- function(baseEventData, StateNames, ForcesTable, SourceString){

    # ::::::::: DESCRIPTION
    #
    # The function performs the following transformations in the data to
    # produce the desired output data:
    #
    # 1. add actual names of states and municipalities from a Census table;
    #    currently the database only has their numeric codes
    # 2. rename columns from Spanish to English (not everyone speaks both languages)
    # 3. convert UNIX timestamp variable to a time object; this will be useful to
    #    seamlessly create a date variable, and extract month names for graphing
    # 4. some additional string changes in state abbreviations that will be useful
    #    when graphing
    # 5. adding a new variable that indicates the armed force involved in the
    #    confrontation event
    # 6. replace all missing values with 0; this will come in handy as we start to
    #    explore the data further
    #
    # ::::: INPUTS
    #
    # i)   BaseEventData - the raw database to be munged
    # ii)  StatesName - a table with State/Municipality names
    # iii) ForcesTable - a table that identifies which armed forces were involved in the event
    # iv)  SourceString  - a string that will identify origin of the table
    #
    #:::::: OUTPUT
    #
    # the function returns a dataframe

    fullData <- baseEventData %>%
        # adding State and Municipality names to dataframe
        left_join(., StateNames,
                  by = c("ESTADO" = "CVE_ENT",
                         "Municipio" = "CVE_MUN")
        ) %>%
        # renaming variables to intelligible English
        rename(day_orig = DIA,
               month_orig = MES,
               year_orig = AÑO,
               state_code = ESTADO,
               mun_code = Municipio,
               state = NOM_ENT,
               state_abbr = NOM_ABR,
               municipality = NOM_MUN,
               event_id = ID,
               unix_timestamp = TIMESTAMP,
               detained = DE,
               total_people_dead = PF,
               military_dead = MIF,
               navy_dead = MAF,
               federal_police_dead = PFF,
               afi_dead = AFIF,
               state_police_dead = PEF,
               ministerial_police_dead = PMF,
               municipal_police_dead = PMUF,
               public_prosecutor_dead = AMPF,
               organized_crime_dead = DOF,
               civilian_dead = CIF,
               total_people_wounded = PL,
               military_wounded = MIL,
               navy_wounded = MAL,
               federal_police_wounded = PFL,
               afi_wounded = AFIFL,
               state_police_wounded = PEL,
               ministerial_police_wounded = PML,
               municipal_police_wounded = PMUL,
               public_prosecutor_wounded = AMPL,
               organized_crime_wounded = DOL,
               civilian_wounded = CIL,
               long_guns_seized = ARL,
               small_arms_seized = ARC,
               cartridge_sezied = CART,
               clips_seized = CARG,
               vehicles_seized = VE
        ) %>%
        # adding new variables to the dataframe that:
        # 1) create a date by converting unix timestamp, other time-related information
        # thet can later be extracted from this variable
        # 2) modify state abbreviations by capitalizing and droping period
        # to "beautify" graph labels later on
        # 3) adding a column that indicates the source of the data
        # to be used later for analysis
        mutate(date = as.Date(as.POSIXct(unix_timestamp, origin="1970-01-01")),
               state_abbr = str_to_upper(str_replace_all(state_abbr, "[[:punct:]]", "")),
               source = SourceString
        ) %>%
        # adding a variable that indicates which armed force was involved in the
        # confrontation event, to use later in analysis
        inner_join(., ForcesTable, by = c("event_id" = "ID")
        ) %>%
        # keeping only necessary variables
        select(event_id, unix_timestamp, date,
               state_code, state, state_abbr, mun_code, municipality,
               detained, total_people_dead, military_dead, navy_dead,
               federal_police_dead, afi_dead, state_police_dead, ministerial_police_dead,
               municipal_police_dead, public_prosecutor_dead, organized_crime_dead,
               civilian_dead, total_people_wounded, military_wounded, navy_wounded,
               federal_police_wounded, afi_wounded, state_police_wounded,
               ministerial_police_wounded, municipal_police_wounded,
               public_prosecutor_wounded, organized_crime_wounded, civilian_wounded,
               long_guns_seized, small_arms_seized, cartridge_sezied, clips_seized,
               vehicles_seized,
               afi, army, federal_police, ministerial_police, municipal_police,
               navy, other, state_police, source
        ) %>%
        # filling in NAs with zeros, to facilitate graphing and basic computations
        # replace_na() requires a list of columns and rules to apply. Code below
        # provides that
        replace_na(
            setNames(            # creates an object with numeric column names
                lapply(          # applies a function that links numeric column names
                                 # with the assignment of 0
                    vector("list", length(select_if(., is.numeric))), # creates a list length 25
                    function(x) x <- 0),  # defines assignment of 0 to numeric col names
                names(select_if(., is.numeric)))  # provides numeric column names
        )
    return(fullData)
    }




wrangle_table <- function(ForcesTable, ForcesNameLookup){
    # ::::::::: DESCRIPTION
    #
    # The function munges table that contains the armed forces participating
    # on each event, and needs to be merged with their actual names. Since
    # certain events have more that one armed force participating, further
    # wrangling needs to happen to produce a single row per event and not
    # losing information, hence that information is placed in additional
    # columns.
    #
    # 1. add actual names of armed forces from a manually input table gathered from
    #    data documentation
    # 2. converts a variable containing the names of armed forces into columns
    # 3. remove duplicate records where more than one armed force participated in
    #    the event
    # 4. keep only needed variables
    #
    # ::::: INPUTS
    #
    # i)   ForcesTable - the raw additional table to be transformed
    # ii)  ForcesNameLookup - a lookup table with corresponding names for codes
    #
    #:::::: OUTPUT
    #
    # the function returns a dataframe

    parsedTable <- ForcesTable %>%
        # appends the names from the lookup table
        left_join(., ForcesNameLookup, by = c("KEY"= "authority_id")) %>%
        # creates a count variable that will populate corresponding columns
        mutate(., count = 1) %>%
        # fills in with "unknown" all cases where no code is present for
        # armed forces. Needed to create the corresponding column
        replace_na(., replace = list(authority = "unknown")) %>%
        # converts all values in the authority variable to columns
        spread(., key = authority, value = count) %>%
        # fills in all missing values in these new columns with zeroes
        # needed for the group_by
        replace_na(
            setNames(            # creates an object with numeric column names
                lapply(          # applies a function that links numeric column names
                                 # with the assignment of 0
                    vector("list", length(select_if(., is.numeric))), # creates a list length 25
                    function(x) x <- 0),  # defines assignment of 0 to numeric col names
                names(select_if(., is.numeric)))  # provides numeric column names
        ) %>%
        # groups by event ID and collapses duplicate values to have one row
        # per event
        group_by(., ID) %>%
        summarise_each(funs(max)) %>%
        # selects just the necessary variables
        select(ID, afi, army, federal_police, ministerial_police,
               municipal_police, navy, other, state_police)
    return(parsedTable)
}
