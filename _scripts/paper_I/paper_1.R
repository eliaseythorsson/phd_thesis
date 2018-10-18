# Analysis for paper I

library(tidyverse)
library(lubridate)

# read dataframe of all visits with study ICD-10 diagnoses
diagnosis <- read_delim(
    file = "_data/paper_I/allar_greiningar_ceftriaxone.txt",
    delim = ";",
    escape_double = FALSE,
    col_types = cols(
        Aldur = col_double(),
        `Allar greiningar` = col_character(),
        Deild = col_character(),
        Kennitala = col_character(), 
        Komudagur = col_datetime(format = "%d.%m.%Y %H:%M"), 
        Nafn = col_character(),
        `Tegund komu` = col_character(), 
        X10 = col_skip(), X11 = col_skip(), X12 = col_skip(),
        X13 = col_skip(), X14 = col_skip(), X15 = col_skip(),
        X16 = col_skip(), X8 = col_skip(), X9 = col_skip()),
    locale = locale(decimal_mark = ","), 
    trim_ws = TRUE)

diagnosis <- diagnosis %>%
    transmute(
        id = Kennitala,
        obs = paste(row.names(diagnosis), "diagnosis", sep = "_"),
        age_y = Aldur, 
        date = round_date(Komudagur, unit = "day"), 
        diagnosis = `Allar greiningar`,
        departm = Deild, 
        visit_type = `Tegund komu`
    )

# read dataframe of all visits were ceftriaxone was given, along with ICD-10 diagnosis
ceftriaxone <- read_delim(
    file = "_data/paper_I/lyfjagjafir_ceftriaxone.txt", 
    delim = ";",
    escape_double = FALSE,
    col_types = cols(
        Aldur = col_integer(), 
        DIA_1 = col_character(), DIA_2 = col_character(), 
        DIA_3 = col_character(), DIA_4 = col_character(), 
        DIA_5 = col_character(), DIA_6 = col_character(), 
        Deild = col_character(), `Heiti lotu` = col_character(), 
        Kennitala = col_character(), 
        Komudagur = col_datetime(format = "%d.%m.%Y %H:%M:%S"), 
        `LYFJAGJÖF Í VÖ_VA (IM)` = col_character(), 
        `LYFJAGJÖF Í Æ_ (IV)` = col_character(), 
        Nafn = col_character(),
        X15 = col_skip()),
    locale = locale(decimal_mark = ","), 
    trim_ws = TRUE)

ceftriaxone <- ceftriaxone %>%
    transmute(
        id = Kennitala,
        obs = paste(row.names(ceftriaxone), "ceftriaxone", sep = "_"),
        age_y = Aldur, 
        date = round_date(Komudagur, unit = "day"), 
        departm = Deild, 
        visit_type = `Heiti lotu`,
        im = `LYFJAGJÖF Í VÖ_VA (IM)`,
        iv = `LYFJAGJÖF Í Æ_ (IV)`
    )

### Create variable which counts number of days since last ceftriaxone injection ###
### I would not do it this way again if I were repeating this analysis now ###

ceftriaxone <-
    ceftriaxone %>%
    arrange(date) %>%
    group_by(id) %>%
    mutate(days_since_last =  difftime(date, lag(date), unit = "days")) %>%
    ungroup()

# read dataframe of all visits with ICD-10 diagnoses compatible with otitis media: H65 and H66
komur <- read_delim(
    file = "_data/paper_I/h65_h66_komur.txt",
    delim = ";",
    escape_double = FALSE,
    col_types = cols(
        ALDUR = col_integer(), 
        DIA_1 = col_character(), DIA_2 = col_character(), 
        DIA_3 = col_character(), DIA_4 = col_character(),
        DIA_5 = col_character(), DIA_6 = col_character(), 
        Deild = col_character(),
        KOMUDAGUR = col_datetime(format = "%d.%m.%Y %H:%M:%S"), 
        PERSON_ID = col_character()), 
    locale = locale(decimal_mark = ","), 
    trim_ws = TRUE)

komur <- komur %>%
    transmute(
        id = PERSON_ID, 
        obs = paste(row.names(komur), "komur", sep = "_"),
        age_y = ALDUR, 
        date = round_date(KOMUDAGUR, unit = "day"),
        departm = Deild
    )

### Create variable which counts number of days since last visit for otitis media ###
### I would not do it this way again if I were repeating this analysis now ###

komur <-
    komur %>%
    arrange(date) %>%
    group_by(id) %>%
    mutate(days_since_last =  difftime(date, lag(date), unit = "days")) %>%
    ungroup()