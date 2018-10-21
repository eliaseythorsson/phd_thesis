# Analysis for paper I

library(tidyverse)
library(lubridate)
library(zoo)
library(epiR)

############################
#### Importing the data ####
############################

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
        DIA_1 = DIA_1,
        DIA_2 = DIA_2, 
        DIA_3 = DIA_3,
        DIA_4 = DIA_4,
        DIA_5 = DIA_5,
        DIA_6 = DIA_6,
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
        DIA_1 = DIA_1,
        DIA_2 = DIA_2, 
        DIA_3 = DIA_3,
        DIA_4 = DIA_4,
        DIA_5 = DIA_5,
        DIA_6 = DIA_6,
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

# read dataframe of all hospital admissions with study ICD-10 diagnoses

# lsh_legur <- read_delim(
#     file = "_data/paper_I/lsh_legur_0515.txt", 
#     delim = ";",
#     escape_double = FALSE,
#     col_types = cols(
#         Aldur = col_integer(), `Allar greiningar` = col_character(), 
#          Dia_1 = col_character(), Dia_10 = col_character(), 
#          Dia_2 = col_character(), Dia_3 = col_character(), 
#          Dia_4 = col_character(), Dia_5 = col_character(), 
#          Dia_6 = col_character(), Dia_7 = col_character(), 
#          Dia_8 = col_character(), Dia_9 = col_character(), 
#          Dánardagur = col_character(), `Inn á LSH` = col_datetime(format = "%d.%m.%Y %H:%M"), 
#          KENNITALA = col_character(), Kyn = col_character(), 
#          Nafn = col_character(), SERGREIN = col_character(), 
#          `Út af LSH` = col_datetime(format = "%d.%m.%Y %H:%M")), 
#     locale = locale(decimal_mark = ","), 
#     trim_ws = TRUE)
# 
# lsh_legur <- lsh_legur %>%
#     select(-Nafn) %>%
#     transmutate(
#         id = KENNITALA,
#         gender = Kyn,
#         age_y = Aldur, 
#         specialty = SERGREIN,
#         diagnosis = `Allar greiningar`, 
#         date_in = round_date(`Inn á LSH`, unit = "day"),
#         date_out = round_date(`Út af LSH`, unit = "day"),
#         death = as.Date(Dánardagur, format = "%d.%m.%Y")
#     )

# All visits to the paediatric emergency department regardless of diagnosis
bmb_komur <- read_csv2(file = "_data/paper_I/bmb_komur.csv")

bmb_komur <- bmb_komur %>%
    filter(X1 != "ALLS") %>%
    select(X1:`2016`) %>%
    gather(key = year, value = n, -X1) %>%
    mutate(month = rep(month.abb, 2016-2002 +1)) %>%
    select(-X1) %>%
    mutate(ym = as.yearmon(paste(.$month, .$year, sep = "/"), format = "%b/%Y"), year = as.integer(year))

# Number of children within a 100km radius of Children's Hospital Iceland
born_100km <- read_delim("_data/paper_I/born_100km.csv", ";", escape_double = FALSE, trim_ws = TRUE)
born_100km <- born_100km %>% gather(key = year, value = n, `2008`:`2015`)
colnames(born_100km) <- c("area", "age_y", "year", "n")

born_0811 <- read_csv2("_data/paper_I/born_100_km_0811.csv")
born_0811 <- born_0811 %>% gather(key = year, value = n, `2008`:`2011`)
colnames(born_0811) <- c("area", "age_y", "year", "n")
born_0811 <- born_0811 %>% filter(area %in% born_100km$area)

born_1216 <- read_csv2("_data/paper_I/born_100_km_1216.csv")
born_1216 <- born_1216 %>% gather(key = year, value = n, `2012`:`2016`)
colnames(born_1216) <- c("area", "age_y", "year", "n")
born_1216 <- born_1216 %>% filter(area %in% born_100km$area)

born_100km <- born_0811 %>% full_join(born_1216) %>% mutate(year = as.integer(year))
rm(born_0811, born_1216)

#######################
#### Data analysis ####
#######################

# The effect of 