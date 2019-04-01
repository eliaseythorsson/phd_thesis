# Creates the base dataframe for the pneumonia cost-effectiveness time-series analysis for paper6

library(tidyverse)
library(readxl)
library(zoo)


############################
#### Importing the data ####
############################

iceland <- read_excel(
    "_data/paper_6/paper_6_pneumonia_J13_J18_diagnostics/synthetic-controls-pneumonia.xls", 
    sheet = "Gögnin",
    col_types = c("text", "text", "text", "text", "numeric")
    )
colnames(iceland) <- c("year", "age_group", "month", "diagnosis", "number")
ids <- read_rds("_data/results/ids.rds")
lsh <- read_rds("_data/results/lsh.rds")

########################
#### Data wrangling ####
########################

iceland <-
    iceland %>%
    expand(year, age_group, month, diagnosis) %>%
    left_join(iceland) %>%
    mutate(date = paste(year, month, "01", sep = "-")) %>%
    mutate(number = if_else(is.na(number), 0, number)) %>%
    select(-year, -month) %>%
    mutate(age_group = case_when(
        age_group %in% c("0", "1", "2", "3", "4") ~ "0-4y",
        age_group %in% c("b. 5 - 9 ára", "c. 10 - 14 ára", "d. 15 - 19 ára") ~ "5-19y",
        age_group %in% c("e. 20 - 24 ára", "f. 25 - 29 ára", "g. 30 - 34 ára", "h. 35 - 39 ára") ~ "20-39y",
        age_group %in% c("i. 40 - 44 ára", "j. 45 - 49 ára", "k. 50 - 54 ára", "l. 55 - 59 ára", "m. 60 - 64 ára") ~ "40-64y",
        age_group %in% c("n. 65 - 69 ára", "o. 70 - 74 ára", "p. 75 - 79 ára") ~"65-79y",
        age_group %in% c("q. 80 - 84 ára", "85 ára og eldri") ~ "80+"
    )) %>%
    group_by(age_group, diagnosis, date) %>%
    summarise(number = sum(number)) %>%
    ungroup() %>%
    filter(!is.na(diagnosis)) %>%
    spread(key = diagnosis, value = number) %>%
    mutate(ach_noj = rowSums(select_if(., is.numeric)))

lsh <- 
    lsh %>%
    filter(
        str_detect(string = icd10_code, pattern = "J13|J15.8|J15.9|J18.1|J18.8|J18.9|J20|J21|J22"),
        lotu_teg == "Legulota" | dur_stay_hours >= 24,
        year >= 2006, year <= 2017) %>%
    left_join(ids) %>%
    mutate(
        date = as.character(as.Date(as.yearmon(date_in))),
        age_y = floor(difftime(date_in, birth_date, unit = "days")/365.25),
        age_group = case_when(
            age_y >= 0 & age_y <= 4 ~ "0-4y",
            age_y >= 5 & age_y <= 19 ~ "5-19y",
            age_y >= 20 & age_y <= 39 ~ "20-39y",
            age_y >= 40 & age_y <= 64 ~ "40-64y",
            age_y >= 65 & age_y <= 79 ~"65-79y",
            age_y >= 80 ~ "80+",
            TRUE ~ "remove"),
        group = case_when(
            str_detect(string = icd10_code, pattern = "J13|J15.8|J15.9|J18.1|J18.8|J18.9") & cost_bacteriology > 0 & cost_radiology > 0 ~ "J13_18",
            str_detect(string = icd10_code, pattern = "J20|J21|J22") ~ "J20_22"
        )) %>%
    filter(group %in% c("J13_18", "J20_22"))

lsh <- 
    lsh %>%
    count(age_group, group, date) %>%
    right_join(expand(lsh, age_group, group, date)) %>%
    mutate(n = if_else(is.na(n), 0L, n)) %>%
    filter(age_group != "remove") %>%
    spread(key = group, value = n)

iceland <-
    iceland %>%
    filter(date >= as.Date("2006-01-01")) %>%
    left_join(lsh) %>%
    select("age_group", "date", "J13_18", "ach_noj", "A10-B99", "C00-D48", "D50-89", "E00-99", 
           "G00-G99", "H00-99", "I00-99", "J20_22", "K00-99", "L00-99", "M00-99", 
           "N00-99", "P00-99", "Q00-99", "R00-99", "S00-T99", "U00-99", 
           "V00-Y99", "Z00-99")

iceland <- data.frame(iceland)	
write_csv(path = "_data/paper_6/paper_6_pneumonia_J13_J18_diagnostics/input-data.csv", x = iceland)
