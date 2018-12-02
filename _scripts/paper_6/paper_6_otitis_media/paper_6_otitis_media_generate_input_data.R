# Creates the base dataframe for the otitis media cost-effectiveness time-series analysis for paper6

library(readxl)
library(zoo)
library(tidyverse)

############################
#### Importing the data ####
############################

iceland <- read_excel(
    path = "_data/paper_6/paper_6_otitis_media/synthetic-controls-otitis-media.xlsx", 
    sheet = 2
)
hg <- read_rds("_data/results/hg.rds")

########################
#### Data wrangling ####
########################

iceland <- iceland %>%
    transmute(
        icd_10 = str_replace(string = ICD_10_KODAR, pattern = "-", replacement = "_"),
        date = as.character(as.Date(
            paste0(MANUDUR, "-01"),
            format = "%Y-%m-%d")),
        age_group = case_when(
            ALDURSFLOKKUR == 0 ~ "0y",
            ALDURSFLOKKUR == 1 ~ "1y",
            ALDURSFLOKKUR == 2 ~ "2y",
            ALDURSFLOKKUR == 3 ~ "3y",
            ALDURSFLOKKUR == 4 ~ "4y",
            ALDURSFLOKKUR %in% 5:9 ~ "5-9y",
            ALDURSFLOKKUR %in% 10:14 ~ "10-14y",
            ALDURSFLOKKUR %in% 15:19 ~ "15-19y",
            TRUE ~ NA_character_),
        n = FJOLDI_EINSTAKLINGA) %>%
    filter(!is.na(age_group), !is.na(date), !is.na(icd_10)) %>%
    group_by(icd_10, date, age_group) %>%
    summarise(n = sum(n, na.rm = T)) %>%
    spread(key = icd_10, value = n, fill = 0) %>%
    ungroup()

hg <- hg %>%
    filter(
        staff_type == "Læ",
        contact_type %in% c("Viðtal","Vitjun"),
        str_detect(string = main_diagnosis, pattern = "H65|H66|H70|H72|H73|J20|J21|J22") |
            str_detect(string = other_diagnosis, pattern = "H65|H66|H70|H72|H73|J20|J21|J22")) %>%
    transmute(
        id = id,
        date = as.character(as.Date(as.yearmon(date_contact))),
        age_group = case_when(
            age_y == 0 ~ "0y",
            age_y == 1 ~ "1y",
            age_y == 2 ~ "2y",
            age_y == 3 ~ "3y",
            age_y == 4 ~ "4y",
            age_y %in% 5:9 ~ "5-9y",
            age_y %in% 10:14 ~ "10-14y",
            age_y %in% 15:19 ~ "15-19y",
            TRUE ~ NA_character_),
        group = case_when(
            str_detect(string = main_diagnosis, pattern = "H65|H66|H70|H72|H73") ~ "H65_H73",
            str_detect(string = other_diagnosis, pattern = "H65|H66|H70|H72|H73") ~ "H65_H73",
            str_detect(string = main_diagnosis, pattern = "J20|J21|J22") ~ "J20_J22",
            str_detect(string = other_diagnosis, pattern = "J20|J21|J22") ~ "J20_J22"
        )) 

hg <- 
    hg %>%
    count(age_group, group, date) %>%
    right_join(hg %>% expand(age_group, group, date)) %>%
    mutate(n = if_else(is.na(n), 0L, n)) %>%
    filter(!is.na(age_group)) %>%
    spread(key = group, value = n)


iceland <-
    iceland %>%
    left_join(hg) %>%
    rename(
        ach_noj = `A00_Z99 ekki J, F og O`,
        A09_K52_K59_R19 = `A09, K52.9, K59.1 og R19.7`,
        A10_B99 = `A10_B99 ekki: B95 og A40.3`,
        H00_H99 = `H00_H99 ekki: H10,H65 og H66`
    ) %>%
    select(age_group, date, H65_H73, ach_noj, everything())

iceland <- data.frame(iceland)	
write_csv(path = "_data/paper_6/paper_6_otitis_media/input-data.csv", x = iceland)
