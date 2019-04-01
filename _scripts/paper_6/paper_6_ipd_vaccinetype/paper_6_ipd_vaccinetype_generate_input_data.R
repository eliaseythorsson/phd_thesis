# Creates the base dataframe for the invasive pneumococcal disease cost-effectiveness time-series analysis for paper6

library(tidyverse)
library(readxl)
library(zoo)


############################
#### Importing the data ####
############################

iceland <- read_excel(
    "_data/paper_6/paper_6_ipd_vaccinetype/synthetic-controls-ipd.xls", 
    sheet = "Gögnin",
    col_types = c("text", "text", "text", "text", "numeric")
)
colnames(iceland) <- c("year", "age_group", "month", "diagnosis", "number")

ids <- read_rds("_data/results/ids.rds")
lsh <- read_rds("_data/results/lsh.rds")
sykl <- read_excel("_data/paper_6/paper_6_ipd_vaccinetype/Ífarandi pneumo_1995_2016_Elias.xls")


########################
#### Data wrangling ####
########################

### Clean microbiology database ###
sykl <- 
    sykl %>%
    select(
        date = dagsetn.,
        kt = kennitala, 
        death = látinn,
        death_date = dánardagur,
        blood = blóð,
        spine = mænuv.,
        joint = liðv.,
        serot = hjúpg.,
        vac_t = `VT (Synflorix)`
    )

sykl <- 
    sykl %>%
    mutate(
        kt = str_replace_all(kt, "-", ""),
        death = if_else(is.na(death), 0, 1),
        blood = if_else(blood == "Nei"|is.na(blood), 0, 1),
        spine = if_else(spine == "Nei"|is.na(spine), 0, 1),
        joint = if_else(joint == "Nei"|is.na(joint), 0, 1),
        serot = str_replace_all(serot, "\\?| |útl|\\.|", ""),
        vac_t = as.numeric(case_when(
            vac_t == "NA" ~ NA,
            vac_t == "No" ~ FALSE,
            vac_t == "Y" ~ TRUE,
            is.na(vac_t) ~ NA
        ))
    )

sykl <- sykl %>% mutate(kt = str_replace_all(kt, "\n", ""))

sykl <-
    sykl %>% 
    mutate(
        serot = case_when(
            str_detect(serot, pattern = "NA|óþekkt|óhjúpgr") ~ as.character(NA), 
            TRUE ~ as.character(serot)),
        birth_d = str_sub(.$kt, 1, 2), 
        birth_m =  str_sub(.$kt, 3, 4),
        birth_y = case_when(
            str_sub(.$kt, 10, 10) == 0 ~ paste0(20, str_sub(.$kt, 5, 6)),
            str_sub(.$kt, 10, 10) == 9 ~ paste0(19, str_sub(.$kt, 5, 6)),
            str_sub(.$kt, 10, 10) == 8 ~ paste0(18, str_sub(.$kt, 5, 6))
        )
    )  %>%
    mutate(birth_date = as.Date(paste(.$birth_y, .$birth_m, .$birth_d, sep = "-"))) %>%
    mutate(age_y = floor(as.numeric(difftime(date, birth_date, units = "days"))/365.25))

sykl <- sykl %>%
    filter(date >= as.Date("2005-01-01"), date <= as.Date("2017-12-31"), vac_t == 1) %>%
    mutate(
        date = as.character(as.Date(as.yearqtr(date))),
        age_group = case_when(
            age_y >= 0 & age_y <= 4 ~ "0-4y",
            age_y >= 5 & age_y <= 64 ~ "5-64y",
            age_y >= 65 ~ "65y+",
            TRUE ~ "remove")
    )
    
sykl <-
    sykl %>%
    count(age_group, date) %>%
    right_join(expand(sykl, age_group, date)) %>%
    mutate(n = if_else(is.na(n), 0L, n)) %>%
    filter(age_group != "remove")

lsh <- 
    lsh %>%
    filter(
        str_detect(string = icd10_code, pattern = "J20|J21|J22"),
        lotu_teg == "Legulota" | dur_stay_hours >= 24,
        year >= 2005, year <= 2017) %>%
    left_join(ids) %>%
    mutate(
        date = as.character(as.Date(as.yearqtr(date_in))),
        age_y = floor(difftime(date_in, birth_date, unit = "days")/365.25),
        age_group = case_when(
            age_y >= 0 & age_y <= 4 ~ "0-4y",
            age_y >= 5 & age_y <= 64 ~ "5-64y",
            age_y >= 65 ~ "65y+",
            TRUE ~ "remove")
    )

lsh <- 
    lsh %>%
    count(age_group, date) %>%
    right_join(expand(lsh, age_group, date)) %>%
    mutate(n = if_else(is.na(n), 0L, n)) %>%
    filter(age_group != "remove")

iceland <-
    iceland %>%
    expand(year, age_group, month, diagnosis) %>%
    left_join(iceland) %>%
    mutate(date = as.character(
        as.Date(
            as.yearqtr(
                as.Date(paste(year, month, "01", sep = "-"))
                )
            )
        )
    ) %>%
    filter(as.Date(date) < as.Date("2017-01-01")) %>%
    mutate(number = if_else(is.na(number), 0, number)) %>%
    select(-year, -month) %>%
    mutate(age_group = case_when(
        age_group %in% c("0", "1", "2", "3", "4") ~ "0-4y",
        age_group %in% c("b. 5 - 9 ára", "c. 10 - 14 ára", "d. 15 - 19 ára", "e. 20 - 24 ára",
                         "f. 25 - 29 ára", "g. 30 - 34 ára", "h. 35 - 39 ára", "i. 40 - 44 ára",
                         "j. 45 - 49 ára", "k. 50 - 54 ára", "l. 55 - 59 ára", "m. 60 - 64 ára") ~ "5-64y",
        age_group %in% c("n. 65 - 69 ára", "o. 70 - 74 ára", "p. 75 - 79 ára", "q. 80 - 84 ára", "85 ára og eldri") ~"65y+"
    )) %>%
    group_by(age_group, diagnosis, date) %>%
    summarise(number = sum(number)) %>%
    ungroup() %>%
    filter(!is.na(diagnosis)) %>%
    spread(key = diagnosis, value = number) %>%
    mutate(ach_noj = rowSums(select_if(., is.numeric)))



iceland <-
    iceland %>%
    left_join(sykl %>% rename(IPD = n)) %>%
    mutate(IPD = if_else(is.na(IPD), 0L, IPD)) %>%
    left_join(lsh %>% rename(J20_22 = n)) %>%
    select("age_group", "date", "IPD", "ach_noj", "A10-B99", "C00-D48", "D50-89", "E00-99", 
           "G00-G99", "H00-99", "I00-99", "J20_22", "K00-99", "L00-99", "M00-99", 
           "N00-99", "P00-99", "Q00-99", "R00-99", "S00-T99", "U00-99", 
           "V00-Y99", "Z00-99")

iceland <- data.frame(iceland)	
write_csv(path = "_data/paper_6/paper_6_ipd_vaccinetype/input-data.csv", x = iceland)
