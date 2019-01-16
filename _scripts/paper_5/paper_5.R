# Analysis for paper 5

library(readxl)
library(tidyverse)
library(lubridate)
library(zoo)
library(epitools)
library(survival)
library(rms)
library(survminer)
library(broom)

# set theme for ggplot2 figures
theme_set(theme_bw(base_size = 10, base_family = "sans"))

#######################################
#### Importing the data; wrangling ####
#######################################

ids <- read_rds("_data/results/ids.rds")
lsh <- read_rds("_data/results/lsh.rds")
hs <- read_rds("_data/results/hs.rds")
sykl <- read_excel("_data/paper_5/Ífarandi pneumo_1995_2016_Elias.xls")

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
    mutate(age_months = as.numeric(difftime(date, birth_date, units = "days"))/365.25*12)

### Clean Landspitali University Hospital patient registry ###
lsh <- 
    lsh %>% 
    filter(date_in <= as.Date("2016-12-31")) %>%
    mutate(
        type_visit = case_when(
            dur_stay_hours >= 24 | lotu_teg == "Legulota" ~ "admitted",
            TRUE ~ "not_admitted"
        ),
        diagnostic_group = case_when(
            str_detect(icd10_code, "G00") ~ "Meningitis",
            str_detect(icd10_code, "A40|A41|A49.1") ~ "Sepsis",
            str_detect(icd10_code, "J09|J10|J11|J12|J13|J14|J15|J16|J17|J18") ~ "Pneumonia",
            str_detect(icd10_code, "H65|H66|H72|H70") ~ "Otitis Media and Complications",
            str_detect(icd10_code, "J00|J01|J02|J03|J04|J05|J06|J36|J39") ~ "Acute upper respiratory infection",
            str_detect(icd10_code, "J20|J21|J22|J40") ~ "Acute Lower Respiratory Tract Infections",
            str_detect(icd10_code, "N30|N39.0") ~ "Urinary tract infection",
            TRUE ~ "Other viral infection"
        ),
        diagnosis = case_when(
            str_detect(icd10_code, "J13") ~ "J13: Pneumonia due to Streptococcus pneumoniae",
            str_detect(icd10_code, "J14") ~ "J14: Pneumonia due to Haemophilus influenzae",
            str_detect(icd10_code, "J16") ~ "J16: Pneumonia due to other infectious organisms",
            str_detect(icd10_code, "J15") ~ "J15: Bacterial pneumonia, not elsewhere classified",
            str_detect(icd10_code, "J18") ~ "J18: Pneumonia, organism unspecified",
            str_detect(icd10_code, "J17") ~ "J17: Pneumonia in diseases classified elsewhere",
            str_detect(icd10_code, "J12") ~ "J12: Viral pneumonia", 
            str_detect(icd10_code, "J11") ~ "J11: Influenza with pneumonia, virus not identified",
            str_detect(icd10_code, "J10") ~ "J10: Influenza with pneumonia, virus identified"
        )
    )

### Baseline demographics for each individual in the study ###
ids_surv <-
    ids %>% 
    filter(birth_y >= 2005, birth_y <= 2015, !is.na(birth_date)) %>% 
    left_join(
        hs %>% 
            select(-move_type) %>% 
            group_by(id) %>%
            arrange(id, date_move) %>%
            filter(date_move == first(date_move))  %>%
            ungroup() %>%
            distinct() 
    ) %>%
    filter(to_from == "move_from_iceland" | is.na(to_from)) %>% 
    mutate(date = as.Date("2016-12-31"), date_move = as.Date(date_move)) %>% 
    mutate(
        date = case_when(
            is.na(death_date) & is.na(date_move) ~ date, 
            is.na(death_date) & !is.na(date_move) & date_move > date ~ date, 
            is.na(death_date) & !is.na(date_move) & date_move <= date ~ date_move, 
            !is.na(death_date) & is.na(date_move) & death_date > date ~ date, 
            !is.na(death_date) & is.na(date_move) & death_date <= date ~ death_date, 
            !is.na(death_date) & !is.na(date_move) & death_date > date & date_move > date ~ date, 
            !is.na(death_date) & !is.na(date_move) & death_date <= date & date_move > date ~ death_date, 
            !is.na(death_date) & !is.na(date_move) & death_date > date & date_move <= date ~ date_move,
            !is.na(death_date) & !is.na(date_move) & death_date <= date & date_move > date ~ death_date,
            !is.na(death_date) & !is.na(date_move) & death_date <= date & date_move <= date & death_date < date_move ~ death_date,
            !is.na(death_date) & !is.na(date_move) & death_date <= date & date_move <= date & death_date >= date_move ~ date_move,
            TRUE ~ date
        )
    ) %>%
    mutate(
        age_no_censor = as.numeric(date - birth_date), 
        age = pmin(365.25*3, as.numeric(date - birth_date))
    ) %>%
    filter(age > 1) %>%
    distinct()

lsh <-
    ids_surv %>%
    select("id", "gender", "birth_y", "birth_date", "death_date") %>%
    left_join(
        lsh
    ) %>%
    mutate(
        age = as.numeric(difftime(date_in, birth_date, unit = "days")),
        age_y = floor(age/365)
    ) %>%
    filter(age_y <= 2)

### Age at n = {1, 2, ... n} hospital admission for meningitis ###
lsh_surv_men <- 
    lsh %>%
    filter(str_detect(icd10_code, "G00"), birth_y >= 2005, birth_y <= 2015, type_visit == "admitted") %>%
    select(id, obs, birth_date, date_in) %>%
    distinct(id, obs, .keep_all = T) %>%
    mutate(age = pmin(3*365.25, pmax(1, as.numeric(as.Date(date_in) - birth_date)))) %>%
    select(-birth_date, -date_in) %>%
    filter(age < 3*365.25) %>%
    group_by(id) %>%
    arrange(id, age) %>%
    mutate(
        obs = cummax(as.numeric(factor(obs, levels = unique(obs))))
    ) %>%
    ungroup() %>%
    spread(key = obs, value = age, sep = "_") %>%
    as.data.frame()

### Create correct time at-risk for meningitis and link to demographics ###
lsh_ag_men <- tmerge(ids_surv, ids_surv, id = id, tstop = ids_surv$age)
lsh_ag_men$tstart <- ifelse(lsh_ag_men$tstart == 0, 1, lsh_ag_men$tstart)
lsh_ag_men$obs_0 <- 0
for(i in 1:(dim(lsh_surv_men)[2] - 1)){
    x <- paste0("obs_", i)
    lsh_ag_men <- tmerge(lsh_ag_men, lsh_surv_men, id = id, visit = event(lsh_surv_men[,x]))
    lsh_ag_men <- tmerge(lsh_ag_men, subset(lsh_ag_men, visit == 1), id = id, obs = cumtdc(tstop))
    lsh_ag_men[, paste0("obs_", i)] <- lsh_ag_men[, "obs"]
    lsh_ag_men[which(lsh_ag_men$obs > 0 & lsh_ag_men$obs > lsh_ag_men[, paste0("obs_", i - 1)]), "tstart"] <-
        lsh_ag_men[which(lsh_ag_men$obs > 0 & lsh_ag_men$obs > lsh_ag_men[, paste0("obs_", i - 1)]), "tstart"] + 30
    lsh_ag_men <- lsh_ag_men[which(lsh_ag_men$tstart < lsh_ag_men$tstop),]
}
lsh_ag_men <- lsh_ag_men[,!grepl("obs_", colnames(lsh_ag_men))]
lsh_ag_men$tstart_m <- lsh_ag_men$tstart/30.45
lsh_ag_men$tstop_m <- lsh_ag_men$tstop/30.45
lsh_ag_men$group <- ifelse(lsh_ag_men$birth_y <= 2010, "Vaccine non-eligible", "Vaccine eligible")
lsh_ag_men$gender <- factor(lsh_ag_men$gender)

### Age at n = {1, 2, ... n} hospital admission for otitis media ###
lsh_surv_aom <- 
    lsh %>%
    filter(str_detect(icd10_code, "H65|H66|H72"), birth_y >= 2005, birth_y <= 2015, type_visit == "admitted") %>%
    select(id, obs, birth_date, date_in) %>%
    distinct(id, obs, .keep_all = T) %>%
    mutate(age = pmin(3*365.25, pmax(1, as.numeric(as.Date(date_in) - birth_date)))) %>%
    select(-birth_date, -date_in) %>%
    filter(age < 3*365.25) %>%
    group_by(id) %>%
    arrange(id, age) %>%
    mutate(
        obs = cummax(as.numeric(factor(obs, levels = unique(obs))))
    ) %>%
    ungroup() %>%
    spread(key = obs, value = age, sep = "_") %>%
    as.data.frame()

### Create correct time at-risk for otitis media and link to demographics ###
lsh_ag_aom <- tmerge(ids_surv, ids_surv, id = id, tstop = ids_surv$age)
lsh_ag_aom$tstart <- ifelse(lsh_ag_aom$tstart == 0, 1, lsh_ag_aom$tstart)
lsh_ag_aom$obs_0 <- 0
for(i in 1:(dim(lsh_surv_aom)[2] - 1)){
    x <- paste0("obs_", i)
    lsh_ag_aom <- tmerge(lsh_ag_aom, lsh_surv_aom, id = id, visit = event(lsh_surv_aom[,x]))
    lsh_ag_aom <- tmerge(lsh_ag_aom, subset(lsh_ag_aom, visit == 1), id = id, obs = cumtdc(tstop))
    lsh_ag_aom[, paste0("obs_", i)] <- lsh_ag_aom[, "obs"]
    lsh_ag_aom[which(lsh_ag_aom$obs > 0 & lsh_ag_aom$obs > lsh_ag_aom[, paste0("obs_", i - 1)]), "tstart"] <-
        lsh_ag_aom[which(lsh_ag_aom$obs > 0 & lsh_ag_aom$obs > lsh_ag_aom[, paste0("obs_", i - 1)]), "tstart"] + 30
    lsh_ag_aom <- lsh_ag_aom[which(lsh_ag_aom$tstart < lsh_ag_aom$tstop),]
}
lsh_ag_aom <- lsh_ag_aom[,!grepl("obs_", colnames(lsh_ag_aom))]
lsh_ag_aom$tstart_m <- lsh_ag_aom$tstart/30.45
lsh_ag_aom$tstop_m <- lsh_ag_aom$tstop/30.45
lsh_ag_aom$group <- ifelse(lsh_ag_aom$birth_y <= 2010, "Vaccine non-eligible", "Vaccine eligible")
lsh_ag_aom$gender <- factor(lsh_ag_aom$gender)

### Age at n = {1, 2, ... n} hospital admission for urti ###
lsh_surv_urti <- 
    lsh %>%
    filter(str_detect(icd10_code, "J00|J01|J02|J03|J04|J05|J06"), birth_y >= 2005, birth_y <= 2015, type_visit == "admitted") %>%
    select(id, obs, birth_date, date_in) %>%
    distinct(id, obs, .keep_all = T) %>%
    mutate(age = pmin(3*365.25, pmax(1, as.numeric(as.Date(date_in) - birth_date)))) %>%
    select(-birth_date, -date_in) %>%
    filter(age < 3*365.25) %>%
    group_by(id) %>%
    arrange(id, age) %>%
    mutate(
        obs = cummax(as.numeric(factor(obs, levels = unique(obs))))
    ) %>%
    ungroup() %>%
    spread(key = obs, value = age, sep = "_") %>%
    as.data.frame() #til þess að forloop með tmerge() til að búa til lg_ag virki

### Create correct time at-risk for urti and link to demographics ###
lsh_ag_urti <- tmerge(ids_surv, ids_surv, id = id, tstop = ids_surv$age)
lsh_ag_urti$tstart <- ifelse(lsh_ag_urti$tstart == 0, 1, lsh_ag_urti$tstart)
lsh_ag_urti$obs_0 <- 0
for(i in 1:(dim(lsh_surv_urti)[2] - 1)){
    x <- paste0("obs_", i)
    lsh_ag_urti <- tmerge(lsh_ag_urti, lsh_surv_urti, id = id, visit = event(lsh_surv_urti[,x]))
    lsh_ag_urti <- tmerge(lsh_ag_urti, subset(lsh_ag_urti, visit == 1), id = id, obs = cumtdc(tstop))
    lsh_ag_urti[, paste0("obs_", i)] <- lsh_ag_urti[, "obs"]
    lsh_ag_urti[which(lsh_ag_urti$obs > 0 & lsh_ag_urti$obs > lsh_ag_urti[, paste0("obs_", i - 1)]), "tstart"] <-
        lsh_ag_urti[which(lsh_ag_urti$obs > 0 & lsh_ag_urti$obs > lsh_ag_urti[, paste0("obs_", i - 1)]), "tstart"] + 30
    lsh_ag_urti <- lsh_ag_urti[which(lsh_ag_urti$tstart < lsh_ag_urti$tstop),]
}
lsh_ag_urti <- lsh_ag_urti[,!grepl("obs_", colnames(lsh_ag_urti))]
lsh_ag_urti$tstart_m <- lsh_ag_urti$tstart/30.45
lsh_ag_urti$tstop_m <- lsh_ag_urti$tstop/30.45
lsh_ag_urti$group <- ifelse(lsh_ag_urti$birth_y <= 2010, "Vaccine non-eligible", "Vaccine eligible")
lsh_ag_urti$gender <- factor(lsh_ag_urti$gender)

### Age at n = {1, 2, ... n} hospital admission for pneumonia ###
lsh_surv_pneum <- 
    lsh %>%
    filter(str_detect(icd10_code, "J10.0|J11.0|J12|J13|J14|J15|J16.0|J16.8|J17|J18"),
           birth_y >= 2005, birth_y <= 2015, type_visit == "admitted") %>%
    select(id, obs, birth_date, date_in) %>%
    distinct(id, obs, .keep_all = T) %>%
    mutate(age = pmin(3*365.25, pmax(1, as.numeric(as.Date(date_in) - birth_date)))) %>%
    select(-birth_date, -date_in) %>%
    filter(age < 3*365.25) %>%
    group_by(id) %>%
    arrange(id, age) %>%
    mutate(
        obs = cummax(as.numeric(factor(obs, levels = unique(obs))))
    ) %>%
    ungroup() %>%
    spread(key = obs, value = age, sep = "_") %>%
    as.data.frame() #til þess að forloop með tmerge() til að búa til lg_ag virki

### Create correct time at-risk for pneumonia and link to demographics ###
lsh_ag_pneum <- tmerge(ids_surv, ids_surv, id = id, tstop = ids_surv$age)
lsh_ag_pneum$tstart <- ifelse(lsh_ag_pneum$tstart == 0, 1, lsh_ag_pneum$tstart)
lsh_ag_pneum$obs_0 <- 0
for(i in 1:(dim(lsh_surv_pneum)[2] - 1)){
    x <- paste0("obs_", i)
    lsh_ag_pneum <- tmerge(lsh_ag_pneum, lsh_surv_pneum, id = id, visit = event(lsh_surv_pneum[,x]))
    lsh_ag_pneum <- tmerge(lsh_ag_pneum, subset(lsh_ag_pneum, visit == 1), id = id, obs = cumtdc(tstop))
    lsh_ag_pneum[, paste0("obs_", i)] <- lsh_ag_pneum[, "obs"]
    lsh_ag_pneum[which(lsh_ag_pneum$obs > 0 & lsh_ag_pneum$obs > lsh_ag_pneum[, paste0("obs_", i - 1)]), "tstart"] <-
        lsh_ag_pneum[which(lsh_ag_pneum$obs > 0 & lsh_ag_pneum$obs > lsh_ag_pneum[, paste0("obs_", i - 1)]), "tstart"] + 30
    lsh_ag_pneum <- lsh_ag_pneum[which(lsh_ag_pneum$tstart < lsh_ag_pneum$tstop),]
}
lsh_ag_pneum <- lsh_ag_pneum[,!grepl("obs_", colnames(lsh_ag_pneum))]
lsh_ag_pneum$tstart_m <- lsh_ag_pneum$tstart/30.45
lsh_ag_pneum$tstop_m <- lsh_ag_pneum$tstop/30.45
lsh_ag_pneum$group <- ifelse(lsh_ag_pneum$birth_y <= 2010, "Vaccine non-eligible", "Vaccine eligible")
lsh_ag_pneum$gender <- factor(lsh_ag_pneum$gender)

### Age at n = {1, 2, ... n} hospital admission for lrti ###
lsh_surv_bronch <- 
    lsh %>%
    filter(str_detect(icd10_code, "J20|J21|J22"), birth_y >= 2005, birth_y <= 2015, type_visit == "admitted") %>%
    select(id, obs, birth_date, date_in) %>%
    distinct(id, obs, .keep_all = T) %>%
    mutate(age = pmin(3*365.25, pmax(1, as.numeric(as.Date(date_in) - birth_date)))) %>%
    select(-birth_date, -date_in) %>%
    filter(age < 3*365.25) %>%
    group_by(id) %>%
    arrange(id, age) %>%
    mutate(
        obs = cummax(as.numeric(factor(obs, levels = unique(obs))))
    ) %>%
    ungroup() %>%
    spread(key = obs, value = age, sep = "_") %>%
    as.data.frame() #til þess að forloop með tmerge() til að búa til lg_ag virki

### Create correct time at-risk for lrti and link to demographics ###
lsh_ag_bronch <- tmerge(ids_surv, ids_surv, id = id, tstop = ids_surv$age)
lsh_ag_bronch$tstart <- ifelse(lsh_ag_bronch$tstart == 0, 1, lsh_ag_bronch$tstart)
lsh_ag_bronch$obs_0 <- 0
for(i in 1:(dim(lsh_surv_bronch)[2] - 1)){
    x <- paste0("obs_", i)
    lsh_ag_bronch <- tmerge(lsh_ag_bronch, lsh_surv_bronch, id = id, visit = event(lsh_surv_bronch[,x]))
    lsh_ag_bronch <- tmerge(lsh_ag_bronch, subset(lsh_ag_bronch, visit == 1), id = id, obs = cumtdc(tstop))
    lsh_ag_bronch[, paste0("obs_", i)] <- lsh_ag_bronch[, "obs"]
    lsh_ag_bronch[which(lsh_ag_bronch$obs > 0 & lsh_ag_bronch$obs > lsh_ag_bronch[, paste0("obs_", i - 1)]), "tstart"] <-
        lsh_ag_bronch[which(lsh_ag_bronch$obs > 0 & lsh_ag_bronch$obs > lsh_ag_bronch[, paste0("obs_", i - 1)]), "tstart"] + 30
    lsh_ag_bronch <- lsh_ag_bronch[which(lsh_ag_bronch$tstart < lsh_ag_bronch$tstop),]
}
lsh_ag_bronch <- lsh_ag_bronch[,!grepl("obs_", colnames(lsh_ag_bronch))]
lsh_ag_bronch$tstart_m <- lsh_ag_bronch$tstart/30.45
lsh_ag_bronch$tstop_m <- lsh_ag_bronch$tstop/30.45
lsh_ag_bronch$group <- ifelse(lsh_ag_bronch$birth_y <= 2010, "Vaccine non-eligible", "Vaccine eligible")
lsh_ag_bronch$gender <- factor(lsh_ag_bronch$gender)

### Age at n = {1, 2, ... n} hospital admission for sepsis ###
lsh_surv_sepsis <- 
    lsh %>%
    filter(str_detect(icd10_code, "A40|A41"), birth_y >= 2005, birth_y <= 2015, type_visit == "admitted") %>%
    select(id, obs, birth_date, date_in) %>%
    distinct(id, obs, .keep_all = T) %>%
    mutate(age = pmin(3*365.25, pmax(1, as.numeric(as.Date(date_in) - birth_date)))) %>%
    select(-birth_date, -date_in) %>%
    filter(age < 3*365.25) %>%
    group_by(id) %>%
    arrange(id, age) %>%
    mutate(
        obs = cummax(as.numeric(factor(obs, levels = unique(obs))))
    ) %>%
    ungroup() %>%
    spread(key = obs, value = age, sep = "_") %>%
    as.data.frame() #til þess að forloop með tmerge() til að búa til lg_ag virki

### Create correct time at-risk for sepsis and link to demographics ###
lsh_ag_sepsis <- tmerge(ids_surv, ids_surv, id = id, tstop = ids_surv$age)
lsh_ag_sepsis$tstart <- ifelse(lsh_ag_sepsis$tstart == 0, 1, lsh_ag_sepsis$tstart)
lsh_ag_sepsis$obs_0 <- 0
for(i in 1:(dim(lsh_surv_sepsis)[2] - 1)){
    x <- paste0("obs_", i)
    lsh_ag_sepsis <- tmerge(lsh_ag_sepsis, lsh_surv_sepsis, id = id, visit = event(lsh_surv_sepsis[,x]))
    lsh_ag_sepsis <- tmerge(lsh_ag_sepsis, subset(lsh_ag_sepsis, visit == 1), id = id, obs = cumtdc(tstop))
    lsh_ag_sepsis[, paste0("obs_", i)] <- lsh_ag_sepsis[, "obs"]
    lsh_ag_sepsis[which(lsh_ag_sepsis$obs > 0 & lsh_ag_sepsis$obs > lsh_ag_sepsis[, paste0("obs_", i - 1)]), "tstart"] <-
        lsh_ag_sepsis[which(lsh_ag_sepsis$obs > 0 & lsh_ag_sepsis$obs > lsh_ag_sepsis[, paste0("obs_", i - 1)]), "tstart"] + 30
    lsh_ag_sepsis <- lsh_ag_sepsis[which(lsh_ag_sepsis$tstart < lsh_ag_sepsis$tstop),]
}
lsh_ag_sepsis <- lsh_ag_sepsis[,!grepl("obs_", colnames(lsh_ag_sepsis))]
lsh_ag_sepsis$tstart_m <- lsh_ag_sepsis$tstart/30.45
lsh_ag_sepsis$tstop_m <- lsh_ag_sepsis$tstop/30.45
lsh_ag_sepsis$group <- ifelse(lsh_ag_sepsis$birth_y <= 2010, "Vaccine non-eligible", "Vaccine eligible")
lsh_ag_sepsis$gender <- factor(lsh_ag_sepsis$gender)

### Age at n = {1, 2, ... n} hospital admission for IPD ###
lsh_surv_ipd <- 
    ids_surv %>%
    left_join(
        sykl %>%
            select(-death_date, -birth_m, -birth_y, -birth_d) %>%
            filter(date >= as.Date("2005-01-01")) %>%
            rename(date_ipd = date) %>%
            left_join(ids %>% select(id, kt, birth_date), by = c("kt", "birth_date")),
        by = c("id", "kt", "birth_date")
    ) %>%
    filter(
        spine == 1 | blood == 1 | joint == 1,
        age_months <= 35
    ) %>%
    left_join(lsh %>% filter(type_visit == "admitted"), by = c("id", "gender", "birth_y", "birth_date", "death_date"))  %>%
    mutate(diff = abs(as.numeric(difftime(date_ipd, date_in, unit = "days")))) %>%
    filter(diff < 7) %>%
    select(id, birth_date, date_ipd) %>%
    distinct(id, .keep_all = T) %>%
    mutate(age = pmin(3*365.25, pmax(1, as.numeric(as.Date(date_ipd) - birth_date)))) %>%
    select(-birth_date, -date_ipd) %>%
    filter(age < 3*365.25) %>%
    group_by(id) %>%
    arrange(id, age) %>%
    ungroup()

lsh_ag_ipd <- tmerge(ids_surv, ids_surv, id = id, tstop = ids_surv$age)
lsh_ag_ipd <- tmerge(lsh_ag_ipd, lsh_surv_ipd, id = id, visit = event(lsh_surv_ipd$age))
lsh_ag_ipd$tstart_m <- lsh_ag_ipd$tstart/30.45
lsh_ag_ipd$tstop_m <- lsh_ag_ipd$tstop/30.45
lsh_ag_ipd$group <- ifelse(lsh_ag_ipd$birth_y <= 2010, "Vaccine non-eligible", "Vaccine eligible")
lsh_ag_ipd$gender <- factor(lsh_ag_ipd$gender)

#######################
#### Data analysis ####
#######################

dd <- with(lsh_ag_men, datadist(gender, group, obs))
options(datadist = "dd")
dd$limits$obs[3] <- 1
lsh_cph_men <- cph(formula = Surv(tstop_m, visit) ~ group, data = subset(lsh_ag_men, obs = 0), x = T, y = T, model = T, surv = T)

dd <- with(lsh_ag_aom, datadist(gender, group, obs))
options(datadist = "dd")
dd$limits$obs[3] <- 1
lsh_cph_aom <- cph(formula = Surv(tstop_m, visit) ~ group, data = subset(lsh_ag_aom, obs = 0), x = T, y = T, model = T, surv = T)

dd <- with(lsh_ag_urti, datadist(gender, group, obs))
options(datadist = "dd")
dd$limits$obs[3] <- 1
lsh_cph_urti <- cph(formula = Surv(tstop_m, visit) ~ group, data = subset(lsh_ag_urti, obs = 0), x = T, y = T, model = T, surv = T)

dd <- with(lsh_ag_pneum, datadist(gender, group, obs))
options(datadist = "dd")
dd$limits$obs[3] <- 1
lsh_cph_pneum <- cph(formula = Surv(tstop_m, visit) ~ group, data = subset(lsh_ag_pneum, obs = 0), x = T, y = T, model = T, surv = T)

dd <- with(lsh_ag_bronch, datadist(gender, group, obs))
options(datadist = "dd")
dd$limits$obs[3] <- 1
lsh_cph_bronch <- cph(formula = Surv(tstop_m, visit) ~ group, data = subset(lsh_ag_bronch, obs = 0), x = T, y = T, model = T, surv = T)

dd <- with(lsh_ag_sepsis, datadist(gender, group, obs))
options(datadist = "dd")
dd$limits$obs[3] <- 1
lsh_cph_sepsis <- cph(formula = Surv(tstop_m, visit) ~ group, data = subset(lsh_ag_sepsis, obs = 0), x = T, y = T, model = T, surv = T)

dd <- with(lsh_ag_ipd, datadist(gender, group))
options(datadist = "dd")
lsh_cph_ipd <- cph(formula = Surv(tstop_m, visit) ~ group, data = subset(lsh_ag_ipd, obs = 0), x = T, y = T, model = T, surv = T)

cbind.data.frame(
    condition = c(
        "Otitis Media and Complications",
        "Acute upper respiratory infection",
        "Pneumonia",
        "Acute Lower Respiratory Tract Infections",
        "Sepsis",
        "Invasive Pneumococcal Disease"
    ),
    HR = c(
        summary(lsh_cph_aom)[2, 4],
        summary(lsh_cph_urti)[2, 4],
        summary(lsh_cph_pneum)[2, 4],
        summary(lsh_cph_bronch)[2, 4],
        summary(lsh_cph_sepsis)[2, 4],
        summary(lsh_cph_ipd)[2, 4]
    ),
    lowerci = c(
        summary(lsh_cph_aom)[2, 6],
        summary(lsh_cph_urti)[2, 6],
        summary(lsh_cph_pneum)[2, 6],
        summary(lsh_cph_bronch)[2, 6],
        summary(lsh_cph_sepsis)[2, 6],
        summary(lsh_cph_ipd)[2, 6]
    ),
    upperci = c(
        summary(lsh_cph_aom)[2, 7],
        summary(lsh_cph_urti)[2, 7],
        summary(lsh_cph_pneum)[2, 7],
        summary(lsh_cph_bronch)[2, 7],
        summary(lsh_cph_sepsis)[2, 7],
        summary(lsh_cph_ipd)[2, 7]
    )
)

### Incidence rate and incidence rate ratio for meningitis ###
ageg_order <- c("0-5","6-11","12-17","18-23","24-29","30-35")

lsh_npy_men <- 
    expand.grid(birth_y = 2005:2015, age_m = 0:35) %>%
    full_join(
        lsh_ag_men %>%
            select(id, birth_y, tstop_m) %>%
            distinct() %>%
            mutate(tstop_m = as.integer(tstop_m)),
        by = "birth_y") %>%
    filter(tstop_m >= age_m) %>%
    group_by(birth_y, age_m) %>%
    summarise(at_risk = n_distinct(id)) %>%
    ungroup() %>%
    mutate(
        group_m = case_when(
            .$age_m >= 0 & .$age_m <= 5 ~ "0-5",
            .$age_m >= 6 & .$age_m <= 11 ~ "6-11",
            .$age_m >= 12 & .$age_m <= 17 ~ "12-17",
            .$age_m >= 18 & .$age_m <= 23 ~ "18-23",
            .$age_m >= 24 & .$age_m <= 29 ~ "24-29",
            .$age_m >= 30 & .$age_m <= 35 ~ "30-35"
        ), 
        group_m = factor(group_m, levels = ageg_order, ordered = T)
    ) %>% 
    group_by(birth_y, group_m) %>%
    summarise(at_risk = round(sum(at_risk)/12)) %>%
    ungroup()

lsh_men_n <-
    lsh_ag_men %>%
    mutate(age_m = as.integer(tstop_m)) %>%
    group_by(birth_y, age_m) %>%
    summarise(visits = sum(visit)) %>%
    ungroup() %>%
    mutate(group_m =
               cut(
                   age_m,
                   breaks = c(0, 5, 11, 17, 23, 29, 35),
                   labels = c("0-5","6-11","12-17","18-23","24-29","30-35"),
                   include.lowest = T
               )
    ) %>%
    mutate(group_m = factor(group_m, levels = ageg_order, ordered = T)) %>%
    group_by(birth_y, group_m) %>%
    summarise(visits = sum(visits)) %>%
    ungroup()

lsh_men_ppy <-
    lsh_npy_men %>%
    left_join(lsh_men_n)

lsh_men_ppy[which(is.na(lsh_men_ppy$visits)), "visits"] <- 0

temp <- matrix(NA, ncol = 3, nrow = dim(lsh_men_ppy)[1])
for(i in 1:dim(lsh_men_ppy)[1]){
    temp[i,1] <- tidy(poisson.test(lsh_men_ppy$visits[i], lsh_men_ppy$at_risk[i]))$estimate*1000
    temp[i,2] <- tidy(poisson.test(lsh_men_ppy$visits[i], lsh_men_ppy$at_risk[i]))$conf.low*1000
    temp[i,3] <- tidy(poisson.test(lsh_men_ppy$visits[i], lsh_men_ppy$at_risk[i]))$conf.high*1000
}

lsh_men_ppy <- 
    cbind(lsh_men_ppy, temp) %>%
    rename(ir = `1`, lci = `2`, uci = `3`) %>%
    mutate(
        ir = format(round(ir, 2), nsmall = 2),
        lci = format(round(lci, 2), nsmall = 2),
        uci = format(round(uci, 2), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(ir, " ", "(", lci, "-", uci, ")"))

lsh_irr_men <-
    lsh_men_ppy %>%
    filter(birth_y == 2010) %>%
    select(group_m, visits, at_risk) %>%
    rename(visit2010 = visits, at_risk2010 = at_risk) %>%
    full_join(lsh_men_ppy %>% select(-ir, -lci, -uci, -ir_ci))

temp <- matrix(NA, ncol = 4, nrow = dim(lsh_irr_men)[1])
for(i in 1:dim(lsh_irr_men)[1]){
    temp[i,1] <- tidy(poisson.test(c(lsh_irr_men$visits[i], lsh_irr_men$visit2010[i]), c(lsh_irr_men$at_risk[i], lsh_irr_men$at_risk2010[i])))$estimate
    temp[i,2] <- tidy(poisson.test(c(lsh_irr_men$visits[i], lsh_irr_men$visit2010[i]), c(lsh_irr_men$at_risk[i], lsh_irr_men$at_risk2010[i])))$conf.low
    temp[i,3] <- tidy(poisson.test(c(lsh_irr_men$visits[i], lsh_irr_men$visit2010[i]), c(lsh_irr_men$at_risk[i], lsh_irr_men$at_risk2010[i])))$conf.high
    temp[i,4] <- tidy(poisson.test(c(lsh_irr_men$visits[i], lsh_irr_men$visit2010[i]), c(lsh_irr_men$at_risk[i], lsh_irr_men$at_risk2010[i])))$p.value
}

lsh_irr_men <- 
    cbind(lsh_irr_men, temp) %>%
    rename(lsh_irr_men = `1`, lci = `2`, uci = `3`, pvalue = `4`) %>%
    mutate(
        lsh_irr_men = format(round(lsh_irr_men, 2), nsmall = 2),
        lci = format(round(lci, 2), nsmall = 2),
        uci = format(round(uci, 2), nsmall = 2),
        pvalue = format(round(pvalue, 6), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(lsh_irr_men, " ", "(", lci, "-", uci, ")"))

### Incidence rate and incidence rate ratio for otitis media ###
lsh_npy_aom <- 
    expand.grid(birth_y = 2005:2015, age_m = 0:35) %>%
    full_join(
        lsh_ag_aom %>%
            select(id, birth_y, tstop_m) %>%
            distinct() %>%
            mutate(tstop_m = as.integer(tstop_m)),
        by = "birth_y") %>%
    filter(tstop_m >= age_m) %>%
    group_by(birth_y, age_m) %>%
    summarise(at_risk = n_distinct(id)) %>%
    ungroup() %>%
    mutate(
        group_m = case_when(
            .$age_m >= 0 & .$age_m <= 5 ~ "0-5",
            .$age_m >= 6 & .$age_m <= 11 ~ "6-11",
            .$age_m >= 12 & .$age_m <= 17 ~ "12-17",
            .$age_m >= 18 & .$age_m <= 23 ~ "18-23",
            .$age_m >= 24 & .$age_m <= 29 ~ "24-29",
            .$age_m >= 30 & .$age_m <= 35 ~ "30-35"
        ), 
        group_m = factor(group_m, levels = ageg_order, ordered = T)
    ) %>% 
    group_by(birth_y, group_m) %>%
    summarise(at_risk = round(sum(at_risk)/12)) %>%
    ungroup()

lsh_aom_n <-
    lsh_ag_aom %>%
    mutate(age_m = as.integer(tstop_m)) %>%
    group_by(birth_y, age_m) %>%
    summarise(visits = sum(visit)) %>%
    ungroup() %>%
    mutate(group_m =
               cut(
                   age_m,
                   breaks = c(0, 5, 11, 17, 23, 29, 35),
                   labels = c("0-5","6-11","12-17","18-23","24-29","30-35"),
                   include.lowest = T
               )
    ) %>%
    mutate(group_m = factor(group_m, levels = ageg_order, ordered = T)) %>%
    group_by(birth_y, group_m) %>%
    summarise(visits = sum(visits)) %>%
    ungroup()

lsh_aom_ppy <-
    lsh_npy_aom %>%
    left_join(lsh_aom_n)

lsh_aom_ppy[which(is.na(lsh_aom_ppy$visits)), "visits"] <- 0

temp <- matrix(NA, ncol = 3, nrow = dim(lsh_aom_ppy)[1])
for(i in 1:dim(lsh_aom_ppy)[1]){
    temp[i,1] <- tidy(poisson.test(lsh_aom_ppy$visits[i], lsh_aom_ppy$at_risk[i]))$estimate*1000
    temp[i,2] <- tidy(poisson.test(lsh_aom_ppy$visits[i], lsh_aom_ppy$at_risk[i]))$conf.low*1000
    temp[i,3] <- tidy(poisson.test(lsh_aom_ppy$visits[i], lsh_aom_ppy$at_risk[i]))$conf.high*1000
}

lsh_aom_ppy <- 
    cbind(lsh_aom_ppy, temp) %>%
    rename(ir = `1`, lci = `2`, uci = `3`) %>%
    mutate(
        ir = format(round(ir, 2), nsmall = 2),
        lci = format(round(lci, 2), nsmall = 2),
        uci = format(round(uci, 2), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(ir, " ", "(", lci, "-", uci, ")"))

lsh_irr_aom <-
    lsh_aom_ppy %>%
    filter(birth_y == 2010) %>%
    select(group_m, visits, at_risk) %>%
    rename(visit2010 = visits, at_risk2010 = at_risk) %>%
    full_join(lsh_aom_ppy %>% select(-ir, -lci, -uci, -ir_ci))

temp <- matrix(NA, ncol = 4, nrow = dim(lsh_irr_aom)[1])
for(i in 1:dim(lsh_irr_aom)[1]){
    temp[i,1] <- tidy(poisson.test(c(lsh_irr_aom$visits[i], lsh_irr_aom$visit2010[i]), c(lsh_irr_aom$at_risk[i], lsh_irr_aom$at_risk2010[i])))$estimate
    temp[i,2] <- tidy(poisson.test(c(lsh_irr_aom$visits[i], lsh_irr_aom$visit2010[i]), c(lsh_irr_aom$at_risk[i], lsh_irr_aom$at_risk2010[i])))$conf.low
    temp[i,3] <- tidy(poisson.test(c(lsh_irr_aom$visits[i], lsh_irr_aom$visit2010[i]), c(lsh_irr_aom$at_risk[i], lsh_irr_aom$at_risk2010[i])))$conf.high
    temp[i,4] <- tidy(poisson.test(c(lsh_irr_aom$visits[i], lsh_irr_aom$visit2010[i]), c(lsh_irr_aom$at_risk[i], lsh_irr_aom$at_risk2010[i])))$p.value
}

lsh_irr_aom <- 
    cbind(lsh_irr_aom, temp) %>%
    rename(lsh_irr_aom = `1`, lci = `2`, uci = `3`, pvalue = `4`) %>%
    mutate(
        lsh_irr_aom = format(round(lsh_irr_aom, 2), nsmall = 2),
        lci = format(round(lci, 2), nsmall = 2),
        uci = format(round(uci, 2), nsmall = 2),
        pvalue = format(round(pvalue, 6), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(lsh_irr_aom, " ", "(", lci, "-", uci, ")"))

### Incidence rate and incidence rate ratio for urti ###
lsh_npy_urti <- 
    expand.grid(birth_y = 2005:2015, age_m = 0:35) %>%
    full_join(
        lsh_ag_urti %>%
            select(id, birth_y, tstop_m) %>%
            distinct() %>%
            mutate(tstop_m = as.integer(tstop_m)),
        by = "birth_y") %>%
    filter(tstop_m >= age_m) %>%
    group_by(birth_y, age_m) %>%
    summarise(at_risk = n_distinct(id)) %>%
    ungroup() %>%
    mutate(
        group_m = case_when(
            .$age_m >= 0 & .$age_m <= 5 ~ "0-5",
            .$age_m >= 6 & .$age_m <= 11 ~ "6-11",
            .$age_m >= 12 & .$age_m <= 17 ~ "12-17",
            .$age_m >= 18 & .$age_m <= 23 ~ "18-23",
            .$age_m >= 24 & .$age_m <= 29 ~ "24-29",
            .$age_m >= 30 & .$age_m <= 35 ~ "30-35"
        ), 
        group_m = factor(group_m, levels = ageg_order, ordered = T)
    ) %>% 
    group_by(birth_y, group_m) %>%
    summarise(at_risk = round(sum(at_risk)/12)) %>%
    ungroup()

lsh_urti_n <-
    lsh_ag_urti %>%
    mutate(age_m = as.integer(tstop_m)) %>%
    group_by(birth_y, age_m) %>%
    summarise(visits = sum(visit)) %>%
    ungroup() %>%
    mutate(group_m =
               cut(
                   age_m,
                   breaks = c(0, 5, 11, 17, 23, 29, 35),
                   labels = c("0-5","6-11","12-17","18-23","24-29","30-35"),
                   include.lowest = T
               )
    ) %>%
    mutate(group_m = factor(group_m, levels = ageg_order, ordered = T)) %>%
    group_by(birth_y, group_m) %>%
    summarise(visits = sum(visits)) %>%
    ungroup()

lsh_urti_ppy <-
    lsh_npy_urti %>%
    left_join(lsh_urti_n)

lsh_urti_ppy[which(is.na(lsh_urti_ppy$visits)), "visits"] <- 0

temp <- matrix(NA, ncol = 3, nrow = dim(lsh_urti_ppy)[1])
for(i in 1:dim(lsh_urti_ppy)[1]){
    temp[i,1] <- tidy(poisson.test(lsh_urti_ppy$visits[i], lsh_urti_ppy$at_risk[i]))$estimate*1000
    temp[i,2] <- tidy(poisson.test(lsh_urti_ppy$visits[i], lsh_urti_ppy$at_risk[i]))$conf.low*1000
    temp[i,3] <- tidy(poisson.test(lsh_urti_ppy$visits[i], lsh_urti_ppy$at_risk[i]))$conf.high*1000
}

lsh_urti_ppy <- 
    cbind(lsh_urti_ppy, temp) %>%
    rename(ir = `1`, lci = `2`, uci = `3`) %>%
    mutate(
        ir = format(round(ir, 2), nsmall = 2),
        lci = format(round(lci, 2), nsmall = 2),
        uci = format(round(uci, 2), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(ir, " ", "(", lci, "-", uci, ")"))

lsh_irr_urti <-
    lsh_urti_ppy %>%
    filter(birth_y == 2010) %>%
    select(group_m, visits, at_risk) %>%
    rename(visit2010 = visits, at_risk2010 = at_risk) %>%
    full_join(lsh_urti_ppy %>% select(-ir, -lci, -uci, -ir_ci))

temp <- matrix(NA, ncol = 4, nrow = dim(lsh_irr_urti)[1])
for(i in 1:dim(lsh_irr_urti)[1]){
    temp[i,1] <- tidy(poisson.test(c(lsh_irr_urti$visits[i], lsh_irr_urti$visit2010[i]), c(lsh_irr_urti$at_risk[i], lsh_irr_urti$at_risk2010[i])))$estimate
    temp[i,2] <- tidy(poisson.test(c(lsh_irr_urti$visits[i], lsh_irr_urti$visit2010[i]), c(lsh_irr_urti$at_risk[i], lsh_irr_urti$at_risk2010[i])))$conf.low
    temp[i,3] <- tidy(poisson.test(c(lsh_irr_urti$visits[i], lsh_irr_urti$visit2010[i]), c(lsh_irr_urti$at_risk[i], lsh_irr_urti$at_risk2010[i])))$conf.high
    temp[i,4] <- tidy(poisson.test(c(lsh_irr_urti$visits[i], lsh_irr_urti$visit2010[i]), c(lsh_irr_urti$at_risk[i], lsh_irr_urti$at_risk2010[i])))$p.value
}

lsh_irr_urti <- 
    cbind(lsh_irr_urti, temp) %>%
    rename(lsh_irr_urti = `1`, lci = `2`, uci = `3`, pvalue = `4`) %>%
    mutate(
        lsh_irr_urti = format(round(lsh_irr_urti, 2), nsmall = 2),
        lci = format(round(lci, 2), nsmall = 2),
        uci = format(round(uci, 2), nsmall = 2),
        pvalue = format(round(pvalue, 6), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(lsh_irr_urti, " ", "(", lci, "-", uci, ")"))

### Incidence rate and incidence rate ratio for pneumonia ###
lsh_npy_pneum <- 
    expand.grid(birth_y = 2005:2015, age_m = 0:35) %>%
    full_join(
        lsh_ag_pneum %>%
            select(id, birth_y, tstop_m) %>%
            distinct() %>%
            mutate(tstop_m = as.integer(tstop_m)),
        by = "birth_y") %>%
    filter(tstop_m >= age_m) %>%
    group_by(birth_y, age_m) %>%
    summarise(at_risk = n_distinct(id)) %>%
    ungroup() %>%
    mutate(
        group_m = case_when(
            .$age_m >= 0 & .$age_m <= 5 ~ "0-5",
            .$age_m >= 6 & .$age_m <= 11 ~ "6-11",
            .$age_m >= 12 & .$age_m <= 17 ~ "12-17",
            .$age_m >= 18 & .$age_m <= 23 ~ "18-23",
            .$age_m >= 24 & .$age_m <= 29 ~ "24-29",
            .$age_m >= 30 & .$age_m <= 35 ~ "30-35"
        ), 
        group_m = factor(group_m, levels = ageg_order, ordered = T)
    ) %>% 
    group_by(birth_y, group_m) %>%
    summarise(at_risk = round(sum(at_risk)/12)) %>%
    ungroup()

lsh_pneum_n <-
    lsh_ag_pneum %>%
    mutate(age_m = as.integer(tstop_m)) %>%
    group_by(birth_y, age_m) %>%
    summarise(visits = sum(visit)) %>%
    ungroup() %>%
    mutate(group_m =
               cut(
                   age_m,
                   breaks = c(0, 5, 11, 17, 23, 29, 35),
                   labels = c("0-5","6-11","12-17","18-23","24-29","30-35"),
                   include.lowest = T
               )
    ) %>%
    mutate(group_m = factor(group_m, levels = ageg_order, ordered = T)) %>%
    group_by(birth_y, group_m) %>%
    summarise(visits = sum(visits)) %>%
    ungroup()

lsh_pneum_ppy <-
    lsh_npy_pneum %>%
    left_join(lsh_pneum_n)

lsh_pneum_ppy[which(is.na(lsh_pneum_ppy$visits)), "visits"] <- 0

temp <- matrix(NA, ncol = 3, nrow = dim(lsh_pneum_ppy)[1])
for(i in 1:dim(lsh_pneum_ppy)[1]){
    temp[i,1] <- tidy(poisson.test(lsh_pneum_ppy$visits[i], lsh_pneum_ppy$at_risk[i]))$estimate*1000
    temp[i,2] <- tidy(poisson.test(lsh_pneum_ppy$visits[i], lsh_pneum_ppy$at_risk[i]))$conf.low*1000
    temp[i,3] <- tidy(poisson.test(lsh_pneum_ppy$visits[i], lsh_pneum_ppy$at_risk[i]))$conf.high*1000
}

lsh_pneum_ppy <- 
    cbind(lsh_pneum_ppy, temp) %>%
    rename(ir = `1`, lci = `2`, uci = `3`) %>%
    mutate(
        ir = format(round(ir, 2), nsmall = 2),
        lci = format(round(lci, 2), nsmall = 2),
        uci = format(round(uci, 2), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(ir, " ", "(", lci, "-", uci, ")"))

lsh_irr_pneum <-
    lsh_pneum_ppy %>%
    filter(birth_y == 2010) %>%
    select(group_m, visits, at_risk) %>%
    rename(visit2010 = visits, at_risk2010 = at_risk) %>%
    full_join(lsh_pneum_ppy %>% select(-ir, -lci, -uci, -ir_ci))

temp <- matrix(NA, ncol = 4, nrow = dim(lsh_irr_pneum)[1])
for(i in 1:dim(lsh_irr_pneum)[1]){
    temp[i,1] <- tidy(poisson.test(c(lsh_irr_pneum$visits[i], lsh_irr_pneum$visit2010[i]), c(lsh_irr_pneum$at_risk[i], lsh_irr_pneum$at_risk2010[i])))$estimate
    temp[i,2] <- tidy(poisson.test(c(lsh_irr_pneum$visits[i], lsh_irr_pneum$visit2010[i]), c(lsh_irr_pneum$at_risk[i], lsh_irr_pneum$at_risk2010[i])))$conf.low
    temp[i,3] <- tidy(poisson.test(c(lsh_irr_pneum$visits[i], lsh_irr_pneum$visit2010[i]), c(lsh_irr_pneum$at_risk[i], lsh_irr_pneum$at_risk2010[i])))$conf.high
    temp[i,4] <- tidy(poisson.test(c(lsh_irr_pneum$visits[i], lsh_irr_pneum$visit2010[i]), c(lsh_irr_pneum$at_risk[i], lsh_irr_pneum$at_risk2010[i])))$p.value
}

lsh_irr_pneum <- 
    cbind(lsh_irr_pneum, temp) %>%
    rename(lsh_irr_pneum = `1`, lci = `2`, uci = `3`, pvalue = `4`) %>%
    mutate(
        lsh_irr_pneum = format(round(lsh_irr_pneum, 2), nsmall = 2),
        lci = format(round(lci, 2), nsmall = 2),
        uci = format(round(uci, 2), nsmall = 2),
        pvalue = format(round(pvalue, 6), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(lsh_irr_pneum, " ", "(", lci, "-", uci, ")"))

### Incidence rate and incidence rate ratio for lrti ###
lsh_npy_bronch <- 
    expand.grid(birth_y = 2005:2015, age_m = 0:35) %>%
    full_join(
        lsh_ag_bronch %>%
            select(id, birth_y, tstop_m) %>%
            distinct() %>%
            mutate(tstop_m = as.integer(tstop_m)),
        by = "birth_y") %>%
    filter(tstop_m >= age_m) %>%
    group_by(birth_y, age_m) %>%
    summarise(at_risk = n_distinct(id)) %>%
    ungroup() %>%
    mutate(
        group_m = case_when(
            .$age_m >= 0 & .$age_m <= 5 ~ "0-5",
            .$age_m >= 6 & .$age_m <= 11 ~ "6-11",
            .$age_m >= 12 & .$age_m <= 17 ~ "12-17",
            .$age_m >= 18 & .$age_m <= 23 ~ "18-23",
            .$age_m >= 24 & .$age_m <= 29 ~ "24-29",
            .$age_m >= 30 & .$age_m <= 35 ~ "30-35"
        ), 
        group_m = factor(group_m, levels = ageg_order, ordered = T)
    ) %>% 
    group_by(birth_y, group_m) %>%
    summarise(at_risk = round(sum(at_risk)/12)) %>%
    ungroup()

lsh_bronch_n <-
    lsh_ag_bronch %>%
    mutate(age_m = as.integer(tstop_m)) %>%
    group_by(birth_y, age_m) %>%
    summarise(visits = sum(visit)) %>%
    ungroup() %>%
    mutate(group_m =
               cut(
                   age_m,
                   breaks = c(0, 5, 11, 17, 23, 29, 35),
                   labels = c("0-5","6-11","12-17","18-23","24-29","30-35"),
                   include.lowest = T
               )
    ) %>%
    mutate(group_m = factor(group_m, levels = ageg_order, ordered = T)) %>%
    group_by(birth_y, group_m) %>%
    summarise(visits = sum(visits)) %>%
    ungroup()

lsh_bronch_ppy <-
    lsh_npy_bronch %>%
    left_join(lsh_bronch_n)

lsh_bronch_ppy[which(is.na(lsh_bronch_ppy$visits)), "visits"] <- 0

temp <- matrix(NA, ncol = 3, nrow = dim(lsh_bronch_ppy)[1])
for(i in 1:dim(lsh_bronch_ppy)[1]){
    temp[i,1] <- tidy(poisson.test(lsh_bronch_ppy$visits[i], lsh_bronch_ppy$at_risk[i]))$estimate*1000
    temp[i,2] <- tidy(poisson.test(lsh_bronch_ppy$visits[i], lsh_bronch_ppy$at_risk[i]))$conf.low*1000
    temp[i,3] <- tidy(poisson.test(lsh_bronch_ppy$visits[i], lsh_bronch_ppy$at_risk[i]))$conf.high*1000
}

lsh_bronch_ppy <- 
    cbind(lsh_bronch_ppy, temp) %>%
    rename(ir = `1`, lci = `2`, uci = `3`) %>%
    mutate(
        ir = format(round(ir, 2), nsmall = 2),
        lci = format(round(lci, 2), nsmall = 2),
        uci = format(round(uci, 2), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(ir, " ", "(", lci, "-", uci, ")"))

lsh_irr_bronch <-
    lsh_bronch_ppy %>%
    filter(birth_y == 2010) %>%
    select(group_m, visits, at_risk) %>%
    rename(visit2010 = visits, at_risk2010 = at_risk) %>%
    full_join(lsh_bronch_ppy %>% select(-ir, -lci, -uci, -ir_ci))

temp <- matrix(NA, ncol = 4, nrow = dim(lsh_irr_bronch)[1])
for(i in 1:dim(lsh_irr_bronch)[1]){
    temp[i,1] <- tidy(poisson.test(c(lsh_irr_bronch$visits[i], lsh_irr_bronch$visit2010[i]), c(lsh_irr_bronch$at_risk[i], lsh_irr_bronch$at_risk2010[i])))$estimate
    temp[i,2] <- tidy(poisson.test(c(lsh_irr_bronch$visits[i], lsh_irr_bronch$visit2010[i]), c(lsh_irr_bronch$at_risk[i], lsh_irr_bronch$at_risk2010[i])))$conf.low
    temp[i,3] <- tidy(poisson.test(c(lsh_irr_bronch$visits[i], lsh_irr_bronch$visit2010[i]), c(lsh_irr_bronch$at_risk[i], lsh_irr_bronch$at_risk2010[i])))$conf.high
    temp[i,4] <- tidy(poisson.test(c(lsh_irr_bronch$visits[i], lsh_irr_bronch$visit2010[i]), c(lsh_irr_bronch$at_risk[i], lsh_irr_bronch$at_risk2010[i])))$p.value
}

lsh_irr_bronch <- 
    cbind(lsh_irr_bronch, temp) %>%
    rename(lsh_irr_bronch = `1`, lci = `2`, uci = `3`, pvalue = `4`) %>%
    mutate(
        lsh_irr_bronch = format(round(lsh_irr_bronch, 2), nsmall = 2),
        lci = format(round(lci, 2), nsmall = 2),
        uci = format(round(uci, 2), nsmall = 2),
        pvalue = format(round(pvalue, 6), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(lsh_irr_bronch, " ", "(", lci, "-", uci, ")"))

### Incidence rate and incidence rate ratio for sepsis ###
lsh_npy_sepsis <- 
    expand.grid(birth_y = 2005:2015, age_m = 0:35) %>%
    full_join(
        lsh_ag_sepsis %>%
            select(id, birth_y, tstop_m) %>%
            distinct() %>%
            mutate(tstop_m = as.integer(tstop_m)),
        by = "birth_y") %>%
    filter(tstop_m >= age_m) %>%
    group_by(birth_y, age_m) %>%
    summarise(at_risk = n_distinct(id)) %>%
    ungroup() %>%
    mutate(
        group_m = case_when(
            .$age_m >= 0 & .$age_m <= 5 ~ "0-5",
            .$age_m >= 6 & .$age_m <= 11 ~ "6-11",
            .$age_m >= 12 & .$age_m <= 17 ~ "12-17",
            .$age_m >= 18 & .$age_m <= 23 ~ "18-23",
            .$age_m >= 24 & .$age_m <= 29 ~ "24-29",
            .$age_m >= 30 & .$age_m <= 35 ~ "30-35"
        ), 
        group_m = factor(group_m, levels = ageg_order, ordered = T)
    ) %>% 
    group_by(birth_y, group_m) %>%
    summarise(at_risk = round(sum(at_risk)/12)) %>%
    ungroup()

lsh_sepsis_n <-
    lsh_ag_sepsis %>%
    mutate(age_m = as.integer(tstop_m)) %>%
    group_by(birth_y, age_m) %>%
    summarise(visits = sum(visit)) %>%
    ungroup() %>%
    mutate(group_m =
               cut(
                   age_m,
                   breaks = c(0, 5, 11, 17, 23, 29, 35),
                   labels = c("0-5","6-11","12-17","18-23","24-29","30-35"),
                   include.lowest = T
               )
    ) %>%
    mutate(group_m = factor(group_m, levels = ageg_order, ordered = T)) %>%
    group_by(birth_y, group_m) %>%
    summarise(visits = sum(visits)) %>%
    ungroup()

lsh_sepsis_ppy <-
    lsh_npy_sepsis %>%
    left_join(lsh_sepsis_n)

lsh_sepsis_ppy[which(is.na(lsh_sepsis_ppy$visits)), "visits"] <- 0

temp <- matrix(NA, ncol = 3, nrow = dim(lsh_sepsis_ppy)[1])
for(i in 1:dim(lsh_sepsis_ppy)[1]){
    temp[i,1] <- tidy(poisson.test(lsh_sepsis_ppy$visits[i], lsh_sepsis_ppy$at_risk[i]))$estimate*1000
    temp[i,2] <- tidy(poisson.test(lsh_sepsis_ppy$visits[i], lsh_sepsis_ppy$at_risk[i]))$conf.low*1000
    temp[i,3] <- tidy(poisson.test(lsh_sepsis_ppy$visits[i], lsh_sepsis_ppy$at_risk[i]))$conf.high*1000
}

lsh_sepsis_ppy <- 
    cbind(lsh_sepsis_ppy, temp) %>%
    rename(ir = `1`, lci = `2`, uci = `3`) %>%
    mutate(
        ir = format(round(ir, 2), nsmall = 2),
        lci = format(round(lci, 2), nsmall = 2),
        uci = format(round(uci, 2), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(ir, " ", "(", lci, "-", uci, ")"))

lsh_irr_sepsis <-
    lsh_sepsis_ppy %>%
    filter(birth_y == 2010) %>%
    select(group_m, visits, at_risk) %>%
    rename(visit2010 = visits, at_risk2010 = at_risk) %>%
    full_join(lsh_sepsis_ppy %>% select(-ir, -lci, -uci, -ir_ci))

temp <- matrix(NA, ncol = 4, nrow = dim(lsh_irr_sepsis)[1])
for(i in 1:dim(lsh_irr_sepsis)[1]){
    temp[i,1] <- tidy(poisson.test(c(lsh_irr_sepsis$visits[i], lsh_irr_sepsis$visit2010[i]), c(lsh_irr_sepsis$at_risk[i], lsh_irr_sepsis$at_risk2010[i])))$estimate
    temp[i,2] <- tidy(poisson.test(c(lsh_irr_sepsis$visits[i], lsh_irr_sepsis$visit2010[i]), c(lsh_irr_sepsis$at_risk[i], lsh_irr_sepsis$at_risk2010[i])))$conf.low
    temp[i,3] <- tidy(poisson.test(c(lsh_irr_sepsis$visits[i], lsh_irr_sepsis$visit2010[i]), c(lsh_irr_sepsis$at_risk[i], lsh_irr_sepsis$at_risk2010[i])))$conf.high
    temp[i,4] <- tidy(poisson.test(c(lsh_irr_sepsis$visits[i], lsh_irr_sepsis$visit2010[i]), c(lsh_irr_sepsis$at_risk[i], lsh_irr_sepsis$at_risk2010[i])))$p.value
}

lsh_irr_sepsis <- 
    cbind(lsh_irr_sepsis, temp) %>%
    rename(lsh_irr_sepsis = `1`, lci = `2`, uci = `3`, pvalue = `4`) %>%
    mutate(
        lsh_irr_sepsis = format(round(lsh_irr_sepsis, 2), nsmall = 2),
        lci = format(round(lci, 2), nsmall = 2),
        uci = format(round(uci, 2), nsmall = 2),
        pvalue = format(round(pvalue, 6), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(lsh_irr_sepsis, " ", "(", lci, "-", uci, ")"))

### Incidence rate and incidence rate ratio for IPD ###
lsh_npy_ipd <- 
    expand.grid(birth_y = 2005:2015, age_m = 0:35) %>%
    full_join(
        lsh_ag_ipd %>%
            select(id, birth_y, tstop_m) %>%
            distinct() %>%
            mutate(tstop_m = as.integer(tstop_m)),
        by = "birth_y") %>%
    filter(tstop_m >= age_m) %>%
    group_by(birth_y, age_m) %>%
    summarise(at_risk = n_distinct(id)) %>%
    ungroup() %>%
    mutate(
        group_m = case_when(
            .$age_m >= 0 & .$age_m <= 5 ~ "0-5",
            .$age_m >= 6 & .$age_m <= 11 ~ "6-11",
            .$age_m >= 12 & .$age_m <= 17 ~ "12-17",
            .$age_m >= 18 & .$age_m <= 23 ~ "18-23",
            .$age_m >= 24 & .$age_m <= 29 ~ "24-29",
            .$age_m >= 30 & .$age_m <= 35 ~ "30-35"
        ), 
        group_m = factor(group_m, levels = ageg_order, ordered = T)
    ) %>% 
    group_by(birth_y, group_m) %>%
    summarise(at_risk = round(sum(at_risk)/12)) %>%
    ungroup()

lsh_ipd_n <-
    lsh_ag_ipd %>%
    mutate(age_m = as.integer(tstop_m)) %>%
    group_by(birth_y, age_m) %>%
    summarise(visits = sum(visit)) %>%
    ungroup() %>%
    mutate(group_m =
               cut(
                   age_m,
                   breaks = c(0, 5, 11, 17, 23, 29, 35),
                   labels = c("0-5","6-11","12-17","18-23","24-29","30-35"),
                   include.lowest = T
               )
    ) %>%
    mutate(group_m = factor(group_m, levels = ageg_order, ordered = T)) %>%
    group_by(birth_y, group_m) %>%
    summarise(visits = sum(visits)) %>%
    ungroup()

lsh_ipd_ppy <-
    lsh_npy_ipd %>%
    left_join(lsh_ipd_n)

lsh_ipd_ppy[which(is.na(lsh_ipd_ppy$visits)), "visits"] <- 0

temp <- matrix(NA, ncol = 3, nrow = dim(lsh_ipd_ppy)[1])
for(i in 1:dim(lsh_ipd_ppy)[1]){
    temp[i,1] <- tidy(poisson.test(lsh_ipd_ppy$visits[i], lsh_ipd_ppy$at_risk[i]))$estimate*1000
    temp[i,2] <- tidy(poisson.test(lsh_ipd_ppy$visits[i], lsh_ipd_ppy$at_risk[i]))$conf.low*1000
    temp[i,3] <- tidy(poisson.test(lsh_ipd_ppy$visits[i], lsh_ipd_ppy$at_risk[i]))$conf.high*1000
}

lsh_ipd_ppy <- 
    cbind(lsh_ipd_ppy, temp) %>%
    rename(ir = `1`, lci = `2`, uci = `3`) %>%
    mutate(
        ir = format(round(ir, 2), nsmall = 2),
        lci = format(round(lci, 2), nsmall = 2),
        uci = format(round(uci, 2), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(ir, " ", "(", lci, "-", uci, ")"))

lsh_irr_ipd <-
    lsh_ipd_ppy %>%
    filter(birth_y == 2010) %>%
    select(group_m, visits, at_risk) %>%
    rename(visit2010 = visits, at_risk2010 = at_risk) %>%
    full_join(lsh_ipd_ppy %>% select(-ir, -lci, -uci, -ir_ci))

temp <- matrix(NA, ncol = 4, nrow = dim(lsh_irr_ipd)[1])
for(i in 1:dim(lsh_irr_ipd)[1]){
    temp[i,1] <- tidy(poisson.test(c(lsh_irr_ipd$visits[i], lsh_irr_ipd$visit2010[i]), c(lsh_irr_ipd$at_risk[i], lsh_irr_ipd$at_risk2010[i])))$estimate
    temp[i,2] <- tidy(poisson.test(c(lsh_irr_ipd$visits[i], lsh_irr_ipd$visit2010[i]), c(lsh_irr_ipd$at_risk[i], lsh_irr_ipd$at_risk2010[i])))$conf.low
    temp[i,3] <- tidy(poisson.test(c(lsh_irr_ipd$visits[i], lsh_irr_ipd$visit2010[i]), c(lsh_irr_ipd$at_risk[i], lsh_irr_ipd$at_risk2010[i])))$conf.high
    temp[i,4] <- tidy(poisson.test(c(lsh_irr_ipd$visits[i], lsh_irr_ipd$visit2010[i]), c(lsh_irr_ipd$at_risk[i], lsh_irr_ipd$at_risk2010[i])))$p.value
}

lsh_irr_ipd <- 
    cbind(lsh_irr_ipd, temp) %>%
    rename(lsh_irr_ipd = `1`, lci = `2`, uci = `3`, pvalue = `4`) %>%
    mutate(
        lsh_irr_ipd = format(round(lsh_irr_ipd, 2), nsmall = 2),
        lci = format(round(lci, 2), nsmall = 2),
        uci = format(round(uci, 2), nsmall = 2),
        pvalue = format(round(pvalue, 6), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(lsh_irr_ipd, " ", "(", lci, "-", uci, ")"))

### Incidence rate graphs for each study diagnostic grouping ###

birth_y <- c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
p_men <- 
    lsh_men_ppy %>%
    mutate(ir = as.numeric(ir),
           lci = as.numeric(lci),
           uci = as.numeric(uci)) %>%
    ggplot(aes(
        x = factor(group_m, levels = ageg_order),
        y = ir,
        color = factor(
            birth_y,
            levels = rev(levels(factor(birth_y))),
            labels = rev(levels(factor(birth_y)))
        )
    )) +
    geom_point(size = 0.5, position = position_dodge(width = 0.9)) +
    geom_errorbar(
        aes(ymin = lci, ymax = uci),
        width = 0.8,
        size = 0.5,
        position = position_dodge(width = 0.9)
    ) +
    geom_vline(aes(xintercept = as.numeric(factor(group_m, levels = ageg_order)) - 0.5), color = "grey") +
    scale_y_continuous(
        breaks = c(0, 2.5, 5, 7.5),
        minor_breaks = c(1.25, 3.75, 6.25),
        limits = c(0, 7.5)
    ) +
    labs(x = NULL, y = NULL, title = "Meningitis") +
    coord_flip(xlim = c(1.1, 5.9)) +
    scale_x_discrete(limits = rev(levels(factor(ageg_order, levels = ageg_order)))) +
    scale_color_manual(
        breaks = levels(factor(birth_y)),
        labels = levels(factor(birth_y)),
        values = c(
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C"
        )
    ) +
    theme(
        legend.title = element_blank(),
        title = element_text(size = 6),
        panel.grid.major.y = element_blank()
    )

p_aom <- 
    lsh_aom_ppy %>%
    mutate(ir = as.numeric(ir),
           lci = as.numeric(lci),
           uci = as.numeric(uci)) %>%
    ggplot(aes(
        x = factor(group_m, levels = ageg_order),
        y = ir,
        color = factor(
            birth_y,
            levels = rev(levels(factor(birth_y))),
            labels = rev(levels(factor(birth_y)))
        )
    )) +
    geom_point(size = 0.5, position = position_dodge(width = 0.9)) +
    geom_errorbar(
        aes(ymin = lci, ymax = uci),
        width = 0.8,
        size = 0.5,
        position = position_dodge(width = 0.9)
    ) +
    geom_vline(aes(xintercept = as.numeric(factor(group_m, levels = ageg_order)) - 0.5), color = "grey") +
    scale_y_continuous(
        breaks = c(0, 5, 10, 15),
        minor_breaks = c(2.5, 7.5, 12.5),
        limits = c(0, 15)
    ) +
    labs(x = NULL, y = NULL, title = "Otitis Media and Complications") +
    coord_flip(xlim = c(1.1, 5.9)) +
    scale_x_discrete(limits = rev(levels(factor(ageg_order, levels = ageg_order)))) +
    scale_color_manual(
        breaks = levels(factor(birth_y)),
        labels = levels(factor(birth_y)),
        values = c(
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C"
        )
    ) +
    theme(
        legend.title = element_blank(),
        title = element_text(size = 6),
        panel.grid.major.y = element_blank()
    )

p_urti <-
    lsh_urti_ppy %>%
    mutate(ir = as.numeric(ir),
           lci = as.numeric(lci),
           uci = as.numeric(uci)) %>%
    ggplot(aes(
        x = factor(group_m, levels = ageg_order),
        y = ir,
        color = factor(
            birth_y,
            levels = rev(levels(factor(birth_y))),
            labels = rev(levels(factor(birth_y)))
        )
    )) +
    geom_point(size = 0.5, position = position_dodge(width = 0.9)) +
    geom_errorbar(
        aes(ymin = lci, ymax = uci),
        width = 0.8,
        size = 0.5,
        position = position_dodge(width = 0.9)
    ) +
    geom_vline(aes(xintercept = as.numeric(factor(group_m, levels = ageg_order)) - 0.5), color = "grey") +
    scale_y_continuous(
        breaks = c(0, 5, 10, 15),
        minor_breaks = c(2.5, 7.5, 12.5),
        limits = c(0, 15)
    ) +
    labs(x = NULL, y = NULL, title = "Acute Upper Respiratory Tract Infections") +
    coord_flip(xlim = c(1.1, 5.9)) +
    scale_x_discrete(limits = rev(levels(factor(ageg_order, levels = ageg_order)))) +
    scale_color_manual(
        breaks = levels(factor(birth_y)),
        labels = levels(factor(birth_y)),
        values = c(
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C"
        )
    ) +
    theme(
        legend.title = element_blank(),
        title = element_text(size = 6),
        panel.grid.major.y = element_blank()
    )

p_pneum <-
    lsh_pneum_ppy %>%
    mutate(ir = as.numeric(ir),
           lci = as.numeric(lci),
           uci = as.numeric(uci)) %>%
    ggplot(aes(
        x = factor(group_m, levels = ageg_order),
        y = ir,
        color = factor(
            birth_y,
            levels = rev(levels(factor(birth_y))),
            labels = rev(levels(factor(birth_y)))
        )
    )) +
    geom_point(size = 0.5, position = position_dodge(width = 0.9)) +
    geom_errorbar(
        aes(ymin = lci, ymax = uci),
        width = 0.8,
        size = 0.5,
        position = position_dodge(width = 0.9)
    ) +
    geom_vline(aes(xintercept = as.numeric(factor(group_m, levels = ageg_order)) - 0.5), color = "grey") +
    scale_y_continuous(
        breaks = c(0, 5, 10, 15),
        minor_breaks = c(2.5, 7.5, 12.5),
        limits = c(0, 15)
    ) +
    labs(x = NULL, y = NULL, title = "Pneumonia") +
    coord_flip(xlim = c(1.1, 5.9)) +
    scale_x_discrete(limits = rev(levels(factor(ageg_order, levels = ageg_order)))) +
    scale_color_manual(
        breaks = levels(factor(birth_y)),
        labels = levels(factor(birth_y)),
        values = c(
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C"
        )
    ) +
    theme(
        legend.title = element_blank(),
        title = element_text(size = 6),
        panel.grid.major.y = element_blank()
    )

p_bronch <-
    lsh_bronch_ppy %>%
    mutate(ir = as.numeric(ir),
           lci = as.numeric(lci),
           uci = as.numeric(uci)) %>%
    ggplot(aes(
        x = factor(group_m, levels = ageg_order),
        y = ir,
        color = factor(
            birth_y,
            levels = rev(levels(factor(birth_y))),
            labels = rev(levels(factor(birth_y)))
        )
    )) +
    geom_point(size = 0.5, position = position_dodge(width = 0.9)) +
    geom_errorbar(
        aes(ymin = lci, ymax = uci),
        width = 0.8,
        size = 0.5,
        position = position_dodge(width = 0.9)
    ) +
    geom_vline(aes(xintercept = as.numeric(factor(group_m, levels = ageg_order)) - 0.5), color = "grey") +
    scale_y_continuous(
        breaks = c(0, 10, 20, 30),
        minor_breaks = c(5, 15, 25),
        limits = c(0, 32)
    ) +
    labs(x = NULL, y = NULL, title = "Acute Lower Respiratory Tract Infections") +
    coord_flip(xlim = c(1.1, 5.9), ylim = c(0,30)) +
    scale_x_discrete(limits = rev(levels(factor(ageg_order, levels = ageg_order)))) +
    scale_color_manual(
        breaks = levels(factor(birth_y)),
        labels = levels(factor(birth_y)),
        values = c(
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C"
        )
    ) +
    theme(
        legend.title = element_blank(),
        title = element_text(size = 6),
        panel.grid.major.y = element_blank()
    )

p_sepsis <-
    lsh_sepsis_ppy %>%
    mutate(ir = as.numeric(ir),
           lci = as.numeric(lci),
           uci = as.numeric(uci)) %>%
    ggplot(aes(
        x = factor(group_m, levels = ageg_order),
        y = ir,
        color = factor(
            birth_y,
            levels = rev(levels(factor(birth_y))),
            labels = rev(levels(factor(birth_y)))
        )
    )) +
    geom_point(size = 0.5, position = position_dodge(width = 0.9)) +
    geom_errorbar(
        aes(ymin = lci, ymax = uci),
        width = 0.8,
        size = 0.5,
        position = position_dodge(width = 0.9)
    ) +
    geom_vline(aes(xintercept = as.numeric(factor(group_m, levels = ageg_order)) - 0.5), color = "grey") +
    scale_y_continuous(
        breaks = c(0, 2.5, 5, 7.5),
        minor_breaks = c(1.25, 3.75, 6.25),
        limits = c(0, 7.5)
    ) +
    labs(x = NULL, y = "Incidence rate per 1000 person-years", title = "Sepsis") +
    coord_flip(xlim = c(1.1, 5.9)) +
    scale_x_discrete(limits = rev(levels(factor(ageg_order, levels = ageg_order)))) +
    scale_color_manual(
        breaks = levels(factor(birth_y)),
        labels = levels(factor(birth_y)),
        values = c(
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C"
        )
    ) +
    theme(
        legend.title = element_blank(),
        title = element_text(size = 6),
        panel.grid.major.y = element_blank()
    )

p_ipd <-
    lsh_ipd_ppy %>%
    mutate(ir = as.numeric(ir),
           lci = as.numeric(lci),
           uci = as.numeric(uci)) %>%
    ggplot(aes(
        x = factor(group_m, levels = ageg_order),
        y = ir,
        color = factor(
            birth_y,
            levels = rev(levels(factor(birth_y))),
            labels = rev(levels(factor(birth_y)))
        )
    )) +
    geom_point(size = 0.5, position = position_dodge(width = 0.9)) +
    geom_errorbar(
        aes(ymin = lci, ymax = uci),
        width = 0.8,
        size = 0.5,
        position = position_dodge(width = 0.9)
    ) +
    geom_vline(aes(xintercept = as.numeric(factor(group_m, levels = ageg_order)) - 0.5), color = "grey") +
    scale_y_continuous(
        breaks = c(0, 2.5, 5, 7.5),
        minor_breaks = c(1.25, 3.75, 6.25),
        limits = c(0, 7.5)
    ) +
    labs(x = NULL, y = "Incidence rate per 1000 person-years", title = "Invasive Pneumococcal Disease") +
    coord_flip(xlim = c(1.1, 5.9)) +
    scale_x_discrete(limits = rev(levels(factor(ageg_order, levels = ageg_order)))) +
    scale_color_manual(
        breaks = levels(factor(birth_y)),
        labels = levels(factor(birth_y)),
        values = c(
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C"
        )
    ) +
    theme(
        legend.title = element_blank(),
        title = element_text(size = 6),
        panel.grid.major.y = element_blank()
    )

incidence_arranged <-
    ggarrange(
        p_aom,
        p_urti,
        p_pneum,
        p_bronch,
        p_ipd,
        p_sepsis,
        ncol = 2,
        nrow = 3,
        labels = c("A", "B", "C", "D", "E", "F"),
        common.legend = T,
        legend = "none",
        font.label = list(size = 8, family = "sans")
    )

ggsave(
    filename = paste0("_figures/paper_5/", Sys.Date(), "-incidence-arranged.pdf"), 
    plot = incidence_arranged,
    width = 12, height = 14, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_5/", Sys.Date(), "-incidence-arranged.png"), 
    plot = incidence_arranged,
    width = 12, height = 14, units = "cm", dpi = 600)

incidence_facet <- lsh_men_ppy %>%
    mutate(label = "Meningitis") %>%
    union_all(lsh_aom_ppy %>%
                  mutate(label = "Otitis")) %>%
    union_all(lsh_urti_ppy %>%
                  mutate(label = "URTI")) %>%
    union_all(lsh_pneum_ppy %>%
                  mutate(label = "Pneumonia")) %>%
    union_all(lsh_bronch_ppy %>%
                  mutate(label = "LRTI")) %>%
    union_all(lsh_sepsis_ppy %>%
                  mutate(label = "Sepsis")) %>%
    union_all(lsh_ipd_ppy %>%
                  mutate(label = "IPD")) %>%
    mutate(ir = as.numeric(ir),
           lci = as.numeric(lci),
           uci = as.numeric(uci)) %>%
    ggplot(aes(x = factor(birth_y),
        y = ir, 
        color = factor(birth_y))) +
    geom_point(size = 0.5, position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = lci, ymax = uci),
        width = 0.8,
        size = 0.8,
        position = position_dodge(width = 0.9)) +
    facet_grid(label ~ group_m, scales = "free_y") +
    scale_x_discrete(breaks = c("2005", "2007", "2009", "2011", "2013", "2015"))+
    scale_color_manual(breaks = levels(factor(birth_y)),
        labels = levels(factor(birth_y)),
        values = c("#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#E41A1C",
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#377EB8",
            "#377EB8")) +
    labs(x = NULL, y = NULL) +
    theme(axis.text.x = element_text(hjust = 0, vjust = 0.5, angle = 90),
        legend.title = element_blank(),
        legend.position = "none",
        title = element_text(size = 10),
        panel.grid.major.y = element_blank())

ggsave(
    filename = paste0("_figures/paper_5/", Sys.Date(), "-incidence-facet.pdf"), 
    plot = incidence_facet,
    width = 12, height = 14, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_5/", Sys.Date(), "-incidence-facet.png"), 
    plot = incidence_facet,
    width = 12, height = 14, units = "cm", dpi = 600)

### Kaplan-Meier plots ###

km_aom <-
    ggsurvplot(
        survfit(Surv(time = tstop_m, event = visit) ~ group, data = lsh_ag_aom),
        break.x.by = 12,
        fun = function(y)
            (1 - y) * 1000,
        size = 0.3, 
        censor = F, 
        conf.int = T,
        legend = "none",
        title = "A    Otitis Media and Complications",
        font.title = 7,
        font.x = 8,
        font.y = 8,
        font.legend = 8,
        font.tickslab = 8,
        ylab = "Cumulative events",
        xlab = NULL,
        ylim = c(0, 8),
        palette = c("#377EB8", "#E41A1C")
    )

km_urti <-
    ggsurvplot(
        survfit(Surv(time = tstop_m, event = visit) ~ group, data = lsh_ag_urti),
        break.x.by = 12,
        fun = function(y)
            (1 - y) * 1000,
        size = 0.3, 
        censor = F, 
        conf.int = T,
        legend = "none",
        title = "B    Acute Upper Respiratory Tract Infections",
        font.title = 7,
        font.x = 8,
        font.y = 8,
        font.legend = 8,
        font.tickslab = 8,
        ylab = NULL,
        xlab = NULL,
        ylim = c(0, 8),
        palette = c("#377EB8", "#E41A1C")
    )

km_pneum <-
    ggsurvplot(
        survfit(Surv(time = tstop_m, event = visit) ~ group, data = lsh_ag_pneum),
        break.x.by = 12,
        fun = function(y)
            (1 - y) * 1000,
        size = 0.3, 
        censor = F, 
        conf.int = T,
        legend = "none",
        title = "C    Pneumonia",
        font.title = 7,
        font.x = 8,
        font.y = 8,
        font.legend = 8,
        font.tickslab = 8,
        ylab = "Cumulative events",
        xlab = NULL,
        ylim = c(0, 18),
        palette = c("#377EB8", "#E41A1C")
    )

km_bronch <-
    ggsurvplot(
        survfit(Surv(time = tstop_m, event = visit) ~ group, data = lsh_ag_bronch),
        break.x.by = 12,
        fun = function(y)
            (1 - y) * 1000,
        size = 0.3, 
        censor = F, 
        conf.int = T,
        legend = "none",
        title = "D    Acute Lower Respiratory Tract Infections",
        font.title = 7,
        font.x = 8,
        font.y = 8,
        font.legend = 8,
        font.tickslab = 8,
        ylab = NULL,
        xlab = NULL,
        ylim = c(0, 18),
        palette = c("#377EB8", "#E41A1C")
    )

km_ipd <-
    ggsurvplot(
        survfit(Surv(time = tstop_m, event = visit) ~ group, data = lsh_ag_ipd),
        break.x.by = 12,
        fun = function(y)
            (1 - y) * 1000,
        size = 0.3, 
        censor = F, 
        conf.int = T,
        legend = "none",
        title = "E    Invasive Pneumococcal Disease",
        font.title = 7,
        font.x = 8,
        font.y = 8,
        font.legend = 8,
        font.tickslab = 8,
        ylab = "Cumulative events",
        xlab = "Age (months)",
        ylim = c(0, 2),
        palette = c("#377EB8", "#E41A1C")
    )

km_sepsis <-
    ggsurvplot(
        survfit(Surv(time = tstop_m, event = visit) ~ group, data = lsh_ag_sepsis),
        break.x.by = 12,
        fun = function(y)
            (1 - y) * 1000,
        size = 0.3, 
        censor = F, 
        conf.int = T,
        legend = "none",
        title = "F    Sepsis",
        font.title = 7,
        font.x = 8,
        font.y = 8,
        font.legend = 8,
        font.tickslab = 8,
        ylab = NULL,
        xlab = "Age (months)",
        ylim = c(0, 2),
        palette = c("#377EB8", "#E41A1C")
    )

km_list <- list(rep(NA, 6))
km_list[[1]] <- km_aom 
km_list[[4]] <- km_urti
km_list[[2]] <- km_pneum 
km_list[[5]] <- km_bronch 
km_list[[3]] <- km_ipd
km_list[[6]] <- km_sepsis

km_plot <- arrange_ggsurvplots(x = km_list, ncol = 2, nrow = 3)

ggsave(
    filename = paste0("_figures/paper_5/", Sys.Date(), "-kmplot.pdf"), 
    plot = km_plot,
    width = 12, height = 14, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_5/", Sys.Date(), "-kmplot.png"), 
    plot = km_plot,
    width = 12, height = 14, units = "cm", dpi = 600)

save.image(file = paste0("_analyses/paper_5/", Sys.Date(), "-04-2-results-paper5", ".RData"))
