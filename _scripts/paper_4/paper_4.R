# Analysis for paper 4

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
si <- read_rds("_data/results/si.rds")
lsh <- read_rds("_data/results/lsh.rds")
lg <- read_rds("_data/results/lg.rds")
hg <- read_rds("_data/results/hg.rds")
hs <- read_rds("_data/results/hs.rds")
si_lsh <- read_excel("_data/paper_4/Nef-hálskirtlatökur ÁH.xlsx")
lsh_proc <- read_excel("_data/paper_4/lsh_proc.xlsx")
pop <- read_csv2("_data/paper_4/pop_gender_1jan_1980_2017.csv")

### Cleaning the reimbursement database ###
# create observation variable
si <- si %>% mutate(obs = paste0(1:nrow(.), "si"))

# collect physician IDs of anesthetists
anesthetists <- si %>% filter(str_detect(price_code, "Z")) %>% .$phys_id

# remove the duplicate reimbursement rows which are due to anesthetists
si <- 
    si %>%
    anti_join(
        si %>% filter(phys_id %in% anesthetists | phys_id == "973" | phys_id == "3349")
    )

# remove duplicates of the same procedure, within the same month on the same individual
si <- 
    si %>%
    distinct(id, year, month, proc_name, .keep_all = T)

# filter correct birth-cohorts and interval
si <- 
    si %>%
    filter(
        #birth_y >= 2005, birth_y <= 2015,
        year >= 2005, year <= 2016,
        age_y >= 0, age_y <= 4
    )

# filter correct procedures
si <- 
    si %>%
    filter(
        str_detect(proc_name, "rör|Rör"),
        !str_detect(proc_name, "fjarlæg")
    )

### Formatting the population demographics dataset ###
pop <-
    pop %>%
    gather(key = year, value = n, -gender, -age_y) %>%
    filter(year >= 2005, year <= 2016) %>%
    group_by(age_y, year) %>%
    summarise(n = sum(n)) %>%
    ungroup()

### Clean dataset containing procedures performed at LSH ###
si_lsh <-
    si_lsh %>%
    mutate(obs = paste0(1:nrow(.), "lsh")) %>%
    gather(key = proc_num, value = proc_code, -kt, -age_y, -date_proc, -obs) %>%
    left_join(lsh_proc) %>%
    left_join(ids %>% select(kt, id, gender, birth_m, birth_y)) %>%
    mutate(year = year(date_proc), month = month(date_proc), lsh = 1) %>%
    filter(
        proc_code == "DCSA20",
        !is.na(id),
        year >= 2005, year <= 2016,
        age_y >= 0, age_y <= 4
    ) %>%
    select(-kt, -date_proc, -proc_num, -proc_code)

### Join to outpatient procedures ###
si <- 
    si %>% 
    full_join(si_lsh) %>%
    mutate(lsh = ifelse(is.na(lsh), 0, lsh)) %>% 
    distinct()

### Baseline demographics for each individual in the study ###
ids_surv <- 
    ids %>% 
    filter(birth_y >= 2005, birth_y <= 2015, !is.na(birth_date)) %>% 
    left_join(
        hs %>% 
            select(-move_type) %>% 
            group_by(id) %>%
            arrange(id, date_move) %>%
            filter(date_move == first(date_move)) %>% 
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
        age_no_censor = round(as.numeric(date - birth_date)/365.25*12), 
        age = floor(pmin(12*5-1, as.numeric(date - birth_date)/365.25*12)) 
    ) %>%
    filter(age >= 1) %>%
    distinct()

### Age at n = {1, 2, ... n} tympanostomy procedure ###
si_surv <- 
    ids_surv %>%
    select(id, birth_y, birth_m, gender, birth_date) %>%
    left_join(
        si %>% mutate(date_proc = as.Date(paste(year, month, "15", sep = "-"), format = "%Y-%m-%d")),
        by = c("id", "gender", "birth_y", "birth_m")
    ) %>%
    left_join(
        si_lsh %>% mutate(date_proc = as.Date(paste(year, month, "15", sep = "-"), format = "%Y-%m-%d")),
        by = c("id", "gender", "birth_y", "birth_m", "date_proc", "obs")
    ) %>%
    select(id, obs, birth_date, date_proc) %>%
    distinct(id, obs, .keep_all = T) %>%
    mutate(age = floor(pmin(12*5-1, pmax(1, as.numeric(date_proc - birth_date))/(365.25/12)))) %>%
    select(-birth_date, -date_proc) %>%
    filter(age < 12*5) %>% #bara mín gögn
    group_by(id) %>%
    arrange(id, age) %>%
    mutate(
        obs = cummax(as.numeric(factor(obs, levels = unique(obs))))
    ) %>%
    ungroup() %>%
    spread(key = obs, value = age)

### Create correct time at-risk and link to demographic ###
ror_surv <- tmerge(ids_surv, ids_surv, id = id, tstop = ids_surv$age)
ror_surv <- tmerge(ror_surv, si_surv, id = id, proc = event(si_surv$`1`))
ror_surv <- tmerge(ror_surv, si_surv, id = id, proc = event(si_surv$`2`))
ror_surv <- tmerge(ror_surv, si_surv, id = id, proc = event(si_surv$`3`))
ror_surv <- tmerge(ror_surv, si_surv, id = id, proc = event(si_surv$`4`))
ror_surv <- tmerge(ror_surv, si_surv, id = id, proc = event(si_surv$`5`))
ror_surv <- tmerge(ror_surv, si_surv, id = id, proc = event(si_surv$`6`))
ror_surv <- tmerge(ror_surv, si_surv, id = id, proc = event(si_surv$`7`))
ror_surv <- tmerge(ror_surv, si_surv, id = id, proc = event(si_surv$`8`))
ror_surv <- tmerge(ror_surv, ror_surv, id = id, cum_proc = cumtdc(tstop))
ror_surv$group <- ifelse(ror_surv$birth_y <= 2010, "Vaccine non-eligible", "Vaccine-eligible")
ror_surv$gender <- factor(ror_surv$gender)
ror_surv$fctbirth_y <- factor(ror_surv$birth_y)

### Age at n = {1, 2, ... n} antimicrobial prescription ###
lg_surv <- 
    ids_surv %>%
    left_join(
        lg %>%
            filter(grepl("J01", .$atc), age_y <= 4),
        by = c("id")
    ) %>%
    select(id, obs, birth_date, date_presc) %>%
    distinct(id, obs, .keep_all = T) %>%
    mutate(age = floor(pmin(59, pmax(1, as.numeric(as.Date(date_presc) - birth_date))/(365.25/12)))) %>%
    select(-birth_date, -date_presc) %>%
    filter(age <= 59) %>%
    group_by(id) %>%
    arrange(id, age) %>%
    mutate(
        obs = cummax(as.numeric(factor(obs, levels = unique(obs))))
    ) %>%
    ungroup() %>%
    spread(key = obs, value = age) %>%
    as.data.frame()

### Create correct time at-risk for joint analysis of procedures and antimicrobial prescriptions ###
ror_lg_surv <- ror_surv

for(i in 1:(dim(lg_surv)[2] -1)){
    x <- paste(i)
    ror_lg_surv <- tmerge(ror_lg_surv, lg_surv, id = id, presc = event(lg_surv[,x]))
}
ror_lg_surv <- tmerge(ror_lg_surv, subset(ror_lg_surv, presc == 1), id = id, cum_presc = cumtdc(tstop))

ror_lg_surv <- subset(ror_lg_surv, cum_proc == 0)

### Baseline demographics for each individual in the study (48 months of age due to restricted primary care data) ###
ids_surv_hg <- 
    ids %>%
    filter(birth_y >= 2005, birth_y <= 2014, !is.na(birth_date)) %>% 
    left_join(
        hs %>% 
            select(-move_type) %>% 
            group_by(id) %>%
            arrange(id, date_move) %>%
            filter(date_move == first(date_move)) %>% 
            ungroup() %>%
            distinct() 
    ) %>%
    filter(to_from == "move_from_iceland" | is.na(to_from)) %>% 
    mutate(date = as.Date("2015-12-31"), date_move = as.Date(date_move)) %>% 
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
        age_no_censor = round(as.numeric(date - birth_date)/365.25*12), 
        age = floor(pmin(12*4-1, as.numeric(date - birth_date)/365.25*12)) 
    ) %>%
    filter(age >= 1) %>%
    distinct()

### Age at n = {1, 2, ... n} tympanostomy procedure (48 months of age due to restricted primary care data) ###
si_surv_hg <- 
    ids_surv_hg %>%
    select(id, birth_y, birth_m, gender, birth_date) %>%
    left_join(
        si %>% mutate(date_proc = as.Date(paste(year, month, "15", sep = "-"), format = "%Y-%m-%d")),
        by = c("id", "gender", "birth_y", "birth_m")
    ) %>%
    left_join(
        si_lsh %>% mutate(date_proc = as.Date(paste(year, month, "15", sep = "-"), format = "%Y-%m-%d")),
        by = c("id", "gender", "birth_y", "birth_m", "date_proc", "obs")
    ) %>%
    select(id, obs, birth_date, date_proc) %>%
    distinct(id, obs, .keep_all = T) %>%
    mutate(age = floor(pmin(12*4-1, pmax(1, as.numeric(date_proc - birth_date))/(365.25/12)))) %>%
    select(-birth_date, -date_proc) %>%
    filter(age < 12*4) %>% #bara mín gögn
    group_by(id) %>%
    arrange(id, age) %>%
    mutate(
        obs = cummax(as.numeric(factor(obs, levels = unique(obs))))
    ) %>%
    ungroup() %>%
    spread(key = obs, value = age)

### Create correct time at-risk and link to demographic (48 months of age due to restricted primary care data) ###
ror_surv_hg <- tmerge(ids_surv_hg, ids_surv_hg, id = id, tstop = ids_surv_hg$age)
ror_surv_hg <- tmerge(ror_surv_hg, si_surv_hg, id = id, proc = event(si_surv_hg$`1`))
ror_surv_hg <- tmerge(ror_surv_hg, si_surv_hg, id = id, proc = event(si_surv_hg$`2`))
ror_surv_hg <- tmerge(ror_surv_hg, si_surv_hg, id = id, proc = event(si_surv_hg$`3`))
ror_surv_hg <- tmerge(ror_surv_hg, si_surv_hg, id = id, proc = event(si_surv_hg$`4`))
ror_surv_hg <- tmerge(ror_surv_hg, si_surv_hg, id = id, proc = event(si_surv_hg$`5`))
ror_surv_hg <- tmerge(ror_surv_hg, si_surv_hg, id = id, proc = event(si_surv_hg$`6`))
ror_surv_hg <- tmerge(ror_surv_hg, si_surv_hg, id = id, proc = event(si_surv_hg$`7`))
ror_surv_hg <- tmerge(ror_surv_hg, si_surv_hg, id = id, proc = event(si_surv_hg$`8`))
ror_surv_hg <- tmerge(ror_surv_hg, ror_surv_hg, id = id, cum_proc = cumtdc(tstop))
ror_surv_hg$group <- ifelse(ror_surv_hg$birth_y <= 2010, "Vaccine non-eligible", "Vaccine-eligible")
ror_surv_hg$gender <- factor(ror_surv_hg$gender)
ror_surv_hg$fctbirth_y <- factor(ror_surv_hg$birth_y)

### Create dataset of all otitis media visits to primary care and pediatric ED ###
hg <-
    ids_surv %>%
    left_join(
        hg, by = "id"
    )

hg <-
    hg %>%
    filter(
        age_y <= 3,
        birth_y >= 2005, birth_y <= 2014,
        date_contact <= as.Date("2015-12-31"),
        contact_type == "Viðtal" | contact_type == "Annað", staff_type == "Læ",
        str_detect(.$main_diagnosis, "H65|H66|H70|H72") | str_detect(.$other_diagnosis, "H65|H66|H70|H72")
    ) %>%
    select(id, obs, birth_date, date_contact)

lsh <- 
    ids_surv_hg %>% 
    left_join(
        lsh %>% select(-starts_with("cost")),
        by = "id"
    ) %>%
    mutate(age_y = floor(as.numeric(difftime(date_in, birth_date, units = "days"))/365.25)) %>%
    filter(
        age_y <= 3,
        birth_y >= 2005, birth_y <= 2014,
        date_in >= as.Date("2005-01-01"), date_in <= as.Date("2015-12-31"),
        str_detect(.$icd10_code, "H65|H66|H70|H72")
    ) %>%
    mutate(obs = paste0(obs, "lsh")) %>% # to avoid duplicate obs when joining with hg later
    rename(date_contact = date_in) %>%
    mutate(date_contact = as.Date(date_contact)) %>%
    select(id, obs, birth_date, date_contact)

hg_lsh_surv<- 
    hg %>%
    full_join(lsh) %>%
    distinct(id, obs, .keep_all = T) %>%
    mutate(age = floor(pmin(12*4-1, pmax(1, as.numeric(as.Date(date_contact) - birth_date))/(365.25/12)))) %>%
    select(-birth_date, -date_contact) %>%
    filter(age <= 12*4-1) %>%
    group_by(id) %>%
    arrange(id, age) %>%
    mutate(
        obs = cummax(as.numeric(factor(obs, levels = unique(obs))))
    ) %>%
    ungroup() %>%
    spread(key = obs, value = age, sep = "_") %>%
    as.data.frame() #til þess að forloop með tmerge() til að búa til hg_ag virki


### Create correct time at-risk for joint analysis of procedures and otitis media visits ###
ror_hg_lsh_surv <- ror_surv_hg

for(i in 1:(dim(hg_lsh_surv)[2] -1)){
    x <- paste0("obs_", i)
    ror_hg_lsh_surv <- tmerge(ror_hg_lsh_surv, hg_lsh_surv, id = id, visit = event(hg_lsh_surv[,x]))
}
ror_hg_lsh_surv <- tmerge(ror_hg_lsh_surv, subset(ror_hg_lsh_surv, visit == 1), id = id, cum_visit = cumtdc(tstop))
ror_hg_lsh_surv <- subset(ror_hg_lsh_surv, cum_proc == 0)

#######################
#### Data analysis ####
#######################

### Incidence rate and incidence ratio figures ###
ageg_order <- c("0-5","6-11","12-17","18-23","24-29","30-35", "36-41", "42-47", "48-53", "54-59")

npy_vtp <- 
    expand.grid(birth_y = 2005:2015, age_m = 0:59) %>%
    full_join(
        ror_surv %>%
            select(id, birth_y, tstop) %>%
            distinct() %>%
            mutate(tstop = as.integer(tstop)),
        by = "birth_y") %>%
    filter(tstop >= age_m) %>%
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
            .$age_m >= 30 & .$age_m <= 35 ~ "30-35",
            .$age_m >= 36 & .$age_m <= 41 ~ "36-41",
            .$age_m >= 42 & .$age_m <= 47 ~ "42-47",
            .$age_m >= 48 & .$age_m <= 53 ~ "48-53",
            .$age_m >= 54 & .$age_m <= 59 ~ "54-59"
        ), 
        group_m = factor(group_m, levels = ageg_order, ordered = T)
    ) %>% 
    group_by(birth_y, group_m) %>%
    summarise(at_risk = round(sum(at_risk)/12)) %>%
    ungroup() %>%
    filter(at_risk > 5)

vtp_n <-
    ror_surv %>%
    mutate(age_m = as.integer(tstop)) %>%
    group_by(birth_y, age_m) %>%
    summarise(procs = sum(proc)) %>%
    ungroup() %>%
    mutate(group_m =
               cut(
                   age_m,
                   breaks = c(0, 5, 11, 17, 23, 29, 35, 41, 47, 53, 59),
                   labels = c("0-5","6-11","12-17","18-23","24-29","30-35", "36-41", "42-47", "48-53", "54-59"),
                   include.lowest = T
               )
    ) %>%
    mutate(group_m = factor(group_m, levels = ageg_order, ordered = T)) %>%
    group_by(birth_y, group_m) %>%
    summarise(procs = sum(procs)) %>%
    ungroup()

vtp_ppy <-
    npy_vtp %>%
    left_join(vtp_n)

vtp_ppy[which(is.na(vtp_ppy$procs)), "procs"] <- 0

temp <- matrix(NA, ncol = 3, nrow = dim(vtp_ppy)[1])
for(i in 1:dim(vtp_ppy)[1]){
    temp[i,1] <- tidy(poisson.test(vtp_ppy$procs[i], vtp_ppy$at_risk[i]))$estimate*100
    temp[i,2] <- tidy(poisson.test(vtp_ppy$procs[i], vtp_ppy$at_risk[i]))$conf.low*100
    temp[i,3] <- tidy(poisson.test(vtp_ppy$procs[i], vtp_ppy$at_risk[i]))$conf.high*100
}

vtp_ppy <- 
    cbind(vtp_ppy, temp) %>%
    rename(ir = `1`, lci = `2`, uci = `3`) %>%
    mutate(
        ir = format(round(ir, 2), nsmall = 2),
        lci = format(round(lci, 2), nsmall = 2),
        uci = format(round(uci, 2), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(ir, " ", "(", lci, "-", uci, ")"))

vtp_ppy_group <-
    npy_vtp %>%
    left_join(vtp_n)

vtp_ppy_group[which(is.na(vtp_ppy_group$procs)), "procs"] <- 0

vtp_ppy_group <- 
    vtp_ppy_group %>%
    mutate(group = ifelse(birth_y <= 2010, "vnec", "vec")) %>%
    group_by(group, group_m) %>%
    summarise(at_risk = sum(at_risk, na.rm = T), procs = sum(procs, na.rm = T)) %>%
    ungroup()

temp <- matrix(NA, ncol = 3, nrow = dim(vtp_ppy_group)[1])
for(i in 1:dim(vtp_ppy_group)[1]){
    temp[i,1] <- tidy(poisson.test(vtp_ppy_group$procs[i], vtp_ppy_group$at_risk[i]))$estimate*100
    temp[i,2] <- tidy(poisson.test(vtp_ppy_group$procs[i], vtp_ppy_group$at_risk[i]))$conf.low*100
    temp[i,3] <- tidy(poisson.test(vtp_ppy_group$procs[i], vtp_ppy_group$at_risk[i]))$conf.high*100
}

vtp_ppy_group <- 
    cbind(vtp_ppy_group, temp) %>%
    rename(ir = `1`, lci = `2`, uci = `3`) %>%
    mutate(
        ir = format(round(ir, 2), nsmall = 2), 
        lci = format(round(lci, 2), nsmall = 2), 
        uci = format(round(uci, 2), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(ir, " ", "(", lci, "-", uci, ")"))

irr_vtp_group <-
    cbind.data.frame(
        group_m = vtp_ppy_group %>% mutate(group_m = as.character(group_m)) %>% .$group_m %>% unique(),
        vnec_procs = vtp_ppy_group %>% filter(group == "vnec") %>% .$procs,
        vnec_atrisk = vtp_ppy_group %>% filter(group == "vnec") %>% .$at_risk,
        vec_procs = vtp_ppy_group %>% filter(group == "vec") %>% .$procs,
        vec_atrisk = vtp_ppy_group %>% filter(group == "vec") %>% .$at_risk
    )

temp <- matrix(NA, ncol = 4, nrow = dim(irr_vtp_group)[1])
for(i in 1:dim(irr_vtp_group)[1]){
    temp[i,1] <- tidy(poisson.test(c(irr_vtp_group$vec_procs[i], irr_vtp_group$vnec_procs[i]), c(irr_vtp_group$vec_atrisk[i], irr_vtp_group$vnec_atrisk[i])))$estimate
    temp[i,2] <- tidy(poisson.test(c(irr_vtp_group$vec_procs[i], irr_vtp_group$vnec_procs[i]), c(irr_vtp_group$vec_atrisk[i], irr_vtp_group$vnec_atrisk[i])))$conf.low
    temp[i,3] <- tidy(poisson.test(c(irr_vtp_group$vec_procs[i], irr_vtp_group$vnec_procs[i]), c(irr_vtp_group$vec_atrisk[i], irr_vtp_group$vnec_atrisk[i])))$conf.high
    temp[i,4] <- tidy(poisson.test(c(irr_vtp_group$vec_procs[i], irr_vtp_group$vnec_procs[i]), c(irr_vtp_group$vec_atrisk[i], irr_vtp_group$vnec_atrisk[i])))$p.value
}

irr_vtp_group <- 
    cbind(irr_vtp_group, temp) %>%
    rename(irr_vtp_group = `1`, lci = `2`, uci = `3`, pvalue = `4`) %>%
    mutate(
        irr_vtp_group = format(round(irr_vtp_group, 2), nsmall = 2),
        lci = format(round(lci, 2), nsmall = 2),
        uci = format(round(uci, 2), nsmall = 2),
        pvalue = format(round(pvalue, 6), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(irr_vtp_group, " ", "(", lci, "-", uci, ")"))

birth_y <- 2005:2015

incidence_fig <- vtp_ppy %>%
    left_join(
        irr_vtp_group %>%
            select(group_m, ir_ci2 = ir_ci) %>%
            mutate(ir_ci2 = paste0("IRR", " ", .$ir_ci2), birth_y = 2010),
        by = c("group_m", "birth_y")
    ) %>%
    mutate(ir = as.numeric(ir), lci = as.numeric(lci), uci = as.numeric(uci)) %>%
    ggplot(
        aes(
            x = factor(group_m, levels = ageg_order),
            y = ir,
            color = factor(birth_y, levels = rev(levels(factor(birth_y))), labels = rev(levels(factor(birth_y))))
        )
    ) +
    geom_point(position = position_dodge(width = 0.9), size = 0.3) +
    geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.8, position = position_dodge(width = 0.9)) +
    geom_text(aes(y = 40, label = ir_ci2), size = 2.7, color = "black") +
    geom_vline(aes(xintercept = as.numeric(factor(group_m, levels = ageg_order)) - 0.5), color = "grey") +
    scale_y_continuous(breaks = c(0,10,20,30), minor_breaks = c(5, 15, 25), limits = c(0, 48)) +
    labs(x = "Age-groups (months)", y = "Incidence rate per 100 person-years") +
    coord_flip(xlim = c(1.1,9.9)) +
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
    ) +    theme(
        legend.title = element_blank(),
        panel.grid.major.y = element_blank()
    )

ggsave(
    filename = paste0("_figures/paper_4/", Sys.Date(), "-incidence.pdf"), 
    plot = incidence_fig,
    width = 12, height = 8, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_4/", Sys.Date(), "-incidence.png"), 
    plot = incidence_fig,
    width = 12, height = 8, units = "cm", dpi = 600)

### Cumulative event graph for tympanostomy tube procedures ###
cumulative_event <- ggsurvplot(
    survfit(
        Surv(tstop, proc) ~ group,
        data = ror_surv %>% filter(cum_proc == 0) %>% mutate(
            group = ifelse(
                birth_y <= 2010,
                "Vaccine non-eligible cohort",
                "Vaccine-eligible cohort"
            )
        )
    ),
    palette = c("#E41A1C", "#377EB8"),
    conf.int = TRUE,
    size = 0.3,
    censor = F,
    ylim = c(0, 0.35),
    break.x.by = 6,
    fun = "event",
    xlab = "Age (months)",
    ylab = "Cumulative proportion",
    legend = c(0.75, 0.25),
    legend.title = element_blank(),
    legend.labs = c("Vaccine non-eligible cohort", "Vaccine-eligible cohort"),
    ggtheme = theme_bw() %+replace% theme(
        text =
            element_text(
                family = "sans",
                face = "plain",
                colour = "black",
                size = 10,
                hjust = 0.5,
                vjust = 0.5,
                angle = 0,
                lineheight = 0.9,
                margin = margin(0),
                debug = FALSE
            ),
        legend.background = element_rect(
            fill = "transparent",
            colour = NA,
            size = NULL,
            linetype = NULL,
            inherit.blank = NULL
        )
    )
)

ggsave(
    filename = paste0("_figures/paper_4/", Sys.Date(), "-cumulative-event.pdf"), 
    plot = cumulative_event $plot,
    width = 12, height = 8, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_4/", Sys.Date(), "-cumulative-event.png"), 
    plot = cumulative_event $plot ,
    width = 12, height = 8, units = "cm", dpi = 600)

### Cox regression analysis of survival until first tympanostomy procedure ###
dd <- with(ror_surv, datadist(gender, fctbirth_y))
options(datadist = "dd")
cox_ror <- cph(Surv(tstart, tstop, proc) ~ cluster(id) + strat(gender) + fctbirth_y, data = subset(ror_surv, cum_proc == 0))

hazardratio_base <- cbind.data.frame(
    fctbirth_y = c(as.character(2005:2009), as.character(2011:2015)),
    summary(cox_ror, fctbirth_y = "2010")[seq(2, 20, 2), c(4, 6, 7)]) %>%
    full_join(data.frame(fctbirth_y = "2010 \n(Reference)", Effect = 1)) %>%
    mutate(fctbirth_y = factor(
        fctbirth_y,
        levels = c("2015", "2014", "2013", "2012", "2011", "2010 \n(Reference)", "2009", "2008", "2007", "2006", "2005"))) %>%
    ggplot(data = ., aes(x = fctbirth_y, y = Effect)) + geom_hline(aes(yintercept = 1), lty = 2) + 
    geom_point() +
    geom_errorbar(aes(ymin = `Lower 0.95`, ymax = `Upper 0.95`), width = 0.2) +
    coord_flip(ylim = c(0.75,1.32)) +
    scale_y_log10(breaks = c(0.8, 0.9, 1, 1.1, 1.2, 1.3)) +
    labs(x = NULL, y = "Hazard ratio")

ggsave(
    filename = paste0("_figures/paper_4/", Sys.Date(), "-hazardratio-base.pdf"), 
    plot = hazardratio_base,
    width = 12, height = 8, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_4/", Sys.Date(), "-hazardratio-base.png"), 
    plot = hazardratio_base,
    width = 12, height = 8, units = "cm", dpi = 600)

### Cox regression analysis of survival until first tympanostomy procedure with antimicrobial prescriptions as covariate ###
dd <- with(ror_lg_surv, datadist(gender, fctbirth_y, cum_presc))
options(datadist = "dd")
dd$limits$cum_presc[1] <- 0
dd$limits$cum_presc[2] <- 1
dd$limits$cum_presc[3] <- 1
cox_lg <- cph(Surv(tstart, tstop, proc) ~ cluster(id) + strat(gender) + fctbirth_y + rcs(cum_presc), data = ror_lg_surv)

hazardratio_presc <- cbind.data.frame(
    fctbirth_y = c(as.character(2005:2009), as.character(2011:2015)),
    summary(cox_lg, cum_presc = 0, fctbirth_y = "2010")[seq(4, 22, 2), c(4, 6, 7)]) %>%
    full_join(data.frame(fctbirth_y = "2010 \n(Reference)", Effect = 1)) %>%
    mutate(fctbirth_y = factor(
        fctbirth_y,
        levels = c("2015", "2014", "2013", "2012", "2011", "2010 \n(Reference)", "2009", "2008", "2007", "2006", "2005"))) %>%
    ggplot(data = ., aes(x = fctbirth_y, y = Effect)) + geom_hline(aes(yintercept = 1), lty = 2) + 
    geom_point() +
    geom_errorbar(aes(ymin = `Lower 0.95`, ymax = `Upper 0.95`), width = 0.2) +
    coord_flip(ylim = c(0.65,1.55)) +
    scale_y_log10(breaks = c(0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5)) +
    labs(x = NULL, y = "Hazard ratio")

ggsave(
    filename = paste0("_figures/paper_4/", Sys.Date(), "-hazardratio-presc.pdf"), 
    plot = hazardratio_presc,
    width = 12, height = 8, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_4/", Sys.Date(), "-hazardratio-presc.png"), 
    plot = hazardratio_presc,
    width = 12, height = 8, units = "cm", dpi = 600)

### Cox regression analysis of survival until first tympanostomy procedure with otitis media visits as a covariate ###
dd <- with(ror_hg_lsh_surv, datadist(gender, fctbirth_y, cum_visit))
dd$limits$cum_visit[3] <- 1
options(datadist = "dd")
cox_hg_lsh <- cph(Surv(tstart, tstop, proc) ~ cluster(id) + strat(gender) + fctbirth_y + rcs(cum_visit), data = ror_hg_lsh_surv)

hazardratio_visits <- cbind.data.frame(
    fctbirth_y = c(as.character(2005:2009), as.character(2011:2014)),
    summary(cox_hg_lsh, cum_visit = 0, fctbirth_y = "2010")[seq(4, 20, 2), c(4, 6, 7)]) %>%
    full_join(data.frame(fctbirth_y = "2010 \n(Reference)", Effect = 1)) %>%
    mutate(fctbirth_y = factor(
        fctbirth_y,
        levels = c("2014", "2013", "2012", "2011", "2010 \n(Reference)", "2009", "2008", "2007", "2006", "2005"))) %>%
    ggplot(data = ., aes(x = fctbirth_y, y = Effect)) + geom_hline(aes(yintercept = 1), lty = 2) + 
    geom_point() +
    geom_errorbar(aes(ymin = `Lower 0.95`, ymax = `Upper 0.95`), width = 0.2) +
    coord_flip(ylim = c(0.7,1.42)) +
    scale_y_log10(breaks = c(0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4)) +
    labs(x = NULL, y = "Hazard ratio")

ggsave(
    filename = paste0("_figures/paper_4/", Sys.Date(), "-hazardratio-visits.pdf"), 
    plot = hazardratio_visits,
    width = 12, height = 8, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_4/", Sys.Date(), "-hazardratio-visits.png"), 
    plot = hazardratio_visits,
    width = 12, height = 8, units = "cm", dpi = 600)

### Combined plot ### 
combined_plot1 <- cbind.data.frame(
    fctbirth_y = c(as.character(2005:2009), as.character(2011:2015)),
    summary(cox_ror, fctbirth_y = "2010")[seq(2, 20, 2), c(4, 6, 7)]) %>%
    full_join(data.frame(fctbirth_y = "Ref", Effect = 1)) %>%
    mutate(fctbirth_y = factor(
        fctbirth_y,
        levels = c("2015", "2014", "2013", "2012", "2011", "Ref", "2009", "2008", "2007", "2006", "2005"))) %>%
    ggplot(data = ., aes(x = fctbirth_y, y = Effect)) + geom_hline(aes(yintercept = 1), lty = 2) + 
    geom_point() +
    geom_errorbar(aes(ymin = `Lower 0.95`, ymax = `Upper 0.95`), width = 0.2) +
    coord_flip(ylim = c(0.65,1.55)) +
    scale_y_log10(breaks = c(0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5)) +
    labs(title = "     Unadjusted hazard ratio", x = NULL, y = NULL) +
    theme(text = element_text(size = 8))

combined_plot2 <- cbind.data.frame(
    fctbirth_y = c(as.character(2005:2009), as.character(2011:2015)),
    summary(cox_lg, cum_presc = 0, fctbirth_y = "2010")[seq(4, 22, 2), c(4, 6, 7)]) %>%
    full_join(data.frame(fctbirth_y = "Ref", Effect = 1)) %>%
    mutate(fctbirth_y = factor(
        fctbirth_y,
        levels = c("2015", "2014", "2013", "2012", "2011", "Ref", "2009", "2008", "2007", "2006", "2005"))) %>%
    ggplot(data = ., aes(x = fctbirth_y, y = Effect)) + geom_hline(aes(yintercept = 1), lty = 2) + 
    geom_point() +
    geom_errorbar(aes(ymin = `Lower 0.95`, ymax = `Upper 0.95`), width = 0.2) +
    coord_flip(ylim = c(0.65,1.55)) +
    scale_y_log10(breaks = c(0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5)) +
    labs(title = "     Hazard ratio adjusted for antimicrobial prescriptions", x = NULL, y = NULL) +
    theme(text = element_text(size = 8))

combined_plot3  <- cbind.data.frame(
    fctbirth_y = c(as.character(2005:2009), as.character(2011:2014)),
    summary(cox_hg_lsh, cum_visit = 0, fctbirth_y = "2010")[seq(4, 20, 2), c(4, 6, 7)]) %>%
    full_join(data.frame(fctbirth_y = "Ref", Effect = 1)) %>%
    mutate(fctbirth_y = factor(
        fctbirth_y,
        levels = c("2014", "2013", "2012", "2011", "Ref", "2009", "2008", "2007", "2006", "2005"))) %>%
    ggplot(data = ., aes(x = fctbirth_y, y = Effect)) + geom_hline(aes(yintercept = 1), lty = 2) + 
    geom_point() +
    geom_errorbar(aes(ymin = `Lower 0.95`, ymax = `Upper 0.95`), width = 0.2) +
    coord_flip(ylim = c(0.65,1.55)) +
    scale_y_log10(breaks = c(0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5)) +
    labs(title = "     Hazard ratio adjusted for otitis media associated visits", x = NULL, y = "Hazard ratio") +
    theme(text = element_text(size = 8))

combined_hazard <- 
    ggarrange(
        labels = c("A", "B", "C"),
        plotlist = list(combined_plot1, combined_plot2, combined_plot3),
        font.label = list(size = 8, family = "sans"),
        nrow = 3)

ggsave(
    filename = paste0("_figures/paper_4/", Sys.Date(), "-combined-hazard.pdf"), 
    plot = combined_hazard,
    width = 12, height = 14, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_4/", Sys.Date(), "-combined-hazard.png"), 
    plot = combined_hazard,
    width = 12, height = 14, units = "cm", dpi = 600)

### Preperation for table in thesis ###

chi_lg <- 
    ror_lg_surv %>%
    filter(proc == 1, birth_y <= 2011) %>%
    count(group, cum_presc) %>%
    right_join(
        ror_lg_surv %>%
            filter(proc == 1, birth_y <= 2011) %>%
            count(group, cum_presc) %>%
            expand(group, cum_presc)
    ) %>%
    mutate(
        group = ifelse(group == "Vaccine non-eligible", "xNon-vaccine eligbile", group),
        cum_presc_agg = case_when(
            cum_presc == 0 ~ "0",
            cum_presc == 1 ~ "1",
            cum_presc == 2 ~ "2",
            cum_presc >= 3 & cum_presc <= 4 ~ "3-4",
            cum_presc >= 5 & cum_presc <= 7 ~ "5-7",
            cum_presc >= 8 ~ "8+"
        )
    ) %>%
    group_by(group, cum_presc_agg) %>%
    summarise(n = sum(n, na.rm = T)) %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    mutate(non = sum(n) -n) %>% 
    ungroup() %>%
    gather(key = exp, value = count, -group, -cum_presc_agg) %>%
    xtabs(data = ., count ~ group + exp +cum_presc_agg, na.action(na.pass)) %>%
    apply(MARGIN = 3, FUN = prop.test, conf.level = 0.95) %>%
    unlist() %>%
    tidy() %>%
    separate(col = names, sep = "\\.", into = c("cum_presc_agg", "name"), extra = "merge" ) %>%
    mutate(x = as.numeric(x)) %>%
    filter(!is.na(x)) %>%
    spread(key = name, value = x) %>%
    mutate(
        `estimate.prop 1` = `estimate.prop 1`*100,
        `estimate.prop 2` = `estimate.prop 2`*100,
        conf.int1 = conf.int1*100, 
        conf.int2 = conf.int2*100,
        diff = `estimate.prop 1` - `estimate.prop 2`,
        cum_presc = as.numeric(cum_presc_agg)
    ) %>%
    arrange(cum_presc_agg) %>%
    transmute(
        cumulative = cum_presc_agg,
        presc_ARD = paste0(
            format(signif(diff, digits = 3), digits = 2, trim = T, nsmall = 2),
            " (",
            format(signif(conf.int1, digits = 3), digits = 2, trim = T, nsmall = 2),
            " to ",
            format(signif(conf.int2, digits = 3), digits = 2, trim = T, nsmall = 2),
            ")"
        )
    )

rr_lg <- 
    ror_lg_surv %>%
    filter(proc == 1, birth_y <= 2011) %>%
    count(group, cum_presc) %>%
    right_join(
        ror_lg_surv %>%
            filter(proc == 1, birth_y <= 2011) %>%
            count(group, cum_presc) %>%
            expand(group, cum_presc)
    ) %>%
    mutate(
        group = ifelse(group == "Vaccine non-eligible", "xNon-vaccine eligbile", group),
        cum_presc_agg = case_when(
            cum_presc == 0 ~ "0",
            cum_presc == 1 ~ "1",
            cum_presc == 2 ~ "2",
            cum_presc >= 3 & cum_presc <= 4 ~ "3-4",
            cum_presc >= 5 & cum_presc <= 7 ~ "5-7",
            cum_presc >= 8 ~ "8+"
        )
    ) %>%
    group_by(group, cum_presc_agg) %>%
    summarise(n = sum(n, na.rm = T)) %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    mutate(non = sum(n) -n) %>% 
    ungroup() %>%
    gather(key = exp, value = count, -group, -cum_presc_agg) %>%
    xtabs(data = ., count ~ group + exp +cum_presc_agg, na.action(na.pass)) %>%
    apply(MARGIN = 3, FUN = riskratio, conf.level = 0.95, rev = "both") %>%
    unlist() %>%
    tidy() %>%
    separate(col = names, sep = "\\.", into = c("cum_presc_agg", "name"), extra = "merge" ) %>%
    mutate(x = as.numeric(x)) %>%
    filter(!is.na(x)) %>%
    spread(key = name, value = x) %>%
    transmute(
        cumulative = cum_presc_agg,
        presc_VNEC = paste0(
            format(signif(data4/data7 * 100, digits = 3), digits = 2, trim = T, nsmall = 2),
            " (",
            format(signif(data4, digits = 3), trim = T, big.mark = ","),
            ")"
        ),
        presc_VEC = paste0(
            format(signif(data5/data8 * 100, digits = 3), digits = 2, trim = T, nsmall = 2),
            " (",
            format(signif(data5, digits = 3), trim = T, big.mark = ","),
            ")"
        ),
        presc_RR = paste0(
            format(signif(measure4, digits = 3), digits = 2, trim = T,  nsmall = 2),
            " (",
            format(signif(measure2, digits = 3), digits = 2, trim = T, nsmall = 2),
            " to ",
            format(signif(measure6, digits = 3), digits = 2, trim = T, nsmall = 2),
            ")"
        )
    )

chi_lsh <- 
    ror_hg_lsh_surv %>%
    filter(proc == 1, birth_y <= 2011) %>%
    count(group, cum_visit) %>%
    right_join(
        ror_hg_lsh_surv %>%
            filter(proc == 1, birth_y <= 2011) %>%
            count(group, cum_visit) %>%
            expand(group, cum_visit)
    ) %>%
    mutate(
        group = ifelse(group == "Vaccine non-eligible", "xNon-vaccine eligbile", group),
        cum_visit_agg = case_when(
            cum_visit == 0 ~ "0",
            cum_visit == 1 ~ "1",
            cum_visit == 2 ~ "2",
            cum_visit >= 3 & cum_visit <= 4 ~ "3-4",
            cum_visit >= 5 & cum_visit <= 7 ~ "5-7",
            cum_visit >= 8 ~ "8+"
        )
    ) %>%
    group_by(group, cum_visit_agg) %>%
    summarise(n = sum(n, na.rm = T)) %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    mutate(non = sum(n) -n) %>% 
    ungroup() %>%
    gather(key = exp, value = count, -group, -cum_visit_agg) %>%
    xtabs(data = ., count ~ group + exp +cum_visit_agg, na.action(na.pass)) %>%
    apply(MARGIN = 3, FUN = prop.test, conf.level = 0.95) %>%
    unlist() %>%
    tidy() %>%
    separate(col = names, sep = "\\.", into = c("cum_visit_agg", "name"), extra = "merge" ) %>%
    mutate(x = as.numeric(x)) %>%
    filter(!is.na(x)) %>%
    spread(key = name, value = x) %>%
    mutate(
        `estimate.prop 1` = `estimate.prop 1`*100,
        `estimate.prop 2` = `estimate.prop 2`*100,
        conf.int1 = conf.int1*100, 
        conf.int2 = conf.int2*100,
        diff = `estimate.prop 1` - `estimate.prop 2`,
        cum_visit = as.numeric(cum_visit_agg)
    ) %>%
    arrange(cum_visit_agg) %>%
    transmute(
        cumulative = cum_visit_agg,
        visit_ARD = paste0(
            format(signif(diff, digits = 3), digits = 2, trim = T, nsmall = 2),
            " (",
            format(signif(conf.int1, digits = 3), digits = 2, trim = T, nsmall = 2),
            " to ",
            format(signif(conf.int2, digits = 3), digits = 2, trim = T, nsmall = 2),
            ")"
        )
    )

rr_lsh <- 
    ror_hg_lsh_surv %>%
    filter(proc == 1, birth_y <= 2011) %>%
    count(group, cum_visit) %>%
    right_join(
        ror_hg_lsh_surv %>%
            filter(proc == 1, birth_y <= 2011) %>%
            count(group, cum_visit) %>%
            expand(group, cum_visit)
    ) %>%
    mutate(
        group = ifelse(group == "Vaccine non-eligible", "xNon-vaccine eligbile", group),
        cum_visit_agg = case_when(
            cum_visit == 0 ~ "0",
            cum_visit == 1 ~ "1",
            cum_visit == 2 ~ "2",
            cum_visit >= 3 & cum_visit <= 4 ~ "3-4",
            cum_visit >= 5 & cum_visit <= 7 ~ "5-7",
            cum_visit >= 8 ~ "8+"
        )
    ) %>%
    group_by(group, cum_visit_agg) %>%
    summarise(n = sum(n, na.rm = T)) %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    mutate(non = sum(n) -n) %>% 
    ungroup() %>%
    gather(key = exp, value = count, -group, -cum_visit_agg) %>%
    xtabs(data = ., count ~ group + exp +cum_visit_agg, na.action(na.pass)) %>%
    apply(MARGIN = 3, FUN = riskratio, conf.level = 0.95, rev = "both") %>%
    unlist() %>%
    tidy() %>%
    separate(col = names, sep = "\\.", into = c("cum_visit_agg", "name"), extra = "merge" ) %>%
    mutate(x = as.numeric(x)) %>%
    filter(!is.na(x)) %>%
    spread(key = name, value = x) %>%
    transmute(
        cumulative = cum_visit_agg,
        visit_VNEC = paste0(
            format(signif(data4/data7 * 100, digits = 3), digits = 2, trim = T, nsmall = 2),
            " (",
            format(signif(data4, digits = 3), trim = T, big.mark = ","),
            ")"
        ),
        visit_VEC = paste0(
            format(signif(data5/data8 * 100, digits = 3), digits = 2, trim = T, nsmall = 2),
            " (",
            format(signif(data5, digits = 3), trim = T, big.mark = ","),
            ")"
        ),
        visit_RR = paste0(
            format(signif(measure4, digits = 3), digits = 2, trim = T,  nsmall = 2),
            " (",
            format(signif(measure2, digits = 3), digits = 2, trim = T, nsmall = 2),
            " to ",
            format(signif(measure6, digits = 3), digits = 2, trim = T, nsmall = 2),
            ")"
        )
    )

save.image(file = paste0("_analyses/paper_4/", Sys.Date(), "-04-2-results-paper4", ".RData"))
