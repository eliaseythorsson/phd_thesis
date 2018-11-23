# Analysis for paper I

library(tidyverse)
library(lubridate)
library(zoo)
library(epiR)

# set theme for ggplot2 figures
theme_set(theme_bw(base_size = 10, base_family = "sans"))

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

# Figure of number of visits
monthly_visits <- bmb_komur %>%
    ggplot(aes(x = ym, y = n)) +
    geom_line() +
    geom_vline(aes(xintercept = as.yearmon("2008-01-01")), lty = 2) +
    geom_vline(aes(xintercept = as.yearmon("2015-01-01")), lty = 2) +
    ylim(0, NA) +
    scale_x_yearmon(format = "%Y", n = length(2002:2016)) +
    labs(y = "Monthly number of visits", x = NULL) +
    theme(axis.text.x = element_text(angle = 90))

ggsave(
    filename = paste0("_figures/paper_I/", Sys.Date(), "-monthly-visits.pdf"), 
    plot = monthly_visits,
    width = 12, height = 8, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_I/", Sys.Date(), "-monthly-visits.png"), 
    plot = monthly_visits,
    width = 12, height = 8, units = "cm", dpi = 600)

# Figure 1 in publication
ceftriaxone_fig1 <- ceftriaxone %>%
    filter(date <= as.Date("2015-12-31")) %>%
    mutate(ym = as.yearqtr(date)) %>%
    filter(age_y <= 3) %>%
    expand(ym, age_y) %>% # Creates grid for each combination of date and age
    left_join( # Adds otits media episodes per year-quarter and age
        ceftriaxone %>%
            unite(col = all_diagnoses, DIA_1:DIA_6) %>%
            filter(
                str_detect(string = all_diagnoses, pattern = "H65|H66"), # only otitis media
                is.na(days_since_last) | days_since_last > 14, # only "new episodes"
                age_y <= 3 
            ) %>%
            mutate(ym = as.yearqtr(date)) %>%
            group_by(ym, age_y) %>%
            summarise(n_aom = n_distinct(obs))
    ) %>%
    left_join( # Adds pneumonia episodes per year-quarter and age
        ceftriaxone %>%
            unite(col = all_diagnoses, DIA_1:DIA_6) %>%
            filter(
                str_detect(string = all_diagnoses, pattern = "J15|J18"), # only pneumonia
                is.na(days_since_last) | days_since_last > 14, # only "new episodes"
                age_y <= 3 
            ) %>%
            mutate(ym = as.yearqtr(date)) %>%
            group_by(ym, age_y) %>%
            summarise(n_pneumonia = n_distinct(obs))
    ) %>%
    left_join( # Adds all episodes other than otitis media and pneumonia
        ceftriaxone %>%
            unite(col = all_diagnoses, DIA_1:DIA_6) %>%
            filter(
                !str_detect(string = all_diagnoses, pattern = "H65|H66|J15|J18"), # Everything except OM/Pneumonia
                is.na(days_since_last) | days_since_last > 14, # only "new episodes"
                age_y <= 3 
            ) %>%
            mutate(ym = as.yearqtr(date)) %>%
            group_by(ym, age_y) %>%
            summarise(n_other = n_distinct(obs))
    ) %>%
    left_join(
        ceftriaxone %>%
            mutate(year = year(date)) %>%
            left_join(
                born_100km %>%
                    group_by(age_y, year) %>%
                    summarise(n_total = sum(n, na.rm = T))
                ) %>%
            mutate(ym = as.yearqtr(date),
                   n_total = n_total / 4) %>%
            select(ym, age_y, n_total) %>%
            distinct()
    ) %>%
    transmute(
        "Otitis~media" = ((n_aom/n_total) * 1000),
        "Pneumonia" = ((n_pneumonia/n_total) *1000),
        "Other" = ((n_other/n_total) * 1000),
        age_y = age_y, 
        ym = ym
    ) %>%
    gather(key = indication, value = n, -ym, -age_y) %>%
    mutate(
        n = if_else(is.na(n), 0, n),
        indication = factor(indication, levels = c("Otitis~media", "Pneumonia", "Other"))
    ) %>%
    ggplot(aes(x = ym, y = n, lty = factor(age_y))) +
    geom_line() +
    facet_wrap(~indication, labeller = label_parsed, nrow = 3, scales = "free_y") +
    scale_y_continuous(limits = c(0, NA)) +
    scale_x_yearqtr(format = "%Y", n = 9) +
    labs(x = NULL, y = "Ceftriaxone treatment episodes \nper 1000 person-years") +
    theme(legend.title = element_blank(), legend.position = "bottom")

ggsave(
    filename = paste0("_figures/paper_I/", Sys.Date(), "-figure1-ceftriaxone-2008-2015.pdf"), 
    plot = ceftriaxone_fig1,
    width = 12, height = 8, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_I/", Sys.Date(), "-figure1-ceftriaxone-2008-2015.png"), 
    plot = ceftriaxone_fig1,
    width = 12, height = 8, units = "cm", dpi = 600)

### AOM visits ###

komur_2by2 <- komur %>%
    filter(
        is.na(days_since_last) | days_since_last > 14,
        age_y >= 0, age_y <= 3,
        date <= as.Date("2015-12-31")) %>%
    mutate(group = if_else(date <= as.Date("2011-12-31"), "Pre-vaccine", "Post-vaccine")) %>%
    count(age_y, group) %>%
    left_join(
        born_100km %>%
            filter(year >= 2008, year <= 2015) %>%
            mutate(group = if_else(year <= 2011, "Pre-vaccine", "Post-vaccine")) %>%
            group_by(age_y, group) %>%
            summarise(n_kom = sum(n, na.rm = T))) %>%
    gather(key = type, value = count, n, n_kom) %>%
    xtabs(count ~ group + type + age_y, data = .) %>%
    epi.2by2(dat = ., method = "cohort.time")

### Ceftriaxone treatment episodes for AOM ###

cef_aom_2by2 <- ceftriaxone %>%
    filter(date <= as.Date("2015-12-31"), age_y <= 3) %>%
    mutate(group = if_else(date <= as.Date("2011-12-31"), "Pre-vaccine", "Post-vaccine")) %>%
    unite(col = all_diagnoses, DIA_1:DIA_6) %>%
    filter(
        str_detect(string = all_diagnoses, pattern = "H65|H66"), # only AOM
        !str_detect(string = all_diagnoses, pattern = "J15|J18"), # no pneumonia
        is.na(days_since_last) | days_since_last > 14) %>% # only "new episodes"
    group_by(group, age_y) %>%
    summarise(n_aom = n_distinct(obs)) %>%
    left_join(
        born_100km %>%
            filter(year >= 2008, year <= 2015) %>%
            mutate(group = if_else(year <= 2011, "Pre-vaccine", "Post-vaccine")) %>%
            group_by(age_y, group) %>%
            summarise(n_kom = sum(n, na.rm = T))) %>%
    gather(key = type, value = count, n_aom, n_kom) %>%
    xtabs(count ~ group + type + age_y, data = .) %>%
    epi.2by2(dat = ., method = "cohort.time")

### Risk of ceftriaxone if presenting to Childrens Hospital Iceland with AOM ###

cef_risk_aom_2by2 <- ceftriaxone %>%
    filter(date <= as.Date("2015-12-31"), age_y <= 3) %>%
    mutate(group = if_else(date <= as.Date("2011-12-31"), "Pre-vaccine", "Post-vaccine")) %>%
    unite(col = all_diagnoses, DIA_1:DIA_6) %>%
    filter(
        str_detect(string = all_diagnoses, pattern = "H65|H66"), # only AOM
        !str_detect(string = all_diagnoses, pattern = "J15|J18"), # no pneumonia
        is.na(days_since_last) | days_since_last > 14) %>% # only "new episodes"
    group_by(group, age_y) %>%
    summarise(n_aom = n_distinct(obs)) %>%
    left_join(
        komur %>%
            filter(
                is.na(days_since_last) | days_since_last > 14,
                age_y >= 0, age_y <= 3,
                date <= as.Date("2015-12-31")) %>%
            mutate(group = if_else(date <= as.Date("2011-12-31"), "Pre-vaccine", "Post-vaccine")) %>%
            count(age_y, group) %>%
            rename(zn = n) # to secure its position alphabetically secondary to n_aom
    ) %>%
    gather(key = type, value = count, n_aom, zn) %>%
    xtabs(count ~ group + type + age_y, data = .) %>%
    epi.2by2(dat = ., method = "cohort.count")

### Ceftriaxone treatment episodes for pneumonia ###

cef_pneum_2by2 <- ceftriaxone %>%
    filter(date <= as.Date("2015-12-31"), age_y <= 3) %>%
    mutate(group = if_else(date <= as.Date("2011-12-31"), "Pre-vaccine", "Post-vaccine")) %>%
    unite(col = all_diagnoses, DIA_1:DIA_6) %>%
    filter(
        str_detect(string = all_diagnoses, pattern = "J15|J18"), # only pneumonia
        is.na(days_since_last) | days_since_last > 14) %>% # only "new episodes"
    group_by(group, age_y) %>%
    summarise(n_pneum = n_distinct(obs)) %>%
    left_join(
        born_100km %>%
            filter(year >= 2008, year <= 2015) %>%
            mutate(group = if_else(year <= 2011, "Pre-vaccine", "Post-vaccine")) %>%
            group_by(age_y, group) %>%
            summarise(zn = sum(n, na.rm = T))) %>% # to secure its position alphabetically secondary to n_pneum
    gather(key = type, value = count, n_pneum, zn) %>%
    xtabs(count ~ group + type + age_y, data = .) %>%
    epi.2by2(dat = ., method = "cohort.time")

### Ceftriaxone treatment episodes for other indications ###

cef_other_2by2 <- ceftriaxone %>%
    filter(date <= as.Date("2015-12-31"), age_y <= 3) %>%
    mutate(group = if_else(date <= as.Date("2011-12-31"), "Pre-vaccine", "Post-vaccine")) %>%
    unite(col = all_diagnoses, DIA_1:DIA_6) %>%
    filter(
        !str_detect(string = all_diagnoses, pattern = "H65|H66|J15|J18"), # not aom or pneumonia
        is.na(days_since_last) | days_since_last > 14) %>% # only "new episodes"
    group_by(group, age_y) %>%
    summarise(n_pneum = n_distinct(obs)) %>%
    left_join(
        born_100km %>%
            filter(year >= 2008, year <= 2015) %>%
            mutate(group = if_else(year <= 2011, "Pre-vaccine", "Post-vaccine")) %>%
            group_by(age_y, group) %>%
            summarise(zn = sum(n, na.rm = T))) %>% # to secure its position alphabetically secondary to n_pneum
    gather(key = type, value = count, n_pneum, zn) %>%
    xtabs(count ~ group + type + age_y, data = .) %>%
    epi.2by2(dat = ., method = "cohort.time")

### Ceftriaxone treatment episodes regardless of age for children of all ages ###

cef_all_ages_2by2 <- ceftriaxone %>%
    filter(date <= as.Date("2015-12-31")) %>%
    mutate(
        group = if_else(date <= as.Date("2011-12-31"), "Pre-vaccine", "Post-vaccine"),
        age_group = factor(case_when(
            age_y >= 0 & age_y <= 3 ~ "0-3 years of age",
            age_y >= 4 & age_y <= 7 ~ "4-7 years of age",
            age_y >= 8 & age_y <= 11 ~ "8-11 years of age",
            age_y >= 12 & age_y <= 17 ~ "12-17 years of age",
            TRUE ~ NA_character_
        ), levels = c("0-3 years of age", "4-7 years of age", "8-11 years of age", "12-17 years of age"))) %>%
    filter(!is.na(age_group), is.na(days_since_last) | days_since_last > 14) %>%
    count(age_group, group) %>%
    left_join(
        born_100km %>%
            filter(year >= 2008, year <= 2015) %>%
            mutate(
                group = if_else(year <= 2011, "Pre-vaccine", "Post-vaccine"),
                age_group = factor(case_when(
                    age_y >= 0 & age_y <= 3 ~ "0-3 years of age",
                    age_y >= 4 & age_y <= 7 ~ "4-7 years of age",
                    age_y >= 8 & age_y <= 11 ~ "8-11 years of age",
                    age_y >= 12 & age_y <= 17 ~ "12-17 years of age",
                    TRUE ~ NA_character_
                ), levels = c("0-3 years of age", "4-7 years of age", "8-11 years of age", "12-17 years of age"))) %>%
            group_by(age_group, group) %>%
            summarise(zn = sum(n, na.rm = T)) 
    ) %>%
    gather(key = type, value = count, n, zn) %>%
    xtabs(count ~ group + type + age_group, data = .) %>%
    epi.2by2(dat = ., method = "cohort.time")

### Figure 2 in article ####

temp_ggtext <- 
    data.frame(
        ym = as.yearmon(rep("2014-03-01", 4)),
        y = rep(35, 4),
        label = c(
            paste0(
                "IRR ",
                round(cef_all_ages_2by2$massoc$IRR.strata.wald$est[1], 2),
                " (",
                round(cef_all_ages_2by2$massoc$IRR.strata.wald$lower[1], 2),
                "-",
                round(cef_all_ages_2by2$massoc$IRR.strata.wald$upper[1], 2),
                ")"),
            paste0(
                "IRR ",
                round(cef_all_ages_2by2$massoc$IRR.strata.wald$est[2], 2),
                " (",
                round(cef_all_ages_2by2$massoc$IRR.strata.wald$lower[2], 2),
                "-",
                round(cef_all_ages_2by2$massoc$IRR.strata.wald$upper[2], 2),
                ")"),
            paste0(
                "IRR ",
                round(cef_all_ages_2by2$massoc$IRR.strata.wald$est[3], 2),
                " (",
                round(cef_all_ages_2by2$massoc$IRR.strata.wald$lower[3], 2),
                "-",
                round(cef_all_ages_2by2$massoc$IRR.strata.wald$upper[3], 2),
                ")"),
            paste0(
                "IRR ",
                round(cef_all_ages_2by2$massoc$IRR.strata.wald$est[4], 2),
                " (",
                round(cef_all_ages_2by2$massoc$IRR.strata.wald$lower[4], 2),
                "-",
                round(cef_all_ages_2by2$massoc$IRR.strata.wald$upper[4], 2),
                ")")),
        age_group = c("0-3 years of age", "4-7 years of age", "8-11 years of age", "12-17 years of age"))

age_group_figure <- ceftriaxone %>%
    filter(date <= as.Date("2015-12-31")) %>%
    mutate(
        ym = as.yearmon(date),
        age_group = case_when(
            age_y >= 0 & age_y <= 3 ~ "0-3 years of age",
            age_y >= 4 & age_y <= 7 ~ "4-7 years of age",
            age_y >= 8 & age_y <= 11 ~ "8-11 years of age",
            age_y >= 12 & age_y <= 17 ~ "12-17 years of age",
            TRUE ~ NA_character_ )) %>%
    filter(!is.na(age_group)) %>%
    expand(age_group, ym) %>%
    left_join(
        ceftriaxone %>%
            mutate(
                ym = as.yearmon(date),
                age_group = case_when(
                    age_y >= 0 & age_y <= 3 ~ "0-3 years of age",
                    age_y >= 4 & age_y <= 7 ~ "4-7 years of age",
                    age_y >= 8 & age_y <= 11 ~ "8-11 years of age",
                    age_y >= 12 & age_y <= 17 ~ "12-17 years of age",
                    TRUE ~ NA_character_)) %>%
    filter(!is.na(age_group), is.na(days_since_last) | days_since_last > 14) %>%
    count(age_group, ym)
    ) %>%
    left_join(
        expand.grid(
            year = 2008:2015,
            month = 1:12,
            age_group = c("0-3 years of age", "4-7 years of age", "8-11 years of age", "12-17 years of age")) %>%
            mutate(ym = as.yearmon(paste0(year, "-", month, "-01"))) %>%
            select(-month) %>%
            left_join(
                born_100km %>%
                    filter(year >= 2008, year <= 2015) %>%
                    mutate(age_group = case_when(
                            age_y >= 0 & age_y <= 3 ~ "0-3 years of age",
                            age_y >= 4 & age_y <= 7 ~ "4-7 years of age",
                            age_y >= 8 & age_y <= 11 ~ "8-11 years of age",
                            age_y >= 12 & age_y <= 17 ~ "12-17 years of age",
                            TRUE ~ NA_character_ )) %>%
                    group_by(age_group, year) %>%
                    summarise(zn = floor(sum(n, na.rm = T)/12))) %>%
            select(-year)) %>%
    left_join(temp_ggtext) %>%
    mutate(
        age_group = factor(age_group, levels = c("0-3 years of age", "4-7 years of age", "8-11 years of age", "12-17 years of age")),
        group = if_else(ym <= as.yearmon("2011-12-01"), "Pre-vaccine", "Post-vaccine"),
        n = if_else(is.na(n), 0L, n),
        incidence = n/zn * 1000) %>%
    group_by(age_group, group) %>%
    mutate(mean = mean(incidence)) %>%
    ggplot(aes(x = ym, y = incidence)) +
    geom_line() +
    geom_line(aes(y = mean, group = group)) +
    geom_vline(aes(xintercept = as.yearmon("2012-01-01")), lty = 2) +
    geom_text(aes(y = y, label = label, group = age_group), size = 2.7) +
    facet_wrap(~ age_group) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = NULL, y = "Incidence per 1000 person-years")

ggsave(
    filename = paste0("_figures/paper_I/", Sys.Date(), "-age-group-figure.pdf"), 
    plot = age_group_figure,
    width = 12, height = 10, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_I/", Sys.Date(), "-age-group-figure.png"), 
    plot = age_group_figure,
    width = 12, height = 10, units = "cm", dpi = 600)

save.image(file = paste0("_analyses/paper_I/", Sys.Date(), "-04-2-results-paper1", ".RData"))
