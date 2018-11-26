
library(knitr)
library(tidyverse)
library(lubridate)
library(zoo)

theme_set(theme_bw(base_size = 10, base_family = "sans"))

############################
#### Importing the data ####
############################

ids <- read_rds("_data/results/ids.rds")
lsh <- read_rds("_data/results/lsh.rds")
hg <- read_rds("_data/results/hg.rds")
hs <- read_rds("_data/results/hs.rds")
bs <- read_rds("_data/results/bs.rds")
si <- read_rds("_data/results/si.rds")
lg <- read_rds("_data/results/lg.rds")

#######################
#### Data analysis ####
#######################

density_age <- ids %>%
    left_join(lsh) %>%
    mutate(
        age_y = floor(as.numeric(abs(
            difftime(birth_date, date_in, units = "day")
        ) / 365.25)),
        dataset = if_else(
            lotu_teg == "Ferlilota",
            "Visits to Landspitali University Hospital",
            "Admissions to Landspitali University Hospital"
        )
    ) %>%
    select(id, obs, age_y, dataset) %>%
    filter(age_y >= 0, age_y <= 99) %>%
    union_all(
        hg %>%
            filter(staff_type == "Læ", age_y >= 0, age_y <= 99) %>%
            transmute(
                id = id,
                obs = obs,
                age_y = age_y,
                dataset = "Visits to Primary Care Centers"
            )
    ) %>%
    mutate(dataset = factor(
        dataset,
        levels = c(
            "Visits to Primary Care Centers",
            "Visits to Landspitali University Hospital",
            "Admissions to Landspitali University Hospital")
    )) %>%
    count(dataset, age_y) %>%
    ggplot(aes(x = age_y, y = n, fill = dataset)) +
    geom_area(position = "stack", alpha = 0.8) +
    labs(y = "Number of contacts", x = "Age (years)") +
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = scales::trans_breaks(identity, identity, n = 20)) +
    facet_wrap( ~ dataset, nrow = 3, scales = "free_y") +
    theme(strip.text = element_text(size = 10),
          legend.position = "none")

ggsave(
    filename = paste0("_figures/results/", Sys.Date(), "-density-age.pdf"), 
    plot = density_age,
    width = 12, height = 10, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/results/", Sys.Date(), "-density-age.png"), 
    plot = density_age,
    width = 12, height = 10, units = "cm", dpi = 600)

bs <- ids %>%
    left_join(bs) %>%
    mutate(age_y = case_when(
        !is.na(age_y) ~ age_y,
        is.na(age_y) ~ abs(as.integer(difftime(birth_date, date_vacc, units = "days")/365.25)),
        TRUE ~ NA_integer_
        )) %>%
    filter(!is.na(date_vacc))

vaccine_df <- bs %>% 
    filter(!str_detect(code_text, "Hafnað")) %>% 
    mutate(
        age_group = factor(case_when(
            age_y < 4 ~ "Children <4 years of age",
            age_y >= 4 & age_y < 18 ~ "Children 4-17 years of age",
            age_y >= 18 & age_y < 65 ~ "Adults 18-64 years of age",
            age_y >= 65 ~ "Adults 65 and older"),
            levels = c("Children <4 years of age",
                       "Children 4-17 years of age",
                       "Adults 18-64 years of age",
                       "Adults 65 and older")
        ),
        date_vacc = as.yearmon(date_vacc),
        code_text = factor(case_when(
            str_detect(string = code_text, pattern = "23|Pneumovax|polysaccharides") ~ "23-PPV",
            str_detect(string = code_text, pattern = "13") ~ "PCV13",
            str_detect(string = code_text, pattern = "Prevenar") ~ "PCV7",
            str_detect(string = code_text, pattern = "Synflorix") ~ "PCV10",
            TRUE ~ NA_character_),
            levels = c("PCV7", "PCV10", "PCV13", "23-PPV")
        )) %>%
    filter(!is.na(code_text), !is.na(age_group)) %>%
    count(date_vacc, age_group, code_text)

vaccine_df <- 
    vaccine_df %>%
    right_join(expand(vaccine_df, date_vacc, age_group, code_text)) %>%
    mutate(n = if_else(is.na(n), 0L, n))

vaccine_time <- vaccine_df %>%
    ggplot(aes(x = date_vacc, y = n, fill = code_text)) +
    geom_vline(aes(xintercept = as.yearmon("2011-04-01")), lty = 2) +
    geom_area(position = "stack", alpha = 0.8) +
    labs(x = "Time (months)", y = "Number of administered vaccine doses") +
    facet_wrap(~ age_group, ncol = 1, scales = "free_y") +
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(3)) +
    scale_x_continuous(breaks = scales::trans_breaks(identity, identity, n = 12)) +
    theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.spacing.x = unit("0.3", "cm"))

ggsave(
    filename = paste0("_figures/results/", Sys.Date(), "-vaccine-time.pdf"), 
    plot = vaccine_time,
    width = 12, height = 10, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/results/", Sys.Date(), "-vaccine-time.png"), 
    plot = vaccine_time,
    width = 12, height = 10, units = "cm", dpi = 600)

vaccine_age <- ids %>%
    filter(birth_y >= 2005, birth_y <= 2017) %>%
    left_join(hs) %>%
    filter(is.na(to_from)) %>%
    left_join(
        bs %>%
            filter(!str_detect(code_text, "Hafnað"), str_detect(atc, "J07AL02|J07AL52")) 
    ) %>% 
    arrange(birth_date, id) %>%
    mutate(
        date_obs = as.Date("2017-12-31"),
        age_obs = as.numeric(abs(difftime(birth_date, date_obs, units = "day"))),
        vaccine_group = if_else(birth_y <= 2010, "Vaccine non-eligible", "Vaccine eligible"),
        age_vacc = as.numeric(abs(difftime(birth_date, date_vacc, units = "day")))) %>% 
    group_by(id) %>%
    mutate(dose = dense_rank(date_vacc)) %>% 
    ungroup() %>%
    filter(!is.na(dose), dose <4) %>%
    mutate(Dose = case_when(
        dose == 1 ~ "First (Primary)",
        dose == 2 ~ "Second (Primary)",
        dose == 3 ~ "Third (Booster)"
    )) %>%
    ggplot(aes(x = birth_date, y = floor(age_obs/30))) +
    geom_point(aes(y = age_vacc/30, color = Dose), alpha = 0.5, size = 0.5) +
    geom_vline(aes(xintercept = as.Date("2011-01-01")), lty = 2) +
    coord_cartesian(ylim = c(0,4*12)) +
    scale_color_brewer(palette = "Set1") +
    scale_y_continuous(labels = scales::comma, breaks = c(0,6,12,18,24,30,36,42,48)) +
    scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
    labs(x = "Date of birth", y = "Age (Months)") +
    theme(legend.title = element_blank(),
          legend.position = "bottom")

ggsave(
    filename = paste0("_figures/results/", Sys.Date(), "-vaccine-age.pdf"), 
    plot = vaccine_age,
    width = 12, height = 10, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/results/", Sys.Date(), "-vaccine-age.png"), 
    plot = vaccine_age,
    width = 12, height = 10, units = "cm", dpi = 600)

prescriptions_age <- lg %>%
    filter(str_detect(atc, "J01")) %>%
    mutate(atc = str_extract(atc, "^.{4}")) %>%
    filter(!(atc %in% c("J01B", "J01G", "J01X"))) %>%
    left_join(data.frame(
        atc = c("J01A", "J01C", "J01D", "J01E", "J01F", "J01M"),
        names = c(
            "Tetracyclines",
            "Beta-lactam antibacterials, penicillins",
            "Other beta-lactam antibacterials",
            "Sulfonamides and trimethoprim",
            "Macrolides, lincosamides and streptogramins",
            "Quinolone antibacterials"
        )
    )) %>%
    mutate(names = factor(
        names,
        levels =  c(
            "Tetracyclines",
            "Beta-lactam antibacterials, penicillins",
            "Other beta-lactam antibacterials",
            "Sulfonamides and trimethoprim",
            "Macrolides, lincosamides and streptogramins",
            "Quinolone antibacterials"
        )
    )) %>%
    count(age_y, names) %>%
    ggplot(aes(x = age_y, y = n, fill = names)) +
    geom_area(alpha = 0.8) +
    scale_color_brewer(palette = "Set1") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(limits = c(0, 100), breaks = scales::pretty_breaks(n = 9)) +
    facet_wrap( ~ names, ncol = 1, scales = "free_y") +
    labs(x = "Age (years)", y = "Number of prescriptions") +
    theme(legend.position = "none")

ggsave(
    filename = paste0("_figures/results/", Sys.Date(), "-prescriptions-age.pdf"), 
    plot = prescriptions_age,
    width = 12, height = 12, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/results/", Sys.Date(), "-prescriptions-age.png"), 
    plot = prescriptions_age,
    width = 12, height = 12, units = "cm", dpi = 600)

save.image(file = paste0("_analyses/results/", Sys.Date(), "-04-1-results", ".RData"))
