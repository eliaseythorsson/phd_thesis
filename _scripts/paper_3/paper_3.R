# Analysis for paper 3

library(tidyverse)
library(lubridate)
library(zoo)
library(epiR)
library(survival)
library(rms)
library(survminer)
library(broom)

# set theme for ggplot2 figures
theme_set(theme_bw(base_size = 10, base_family = "sans"))

#######################################
#### Importing the data; wrangling ####
#######################################

### Decided to use complete data for thesis ###
### which will result in slightly different result than published paper ###

max_age <- round(365.25*3) # maximum follow-up age used in the study

pop <- read_csv2("_data/paper_3/pop_gender_1jan_1980_2017.csv")
ids <- read_rds("_data/results/ids.rds")
lg <- read_rds("_data/results/lg.rds")
hg <- read_rds("_data/results/hg.rds")
hs <- read_rds("_data/results/hs.rds")

### Baseline demographics for each individual in the study ###
ids_surv <- ids %>% # all individuals in the study population
    filter(birth_y >= 2005, birth_y <= 2015, !is.na(birth_date)) %>% # birthcohorts 2005-2015
    left_join(
        hs %>% # immigration/emigration data
            select(-move_type) %>% 
            group_by(id) %>%
            arrange(id, date_move) %>%
            filter(date_move == first(date_move)) %>% # first move to or from the country
            ungroup() %>%
            distinct()
    ) %>%
    filter(to_from == "move_from_iceland" | is.na(to_from)) %>% # censor children how moved TO Iceland
    mutate(date = as.Date("2016-12-31"), date_move = as.Date(date_move)) %>% # baseline follow-up til Dec 31 2016 unless death, emigration
    mutate(
        date = case_when(
            is.na(death_date) & is.na(date_move) ~ date, # if no death, no move -> Dec31 2016
            is.na(death_date) & !is.na(date_move) & date_move > date ~ date, # if no death, move after Dec31 2016 -> Dec31 2016
            is.na(death_date) & !is.na(date_move) & date_move <= date ~ date_move, # if no death, move before Dec31 2016 -> move
            !is.na(death_date) & is.na(date_move) & death_date > date ~ date, # if death, no move, death after Dec31 2016 -> Dec31 2016
            !is.na(death_date) & is.na(date_move) & death_date <= date ~ death_date, # if death, no move, death before Dec31 2016 -> death
            !is.na(death_date) & !is.na(date_move) & death_date > date & date_move > date ~ date, # etc
            !is.na(death_date) & !is.na(date_move) & death_date <= date & date_move > date ~ death_date, # etc
            !is.na(death_date) & !is.na(date_move) & death_date > date & date_move <= date ~ date_move, # etc
            !is.na(death_date) & !is.na(date_move) & death_date <= date & date_move > date ~ death_date, # etc
            !is.na(death_date) & !is.na(date_move) & death_date <= date & date_move <= date & death_date < date_move ~ death_date,
            !is.na(death_date) & !is.na(date_move) & death_date <= date & date_move <= date & death_date >= date_move ~ date_move,
            TRUE ~ date
        )
    ) %>%
    mutate(
        age_no_censor = as.numeric(date - birth_date), # true followup without artifical censoring at third birthday
        age = pmin(max_age, as.numeric(date - birth_date)) # follow up when censored to third birthday
    ) %>%
    filter(age >= 1) %>% # removes ~7 children who emigrated on their date of birth (which wreaks havoc on survival analysis)
    distinct()

### Age at n = {1, 2, ... n} antimicrobial prescription ###
lg_surv<- 
    ids_surv %>%
    left_join(
        lg %>%
            select(id, obs, atc, date_presc) %>%
            filter(str_detect(string = atc, pattern = "J01")) # only system antibacterials
    ) %>%
    mutate(
        age = pmin( # Chooses which is lower ... (effectively censores on maximum age in study)
            max_age, # maximum age used in the study, or... 
            pmax(
                1, # [age of first antimicrobial prescription cannot be younger than 1 day]
                as.numeric(as.Date(date_presc) - birth_date) # ... age at the time of antimicrobial prescription
            )
        )
    ) %>%
    filter(age < max_age) %>% # censores all antimicrobial prescriptions occuring after maximum age
    group_by(id) %>%
    arrange(id, age) %>% # arrange by increasing age
    mutate(
        obs = cummax(as.numeric(factor(obs, levels = unique(obs)))) # counts {first, second, ... } rank number of prescriptions within id
    ) %>%
    ungroup() %>%
    select(id, obs, age) %>%
    distinct() %>%
    spread(key = obs, value = age)

### Create correct time at-risk ###
lg_ag <- tmerge(ids_surv, ids_surv, id = id, tstop = ids_surv$age)
lg_ag$tstart <- ifelse(lg_ag$tstart == 0, 1, lg_ag$tstart)

for(i in 1:(dim(lg_surv)[2] - 1)){
    x <- paste(i)
    lg_ag <- tmerge(lg_ag, lg_surv, id = id, presc = event(unlist(lg_surv[,x])))
}

lg_ag <- tmerge(lg_ag, subset(lg_ag, presc == 1), id = id, cum_presc = cumtdc(tstop))

### Population demographics ###
pop <-
    pop %>%
    gather(key = year, value = n, -gender, -age_y) %>%
    filter(year >= 2005, year <= 2016, age_y <= 2) %>%
    group_by(year) %>%
    summarise(n = sum(n))

### Prescription data for linkage to the primary care registry ###
first_line_pen <- c("J01CA04", "J01CA01", "J01CF01", "J01CF04", "J01CE02")
sec_line_pen <- c("J01CR02")
first_gen_macro <- c("J01FA01")
sec_gen_macro <- c("J01FA10", "J01FA09")

lg_presc <-
    lg %>%
    mutate(year = year(date_presc)) %>%
    select(obs, age_y, year, atc) %>%
    filter(str_detect(atc, "J01"), age_y <= 2, year >= 2005, year <= 2016) %>%
    mutate(
        class = case_when(
            atc %in% first_line_pen ~ "First-line penecillin",
            atc %in% sec_line_pen ~ "Second-line penecillin",
            atc %in% first_gen_macro ~ "First-generation macrolide", 
            atc %in% sec_gen_macro ~ "Second-generation macrolide",
            str_detect(string = atc, pattern = "J01D") ~ "Cephalosporin",
            TRUE ~ "Other"
        )
    )
### Primary care registry prepared for analysis ###
hg <-
    hg %>%
    filter(
        age_y <= 2,
        contact_type %in% c("Viðtal", "Annað"),
        staff_type == "Læ",
        !str_detect(.$main_diagnosis, "N30|N39") | !str_detect(.$other_diagnosis, "N30|N39")
    ) %>%
    mutate(
        diagnosis = case_when(
            str_detect(.$main_diagnosis, "H66|H65|H72") | str_detect(.$other_diagnosis, "H66|H65|H72") ~ "Otitis media",
            str_detect(.$main_diagnosis, "J10|J11|J12|J13|J14|J15|J16|J17|J18") | 
                str_detect(.$other_diagnosis, "J10|J11|J12|J13|J14|J15|J16|J17|J18") ~ "Influenza and pneumonia",
            str_detect(.$main_diagnosis, "J20|J21|J22") | str_detect(.$other_diagnosis, "J20|J21|J22") ~ "Other acute lower respiratory infections",
            str_detect(.$main_diagnosis, "J00|J01|J02|J03|J04|J05|J06") |
                str_detect(.$other_diagnosis, "J00|J01|J02|J03|J04|J05|J06") ~ "Acute upper respiratory infections",
            TRUE ~ "Other viral infections"
        ),
        year = year(date_contact)
    ) %>%
    select(-contact_type,-staff_type,-institute_id)

hg2 <- 
    hg %>%
    rename(contact_id = obs) %>%
    select(-age_y) %>%
    full_join(
        lg %>% filter(str_detect(.$atc, "J01")),
        by = "id") %>%
    mutate(difftime = as.numeric(difftime(date_presc, date_contact, units = "days"))) %>%
    filter(difftime >= 0, difftime <= 3) %>% distinct()

hg2 <- 
    hg2 %>%
    right_join(hg %>% rename(contact_id = obs))

hg2 <- 
    hg2 %>%
    group_by(id, contact_id) %>%
    filter(is.na(obs) | obs == first(obs))

hg_sum <- 
    hg2 %>%
    group_by(year) %>%
    summarise(total_yr = n_distinct(contact_id)) %>%
    left_join(
        hg2 %>%
            mutate(presc = ifelse(is.na(obs), "no", "yes")) %>%
            group_by(year, diagnosis, presc) %>%
            summarise(n_contact = n_distinct(contact_id)) %>%
            ungroup(),
        by = "year") %>% 
    spread(key = diagnosis, value = n_contact)

#######################
#### Data analysis ####
#######################

lg_ag <-
    lg_ag %>%
    mutate(
        tstart_m = tstart/30.45,
        tstop_m = tstop/30.45,
        group = if_else(birth_y <= 2010, "Non-vaccine eligible", "Vaccine-eligible"),
        birth_y_cha = as.factor(birth_y),
        gender = as.factor(gender)
    )


survobj <- with(lg_ag, Surv(tstart_m, tstop_m, presc))
dd <- with(lg_ag, datadist(gender, birth_y_cha, group, birth_m, cum_presc))
dd$limits$cum_presc[3] <- 2
options(datadist = "dd")

lg_cph <- cph(formula = survobj ~ cluster(id) + strat(gender) + rcs(cum_presc) * birth_y_cha, data = lg_ag, x = T, y = T, model = T)

hazardratio_fig <- cbind.data.frame(
    birth_y_cha = c(as.character(2005:2009), as.character(2011:2015)),
    summary(lg_cph, cum_presc = 0, birth_y_cha = "2010")[seq(4, 22, 2), c(4, 6, 7)]) %>%
    full_join(data.frame(birth_y_cha = "2010 \n(Reference)", Effect = 1)) %>%
    mutate(birth_y_cha = factor(
        birth_y_cha,
        levels = c("2015", "2014", "2013", "2012", "2011", "2010 \n(Reference)", "2009", "2008", "2007", "2006", "2005"))) %>%
    ggplot(data = ., aes(x = birth_y_cha, y = Effect)) + geom_hline(aes(yintercept = 1), lty = 2) + 
    geom_point() +
    geom_errorbar(aes(ymin = `Lower 0.95`, ymax = `Upper 0.95`), width = 0.2) +
    coord_flip(ylim = c(0.8,1.25)) +
    scale_y_log10(breaks = c(0.8, 0.9, 1, 1.1, 1.2)) +
    labs(x = NULL, y = "Hazard ratio")


ggsave(
    filename = paste0("_figures/paper_3/", Sys.Date(), "-hazardratio.pdf"), 
    plot = hazardratio_fig,
    width = 12, height = 8, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_3/", Sys.Date(), "-hazardratio.png"), 
    plot = hazardratio_fig ,
    width = 12, height = 8, units = "cm", dpi = 600)

### Hazard ratio per numer of previous visits ###
dd <- with(lg_ag, datadist(gender, group, cum_presc))
options(datadist = "dd")
dd$limits$cum_presc[3] <- 1
cox_lg <- cph(formula = survobj ~ cluster(id) + strat(gender) + rcs(cum_presc) * group, data = lg_ag, x = T, y = T, surv = T)


x <- matrix(summary(cox_lg, cum_presc = 0)[4,c(4,6,7)], ncol = 3)
for(i in 1:(lg_ag %>% summarise(n = max(cum_presc)) %>% .$n)){
    x <- rbind(x, summary(cox_lg, cum_presc = i)[4,c(4,6,7)])
}

hr_previous <- x %>% 
    as.tibble() %>%
    mutate(cum_presc = 0:(nrow(.)-1)) %>%
    ggplot(aes(x = cum_presc)) +
    geom_line(aes(y = Effect)) +
    geom_ribbon(aes(ymin = `Lower 0.95`, ymax = `Upper 0.95`), alpha = .2) +
    geom_hline(yintercept = 1, lty = 2) +
    scale_x_continuous(breaks = seq(0,54,4), minor_breaks = NULL) +
    scale_y_continuous(breaks = seq(0.5, 2, 0.1), minor_breaks = NULL) +
    coord_trans(y = "log10", limy = c(0.5,2)) +
    labs(x = "No. of previous prescriptions", y = "Hazard ratio (VEC/NVEC)") +
    theme_bw()

ggsave(
    filename = paste0("_figures/paper_3/", Sys.Date(), "-hr-previous.pdf"), 
    plot = hr_previous,
    width = 12, height = 8, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_3/", Sys.Date(), "-hr-previous.png"), 
    plot = hr_previous,
    width = 12, height = 8, units = "cm", dpi = 600)

### Nelson-Aalen cumulative mean ###
dd <- with(lg_ag, datadist(gender, group, cum_presc))
options(datadist = "dd")
dd$limits$cum_presc[3] <- 1
cox_lg <- cph(formula = survobj ~ cluster(id) + strat(gender) + rcs(cum_presc) * strat(group), data = lg_ag, x = T, y = T, surv = T)

nelsonaalen_fig <- ggsurvplot(
    survfit(cox_lg),
    data = lg_ag,
    break.x.by = 6,
    conf.int = T,
    censor = FALSE,
    xlab = "Age (months)",
    ylab = "Mean number of prescriptions",
    legend.labs = c("Vaccine non-eligible, female", "Vaccine non-eligible, male", "Vaccine eligible, female", "Vaccine eligible, male"),
    fun = "cumhaz",
    legend.title = "",
    legend = c(0.75, 0.25),
    xlim = c(0, 36),
    ylim = c(0, 7),
    palette = c("#E41A1C", "#377EB8", "#E41A1C", "#377EB8"),
    linetype = c(1, 1, 2, 2),
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
    filename = paste0("_figures/paper_3/", Sys.Date(), "-nelsonaalen.pdf"), 
    plot = nelsonaalen_fig$plot,
    width = 12, height = 8, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_3/", Sys.Date(), "-nelsonaalen.png"), 
    plot = nelsonaalen_fig$plot ,
    width = 12, height = 8, units = "cm", dpi = 600)

### Proportion of diagnosis resulting in antimicrobial prescription ###

p_prop_ant <- hg_sum %>%
    gather(key = diagnosis, value = n_contact, -year, -total_yr, -presc) %>%
    filter(presc == "yes") %>%
    left_join(
        hg_sum %>%
            gather(key = diagnosis, value = n_contact, -year, -total_yr, -presc) %>%
            filter(presc == "yes") %>%
            group_by(year) %>%
            summarise(total_n = sum(n_contact)) %>%
            ungroup()
    ) %>%
    ggplot(aes(x = year, y = n_contact/total_n, color = diagnosis)) +
    geom_line(alpha = 0.3) +
    geom_point() +
    scale_y_continuous(breaks = seq(0, 0.7, 0.1), labels = scales::percent) +
    scale_x_continuous(breaks = c(2005, 2007, 2009, 2011, 2013, 2015, 2017)) +
    scale_color_brewer(palette = "Set1") +
    labs(x = NULL, y = "Proportion (%)", title = "Proportion of total antimicrobial prescriptions \nassociated with diagnostic group") +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    guides(color=guide_legend(nrow=3))

p_inc <- hg_sum %>%
    gather(key = diagnosis, value = n_contact, -year, -total_yr, -presc) %>%
    filter(presc == "yes") %>%
    left_join(
        hg_sum %>%
            gather(key = diagnosis, value = n_contact, -year, -total_yr, -presc) %>%
            filter(presc == "yes") %>%
            group_by(year) %>%
            summarise(total_n = sum(n_contact)) %>%
            ungroup()
    ) %>%
    left_join(pop %>% mutate(year = as.integer(year)), by = "year") %>%
    mutate(inc_100 = n_contact/n*100) %>%
    ggplot(aes(x = year, y = inc_100, color = diagnosis)) +
    geom_line(alpha = 0.3) +
    geom_point(show.legend = F) +
    scale_y_continuous(breaks = c(0, 20, 40, 60), minor_breaks = c(10, 30, 50), limits = c(0, 60))+
    scale_x_continuous(breaks = c(2005, 2007, 2009, 2011, 2013, 2015, 2017)) +
    scale_color_brewer(palette = "Set1") +
    labs(x = "Calendar year", y = "Incidence \n(per 100 person-years)", title = "Incidence of antimicrobial prescriptions \nby diagnostic group")


arranged_plot <- ggarrange(p_prop_ant, p_inc, nrow = 2, labels = c("A", "B"), common.legend = T, legend = "bottom")

ggsave(
    filename = paste0("_figures/paper_3/", Sys.Date(), "-prop-incidence.pdf"), 
    plot = arranged_plot,
    width = 12, height = 13, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_3/", Sys.Date(), "-prop-incidence.png"), 
    plot = arranged_plot,
    width = 12, height = 13, units = "cm", dpi = 600)

### Incidence rate and incidence ratio figures ###
ageg_order <- c("0-5","6-11","12-17","18-23","24-29","30-35")

npy <- 
    expand.grid(birth_y = as.character(2005:2015), age_m = 0:35, gender = as.factor(c("male", "female"))) %>%
    full_join(
        lg_ag %>%
            mutate(birth_y = birth_y_cha) %>%
            filter(!is.na(gender)) %>%
            select(id, gender, birth_y, tstop_m) %>%
            distinct() %>%
            mutate(tstop_m = as.integer(tstop_m)),
        by = c("birth_y", "gender")) %>%
    filter(tstop_m >= age_m) %>%
    group_by(birth_y, gender, age_m) %>%
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
    group_by(birth_y, gender, group_m) %>%
    summarise(at_risk = round(sum(at_risk)/12)) %>%
    ungroup()

lg_n <-
    lg_ag %>%
    mutate(birth_y = birth_y_cha) %>%
    filter(!is.na(gender)) %>%
    mutate(age_m = as.integer(tstop_m)) %>%
    group_by(birth_y, gender, age_m) %>%
    summarise(prescs = sum(presc)) %>%
    ungroup() %>%
    mutate(group_m =
               cut(
                   age_m,
                   breaks = c(0, 5, 11, 17, 23, 29, 35),
                   labels = c("0-5", "6-11", "12-17", "18-23", "24-29", "30-35"),
                   include.lowest = T
               )
    ) %>%
    mutate(group_m = factor(group_m, levels = ageg_order, ordered = T)) %>%
    group_by(birth_y, gender, group_m) %>%
    summarise(prescs = sum(prescs)) %>%
    ungroup()

lg_ppy <-
    npy %>%
    left_join(lg_n) %>%
    group_by(birth_y, group_m) %>%
    summarise(at_risk = sum(at_risk), prescs = sum(prescs))

lg_ppy[which(is.na(lg_ppy$prescs)), "prescs"] <- 0

temp <- matrix(NA, ncol = 3, nrow = dim(lg_ppy)[1])
for(i in 1:dim(lg_ppy)[1]){
    temp[i,1] <- tidy(poisson.test(lg_ppy$prescs[i], lg_ppy$at_risk[i]))$estimate*100
    temp[i,2] <- tidy(poisson.test(lg_ppy$prescs[i], lg_ppy$at_risk[i]))$conf.low*100
    temp[i,3] <- tidy(poisson.test(lg_ppy$prescs[i], lg_ppy$at_risk[i]))$conf.high*100
}

lg_ppy <- 
    cbind.data.frame(lg_ppy, temp) %>%
    rename(ir = `1`, lci = `2`, uci = `3`) %>%
    mutate(
        ir = round(ir, 2),
        lci = round(lci, 2),
        uci = round(uci, 2)
    ) %>%
    mutate(ir_ci = paste0(ir, " ", "(", lci, "-", uci, ")"))

lg_ppy_group <-
    npy %>%
    left_join(lg_n) %>%
    group_by(birth_y, group_m) %>%
    summarise(at_risk = sum(at_risk), prescs = sum(prescs))

lg_ppy_group[which(is.na(lg_ppy_group$prescs)), "prescs"] <- 0

lg_ppy_group <- 
    lg_ppy_group %>%
    ungroup() %>%
    mutate(birth_y = as.numeric(as.character(birth_y))) %>%
    mutate(group = ifelse(birth_y <= 2010, "VNEC", "VEC")) %>%
    group_by(group, group_m) %>%
    summarise(at_risk = sum(at_risk, na.rm = T), prescs = sum(prescs, na.rm = T)) %>%
    ungroup()

temp <- matrix(NA, ncol = 3, nrow = dim(lg_ppy_group)[1])
for(i in 1:dim(lg_ppy_group)[1]){
    temp[i,1] <- tidy(poisson.test(lg_ppy_group$prescs[i], lg_ppy_group$at_risk[i]))$estimate*100
    temp[i,2] <- tidy(poisson.test(lg_ppy_group$prescs[i], lg_ppy_group$at_risk[i]))$conf.low*100
    temp[i,3] <- tidy(poisson.test(lg_ppy_group$prescs[i], lg_ppy_group$at_risk[i]))$conf.high*100
}

lg_ppy_group <- 
    cbind(lg_ppy_group, temp) %>%
    rename(ir = `1`, lci = `2`, uci = `3`) %>%
    mutate(
        ir = format(round(ir, 2), nsmall = 2), 
        lci = format(round(lci, 2), nsmall = 2), 
        uci = format(round(uci, 2), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(ir, " ", "(", lci, "-", uci, ")"))

irr <-
    cbind.data.frame(
        group_m = c("0-5", "6-11", "12-17", "18-23", "24-29", "30-35"),
        VNEC_prescs = lg_ppy_group %>% filter(group == "VNEC") %>% .$prescs,
        VNEC_atrisk = lg_ppy_group %>% filter(group == "VNEC") %>% .$at_risk,
        vec_prescs = lg_ppy_group %>% filter(group == "VEC") %>% .$prescs,
        vec_atrisk = lg_ppy_group %>% filter(group == "VEC") %>% .$at_risk
    )

temp <- matrix(NA, ncol = 4, nrow = dim(irr)[1])
for(i in 1:dim(irr)[1]){
    temp[i,1] <- tidy(poisson.test(c(irr$vec_prescs[i], irr$VNEC_prescs[i]), c(irr$vec_atrisk[i], irr$VNEC_atrisk[i])))$estimate
    temp[i,2] <- tidy(poisson.test(c(irr$vec_prescs[i], irr$VNEC_prescs[i]), c(irr$vec_atrisk[i], irr$VNEC_atrisk[i])))$conf.low
    temp[i,3] <- tidy(poisson.test(c(irr$vec_prescs[i], irr$VNEC_prescs[i]), c(irr$vec_atrisk[i], irr$VNEC_atrisk[i])))$conf.high
    temp[i,4] <- tidy(poisson.test(c(irr$vec_prescs[i], irr$VNEC_prescs[i]), c(irr$vec_atrisk[i], irr$VNEC_atrisk[i])))$p.value
}

irr <- 
    cbind(irr, temp) %>%
    rename(irr = `1`, lci = `2`, uci = `3`, pvalue = `4`) %>%
    mutate(
        irr = format(round(irr, 2), nsmall = 2),
        lci = format(round(lci, 2), nsmall = 2),
        uci = format(round(uci, 2), nsmall = 2),
        pvalue = format(round(pvalue, 6), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(irr, " ", "(", lci, "-", uci, ")"))

birth_y <- c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")

incidence_fig <- lg_ppy %>%
    left_join(
        irr %>% select(group_m, ir_ci2 = ir_ci) %>%
            mutate(ir_ci2 = paste0("IRR", " ", .$ir_ci2), birth_y = "2010"),
        by = c("group_m", "birth_y")
    ) %>%
    mutate(
        group_m = factor(group_m, levels = ageg_order),
        birth_y = factor(
            birth_y,
            levels = rev(levels(factor(birth_y))),
            labels = rev(levels(factor(birth_y)))
        ),
        ir = as.numeric(ir),
        lci = as.numeric(lci),
        uci = as.numeric(uci)
    ) %>%
    ggplot(aes(x = group_m, y = ir, color = birth_y)) +
    geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.8, position = position_dodge(width = 0.9)) +
    geom_vline(aes(xintercept = as.numeric(group_m) - 0.5), color = "grey") +
    geom_point(position = position_dodge(width = 0.9), size = 0.3) +
    geom_text(aes(y = 400, label = ir_ci2), color = "black", size = 2.7) +
    scale_y_continuous(breaks = c(0,50,100,150,200,250,300), minor_breaks = c(25,75,125,175,225,275), limits = c(0, 475)) +
    labs(x = "Age-group", y = "Incidence rate per 100 person-years") +
    coord_flip(xlim = c(1.1,5.9)) +
    scale_x_discrete(limits = rev(ageg_order)) +
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
        legend.title = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.background = element_rect(
            fill = "transparent",
            colour = NA,
            size = NULL,
            linetype = NULL,
            inherit.blank = NULL
        )
    )

ggsave(
    filename = paste0("_figures/paper_3/", Sys.Date(), "-incidence.pdf"), 
    plot = incidence_fig,
    width = 12, height = 8, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_3/", Sys.Date(), "-incidence.png"), 
    plot = incidence_fig,
    width = 12, height = 8, units = "cm", dpi = 600)

### Test of proportions ###

crude_data <-
    lg_ag %>%
    filter(age >= 1095, birth_y <= 2013) %>%
    group_by(id) %>%
    filter(cum_presc == max(cum_presc)) %>%
    ungroup() %>%
    mutate(cum_presc == ifelse(.$presc == 0, .$cum_presc, .$cum_presc + 1)) %>% 
    group_by(group, cum_presc) %>%
    summarise(n = n_distinct(id)) %>%
    spread(key = cum_presc, value = n, fill = 0) %>%
    ungroup() %>%
    select(-group) %>%
    as.matrix()

crude_null <-
    matrix(
        c(
            crude_data[,"0"],
            apply(crude_data, MARGIN = 1, FUN = sum) -
                crude_data[,"0"]
        ), nrow = 2, byrow = F)

dimnames(crude_null) <- list("Exposure" = c("non", "vacc"), "Fjoldi" = c("0", "allelse"))

epi_null <- epi.2by2(crude_null[c(2,1),], method ="cohort.count", conf.level = 1-0.05)

crude_14 <-
    matrix(
        c(
            apply(crude_data[,c("1","2","3","4")], MARGIN = 1, FUN = sum),
            apply(crude_data, MARGIN = 1, FUN = sum) -
                apply(crude_data[,c("1","2","3","4")], MARGIN = 1, FUN = sum)
        ), nrow = 2, byrow = F)

dimnames(crude_14) <- list("Exposure" = c("non", "vacc"), "Fjoldi" = c("1-4", "allelse"))

epi_14<- epi.2by2(crude_14[c(2,1),], method ="cohort.count", conf.level = 1-0.05)

crude_59 <-
    matrix(
        c(
            apply(crude_data[,c("5", "6", "7", "8", "9")], MARGIN = 1, FUN = sum),
            apply(crude_data, MARGIN = 1, FUN = sum) -
                apply(crude_data[,c("5", "6", "7", "8", "9")], MARGIN = 1, FUN = sum)
        ), nrow = 2, byrow = F)

dimnames(crude_59) <- list("Exposure" = c("non", "vacc"), "Fjoldi" = c("5-9", "allelse"))

epi_59 <- epi.2by2(crude_59[c(2,1),], method ="cohort.count", conf.level = 1-0.05)

crude_1014 <-
    matrix(
        c(
            apply(crude_data[,c("10", "11", "12", "13", "14")], MARGIN = 1, FUN = sum),
            apply(crude_data, MARGIN = 1, FUN = sum) -
                apply(crude_data[,c("10", "11", "12", "13", "14")], MARGIN = 1, FUN = sum)
        ), nrow = 2, byrow = F)

dimnames(crude_1014) <- list("Exposure" = c("non", "vacc"), "Fjoldi" = c("10-14", "allelse"))

epi_1014 <- epi.2by2(crude_1014[c(2,1),], method ="cohort.count", conf.level = 1-0.05)

crude_15p <-
    matrix(
        c(
            apply(crude_data[,c(sapply(c(15:38, 40:45, 50,51,53,55), paste0))], MARGIN = 1, FUN = sum),
            apply(crude_data, MARGIN = 1, FUN = sum) -
                apply(crude_data[,c(sapply(c(15:38, 40:45, 50,51,53,55), paste0))], MARGIN = 1, FUN = sum)
        ), nrow = 2, byrow = F)

dimnames(crude_15p) <- list("Exposure" = c("non", "vacc"), "Fjoldi" = c("15+", "allelse"))

epi_15p <- epi.2by2(crude_15p[c(2,1),], method ="cohort.count", conf.level = 1-0.05)


save.image(file = paste0("_analyses/paper_3/", Sys.Date(), "-04-2-results-paper3", ".RData"))
