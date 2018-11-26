# Analysis for paper 2

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

ids <- read_rds("_data/results/ids.rds")
hg <- read_rds("_data/results/hg.rds")

hg <- ids %>%
    left_join(hg)

### create new variables: age in months, age in days ###
hg <- hg %>% 
    mutate(
        age_m = round(as.integer(difftime(date_contact, birth_date, unit = "days"))/30),
        age_d = as.integer(difftime(date_contact, birth_date, unit = "days"))
    ) %>% 
    filter(age_m >= 0 | is.na(age_m))


### creates dataframe of every individual in the current study, along with the follow-up age ###  
ids_surv <- 
    ids %>%
    filter(birth_y >= 2005, birth_y <= 2015) %>%
    distinct(id, .keep_all = T) %>%
    mutate(date = as.Date("2015-12-31")) %>%
    mutate(
        date = case_when(
            is.na(.$death_date) ~ .$date,
            !is.na(.$death_date) & .$death_date > .$date ~ .$date,
            !is.na(.$death_date) & .$death_date <= .$date ~ .$death_date,
            TRUE ~ .$date
        )
    ) %>%
    mutate(age = pmin(max_age, as.numeric(as.Date("2015-12-31") - birth_date))) %>%
    filter(age >= 1) %>%
    mutate(tstart = 0)

### creates dataframe of every individual in the current study, along with age at n = {1, 2, ... , n} AOM visit ###
hg_aom <-
    hg %>%
    filter(
        birth_y >= 2005, birth_y <= 2015,
        contact_type == "Viðtal", staff_type == "Læ",
        str_detect(.$main_diagnosis, "H66") | str_detect(.$other_diagnosis, "H66")
    ) %>%
    arrange(date_contact) %>%
    group_by(id) %>%
    mutate(days_since_last =  difftime(date_contact, lag(date_contact), unit = "days")) %>%
    ungroup()

hg_aom[which(is.na(hg_aom$days_since_last)), "days_since_last"] <- 0

hg_surv<- 
    ids %>%
    filter(birth_y >= 2005, birth_y <= 2015) %>%
    left_join(
        hg_aom,
        by = c("id", "gender", "birth_y", "birth_m", "birth_d", "birth_date", "death_date")
    ) %>%
    select(id, contact_id = obs, birth_date, date_contact) %>%
    distinct(id, contact_id, .keep_all = T) %>%
    mutate(age = pmin(max_age, pmax(30, as.numeric(date_contact - birth_date)))) %>%
    select(-birth_date, -date_contact) %>%
    filter(age < max_age) %>% #bara mín gögn
    group_by(id) %>%
    arrange(id, age) %>%
    mutate(
        contact_id = cummax(as.numeric(factor(contact_id, levels = unique(contact_id))))
    ) %>%
    ungroup() %>%
    spread(key = contact_id, value = age, sep = "_") %>%
    as.data.frame()

hg_ag <- tmerge(ids_surv, ids_surv, id = id, tstop = ids_surv$age)

### creates correct time at-risk ###
hg_ag <- tmerge(ids_surv, ids_surv, id = id, tstart = tstart, tstop = ids_surv$age)

hg_ag$cum_visit_0 <- 0
for(i in 1:(dim(hg_surv)[2] - 1)){
    x <- paste0("contact_id_", i)
    hg_ag <- tmerge(hg_ag, hg_surv, id = id, visit = event(hg_surv[,x]))
    hg_ag <- tmerge(hg_ag, subset(hg_ag, visit == 1), id = id, cum_visit = cumtdc(tstop))
    hg_ag[, paste0("cum_visit_", i)] <- hg_ag[, "cum_visit"]
    hg_ag[which(hg_ag$cum_visit > 0 & hg_ag$cum_visit > hg_ag[, paste0("cum_visit_", i - 1)]), "tstart"] <-
        hg_ag[which(hg_ag$cum_visit > 0 & hg_ag$cum_visit > hg_ag[, paste0("cum_visit_", i - 1)]), "tstart"] + 30
    hg_ag <- hg_ag[which(hg_ag$tstart < hg_ag$tstop),]
}
hg_ag <- hg_ag[,!grepl("cum_visit_", colnames(hg_ag))]

#######################
#### Data analysis ####
#######################

### Andersen-Gill extension of the Cox regression ###
hg_ag$tstart_m <- hg_ag$tstart/30.45
hg_ag$tstop_m <- hg_ag$tstop/30.45
hg_ag$group <- ifelse(hg_ag$birth_y <= 2010, "Vaccine non-eligible", "Vaccine-eligible")
hg_ag$gender <- factor(hg_ag$gender)
survobj <- with(hg_ag, Surv(tstart_m, tstop_m, visit))
dd <- with(hg_ag, datadist(gender, birth_y, cum_visit))
dd$limits$cum_visit[2] <- 0
options(datadist = "dd")
hg_cph <- cph(formula = survobj ~ cluster(id) + strat(gender) + rcs(cum_visit) * birth_y, data = hg_ag, x = T, y = T, surv = T)

hazardratio_fig <- cbind.data.frame(
    birth_y = c(as.character(2005:2009), as.character(2011:2015)),
    summary(hg_cph, cum_visit = 0, birth_y = 2010)[seq(4, 22, 2), c(4, 6, 7)]) %>%
    full_join(data.frame(birth_y = "2010 \n(Reference)", Effect = 1)) %>%
    mutate(birth_y = factor(
        birth_y,
        levels = c("2015", "2014", "2013", "2012", "2011", "2010 \n(Reference)", "2009", "2008", "2007", "2006", "2005"))) %>%
    ggplot(data = ., aes(x = birth_y, y = Effect)) + geom_hline(aes(yintercept = 1), lty = 2) + 
    geom_point() +
    geom_errorbar(aes(ymin = `Lower 0.95`, ymax = `Upper 0.95`), width = 0.2) +
    coord_flip(ylim = c(0.7,1.4)) +
    scale_y_log10(breaks = c(0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4)) +
    labs(x = NULL, y = "Hazard ratio")

ggsave(
    filename = paste0("_figures/paper_2/", Sys.Date(), "-hazardratio.pdf"), 
    plot = hazardratio_fig,
    width = 12, height = 8, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_2/", Sys.Date(), "-hazardratio.png"), 
    plot = hazardratio_fig ,
    width = 12, height = 8, units = "cm", dpi = 600)

### Nelson-Aalen cumulative mean ###
dd <- with(hg_ag, datadist(gender, group, cum_visit))
options(datadist = "dd")
dd$limits$cum_visit[3] <- 1
cox_hg <- cph(formula = Surv(tstart_m, tstop_m, visit) ~ cluster(id) + gender + rcs(cum_visit) * strat(group), data = hg_ag, x = T, y = T, surv = T)


nelsonaalen_fig <- ggsurvplot(
    survfit(cox_hg),
    data = hg_ag,
    break.x.by = 6,
    conf.int = T,
    censor = FALSE,
    xlab = "Age (months)",
    ylab = "Mean number of episodes",
    legend.labs = levels(factor(hg_ag$group)),
    fun = "cumhaz",
    legend.title = "",
    legend = c(0.80, 0.25),
    xlim = c(0, 36),
    ylim = c(0, 1.75),
    palette = "Set1",
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
    filename = paste0("_figures/paper_2/", Sys.Date(), "-nelsonaalen.pdf"), 
    plot = nelsonaalen_fig$plot,
    width = 12, height = 8, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_2/", Sys.Date(), "-nelsonaalen.png"), 
    plot = nelsonaalen_fig$plot ,
    width = 12, height = 8, units = "cm", dpi = 600)

### Hazard ratio per numer of previous visits ###
dd <- with(hg_ag, datadist(gender, group, cum_visit))
options(datadist = "dd")
dd$limits$cum_visit[2] <- 0
options(datadist = "dd")
cox_hg2 <- cph(formula = survobj ~ cluster(id) + strat(gender) + rcs(cum_visit) * group, data = hg_ag, x = T, y = T, surv = T)


x <- matrix(summary(cox_hg2, cum_visit = 0)[4,c(4,6,7)], ncol = 3)
for(i in 1:(hg_ag %>% summarise(n = max(cum_visit)) %>% .$n)){
    x <- rbind(x, summary(cox_hg2, cum_visit = i)[4,c(4,6,7)])
}

hr_previous <- x %>% 
    as.tibble() %>%
    mutate(cum_visit = 0:(nrow(.)-1)) %>%
    ggplot(aes(x = cum_visit)) +
    geom_line(aes(y = Effect)) +
    geom_ribbon(aes(ymin = `Lower 0.95`, ymax = `Upper 0.95`), alpha = .2) +
    geom_hline(yintercept = 1, lty = 2) +
    scale_x_continuous(breaks = 0:13, minor_breaks = NULL) +
    scale_y_continuous(breaks = seq(0.5, 1.2, 0.1)) +
    coord_trans(y = "log10", limy = c(0.5,1.2)) +
    labs(x = "No. of previous visits", y = "Hazard ratio (VEC/NVEC)") +
    theme_bw()

ggsave(
    filename = paste0("_figures/paper_2/", Sys.Date(), "-hr-previous.pdf"), 
    plot = hr_previous,
    width = 12, height = 8, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_2/", Sys.Date(), "-hr-previous.png"), 
    plot = hr_previous,
    width = 12, height = 8, units = "cm", dpi = 600)

### Incidence rate and incidence ratio figures ###
ageg_order <- c("0-3","4-7","8-11","12-15","16-19","20-23","24-27","28-31","32-35")

npy <- 
    expand.grid(birth_y = as.character(2005:2015), age_m = 0:35) %>%
    full_join(
        hg_ag %>%
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
            .$age_m >= 0 & .$age_m <= 3 ~ "0-3",
            .$age_m >= 4 & .$age_m <= 7 ~ "4-7",
            .$age_m >= 8 & .$age_m <= 11 ~ "8-11",
            .$age_m >= 12 & .$age_m <= 15 ~ "12-15",
            .$age_m >= 16 & .$age_m <= 19 ~ "16-19",
            .$age_m >= 20 & .$age_m <= 23 ~ "20-23",
            .$age_m >= 24 & .$age_m <= 27 ~ "24-27",
            .$age_m >= 28 & .$age_m <= 31 ~ "28-31",
            .$age_m >= 32 & .$age_m <= 35 ~ "32-35"
        ), 
        group_m = factor(group_m, levels = ageg_order, ordered = T)
    ) %>% 
    group_by(birth_y, group_m) %>%
    summarise(at_risk = round(sum(at_risk)/12)) %>%
    ungroup()

hg_n <-
    hg_ag %>%
    mutate(age_m = as.integer(tstop_m)) %>%
    group_by(birth_y, age_m) %>%
    summarise(visits = sum(visit)) %>%
    ungroup() %>%
    mutate(group_m =
               cut(
                   age_m,
                   breaks = c(0, 3, 7, 11, 15, 19, 23, 27, 31, 35),
                   labels = c("0-3", "4-7", "8-11", "12-15", "16-19", "20-23", "24-27", "28-31", "32-35"),
                   include.lowest = T
               )
    ) %>%
    mutate(group_m = factor(group_m, levels = ageg_order, ordered = T)) %>%
    group_by(birth_y, group_m) %>%
    summarise(visits = sum(visits)) %>%
    ungroup()

hg_ppy <-
    npy %>%
    left_join(hg_n)

hg_ppy[which(is.na(hg_ppy$visits)), "visits"] <- 0

temp <- matrix(NA, ncol = 3, nrow = dim(hg_ppy)[1])
for(i in 1:dim(hg_ppy)[1]){
    temp[i,1] <- tidy(poisson.test(hg_ppy$visits[i], hg_ppy$at_risk[i]))$estimate*100
    temp[i,2] <- tidy(poisson.test(hg_ppy$visits[i], hg_ppy$at_risk[i]))$conf.low*100
    temp[i,3] <- tidy(poisson.test(hg_ppy$visits[i], hg_ppy$at_risk[i]))$conf.high*100
}

hg_ppy <- 
    cbind(hg_ppy, temp) %>%
    rename(ir = `1`, lci = `2`, uci = `3`) %>%
    mutate(
        ir = format(round(ir, 2), nsmall = 2),
        lci = format(round(lci, 2), nsmall = 2),
        uci = format(round(uci, 2), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(ir, " ", "(", lci, "-", uci, ")"))

hg_ppy_group <-
    npy %>%
    left_join(hg_n)

hg_ppy_group[which(is.na(hg_ppy_group$visits)), "visits"] <- 0

hg_ppy_group <- 
    hg_ppy_group %>%
    mutate(group = ifelse(birth_y <= 2010, "nvec", "vec")) %>%
    group_by(group, group_m) %>%
    summarise(at_risk = sum(at_risk, na.rm = T), visits = sum(visits, na.rm = T)) %>%
    ungroup()

temp <- matrix(NA, ncol = 3, nrow = dim(hg_ppy_group)[1])
for(i in 1:dim(hg_ppy_group)[1]){
    temp[i,1] <- tidy(poisson.test(hg_ppy_group$visits[i], hg_ppy_group$at_risk[i]))$estimate*100
    temp[i,2] <- tidy(poisson.test(hg_ppy_group$visits[i], hg_ppy_group$at_risk[i]))$conf.low*100
    temp[i,3] <- tidy(poisson.test(hg_ppy_group$visits[i], hg_ppy_group$at_risk[i]))$conf.high*100
}

hg_ppy_group <- 
    cbind(hg_ppy_group, temp) %>%
    rename(ir = `1`, lci = `2`, uci = `3`) %>%
    mutate(
        ir = format(round(ir, 2), nsmall = 2), 
        lci = format(round(lci, 2), nsmall = 2), 
        uci = format(round(uci, 2), nsmall = 2)
    ) %>%
    mutate(ir_ci = paste0(ir, " ", "(", lci, "-", uci, ")"))


irr <-
    cbind.data.frame(
        group_m = hg_ppy_group %>% mutate(group_m = as.character(group_m)) %>% .$group_m %>% unique(),
        nvec_visits = hg_ppy_group %>% filter(group == "nvec") %>% .$visits,
        nvec_atrisk = hg_ppy_group %>% filter(group == "nvec") %>% .$at_risk,
        vec_visits = hg_ppy_group %>% filter(group == "vec") %>% .$visits,
        vec_atrisk = hg_ppy_group %>% filter(group == "vec") %>% .$at_risk
    )

temp <- matrix(NA, ncol = 4, nrow = dim(irr)[1])
for(i in 1:dim(irr)[1]){
    temp[i,1] <- tidy(poisson.test(c(irr$vec_visits[i], irr$nvec_visits[i]), c(irr$vec_atrisk[i], irr$nvec_atrisk[i])))$estimate
    temp[i,2] <- tidy(poisson.test(c(irr$vec_visits[i], irr$nvec_visits[i]), c(irr$vec_atrisk[i], irr$nvec_atrisk[i])))$conf.low
    temp[i,3] <- tidy(poisson.test(c(irr$vec_visits[i], irr$nvec_visits[i]), c(irr$vec_atrisk[i], irr$nvec_atrisk[i])))$conf.high
    temp[i,4] <- tidy(poisson.test(c(irr$vec_visits[i], irr$nvec_visits[i]), c(irr$vec_atrisk[i], irr$nvec_atrisk[i])))$p.value
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
incidence_fig <- hg_ppy %>%
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
    geom_vline(aes(xintercept = as.numeric(group_m) - 0.5), color = "grey") +
    geom_errorbar(aes(ymin = lci, ymax = uci),
                  width = 0.8,
                  position = position_dodge(width = 0.9)) +
    geom_point(position = position_dodge(width = 0.9), size = 0.3) +
    geom_text(aes(y = 105, label = ir_ci2), color = "black", size = 2.5) +
    scale_y_continuous(
        breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80),
        minor_breaks = c(5, 15, 25, 35, 45, 55, 65, 75, 85),
        limits = c(0, 120)
    ) +
    labs(x = "Age-groups (Months)", y = "Incidence rate per 100 person-years") +
    coord_flip(xlim = c(1.1, 8.9)) +
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
    filename = paste0("_figures/paper_2/", Sys.Date(), "-incidence.pdf"), 
    plot = incidence_fig,
    width = 12, height = 8, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_2/", Sys.Date(), "-incidence.png"), 
    plot = incidence_fig,
    width = 12, height = 8, units = "cm", dpi = 600)

### Test of proportions ###

crude_data <-
    hg_ag %>%
    filter(age >= 1095) %>%
    group_by(id) %>%
    filter(cum_visit == max(cum_visit)) %>%
    ungroup() %>%
    mutate(cum_visit == ifelse(.$visit == 0, .$cum_visit, .$cum_visit + 1)) %>% 
    group_by(group, cum_visit) %>%
    summarise(n = n_distinct(id)) %>%
    spread(key = cum_visit, value = n, fill = 0) %>%
    ungroup() %>%
    select(-group) %>%
    as.matrix()

crude_data %>% chisq.test()

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

crude_512 <-
    matrix(
        c(
            apply(crude_data[,c("5", "6", "7", "8", "9", "10", "11", "12")], MARGIN = 1, FUN = sum),
            apply(crude_data, MARGIN = 1, FUN = sum) -
                apply(crude_data[,c("5", "6", "7", "8", "9", "10", "11", "12")], MARGIN = 1, FUN = sum)
        ), nrow = 2, byrow = F)

dimnames(crude_512) <- list("Exposure" = c("non", "vacc"), "Fjoldi" = c("5-12", "allelse"))

epi_512 <- epi.2by2(crude_512[c(2,1),], method ="cohort.count", conf.level = 1-0.05)

save.image(file = paste0("_analyses/paper_2/", Sys.Date(), "-04-2-results-paper2", ".RData"))
