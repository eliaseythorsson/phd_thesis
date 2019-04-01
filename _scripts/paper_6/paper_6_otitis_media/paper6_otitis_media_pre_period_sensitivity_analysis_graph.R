library(tidyverse)

theme_set(theme_bw(base_size = 10, base_family = "sans"))

stack_otitis_media_2005_2010 <- readRDS("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2018-12-02-131949/paper_6_otitis_mediaStack estimates.rds")
stack_otitis_media_2005_2009 <- readRDS("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2005_2009_2019-03-29-140541/paper_6_otitis_media_2005_2009Stack estimates.rds")
stack_otitis_media_2005_2008 <- readRDS("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2005_2007_2019-03-29-134610/paper_6_otitis_media_2005_2007Stack estimates.rds")
stack_otitis_media_2005_2007 <- readRDS("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2005_2007_2019-03-29-132651/paper_6_otitis_media_2005_2007Stack estimates.rds")

pca_otitis_media_2005_2010 <- read_csv("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2018-12-02-131949/paper_6_otitis_media_rr_pca.csv")
pca_otitis_media_2005_2009 <- read_csv("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2005_2009_2019-03-29-140541/paper_6_otitis_media_2005_2009_rr_pca.csv")
pca_otitis_media_2005_2008 <- read_csv("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2005_2007_2019-03-29-134610/paper_6_otitis_media_2005_2007_rr_pca.csv")
pca_otitis_media_2005_2007 <- read_csv("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2005_2007_2019-03-29-132651/paper_6_otitis_media_2005_2007_rr_pca.csv")

sc_otitis_media_2005_2010 <- read_csv("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2018-12-02-131949/paper_6_otitis_media_rr_full.csv")
sc_otitis_media_2005_2009 <- read_csv("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2005_2009_2019-03-29-140541/paper_6_otitis_media_2005_2009_rr_full.csv")
sc_otitis_media_2005_2008 <- read_csv("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2005_2007_2019-03-29-134610/paper_6_otitis_media_2005_2007_rr_full.csv")
sc_otitis_media_2005_2007 <- read_csv("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2005_2007_2019-03-29-132651/paper_6_otitis_media_2005_2007_rr_full.csv")

its_otitis_media_2005_2010 <- read_csv("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2018-12-02-131949/paper_6_otitis_media_rr_time_trend.csv")
its_otitis_media_2005_2009 <- read_csv("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2005_2009_2019-03-29-140541/paper_6_otitis_media_2005_2009_rr_time_trend.csv")
its_otitis_media_2005_2008 <- read_csv("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2005_2007_2019-03-29-134610/paper_6_otitis_media_2005_2007_rr_time_trend.csv")
its_otitis_media_2005_2007 <- read_csv("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2005_2007_2019-03-29-132651/paper_6_otitis_media_2005_2007_rr_time_trend.csv")


cbind.data.frame(
    "2005-2010" = stack_otitis_media_2005_2010$rr_mean_stack,
    "2005-2009" = stack_otitis_media_2005_2009$rr_mean_stack,
    "2005-2008" = stack_otitis_media_2005_2008$rr_mean_stack,
    "2005-2007" = stack_otitis_media_2005_2007$rr_mean_stack) %>%
    rownames_to_column(var = "age_group") %>%
    gather(key = pre_period, value = estimate, -age_group) %>%
    separate(col = pre_period, into = c("pre_period", "interval"), sep = "\\.") %>%
    spread(key = interval, value = estimate) %>%
    mutate(age_group = fct_relevel(age_group, "0y", "1y", "2y", "3-4y", "5-9y", "10-14y", "15-19y")) %>%
    ggplot(aes(x = age_group, color = pre_period, shape = pre_period)) +
    geom_hline(aes(yintercept = 1), lty = 2) +
    geom_pointrange(aes(y = `Point Estimate`, ymin = `Lower CI`, ymax = `Upper CI`), position = position_dodge(width = 0.5)) +
    scale_y_log10(breaks = c(0.1, 1, 10, 100)) +
    annotation_logticks(sides = "l") +
    coord_cartesian(ylim = c(0.39, 2.1)) +
    labs(x = NULL, y = "Rate ratio") +
    theme(legend.title = element_blank())


cbind.data.frame(
    pca_otitis_media_2005_2007[,1],
    "2005-2010" = pca_otitis_media_2005_2010[,2:4],
    "2005-2009" = pca_otitis_media_2005_2009[,2:4],
    "2005-2008" = pca_otitis_media_2005_2008[,2:4],
    "2005-2007" = pca_otitis_media_2005_2007[,2:4]) %>%
    rename(age_group = X1) %>%
    gather(key = pre_period, value = estimate, -age_group) %>%
    separate(col = pre_period, into = c("pre_period", "interval"), sep = "\\.") %>%
    spread(key = interval, value = estimate) %>%
    mutate(age_group = fct_relevel(age_group, "0y", "1y", "2y", "3-4y", "5-9y", "10-14y", "15-19y")) %>%
    ggplot(aes(x = age_group, color = pre_period, shape = pre_period)) +
    geom_hline(aes(yintercept = 1), lty = 2) +
    geom_pointrange(aes(y = `Point Estimate`, ymin = `Lower CI`, ymax = `Upper CI`), position = position_dodge(width = 0.5)) +
    scale_y_log10(breaks = c(0.1, 1, 10, 100)) +
    annotation_logticks(sides = "l") +
    coord_cartesian(ylim = c(0.39, 2.1)) +
    labs(x = NULL, y = "Rate ratio") +
    theme(legend.title = element_blank())

cbind.data.frame(
    sc_otitis_media_2005_2007[,1],
    "2005-2010" = sc_otitis_media_2005_2010[,2:4],
    "2005-2009" = sc_otitis_media_2005_2009[,2:4],
    "2005-2008" = sc_otitis_media_2005_2008[,2:4],
    "2005-2007" = sc_otitis_media_2005_2007[,2:4]) %>%
    rename(age_group = X1) %>%
    gather(key = pre_period, value = estimate, -age_group) %>%
    separate(col = pre_period, into = c("pre_period", "interval"), sep = "\\.") %>%
    spread(key = interval, value = estimate) %>%
    mutate(age_group = fct_relevel(age_group, "0y", "1y", "2y", "3-4y", "5-9y", "10-14y", "15-19y")) %>%
    ggplot(aes(x = age_group, color = pre_period, shape = pre_period)) +
    geom_hline(aes(yintercept = 1), lty = 2) +
    geom_pointrange(aes(y = `Point Estimate`, ymin = `Lower CI`, ymax = `Upper CI`), position = position_dodge(width = 0.5)) +
    scale_y_log10(breaks = c(0.1, 1, 10, 100)) +
    annotation_logticks(sides = "l") +
    coord_cartesian(ylim = c(0.39, 2.1)) +
    labs(x = NULL, y = "Rate ratio") +
    theme(legend.title = element_blank())

cbind.data.frame(
    its_otitis_media_2005_2007[,1],
    "2005-2010" = its_otitis_media_2005_2010[,2:4],
    "2005-2009" = its_otitis_media_2005_2009[,2:4],
    "2005-2008" = its_otitis_media_2005_2008[,2:4],
    "2005-2007" = its_otitis_media_2005_2007[,2:4]) %>%
    rename(age_group = X1) %>%
    gather(key = pre_period, value = estimate, -age_group) %>%
    separate(col = pre_period, into = c("pre_period", "interval"), sep = "\\.") %>%
    spread(key = interval, value = estimate) %>%
    mutate(age_group = fct_relevel(age_group, "0y", "1y", "2y", "3-4y", "5-9y", "10-14y", "15-19y")) %>%
    ggplot(aes(x = age_group, color = pre_period, shape = pre_period)) +
    geom_hline(aes(yintercept = 1), lty = 2) +
    geom_pointrange(aes(y = `Time_trend Point Estimate`, ymin = `Time_trend Lower CI`, ymax = `Time_trend Upper CI`), position = position_dodge(width = 0.5)) +
    scale_y_log10(breaks = c(0.1, 1, 10, 100)) +
    annotation_logticks(sides = "l") +
    coord_cartesian(ylim = c(0.39, 2.1)) +
    labs(x = NULL, y = "Rate ratio") +
    theme(legend.title = element_blank())

otitis_media_pre_period_sensitivity_analysis <- cbind.data.frame(
    "2005-2010" = stack_otitis_media_2005_2010$rr_mean_stack,
    "2005-2009" = stack_otitis_media_2005_2009$rr_mean_stack,
    "2005-2008" = stack_otitis_media_2005_2008$rr_mean_stack,
    "2005-2007" = stack_otitis_media_2005_2007$rr_mean_stack) %>%
    rownames_to_column(var = "age_group") %>%
    gather(key = pre_period, value = estimate, -age_group) %>%
    separate(col = pre_period, into = c("pre_period", "interval"), sep = "\\.") %>%
    spread(key = interval, value = estimate) %>%
    mutate(model = "Stacked") %>%
    union_all(
        cbind.data.frame(
            pca_otitis_media_2005_2007[,1],
            "2005-2010" = pca_otitis_media_2005_2010[,2:4],
            "2005-2009" = pca_otitis_media_2005_2009[,2:4],
            "2005-2008" = pca_otitis_media_2005_2008[,2:4],
            "2005-2007" = pca_otitis_media_2005_2007[,2:4]) %>%
            rename(age_group = X1) %>%
            gather(key = pre_period, value = estimate, -age_group) %>%
            separate(col = pre_period, into = c("pre_period", "interval"), sep = "\\.") %>%
            spread(key = interval, value = estimate) %>%
            mutate(model = "PCA")
    ) %>%
    union_all(
        cbind.data.frame(
            sc_otitis_media_2005_2007[,1],
            "2005-2010" = sc_otitis_media_2005_2010[,2:4],
            "2005-2009" = sc_otitis_media_2005_2009[,2:4],
            "2005-2008" = sc_otitis_media_2005_2008[,2:4],
            "2005-2007" = sc_otitis_media_2005_2007[,2:4]) %>%
            rename(age_group = X1) %>%
            gather(key = pre_period, value = estimate, -age_group) %>%
            separate(col = pre_period, into = c("pre_period", "interval"), sep = "\\.") %>%
            spread(key = interval, value = estimate) %>%
            mutate(model = "Synthetic Control")
    ) %>%
    union_all(
        cbind.data.frame(
            its_otitis_media_2005_2007[,1],
            "2005-2010" = its_otitis_media_2005_2010[,2:4],
            "2005-2009" = its_otitis_media_2005_2009[,2:4],
            "2005-2008" = its_otitis_media_2005_2008[,2:4],
            "2005-2007" = its_otitis_media_2005_2007[,2:4]) %>%
            rename(age_group = X1) %>%
            gather(key = pre_period, value = estimate, -age_group) %>%
            separate(col = pre_period, into = c("pre_period", "interval"), sep = "\\.") %>%
            spread(key = interval, value = estimate) %>%
            rename(`Lower CI` =  `Time_trend Lower CI`, `Point Estimate` = `Time_trend Point Estimate`, `Upper CI` = `Time_trend Upper CI` ) %>%
            mutate(model = "ITS")
    ) %>%
    mutate(
        age_group = fct_relevel(age_group, "0y", "1y", "2y", "3-4y", "5-9y", "10-14y", "15-19y"),
        model = fct_relevel(model, "Stacked", "Synthetic Control", "PCA", "ITS")) %>%
    ggplot(aes(x = age_group, color = pre_period, shape = pre_period)) +
    geom_hline(aes(yintercept = 1), lty = 2) +
    geom_pointrange(aes(y = `Point Estimate`, ymin = `Lower CI`, ymax = `Upper CI`), position = position_dodge(width = 0.5)) +
    scale_y_log10(breaks = c(0.1, 1, 10, 100)) +
    annotation_logticks(sides = "l") +
    coord_cartesian(ylim = c(0.39, 2.1)) +
    facet_wrap(~ model, ncol = 1) +
    labs(x = NULL, y = "Rate ratio") +
    theme(legend.title = element_blank(), legend.position = "bottom")

ggsave(
    filename = paste0("_figures/paper_6/paper_6_otitis_media/", Sys.Date(), "-otitis_media-pre-period-sensitivity-analysis.pdf"), 
    plot = otitis_media_pre_period_sensitivity_analysis,
    width = 12, height = 14, units = "cm", dpi = 300)

ggsave(
    filename = paste0("_figures/paper_6/paper_6_otitis_media/", Sys.Date(), "-otitis_media-pre-period-sensitivity-analysis.png"), 
    plot = otitis_media_pre_period_sensitivity_analysis,
    width = 12, height = 14, units = "cm", dpi = 300)