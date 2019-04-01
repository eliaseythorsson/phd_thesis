library(tidyverse)
library(ggrepel)

theme_set(theme_bw(base_size = 10, base_family = "sans"))

stack_otitis_media_base <- readRDS("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2018-12-02-131949/paper_6_otitis_mediaStack estimates.rds")
stack_otitis_media_H66 <- readRDS("_analyses/paper_6/paper_6_otitis_media_H66/Results_paper_6_otitis_media_H66_2019-03-17-150828/paper_6_otitis_media_H66Stack estimates.rds")
stack_otitis_media_antimicrobials<- readRDS("_analyses/paper_6/paper_6_otitis_media_antimicrobials/Results_paper_6_otitis_media_antimicrobials_2019-03-17-143555/paper_6_otitis_media_antimicrobialsStack estimates.rds")

case_def <- cbind.data.frame(
    "Base" = stack_otitis_media_base$rr_mean_stack,
    "H66" = stack_otitis_media_H66$rr_mean_stack,
    "Base + antimicrobial" = stack_otitis_media_antimicrobials$rr_mean_stack) %>%
    rownames_to_column(var = "age_group") %>%
    gather(key = case_def, value = estimate, -age_group) %>%
    separate(col = case_def, into = c("case_def", "interval"), sep = "\\.") %>%
    spread(key = interval, value = estimate) %>%
    mutate(age_group = fct_relevel(age_group, "0y", "1y", "2y", "3-4y", "5-9y", "10-14y", "15-19y")) %>%
    ggplot(aes(x = age_group, color = case_def, shape = case_def)) +
    geom_hline(aes(yintercept = 1), lty = 2) +
    geom_pointrange(aes(y = `Point Estimate`, ymin = `Lower CI`, ymax = `Upper CI`), position = position_dodge(width = 0.5)) +
    scale_y_log10(breaks = c(0.1, 1, 10, 100)) +
    annotation_logticks(sides = "l") +
    coord_cartesian(ylim = c(0.49, 1.1)) +
    labs(x = NULL, y = "Rate ratio") +
    theme(legend.title = element_blank(), legend.position = "bottom")

ggsave(
    filename = paste0("_figures/paper_6/paper_6_otitis_media/", Sys.Date(), "-otitis_media-case-def-sensitivity.pdf"), 
    plot = case_def,
    width = 12, height = 10, units = "cm", dpi = 300)

ggsave(
    filename = paste0("_figures/paper_6/paper_6_otitis_media/", Sys.Date(), "-otitis_media-case-def-sensitivity.png"), 
    plot = case_def,
    width = 12, height = 10, units = "cm", dpi = 300)

