library(tidyverse)

theme_set(theme_bw(base_size = 10, base_family = "sans"))

stack_ipd_base <- readRDS("_analyses/paper_6/paper_6_ipd/Results_paper_6_ipd_2018-12-04-192314/paper_6_ipdStack estimates.rds")
stack_ipd_vaccinetype <- readRDS("_analyses/paper_6/paper_6_ipd_vaccinetype/Results_paper_6_ipd_vaccinetype_2019-03-11-213809/paper_6_ipd_vaccinetypeStack estimates.rds")

case_def <- cbind.data.frame(
    "Base (all serotypes)" = stack_ipd_base$rr_mean_stack,
    "Vaccine-type IPD" = stack_ipd_vaccinetype$rr_mean_stack) %>%
    rownames_to_column(var = "age_group") %>%
    gather(key = case_def, value = estimate, -age_group) %>%
    separate(col = case_def, into = c("case_def", "interval"), sep = "\\.") %>%
    spread(key = interval, value = estimate) %>%
    mutate(age_group = fct_relevel(age_group, "0-4y", "5-64y", "65y+")) %>%
    ggplot(aes(x = age_group, color = case_def, shape = case_def)) +
    geom_hline(aes(yintercept = 1), lty = 2) +
    geom_pointrange(aes(y = `Point Estimate`, ymin = `Lower CI`, ymax = `Upper CI`), position = position_dodge(width = 0.5)) +
    scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2)) +
    coord_cartesian(ylim = c(0, 2)) +
    labs(x = NULL, y = "Rate ratio [not log transformed]") +
    theme(legend.title = element_blank(), legend.position = "bottom")

ggsave(
    filename = paste0("_figures/paper_6/paper_6_ipd/", Sys.Date(), "-ipd-case-def-sensitivity.pdf"), 
    plot = case_def,
    width = 12, height = 10, units = "cm", dpi = 300)

ggsave(
    filename = paste0("_figures/paper_6/paper_6_ipd/", Sys.Date(), "-ipd-case-def-sensitivity.png"), 
    plot = case_def,
    width = 12, height = 10, units = "cm", dpi = 300)

