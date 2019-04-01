library(tidyverse)

theme_set(theme_bw(base_size = 10, base_family = "sans"))

stack_pneumonia_base <- readRDS("_analyses/paper_6/paper_6_pneumonia/Results_paper_6_pneumonia_2018-12-02-141443/paper_6_pneumoniaStack estimates.rds")
stack_pneumonia_J13J18 <- readRDS("_analyses/paper_6/paper_6_pneumonia_J13_J18/Results_paper_6_pneumonia_J13_J18_2019-03-17-183007/paper_6_pneumonia_J13_J18Stack estimates.rds")
stack_pneumonia_diagnostics <- readRDS("_analyses/paper_6/paper_6_pneumonia_J13_J18_diagnostics/Results_paper_6_pneumonia_J13_J18_diagnostics_2019-03-17-185822/paper_6_pneumonia_J13_J18_diagnosticsStack estimates.rds")

case_def <- cbind.data.frame(
    "Base" = stack_pneumonia_base$rr_mean_stack,
    "J13-J18" = stack_pneumonia_J13J18$rr_mean_stack,
    "J13-J18 + tests" = stack_pneumonia_diagnostics$rr_mean_stack) %>%
    rownames_to_column(var = "age_group") %>%
    gather(key = case_def, value = estimate, -age_group) %>%
    separate(col = case_def, into = c("case_def", "interval"), sep = "\\.") %>%
    spread(key = interval, value = estimate) %>%
    mutate(age_group = fct_relevel(age_group, "0-4y", "5-19y", "20-39y", "40-64y", "65-79y", "80+")) %>%
    ggplot(aes(x = age_group, color = case_def, shape = case_def)) +
    geom_hline(aes(yintercept = 1), lty = 2) +
    geom_pointrange(aes(y = `Point Estimate`, ymin = `Lower CI`, ymax = `Upper CI`), position = position_dodge(width = 0.5)) +
    scale_y_log10(breaks = c(0.1, 1, 10, 100)) +
    annotation_logticks(sides = "l") +
    coord_cartesian(ylim = c(0.04, 4.1)) +
    labs(x = NULL, y = "Rate ratio") +
    theme(legend.title = element_blank(), legend.position = "bottom")

ggsave(
    filename = paste0("_figures/paper_6/paper_6_pneumonia/", Sys.Date(), "-pneumonia-case-def-sensitivity.pdf"), 
    plot = case_def,
    width = 12, height = 10, units = "cm", dpi = 300)

ggsave(
    filename = paste0("_figures/paper_6/paper_6_pneumonia/", Sys.Date(), "-pneumonia-case-def-sensitivity.png"), 
    plot = case_def,
    width = 12, height = 10, units = "cm", dpi = 300)

