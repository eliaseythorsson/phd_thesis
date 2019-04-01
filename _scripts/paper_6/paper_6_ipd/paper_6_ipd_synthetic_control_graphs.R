library(tidyverse)
library(ggrepel)

theme_set(theme_bw(base_size = 10, base_family = "sans"))

paper_6_ipd_sensitivity_table <- read_delim("_analyses/paper_6/paper_6_ipd/Results_paper_6_ipd_2018-12-04-192314/paper_6_ipd_sensitivity_table_long.csv", 
                                            ";", escape_double = FALSE, trim_ws = TRUE)

sensitivity_table <- paper_6_ipd_sensitivity_table %>%
    mutate(
        age_group = fct_relevel(age_group, "0-4y", "5-64y", "65y+"),
        removed = fct_relevel(removed, "None", "First", "Second", "Third")) %>%
    ggplot(aes(x = age_group, color = removed, shape = removed, label = paste0(
        if_else(is.na(.$topctrl), "", paste0(.$topctrl, " (")),
        if_else(is.na(.$incl_prob), "",  paste0(.$incl_prob, ")"))
    ))) +
    geom_hline(aes(yintercept = 1), lty = 2) +
    geom_pointrange(aes(y = estimate, ymin = lower_ci, ymax = upper_ci), position = position_dodge(width = 0.5)) +
    geom_text_repel(aes(y = 1 + as.numeric(fct_rev(removed))/10), segment.colour = NA, nudge_y = 0.1, direction = "y", size = 3) +
    scale_y_log10(breaks = c(0.1, 0.5, 1, 2)) +
    annotation_logticks(sides = "l") +
    coord_cartesian(ylim = c(0.009, 3.5)) +
    labs(x = NULL, y = "Rate ratio") +
    theme(legend.title = element_blank(), legend.position = "bottom")


ggsave(
    filename = paste0("_figures/paper_6/paper_6_ipd/", Sys.Date(), "-ipd-sensitivity-table.pdf"), 
    plot = sensitivity_table,
    width = 12, height = 10, units = "cm", dpi = 300)

ggsave(
    filename = paste0("_figures/paper_6/paper_6_ipd/", Sys.Date(), "-ipd-sensitivity-table.png"), 
    plot = sensitivity_table,
    width = 12, height = 10, units = "cm", dpi = 300)

