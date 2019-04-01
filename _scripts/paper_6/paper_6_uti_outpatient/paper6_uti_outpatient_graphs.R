source("_scripts/paper_6/paper_6_uti_outpatient/paper_6_uti_outpatient_synthetic_control_functions.R", local = T)

input_directory  <- "_data/paper_6/paper_6_uti_outpatient/" # Directory (or URL) containing input data file.
file_name <- "input-data.csv"
data_file <- paste0(input_directory, file_name)
prelog_data <- read.csv(data_file, check.names = FALSE)

group_name   <- 'age_group' # Name of column containing group labels.
date_name    <- 'date'      # Name of column containing dates.
outcome_name <- 'N10_N39'    # Name of column containing outcome.
denom_name   <- "ach_noj"   # Name of column containing denominator to be used in offset.

#MOST DATES MUST BE IN FORMAT "YYYY-MM-01", exception is end of pre period, which is 1 day before end of post period
start_date        <- as.Date('2005-01-01') # Indicates the date of the first data point.
intervention_date <- as.Date('2010-12-31') # Indicates the date of intervention in the data.
end_date          <- as.Date('2015-12-01') # Indicates the date of the last data point.
pre_period        <- as.Date(c('2005-01-01', '2010-12-31')) # Range over which the data is trained for the CausalImpact model.
post_period       <- as.Date(c('2011-01-01', '2015-12-01')) # Range from the intervention date to the end date.
eval_period       <- as.Date(c('2013-01-01', '2015-12-01')) # Range over which rate ratio calculation will be performed.
year_def   <- 'cal_year'  #Can be cal_year to aggregate results by Jan-Dec; 'epi_year' to aggregate July-June
n_seasons <- 12

groups <- c("0y", "1y", "2y", "3-4y", "5-9y", "10-14y", "15-19y")
time_points <- as.character(seq.Date(from = as.Date("2005-01-01"), to = as.Date("2015-12-01"), by = "month"))

packages <-
    c(
        'parallel',
        'splines',
        'lubridate',
        'loo',
        'RcppRoll',
        'pomp',
        'lme4',
        'BoomSpikeSlab',
        'ggplot2',
        'reshape',
        'dummies'
    )
packageHandler(packages, update_packages, install_packages)
sapply(packages,
       library,
       quietly = TRUE,
       character.only = TRUE)

library(tidyverse)
library(lubridate)
library(zoo)
library(ggpubr)

ids <- read_rds("_data/results/ids.rds")
lsh <- read_rds("_data/results/lsh.rds")
impact_full <- read_rds("_analyses/paper_6/paper_6_uti_outpatient/Results_paper_6_uti_outpatient_2019-03-10-183718/paper_6_uti_outpatient_synthetic_control_model.rds")
impact_stack <- read_rds("_analyses/paper_6/paper_6_uti_outpatient/Results_paper_6_uti_outpatient_2019-03-10-183718/paper_6_uti_outpatient_stacked_model.rds")


# set theme for ggplot2 figures
theme_set(theme_bw(base_size = 10, base_family = "sans"))

uti_number <- prelog_data %>% 
    mutate(
        date = as.Date(date),
        age_group = fct_relevel(
            age_group, "0y", "1y", "2y", "3-4y", "5-9y", "10-14y", "15-19y")
    ) %>%
    select(age_group, date, n = N10_N39) %>%
    ggplot(aes(x = date, y = n, color = age_group)) +
    geom_line(show.legend = F) +
    geom_vline(aes(xintercept = as.Date("2011-01-01")), lty = 2) +
    scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
    scale_color_brewer(palette = "Set1") +
    labs(y = "Number of visits", x = "Calendar-month")

uti_standard <- prelog_data %>% 
    mutate(
        date = as.Date(date),
        age_group = fct_relevel(
            age_group, "0y", "1y", "2y", "3-4y", "5-9y", "10-14y", "15-19y")
    ) %>%
    select(-ach_noj, -Z00_Z99, -A10_B99, -R00_R99, -U00_U99, -N00_N99) %>%
    group_by(age_group) %>%
    mutate_if(.predicate = is.numeric, .funs = scale) %>%
    ungroup() %>%
    gather(key = diagnosis, value = n, -age_group, -date, -N10_N39) %>%
    ggplot(aes(x = date, y = N10_N39, group = interaction(age_group, diagnosis), color = age_group)) +
    geom_line(alpha = 0.3, size = 0.2) +
    geom_smooth(aes(group = age_group, color = age_group), alpha = 0.5, method = "loess", se = F) +
    geom_hline(aes(yintercept = 0), lty = 2) +
    geom_vline(aes(xintercept = as.Date("2011-01-01")), lty = 2) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_y_continuous(breaks = -2:2, limits = c(-3,3)) +
    scale_color_brewer(palette = "Set1") +
    coord_cartesian(ylim = c(-1.5, 1.5)) +
    labs(y = "Standardized visits", x = "Calendar-month") +
    guides(color = guide_legend(ncol = 7, override.aes = list(fill = NA))) +
    theme(legend.title = element_blank())

else_standard <- prelog_data %>% 
    mutate(
        date = as.Date(date),
        age_group = fct_relevel(
            age_group, "0y", "1y", "2y", "3-4y", "5-9y", "10-14y", "15-19y")
    ) %>%
    select(-ach_noj, -Z00_Z99, -A10_B99, -R00_R99, -U00_U99) %>%
    group_by(age_group) %>%
    mutate_if(.predicate = is.numeric, .funs = scale) %>%
    ungroup() %>%
    gather(key = diagnosis, value = n, -age_group, -date, -N10_N39) %>%
    ggplot(aes(x = date, y = n, group = interaction(age_group, diagnosis), color = age_group)) +
    geom_line(alpha = 0.3, size = 0.2) +
    geom_smooth(aes(group = age_group, color = age_group), alpha = 0.5, method = "loess", se = F) +
    geom_hline(aes(yintercept = 0), lty = 2) +
    geom_vline(aes(xintercept = as.Date("2011-01-01")), lty = 2) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_y_continuous(breaks = -2:2, limits = c(-3,3)) +
    scale_color_brewer(palette = "Set1") +
    coord_cartesian(ylim = c(-1.5, 1.5)) +
    labs(y = "Standardized visits", x = "Calendar-month") +
    guides(color = guide_legend(ncol = 7, override.aes = list(fill = NA))) +
    theme(legend.title = element_blank())

uti_arranged_paper6 <- ggarrange(
    plotlist = list(
        uti_number,
        ggarrange(
            plotlist =
                list(uti_standard, else_standard),
            common.legend = T,
            legend = "bottom",
            labels = c("B", "C"),
            font.label = list(size = 10, family = "sans"))
    ),
    ncol = 1,
    nrow = 2,
    labels = c("A", ""),
    font.label = list(size = 10, family = "sans"))

ggsave(
    filename = paste0("_figures/paper_6/paper_6_uti_outpatient/", Sys.Date(), "-uti-arranged.pdf"), 
    plot = uti_arranged_paper6,
    width = 12, height = 14, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_6/paper_6_uti_outpatient/", Sys.Date(), "-uti-arranged.png"),
    plot = uti_arranged_paper6,
    width = 12, height = 14, units = "cm", dpi = 600)


age_groups <- c("0y", "1y", "2y", "3-4y", "5-9y", "10-14y", "15-19y")
list_groups <- as.list(age_groups)
for(i in age_groups){
    list_groups[[i]] <- impact_stack[[i]]$all.preds
}

pred_groups <- do.call(rbind.data.frame, list_groups)[-(1:7),] %>%
    rownames_to_column() %>%
    separate(
        col = "rowname",
        into = c("age_group", "row_number"),
        sep = "\\."
    ) %>%
    group_by(age_group) %>%
    mutate(date = as.character(seq.Date(
        from = as.Date("2005-01-01"),
        to = as.Date("2015-12-31"),
        by = "1 months"
    ))) %>%
    left_join(prelog_data %>% select(age_group, date, N10_N39)) %>%
    ungroup() %>%
    rename("Synthetic control" = pred.full,
           "ITS with offset" = pred.time,
           "ITS without offset" = pred.time.no_offset,
           "STL + PCA" = pred.pca) %>%
    mutate(age_group = fct_relevel(age_group, "0y", "1y", "2y", "3-4y", "5-9y", "10-14y", "15-19y")) %>%
    gather(key = model, value = pred, -age_group, -date, -N10_N39, -row_number) %>%
    mutate(row_number = as.integer(row_number),
           date = as.Date(date), 
           pred = as.numeric(pred))

predict_models <- pred_groups %>%
    ggplot(aes(x = date, y = N10_N39)) +
    geom_line(aes(y = pred, color = model)) +
    geom_segment(aes(xend = date, y = pred, yend = N10_N39), size = 0.3) +
    geom_point(size = 0.3) + 
    geom_vline(aes(xintercept = as.Date("2011-01-01")), lty = 2) +
    facet_wrap(~ age_group, ncol = 1, scales = "free_y", strip.position = "right") +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_color_brewer(palette = "Set2") +
    labs(x = NULL, y = "Number of visits") +
    guides(color = guide_legend(ncol = 4)) +
    theme(legend.title = element_blank(), legend.position = "bottom")

ggsave(
    filename = paste0("_figures/paper_6/paper_6_uti_outpatient/", Sys.Date(), "-predict-models.pdf"), 
    plot = predict_models,
    width = 12, height = 14, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_6/paper_6_uti_outpatient/", Sys.Date(), "-predict-models.png"),
    plot = predict_models,
    width = 12, height = 14, units = "cm", dpi = 600)



list_groups <- as.list(age_groups)
for(i in age_groups){
    list_groups[[i]] <- impact_stack[[i]]$pred.stack.q
}

stack_groups <- do.call(rbind.data.frame, list_groups)[-(1:7),] %>%
    rownames_to_column() %>%
    separate(
        col = "rowname",
        into = c("age_group", "row_number"),
        sep = "\\."
    ) %>%
    group_by(age_group) %>%
    mutate(date = as.character(seq.Date(
        from = as.Date("2005-01-01"),
        to = as.Date("2015-12-31"),
        by = "1 months"
    ))) %>%
    left_join(prelog_data %>% select(age_group, date, N10_N39)) %>%
    ungroup() %>%
    rename("lower_quarter" = `2.5%`,
           "median" = `50%`,
           "upper_quarter" = `97.5%`) %>%
    mutate(age_group = fct_relevel(age_group, "0y", "1y", "2y", "3-4y", "5-9y", "10-14y", "15-19y")) %>%
    mutate(row_number = as.integer(row_number),
           date = as.Date(date), 
           lower_quarter = as.numeric(lower_quarter),
           median = as.numeric(median),
           upper_quarter = as.numeric(upper_quarter))


stacked_models <- stack_groups %>%
    ggplot(aes(x = date, y = N10_N39, group = age_group)) +
    geom_line(aes(y = median, color = age_group)) +
    geom_ribbon(aes(ymin = lower_quarter, ymax = upper_quarter, fill = age_group), alpha = 0.4) +
    geom_segment(aes(xend = date, y = median, yend = N10_N39), size = 0.3) +
    geom_point(size = 0.3) + 
    geom_vline(aes(xintercept = as.Date("2011-01-01")), lty = 2) +
    facet_wrap(~ age_group, ncol = 1, scales = "free_y", strip.position = "right") +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    labs(x = NULL, y = "Number of visits") +
    theme(legend.position = "None")

ggsave(
    filename = paste0("_figures/paper_6/paper_6_uti_outpatient/", Sys.Date(), "-stacked-models.pdf"), 
    plot = stacked_models,
    width = 12, height = 14, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_6/paper_6_uti_outpatient/", Sys.Date(), "-stacked-models.png"),
    plot = stacked_models,
    width = 12, height = 14, units = "cm", dpi = 600)



quantiles_stack <-
    setNames(lapply(
        groups,
        FUN = function(group) {
            rrPredQuantiles(
                impact = impact_stack[[group]],
                denom_data = ds[[group]][, denom_name],
                eval_period = eval_period,
                post_period = post_period
            )
        }
    ), groups)

rr_roll_stack <-
    sapply(
        quantiles_stack,
        FUN = function(quantiles_stack) {
            quantiles_stack$roll_rr
        },
        simplify = 'array'
    )

rolling_rate <- as.data.frame.table(rr_roll_stack, stringsAsFactors = F) %>%
    dplyr::select(quantile = Var2, age_group = Var3, rate = Freq) %>%
    group_by(age_group, quantile) %>% 
    mutate(date = seq.Date(
        from = as.Date("2010-12-01"),
        to = as.Date("2015-12-31"),
        by = "1 months")) %>%
    ungroup() %>%
    spread(key = quantile, value = rate) %>%
    dplyr::rename("lower_quarter" = `2.5%`,
           "median" = `50%`,
           "upper_quarter" = `97.5%`) %>%
    mutate(age_group = fct_relevel(age_group, "0y", "1y", "2y", "3-4y", "5-9y", "10-14y", "15-19y")) %>%
    ggplot(aes(x = date, y = median, group = age_group)) +
    geom_line(aes(color = age_group)) +
    geom_ribbon(aes(ymin = lower_quarter, ymax = upper_quarter, fill = age_group), alpha = 0.5) +
    geom_hline(aes(yintercept = 1), lty = 2) +
    facet_wrap(~ age_group, ncol = 1, strip.position = "right") +
    scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
    scale_y_continuous(limits = c(0.2, 20)) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    coord_trans(y = "log10", limy = c(0.4, 2.5)) +
    labs(x = NULL, y = "Rolling rate ratio")+
    theme(legend.position = "None")


ggsave(
    filename = paste0("_figures/paper_6/paper_6_uti_outpatient/", Sys.Date(), "-rolling-rate.pdf"), 
    plot = rolling_rate,
    width = 12, height = 14, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_6/paper_6_uti_outpatient/", Sys.Date(), "-rolling-rate.png"),
    plot = rolling_rate,
    width = 12, height = 14, units = "cm", dpi = 600)

prelog_data[, date_name] <-
    as.Date(as.character(prelog_data[, date_name]),
            tryFormats = c("%m/%d/%Y", '%Y-%m-%d'))


prelog_data[, date_name] <- formatDate(prelog_data[, date_name])

prelog_data <-
    setNames(
        lapply(
            groups,
            FUN = splitGroup,
            ungrouped_data = prelog_data,
            group_name = group_name,
            date_name = date_name,
            start_date = start_date,
            end_date = end_date,
            no_filter = c(group_name, date_name, outcome_name, denom_name)
        ),
        groups
    )

outcome <-
    sapply(
        prelog_data,
        FUN = function(data) {
            data[, outcome_name]
        }
    )

quantiles_full <-
    setNames(lapply(
        groups,
        FUN = function(group) {
            rrPredQuantiles(
                impact = impact_stack[[group]],
                denom_data = ds[[group]][, denom_name],
                eval_period = eval_period,
                post_period = post_period
            )
        }
    ), groups)

cumsum_prevented <-
    sapply(groups,
           FUN = cumsum_func,
           quantiles = quantiles_full,
           simplify = 'array')

cumsum_all_func <- function(group, quantiles) {
    is_post_period <- which(time_points >= post_period[1])
    is_pre_period <- which(time_points < post_period[1])
    
    #Cumulative sum of prevented cases
    cases_prevented <- quantiles[[group]]$pred_samples - outcome[, group]
    cumsum_prevented_post <- apply(cases_prevented[is_post_period,], 2, cumsum)
    cumsum_prevented_pre <-
        matrix(0,
               nrow = nrow(cases_prevented[is_pre_period,]),
               ncol = ncol(cases_prevented[is_pre_period,]))
    
    cumsum_prevented_all <- rbind(cumsum_prevented_pre, cumsum_prevented_post)
}

cumsum_prevented_all <-
    setNames(lapply(groups,
                    FUN = cumsum_all_func,
                    quantiles = quantiles_full), groups)

cumsum_prevented_all <- Reduce("+", cumsum_prevented_all)

cumsum_prevented_all <-
    t(apply(
        cumsum_prevented_all,
        1,
        quantile,
        probs = c(0.025, 0.5, 0.975),
        na.rm = TRUE
    ))

rolling_rate_one <- as.data.frame.table(rr_roll_stack, stringsAsFactors = F) %>%
    dplyr::select(quantile = Var2, age_group = Var3, rate = Freq) %>%
    group_by(age_group, quantile) %>% 
    mutate(date = seq.Date(
        from = as.Date("2010-12-01"),
        to = as.Date("2015-12-31"),
        by = "1 months")) %>%
    ungroup() %>%
    spread(key = quantile, value = rate) %>%
    dplyr::rename("lower_quarter" = `2.5%`,
                  "median" = `50%`,
                  "upper_quarter" = `97.5%`) %>%
    mutate(age_group = fct_relevel(age_group, "0y", "1y", "2y", "3-4y", "5-9y", "10-14y", "15-19y")) %>%
    ggplot(aes(x = date, y = median, group = age_group)) +
    geom_ribbon(aes(ymin = lower_quarter, ymax = upper_quarter, fill = age_group), alpha = 0.25) +
    geom_line(aes(color = age_group), size = 0.6) +
    geom_hline(aes(yintercept = 1), lty = 2) +
    scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
    scale_y_continuous(limits = c(0.2, 10)) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    coord_trans(y = "log10", limy = c(0.40, 2.5)) +
    labs(x = NULL, y = "Rolling rate ratio") +
    guides(color = guide_legend(ncol = 7, override.aes = list(fill = NA))) +
    theme(legend.position = "None", legend.background = element_rect(fill = alpha("white", 0)))

cum_prevented_group <- as.data.frame.table(cumsum_prevented, stringsAsFactors = F) %>%
    dplyr::select(quantile = Var2, age_group = Var3, rate = Freq) %>%
    group_by(age_group, quantile) %>% 
    mutate(date = seq.Date(
        from = as.Date("2005-01-01"),
        to = as.Date("2015-12-31"),
        by = "1 months")) %>%
    ungroup() %>%
    spread(key = quantile, value = rate) %>%
    dplyr::rename("lower_quarter" = `2.5%`,
                  "median" = `50%`,
                  "upper_quarter" = `97.5%`) %>%
    mutate(age_group = fct_relevel(age_group, "0y", "1y", "2y", "3-4y", "5-9y", "10-14y", "15-19y")) %>%
    filter(date >= as.Date("2011-01-01")) %>%
    ggplot(aes(x = date, y = median, group = age_group)) +
    geom_ribbon(aes(ymin = lower_quarter, ymax = upper_quarter, fill = age_group), alpha = 0.25) +
    geom_line(aes(color = age_group), size = 0.6) +
    geom_hline(aes(yintercept = 0), lty = 2) +
    scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
    scale_y_continuous(labels = scales::comma) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    labs(y = "Prevented cases", x = NULL) +
    guides(color = guide_legend(ncol = 2, override.aes = list(fill = NA))) +
    theme(legend.title = element_blank(), legend.position = c(0.2, 0.6), legend.background = element_rect(fill = alpha("white", 0)))

cum_prevented <- cumsum_prevented_all %>%
    tbl_df %>%
    mutate(date = seq.Date(
        from = as.Date("2005-01-01"),
        to = as.Date("2015-12-31"),
        by = "1 months")) %>%
    dplyr::rename("lower_quarter" = `2.5%`,
                  "median" = `50%`,
                  "upper_quarter" = `97.5%`) %>%
    filter(date >= as.Date("2011-01-01")) %>%
    ggplot(aes(x = date, y = median)) +
    geom_ribbon(aes(ymin = lower_quarter, ymax = upper_quarter), alpha = 0.25) +
    geom_line(size = 0.6) +
    geom_hline(aes(yintercept = 0), lty = 2) +
    scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
    scale_y_continuous(labels = scales::comma) +
    labs(y = "Total \nprevented cases", x = "Calendar-month")


arranged_cum_prevented <- ggarrange(
    plotlist = list(rolling_rate_one, cum_prevented_group, cum_prevented),
    ncol = 1,
    nrow = 3,
    labels = c("A", "B", "C"),
    font.label = list(size = 10, family = "sans")
)

ggsave(
    filename = paste0("_figures/paper_6/paper_6_uti_outpatient/", Sys.Date(), "-cum-arranged.pdf"), 
    plot = arranged_cum_prevented,
    width = 12, height = 14, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/paper_6/paper_6_uti_outpatient/", Sys.Date(), "-cum-arranged.png"),
    plot = arranged_cum_prevented,
    width = 12, height = 14, units = "cm", dpi = 600)
