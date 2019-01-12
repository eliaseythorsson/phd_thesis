# Cost bootstrap

library(tidyverse)
library(zoo)

############################
#### Importing the data ####
############################

ids <- read_rds("_data/results/ids.rds")
lsh <- read_rds("_data/results/lsh.rds")
impact_full <- read_rds("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2018-12-02-131949/paper_6_otitis_media_synthetic_control_model.rds")
impact_stack <- read_rds("_analyses/paper_6/paper_6_otitis_media/Results_paper_6_otitis_media_2018-12-02-131949/paper_6_otitis_media_stacked_model.rds")

source("_scripts/paper_6/paper_6_otitis_media/paper_6_otitis_media_synthetic_control_functions.R", local = T)

# Assign variable values
country       <- 'paper_6_otitis_media' # Country or region name.
n_seasons     <- 12       # Number of months (seasons) per year. 12 for monthly, 4 for quarterly, 3 for trimester data.
exclude_covar <- c("Z00_Z99", "A10_B99", "R00_R99", "U00_U99", "K80", "I60_I64", "E86", "E40_E46", "E10_E14") # User-defined list of covariate columns to exclude from all analyses.
exclude_group <- c()      # User-defined list of groups to exclude from analyses.
if(country=="Brazil"){code_change   <- TRUE     # Used for Brazil data. Set to TRUE to adjust for year 2008 coding changes; otherwise, set to FALSE.
}else{
    code_change   <- FALSE
}

input_directory  <- "_data/paper_6/paper_6_otitis_media/" # Directory (or URL) containing input data file.
file_name <- "input-data.csv"
output_directory <- '_analyses/paper_6/paper_6_otitis_media/Results'   # Directory where results will be saved.
output_directory <- paste(output_directory, '_', country,'_', format(Sys.time(), '%Y-%m-%d-%H%M%S'), '/', sep = '')                     #Adds a subfolder to output directory to organize results by date and time run.
data_file <- paste0(input_directory, file_name)
prelog_data <- read.csv(data_file, check.names = FALSE) # IF IMPORTING FROM LOCAL

group_name   <- 'age_group' # Name of column containing group labels.
date_name    <- 'date'      # Name of column containing dates.
outcome_name <- 'H65_H73'    # Name of column containing outcome.
denom_name   <- "ach_noj"   # Name of column containing denominator to be used in offset.

#MOST DATES MUST BE IN FORMAT "YYYY-MM-01", exception is end of pre period, which is 1 day before end of post period
start_date        <- as.Date('2005-01-01') # Indicates the date of the first data point.
intervention_date <- as.Date('2010-12-31') # Indicates the date of intervention in the data.
end_date          <- as.Date('2015-12-01') # Indicates the date of the last data point.
pre_period        <- as.Date(c('2005-01-01', '2010-12-31')) # Range over which the data is trained for the CausalImpact model.
post_period       <- as.Date(c('2011-01-01', '2015-12-01')) # Range from the intervention date to the end date.
eval_period       <- as.Date(c('2013-01-01', '2015-12-01')) # Range over which rate ratio calculation will be performed.
year_def   <- 'cal_year'  #Can be cal_year to aggregate results by Jan-Dec; 'epi_year' to aggregate July-June

### Consumer Price Index found at www.statice.is ### 

consumer_price_index <- bind_cols( #consumer price index
    date = as.character(seq.Date(from = as.Date("2005-01-01"), to = as.Date("2015-12-01"), by = "months")),
    cpi =c(140.3, 141.9, 142.5, 142.9, 142.8, 142.5, 143.3, 143.6, 143.8, 144.6, 144.7, 144.4, 145, 145.1, 145.9, 146.7, 147.3, 148.5, 150.1, 150.2, 150, 149.7, 149.6, 150.4, 151.3, 151.7, 151.9, 152.1, 151.8, 152.3, 153.8, 154.2, 154.8, 154.7, 155.4, 155.6, 158, 158.2, 158.4, 162.6, 162.9, 164, 164.4, 163.3, 164.1, 167.2, 172.3, 178.6, 183.3, 183.2, 182.3, 180.4, 182.4, 184.2, 186.1, 186.5, 187.7, 189.5, 190.1, 190.4, 193.2, 193.5, 196.1, 196.4, 197, 198, 197.5, 197.4, 197.7, 197, 197.1, 197.1, 198.3, 196.3, 196.6, 197.2, 197, 199.9, 200.4, 198.2, 199.3, 199.5, 199.8, 200.9, 203.8, 204.5, 203.9, 204.8, 204.9, 205.6, 205, 204.4, 202.7, 205.3, 206.7, 207.6, 210.7, 213.3, 213.9, 213.7, 209.9, 210.4, 211.4, 212, 208.6, 210.2, 211.3, 211.8, 217.2, 217.9, 218.8, 221.4, 222.1, 222.1, 223.6, 223.6, 223.7, 224.1, 224.2, 224.5, 224.9, 225.6, 226.5, 227.5, 227, 226.7, 227.6, 227.9, 227.5, 228, 228.6, 228.7),
    cpi_jan2011 = rep(198.3, 132)
)

### Cost of vaccine found in personal communication with Sóttvarnalæknir ### 

cost_vaccine <- bind_cols(
    price = c(rep(0, 72), rep(5505, 12), rep(5153, 12), rep(5064, 12), rep(5050, 12), rep(5202, 12)),
    number_doses = c(rep(0, 72), rep(7447/12, 12), rep(12557/12, 12), rep(12887/12, 12), rep(12953/12, 12), rep(12569/12, 12)),
    consumer_price_index) %>%
    mutate(price = price * number_doses * cpi_jan2011/cpi) %>%
    .$price

### Calculated distributions of absent days, wage ###

dist_absent_days <- rpois(n = 1000, lambda = 1) # assumed, no data

### Parameters from lognormal distribution found by fitting decils of wage from www.statice.is ... ###
### ... to lognormal distribution using the get.lnorm.par() function from the rriskDistributions package ###
dist_wage <- rlnorm(n = 1000, meanlog = 12.85443, sdlog = 0.35430) 

wage_per_day <- 0.010385 #standard devision of wage to get wage/day


### Packages ###

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

groups <- c("0y", "1y", "2y", "3-4y", "5-9y", "10-14y", "15-19y")
time_points <- as.character(seq.Date(from = as.Date("2005-01-01"), to = as.Date("2015-12-01"), by = "month"))

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

ggplot(data = cbind.data.frame(
    date = seq.Date(
        from = as.Date("2005-01-01"),
        to = as.Date("2015-12-01"),
        by = "months"
    ),
    cumsum_prevented_all
),
aes(x = date, y = `50%`)) +
    geom_vline(aes(xintercept = as.Date("2011-01-01")), lty = 2) +
    geom_line() +
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.3) +
    scale_y_continuous(labels = scales::comma)


lsh <- 
    lsh %>%
    filter(
        str_detect(string = icd10_code, pattern = "H65|H66|H72|H73"),
        lotu_teg == "Ferlilota" & dur_stay_hours < 4,
        year >= 2005, year <= 2015) %>%
    left_join(ids) %>%
    mutate(
        date = as.character(as.Date(as.yearmon(date_in))),
        age_y = floor(difftime(date_in, birth_date, unit = "days")/365.25),
        age_group = case_when(
            age_y == 0 ~ "0y",
            age_y == 1 ~ "1y",
            age_y == 2 ~ "2y",
            age_y %in% 3:4 ~ "3-4y",
            age_y %in% 5:9 ~ "5-9y",
            age_y %in% 10:14 ~ "10-14y",
            age_y %in% 15:19 ~ "15-19y",
            TRUE ~ "remove")
    ) %>%
    left_join(consumer_price_index) %>%
    mutate(cost_total = cost_total * cpi_jan2011/cpi)

lsh <- data.frame(lsh)



indirect_cost_vector <- dist_absent_days * dist_wage * wage_per_day

cumsum_cost_func <- function(group, quantiles, direct_cost, indirect_cost_vector){
    
    is_post_period <- which(time_points >= post_period[1])
    is_pre_period <- which(time_points < post_period[1])
    direct_cost <- direct_cost[direct_cost$age_group == group, "cost_total"]
    cases_prevented <- quantiles[[group]]$pred_samples - outcome[, group]
    
    #Direct costs
    direct_cost_saved <- matrix(mapply(
        FUN = function(cases_prevented, direct_cost){
            sum(sample(
                x = direct_cost,
                size = abs(cases_prevented),
                replace = T)) * sign(cases_prevented)
        },
        cases_prevented = cases_prevented,
        direct_cost = direct_cost), 
        nrow = dim(cases_prevented)[1], ncol = dim(cases_prevented)[2])
    
    cumsum_direct_cost_saved_post <- apply(direct_cost_saved[is_post_period,], 2, cumsum)
    cumsum_direct_cost_saved_pre <- matrix(0,
                                           nrow = nrow(direct_cost_saved[is_pre_period,]),
                                           ncol = ncol(direct_cost_saved[is_pre_period,])
    )
    
    cumsum_direct_cost_saved <- rbind(cumsum_direct_cost_saved_pre, cumsum_direct_cost_saved_post)
    
    #Indirect costs
    indirect_cost_saved <- matrix(mapply(
        FUN = function(cases_prevented, indirect_cost_vector){
            sum(sample(
                x = indirect_cost_vector,
                size = abs(cases_prevented),
                replace = T)) * sign(cases_prevented)
        },
        cases_prevented = cases_prevented,
        indirect_cost_vector = indirect_cost_vector), 
        nrow = dim(cases_prevented)[1], ncol = dim(cases_prevented)[2])
    
    cumsum_indirect_cost_saved_post <- apply(indirect_cost_saved[is_post_period,], 2, cumsum)
    cumsum_indirect_cost_saved_pre <- matrix(0,
                                           nrow = nrow(indirect_cost_saved[is_pre_period,]),
                                           ncol = ncol(indirect_cost_saved[is_pre_period,])
    )
    
    cumsum_indirect_cost_saved <- rbind(cumsum_indirect_cost_saved_pre, cumsum_indirect_cost_saved_post)
    cumsum_direct_cost_saved <- cumsum_direct_cost_saved + cumsum_indirect_cost_saved
    
    cumsum_direct_cost_saved <-
        t(apply(
            cumsum_direct_cost_saved,
            1,
            quantile,
            probs = c(0.025, 0.5, 0.975),
            na.rm = TRUE
        ))
}

cumsum_cost_saved <-
    sapply(groups,
           FUN = cumsum_cost_func,
           quantiles = quantiles_full,
           direct_cost = lsh, 
           indirect_cost_vector = indirect_cost_vector,
           simplify = 'array')

cumsum_cost_all_func <- function(group, quantiles, direct_cost, indirect_cost_vector){
    is_post_period <- which(time_points >= post_period[1])
    is_pre_period <- which(time_points < post_period[1])
    
    direct_cost <- direct_cost[direct_cost$age_group == group, "cost_total"]
    cases_prevented <- quantiles[[group]]$pred_samples - outcome[, group]
    
    #Direct costs
    direct_cost_saved <- matrix(mapply(
        FUN = function(cases_prevented, direct_cost){
            sum(sample(
                x = direct_cost,
                size = abs(cases_prevented),
                replace = T)) * sign(cases_prevented)
        },
        cases_prevented = cases_prevented,
        direct_cost = direct_cost), 
        nrow = dim(cases_prevented)[1], ncol = dim(cases_prevented)[2])
    
    cumsum_direct_cost_saved_post <- apply(direct_cost_saved[is_post_period,], 2, cumsum)
    cumsum_direct_cost_saved_pre <- matrix(0,
                                           nrow = nrow(direct_cost_saved[is_pre_period,]),
                                           ncol = ncol(direct_cost_saved[is_pre_period,])
    )
    
    cumsum_direct_cost_saved <- rbind(cumsum_direct_cost_saved_pre, cumsum_direct_cost_saved_post)

    #Indirect costs
    indirect_cost_saved <- matrix(mapply(
        FUN = function(cases_prevented, indirect_cost_vector){
            sum(sample(
                x = indirect_cost_vector,
                size = abs(cases_prevented),
                replace = T)) * sign(cases_prevented)
        },
        cases_prevented = cases_prevented,
        indirect_cost_vector = indirect_cost_vector), 
        nrow = dim(cases_prevented)[1], ncol = dim(cases_prevented)[2])
    
    cumsum_indirect_cost_saved_post <- apply(indirect_cost_saved[is_post_period,], 2, cumsum)
    cumsum_indirect_cost_saved_pre <- matrix(0,
                                             nrow = nrow(indirect_cost_saved[is_pre_period,]),
                                             ncol = ncol(indirect_cost_saved[is_pre_period,])
    )
    
    cumsum_indirect_cost_saved <- rbind(cumsum_indirect_cost_saved_pre, cumsum_indirect_cost_saved_post)
    cumsum_direct_cost_saved <- cumsum_direct_cost_saved + cumsum_indirect_cost_saved
}

cumsum_cost_all_saved <-
    setNames(lapply(groups,
                    FUN = cumsum_cost_all_func,
                    quantiles = quantiles_full,
                    direct_cost = lsh,
                    indirect_cost_vector = indirect_cost_vector), groups)

cumsum_cost_all_saved <- Reduce("+", cumsum_cost_all_saved)

cumsum_cost_all_saved <-
    t(apply(
        cumsum_cost_all_saved,
        1,
        quantile,
        probs = c(0.025, 0.5, 0.975),
        na.rm = TRUE
    ))

ggplot(data = cbind.data.frame(
    date = seq.Date(
        from = as.Date("2005-01-01"),
        to = as.Date("2015-12-01"),
        by = "months"
    ),
    cumsum_cost_all_saved - cost_vaccine
),
aes(x = date, y = `50%`)) +
    geom_vline(aes(xintercept = as.Date("2011-01-01")), lty = 2) +
    geom_line() +
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.3) +
    scale_y_continuous(labels = scales::comma)
