# Cost bootstrap

library(tidyverse)
library(zoo)

############################
#### Importing the data ####
############################

ids <- read_rds("_data/results/ids.rds")
lsh <- read_rds("_data/results/lsh.rds")
impact_full <- read_rds("_analyses/paper_6/paper_6_pneumonia/Results_paper_6_pneumonia_2018-12-02-141443/paper_6_pneumonia_synthetic_control_model.rds")
impact_stack <- read_rds("_analyses/paper_6/paper_6_pneumonia/Results_paper_6_pneumonia_2018-12-02-141443/paper_6_pneumonia_stacked_model.rds")

source("_scripts/paper_6/paper_6_pneumonia/paper_6_pneumonia_synthetic_control_functions.R", local = T)

country       <- 'paper_6_pneumonia' # Country or region name.
n_seasons     <- 12       # Number of months (seasons) per year. 12 for monthly, 4 for quarterly, 3 for trimester data.
exclude_covar <- c() # User-defined list of covariate columns to exclude from all analyses.
exclude_group <- c()      # User-defined list of groups to exclude from analyses.
if(country=="Brazil"){code_change   <- TRUE     # Used for Brazil data. Set to TRUE to adjust for year 2008 coding changes; otherwise, set to FALSE.
}else{
    code_change   <- FALSE
}

input_directory  <- "_data/paper_6/paper_6_pneumonia/" # Directory (or URL) containing input data file.
file_name <- "input-data.csv"
output_directory <- '_analyses/paper_6/paper_6_pneumonia/Results'    # Directory where results will be saved.
output_directory <- paste(output_directory, '_', country,'_', format(Sys.time(), '%Y-%m-%d-%H%M%S'), '/', sep = '')                     #Adds a subfolder to output directory to organize results by date and time run.
data_file <- paste0(input_directory, file_name)
prelog_data <- read.csv(data_file, check.names = FALSE) # IF IMPORTING FROM LOCAL

group_name   <- 'age_group' # Name of column containing group labels.
date_name    <- 'date'      # Name of column containing dates.
outcome_name <- 'J12_18'    # Name of column containing outcome.
denom_name   <- "ach_noj"   # Name of column containing denominator to be used in offset.

#MOST DATES MUST BE IN FORMAT "YYYY-MM-01", exception is end of pre period, which is 1 day before end of post period
start_date        <- as.Date('2005-01-01') # Indicates the date of the first data point.
intervention_date <- as.Date('2010-12-31') # Indicates the date of intervention in the data.
end_date          <- as.Date('2017-12-01') # Indicates the date of the last data point.
pre_period        <- as.Date(c('2005-01-01', '2010-12-31')) # Range over which the data is trained for the CausalImpact model.
post_period       <- as.Date(c('2011-01-01', '2017-12-01')) # Range from the intervention date to the end date.
eval_period       <- as.Date(c('2013-01-01', '2017-12-01')) # Range over which rate ratio calculation will be performed.
year_def   <- 'cal_year'  #Can be cal_year to aggregate results by Jan-Dec; 'epi_year' to aggregate July-June

### Consumer Price Index found at www.statice.is ### 

consumer_price_index <- bind_cols( #consumer price index
    date = as.character(seq.Date(from = as.Date("2005-01-01"), to = as.Date("2017-12-01"), by = "months")),
    cpi =c(140.3, 141.9, 142.5, 142.9, 142.8, 142.5, 143.3, 143.6, 143.8, 144.6, 144.7, 144.4, 145, 145.1, 145.9, 146.7, 147.3, 148.5, 150.1, 150.2, 150, 149.7, 149.6, 150.4, 151.3, 151.7, 151.9, 152.1, 151.8, 152.3, 153.8, 154.2, 154.8, 154.7, 155.4, 155.6, 158, 158.2, 158.4, 162.6, 162.9, 164, 164.4, 163.3, 164.1, 167.2, 172.3, 178.6, 183.3, 183.2, 182.3, 180.4, 182.4, 184.2, 186.1, 186.5, 187.7, 189.5, 190.1, 190.4, 193.2, 193.5, 196.1, 196.4, 197, 198, 197.5, 197.4, 197.7, 197, 197.1, 197.1, 198.3, 196.3, 196.6, 197.2, 197, 199.9, 200.4, 198.2, 199.3, 199.5, 199.8, 200.9, 203.8, 204.5, 203.9, 204.8, 204.9, 205.6, 205, 204.4, 202.7, 205.3, 206.7, 207.6, 210.7, 213.3, 213.9, 213.7, 209.9, 210.4, 211.4, 212, 208.6, 210.2, 211.3, 211.8, 217.2, 217.9, 218.8, 221.4, 222.1, 222.1, 223.6, 223.6, 223.7, 224.1, 224.2, 224.5, 224.9, 225.6, 226.5, 227.5, 227, 226.7, 227.6, 227.9, 227.5, 228, 228.6, 228.7, 229.8, 230.4, 232.3, 232.7, 232.3, 233.1, 233.4, 233.3, 232.1, 232.2, 231.5, 232.2, 231, 230.7, 232.3, 232, 234, 232.2, 232.8, 233.9, 233.3, 234.6, 235.6, 235.6),
    cpi_jan2011 = rep(198.3, 156)
)

### Parameters from lognormal distribution found by fitting decils of wage from www.statice.is ... ###
### ... to lognormal distribution using the get.lnorm.par() function from the rriskDistributions package ###
dist_wage <- rlnorm(n = 8000, meanlog = 12.85443, sdlog = 0.35430) 

# OECD (2019), Employment rate by age group (indicator). doi: 10.1787/084f32c7-en (Accessed on 03 February 2019)
unemployment <- c(0.62, 0.65, 0.69, 0.70, 0.73, 0.77, 0.75,
                  0.83, 0.84, 0.84, 0.86, 0.88, 0.90, 0.90,
                  0.79, 0.79, 0.81, 0.84, 0.84, 0.84, 0.83)

unemployment <- rbinom(n = 8000, size = 1, prob = unemployment) 

# Midian daily conversion rate betwee ISK and USD from 2011 to 2017 ccording to the Icelandic Central Bank
exchange_rates <- read_csv2("_data/paper_6/paper_6_pneumonia/ISK-USD-conversion.csv", trim_ws = TRUE)
exchange_rates <- exchange_rates$MED
exchange_rates <- sample(x = exchange_rates, size = 8000, replace = T)

wage_per_day <- 0.010385 #standard devision of wage to get wage/day

### Cost of vaccine found in personal communication with Sóttvarnalæknir ### 

cost_vaccine <- bind_cols(
    price = c(rep(0, 72), rep(5505, 12), rep(5153, 12), rep(5064, 12), rep(5050, 12), rep(5202, 12), rep(4517, 12), rep(3739, 12)),
    number_doses = c(rep(0, 72), rep(7447/12, 12), rep(12557/12, 12), rep(12887/12, 12), rep(12953/12, 12), rep(12569/12, 12), rep(12209/12, 12), rep(11957/12, 12)),
    consumer_price_index) %>%
    mutate(price = price * number_doses * cpi_jan2011/cpi) %>%
    .$price / mean(exchange_rates)

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

groups <- c("0-4y", "5-19y", "20-39y", "40-64y", "65-79y", "80+")
time_points <- as.character(seq.Date(from = as.Date("2005-01-01"), to = as.Date("2017-12-01"), by = "month"))

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

# ggplot(data = cbind.data.frame(
#     date = seq.Date(
#         from = as.Date("2005-01-01"),
#         to = as.Date("2017-12-01"),
#         by = "months"
#     ),
#     cumsum_prevented_all
# ),
# aes(x = date, y = `50%`)) +
#     geom_vline(aes(xintercept = as.Date("2011-01-01")), lty = 2) +
#     geom_line() +
#     geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.3) +
#     scale_y_continuous(labels = scales::comma)

lsh <- 
    lsh %>%
    filter(
        str_detect(string = icd10_code, pattern = "J12|J13|J14|J15|J16|J17|J18"),
        lotu_teg == "Legulota" | dur_stay_hours >= 24,
        year >= 2005, year <= 2017) %>%
    left_join(ids) %>%
    mutate(
        date = as.character(as.Date(as.yearmon(date_in))),
        age_y = floor(difftime(date_in, birth_date, unit = "days")/365.25),
        age_group = case_when(
            age_y >= 0 & age_y <= 4 ~ "0-4y",
            age_y >= 5 & age_y <= 19 ~ "5-19y",
            age_y >= 20 & age_y <= 39 ~ "20-39y",
            age_y >= 40 & age_y <= 64 ~ "40-64y",
            age_y >= 65 & age_y <= 79 ~"65-79y",
            age_y >= 80 ~ "80+",
            TRUE ~ "remove")
    ) %>%
    left_join(consumer_price_index) %>%
    mutate(cost_total = cost_total * cpi_jan2011/cpi)

lsh <- data.frame(lsh)

dist_hospital_days <- setNames(lapply(
    groups,
    FUN = function(group, lsh) {
        lsh <- lsh[lsh$age_group == group, "dur_stay_hours"]
        sample(
            x = ceiling(lsh/24),
            size = 8000,
            replace = T
        )},
    lsh = lsh), groups)

dist_absent_days <- setNames(lapply(
    groups,
    FUN = function(group, lsh) {
        lsh <- lsh[lsh$age_group == group, "dur_stay_hours"]
        sample(
            x = ceiling(lsh/24 + rpois(n = length(lsh), lambda = lsh/24/2)),
            size = 8000,
            replace = T
        )},
    lsh = lsh), groups)

indirect_cost_vector <- setNames(lapply(
    groups,
    FUN = function(group, dist_absent_days, dist_wage, wage_per_day, unemployment, exchange_rates) {
        (dist_absent_days[[group]] * dist_wage * wage_per_day * unemployment)/exchange_rates
    },
    dist_absent_days = dist_absent_days,
    dist_wage = dist_wage,
    wage_per_day = wage_per_day,
    unemployment = unemployment,
    exchange_rates = exchange_rates), groups)

lsh <- 
    lsh %>%
    sample_n(size = 8000, replace = T) %>%
    mutate_at(
        .vars =  vars(starts_with("cost")),
        .funs = function(x) {x/exchange_rates}
    )

### Cumulative sum of direct costs ### 

cumsum_direct_cost_func <- function(group, quantiles, direct_cost){
    
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
    cumsum_direct_cost_saved <-
        t(apply(
            cumsum_direct_cost_saved,
            1,
            quantile,
            probs = c(0.025, 0.5, 0.975),
            na.rm = TRUE
        ))
}

cumsum_direct_cost_saved <-
    sapply(groups,
           FUN = cumsum_direct_cost_func,
           quantiles = quantiles_full,
           direct_cost = lsh,
           simplify = 'array')

### Cumulative sum of total costs including vaccine cost, assuming only direct cost savings ### 

cumsum_vaccine_direct_func <- function(group, quantiles, direct_cost, cost_vaccine){
    
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
    
    direct_cost_saved <- direct_cost_saved - cost_vaccine
    
    cumsum_direct_cost_saved_post <- apply(direct_cost_saved[is_post_period,], 2, cumsum)
    cumsum_direct_cost_saved_pre <- matrix(0,
                                           nrow = nrow(direct_cost_saved[is_pre_period,]),
                                           ncol = ncol(direct_cost_saved[is_pre_period,])
    )
    cumsum_direct_cost_saved <- rbind(cumsum_direct_cost_saved_pre, cumsum_direct_cost_saved_post)
    cumsum_direct_cost_saved <-
        t(apply(
            cumsum_direct_cost_saved,
            1,
            quantile,
            probs = c(0.025, 0.5, 0.975),
            na.rm = TRUE
        ))
}

cumsum_vaccine_direct <-
    sapply(groups,
           FUN = cumsum_vaccine_direct_func,
           quantiles = quantiles_full,
           direct_cost = lsh,
           cost_vaccine = cost_vaccine,
           simplify = 'array')

### ICER for direct costs only; health sector perspective ###

ICER_direct_cost_func <- function(group, quantiles, direct_cost, cost_vaccine){
    
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
    
    direct_cost_saved <- direct_cost_saved - cost_vaccine
    
    cumsum_direct_cost_saved_post <- apply(direct_cost_saved[is_post_period,], 2, cumsum)
    cumsum_direct_cost_saved_pre <- matrix(0,
                                           nrow = nrow(direct_cost_saved[is_pre_period,]),
                                           ncol = ncol(direct_cost_saved[is_pre_period,])
    )
    cumsum_direct_cost_saved <- rbind(cumsum_direct_cost_saved_pre, cumsum_direct_cost_saved_post)
    cumsum_direct_cost_total <- cumsum_direct_cost_saved
    
    #Cases prevented
    cumsum_cases_prevented_post <- apply(cases_prevented[is_post_period,], 2, cumsum)
    cumsum_cases_prevented_pre <- matrix(1,
                                         nrow = nrow(cases_prevented[is_pre_period,]),
                                         ncol = ncol(cases_prevented[is_pre_period,])
    )
    cumsum_cases_prevented <- rbind(cumsum_cases_prevented_pre, cumsum_cases_prevented_post)
    
    ICER_direct_cost <- cumsum_direct_cost_total/cumsum_cases_prevented
    
    ICER_direct_cost <-
        t(apply(
            ICER_direct_cost,
            1,
            quantile,
            probs = c(0.025, 0.5, 0.975),
            na.rm = TRUE
        ))
}

ICER_direct_cost <-
    sapply(groups,
           FUN = ICER_direct_cost_func,
           quantiles = quantiles_full,
           direct_cost = lsh,
           cost_vaccine = cost_vaccine,
           simplify = 'array')


### Cumulative sum of absent days averted ###

cumsum_absent_days_func <- function(group, quantiles, dist_absent_days){
    
    is_post_period <- which(time_points >= post_period[1])
    is_pre_period <- which(time_points < post_period[1])
    cases_prevented <- quantiles[[group]]$pred_samples - outcome[, group]
    
    absent_days <- matrix(mapply(
        FUN = function(cases_prevented, dist_absent_days){
            sum(sample(
                x = dist_absent_days,
                size = abs(cases_prevented),
                replace = T)) * sign(cases_prevented)
        },
        cases_prevented = cases_prevented,
        dist_absent_days = dist_absent_days), 
        nrow = dim(cases_prevented)[1], ncol = dim(cases_prevented)[2])
    
    cumsum_absent_days_post <- apply(absent_days[is_post_period,], 2, cumsum)
    cumsum_absent_days_pre <- matrix(0,
                                     nrow = nrow(absent_days[is_pre_period,]),
                                     ncol = ncol(absent_days[is_pre_period,])
    )
    
    cumsum_absent_days <- rbind(cumsum_absent_days_pre, cumsum_absent_days_post)
    
    cumsum_absent_days <-
        t(apply(
            cumsum_absent_days,
            1,
            quantile,
            probs = c(0.025, 0.5, 0.975),
            na.rm = TRUE
        ))
}

cumsum_absent_days <-
    sapply(groups,
           FUN = cumsum_absent_days_func,
           quantiles = quantiles_full,
           dist_absent_days = dist_absent_days,
           simplify = 'array')

### Cumulative sum of indirect costs ###

cumsum_indirect_cost_func <- function(group, quantiles, indirect_cost_vector){
    
    is_post_period <- which(time_points >= post_period[1])
    is_pre_period <- which(time_points < post_period[1])
    cases_prevented <- quantiles[[group]]$pred_samples - outcome[, group]
    
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
    
    cumsum_indirect_cost_saved <-
        t(apply(
            cumsum_indirect_cost_saved,
            1,
            quantile,
            probs = c(0.025, 0.5, 0.975),
            na.rm = TRUE
        ))
}

cumsum_indirect_cost_saved <-
    sapply(groups,
           FUN = cumsum_indirect_cost_func,
           quantiles = quantiles_full,
           indirect_cost_vector = indirect_cost_vector,
           simplify = 'array')

### Cumulative sum of direct and indirect costs ###

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

### Cumulative sum of total costs including vaccine cost, assuming only direct cost savings ### 

cumsum_vaccine_cost_func <- function(group, quantiles, direct_cost, indirect_cost_vector, cost_vaccine){
    
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
    
    direct_cost_saved <- direct_cost_saved - cost_vaccine
    
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

cumsum_vaccine_cost <-
    sapply(groups,
           FUN = cumsum_vaccine_cost_func,
           quantiles = quantiles_full,
           direct_cost = lsh, 
           indirect_cost_vector = indirect_cost_vector,
           cost_vaccine = cost_vaccine,
           simplify = 'array')

### ICER for direct and indirect costs; societal perspective ###

ICER_total_cost_func <- function(group, quantiles, direct_cost, indirect_cost_vector, cost_vaccine){
    
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
    
    direct_cost_saved <- direct_cost_saved - cost_vaccine
    
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
    
    #Total costs
    cumsum_direct_cost_saved <- cumsum_direct_cost_saved + cumsum_indirect_cost_saved
    cumsum_direct_cost_total <- cumsum_direct_cost_saved
    
    #Cases prevented
    cumsum_cases_prevented_post <- apply(cases_prevented[is_post_period,], 2, cumsum)
    cumsum_cases_prevented_pre <- matrix(1,
                                         nrow = nrow(cases_prevented[is_pre_period,]),
                                         ncol = ncol(cases_prevented[is_pre_period,])
    )
    cumsum_cases_prevented <- rbind(cumsum_cases_prevented_pre, cumsum_cases_prevented_post)
    
    ICER_total_cost <- cumsum_direct_cost_total/cumsum_cases_prevented
    
    ICER_total_cost <-
        t(apply(
            ICER_total_cost,
            1,
            quantile,
            probs = c(0.025, 0.5, 0.975),
            na.rm = TRUE
        ))
}

ICER_total_cost <-
    sapply(groups,
           FUN = ICER_total_cost_func,
           quantiles = quantiles_full,
           direct_cost = lsh,
           indirect_cost_vector = indirect_cost_vector,
           cost_vaccine = cost_vaccine,
           simplify = 'array')

### Cumulative sum of direct costs only regardless of age-group ###

cumsum_direct_cost_all_func <- function(group, quantiles, direct_cost){
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
}

cumsum_direct_cost_all_draws <-
    setNames(lapply(groups,
                    FUN = cumsum_direct_cost_all_func,
                    quantiles = quantiles_full,
                    direct_cost = lsh), groups)

cumsum_direct_cost_all_saved <- Reduce("+", cumsum_direct_cost_all_draws)

cumsum_direct_cost_all_saved <-
    t(apply(
        cumsum_direct_cost_all_saved,
        1,
        quantile,
        probs = c(0.025, 0.5, 0.975),
        na.rm = TRUE
    ))

### Cumulative sum of direct costs minus the vaccine costs, used only for the following ICER calculations ###

cumsum_vaccine_direct_all_func <- function(group, quantiles, direct_cost, cost_vaccine){
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
    direct_cost_saved <- direct_cost_saved - cost_vaccine
    
    cumsum_direct_cost_saved_post <- apply(direct_cost_saved[is_post_period,], 2, cumsum)
    cumsum_direct_cost_saved_pre <- matrix(0,
                                           nrow = nrow(direct_cost_saved[is_pre_period,]),
                                           ncol = ncol(direct_cost_saved[is_pre_period,])
    )
    
    cumsum_direct_cost_saved <- rbind(cumsum_direct_cost_saved_pre, cumsum_direct_cost_saved_post)
}

### ICER for direct costs only regardless of age-group; health sector perspective ###

ICER_direct_cost_all <-
    setNames(lapply(groups[1:length(groups)-1],
                    FUN = cumsum_direct_cost_all_func,
                    quantiles = quantiles_full,
                    direct_cost = lsh), groups[1:length(groups)-1])

ICER_direct_cost_last_with_vaccine <-
    setNames(lapply(groups[length(groups)],
                    FUN = cumsum_vaccine_direct_all_func,
                    quantiles = quantiles_full,
                    direct_cost = lsh,
                    cost_vaccine = cost_vaccine), groups[length(groups)])

ICER_direct_cost_all <- c(ICER_direct_cost_all, ICER_direct_cost_last_with_vaccine)
ICER_direct_cost_all <- Reduce("+", ICER_direct_cost_all)

ICER_direct_cost_all_func <- function(group, quantiles){
    is_post_period <- which(time_points >= post_period[1])
    is_pre_period <- which(time_points < post_period[1])
    cases_prevented <- quantiles[[group]]$pred_samples - outcome[, group]
    
    #Cases prevented
    cumsum_cases_prevented_post <- apply(cases_prevented[is_post_period,], 2, cumsum)
    cumsum_cases_prevented_pre <- matrix(1,
                                         nrow = nrow(cases_prevented[is_pre_period,]),
                                         ncol = ncol(cases_prevented[is_pre_period,])
    )
    cumsum_cases_prevented <- rbind(cumsum_cases_prevented_pre, cumsum_cases_prevented_post)
}

ICER_prevented_cases_all <-
    setNames(lapply(groups,
                    FUN = ICER_direct_cost_all_func,
                    quantiles = quantiles_full), groups)

ICER_prevented_cases_all <- Reduce("+", ICER_prevented_cases_all)

ICER_direct_cost_all <- ICER_direct_cost_all/ICER_prevented_cases_all

ICER_direct_cost_all <-
    t(apply(
        ICER_direct_cost_all,
        1,
        quantile,
        probs = c(0.025, 0.5, 0.975),
        na.rm = TRUE
    ))

### Cumulative sum of absent days averted regardless of age-group ###

cumsum_absent_days_all_func <- function(group, quantiles, dist_absent_days){
    
    is_post_period <- which(time_points >= post_period[1])
    is_pre_period <- which(time_points < post_period[1])
    cases_prevented <- quantiles[[group]]$pred_samples - outcome[, group]
    
    absent_days <- matrix(mapply(
        FUN = function(cases_prevented, dist_absent_days){
            sum(sample(
                x = dist_absent_days,
                size = abs(cases_prevented),
                replace = T)) * sign(cases_prevented)
        },
        cases_prevented = cases_prevented,
        dist_absent_days = dist_absent_days), 
        nrow = dim(cases_prevented)[1], ncol = dim(cases_prevented)[2])
    
    cumsum_absent_days_post <- apply(absent_days[is_post_period,], 2, cumsum)
    cumsum_absent_days_pre <- matrix(0,
                                     nrow = nrow(absent_days[is_pre_period,]),
                                     ncol = ncol(absent_days[is_pre_period,])
    )
    
    cumsum_absent_days <- rbind(cumsum_absent_days_pre, cumsum_absent_days_post)
}

cumsum_absent_days_all <-
    setNames(
        lapply(
            groups,
            FUN = cumsum_absent_days_all_func,
            quantiles = quantiles_full,
            dist_absent_days = dist_absent_days
        ),
        groups
    )

cumsum_absent_days_all <- Reduce("+", cumsum_absent_days_all)

cumsum_absent_days_all <-
    t(apply(
        cumsum_absent_days_all,
        1,
        quantile,
        probs = c(0.025, 0.5, 0.975),
        na.rm = TRUE
    ))

### Cumulative sum of indirect costs regardless of age-group ###

cumsum_indirect_cost_all_func <- function(group, quantiles, indirect_cost_vector){
    
    is_post_period <- which(time_points >= post_period[1])
    is_pre_period <- which(time_points < post_period[1])
    cases_prevented <- quantiles[[group]]$pred_samples - outcome[, group]
    
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
}

cumsum_indirect_cost_all_saved <-
    setNames(lapply(
        groups,
        FUN = cumsum_indirect_cost_all_func,
        quantiles = quantiles_full,
        indirect_cost_vector = indirect_cost_vector
    ),
    groups
    )

cumsum_indirect_cost_all_saved <- Reduce("+", cumsum_indirect_cost_all_saved)

cumsum_indirect_cost_all_saved <-
    t(apply(
        cumsum_indirect_cost_all_saved,
        1,
        quantile,
        probs = c(0.025, 0.5, 0.975),
        na.rm = TRUE
    ))

### Cumulative sum of direct and indirect costs regardless of age-group ###

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

cumsum_cost_all_draws <-
    setNames(lapply(groups,
                    FUN = cumsum_cost_all_func,
                    quantiles = quantiles_full,
                    direct_cost = lsh,
                    indirect_cost_vector = indirect_cost_vector), groups)

cumsum_cost_all_saved <- Reduce("+", cumsum_cost_all_draws)

cumsum_cost_all_saved <-
    t(apply(
        cumsum_cost_all_saved,
        1,
        quantile,
        probs = c(0.025, 0.5, 0.975),
        na.rm = TRUE
    ))

### Cumulative sum of direct costs minus the vaccine costs, used only for the following ICER calculations ###

cumsum_vaccine_cost_all_func <- function(group, quantiles, direct_cost, indirect_cost_vector, cost_vaccine){
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
    direct_cost_saved <- direct_cost_saved - cost_vaccine
    
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

### ICER for direct and indirect costs regardless of age-group; societal perspective ###

ICER_total_cost_all <-
    setNames(lapply(groups[1:length(groups)-1],
                    FUN = cumsum_cost_all_func,
                    quantiles = quantiles_full,
                    indirect_cost_vector,
                    direct_cost = lsh), groups[1:length(groups)-1])

ICER_total_cost_last_with_vaccine <-
    setNames(lapply(groups[length(groups)],
                    FUN = cumsum_vaccine_cost_all_func,
                    quantiles = quantiles_full,
                    direct_cost = lsh,
                    indirect_cost_vector,
                    cost_vaccine = cost_vaccine), groups[length(groups)])

ICER_total_cost_all <- c(ICER_total_cost_all, ICER_total_cost_last_with_vaccine)
ICER_total_cost_all <- Reduce("+", ICER_total_cost_all)
ICER_total_cost_all <- ICER_total_cost_all/ICER_prevented_cases_all

ICER_total_cost_all <-
    t(apply(
        ICER_total_cost_all,
        1,
        quantile,
        probs = c(0.025, 0.5, 0.975),
        na.rm = TRUE
    ))

# ggplot(data = cbind.data.frame(
#     date = seq.Date(
#         from = as.Date("2005-01-01"),
#         to = as.Date("2015-12-01"),
#         by = "months"
#     ),
#     cumsum_cost_all_saved - cost_vaccine
# ),
# aes(x = date, y = `50%`)) +
#     geom_vline(aes(xintercept = as.Date("2011-01-01")), lty = 2) +
#     geom_line() +
#     geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.3) +
#     scale_y_continuous(labels = scales::comma)

saveRDS(object = cumsum_cost_all_draws, file = '_analyses/paper_6/paper_6_pneumonia/cumsum_cost_saved')
saveRDS(object = cumsum_direct_cost_all_draws, file = '_analyses/paper_6/paper_6_pneumonia/cumsum_direct_cost_saved')
