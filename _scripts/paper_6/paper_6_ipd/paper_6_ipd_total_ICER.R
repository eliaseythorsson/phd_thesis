# Cost bootstrap

library(readxl)
library(tidyverse)
library(zoo)

############################
#### Importing the data ####
############################

ids <- read_rds("_data/results/ids.rds")
lsh <- read_rds("_data/results/lsh.rds")
sykl <- read_excel("_data/paper_6/paper_6_ipd/Ífarandi pneumo_1995_2016_Elias.xls")
impact_full <- read_rds("_analyses/paper_6/paper_6_ipd/Results_paper_6_ipd_2018-12-02-152110/paper_6_ipd_synthetic_control_model.rds")
#impact_stack <- read_rds("_analyses/paper_6/paper_6_ipd/Results_paper_6_ipd_2018-12-02-152110/paper_6_ipd_stacked_model.rds")

source("_scripts/paper_6/paper_6_ipd/paper_6_ipd_synthetic_control_functions.R", local = T)

# Assign variable values
country       <- 'paper_6_ipd' # Country or region name.
n_seasons     <- 4       # Number of months (seasons) per year. 12 for monthly, 4 for quarterly, 3 for trimester data.
exclude_covar <- c() # User-defined list of covariate columns to exclude from all analyses.
exclude_group <- c()      # User-defined list of groups to exclude from analyses.
if(country=="Brazil"){code_change   <- TRUE     # Used for Brazil data. Set to TRUE to adjust for year 2008 coding changes; otherwise, set to FALSE.
}else{
    code_change   <- FALSE
}

input_directory  <- "_data/paper_6/paper_6_ipd/" # Directory (or URL) containing input data file.
file_name <- "input-data.csv"
output_directory <- '_analyses/paper_6/paper_6_ipd/Results'   # Directory where results will be saved.
output_directory <- paste(output_directory, '_', country,'_', format(Sys.time(), '%Y-%m-%d-%H%M%S'), '/', sep = '')                     #Adds a subfolder to output directory to organize results by date and time run.
data_file <- paste0(input_directory, file_name)
prelog_data <- read.csv(data_file, check.names = FALSE) # IF IMPORTING FROM LOCAL

group_name   <- 'age_group' # Name of column containing group labels.
date_name    <- 'date'      # Name of column containing dates.
outcome_name <- 'IPD'    # Name of column containing outcome.
denom_name   <- "ach_noj"   # Name of column containing denominator to be used in offset.

#MOST DATES MUST BE IN FORMAT "YYYY-MM-01", exception is end of pre period, which is 1 day before end of post period
start_date        <- as.Date('2005-01-01') # Indicates the date of the first data point.
intervention_date <- as.Date('2010-12-31') # Indicates the date of intervention in the data.
end_date          <- as.Date('2016-10-01') # Indicates the date of the last data point.
pre_period        <- as.Date(c('2005-01-01', '2010-12-31')) # Range over which the data is trained for the CausalImpact model.
post_period       <- as.Date(c('2011-01-01', '2016-10-01')) # Range from the intervention date to the end date.
eval_period       <- as.Date(c('2013-01-01', '2016-10-01')) # Range over which rate ratio calculation will be performed.
year_def   <- 'cal_year'  #Can be cal_year to aggregate results by Jan-Dec; 'epi_year' to aggregate July-June

### Consumer Price Index found at www.statice.is ### 

consumer_price_index <- bind_cols( #consumer price index
    date = as.character(seq.Date(from = as.Date("2005-01-01"), to = as.Date("2016-12-01"), by = "3 months")),
    cpi = c(140.3, 142.9, 143.3, 144.6, 145, 146.7, 150.1, 149.7, 151.3, 152.1, 153.8, 154.7, 158, 162.6, 164.4, 167.2, 183.3, 180.4, 186.1, 189.5, 193.2, 196.4, 197.5, 197, 198.3, 197.2, 200.4, 199.5, 203.8, 204.8, 205, 205.3, 210.7, 213.7, 211.4, 210.2, 217.2, 221.4, 223.6, 224.1, 224.9, 227.5, 227.6, 228, 229.8, 232.7, 233.4, 232.2),
    cpi_jan2015 = rep(224.9, 48)
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
exchange_rates <- read_csv2("_data/paper_6/paper_6_ipd/ISK-USD-conversion.csv", trim_ws = TRUE)
exchange_rates <- exchange_rates$MED
exchange_rates <- sample(x = exchange_rates, size = 8000, replace = T)

wage_per_day <- 0.010385 #standard devision of wage to get wage/day

### Cost of vaccine found in personal communication with Sóttvarnalæknir ### 

vaccine_price <- bind_cols(
    price = c(rep(0, 24), rep(5505, 4), rep(5153, 4), rep(5064, 4), rep(5050, 4), rep(5202, 4), rep(4517, 4)),
    consumer_price_index) %>%
    mutate(price = (price * cpi_jan2015/cpi)/mean(exchange_rates))

vaccine_price <- t(sapply(X = t(vaccine_price$price), FUN = function(x) rnorm(n = 8000, mean = x, sd = x/10)))

number_doses = c(rep(0, 24), rep(7447/4, 4), rep(12557/4, 4), rep(12887/4, 4), rep(12953/4, 4), rep(12569/4, 4), rep(12209/4, 4))

cost_vaccine <- vaccine_price * number_doses

# cost_vaccine <- bind_cols(
#     price = c(rep(0, 24), rep(5505, 4), rep(5153, 4), rep(5064, 4), rep(5050, 4), rep(5202, 4), rep(4517, 4)),
#     number_doses = c(rep(0, 24), rep(7447/4, 4), rep(12557/4, 4), rep(12887/4, 4), rep(12953/4, 4), rep(12569/4, 4), rep(12209/4, 4)),
#     consumer_price_index) %>%
#     mutate(price = price * number_doses * cpi_jan2015/cpi) %>%
#     .$price / mean(exchange_rates)

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

groups <- c("0-4y", "5-64y", "65y+")
time_points <- as.character(seq.Date(from = as.Date("2005-01-01"), to = as.Date("2016-10-01"), by = "3 month"))

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
                impact = impact_full[[group]],
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
# 
# ggplot(data = cbind.data.frame(
#     date = seq.Date(
#         from = as.Date("2005-01-01"),
#         to = as.Date("2016-12-01"),
#         by = "3 months"
#     ),
#     cumsum_prevented_all
# ),
# aes(x = date, y = `50%`)) +
#     geom_vline(aes(xintercept = as.Date("2011-01-01")), lty = 2) +
#     geom_line() +
#     geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.3) +
#     scale_y_continuous(labels = scales::comma)


sykl <- 
    sykl %>%
    dplyr::select(
        date = dagsetn.,
        kt = kennitala, 
        death = látinn,
        death_date = dánardagur,
        blood = blóð,
        spine = mænuv.,
        joint = liðv.,
        serot = hjúpg.,
        vac_t = `VT (Synflorix)`
    )

sykl <- 
    sykl %>%
    mutate(
        kt = str_replace_all(kt, "-", ""),
        death = if_else(is.na(death), 0, 1),
        blood = if_else(blood == "Nei"|is.na(blood), 0, 1),
        spine = if_else(spine == "Nei"|is.na(spine), 0, 1),
        joint = if_else(joint == "Nei"|is.na(joint), 0, 1),
        serot = str_replace_all(serot, "\\?| |útl|\\.|", ""),
        vac_t = as.numeric(case_when(
            vac_t == "NA" ~ NA,
            vac_t == "No" ~ FALSE,
            vac_t == "Y" ~ TRUE,
            is.na(vac_t) ~ NA
        ))
    )

sykl <- sykl %>% mutate(kt = str_replace_all(kt, "\n", ""))

sykl <-
    sykl %>% 
    mutate(
        serot = case_when(
            str_detect(serot, pattern = "NA|óþekkt|óhjúpgr") ~ as.character(NA), 
            TRUE ~ as.character(serot)),
        birth_d = str_sub(.$kt, 1, 2), 
        birth_m =  str_sub(.$kt, 3, 4),
        birth_y = case_when(
            str_sub(.$kt, 10, 10) == 0 ~ paste0(20, str_sub(.$kt, 5, 6)),
            str_sub(.$kt, 10, 10) == 9 ~ paste0(19, str_sub(.$kt, 5, 6)),
            str_sub(.$kt, 10, 10) == 8 ~ paste0(18, str_sub(.$kt, 5, 6))
        )
    )  %>%
    mutate(birth_date = as.Date(paste(.$birth_y, .$birth_m, .$birth_d, sep = "-"))) %>%
    mutate(age_y = floor(as.numeric(difftime(date, birth_date, units = "days"))/365.25))

lsh <- 
    sykl %>%
    dplyr::select(date, kt) %>%
    left_join(ids %>% dplyr::select(id, kt)) %>%
    left_join(lsh) %>%
    mutate(difftime1 = as.numeric(difftime(date, date_in, units = "day"))) %>%
    filter(
        !is.na(id),
        difftime1 <= 60, difftime1 >= -7,
        lotu_teg == "Legulota" | dur_stay_hours >= 24,
        year >= 2005, year <= 2016) %>%
    left_join(ids) %>%
    mutate(
        date = as.character(as.Date(as.yearqtr(date_in))),
        age_y = floor(difftime(date_in, birth_date, unit = "days")/365.25),
        age_group = case_when(
            age_y >= 0 & age_y <= 4 ~ "0-4y",
            age_y >= 5 & age_y <= 64 ~ "5-64y",
            age_y >= 65 ~ "65y+",
            TRUE ~ "remove")
    ) %>%
    left_join(consumer_price_index) %>%
    mutate(cost_total = cost_total * cpi_jan2015/cpi)

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


## First cumulative sum without vaccine costs are calculated for the first 1:n-1 age-groups
ICER_direct_cost_all <-
    setNames(lapply(groups[1:length(groups)-1],
                    FUN = cumsum_direct_cost_all_func,
                    quantiles = quantiles_full,
                    direct_cost = lsh), groups[1:length(groups)-1])

## Then the cumulative sum with the vaccine cost included is caclulated for the nth age-group
## This ensures that vaccine-costs are only included once

ICER_direct_cost_last_with_vaccine <-
    setNames(lapply(groups[length(groups)],
                    FUN = cumsum_vaccine_direct_all_func,
                    quantiles = quantiles_full,
                    direct_cost = lsh,
                    cost_vaccine = cost_vaccine), groups[length(groups)])

## The cumulative sum is then added together into one list
ICER_direct_cost_all <- c(ICER_direct_cost_all, ICER_direct_cost_last_with_vaccine)

## Because the IPD data is year-quarterly, the cumulative sum needs to be lengthend to its monthly equivalent

add_length <- rep(1:nrow(ICER_direct_cost_all[[groups[1]]]), each = 3)
ICER_direct_cost_all <- setNames(lapply(groups,
                               FUN = function(group, ICER, add_length){
                                   ICER <- ICER[[group]][add_length, ][1:132, ] #1:132 reduces to the shortest series, the AOM series
                               },
                               ICER = ICER_direct_cost_all,
                               add_length = add_length),
                               groups
)

## Now we calculate the number of prevented cases ... 
prevented_cases_func <- function(group, quantiles){
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
                    FUN = prevented_cases_func,
                    quantiles = quantiles_full), groups)

## ... and lengthen as we did above

ICER_prevented_cases_all <- setNames(lapply(groups,
                                        FUN = function(group, ICER, add_length){
                                            ICER <- ICER[[group]][add_length, ][1:132, ]
                                        },
                                        ICER = ICER_prevented_cases_all,
                                        add_length = add_length),
                                 groups
)

ICER_prevented_cases_all <- Reduce("+", ICER_prevented_cases_all)

## Now we read in the cumulative sum for the pneumonia cases
cumsum_direct_cost_all_pneumonia <- readRDS(file = '_analyses/paper_6/paper_6_pneumonia/cumsum_direct_cost_saved')

groups_pneumonia <- c("0-4y", "5-19y",  "20-39y", "40-64y", "65-79y","80+")   

## ... and shorten to the length of the shortest series (AOM series)
cumsum_direct_cost_all_pneumonia <- setNames(lapply(groups_pneumonia,
                                                    FUN = function(group, ICER){
                                                        ICER <- ICER[[group]][1:132, ]
                                                    },
                                                    ICER = cumsum_direct_cost_all_pneumonia),
                                             groups_pneumonia
)


## Now we reat the cumulatvie sum of the AOM cases
cumsum_direct_cost_all_aom <- readRDS(file = '_analyses/paper_6/paper_6_otitis_media/cumsum_direct_cost_saved')

## Combine all of the above into one list
ICER_direct <- c(ICER_direct_cost_all, cumsum_direct_cost_all_aom, cumsum_direct_cost_all_pneumonia)

## And reduce this list
ICER_direct <- Reduce("+", ICER_direct)

## The total prevented direct costs of all series 
total_prevented_direct_costs <-
    t(apply(
        ICER_direct,
        1,
        quantile,
        probs = c(0.025, 0.5, 0.975),
        na.rm = TRUE
    ))


## And now calculate the ICER for all series
ICER_direct <- ICER_direct/ICER_prevented_cases_all

ICER_direct <-
    t(apply(
        ICER_direct,
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

## First cumulative sum without vaccine costs are calculated for the first 1:n-1 age-groups
ICER_cost_all <-
    setNames(lapply(groups[1:length(groups)-1],
                    FUN = cumsum_cost_all_func,
                    quantiles = quantiles_full,
                    indirect_cost_vector = indirect_cost_vector,
                    direct_cost = lsh), groups[1:length(groups)-1])

## Then the cumulative sum with the vaccine cost included is caclulated for the nth age-group
## This ensures that vaccine-costs are only included once

ICER_cost_last_with_vaccine <-
    setNames(lapply(groups[length(groups)],
                    FUN = cumsum_vaccine_cost_all_func,
                    quantiles = quantiles_full,
                    direct_cost = lsh,
                    indirect_cost_vector = indirect_cost_vector,
                    cost_vaccine = cost_vaccine), groups[length(groups)])

## The cumulative sum is then added together into one list
ICER_cost_all <- c(ICER_cost_all, ICER_cost_last_with_vaccine)

## Because the IPD data is year-quarterly, the cumulative sum needs to be lengthend to its monthly equivalent

ICER_cost_all <- setNames(lapply(groups,
                                        FUN = function(group, ICER, add_length){
                                            ICER <- ICER[[group]][add_length, ][1:132, ] #1:132 reduces to the shortest series, the AOM series
                                        },
                                        ICER = ICER_cost_all,
                                        add_length = add_length),
                                 groups
)


## Now we read in the cumulative sum for the pneumonia cases
cumsum_cost_all_pneumonia <- readRDS(file = '_analyses/paper_6/paper_6_pneumonia/cumsum_cost_saved')

## ... and shorten to the length of the shortest series (AOM series)
cumsum_cost_all_pneumonia <- setNames(lapply(groups_pneumonia,
                                                    FUN = function(group, ICER){
                                                        ICER <- ICER[[group]][1:132, ]
                                                    },
                                                    ICER = cumsum_cost_all_pneumonia),
                                             groups_pneumonia
)


## Now we reat the cumulatvie sum of the AOM cases
cumsum_cost_all_aom <- readRDS(file = '_analyses/paper_6/paper_6_otitis_media/cumsum_cost_saved')

## Combine all of the above into one list
ICER_total <- c(ICER_cost_all, cumsum_cost_all_aom, cumsum_cost_all_pneumonia)

## And reduce this list
ICER_total <- Reduce("+", ICER_total)

## The total prevented direct costs of all series 
total_prevented_costs <-
    t(apply(
        ICER_total,
        1,
        quantile,
        probs = c(0.025, 0.5, 0.975),
        na.rm = TRUE
    ))


## And now calculate the ICER for all series
ICER_total <- ICER_total/ICER_prevented_cases_all

ICER_total <-
    t(apply(
        ICER_total,
        1,
        quantile,
        probs = c(0.025, 0.5, 0.975),
        na.rm = TRUE
    ))

saveRDS(object = total_prevented_direct_costs, file = '_analyses/paper_6/paper_6_ipd/total_prevented_direct_costs')
saveRDS(object = total_prevented_costs, file = '_analyses/paper_6/paper_6_ipd/total_prevented_costs')
saveRDS(object = ICER_direct, file = '_analyses/paper_6/paper_6_ipd/ICER_direct')
saveRDS(object = ICER_total, file = '_analyses/paper_6/paper_6_ipd/ICER_total')