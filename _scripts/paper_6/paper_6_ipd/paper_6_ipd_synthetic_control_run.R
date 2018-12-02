#This is the file used to set variables to be used in analysis, as well as to run the analysis.
#Make sure *_analysis.R, *_report.R, *_report.Rmd, *_functions.R, and *_plot.R are all in the same folder as this file.
#Model the setup shown in this file, then run this file from the console using source('This file's directory/This file's name'). 
#Clear the workspace
rm(list = ls(all = TRUE))
gc()
require(RCurl)

#### WORKING DIRECTORY Should be set as the directory where .Rmd file is saved  ####

# Used to check for relevant packages and update them if out of date or install them if not installed.
update_packages  <- FALSE #Whether to update outdated packages.
install_packages <- FALSE #Whether to install missing packages.
install_pandoc   <- FALSE #Whether to install pandoc, which requires an external installer, and rmarkdown, a package that depends on pandoc's successful installation.

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
intervention_date <- as.Date('2011-04-01') # Indicates the date of intervention in the data.
end_date          <- as.Date('2016-10-01') # Indicates the date of the last data point.
pre_period        <- as.Date(c('2005-01-01', '2010-07-01')) # Range over which the data is trained for the CausalImpact model.
post_period       <- as.Date(c('2011-04-01', '2016-10-01')) # Range from the intervention date to the end date.
eval_period       <- as.Date(c('2013-01-01', '2016-10-01')) # Range over which rate ratio calculation will be performed.
year_def   <- 'cal_year'  #Can be cal_year to aggregate results by Jan-Dec; 'epi_year' to aggregate July-June

sensitivity= TRUE
crossval= FALSE #run cross validation? Note this takes time...adds ~40 min with 10 age groups, 7 cores

#Run analysis and generate HTML report
source('_scripts/paper_6/paper_6_ipd/paper_6_ipd_synthetic_control_report.R', local = TRUE)
source('_scripts/paper_6/paper_6_ipd/paper_6_ipd_synthetic_control_write_results.R', local = TRUE) #save .csv files with output tables
