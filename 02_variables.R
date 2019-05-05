# Company information
company_name <- "hrsample"
company_website <- "hrsample.com"

# Number of potential employees
number_of_employees <- 45000
company_open_date <- as.Date('1993/07/02')
first_date_of_hierarchy <- as.Date('1999/01/01')
end_date_of_hierarchy <- as.Date("2019/01/01")

# Substitute hierarchy_start_date and max_date with dates above
#hierarchy_start_date <- as.Date("1999-01-01")
#max_date <- as.Date("2019/01/01")

first_name_sample_size <- 80000
female_male_ratio <- .55

# Ratio of bad employees. Receive lower review scores. They and their direct reports have increased turnover and shorter tenure. 
bad_employee_ratio <- c(.1,.9)

# Percent of normal job tenure for bad employees
bad_employee_time_in_job_multiplier <- -.65 

# Bad employee termination multiplier
bad_employee_termination_multiplier <- 1.5 

# High turnover job multiplier
high_turnover_job_multiplier <- 1.3 #not currently used

# Next level promotion Ratio. If mgr job is available, chances that employee gets it.
next_level_ratio <- .4

# Recruiting initial contact date NA ratio
initial_contact_date_NA_ratio <- 37
recruiter_missing_ratio <- 5

# Education information
BA_pct <- .81
BS_pct <- .06
MS_pct <- .06  #If BA or BS only
MA_pct <- .03  #If BA or BS only
PhD_pct <- .33 #If MS or MA only
