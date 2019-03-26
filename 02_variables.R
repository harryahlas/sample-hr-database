# Number of potential employees
number_of_employees <- 45000
company_open_date <- as.Date('1993/07/02')
first_date_of_hierarchy <- as.Date('1999/01/01')
end_date_of_hierarchy <- as.Date("2004/01/01") #as.Date("2019/01/01")

# Substitute hierarchy_start_date and max_date with dates above
#hierarchy_start_date <- as.Date("1999-01-01")
#max_date <- as.Date("2019/01/01")

first_name_sample_size <- 80000
female_male_ratio <- .55

# Ratio of bad employees. Receive lower review scores. They and their direct reports have increased turnover and shorter tenure. 
bad_employee_ratio <- c(.1,.9)
