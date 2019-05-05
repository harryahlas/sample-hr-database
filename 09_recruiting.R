library(RMariaDB)
library(tidyverse)
library(lubridate)
source("01_functions.R")
source("02_variables.R")

HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbListTables(HRSAMPLE)

# Build recruiting table -------------------------------------------------

dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS = 0;") 
dbExecute(HRSAMPLE, "DROP TABLE IF EXISTS recruiting;") 
dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS=1;")
dbExecute(HRSAMPLE, "CREATE TABLE recruiting (
          employee_num INT (11),
          recruiting_source VARCHAR (255),
          first_contact_date DATE,
          recruiter_employee_num INT (11),
          FOREIGN KEY (employee_num) REFERENCES employeeinfo (employee_num)  ON DELETE CASCADE ON UPDATE CASCADE
  );")


# Import data -------------------------------------------------------------

deskhistory_table <- dbGetQuery(HRSAMPLE, "SELECT * FROM deskhistory")
hierarchy_table <- dbGetQuery(HRSAMPLE, "SELECT * FROM hierarchy")
recruiters <- read_csv("data/recruiters.csv")


# Recruiting --------------------------------------------------------------

# Create distribution for initial contact date ----------------------------
# Initial contact date will be a number of days randomly chosen from here prior to their start date
initial_contact_date_prior_days_distribution <- round(rgamma(1000, shape=3.777666, scale=45/3.777666) ,0) * 1

get_first_contact_date <- function(f_desk_id_start_date) {
  # Assign desk_id_start_date if first date of hierarchy
  # Randomly assign NAs (desk_id_start_date)
  # Otherwise create initial contact date based on distribution above
  
  if (f_desk_id_start_date == first_date_of_hierarchy) { return(f_desk_id_start_date)}
  if (sample(c(1,0), 1, prob = c(initial_contact_date_NA_ratio, 100 - initial_contact_date_NA_ratio), replace = T) == 1) { return(f_desk_id_start_date)}
  
  initial_contact_date_prior_days = as.integer(sample(initial_contact_date_prior_days_distribution, 1))

  first_contact_date = f_desk_id_start_date - initial_contact_date_prior_days

  return(first_contact_date)
}

# Create list of recruiter desk_ids
recruiter_desk_id_list <- hierarchy_table %>% 
  filter(org == "Hmn Rsrcs - Emply Dvlpmnt - Project Services") %>% 
  select(mgr_desk_id = desk_id, mgr_org = org) %>% 
  left_join(hierarchy_table, by = c("mgr_desk_id" = "parent_id")) %>% 
  left_join(deskhistory_table)

# Function to retrieve recruiter at given date
get_recruiter <- function(f_first_contact_date, f_desk_id_start_date) {
  # Assign NA if first date of hierarchy (high employe_num)
  # Randomly assign NAs (high employe_num)
  # If first contact date is not null then pull based on that
  # If first contact date is null then pull based on desk id start date
  if (f_desk_id_start_date == first_date_of_hierarchy) { return(9999999)}
  
  if (sample(c(1,0), 1, prob = c(recruiter_missing_ratio, 100 - recruiter_missing_ratio), replace = T) == 1) { return(9999999)}
  
  recruiter_date <- f_first_contact_date
  
  if (is.na(recruiter_date)) { recruiter_date <- f_desk_id_start_date }
  
  recruiter_return <- recruiter_desk_id_list %>%
    filter(desk_id_start_date <= recruiter_date, 
           desk_id_end_date >= recruiter_date) %>% 
    select(employee_num) %>% 
    sample_n(1) 
  
  return(recruiter_return$employee_num[1])
}

# For each employee in deskhistory, take their first hire date
initial_hire_dates <- deskhistory_table %>% 
  group_by(employee_num) %>% 
  filter(desk_id_start_date == min(desk_id_start_date)) %>% 
  select(employee_num, desk_id_start_date) %>% 
  ungroup()

# Function to assign recruiting source based on start date
# If start date is first_date_of_hierarchy then NA
get_recruiting_source <- function(f_desk_id_start_date) {
  f_desk_id_start_year <- year(f_desk_id_start_date)
  
  f_possible_recruiters <- recruiters %>% 
    filter(effective_year <= f_desk_id_start_year)
  
  f_recruiter <- sample(f_possible_recruiters$recruiting_source,
                        1, 
                        replace = TRUE, 
                        prob = f_possible_recruiters$recruiting_source_ratio)
  f_recruiter <- if_else(f_desk_id_start_date == first_date_of_hierarchy, "NA", f_recruiter)
  
  return(f_recruiter)
}




# Identify recruiting source and create initial contact date --------------

recruiting_table <- initial_hire_dates %>% 
  rowwise() %>% 
  mutate(recruiting_source = get_recruiting_source(desk_id_start_date),
         first_contact_date = get_first_contact_date(desk_id_start_date) ,
         recruiter_employee_num = get_recruiter(first_contact_date, desk_id_start_date))

    
# Validation --------------------------------------------------------------

recruiting_table %>% 
  ungroup() %>% 
  count(recruiting_source, year = year(desk_id_start_date)) %>% 
  filter(year != 1999) %>% 
  ggplot(aes(year, n, fill = recruiting_source)) +
  geom_col() +
  facet_wrap(~recruiting_source)

# Placeholder for rehires: for terminated employees, grab their first hire after termination and ...

# Populate recruiting -------------------------------------------------

# Populate recruiting
recruiting_sql <- paste(
  "INSERT INTO recruiting (employee_num, recruiting_source, first_contact_date, recruiter_employee_num) VALUES ",
  paste0(
    "('",
    recruiting_table$employee_num, "','",
    recruiting_table$recruiting_source, "','",
    recruiting_table$first_contact_date, "','",
    recruiting_table$recruiter_employee_num, "')",
    collapse = ", "),
  ";"
)

dbExecute(HRSAMPLE, recruiting_sql)
