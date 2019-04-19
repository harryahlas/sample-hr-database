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
          FOREIGN KEY (employee_num) REFERENCES employeeinfo (employee_num)  ON DELETE CASCADE ON UPDATE CASCADE
  );")


# Import data -------------------------------------------------------------

deskhistory_table <- dbGetQuery(HRSAMPLE, "SELECT * FROM deskhistory")
recruiters <- read_csv("data/recruiters.csv")


# Recruiting --------------------------------------------------------------

# For each employee in deskhistory, take their first hire date
initial_hire_dates <- deskhistory_table %>% 
  group_by(employee_num) %>% 
  filter(desk_id_start_date == min(desk_id_start_date)) %>% 
  select(employee_num, desk_id_start_date) %>% 
  ungroup()

# Function to assign recruiter based on start date
# If start date is first_date_of_hierarchy then NA
get_recruiter <- function(f_desk_id_start_date) {
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


# Identify recruiter ------------------------------------------------------

recruiting_table <- initial_hire_dates %>% 
  rowwise() %>% 
  mutate(recruiting_source = get_recruiter(desk_id_start_date))


# Validation --------------------------------------------------------------

recruiting_table %>% 
  ungroup() %>% 
  count(recruiting_source, year = year(desk_id_start_date)) %>% 
  filter(year != 1999) %>% 
  ggplot(aes(year, n, fill = recruiting_source)) +
  geom_col() +
  facet_wrap(~recruiting_source)

# Placeholder for rehires: for terminated employees, grab their first hire after termination and 

# Populate recruiting -------------------------------------------------

# Populate recruiting
recruiting_sql <- paste(
  "INSERT INTO recruiting (employee_num, recruiting_source) VALUES ",
  paste0(
    "('",
    recruiting_table$employee_num, "','",
    recruiting_table$recruiting_source, "')",
    collapse = ", "),
  ";"
)

dbExecute(HRSAMPLE, recruiting_sql)
