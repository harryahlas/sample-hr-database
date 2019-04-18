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

# Assign recruitment
# If start date is first_date_of_hierarchy then NA
recruiting <- initial_hire_dates %>% 
  mutate(recruiting_source = case_when(desk_id_start_date == first_date_of_hierarchy ~ "NA",
                                       TRUE ~ "TBD"))

# Placeholder for rehires: for terminated employees, grab their first hire after termination and 

# Create Recruiting Table