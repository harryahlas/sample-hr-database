library(RMariaDB)
library(tidyverse)
library(openxlsx)
source("02_variables.R")

HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbListTables(HRSAMPLE)


# Create employee list ----------------------------------------------------
sql_random_employees <- "SELECT employee_num FROM `hrsample`.`deskhistory` ORDER BY RAND() LIMIT 25"
random_employees <- dbGetQuery(HRSAMPLE, sql_random_employees)

# Create date range -------------------------------------------------------
legal_start_date <- sample(seq(as.Date(first_date_of_hierarchy),
                               as.Date(end_date_of_hierarchy), 
                               by = "day") ,
                           1)

legal_end_date <- sample(seq(legal_start_date,
                               as.Date(end_date_of_hierarchy), 
                               by = "day") ,
                           1)


date_needed <- sample(seq(legal_start_date,
                          legal_end_date, 
                          by = "day") ,
                      length(random_employees$employee_num), 
                      replace = TRUE)


# Create roles ------------------------------------------------------------
employee_role <- sample(c("Potential Suspect", "Related Manager", "Involved"),
                        length(random_employees$employee_num), 
                        replace = TRUE,
                        prob = c(.2, .15, .65))


# Combine into data set ---------------------------------------------------
client_request_input_data <- tibble(`Employee Number` = random_employees$employee_num,
                                    `Date of incident or notification` = date_needed,
                                    `Role in incident` = employee_role,
                                    `Job Name` = NA) 


# Export ------------------------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "HR data needed")
writeDataTable(wb, 1, client_request_input_data)
saveWorkbook(wb, "data/Johnson litigation research.xlsx", TRUE)


