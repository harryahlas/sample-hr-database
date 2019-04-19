library(RMariaDB)
library(tidyverse)
library(lubridate)
source("01_functions.R")
source("02_variables.R")

HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbListTables(HRSAMPLE)

# Build bad_employee table ------------------------------------------------
dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS = 0;") 
dbExecute(HRSAMPLE, "DROP TABLE IF EXISTS bademployee;") 
dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS=1;")
dbExecute(HRSAMPLE, "CREATE TABLE bademployee (
          SELECT employee_num,  bad_employee_flag FROM employeeinfo
);")

bademployee_table <- dbGetQuery(HRSAMPLE, "SELECT * FROM bademployee")


# Remove Bad Employee data from employee_info -----------------------------

dbExecute(HRSAMPLE, "ALTER TABLE employeeinfo DROP COLUMN bad_employee_flag;") 



# Remove people who were never employees ----------------------------------

# Get list of employees from deskhistory
deskhistory_table <- dbGetQuery(HRSAMPLE, "SELECT * FROM deskhistory")

id_list <- deskhistory_table %>% 
  select(employee_num) %>% 
  distinct() 

id_list <- paste0(id_list$employee_num, collapse = ",")

# Create SQL

employeeinfo_remove_sql <- paste("DELETE FROM employeeinfo WHERE employee_num NOT IN (",
                                 id_list,
                                 ")")
                                 
# Delete Data
dbExecute(HRSAMPLE, employeeinfo_remove_sql)

# Check
df <- dbGetQuery(HRSAMPLE, "SELECT *  FROM employeeinfo")

