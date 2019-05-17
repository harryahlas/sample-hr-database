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



# Remove termination_flag from current employees --------------------------

fix_end_date_of_hierarchy_sql <- paste0("UPDATE deskhistory SET termination_flag = 0 WHERE desk_id_end_date > '", 
                                        end_date_of_hierarchy, "'")

dbExecute(HRSAMPLE, fix_end_date_of_hierarchy_sql)


# Add termination_flag for lingering employees ----------------------------

employee_term_repair <- deskhistory_table %>% 
  group_by(employee_num) %>% 
  mutate(desk_id_end_date_max = max(desk_id_end_date)) %>% 
  filter(desk_id_end_date_max < as.Date("2999-01-01"),
         desk_id_end_date_max == desk_id_end_date,
         termination_flag == 0)

employee_term_repair_sql <- paste("UPDATE deskhistory SET termination_flag = 1 WHERE (",
                                  paste0(" desk_id_end_date = '",   employee_term_repair$desk_id_end_date, "' AND employee_num = ",
                                         employee_term_repair$employee_num, collapse = ") OR ("),
                                  ");")

dbExecute(HRSAMPLE, employee_term_repair_sql)

