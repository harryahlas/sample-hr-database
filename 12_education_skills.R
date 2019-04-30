library(RMariaDB)
library(tidyverse)
library(lubridate)
source("01_functions.R")
source("02_variables.R")

HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbListTables(HRSAMPLE)

salaryhistory_table <- dbGetQuery(HRSAMPLE, "SELECT * FROM salaryhistory")

# Build contact table -----------------------------------------------------

dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS = 0;") 
dbExecute(HRSAMPLE, "DROP TABLE IF EXISTS education;") 
dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS=1;")
dbExecute(HRSAMPLE, "CREATE TABLE education (
          employee_num INT (11),
          degree VARCHAR (5),
          school_name VARCHAR (255),
          FOREIGN KEY (employee_num) REFERENCES employeeinfo (employee_num)  ON DELETE CASCADE ON UPDATE CASCADE
);")


# Import data -------------------------------------------------------------
#https://www.4icu.org/us/
colleges <- read_delim("data/colleges.txt", delim = "|")





# Upload data -------------------------------------------------------------

# Populate education
education_sql <- paste(
  "INSERT INTO education (employee_num, degree, school_name) VALUES ",
  paste0(
    "('",
    contact_table$employee_num, "','",
    contact_table$contact_type, "','",
    contact_table$contact_sub_type, "','",
    contact_table$contact, "','",
    contact_table$contact_end_date, "')",
    collapse = ", "),
  ";"
)

dbExecute(HRSAMPLE, education_sql)



