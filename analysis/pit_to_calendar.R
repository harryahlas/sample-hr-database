#I like the simplicity of SQLite. Very fast for this type of database and easy to install.
# Use hrsample
# calendar table is good for month end reporting

setwd("C:\\Users\\Anyone\\Desktop\\Toss")
library(tidyverse)
library(RSQLite)
library(lubridate)
#library(DBI) test without this

# Load hrsample -----------------------------------------------------------
#devtools::install_github("harryahlas/hrsample")

hrsample::hrsampleCreateSQLite("my_db.sqlite3")


# Connect to database
con <- dbConnect(SQLite(),'my_db.sqlite3')

# Retrieve deskhistory (point in time table)
dh <- dbGetQuery(con, "SELECT * FROM DESKHISTORY")

## One of the drawbacks of SQLite is that it does not store date formats.  So we will store them as text here.
# Convert SQLite date fields from text to date
dh <- dh %>% 
  mutate(desk_id_start_date = as.Date(desk_id_start_date),
         desk_id_end_date = as.Date(desk_id_end_date))

# Create empty deskhistory_trend table (monthly calendar table)
dh_trend <- tibble()

  


# Sample of one TM
dh %>% filter(employee_num == 21065)

# Get list of all months, by end of month
start_date <- as.Date("1999-01-01")
end_date <- as.Date("2018-12-30")  

month_list <- seq.Date(start_date, end_date, by = "month")

# Iterate through each deskhistory row to populate each month actives


## Note we had to reassign the termination flag for months that they did not terminate
# Find employees that were active for entire month or termed during that month
# Example For 2010-01-31: if their end date is > 2010-01-01 and start date <= 2010-01-31 and not termed then keep
for (i in 1:length(month_list)) {
  print(month_list[i])
  dh_trend_active <- dh %>% 
    filter(desk_id_start_date <= ceiling_date(month_list[i]),
           desk_id_end_date > month_list[i]) %>% 
    mutate(trend_month = month_list[i],
           termination_flag = 0)
  
  dh_trend_term <- dh %>% 
    filter(termination_flag == 1,
           desk_id_end_date <= ceiling_date(month_list[i], "month"),
           desk_id_end_date >= month_list[i]
           ) %>% 
    mutate(trend_month = month_list[i])
  
  dh_trend <- bind_rows(dh_trend, dh_trend_active, dh_trend_term)
  
}

# Convert date columns for SQLite
#dh_trend_backup <- dh_trend # take this out
dh_trend$desk_id_start_date <- format(dh_trend$desk_id_start_date, "%Y-%m-%d")
dh_trend$desk_id_end_date <- format(dh_trend$desk_id_end_date, "%Y-%m-%d")
dh_trend$trend_month <- format(dh_trend$trend_month, "%Y-%m-%d")

# Next we'll add the job title to our data. Each desk_id has a job assigned to it.  That assignment is on the deskjob table
# add Job title
dj <- dbGetQuery(con, "SELECT * FROM DESKJOB")
dh_trend <- dh_trend %>% 
  left_join(dj)


## Having a hierarchy will make this table more valuable for reporting. Let's add org information. 
## The Rollup view lists all the company's desk_ids 4 levels deep along with the level of each desk.
## We will use the lvl04_desk_id to join the company hierarchy data.
## NEED TO EXPLAIN BETTER
# add rollup view
ru <- dbGetQuery(con, "SELECT * FROM ROLLUP")
dh_trend <- dh_trend %>% 
  left_join(ru, by = c("desk_id" = "lvl04_desk_id"))

## Our last step is to upload the data to a new table as employee_trend
#upload new table employee_trend
dbWriteTable(con, "employee_trend", dh_trend, overwrite = TRUE)

# See results
dbGetQuery(con, "SELECT TREND_MONTH, COUNT(*) FROM EMPLOYEE_TREND WHERE TERMINATION_FLAG = 1 GROUP BY TREND_MONTH")

# Close connection
dbDisconnect(con) 
