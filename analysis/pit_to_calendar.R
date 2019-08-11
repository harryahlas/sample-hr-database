if (file.exists("my_db.sqlite3")) {file.remove("my_db.sqlite3")}

# Set up environment
library(tidyverse)
library(RSQLite)
library(lubridate)

# Load hrsample 
#devtools::install_github("harryahlas/hrsample")
hrsample::hrsampleCreateSQLite("my_db.sqlite3")

# Connect to database
con <- dbConnect(SQLite(),'my_db.sqlite3')

# Retrieve deskhistory (point in time table)
dh <- dbGetQuery(con, "SELECT * FROM DESKHISTORY")

# Sample view
sample_employee_num <- sample(dh$employee_num,1)
dh %>% filter(employee_num == sample_employee_num)

# Convert SQLite date fields from text to date, remove promotions
dh <- dh %>% 
  mutate(desk_id_start_date = as.Date(desk_id_start_date),
         desk_id_end_date = as.Date(desk_id_end_date)) %>% 
  select(-promotion_flag)

# Create empty deskhistory_trend table (monthly calendar table)
dh_trend <- tibble()


# Get list of all months, by end of month
start_date <- as.Date("1999-01-01")
end_date <- as.Date("2018-12-31")  
month_list <- seq.Date(start_date, end_date, by = "month")

# Loop
for (i in 1:length(month_list)) {
  dh_trend_active <- dh %>% 
    filter(desk_id_start_date <= ceiling_date(month_list[i], "month") - 1,
           desk_id_end_date >= ceiling_date(month_list[i], "month") - 1) %>% 
    filter(termination_flag == 0 | desk_id_end_date != ceiling_date(month_list[i], "month") - 1) %>% 
    mutate(trend_month = month_list[i],
           termination_flag = 0)
  
  dh_trend_term <- dh %>% 
    filter(termination_flag == 1,
           desk_id_end_date <= ceiling_date(month_list[i], "month") - 1,
           desk_id_end_date >= month_list[i]
    ) %>% 
    mutate(trend_month = month_list[i])
  
  dh_trend <- bind_rows(dh_trend, dh_trend_active, dh_trend_term)
}

# View results
dh_trend %>% sample_n(3)

# Convert date columns for SQLite
dh_trend$desk_id_start_date <- format(dh_trend$desk_id_start_date, "%Y-%m-%d")
dh_trend$desk_id_end_date <- format(dh_trend$desk_id_end_date, "%Y-%m-%d")
dh_trend$trend_month <- format(dh_trend$trend_month, "%Y-%m-%d")

# Add Job title
dj <- dbGetQuery(con, "SELECT * FROM DESKJOB")
dh_trend <- dh_trend %>% 
  left_join(dj)

# Add rollup view
ru <- dbGetQuery(con, "SELECT * FROM ROLLUP")

# Remove the CEO columns 
ru <- ru %>% 
  select(-lvl00_desk_id, - lvl00_org)

# Join rollup data
dh_trend <- dh_trend %>% 
  left_join(ru, by = c("desk_id" = "lvl04_desk_id"))

# Add the employee's name
ei <- dbGetQuery(con, "SELECT * FROM EMPLOYEEINFO")
dh_trend <- dh_trend %>% 
  left_join(ei %>% select(employee_num,
                          last_name,
                          first_name))

# Reorder columns
dh_trend <- dh_trend %>% 
  select(trend_month,
         employee_num,
         last_name,
         first_name, 
         job_name,
         depth,
         everything())

# View
dh_trend %>% sample_n(3)

# Upload new table employee_trend
dbWriteTable(con, "employee_trend", dh_trend, overwrite = TRUE)

# See results
et_sample <- dbGetQuery(con, "SELECT * FROM EMPLOYEE_TREND ORDER BY RANDOM() LIMIT 3")
glimpse(et_sample)

# Close connection
dbDisconnect(con) 
