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


# Connect to database -----------------------------------------------------

con <- dbConnect(SQLite(),'my_db.sqlite3')


# Retrieve deskhistory (point in time table)
dh <- dbGetQuery(con, "SELECT * FROM DESKHISTORY")

# Convert to date
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

i =1

month_list[2]

# Find employees that were active for entire month or termed during that month
# Example For 2010-01-31: if their end date is > 2010-01-01 and start date <= 2010-01-31 and not termed then keep
for (i in 1:length(month_list)) {
  print(month_list[i])
  dh_trend_active <- dh %>% 
    filter(desk_id_start_date <= ceiling_date(month_list[i]),
           desk_id_end_date > month_list[i]) %>% 
    mutate(trend_month = month_list[i])
  
  dh_trend_term <- dh %>% 
    filter(termination_flag == 1,
           desk_id_end_date <= ceiling_date(month_list[i], "month"),
           desk_id_end_date >= month_list[i]
           ) %>% 
    mutate(trend_month = month_list[i])
  
  dh_trend <- bind_rows(dh_trend, dh_trend_active, dh_trend_term)
  
}

# Convert date columns for SQLite
dh_trend_backup <- dh_trend # take this out
dh_trend$desk_id_start_date <- format(dh_trend$desk_id_start_date, "%Y-%m-%d")
dh_trend$desk_id_end_date <- format(dh_trend$desk_id_end_date, "%Y-%m-%d")

# Next we'll add the job title to our data. Each desk_id has a job assigned to it.  That assignment is on the deskjob table
# add Job title
dj <- dbGetQuery(con, "SELECT * FROM DESKJOB")
dh_trend <- dh_trend %>% 
  left_join(dj)


## Having a hierarchy will make this table more valuable for reporting. Let's add org information. 
## The Rollup view lists all the company's desk_ids 4 levels deep.
## We will use the lvl04_desk_id to join the company hierarchy data.
## NEED TO EXPLAIN BETTER
# add rollup view
ru <- dbGetQuery(con, "SELECT * FROM ROLLUP")
dh_trendx <- dh_trend %>% 
  left_join(ru)

#upload new table employee_trend


 
# Iterate through each deskhistory row to populate each month terms
 # if they termed and they termed in the same month as the month above then keep
# Make sure terms are clearly marked

# x - iterate through one row
example_tm <- dh %>%
  filter(employee_num == 21065) %>% 
  filter(row_number() == 1)
  
seq.Date(as.Date(example_tm$desk_id_start_date), as.Date(example_tm$desk_id_end_date), by = "month")

# Iterate through deskhistory and create monthly trend table


# upload deskhistory_trend








# Import data -------------------------------------------------------------
temp_file <- tempfile(fileext = ".xlsx")
req <- GET("https://raw.githubusercontent.com/harryahlas/sample-hr-database/master/data/JohnsonLitigationResearch.xlsx", 
           # write result to disk
           write_disk(path = temp_file))
input_data <- read_excel(temp_file)

# Remove column that will be replaced and change date format
input_data <- input_data %>% 
  select(-`Job Name`) %>% 
  mutate(`Date of incident or notification` = 
           as.Date(`Date of incident or notification`))


# Connect to database -----------------------------------------------------
con <- dbConnect(RSQLite::SQLite(), 
                 'my_db.sqlite3')

dbListTables(con)


# Run test query ----------------------------------------------------------
test_sql <- read_file("https://raw.githubusercontent.com/harryahlas/sample-hr-database/master/scripts/mcdr_test.sql")
test_df <- dbGetQuery(con, test_sql)
test_df

# Retrieve single row using placeholders ----------------------------------
# Import sql script with placeholders
mvdr_sql_placeholder <- read_file("https://raw.githubusercontent.com/harryahlas/sample-hr-database/master/scripts/mcdr.sql")

# Replace placeholders with sample employee_num and date
mvdr_sql <- mvdr_sql_placeholder %>% 
  gsub(pattern = '%EMP_ID%',
       replacement = input_data$`Employee Number`[1],
       x = .) %>% 
  gsub(pattern = '%DATE_ID%',
       replacement = input_data$`Date of incident or notification`[1],
       x = .)

# Retrieve data
df_one_row <- dbGetQuery(con, mvdr_sql)

df_one_row

# Retrieve all data using placeholder -------------------------------------
# Create empty tibble
df <- tibble()

for (i in 1:nrow(input_data)) {
  # Replace placeholders with employee_num and date
  mvdr_sql <- mvdr_sql_placeholder %>% 
    gsub(pattern = '%EMP_ID%',
         replacement = input_data$`Employee Number`[i],
         x = .) %>% 
    gsub(pattern = '%DATE_ID%',
         replacement = input_data$`Date of incident or notification`[i],
         x = .)
  
  # Retrieve data to temporary table
  df_temp <- dbGetQuery(con, mvdr_sql)
  
  df_temp$`Date of incident or notification` <- as.Date(df_temp$`Date of incident or notification`)
  df <- bind_rows(df, df_temp)
}

# Join retrieved data to input data ---------------------------------------
output <- input_data %>% 
  left_join(df %>% select(-desk_id),
            by = c("Date of incident or notification", 
                   "Employee Number" = "employee_num")) %>% 
  replace_na(list(job_name = "not with company at this time")) %>% 
  rename(`Job Name` = job_name)

# Note: not including a disclaimer tab here though normally would

# Export ------------------------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "HR data needed with output")
writeDataTable(wb, 1, output)
saveWorkbook(wb, "Johnson litigation research with job_name.xlsx", TRUE)
