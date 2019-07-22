library(tidyverse)
library(openxlsx)
library(readxl)
library(httr)

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

# Load hrsample -----------------------------------------------------------
devtools::install_github("harryahlas/hrsample")
library(hrsample)
hrsampleCreateSQLite("my_db.sqlite3")

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
