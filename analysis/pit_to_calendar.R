file.remove("my_db.sqlite3")
# Basic ETL with R and SQL: Create Calendar Table from Point In Time Data 
#Today we will dive into ETL: <li>Extract data from a point in time table, Transform it into a monthly table, and then Load it back to the database.
# for pit vs calendar see here, and here

# For this exercise we will use R and SQLite.  I like the simplicity of SQLite.  It is very fast for this type of database and easy to install, though there are some drawbacks, as we will touch upon.

# Use hrsample
# calendar table is good for month end reporting


setwd("C:\\Users\\Anyone\\Desktop\\Toss")
#These are the CRAN packages we will need:

library(tidyverse)
library(RSQLite)
library(lubridate)
#library(DBI) test without this


# Additionally, we use the data You will aWe will start by installing the hrsample (bloglink, rpackagelink) HR database.  If you went through my prior blog entry (linkw/name), you can continue to use that same database.
# Note: for more information about <em>hrsample</em> please see <a my blog entry, link to the package on github, link to the development (samplehar)

# Load hrsample -----------------------------------------------------------
#devtools::install_github("harryahlas/hrsample")
hrsample::hrsampleCreateSQLite("my_db.sqlite3")

#<h1>Extract
# Let's connect to the database.

# Connect to database
con <- dbConnect(SQLite(),'my_db.sqlite3')

#The first table we will need is the deskhistory table.  We will import it into an object called <code>dh</code>.
# Retrieve deskhistory (point in time table)
dh <- dbGetQuery(con, "SELECT * FROM DESKHISTORY")


# Let's look a little closer at the <code>dh</code> (deskhistory) table.

sample_employee_num <- sample(dh$employee_num,1)
dh %>% filter(employee_num == sample_employee_num)
# employee_num desk_id desk_id_start_date desk_id_end_date termination_flag
# 1         5970     274         2009-02-03       2012-04-25                0
# 2         5970     274         2012-04-26       2014-05-23                0
# 3         5970     990         2014-05-24       2016-03-12                1

# The deskhistory data is transactional. It shows one row for each instance an employee was in a desk_id (aka position). 
# It includes a start date <code>desk_id_start_date</code> and end date <code>desk_id_end_date</code>. This is known as point in time data.

#<h1>Transform

# Our goal is to get this point in time data in to a monthly snapshot, which can be very useful for reporting and analysis.
# The monthly table will have one row for each month the employee is in a desk_id.
## One of the drawbacks of SQLite is that it does not store date formats.  So the first transform will be to convert the date fields from text into dates.
# Convert SQLite date fields from text to date
dh <- dh %>% 
  mutate(desk_id_start_date = as.Date(desk_id_start_date),
         desk_id_end_date = as.Date(desk_id_end_date)) %>% 
  select(-promotion_flag)

# Next, let's create a placeholder data frame for our monthly calendar data
# Create empty deskhistory_trend table (monthly calendar table)
dh_trend <- tibble()


#We need to create a list of all months that will be included in our calendar table data t
# Get list of all months, by end of month
start_date <- as.Date("1999-01-01")
end_date <- as.Date("2018-12-30")  
month_list <- seq.Date(start_date, end_date, by = "month")

# Now we are ready to do the real work - creating the new data.  To do so, we will:
# <li>Iterate through each month in our list of months
# <li>Identify the employees that were active as of the last day of that month
# <li>Identify the employees that terminated during that month
# <li>Add rows to our table for these active and terminated employees

# Here is the code.  We'll review it in more detail below.
## Note we had to reassign the termination flag for months that they did not terminate
## Also, we'll remove the promotion rows
# Find employees that were active for entire month or termed during that month
# Example For 2010-01-31: if their end date is > 2010-01-01 and start date <= 2010-01-31 and not termed then keep
for (i in 1:length(month_list)) {
  print(month_list[i])
  dh_trend_active <- dh %>% 
    filter(desk_id_start_date <= ceiling_date(month_list[i]),
           desk_id_end_date >= ceiling_date(month_list[i])) %>% 
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

# A more detailed look at the code is below. Feel free to SKIP AHEAD (link).

# <sub header>Inside the for loop
# The first piece of the for loop specifies how many loops to run.  This is the number of months in our list of months (<code>month_list</code>) 
# for (i in 1:length(month_list)) {
  
# Then we'll create a temporary data frame <code>dh_trend_active</code> that captures active employees for the month in the current iteration. 
# The code looks at the <code>desk_id_start_date</code> and <code>desk_id_end_date</code> columns to determine if an employee was active during that month.
I THINK desk_id_end_date > month_list[i]) NEEDS TO BE CEILING DATE
dh_trend_active <- dh %>% 
  filter(desk_id_start_date <= ceiling_date(month_list[i]),
         desk_id_end_date >= ceiling_date(month_list[i])) %>%

    # The next piece of this <em>dplyr chain</em> creates a new column for the month and recodes the <code>termination_flag</code> to 0 since these employees were all active during that month.
  mutate(trend_month = month_list[i],
         termination_flag = 0)

# The loop repeats a nearly identical process for terminated employees. The only difference is there 
  dh_trend_term <- dh %>% 
  filter(termination_flag == 1,
         desk_id_end_date <= ceiling_date(month_list[i], "month"),
         desk_id_end_date >= month_list[i]
  ) %>% 
  mutate(trend_month = month_list[i])

  # the last piece of the loop adds the new rows to our <code>dh_trend</code> table
  dh_trend <- bind_rows(dh_trend, dh_trend_active, dh_trend_term)
  
#CHECK, SHOULD BE NONE >1
  dh_trend %>% count(trend_month, employee_num, name = "empmonth", sort = T)
#  ALSO CHECK THAT NO EMPIDS SHOW UP TWICE IN SAME MONTH, EXCEPT FOR THOSE TWO ERRORS

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
## We are going to remove the CEO columns since they are not adding value
# add rollup view
ru <- dbGetQuery(con, "SELECT * FROM ROLLUP")

## We are going to remove the CEO columns since they are not adding value
ru <- ru %>% 
  select(-lvl00_desk_id, - lvl00_org)

dh_trend <- dh_trend %>% 
  left_join(ru, by = c("desk_id" = "lvl04_desk_id"))

# Let's also add the employee's name
ei <- dbGetQuery(con, "SELECT * FROM EMPLOYEEINFO")
dh_trend <- dh_trend %>% 
  left_join(ei %>% select(employee_num,
                          last_name,
                          first_name))

# Let's reorder some of the rows.
dh_trend <- dh_trend %>% 
  select(trend_month,
         employee_num,
         last_name,
         first_name, 
         job_name,
         depth,
         everything())

#<h1>Load

## Our last step is to upload the data to a new table as employee_trend
#upload new table employee_trend
dbWriteTable(con, "employee_trend", dh_trend, overwrite = TRUE)

# See results
et_sample <- dbGetQuery(con, "SELECT * FROM EMPLOYEE_TREND ORDER BY RANDOM() LIMIT 5")
glimpse(et_sample)

dbGetQuery(con, "SELECT TREND_MONTH, COUNT(*) FROM EMPLOYEE_TREND WHERE TERMINATION_FLAG = 1 GROUP BY TREND_MONTH")
# Plot trend? maybe by org?

# Close connection
dbDisconnect(con) 
