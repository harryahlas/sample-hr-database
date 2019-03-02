# First: set up MySQL Workbench here
# Second: set up tables
#https://programminghistorian.org/en/lessons/getting-started-with-mysql-using-r#downloading-and-installing-mysql

setwd("C:\\Development\\R code\\database")

#install.packages("RMariaDB")
library(RMariaDB)
library(tidyverse)

HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbListTables(HRSAMPLE)

dbExecute(HRSAMPLE, "CREATE TABLE xxxx (
          region_id INT (11) AUTO_INCREMENT PRIMARY KEY,
          region_name VARCHAR (25) DEFAULT NULL
);")


# View table
dbGetQuery(HRSAMPLE, "SELECT *  FROM hierarchy")


# Example:
# start_date <- as.Date('1999/01/01')
# max_date <- as.Date('2019/01/01')
# end_date <- as.Date('2019/01/01')
# create_date(start_date, end_date)


# Create jobs
jobs <- read_csv("job_name, single_lob,avg_salary,rank,pct_of_lob
                       Administrative Assistant,,50000,1,
                       Salesperson,Sales,75000,1,1
                       Manager,,90000,3,
                       Project Manager,,100000,2,
                       Consultant,,115000,2,
                       Developer,Technology,150000,1,8
                       Attorney,Legal,150000,9,.6
                       Paralegal,Legal,80000,2,.3
                       Analyst,,140000,2")






# Add employee_num
employee_info <- employee_info %>% 
  arrange(hire_date) %>% 
  mutate(employee_num = 100000 + row_number())





job_hist_table = tibble()
# for each employee in employee list

for (i in (1:length(employee_info$employee_num))) {
  #for (i in (1:20)) {
  # add first row of employment
  temp_days_in_job <- round(rgamma(1, shape=3.777666, scale=1000/3.777666),0)
  temp_employee_num = employee_info$employee_num[i] 
  temp_start_date = employee_info$hire_date[i]
  temp_end_date = temp_start_date + temp_days_in_job
  temp_end_date = if_else(temp_end_date < max_date, temp_end_date, as.Date("2999-01-01"))
  
  temp_lob = sample(lob$lob,1,prob  = lob$proportion, replace = TRUE)
  
  termination_flag <- sample(c("Termination", "Not Termination"), 1, prob=c(0.10, 0.95), replace=TRUE)
  termination_flag <- if_else(temp_end_date == as.Date("2999-01-01"),  "Not Termination", termination_flag) #if it is the last end date then it can't be a termination
  
  temp_job_hist_table <- tibble(employee_num = employee_info$employee_num[i],
                                start_date = temp_start_date,
                                end_date = temp_end_date,
                                termination_flag = termination_flag,
                                lob = temp_lob)
  
  job_hist_table <- bind_rows(job_hist_table, temp_job_hist_table)
  
  if (termination_flag == "Termination") {next}
  
  employee_max_end_date_row <- job_hist_table %>%
    filter(employee_num == temp_employee_num) %>% 
    group_by(employee_num) %>% 
    summarize(max(end_date)) 
  
  employee_max_end_date <- employee_max_end_date_row$`max(end_date)`[1]
  
  # get max end_date
  # if it is less than max_date then
  # create new single row tibble
  while (employee_max_end_date < as.Date("2999-01-01")) {
    
    
    # if the TM's greatest end date is not as.Date("2999-01-01") then add another row
    temp_days_in_job_add <- round(rgamma(1, shape=3.777666, scale=1000/3.777666) ,0)
    temp_start_date_add = employee_max_end_date + 1
    temp_end_date_add = temp_start_date_add + temp_days_in_job_add
    temp_end_date_add = if_else(temp_end_date_add < max_date, temp_end_date_add, as.Date("2999-01-01"))
    
    termination_flag <- sample(c("Termination", "Not Termination"), 1, prob=c(0.10, 0.90), replace=TRUE)
    termination_flag <- if_else(temp_end_date == as.Date("2999-01-01"),  "Not Termination", termination_flag) #if it is the last end date then it can't be a termination
    
    change_lob_flag <- sample(c(TRUE, FALSE), 1, prob = c(.3, .7))
    
    temp_lob = if_else(change_lob_flag == TRUE,
                       sample(lob$lob,1,prob  = lob$proportion, replace = TRUE),
                       temp_lob)
    
    
    temp_job_hist_table <- tibble(employee_num = temp_employee_num,
                                  start_date = temp_start_date_add,
                                  end_date = temp_end_date_add,
                                  termination_flag = termination_flag,
                                  lob = temp_lob)
    
    job_hist_table <- bind_rows(job_hist_table, temp_job_hist_table)
    
    if (termination_flag == "Termination") {break}
    
    
    employee_max_end_date_row <- job_hist_table %>%
      filter(employee_num == temp_employee_num) %>% 
      group_by(employee_num) %>% 
      summarize(max(end_date)) 
    
    employee_max_end_date <- employee_max_end_date_row$`max(end_date)`[1]
  }
}


job_hist_table %>% 
  filter(end_date == as.Date("2999-01-01")) %>% 
  count(lob)








