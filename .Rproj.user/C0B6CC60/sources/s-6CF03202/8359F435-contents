#install.packages("RMariaDB")
library(RMariaDB)
library(tidyverse)
source("01_functions.R")

# Import jobs
jobs <- read_csv("data/jobs.csv")
jobs_that_can_change_org <- jobs %>% 
  filter(cannot_change_org == 0) %>% 
  select(job_name)

# Essentially the as of date.  Highest possible date to quit job, needs to be an imported variable
max_date <- as.Date('2019/01/01')

# Connect to database stored on localhost
HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbListTables(HRSAMPLE)

# duplicate deskhistory for testing
# temporary - replace deskhistory2 with deskhistory later and delete this command
dbExecute(HRSAMPLE, "create table deskhistory2 select * from deskhistory;")


# Retrieve tables from database
# deskhistory_table is the table that will be updated here. It has already been
# established as of the start of the database.
# deskjob_table maps the desk_ids to the job.  Currently, this is always the same job_name.
# Future updates may enable these job_names to change.
### NOTE this will need to be updated, remove 2 from name
deskhistory_table <- dbGetQuery(HRSAMPLE, "SELECT *  FROM deskhistory2")
deskjob_table <- dbGetQuery(HRSAMPLE, "SELECT *  FROM deskjob")


# Retrieve depth table - NOTE: if desk_ids are added/moved/removed then this needs to be included in loop
hierarchy_with_depth.sql <- read_file("scripts/hierarchy_with_depth.sql")
hierarchy_with_depth <- dbGetQuery(HRSAMPLE, hierarchy_with_depth.sql)

# Error log for debugging
error_log <- data.frame(employee_num = integer(), desk_id = integer(), issue = character())


# Create deskhistory_table_most_recent. This selects the most recent row for each employee.
# So each desk_id has only 1 row. Think of this as the current layout of the organization.
# Note: This is run once prior to the loop, and then run run this first even though duplicative...
deskhistory_table_most_recent <- refresh_deskhistory_table_most_recent()

# i helps identify the most recent row that we look at on deskhistory_table_most_recent
# If i = 2 then we look at the 2nd most recent row in deskhistory_table_most_recent
i = 1

# Set loopnumber to help with troubleshooting
loopnumber = 0

# The upcoming while loop will update records that have a desk_id_end_date up to this date
run_through_date <- as.Date("2008-07-01")


# while loop --------------------------------------------------------------

# The while loop looks at employees 1 by 1, starting with the employee with the oldest 
# desk_id end date to determine their next action. The next action will be one of these three: 
#   termination - the desk_id will open up for someone else
#   find a new job - move to a new desk_id and their old desk_id will open up for someone else
#   get promoted - keep the same desk_id (eg move from Salesperson 1 to Salesperson 2)

while (sort(deskhistory_table_most_recent$desk_id_end_date, TRUE)[length(deskhistory_table_most_recent$desk_id_end_date)- i] < run_through_date) {

##############NOT SURE WE NEED TO INCREASE i AFTER A TERMINATION. TRY REMOVING IT
  
  # update the loopnumber
  loopnumber = loopnumber + 1

  # Refresh deskhistory_table_most_recent, which has only the most recent desk records.
  deskhistory_table_most_recent <- refresh_deskhistory_table_most_recent()  

  # Get information about employee and their current job before they move on
  temp_employee_num <- deskhistory_table_most_recent$employee_num[i]
  temp_desk_id <- deskhistory_table_most_recent$desk_id[i]
  temp_depth <- hierarchy_with_depth$depth[hierarchy_with_depth$desk_id == temp_desk_id]
  temp_end_date <- deskhistory_table_most_recent$desk_id_end_date[i]
  temp_job_name <- deskjob_table$job_name[deskjob_table$desk_id == temp_desk_id]
  
  # If the employee's current job shows termination then move to the next employee.
  # The desk_id will open up for someone else.
  if (deskhistory_table_most_recent$termination_flag[i] == 1) {
    error_log <- error_log %>% 
      bind_rows(data.frame(loopnumber = loopnumber, employee_num = temp_employee_num, desk_id = temp_desk_id, issue = paste("Job opening not filled because it was a termination")))
    i = i +1 # increase row number to look at
    next}
  
  ####NEED TO UPDATE LEADERS BELOW
  # if it is depth 0-3 then skip for now, leave plug
  if (temp_depth < 4) {
    error_log <- error_log %>% 
      bind_rows(data.frame(loopnumber = loopnumber, employee_num = temp_employee_num, desk_id = temp_desk_id, issue = paste("- Job opening not filled because in depth 0-3")))
    i = i +1 # increase row number to look at (CAN BE REMOVED WHEN THIS PIECE IS FIXED)
      next
    }
  
  # Future plug that looks at salary and terms based on that
  # salary_check_term_flag <- salary_check(temp_employee_num, temp_end_date)
  # if (salary_check_term_flag == TRUE) {  EMPLOYEE TERMINATES DUE TO SALARY}
  
  # Future plug that looks at performance reviews to see if they should term (2 in a row should be 90% term probability)
  # performance_review_term_flag <- performance_review_check(temp_employee_num, temp_end_date)
  # if (performance_review_term_flag == TRUE) {  EMPLOYEE TERMINATES DUE TO PERFORMANCE REVIEWS}
  
  
  # If there are openings on the employee's team and they are for the same job and the opening's
  # end date is les than 60 days prior to the employee's end date then they get this job.
  # - Find parent of this desk
  temp_parent_id <- hierarchy_with_depth %>% 
    filter(desk_id == temp_desk_id) %>% 
    select(parent_id) %>% 
    as.integer()
  
  # - Find other children who have the same parent
  # - If there are openings in this node within the last 60 days and same job - take this job
  temp_children_same_parent_job <- hierarchy_with_depth %>% 
    filter(parent_id == temp_parent_id, desk_id != temp_desk_id) %>% 
    left_join(deskhistory_table_most_recent) %>% 
    left_join(deskjob_table) %>% 
    mutate(days_since_last_opening = temp_end_date - desk_id_end_date) %>% 
    filter(days_since_last_opening < 60,
           days_since_last_opening > 0,
           job_name == temp_job_name) %>% 
    arrange(days_since_last_opening)  

  # Are there any rows that meet this criteria?    
  same_node_and_job_availability <- if_else(nrow(temp_children_same_parent_job) == 0, FALSE, TRUE)

  if(same_node_and_job_availability == TRUE) {
  
    # If so create a row for the oldest opening to be added to deskhistory_table
    # Note: this function determines the duration of this new job as well as whether
    # or not the employee will terminate after this job
    temp_deskhistory_table <- create_deskhistory_row(
      f_temp_new_desk_id = temp_children_same_parent_job$desk_id[1])
    
    # Add new row to deskhistory_table
    deskhistory_table <- bind_rows(deskhistory_table, temp_deskhistory_table)
    
    # Error log printout for troubleshooting
    error_log <- error_log %>% 
      bind_rows(data.frame(loopnumber = loopnumber, 
                           employee_num = temp_employee_num, 
                           desk_id = temp_desk_id,
                           new_desk_id = temp_children_same_parent_job$desk_id[1],
                           issue = paste(i, "Job added, same org and job"),
                           old_job = temp_job_name,
                           new_job = deskjob_table$job_name[deskjob_table$desk_id == temp_children_same_parent_job$desk_id[1]]))
    
    next
  }
          
    # Similar to logic above with a few differences. It can be a different job in the same node.
  # And it can be 90 days old (vs 60). Also note there is a 65% chance of this happening.
  temp_children_same_parent <- hierarchy_with_depth %>% 
    filter(parent_id == temp_parent_id, desk_id != temp_desk_id) %>% 
    left_join(deskhistory_table_most_recent) %>% 
    left_join(deskjob_table) %>% 
    mutate(days_since_last_opening = temp_end_date - desk_id_end_date) %>% 
    filter(days_since_last_opening < 90,
           days_since_last_opening > 0) %>% 
    arrange(days_since_last_opening)  

  # Ensure there are rows
  same_node_availability <- if_else(nrow(temp_children_same_parent) == 0, FALSE, TRUE)
  
  if(same_node_availability == TRUE &
     sample(0:100,1) > 35) # 65% of the time have this happen
    {
  
    # Create a row for the oldest opening to be added to deskhistory_table
    temp_deskhistory_table <- create_deskhistory_row(
      f_temp_new_desk_id = temp_children_same_parent$desk_id[1])
    
    # Add new row to deskhistory_table
    deskhistory_table <- bind_rows(deskhistory_table, temp_deskhistory_table)
    
    # Error log printout for troubleshooting
    error_log <- error_log %>% 
      bind_rows(data.frame(loopnumber = loopnumber, 
                           employee_num = temp_employee_num, 
                           desk_id = temp_desk_id,
                           new_desk_id = temp_children_same_parent$desk_id[1],
                           issue = paste("Job added, same org maybe same job"),
                           old_job = temp_job_name,
                           new_job = deskjob_table$job_name[deskjob_table$desk_id == temp_children_same_parent$desk_id[1]]))

    next

  } else if (same_node_availability == TRUE) {

    # Update error log if same node different job doesn't happen due to random sample.
    error_log <- error_log %>% 
      bind_rows(data.frame(loopnumber = loopnumber, employee_num = temp_employee_num, desk_id = temp_desk_id, issue = paste("Random sample prevented different job in same node.")))
  
  } else if (temp_job_name %in% jobs_that_can_change_org$job_name &
             sample(0:100,1) > 40) {

    # Repeat process for jobs that can change (not sales/attorney etc)
    # Select all open jobs for that position within last 90 days
    temp_same_job_any_org <- deskhistory_table_most_recent %>% 
      left_join(deskjob_table) %>% 
      filter(job_name == temp_job_name) %>% 
      mutate(days_since_last_opening = temp_end_date - desk_id_end_date) %>% 
      filter(days_since_last_opening < 90,
             days_since_last_opening > 0) %>% 
      arrange(days_since_last_opening) 
    
    # Ensure there are rows
    same_job_availability <- if_else(nrow(temp_same_job_any_org) == 0, FALSE, TRUE)
    
    if(same_job_availability == TRUE) {
    
      # Create a row for the oldest opening to be added to deskhistory_table
      temp_deskhistory_table <- create_deskhistory_row(
        f_temp_new_desk_id = temp_same_job_any_org$desk_id[1])
      
      # Add new row to deskhistory_table
      deskhistory_table <- bind_rows(deskhistory_table, temp_deskhistory_table)
      
      # Error log printout for troubleshooting
      error_log <- error_log %>% 
        bind_rows(data.frame(loopnumber = loopnumber, 
                             employee_num = temp_employee_num, 
                             desk_id = temp_desk_id,
                             new_desk_id = temp_deskhistory_table$desk_id[1],
                             issue = paste(i, "got same job in different org"),
                             old_job = temp_job_name,
                             new_job = deskjob_table$job_name[deskjob_table$desk_id == temp_deskhistory_table$desk_id[1]]))
      next
    }
      
    
  }
  
    
  # If none of above conditions are met, give promotion, keeping same desk_id.
  temp_deskhistory_table <- create_deskhistory_row(
    f_temp_new_desk_id = temp_desk_id,
    f_temp_promotion_flag = 1)

  # Error log printout for troubleshooting
  error_log <- error_log %>% 
    bind_rows(data.frame(loopnumber = loopnumber, 
                         employee_num = temp_employee_num, 
                         desk_id = temp_desk_id,
                         new_desk_id = temp_desk_id, #temp_children_same_parent$desk_id[1], ### <- PRETTY SURE THIS IS WRONG
                         issue = paste(i, "gave promotion"),
                         old_job = temp_job_name,
                         new_job = "same job due to promotion"))
  
  print(paste0(loopnumber, "loopnumber"))
}

# Other possible plugs:
# --- (maybe plug for later) try to get it where the job region type is similar (PSI) (80%)
# --- (maybe plug for later) 20% should go to random region type
# -- if not, look at new jobs that are next tier pick # b/w 0-90 days prior, if found then 1% probability

# Table is for troubleshooting
deskhistorytroubleshoot <- deskhistory_table %>% 
  left_join(deskjob_table) %>% 
  rename(new_job_name = job_name) %>% 
  mutate(prior_end_date = desk_id_start_date - 1) %>% 
  left_join(deskhistory_table %>% 
              select(prior_desk_id = desk_id, 
                     prior_end_date = desk_id_end_date, 
                     employee_num)) %>% 
  left_join(deskjob_table, by = c("prior_desk_id" = "desk_id")) %>% 
  rename(old_job_name = job_name) %>% 
  left_join(hierarchy_with_depth %>% select(desk_id, new_org = org)) %>% 
  left_join(hierarchy_with_depth %>% select(desk_id, old_org = org), by = c("prior_desk_id" = "desk_id"))

# Visualize
error_log %>% 
  mutate(loopround = round(loopnumber, -2),
         issue = gsub('[[:digit:]]+|', '', issue)) %>% 
  count(loopround, issue) %>% 
  ggplot(aes(x = loopround, y = n)) +
  geom_col() +
  facet_wrap(~issue)


