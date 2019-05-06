library(RMariaDB)
library(tidyverse)
library(lubridate)
source("01_functions.R")
source("02_variables.R")


set.seed(999)

# Import jobs
jobs <- read_csv("data/jobs.csv")
jobs_that_can_change_org <- jobs %>% 
  filter(cannot_change_org == 0) %>% 
  select(job_name)

jobs_that_can_increase_org <- jobs %>% 
  filter(cannot_increase_org == 0) %>% 
  select(job_name)

# Essentially the as of date.  Highest possible date to quit job, needs to be an imported variable
max_date <- end_date_of_hierarchy 

# Connect to database stored on localhost
HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbListTables(HRSAMPLE)

# Retrieve employeeinfo table.  This is used to find new hires.
employeeinfo_table <- dbGetQuery(HRSAMPLE, "SELECT *  FROM employeeinfo")

# Load hierarchy_table_with_state, saved from 04.
# Note this will not account for new jobs if they are created after step 04.
load("data/hierarchy_table_with_state.rda")

# Reimport city/state info
cities <- read_delim("data/cities.csv", delim = "|")

# Retrieve tables from database
# deskhistory_table is the table that will be updated here. It has already been
# established as of the start of the database.
# deskjob_table maps the desk_ids to the job.  Currently, this is always the same job_name.
# Future updates may enable these job_names to change.
### NOTE this will need to be updated, remove 2 from name
deskhistory_table <- dbGetQuery(HRSAMPLE, "SELECT *  FROM deskhistory")
deskjob_table <- dbGetQuery(HRSAMPLE, "SELECT *  FROM deskjob")


# Retrieve depth table - NOTE: if desk_ids are added/moved/removed then this needs to be included in loop
hierarchy_with_depth.sql <- read_file("scripts/hierarchy_with_depth.sql")
hierarchy_with_depth <- dbGetQuery(HRSAMPLE, hierarchy_with_depth.sql)

# Error log for debugging
error_log <- data.frame(employee_num = integer(), desk_id = integer(), issue = character())

# Update CEO so that there is no churn there.
deskhistory_table$desk_id_end_date[deskhistory_table$desk_id == 1] <- as.Date("2999-01-01")

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
run_through_date <- end_date_of_hierarchy#as.Date("2018-12-31")
#run_through_date <- as.Date("2019-01-31")
# while loop --------------------------------------------------------------

# The while loop looks at employees 1 by 1, starting with the employee with the oldest 
# desk_id end date to determine their next action. The next action will be one of these three: 
#   termination - the desk_id will open up for someone else
#   find a new job - move to a new desk_id and their old desk_id will open up for someone else
#   get promoted - keep the same desk_id (eg move from Salesperson 1 to Salesperson 2)

#TRY THIS AT BEGINNING
loop_date <- sort(deskhistory_table_most_recent$desk_id_end_date, TRUE)[length(deskhistory_table_most_recent$desk_id_end_date)- i]
##### see if it's a term then do new hire, what happens if not?
while (sort(deskhistory_table_most_recent$desk_id_end_date, TRUE)[nrow(deskhistory_table_most_recent)] < (run_through_date - 90)) {

  if(nrow(deskhistory_table) != nrow(distinct(deskhistory_table))) {
    print("error found")
    break}
  
print("a")
print("b")

    temp_deskhistory_table_append <- NULL
    temp_deskhistory_table <- NULL

  # update the loopnumber
  loopnumber = loopnumber + 1
print(paste("made it 1, loop:", loopnumber))
  # Refresh deskhistory_table_most_recent, which has only the most recent desk records.
  deskhistory_table_most_recent <- refresh_deskhistory_table_most_recent()  

  #FIRST CHECK WHAT NEW HIRES IS DOING HERE ANDT THINK ABOUT IT
  #IS MOST RECENT ROW A TERMINATION > 90 DAYS?
  
  # Check most recent and see if new hire is needed
  temp_employee_num <- deskhistory_table_most_recent$employee_num[1]
  temp_desk_id <- deskhistory_table_most_recent$desk_id[1]
  temp_depth <-  deskhistory_table_most_recent$depth[1]
  temp_end_date <- deskhistory_table_most_recent$desk_id_end_date[1]
  temp_job_name <- deskjob_table$job_name[deskjob_table$desk_id == temp_desk_id]
  temp_termination_flag <- deskhistory_table_most_recent$termination_flag[1]
  print("c")
  
  if (temp_desk_id == 1) {next}
  
  time_since_oldest_end_date <- as.numeric(loop_date - temp_end_date)

  # Old Desk ID cleanup
  # If the desk id end date is > 90 and the TM has not started a new job then give a promotion
  # Note this is sloppy cleanup that may not be needed if "i" process improves.
  if (time_since_oldest_end_date > 90 & temp_termination_flag == 0) {
    # Does TM have new job?
    temp_most_recent_desk_id <- deskhistory_table %>% 
      filter(employee_num == temp_employee_num) %>% 
      arrange(desc(desk_id_end_date)) %>% 
      filter(row_number() == 1) %>% 
      select(desk_id) %>% 
      as.character()
    
    emp_does_not_have_new_job <- temp_most_recent_desk_id == temp_desk_id
    print("d")
    
    # If no then give him a promotion using duplicate promotion code at bottom
    if (emp_does_not_have_new_job) {
      print("Job still open after 90 days and emp has not found a new job, giving promotion")
      print("e")
      #- Start duplicate code from below
        # If none of above conditions are met, give promotion, keeping same desk_id.
        temp_deskhistory_table <- create_deskhistory_row(
          f_temp_new_desk_id = temp_desk_id,
          f_temp_promotion_flag = 1)
        
        deskhistory_table <- bind_rows(deskhistory_table, temp_deskhistory_table)
        
        # Error log printout for troubleshooting
        error_log <- error_log %>% 
          bind_rows(data.frame(loopnumber = loopnumber, 
                               employee_num = temp_employee_num, 
                               desk_id = temp_desk_id,
                               new_desk_id = temp_desk_id, #temp_children_same_parent$desk_id[1], ### <- PRETTY SURE THIS IS WRONG
                               issue = paste("gave promotion"),
                               old_job = temp_job_name,
                               new_job = "same job due to promotion, 90 day rule"))
        #- End duplicate code
        # If yes then next
        print("f")
        
        next
        print("g")
        
      }
    
  }
  print("h")
  
    ## Start 90 day new hire fill for terminated desk_ids.  May want to make 90 days more random
  #OLD - if (time_since_oldest_end_date > 90 & temp_termination_flag == 1) {#not putting this into global variable until permanent
  if (time_since_oldest_end_date > 90) {#not putting this into global variable until permanent
    print("i")
    
    external_hire_text <- "" # for error log
    external_hire_employee_number <- find_external_hire()
    external_hire_text <- "and position filled by external hire"
    temp_deskhistory_table_append <- create_deskhistory_row(
      f_temp_new_desk_id = temp_desk_id,
      f_temp_employee_num = external_hire_employee_number)
    temp_deskhistory_table <- temp_deskhistory_table %>% 
      bind_rows(temp_deskhistory_table_append)
    # Add new row to deskhistory_table
    deskhistory_table <- bind_rows(deskhistory_table, temp_deskhistory_table)
     
    #######3EDIT BELOW???????????????????????
    # Error log printout for troubleshooting
    error_log <- error_log %>% 
      bind_rows(data.frame(loopnumber = loopnumber, 
                           employee_num = temp_employee_num, 
                           desk_id = NA,
                           new_desk_id = temp_desk_id,
                           issue = paste("EXTERNAL HIRE depth: (", temp_depth, ")."),
                           old_job = "none-external hire",
                           new_job = temp_job_name))
    print("j")
    i = max(1, i - 1) # bring marker back up if a term was hired
    print("external hire due to 90 days")
    next
    print("k")
    }  ## End 90 day new hire fill
  print("l")
  

print("1. not external hire")
  #IF YES THEN HAVE NEW HIRE?
  #ELSE PICK FIRST ROW THAT IS NOT A TERMINATION
  
# If i is for a row that is at max end date then reduce i by 1
if(deskhistory_table_most_recent$desk_id_end_date[i] == as.Date("2999-01-01")) {
  i = i-1
}
  
  # Get information about employee and their current job before they move on
  temp_employee_num <- deskhistory_table_most_recent$employee_num[i]
  temp_desk_id <- deskhistory_table_most_recent$desk_id[i]
  temp_depth <-  deskhistory_table_most_recent$depth[i]
  temp_end_date <- deskhistory_table_most_recent$desk_id_end_date[i]
  temp_job_name <- deskjob_table$job_name[deskjob_table$desk_id == temp_desk_id]
  
  if (temp_desk_id == 1) {next}
  
print(paste("desk id:", temp_desk_id, "- eid:", temp_employee_num))
  # If the employee's current job shows termination then move to the next employee.
  # The desk_id will open up for someone else.
  if (deskhistory_table_most_recent$termination_flag[i] == 1) {
    error_log <- error_log %>%
      bind_rows(data.frame(loopnumber = loopnumber, employee_num = temp_employee_num, desk_id = temp_desk_id, issue = paste("Job opening not filled because it was a termination")))
    i = i +1 # increase row number to look at if there was a term.
print("termination flag, let job open for someone else")
    next}
  

# Movement between levels -------------------------------------------------
  # Check to see if it is a job that can move up a level
  # Check to see if it is a bad employee
  # Check to see if mgr job opened within past 90 days (max end date)

    reports_to_temp <- hierarchy_with_depth %>% 
      filter(desk_id == temp_desk_id) %>%
      select(parent_id) %>% 
    left_join(deskhistory_table, by = c("parent_id" = "desk_id")) 
    
    # Get open date of mgr's job
    mgr_job_open_date <- max(reports_to_temp$desk_id_end_date) + 1
  
    # Is the mgr's job open within 90 days of this end date?
    mgr_job_available <- temp_end_date >= mgr_job_open_date &
      temp_end_date <= mgr_job_open_date + 90

    ###NEED TO ADD CHECK HERE TO SEE IF MGR EITHER TERMINATED OR GOT A NEW JOB
    ###LOOK TO SEE IF THE MOST RECENT EMP NUM FOR THE MGR JOB HAS A MORE RECENT DESK ID
    ###IF NEITHER THEN CANNOT PROMOTE
    
    ## NOte: not worrying about if incumbent termed, I think this is covered earlier
    # If it is a job that can jump a level and the mgr's job is open...
    if (temp_job_name %in% jobs_that_can_increase_org$job_name & mgr_job_available == TRUE) {
      # Check for bad employee
      bad_emp_flag <- employeeinfo_table %>% 
        filter(employee_num == temp_employee_num) %>% 
        select(bad_employee_flag) %>% 
        as.numeric()
      
      # Randomize
      fill_mgr_flag <- sample(c(1,0), size = 1, prob = c(next_level_ratio, 1 - next_level_ratio))
      
      # If all good then promote to next level
      if (bad_emp_flag == 0 & fill_mgr_flag == 1) {
        temp_deskhistory_table_append <- create_deskhistory_row(
          f_temp_new_desk_id = reports_to_temp$parent_id[1],
          f_temp_promotion_flag = 1)
        
        temp_deskhistory_table <- temp_deskhistory_table %>% 
          bind_rows(temp_deskhistory_table_append)
        
        # Add new row to deskhistory_table
        deskhistory_table <- bind_rows(deskhistory_table, temp_deskhistory_table)
        
        # Error log printout for troubleshooting
        error_log <- error_log %>% 
          bind_rows(data.frame(loopnumber = loopnumber, 
                               employee_num = temp_employee_num, 
                               desk_id = temp_desk_id,
                               new_desk_id = reports_to_temp$parent_id,
                               issue = paste("Promoted to higher level"),
                               old_job = temp_job_name,
                               new_job = "look it up yourself"))
        next
      }
    }  
    # End Movement between levels



  ####NEED TO UPDATE LEADERS BELOW
  # if it is depth 0-3 then skip for now, leave plug
  if (temp_depth == 3) {
print("temp depth = 3")   
    # This code is essentially the same for levels 1, 2, and 3
    # look for open level 1 jobs within past 90 days.  
    # If there are any then take them
    # else promotion, same job
    # 10% chance of terminating
    
    temp_same_level <- get_temp_same_level()
    
    # Are there any rows that meet this criteria?    
    same_level_availability <- if_else(nrow(temp_same_level) == 0, FALSE, TRUE)
    
    if(same_level_availability == TRUE) {
      
      # If so create a row 
      # Note: this function determines the duration of this new job as well as whether
      # or not the employee will terminate after this job
      temp_deskhistory_table <- create_deskhistory_row(
        f_temp_new_desk_id = temp_same_level$desk_id[1])
      
      # Create new hire to fill open position 50% of time
      external_hire_text <- "" # for error log
      if (sample(1:3,1 ) == 1) {
        external_hire_employee_number <- find_external_hire()
        external_hire_text <- "and position filled by external hire"
        temp_deskhistory_table_append <- create_deskhistory_row(
          f_temp_new_desk_id = temp_desk_id,
          f_temp_employee_num = external_hire_employee_number)
        temp_deskhistory_table <- temp_deskhistory_table %>% 
          bind_rows(temp_deskhistory_table_append)
      }

            # Add new row to deskhistory_table
      deskhistory_table <- bind_rows(deskhistory_table, temp_deskhistory_table)
      
      # Error log printout for troubleshooting
      error_log <- error_log %>% 
        bind_rows(data.frame(loopnumber = loopnumber, 
                             employee_num = temp_employee_num, 
                             desk_id = temp_desk_id,
                             new_desk_id = temp_same_level$desk_id[1],
                             issue = paste("Job added, same level in hierarchy (", temp_depth, ").", external_hire_text),
                             old_job = temp_job_name,
                             new_job = deskjob_table$job_name[deskjob_table$desk_id == temp_same_level$desk_id[1]]))
      
      next
    } else {
      error_log <- error_log %>% 
        bind_rows(data.frame(loopnumber = loopnumber, employee_num = temp_employee_num, desk_id = temp_desk_id, issue = paste("Job opening not available at that level (", temp_depth, ")")))
      
      
      
      ########START EDIT
      # If none of above conditions are met, give promotion, keeping same desk_id.
      temp_deskhistory_table <- create_deskhistory_row(
        f_temp_new_desk_id = temp_desk_id,
        f_temp_promotion_flag = 1)
      
      deskhistory_table <- bind_rows(deskhistory_table, temp_deskhistory_table)
      
      
      #i = i +1 # increase row number to look at because job did not change
      ########END EDIT
      
      
      
      
      
      
    }
    next
  }

  if (temp_depth == 2) {
print("temp depth = 2")   
    
    # This code is essentially the same for levels 1, 2, and 3
    # look for open level 1 jobs within past 90 days.  
    # If there are any then take them
    # else promotion, same job
    # 10% chance of terminating
    
    temp_same_level <- get_temp_same_level()
    
    # Are there any rows that meet this criteria?    
    same_level_availability <- if_else(nrow(temp_same_level) == 0, FALSE, TRUE)
    
    if(same_level_availability == TRUE) {
      
      # If so create a row 
      # Note: this function determines the duration of this new job as well as whether
      # or not the employee will terminate after this job
      temp_deskhistory_table <- create_deskhistory_row(
        f_temp_new_desk_id = temp_same_level$desk_id[1])
      
      # Create new hire to fill open position 50% of time
      external_hire_text <- "" # for error log
      if (sample(1:2,1 ) == 1) {
        external_hire_employee_number <- find_external_hire()
        external_hire_text <- "and position filled by external hire"
        temp_deskhistory_table_append <- create_deskhistory_row(
          f_temp_new_desk_id = temp_desk_id,
          f_temp_employee_num = external_hire_employee_number)
        temp_deskhistory_table <- temp_deskhistory_table %>% 
          bind_rows(temp_deskhistory_table_append)
      }
      
      # Add new row to deskhistory_table
      deskhistory_table <- bind_rows(deskhistory_table, temp_deskhistory_table)
      
      # Error log printout for troubleshooting
      error_log <- error_log %>% 
        bind_rows(data.frame(loopnumber = loopnumber, 
                             employee_num = temp_employee_num, 
                             desk_id = temp_desk_id,
                             new_desk_id = temp_same_level$desk_id[1],
                             issue = paste("Job added, same level in hierarchy (", temp_depth, ").", external_hire_text),
                             old_job = temp_job_name,
                             new_job = deskjob_table$job_name[deskjob_table$desk_id == temp_same_level$desk_id[1]]))
      
      next
    } else {
      error_log <- error_log %>% 
        bind_rows(data.frame(loopnumber = loopnumber, employee_num = temp_employee_num, desk_id = temp_desk_id, issue = paste("Job opening not available at that level (", temp_depth, ")")))
      
      
      
      ########START EDIT
      # If none of above conditions are met, give promotion, keeping same desk_id.
      temp_deskhistory_table <- create_deskhistory_row(
        f_temp_new_desk_id = temp_desk_id,
        f_temp_promotion_flag = 1)
      
      deskhistory_table <- bind_rows(deskhistory_table, temp_deskhistory_table)
      
      
      #i = i +1 # increase row number to look at because job did not change
      ########END EDIT
      
      
      
      
      
      
      
    }
    next
  }
  
  if (temp_depth == 1) {
print("temp depth = 1")   
    
    # This code is essentially the same for levels 1, 2, and 3
    # look for open level 1 jobs within past 90 days.  
    # If there are any then take them
    # else promotion, same job
    # 10% chance of terminating
    
    temp_same_level <- get_temp_same_level()
    
    # Are there any rows that meet this criteria?    
    same_level_availability <- if_else(nrow(temp_same_level) == 0, FALSE, TRUE)
    
    if(same_level_availability == TRUE) {
      
      # If so create a row 
      # Note: this function determines the duration of this new job as well as whether
      # or not the employee will terminate after this job
      temp_deskhistory_table <- create_deskhistory_row(
        f_temp_new_desk_id = temp_same_level$desk_id[1])

            # Create new hire to fill open position 50% of time
      external_hire_text <- "" # for error log
      if (sample(1:2,1 ) == 1) {
        external_hire_employee_number <- find_external_hire()
        external_hire_text <- "and position filled by external hire"
        temp_deskhistory_table_append <- create_deskhistory_row(
          f_temp_new_desk_id = temp_desk_id,
          f_temp_employee_num = external_hire_employee_number)
        temp_deskhistory_table <- temp_deskhistory_table %>% 
          bind_rows(temp_deskhistory_table_append)
      }
      
      # Add new row to deskhistory_table
      deskhistory_table <- bind_rows(deskhistory_table, temp_deskhistory_table)
      
      # Error log printout for troubleshooting
      error_log <- error_log %>% 
        bind_rows(data.frame(loopnumber = loopnumber, 
                             employee_num = temp_employee_num, 
                             desk_id = temp_desk_id,
                             new_desk_id = temp_same_level$desk_id[1],
                             issue = paste("Job added, same level in hierarchy (", temp_depth, ").", external_hire_text),
                             old_job = temp_job_name,
                             new_job = deskjob_table$job_name[deskjob_table$desk_id == temp_same_level$desk_id[1]]))
      
      next
    } else {
      error_log <- error_log %>% 
        bind_rows(data.frame(loopnumber = loopnumber, employee_num = temp_employee_num, desk_id = temp_desk_id, issue = paste("Job opening not available at that level (", temp_depth, ")")))
      
      
      
      ########START EDIT
      # If none of above conditions are met, give promotion, keeping same desk_id.
      temp_deskhistory_table <- create_deskhistory_row(
        f_temp_new_desk_id = temp_desk_id,
        f_temp_promotion_flag = 1)
      
      deskhistory_table <- bind_rows(deskhistory_table, temp_deskhistory_table)
      
      
      #i = i +1 # increase row number to look at because job did not change
      ########END EDIT
      
      
      
      
      
      
      
    }
    next
  }

  # Skip if CEO
  # 2/17 removing below since CEO is nonexpiring
  # if (temp_depth == 0) { 
  #   
  #   error_log <- error_log %>% 
  #     bind_rows(data.frame(loopnumber = loopnumber, employee_num = temp_employee_num, desk_id = temp_desk_id, issue = paste("CEO, not changing")))
  #   i = i +1 # increase row number to look at (CAN BE REMOVED WHEN THIS PIECE IS FIXED)
  #   next
  # }
  print("made it 2")
  
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
  print("made it 3")
  
  # Are there any rows that meet this criteria?    
  same_node_and_job_availability <- if_else(nrow(temp_children_same_parent_job) == 0, FALSE, TRUE)

  if(same_node_and_job_availability == TRUE) {
    print("6 same_node_and_job_availability == TRUE")
    
    
    # If so create a row for the oldest opening to be added to deskhistory_table
    # Note: this function determines the duration of this new job as well as whether
    # or not the employee will terminate after this job
    temp_deskhistory_table <- create_deskhistory_row(
      f_temp_new_desk_id = temp_children_same_parent_job$desk_id[1])
    print("made it 5")
    
    # Create new hire to fill open position 66% of time
    external_hire_text <- "" # for error log
    if (sample(1:3,1 ) != 1) {
      print("made it 5b")
      external_hire_employee_number <- find_external_hire()
      external_hire_text <- "and position filled by external hire"
      temp_deskhistory_table_append <- create_deskhistory_row(
        f_temp_new_desk_id = temp_desk_id,
        f_temp_employee_num = external_hire_employee_number)
      print("made it 5c")
      temp_deskhistory_table <- temp_deskhistory_table %>% 
        bind_rows(temp_deskhistory_table_append)
      print("made it 6")
    } else {print("failed 6")}
    
    
    # Add new row to deskhistory_table
    deskhistory_table <- bind_rows(deskhistory_table, temp_deskhistory_table)


    # Error log printout for troubleshooting
    error_log <- error_log %>% 
      bind_rows(data.frame(loopnumber = loopnumber, 
                           employee_num = temp_employee_num, 
                           desk_id = temp_desk_id,
                           new_desk_id = temp_children_same_parent_job$desk_id[1],
                           issue = paste("Job added, same org and job", external_hire_text),
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
    print("same_node_availability and .65 random")
   ###???? 
    # Create a row for the oldest opening to be added to deskhistory_table
    temp_deskhistory_table <- create_deskhistory_row(
      f_temp_new_desk_id = temp_children_same_parent$desk_id[1])
    
    ###??? IS THIS AN ERROR? WHY APPEND ROWS BELOW?
    # Create new hire to fill open position 50% of time
    external_hire_text <- "" # for error log
    if (sample(1:3, 1 ) != 1) {
      print("made it 9")
      
      external_hire_employee_number <- find_external_hire()
      external_hire_text <- "and position filled by external hire"
      temp_deskhistory_table_append <- create_deskhistory_row(
        f_temp_new_desk_id = temp_desk_id,
        f_temp_employee_num = external_hire_employee_number)
      temp_deskhistory_table <- temp_deskhistory_table %>% 
        bind_rows(temp_deskhistory_table_append)
    } else {print("failed 8 or 9")}

    # Add new row to deskhistory_table
    deskhistory_table <- bind_rows(deskhistory_table, temp_deskhistory_table)
    
    # Error log printout for troubleshooting
    error_log <- error_log %>% 
      bind_rows(data.frame(loopnumber = loopnumber, 
                           employee_num = temp_employee_num, 
                           desk_id = temp_desk_id,
                           new_desk_id = temp_children_same_parent$desk_id[1],
                           issue = paste("Job added, same org maybe same job", external_hire_text),
                           old_job = temp_job_name,
                           new_job = deskjob_table$job_name[deskjob_table$desk_id == temp_children_same_parent$desk_id[1]]))

    next

  } else if (same_node_availability == TRUE) {
    print("made it 10")
    
    # Update error log if same node different job doesn't happen due to random sample.
    error_log <- error_log %>% 
      bind_rows(data.frame(loopnumber = loopnumber, employee_num = temp_employee_num, desk_id = temp_desk_id, issue = paste("Random sample prevented different job in same node.")))
  
  } else if (temp_job_name %in% jobs_that_can_change_org$job_name &
             sample(0:100,1) > 40) {
    print("made it 11")
    
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
      print("made it 12")
      
      # Create a row for the oldest opening to be added to deskhistory_table
      temp_deskhistory_table <- create_deskhistory_row(
        f_temp_new_desk_id = temp_same_job_any_org$desk_id[1])
      
      
      
      # Create new hire to fill open position 66% of time
      external_hire_text <- "" # for error log
      if (sample(1:3,1 ) != 1) {
        external_hire_employee_number <- find_external_hire()
        external_hire_text <- "and position filled by external hire"
        temp_deskhistory_table_append <- create_deskhistory_row(
          f_temp_new_desk_id = temp_desk_id,
          f_temp_employee_num = external_hire_employee_number)
        temp_deskhistory_table <- temp_deskhistory_table %>% 
          bind_rows(temp_deskhistory_table_append)
        print("made it 13")
      }
      
      # Add new row to deskhistory_table
      deskhistory_table <- bind_rows(deskhistory_table, temp_deskhistory_table)
      
      # Error log printout for troubleshooting
      error_log <- error_log %>% 
        bind_rows(data.frame(loopnumber = loopnumber, 
                             employee_num = temp_employee_num, 
                             desk_id = temp_desk_id,
                             new_desk_id = temp_deskhistory_table$desk_id[1],
                             issue = paste("got same job in different org", external_hire_text),
                             old_job = temp_job_name,
                             new_job = deskjob_table$job_name[deskjob_table$desk_id == temp_deskhistory_table$desk_id[1]]))
      next
    }
      
    
  }
  else {
  
  print("made it 14")
  
  
  
  # If none of above conditions are met, give promotion, keeping same desk_id.
  temp_deskhistory_table <- create_deskhistory_row(
    f_temp_new_desk_id = temp_desk_id,
    f_temp_promotion_flag = 1)

  deskhistory_table <- bind_rows(deskhistory_table, temp_deskhistory_table)

  # Error log printout for troubleshooting
  error_log <- error_log %>% 
    bind_rows(data.frame(loopnumber = loopnumber, 
                         employee_num = temp_employee_num, 
                         desk_id = temp_desk_id,
                         new_desk_id = temp_desk_id, #temp_children_same_parent$desk_id[1], ### <- PRETTY SURE THIS IS WRONG
                         issue = paste("gave promotion"),
                         old_job = temp_job_name,
                         new_job = "same job due to promotionxxx"))

  }
  
  loop_date <-  sort(deskhistory_table_most_recent$desk_id_end_date, TRUE)[length(deskhistory_table_most_recent$desk_id_end_date)- i]
  print(paste0("Loopnumber: ", loopnumber,  " - Date: ", loop_date ))
}


# Clean up employees that have no job but didn't term ---------------------
# Count of TMs that had max date before as.Date("2999-01-01") but did not terminate by month
# These need to terminate
employees_to_add_terms <- deskhistory_table %>% 
  group_by(employee_num) %>% 
  summarize(last_day = max(desk_id_end_date),
            term_flag = max(termination_flag)) %>% 
  filter(last_day < end_date_of_hierarchy - 90,
         term_flag == 0) %>% 
  select(-term_flag)

# Terminate their most recent desk_history row
deskhistory_table <- deskhistory_table %>% 
  left_join(employees_to_add_terms) %>% 
  mutate(termination_flag = case_when(last_day == desk_id_end_date ~ 1,
                                      TRUE ~ termination_flag)) %>% 
  select(-last_day)

#check
bob <- deskhistory_table %>% 
  filter(desk_id_end_date > (loop_date - 90)) %>% 
  count(desk_id) %>% 
  filter(n > 1) %>% 
  left_join(deskhistory_table) %>% 
  arrange(desk_id)

# Upload data -------------------------------------------------------------

# First, clear old data from deskhistory
dbExecute(HRSAMPLE, "DELETE FROM deskhistory")


# Populate deskhistory
deskhistory_sql <- paste(
  "INSERT INTO deskhistory (employee_num, desk_id, desk_id_start_date, desk_id_end_date, termination_flag, promotion_flag) VALUES ",
  paste0(
    "('",
    deskhistory_table$employee_num, "','",
    deskhistory_table$desk_id, "','",
    deskhistory_table$desk_id_start_date, "','",
    deskhistory_table$desk_id_end_date, "','",
    deskhistory_table$termination_flag, "','",
    deskhistory_table$promotion_flag, "')",
    collapse = ", "),
  ";"
)

dbExecute(HRSAMPLE, deskhistory_sql)


# Save data for alternate use ---------------------------------------------
# Note: this may be removeable
save(deskhistory_table, file = "data/deskhistory.rda")
save(deskhistory_table_most_recent, file = "data/deskhistory_table_most_recent.rda")

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


job_changes <- deskhistorytroubleshoot %>% 
  filter(!is.na(old_job_name)) %>% 
  count(old_job_name, new_job_name)
  
# Term rates
employees2002 <- deskhistory_table %>% filter(desk_id_start_date <=as.Date("2002-12-31") & desk_id_end_date >= as.Date("2002-01-01"))
terminations2002 <- employees2002 %>% 
  filter(promotion_flag ==1, year(desk_id_end_date) == 2002)
length(terminations2002$employee_num)/length(employees2002$employee_num)

# New Hire rates
newhires2002 <- employees2002 %>% 
  group_by(employee_num) %>% 
  summarize(hire_date = min(desk_id_start_date)) %>% 
  filter(year(hire_date) == 2002) %>% 
  left_join(employees2002, by = c("employee_num", "hire_date" = "desk_id_start_date"))
length(newhires2002$employee_num)/length(employees2002$employee_num)

deskhistory_table %>% 
  group_by(employee_num) %>% 
  summarize(hire_year = year(min(desk_id_start_date))) %>% 
  count(hire_year)

# terms by year
deskhistory_table %>% 
  filter(termination_flag == 1, desk_id_end_date != as.Date("2999-01-01")) %>% 
  count(year(desk_id_end_date)) %>% 
  ggplot(aes(x = `year(desk_id_end_date)`, y = n)) +
  geom_col()

deskhistory_table %>% 
  filter(desk_id_end_date == as.Date("2999-01-01")) %>% 
  nrow()

# Count of TMs that started in first_date_of_hierarchy
deskhistory_table %>% 
  group_by(employee_num) %>% 
  summarize(first_day = min(desk_id_start_date)) %>% 
  filter(first_day == first_date_of_hierarchy) %>%
  nrow()

# Count of TMs that terminated by month
deskhistory_table %>% 
  filter(termination_flag == 1, desk_id_end_date != as.Date("2999-01-01")) %>% 
  mutate(termmonth = floor_date(desk_id_end_date, "month")) %>% 
  count(termmonth) %>% 
  ggplot(aes(x = termmonth, y = n)) +
  geom_col()
  
# Count of TMs that were hired after first_date_of_hierarchy
deskhistory_table %>% 
  group_by(employee_num) %>% 
  summarize(first_day = min(desk_id_start_date)) %>% 
  filter(first_day > first_date_of_hierarchy) %>%
  nrow()

# Count of TMs that had max date before as.Date("2999-01-01") but did not terminate
joe <- deskhistory_table %>% 
  group_by(employee_num) %>% 
  summarize(last_day = max(desk_id_end_date),
            term_flag = max(termination_flag)) %>% 
  filter(last_day < as.Date("2999-01-01"), term_flag == 0) %>%
  mutate(last_month = floor_date(last_day, "month")) %>% 
  count(last_month)
  
deskhistory_table %>% 
  group_by(employee_num) %>% 
  summarize(last_day = max(desk_id_end_date),
            term_flag = max(termination_flag)) %>% 
  filter(last_day < as.Date("2999-01-01"), term_flag == 0) %>%
  mutate(last_month = floor_date(last_day, "month")) %>% 
  count(last_month) %>% 
  ggplot(aes(x = last_month, y = n)) +
  geom_col()






bob <- deskhistory_table %>% 
  group_by(employee_num) %>% 
  mutate(last_day = max(desk_id_end_date)) %>% 
  ungroup() %>% 
  filter(last_day < as.Date("2019-01-01"),
         desk_id_end_date == last_day,
         termination_flag != 1) %>% 
  arrange(desc(last_day)) %>% 
  #filter(row_number() == 3000)
  select(employee_num, last_day) %>% 
  left_join(deskhistory_table)          

sam <- deskhistory_table %>% 
  group_by(employee_num) %>% 
  mutate(last_day = max(desk_id_end_date)) %>% 
  ungroup() %>% 
  filter(last_day < as.Date("2019-01-01"),
         desk_id_end_date == last_day,
         termination_flag != 1) %>% 
  arrange(desc(last_day)) %>% 
  left_join(error_log, by = c("employee_num", "desk_id"))
  
  #filter(row_number() == 3000)
  #select(employee_num, last_day) %>% 
# New Hire jobs
oldnewhires2002 <- newhires2002 %>% 
  left_join(deskjob_table) %>% 
  count(job_name) %>% 
  mutate(pct_jobs_new_hire = n/sum(n)) %>% 
  left_join(count(deskjob_table, job_name) %>% arrange(desc(n)) %>% mutate(pct = n/sum(n)) %>% rename(overalljobcount = n))

# Save to csv for github - maybe move elsewhere?
write_csv(deskhistory_table, "data/deskhistory_table.csv")
write_csv(deskjob_table, "data/deskjob_table.csv")


