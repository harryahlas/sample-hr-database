#install.packages("RMariaDB")
library(RMariaDB)
library(tidyverse)

# Import jobs
jobs <- read_csv("data/jobs.csv")

# highest possible date to quit job, needs to be an imported variable
max_date <- as.Date('2019/01/01')

HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbListTables(HRSAMPLE)

# duplicate deskhistory for testing
# temporary - replace deskhistory2 with deskhistory later and delete this command
dbExecute(HRSAMPLE, "create table deskhistory2 select * from deskhistory;")

################ Start loop here

# Retrieve tables from database
deskhistory_table <- dbGetQuery(HRSAMPLE, "SELECT *  FROM deskhistory2")
deskjob_table <- dbGetQuery(HRSAMPLE, "SELECT *  FROM deskjob")


# Retrieve depth table - NOTE: if positions are added/removed then this needs to be included in loop
hierarchy_with_depth.sql <- read_file("scripts/hierarchy_with_depth.sql")
hierarchy_with_depth <- dbGetQuery(HRSAMPLE, hierarchy_with_depth.sql)



#####START LOOP HERE

# select most recent row for each employee
deskhistory_table_most_recent <- deskhistory_table %>% 
  arrange(desc(desk_id_end_date)) %>% 
  group_by(desk_id) %>% 
  filter(row_number() == 1) %>% 
  arrange(desk_id_end_date) 

error_log <- data.frame(employee_num = integer(), desk_id = integer(), issue = character())

# Start process/loop - comments repeated below
# Select 1st desk_id
# if it is a termination, then next
# if it is depth 0-3 then skip for now, leave plug
# Plug that looks at salary and terms based on that
# Plug that looks at performance reviews to see if they should term (2 in a row should be 90% term probability)
# if there are openings on the node for same job type that have end date within 90 days then take that desk_id (90%)
# if it is not a special job like sales or legal then 
# - look for another similar job type on other teams
# -- retrieve desk_ids where job name and grandparent are same - (50%)if there is an opening in same dept and same job
# -- if not, look at other same job openings around company (20%)
# --- try to get it where the job region type is similar (PSI) (80%)
# --- 20% should go to random region type
# -- if not, look at new jobs that are next tier (1%)
# -- else, keep same job, flag as promotion? (50%)
# end date + 1 becomes new start date
# look at v7 row 97 for next steps


# Select 1st desk_id
i=1
i = 307
temp_employee_num <- deskhistory_table_most_recent$employee_num[i]
temp_desk_id <- deskhistory_table_most_recent$desk_id[i]
temp_depth <- hierarchy_with_depth$depth[hierarchy_with_depth$desk_id == temp_desk_id]
temp_end_date <- deskhistory_table_most_recent$desk_id_end_date[i]
temp_job_name <- deskjob_table$job_name[deskjob_table$desk_id == temp_desk_id]

# if it is a termination, then next
if (deskhistory_table_most_recent$termination_flag[i] == 1) {next}

# if it is depth 0-3 then skip for now, leave plug
if (temp_depth < 4) {
  error_log <- error_log %>% 
    bind_rows(data.frame(employee_num = temp_employee_num, desk_id = temp_desk_id, issue = "Job opening not filled because in depth 0-3"))
}

# Plug that looks at salary and terms based on that
# salary_check_term_flag <- salary_check(temp_employee_num, temp_end_date)
# if (salary_check_term_flag == TRUE) {  EMPLOYEE TERMINATES DUE TO SALARY}

# Plug that looks at performance reviews to see if they should term (2 in a row should be 90% term probability)
# performance_review_term_flag <- performance_review_check(temp_employee_num, temp_end_date)
# if (performance_review_term_flag == TRUE) {  EMPLOYEE TERMINATES DUE TO PERFORMANCE REVIEWS}


# if there are openings on the node for same job type that have end date within 90 days then take that desk_id (90%)
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
  
same_node_availability <- if_else(nrow(temp_children_same_parent_job) == 0, FALSE, TRUE)


if(same_node_availability == TRUE) {
  #turn this into function
  ##dont think we need this: prior_employee_end_date <- temp_children_same_parent_job$desk_id_end_date[1]
  temp_start_date_add = temp_end_date + 1
  temp_days_in_job_add <- round(rgamma(1, shape=3.777666, scale=1000/3.777666) ,0)
  temp_end_date_add = temp_start_date_add + temp_days_in_job_add
  temp_end_date_add = if_else(temp_end_date_add < max_date, temp_end_date_add, as.Date("2999-01-01")) # "2999-01-01" is for active information
  
  # can remove 2 rows below??
  #termination_flag <- sample(c("Termination", "Not Termination"), 1, prob=c(0.10, 0.90), replace=TRUE)
  #termination_flag <- if_else(temp_end_date == as.Date("2999-01-01"),  "Not Termination", termination_flag) #if it is the last end date then it can't be a termination
  
  # Determine whether the new end date will be due to termination.  If the end date is current then it cannot be a termination.
  termination_flag_text <- sample(c("Termination", "Not Termination"), 1, prob=c(0.10, 0.95), replace=TRUE)
  termination_flag_text <- if_else(temp_end_date == as.Date("2999-01-01"),  "Not Termination", termination_flag_text) #if it is the last end date then it can't be a termination
  termination_flag <- if_else(termination_flag_text == "Termination", 1, 0)
  
  temp_deskhistory_table <- data.frame(
    employee_num = temp_employee_num,
    desk_id = temp_children_same_parent_job$desk_id[1],
    desk_id_start_date = temp_start_date_add,
    desk_id_end_date = temp_end_date_add,
    termination_flag = termination_flag,
    job_nameREMOVE= temp_children_same_parent_job$job_name[1])

  temp_deskhistory_table2 <- create_deskhistory_row()
  
  print(paste("old info - desk_id:", temp_desk_id, "temp_job_name:", temp_job_name, 
              "temp_employee_num:", temp_employee_num, "temp_end_date:", temp_end_date,
              "org_name:", hierarchy_with_depth$org[hierarchy_with_depth$desk_id == temp_parent_id]))
  
  print("expected new info:")
  print(temp_children_same_parent_job[1,])
  print("actual new info:")
  print(temp_deskhistory_table)
  
  #########NOTE: THIS NEEDS TO BE CHANGED TO UPDATE THE DATABASE AND START OVER  
  deskhistory_table <- bind_rows(deskhistory_table, temp_deskhistory_table)
  print("move to next option")
  #### add next
  
  
}
        

         

# PLUG - If there are openings in this node within the last 90 days - 50% take this job
# needs to be same or greater level, ie not attorney to paralegal

# if it is a special job...

# - if it is salesperson, needs to be same state.  If opening in same node last 90 days then take it.
# -- Else, keep same job, flag as promotion? (100%)
# - if not salesperson, 50% take most recent opening like it
# - Else, keep same job, flag as promotion? (100%)

# else check if it is not a special job like sales or legal 
# Look up job and if it is a special job then research
# - look for another similar job type on other teams
# -- retrieve desk_ids where job name and grandparent are same - (50%)if there is an opening in same dept and same job
# -- if not, look at other same job openings around company pick random # b/w 0-90 days prior, if found then (20%)
# --- (maybe plug for later) try to get it where the job region type is similar (PSI) (80%)
# --- (maybe plug for later) 20% should go to random region type
# -- if not, look at new jobs that are next tier pick # b/w 0-90 days prior, if found then 1% probability
# -- else, keep same job, flag as promotion? (50%)

# end date + 1 becomes new start date
# look at v7 row 97 for next steps

