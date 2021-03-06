library(RMariaDB)
library(tidyverse)
library(stringr)
source("02_variables.R")

HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbListTables(HRSAMPLE)


# Build deskhistory table -------------------------------------------------
dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS = 0;") 
dbExecute(HRSAMPLE, "DROP TABLE IF EXISTS deskhistory;") 
dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS=1;")
dbExecute(HRSAMPLE, "CREATE TABLE deskhistory (
  employee_num INT (11),
  desk_id INT (10) unsigned NOT NULL,
  desk_id_start_date DATE,
  desk_id_end_date DATE,
  termination_flag INT (8),
  promotion_flag INT (8),
  FOREIGN KEY (employee_num) REFERENCES employeeinfo (employee_num)  ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (desk_id) REFERENCES hierarchy (desk_id)  ON DELETE CASCADE ON UPDATE CASCADE
);")


# Get number of employees needed to populate day 1 (hierarchy table)
starting_employee_count <- dbGetQuery(HRSAMPLE, "SELECT COUNT(*) AS starting_employee_count FROM hierarchy") 
starting_employee_count$starting_employee_count


# Import jobs
jobs <- read_csv("data/jobs.csv")


# Reimport city/state info
cities <- read_delim("data/cities.csv", delim = "|")

# For orgs that have state names let's pull employees from that state
state_list <- read_csv("data/sales_nesw_regions.csv") %>% 
  select(region) %>% 
  distinct() %>% 
  left_join(cities %>% 
              select(`State short`, `State full`) %>% 
              distinct(), 
            by = c("region" = "State full"))

# Create states for grepl function
state_search_terms <- paste0(state_list$region, collapse = "|")

# Pull hierarchy table and create flag for orgs with state names
hierarchy_table_with_state <- dbGetQuery(HRSAMPLE, "SELECT * FROM hierarchy") %>% 
  mutate(state_flag = grepl(state_search_terms, org),
         state_present = str_extract(org, state_search_terms))

# Save hierarchy_table_with_state
save(hierarchy_table_with_state, file = "data/hierarchy_table_with_state.rda")

# Pull list of employees who started prior to start date
employeeinfo_table <- dbGetQuery(HRSAMPLE, "SELECT * FROM employeeinfo")

#need to create this as import, used in 06
# First day of hierarchy, same as max(employeeinfo_table$hire_date)
hierarchy_start_date <- first_date_of_hierarchy

#need to create this as import, used in 06
# Most recent date that a new job could be had
max_date <- end_date_of_hierarchy

deskhistory_table = tibble(employee_num = as.integer(""))
desk_ids_for_removal = data.frame()

set.seed(555)

### get hierachy with levels
# Query to get hierarchy levels of all desk_ids ---------------------------
hierarchy_with_depth.sql <- read_file("scripts/hierarchy_with_depth.sql")
hierarchy_with_depth <- dbGetQuery(HRSAMPLE, hierarchy_with_depth.sql)

# Rollup
# Note: hierachy_spread here will have multiple rows for some desk_ids.  This is ok here.
# Will choose 1 of these "special" instances later.
hierarchy_spread <- hierarchy_table_with_state %>% 
  mutate(lvl00_desk_id = 0,
         lvl00_org = "CEO") %>% 
  select(lvl00_desk_id,
         lvl00_org,
         everything()) %>% 
  filter(parent_id == 1) %>% 
  rename(lvl01_desk_id = desk_id,
         lvl01_org = org) %>% 
  select(-parent_id, -state_present, -state_flag) %>% 
  left_join(hierarchy_table_with_state, by = c("lvl01_desk_id" = "parent_id")) %>% 
  rename(lvl02_desk_id = desk_id,
         lvl02_org = org) %>% 
  select(-state_present, -state_flag) %>% 
  left_join(hierarchy_table_with_state, by = c("lvl02_desk_id" = "parent_id")) %>% 
  rename(lvl03_desk_id = desk_id,
         lvl03_org = org) %>% 
  select(-state_present, -state_flag) %>% 
  left_join(hierarchy_table_with_state, by = c("lvl03_desk_id" = "parent_id")) %>% 
  left_join(jobs %>% 
              filter(!is.na(single_lob)) %>% 
            select(single_lob, job_name, pct_of_lob),
            by = c("lvl01_org" = "single_lob"))# select(-proportion) 

write_csv(hierarchy_spread, "data/hierarchy_spread.csv")


# Populate first round of jobs in hierarchy -------------------------------
# This loop assigns an employee and job_name to each desk_id.
# The employee is selected randomly unless the desk_id is reporting to
# - a state region, in which case the employee is selected randomly from that 
# - state. If no employees from that state are available, then that desk_id is
# - left unfilled and noted in desk_ids_for_removal.
# The job_name is determined by looking at depth in the company:
# If depth = 0 then CEO
# If depth = 1 then LOB Leader
# If depth = 2 then Department Leader
# If depth = 3 then Regional Manager
# If depth > 3 then:
# - look at the job table (via hierarchy_spread) to see if there are any
# -- specified distributions for that LOB, e.g. a high percentage of
# -- legal employees are attorneys. If so, then select a job using that
# -- distribution
# - If there are no specified distributions for that LOB then randomly select
# -- a job

for (i in (1:nrow(hierarchy_table_with_state))) {
  # Process for orgs with state names
  if(hierarchy_table_with_state$state_flag[i] == TRUE) {
    temp_deskhistory_table <- state_list %>% 
      filter(region == hierarchy_table_with_state$state_present[i]) %>% 
      left_join(employeeinfo_table, by = c("State short" = "state" )) %>% 
      rename(state = `State short`) %>% 
      select(-region) %>% 
      filter(!employee_num %in% deskhistory_table$employee_num)
    
    if (nrow(temp_deskhistory_table) == 0) {
      print(paste("Not enough employees in", hierarchy_table_with_state$state_present[i], "- Please remove desk_id", i))
      desk_ids_for_removal <- desk_ids_for_removal %>% 
        bind_rows(hierarchy_table_with_state[i,])
      next
    }
    # if there is an employee from that state then  add
    temp_deskhistory_table <- temp_deskhistory_table %>% 
      sample_n(1)
  # Process for orgs without state names    
  } else {
    temp_deskhistory_table <- employeeinfo_table %>% 
      filter(!employee_num %in% deskhistory_table$employee_num) %>% 
      sample_n(1)
  }
    
  temp_days_in_job <- round(rgamma(1, shape=3.777666, scale=1000/3.777666),0)
  temp_end_date = hierarchy_start_date + temp_days_in_job
  temp_end_date = if_else(temp_end_date < max_date, temp_end_date, as.Date("2999-01-01"))

  termination_flag_text <- sample(c("Termination", "Not Termination"), 1, prob=c(0.10, 0.95), replace=TRUE)
  termination_flag_text <- if_else(temp_end_date == as.Date("2999-01-01"),  "Not Termination", termination_flag_text) #if it is the last end date then it can't be a termination
  termination_flag <- if_else(termination_flag_text == "Termination", 1, 0)
  
    # Assign job names.  Use hierarchy level to determine job name if they are in top tiers
  # Else they are individual contributor
  temp_desk_id <- hierarchy_table_with_state$desk_id[i]
  temp_job <- case_when(hierarchy_with_depth$depth[hierarchy_with_depth$desk_id == temp_desk_id]== 0 ~ "CEO",
                        hierarchy_with_depth$depth[hierarchy_with_depth$desk_id == temp_desk_id]== 1 ~ "Business Leader",
                        hierarchy_with_depth$depth[hierarchy_with_depth$desk_id == temp_desk_id]== 2 ~ "Department Leader",
                        hierarchy_with_depth$depth[hierarchy_with_depth$desk_id == temp_desk_id]== 3 ~ "Regional Leader",
                        TRUE ~ "Individual Contributor")
  
  # If they are IC then look at the probabilities in the hierarchy_spread file, originall sourced from jobs table
  if (temp_job == "Individual Contributor") {
    temp_special_job_probabilities <- hierarchy_spread %>% 
      filter(desk_id == temp_desk_id) %>% 
      select(job_name, pct_of_lob)
    
    temp_special_job_probabilities_flag <- if_else(is.na(sum(temp_special_job_probabilities$pct_of_lob)),
                                              "other",
                                              "special")
    
    if (temp_special_job_probabilities_flag == "special") {
      #do special calculation if LOB has specific job distribution
      temp_potential_jobs <- c(temp_special_job_probabilities$job_name,"other")
      temp_potential_jobs_probabilities <- c(temp_special_job_probabilities$pct_of_lob, 
                                             1- sum(temp_special_job_probabilities$pct_of_lob))
      temp_job <- sample(temp_potential_jobs,
                         1,
                         prob=temp_potential_jobs_probabilities, replace=TRUE)
      
    } else if (temp_special_job_probabilities_flag == "other") {
      temp_job <- "other"
    }  
    
    temp_job <- case_when(temp_job != "other" ~ temp_job,
                          temp_job == "other" ~ jobs %>% 
                            filter(is.na(pct_of_lob),
                                   is.na(leadership_role)) %>% 
                            select(job_name) %>% 
                            sample_n(1) %>% # Randomly select another job
                            as.character())
    

    
    # if there are no
    
    
    
  }
  
  
  temp_deskhistory_table <- temp_deskhistory_table %>% 
    mutate(desk_id = hierarchy_table_with_state$desk_id[i],
           job_name = temp_job,
           start_date = hierarchy_start_date,
           end_date = temp_end_date,
           termination_flag = termination_flag)
  
  deskhistory_table <- bind_rows(deskhistory_table, temp_deskhistory_table)
  print(i)
}

# Filter out NAs
deskhistory_table <- deskhistory_table %>% 
  filter(!is.na(employee_num))

# Validation --------------------------------------------------------------

# view desk_ids that were not filled due to not enough TMs from that state.
desk_ids_for_removal

# Check for duplicate employees
deskhistory_table %>% 
  select(employee_num) %>% 
  n_distinct() == nrow(deskhistory_table)

deskhistory_table %>% 
  select(first_name, last_name) %>% 
  n_distinct() == nrow(deskhistory_table)


# Upload data -------------------------------------------------------------

deskhistory_sql <- paste(
  "INSERT INTO deskhistory (employee_num, desk_id, desk_id_start_date, desk_id_end_date, termination_flag, promotion_flag) VALUES ",
  paste0(
    "('",
    deskhistory_table$employee_num, "','",
    deskhistory_table$desk_id, "','",
    deskhistory_table$start_date, "','",
    deskhistory_table$end_date, "','",
    deskhistory_table$termination_flag, "', '0')",
    collapse = ", "),
  ";"
)

dbExecute(HRSAMPLE, deskhistory_sql)






# Create and populate deskjob table ---------------------------------------

df <- dbGetQuery(HRSAMPLE, "SELECT *  FROM deskhistory")

dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS = 0;") 
dbExecute(HRSAMPLE, "DROP TABLE IF EXISTS deskjob;") 
dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS=1;")
dbExecute(HRSAMPLE, "CREATE TABLE deskjob (
          desk_id INT (10) unsigned NOT NULL,
          job_name VARCHAR (255),
          FOREIGN KEY (desk_id) REFERENCES hierarchy (desk_id)  ON DELETE CASCADE ON UPDATE CASCADE
);")



# Upload data -------------------------------------------------------------

deskjob_sql <- paste(
  "INSERT INTO deskjob (desk_id, job_name) VALUES ",
  paste0(
    "('",
    deskhistory_table$desk_id, "','",
    deskhistory_table$job_name, "')",
    collapse = ", "),
  ";"
)

dbExecute(HRSAMPLE, deskjob_sql)








