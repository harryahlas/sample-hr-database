# create_date -------------------------------------------------------------
# Used in 01
# Function to generate random date between 2 dates

#may not be needed

#create_date <- function(start_date, end_date, number_of_dates = 1) {
#  sample(seq(start_date, end_date, by="day"), number_of_dates)
#}


# create_deskhistory_row --------------------------------------------------
# Used in 05
# Creates row to be input into deskhistory table

create_deskhistory_row <- function(f_temp_end_date = temp_end_date, 
                                   f_max_date = max_date,
                                   f_temp_employee_num = temp_employee_num, 
                                   f_temp_new_desk_id = new_desk_id,
                                   f_temp_promotion_flag = 0) {
  f_temp_start_date_add = f_temp_end_date + 1
  f_temp_days_in_job_add <- round(rgamma(1, shape=3.777666, scale=1000/3.777666) ,0)
  f_temp_end_date_add = f_temp_start_date_add + f_temp_days_in_job_add
  f_temp_end_date_add = if_else(f_temp_end_date_add < f_max_date, 
                                f_temp_end_date_add, 
                                as.Date("2999-01-01")) # "2999-01-01" is for active information
  
  
  # Determine whether the new end date will be due to termination.  If the end date is current then it cannot be a termination.
  f_termination_flag_text <- sample(c("Termination", "Not Termination"), 1, prob=c(0.10, 0.95), replace=TRUE)
  f_termination_flag_text <- if_else(f_temp_end_date == as.Date("2999-01-01"),  "Not Termination", f_termination_flag_text) #if it is the last end date then it can't be a termination
  f_termination_flag <- if_else(f_termination_flag_text == "Termination", 1, 0)
  
  f_temp_deskhistory_table <- data.frame(
    employee_num = f_temp_employee_num,
    desk_id = f_temp_new_desk_id,
    desk_id_start_date = f_temp_start_date_add,
    desk_id_end_date = f_temp_end_date_add,
    termination_flag = f_termination_flag,
    promotion_flag = f_temp_promotion_flag)

  return(f_temp_deskhistory_table)
  }



# refresh_deskhistory_table_most_recent -----------------------------------
# Used in 05
# Create deskhistory_table_most_recent. This selects the most recent row 
# in deskhistory_table for each employee. So each desk_id has only 1 row. 
# Think of this as the current layout of the organization.
# Does not need an argument.

refresh_deskhistory_table_most_recent <- function(source_table = deskhistory_table,
                                                  depth_table = hierarchy_with_depth) {
  output_table <- source_table  %>%
    arrange(desc(desk_id_end_date)) %>%
    group_by(desk_id) %>%
    filter(row_number() == 1) %>%
    arrange(desk_id_end_date) %>%
    filter(!is.na(desk_id)) %>% 
    left_join(depth_table %>% select(desk_id, depth))
  return(output_table)
}



# get_temp_same_level -----------------------------------------------------
# Used in 05
# Gets list of all open jobs for that hierarchy level within default 90 days

get_temp_same_level <- function (f_temp_depth = temp_depth,
                                 f_temp_desk_id = temp_desk_id,
                                 source_table = deskhistory_table_most_recent,
                                 opening_window = 90) {
  output_table <- source_table %>% 
    filter(depth == f_temp_depth, desk_id != f_temp_desk_id) %>% 
    mutate(days_since_last_opening = temp_end_date - desk_id_end_date) %>% 
    filter(days_since_last_opening < opening_window,
           days_since_last_opening > 0) %>% 
    arrange(days_since_last_opening)
  return(output_table)
}


# find_external_hire ------------------------------------------------------
# This function finds a random employee number for a new employee.  
# Will be in certain states needed

find_external_hire <- function(f_desk_id = temp_desk_id,
                               f_employeeinfo_table = employeeinfo_table,
                               f_deskhistory_table = deskhistory_table,
                               f_desk_id_start_date = temp_end_date + 1,
                               #f_job_name = temp_job_name,
                               f_hierarchy_table_with_state = hierarchy_table_with_state,
                               f_cities = cities) {
  
  # Check to see if this desk_id is locked to a state and get state initials.  
  # If not associated with state then will return NA
  desk_id_state <- f_hierarchy_table_with_state %>% 
    filter(desk_id == f_desk_id) %>% 
    left_join(cities, by = c("state_present" = "State full")) %>% 
    select(state = `State short`) %>% 
    distinct() %>% 
    as.character()
  
  # Flag if desk_id is associated with state
  desk_id_state_check <- !is.na(desk_id_state)
  
  # If it is a salesjob then pick random person in same state
  # Else pick random person from any state. 
  eligible_employee <- f_employeeinfo_table %>% 
    anti_join(f_deskhistory_table) %>% #Only pick employees who are not on deskhistory
    filter(if (desk_id_state_check == TRUE)  state == desk_id_state
           else TRUE) %>% 
    sample_n(1)
  
  return(eligible_employee$employee_num)  
  
}



# create_starting_salary --------------------------------------------------
# This function looks up an employee's first deskhistory row and assigns a starting salary
# Output is row that can be appended to salary_list

create_starting_salary <- function (employee_num_temp) {
  deskhistory_table %>% 
    filter(employee_num == employee_num_temp) %>% 
    arrange(desk_id_start_date) %>% 
    filter(row_number() == 1) %>% 
    left_join(deskjob_table) %>% 
    left_join(jobs) %>% 
    rowwise() %>% 
    mutate(starting_salary_flag = "Y",
           salary_increase = 0,
           salary = round(avg_starting_salary * sample(seq(.9, 1.1, .001), 1)), 0) %>% 
    select(employee_num, 
           salary_increase_date = desk_id_start_date, 
           salary,
           salary_increase,
           starting_salary_flag) %>% 
    ungroup()
}
