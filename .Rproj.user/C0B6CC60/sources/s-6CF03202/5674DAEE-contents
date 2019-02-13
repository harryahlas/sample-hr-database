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

refresh_deskhistory_table_most_recent <- function(source_table = deskhistory_table) {
  output_table <- source_table  %>%
    arrange(desc(desk_id_end_date)) %>%
    group_by(desk_id) %>%
    filter(row_number() == 1) %>%
    arrange(desk_id_end_date) %>%
    filter(!is.na(desk_id))
  return(output_table)
}
