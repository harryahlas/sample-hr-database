library(RMariaDB)
library(tidyverse)
library(lubridate)
source("01_functions.R")
source("02_variables.R")

HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbListTables(HRSAMPLE)

# Build contact table -----------------------------------------------------

dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS = 0;") 
dbExecute(HRSAMPLE, "DROP TABLE IF EXISTS contact;") 
dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS=1;")
dbExecute(HRSAMPLE, "CREATE TABLE contact (
          employee_num INT (11),
          contact_type VARCHAR (255),
          contact_sub_type VARCHAR (255),
          contact VARCHAR (255),
          contact_end_date DATE,
          FOREIGN KEY (employee_num) REFERENCES employeeinfo (employee_num)  ON DELETE CASCADE ON UPDATE CASCADE
);")


# Import data -------------------------------------------------------------
deskhistory_table <- dbGetQuery(HRSAMPLE, "select * from deskhistory")
employeeinfo_table <- dbGetQuery(HRSAMPLE, "select * from employeeinfo")

# Import area codes to be used.  These are actual planned area codes.  7 trailing digits are composed randomly.
area_codes <- read_csv("data/area_codes.csv")


contact_max_end_date <- as.Date("2999-01-01")


# Create work email addresses ---------------------------------------------
email_addresses <- deskhistory_table %>% 
  select(employee_num) %>% 
  distinct() %>% 
  left_join(employeeinfo_table) %>% 
  mutate(add_work_email = sample(c(1,0), 1, prob = c(993,7), replace = TRUE),
         contact = ifelse(add_work_email == 1, paste0(tolower(first_name), ".", tolower(last_name), "@", company_website), NA),
         contact_type = "email",
         contact_sub_type = "work",
         contact_end_date = contact_max_end_date) 

# Count/validate email unique
# If this is false then there will be duplicate email addresses
nrow(email_addresses) == email_addresses %>% select(contact) %>% n_distinct()


# Create work and personal phone numbers ----------------------------------

# Get distinct list of all employees, desk_id, and max desk_id_end_date
# For each row get 1 phone number, should be work number.  contact end date is desk_id end date.  Some should be blank (1/387)

work_phone_list <- deskhistory_table %>% 
  group_by(employee_num, desk_id) %>% 
  summarize(desk_id_end_date_max = max(desk_id_end_date)) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(add_work_phone = sample(c(1,0), 1, prob = c(387,1), replace = TRUE),
         contact = ifelse(add_work_phone == 1, create_phone_number(), NA),
         contact_end_date = desk_id_end_date_max,
         contact_type = "phone", 
         contact_sub_type = "work") 

# Get distinct list of all  employees, and max desk_id_end_date, min desk_id_start_date
# For each row .89 add personal phone
# For each row .08 add personal phone 2
# for each person, get their minimum start date, for .18 of them add expired personal phone during min/max dates
# for each person, get their minimum start date, for .06 of them add expired personal phone during min/max dates
# for each person, get their minimum start date, for .004 of them add expired personal phone during min/max dates

personal_phone_list1 <- deskhistory_table %>% 
  select(employee_num) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(add_personal_phone = sample(c(1,0), 1, prob = c(89,11), replace = TRUE),
         contact = ifelse(add_personal_phone == 1, create_phone_number(), NA),
         contact_end_date = contact_max_end_date) %>% 
  filter(!is.na(contact)) %>% 
  mutate(contact_type = "phone", contact_sub_type = "personal")

personal_phone_list2 <- deskhistory_table %>% 
  select(employee_num) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(add_personal_phone = sample(c(1,0), 1, prob = c(8,92), replace = TRUE),
         contact = ifelse(add_personal_phone == 1, create_phone_number(), NA),
         contact_end_date = contact_max_end_date) %>% 
  filter(!is.na(contact)) %>% 
  mutate(contact_type = "phone", contact_sub_type = "personal")

personal_phone_list3 <- deskhistory_table %>% 
  group_by(employee_num) %>% 
  mutate(desk_id_end_date_max = max(desk_id_end_date),
            desk_id_end_date_min = min(desk_id_end_date)) %>% 
  ungroup() %>% 
  #####filter out highest end date (2099)
  filter(desk_id_end_date != contact_max_end_date) %>% 
  rowwise() %>% 
  mutate(add_personal_phone = sample(c(1,0), 1, prob = c(18,82), replace = TRUE),
         contact = ifelse(add_personal_phone == 1, create_phone_number(), NA),
         contact_end_date = sample(seq.Date(desk_id_end_date_min, min(desk_id_end_date_max, end_date_of_hierarchy), by = "days"),1)) %>% 
  filter(!is.na(contact)) %>% 
  mutate(contact_type = "phone", contact_sub_type = "personal")


contact_table <- bind_rows(work_phone_list,
                          personal_phone_list1,
                          personal_phone_list2,
                          personal_phone_list3,
                          email_addresses) %>% 
  select(employee_num, 
         contact_type,
         contact_sub_type, 
         contact, 
         contact_end_date)


# Create collisions -------------------------------------------------------

# Get sample of employees with active work number
employee_work_numbers_to_replace <- contact_table %>% 
  filter(contact_sub_type == "personal", contact_end_date == as.Date("2999-01-01")) %>% 
  sample_n(45) %>% 
  select(employee_num, contact_new = contact) %>% 
  mutate(replace_work_number = "yes")

# Replace work numbers with personal number selected above
contact_table <- contact_table %>% 
  left_join(employee_work_numbers_to_replace) %>% 
  rowwise() %>% 
  mutate(contact = if_else(replace_work_number == "yes" & 
                             contact_sub_type == "work" &
                             contact_end_date == as.Date("2999-01-01"),
                           contact_new,
                           contact)) %>% 
  select(-contact_new, -replace_work_number)






# Validation --------------------------------------------------------------

contact_table %>% count(contact, sort = T)
contact_table %>% count(employee_num, contact_end_date) %>% arrange(desc(n))
contact_table %>% filter(contact_end_date == as.Date("2999-01-01")) %>% count(contact_sub_type)


# Upload data -------------------------------------------------------------

# Populate contact
contact_sql <- paste(
  "INSERT INTO contact (employee_num, contact_type, contact_sub_type, contact, contact_end_date) VALUES ",
  paste0(
    "('",
    contact_table$employee_num, "','",
    contact_table$contact_type, "','",
    contact_table$contact_sub_type, "','",
    contact_table$contact, "','",
    contact_table$contact_end_date, "')",
    collapse = ", "),
  ";"
)

dbExecute(HRSAMPLE, contact_sql)


