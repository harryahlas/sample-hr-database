library(RMariaDB)
library(tidyverse)
library(lubridate)
library(fuzzyjoin)
source("01_functions.R")
source("02_variables.R")

# Connect to database -----------------------------------------------------
HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')

# Build performancereview table -------------------------------------------
dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS = 0;") 
dbExecute(HRSAMPLE, "DROP TABLE IF EXISTS performancereview;") 
dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS=1;")
dbExecute(HRSAMPLE, "CREATE TABLE performancereview (
          employee_num INT (11),
          year INT (4),
          perf_review_score INT (1),
          FOREIGN KEY (employee_num) REFERENCES employeeinfo (employee_num)  ON DELETE CASCADE ON UPDATE CASCADE
);")

# Build salaryhistory table -----------------------------------------------
dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS = 0;") 
dbExecute(HRSAMPLE, "DROP TABLE IF EXISTS salaryhistory;") 
dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS=1;")
dbExecute(HRSAMPLE, "CREATE TABLE salaryhistory (
          employee_num INT (11),
          salary_effective_date DATE,
          salary DECIMAL (13,2),
          salary_increase DECIMAL (4,3),
          starting_salary_flag VARCHAR (1),
          FOREIGN KEY (employee_num) REFERENCES employeeinfo (employee_num)  ON DELETE CASCADE ON UPDATE CASCADE
);")

#need to create this as import, used in 06
# First day of hierarchy, same as max(employeeinfo_table$hire_date)
hierarchy_start_date <- first_date_of_hierarchy

#need to create this as import, used in 06
# Most recent date that a new job could be had
max_date <- end_date_of_hierarchy

# Import deskhistory
deskhistory_table <- dbGetQuery(HRSAMPLE, "SELECT *  FROM deskhistory")

# Import desk_job table
deskjob_table <- dbGetQuery(HRSAMPLE, "SELECT *  FROM deskjob")

# Create business lines from 01
lob <- read_csv("data/lob.csv")

# Import jobs
jobs <- read_csv("data/jobs.csv")

# Create hierarchy and join to lob for report to count  from 01
hierarchy <- dbGetQuery(HRSAMPLE, "SELECT *  FROM hierarchy")


# Rollup
hierarchy_spread <- hierarchy %>% 
  mutate(lvl00_desk_id = 0,
         lvl00_org = "CEO") %>% 
  select(lvl00_desk_id,
         lvl00_org,
         everything()) %>% 
  filter(parent_id == 1) %>% 
  rename(lvl01_desk_id = desk_id,
         lvl01_org = org) %>% 
  select(-parent_id) %>% 
  left_join(hierarchy, by = c("lvl01_desk_id" = "parent_id")) %>% 
  rename(lvl02_desk_id = desk_id,
         lvl02_org = org) %>% 
  left_join(hierarchy, by = c("lvl02_desk_id" = "parent_id")) %>% 
  rename(lvl03_desk_id = desk_id,
         lvl03_org = org) %>% 
  left_join(hierarchy, by = c("lvl03_desk_id" = "parent_id"))  

# Add level 1-3 employees since hierarchy_spread only counts level 04


hierarchy_spread_lvl02 <- hierarchy_spread %>% 
  select(lvl01_desk_id, lvl01_org, desk_id = lvl02_desk_id) %>% 
  distinct()

hierarchy_spread_lvl03 <- hierarchy_spread %>% 
  select(lvl01_desk_id, lvl01_org, desk_id = lvl03_desk_id) %>% 
  distinct()

hierarchy_spread_lvl01 <- hierarchy_spread %>% 
  select(lvl00_desk_id, lvl00_org, desk_id = lvl01_desk_id) %>% 
  distinct() %>% 
  left_join((hierarchy_spread_lvl02 %>% select(lvl01_desk_id, lvl01_org)), by = c("desk_id" = "lvl01_desk_id"))

### Note this hierarchy_spread_all is different than from 04. Includes everyone except CEO
hierarchy_spread_all <- hierarchy_spread %>% 
  bind_rows(hierarchy_spread_lvl01, hierarchy_spread_lvl02, hierarchy_spread_lvl03)



# Get highest seat of lvl01s. Anyone below this number will not get a review here
desk_id_no_review_max = max(hierarchy_spread_all$lvl01_desk_id, na.rm = TRUE)

# Import perf_review_distributions
performance_review_ratios <- read_csv("data/performance_review_ratios.csv")

# Get list of all employees in deskhistory except CEO (desk_id 1)
employee_list <- deskhistory_table %>% 
  filter(desk_id > desk_id_no_review_max) %>% 
  select(employee_num) %>% 
  distinct()

# Economic trend information
prime_rate <- read_csv("data/prime_rate.csv")

# Random chance that employee does not get a review for some reason
odds_of_no_review <- .013

review_year_list <- tibble()
salaryhistory_table <- tibble()

# Promotion/new desk_id salary increase min/max
promo_new_desk_salary_increase_min <- .04
promo_new_desk_salary_increase_max <- .15
standard_salary_increase_min <- .01
standard_salary_increase_max <- .03



# Start loop --------------------------------------------------------------

for (i in 1:nrow(employee_list)) {

  employee_num_temp <- employee_list$employee_num[i]
  # Minimum review value reset
  min_review_value <- 1
  
  # Pull employee's entire desk history
  deskhistory_table_temp <- deskhistory_table %>% 
    filter(employee_num == employee_num_temp)

  # Identify full years employee was active
  
  
  # Find years with promotions
  promotions_temp <- deskhistory_table_temp %>% 
    filter(promotion_flag == 1) %>% 
    mutate(promotion_year = year(desk_id_start_date)) %>% 
    select(promotion_year)
  
  # See if employee has any promotions
  has_promotion <- nrow(promotions_temp) > 0
  
  # if they have any promotions then their minimum perf_review = 3
  if (has_promotion == TRUE) {min_review_value <- 3}
  
  # for each row, identify the full years they were active. 
  company_start <- min(deskhistory_table_temp$desk_id_start_date)
  company_start_month <- month(company_start)
  company_start_year <- year(company_start)
  company_end <- min(max(deskhistory_table_temp$desk_id_end_date), max_date)
  company_end_month <- month(company_end)
  company_end_year <- year(company_end)
  
  # if start month is before october then count that year, else count next year
  review_year_start <- case_when(company_start_month < 10 ~ company_start_year, 
                                 (company_start_month >= 10) & (company_end_year > (company_start_year + 1)) ~   company_start_year + 1,
                                 TRUE ~ 0)
  #####IF REVIEWS ARE MISSING MAYBE BECAUSE CHANGED LINE BELOW
  review_year_end <- case_when(company_end_month > 3 ~ company_end_year - 1, 
                               #company_end_month <= 3 ~ max(company_end_year - 2, review_year_start),
                               company_end_month <= 3 ~ company_end_year - 2,
                               TRUE ~ 0)
  if (review_year_start == 0 | review_year_end == 0) next
  if (review_year_start > review_year_end) next
  
  review_years <- seq(review_year_start, review_year_end, 1)

  # Calculate reviews
  review_year_list_append <- tibble(employee_numx = rep(employee_num_temp, length(review_years)),
                                                       review_year = review_years,
                                    review_date = as.Date(paste0(review_year + 1, "-03-01"))) %>% 
    fuzzy_left_join(deskhistory_table, by = c(
      "employee_numx" = "employee_num",
      "review_date" = "desk_id_start_date",
      "review_date" = "desk_id_end_date"
    ),
    match_fun = list(`==`, `>=`, `<=`)) %>% 
    select(-employee_numx) %>% 
    left_join(hierarchy_spread_all %>% select(desk_id, lvl01_org)) %>%
    distinct() %>% 
    left_join(performance_review_ratios, by = c("lvl01_org" = "LOB")) %>% 
    arrange(review_year) %>% 
    mutate(next_year_promotion_flag = lead(promotion_flag)) %>% #did they have promotion the next year?
    mutate(perf_review_score_4 = if_else(promotion_flag ==1, perf_review_score_4 * 2, perf_review_score_4),
           perf_review_score_5 = if_else(promotion_flag ==1, perf_review_score_5 * 2, perf_review_score_5)) %>% #if so then double their chances of getting a 4 or 5 
    rowwise() %>% 
    filter(!is.na(employee_num)) #%>% #Added to fix rehire issue
    
  if(nrow(review_year_list_append) == 0) next
  
  review_year_list_append <- review_year_list_append %>% 
    # Calculate performance review
    mutate(perf_review_score = max(sample(c(1,2,3,4,5,NA), 1, 
                                          prob= c(perf_review_score_1,
                                                  perf_review_score_2,
                                                  perf_review_score_3,
                                                  perf_review_score_4,
                                                  perf_review_score_5,
                                                  odds_of_no_review),
                                          replace=TRUE),
                                   min_review_value)) 

  review_year_list <- bind_rows(review_year_list, review_year_list_append)

  
  # Create salary increases for promotions and job changes
  salary_increases_promo_new_desk <- deskhistory_table_temp %>% 
    # Remove increase for start dates at beginning of data
    rowwise() %>% 
    mutate(salary_increase = case_when(
      desk_id_start_date == hierarchy_start_date ~ 0,
      ### NEW added below line and needs testing
      desk_id_start_date == min(desk_id_start_date) ~ 0,
      TRUE ~ sample(seq(promo_new_desk_salary_increase_min,promo_new_desk_salary_increase_max,.005),1))) %>% 
    ungroup() %>% 
    mutate(new_desk_id = lag(desk_id) != desk_id) %>% # if it is a new desk_id d b/w oct 1 - april 30 then add year to list so no march 1 increas
    mutate(merit_increase_remove_year = case_when(
      (promotion_flag == 1 | new_desk_id == TRUE ) & month(desk_id_start_date) <= 4 ~ year(desk_id_start_date) - 1, # no merit increase if recently got promotion
      (promotion_flag == 1 | new_desk_id == TRUE ) & month(desk_id_start_date) >= 10 ~ year(desk_id_start_date)  # or if got new deskid at end of last year
    ),
    salary_increase_date = desk_id_start_date) %>% 
    filter(salary_increase > 0)
             
  # create years for no increase 
  # if there was a promotion between Jan 1 and Apr 30 then add year to list so no march 1 increase
  # if it is a new desk id b/w oct 1 - april 30 then add year to list so no march 1 increase
  
  # Create salary increases for years without promotions and job changes
  # tie this to prime rate and maybe LOB, should be 1-7%
  ####ERROR HERE
  salary_increases_standard <- tibble(review_year = review_years) %>% 
    anti_join(salary_increases_promo_new_desk, by = c("review_year" = "merit_increase_remove_year")) 
  
  if(nrow(salary_increases_standard) == 0) next # needed to avoid error in next section

  salary_increases_standard <- salary_increases_standard %>% 
    left_join(prime_rate, by = c("review_year" = "year")) %>% 
    left_join(review_year_list_append %>% select(review_year, perf_review_score)) %>% 
    rowwise() %>% 
    mutate(employee_num = employee_num_temp,
           prime_rate_add = max((prime_rate_est - .03) / 2, 0),
           perf_review_add = case_when(perf_review_score == 5 ~ .03,
                                       perf_review_score == 4 ~ .015,
                                       TRUE ~ 0),
           no_merit_multiplier = if_else(perf_review_score %in% c(1,2), 0, 1)) %>% 
    mutate(salary_increase = (sample(seq(standard_salary_increase_min, standard_salary_increase_max,.001),1) 
                              + prime_rate_add 
                              + perf_review_add) 
                              * no_merit_multiplier,
           salary_increase_date = as.Date(paste0(review_year + 1, "-03-01")))
  
  ### enhancemnt: if possible make it higher if they got 4 or 5
  
  # Starting salary plug for later function
  starting_salary_table <-  create_starting_salary(employee_num_temp)
  starting_salary <- starting_salary_table$salary[1] 
  
  # Bind starting salary, promotions/new desk, and standard increases
  salaryhistory_table_append <- bind_rows(starting_salary_table, 
                                  salary_increases_promo_new_desk %>% 
                                    select(employee_num, salary_increase_date, salary_increase),
                                  salary_increases_standard %>% 
                                    select(employee_num, salary_increase_date, salary_increase)) %>% 
    arrange(salary_increase_date) %>% 
    mutate(salary = starting_salary * cumprod(1 + salary_increase))
                                        
  if (is.na(salaryhistory_table_append$salary[1])) {
    print("skipped, initial salary missing") 
  next
    }
  salaryhistory_table <- bind_rows(salaryhistory_table, salaryhistory_table_append)
  
  print(i)
}

# Remove rows without performance review scores from performance review table
review_year_list <- review_year_list %>% 
  filter(!is.na(perf_review_score))


# Replace NAs on salaryhistory starting_salary_flag with N 
salaryhistory_table <- salaryhistory_table %>% 
  mutate(starting_salary_flag = if_else(is.na(starting_salary_flag),
                                        "N",
                                        starting_salary_flag))

# Backup
save(review_year_list, file = "data/review_year_list.rda")
save(salaryhistory_table, file = "data/salaryhistory_table.rda")



# Populate performancereview ----------------------------------------------

# First, clear old data from performancereview
#dbExecute(HRSAMPLE, "DELETE FROM performancereview")

# Populate performancereview
review_year_list_sql <- paste(
  "INSERT INTO performancereview (employee_num, year, perf_review_score) VALUES ",
  paste0(
    "('",
    review_year_list$employee_num, "','",
    review_year_list$review_year, "','",
    review_year_list$perf_review_score, "')",
    collapse = ", "),
  ";"
)

dbExecute(HRSAMPLE, review_year_list_sql)


# Populate salaryhistory ----------------------------------------------

# First, clear old data from salaryhistory
#dbExecute(HRSAMPLE, "DELETE FROM salaryhistory")

# Populate salaryhistory
salaryhistory_sql <- paste(
  "INSERT INTO salaryhistory (employee_num, salary_effective_date, salary, salary_increase, starting_salary_flag) VALUES ",
  paste0(
    "('",
    salaryhistory_table$employee_num, "','",
    salaryhistory_table$salary_increase_date, "','",
    salaryhistory_table$salary, "','",
    salaryhistory_table$salary_increase, "','",
    salaryhistory_table$starting_salary_flag, "')",
    collapse = ", "),
  ";"
)

dbExecute(HRSAMPLE, salaryhistory_sql)










#analysis/validations

temp_salary_calc_table <- salaryhistory_table %>% 
  arrange(employee_num, salary_increase) %>% 
  mutate(salary = 0) %>% 
  mutate(salary = case_when(lag(employee_num) != employee_num ~ 50000,
                            TRUE ~ lag(salary) * (1 + salary_increase)))

# Check TM
checktable <- deskhistory_table %>% 
  group_by(employee_num) %>% 
  summarize(start_date = min(desk_id_start_date),
            end_date = max(desk_id_end_date))

reviewcheck <- review_year_list %>% 
  group_by(employee_num) %>% 
  summarize(start_year = min(review_year),
            end_year = max(review_year))

validation <- checktable %>% 
  left_join(reviewcheck)



### Need to add a check that TMs that left company and came back did not get a review during that period


deskhistory_table %>% 
  group_by(employee_num) %>% 
  summarize(promotion_count = sum(promotion_flag)) %>% 
  count(promotion_count)
