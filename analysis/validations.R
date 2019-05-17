library(tidyverse)
library(hrsample)
library(lubridate)
library(fuzzyjoin)


# Check that active employees have 2999 end date --------------------------

active_employees <- deskhistory_table %>% 
  group_by(employee_num) %>% 
  mutate(max_desk_id_end_date = max(desk_id_end_date),
            max_termination_flag = max(termination_flag)) %>% 
  ungroup() %>% 
  filter(desk_id_end_date == max_desk_id_end_date,
         max_termination_flag == 0) 

# Should only have 1 row (max date)
active_employees %>% 
  count(desk_id_end_date)


# Make sure new jobs are starting toward end of process -------------------

# Should return multiple rows
deskhistory_table %>% 
  filter(between(desk_id_start_date, as.Date("2018-10-01"), as.Date("2019-01-01")))

# Make sure all tables have valid employees and desks ----------------------
# Should all be 0
deskhistory_table %>% 
  anti_join(employeeinfo_table) %>% 
  nrow()

deskjob_table %>% 
  anti_join(deskhistory_table) %>% 
  nrow()

hierarchy_table %>% 
  anti_join(deskhistory_table) %>% 
  nrow()

performancereview_table %>% 
  anti_join(employeeinfo_table) %>% 
  nrow()

salaryhistory_table %>% 
  anti_join(employeeinfo_table) %>% 
  nrow()

recruiting_table %>% 
  anti_join(employeeinfo_table) %>% 
  nrow()

rollup_view %>% 
  anti_join(deskhistory_table, by = c("lvl04_desk_id" = "desk_id")) %>% 
  nrow()

contact_table %>% 
  anti_join(employeeinfo_table) %>% 
  nrow()

education_table %>% 
  anti_join(employeeinfo_table) %>% 
  nrow()

skills_table %>% 
  anti_join(employeeinfo_table) %>% 
  nrow()


# Employees without a review ----------------------------------------------

no_review <- deskhistory_table %>%
  select(employee_num) %>% 
  distinct() %>% 
  anti_join(performancereview_table) %>% 
  left_join(deskhistory_table) %>% 
  arrange(employee_num, desk_id_end_date)

# Rehires -----------------------------------------------------------------

rehires_history <- deskhistory_table %>% 
  mutate(endyear = year(desk_id_end_date),
         termyear = ifelse(termination_flag == 1, endyear, NA)) %>% 
  group_by(employee_num) %>% 
  mutate(mintermyear = min(termyear, na.rm = TRUE),
         maxyear = max(endyear)) %>% 
  filter(maxyear > termyear) %>% 
  select(employee_num) %>% 
  left_join(deskhistory_table) %>% 
  arrange(employee_num, desk_id_end_date)

# Number of rehires
rehires_history %>% 
  select(employee_num) %>% 
  n_distinct()

# Check that TMs that left company and came back did not get a review during that period ----
rehires_reviews <- rehires_history %>% 
  select(employee_num) %>% 
  distinct() %>% 
  left_join(performancereview_table)

# Check this later

# 3. Graph review score vs tenure
# a. review in 2008 < 3 then higher chance of turnover in 2009?
deskhistory_terms <- deskhistory_table %>% 
  mutate(endyear = year(desk_id_end_date),
         termyear = ifelse(termination_flag == 1, endyear, NA)) %>% 
  filter(!is.na(termyear))
  
perf_terms <- performancereview_table %>% 
  mutate(termyearcheck = year + 1) %>% 
  left_join(deskhistory_terms, by = c("employee_num", "termyearcheck" = "termyear"))

perf_terms %>% 
  mutate(poor_perf_review_flag = if_else(perf_review_score %in% c(1,2), "1 or 2", "3 or better")) %>% 
  count(termination_flag,  poor_perf_review_flag) %>% 
  ggplot(aes(x = as.factor(poor_perf_review_flag), y = n, fill = termination_flag)) +
  geom_col(position = "fill") +
  labs(title = "% of employees that termed the year \nfollowing review score below",
       subtitle = "Should be higher for 1 or 2 scores")

#   b. turnover rate for tms with 1-2 vs 3, 4,5
performancereview_table %>% 
  group_by(employee_num) %>% 
  mutate(min_perf_review = min(perf_review_score, na.rm = TRUE)) %>% 
  filter(perf_review_score == min_perf_review) %>% 
  filter(row_number() == 1) %>% 
  left_join(deskhistory_terms %>% 
              select(employee_num, termination_flag)) %>% 
  ungroup() %>% 
  count(min_perf_review, termination_flag) %>% 
  ggplot(aes(x = as.factor(min_perf_review), y = n, fill = termination_flag)) +
  geom_col(position = "fill") +
  labs(title = "% of employees that termed by their minimum review score",
       subtitle = "Not higher for 1 or 2 scores because it means less jobs")

#   b. tenure for tms with 1-2 vs 3, 4,5
termed_employees_start_end <- deskhistory_terms %>% 
  select(employee_num) %>% 
  left_join(deskhistory_table, by = "employee_num") %>% 
  group_by(employee_num) %>% 
  summarize(start_date = min(desk_id_start_date),
            end_date = max(desk_id_end_date)) %>% 
  filter(end_date != as.Date("2999-01-01")) %>% 
  mutate(tenure = as.numeric(round((end_date - start_date) / 365, 3))) %>% 
  ungroup()



performancereview_table %>% 
  group_by(employee_num) %>% 
  mutate(min_perf_review = min(perf_review_score, na.rm = TRUE)) %>% 
  filter(perf_review_score == min_perf_review) %>% 
  filter(row_number() == 1) %>% 
  select(employee_num, min_perf_review) %>% 
  right_join(termed_employees_start_end) %>% 
  ungroup() %>% 
  group_by(min_perf_review) %>% 
  summarize(avg_tenure = mean(tenure, na.rm = TRUE),
            count = n()) %>% 
  ggplot(aes(x = as.factor(min_perf_review), y = avg_tenure, size = count)) +
  geom_point() +
  labs(title = "Avg Tenure by their minimum review score",
       subtitle = "Should be lower for 1 or 2 scores.
       Difficult to have minimum 4 or 5 score.  
       Super difficult if you have long tenure. 
       For that reason, lower tenure for 4-5s.")


# 4. Check movement, turnover, and promotions for levels 1-3

# Retrieve depth table - NOTE: if desk_ids are added/moved/removed then this needs to be included in loop
library(RMariaDB)
HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
hierarchy_with_depth.sql <- read_file("C:\\Development\\github\\sample-hr-database\\scripts\\hierarchy_with_depth.sql")
hierarchy_with_depth <- dbGetQuery(HRSAMPLE, hierarchy_with_depth.sql)

# a. Some movement between levels
deskhistory_table %>% 
  select(employee_num, desk_id) %>%
  distinct() %>% 
  left_join(hierarchy_with_depth) %>% 
  select(employee_num, depth) %>% 
  distinct() %>% 
  count(employee_num) %>% 
  rename(count_of_different_levels = n) %>% 
  arrange(desc(count_of_different_levels)) %>% 
  count(count_of_different_levels)

# b. Turnover seems ok
deskhistory_table %>% 
  select(employee_num, desk_id) %>%
  distinct() %>% 
  left_join(hierarchy_with_depth) %>% 
  select(employee_num, depth) %>% 
  distinct() %>%
  left_join(termed_employees_start_end) %>%
  mutate(status = if_else(!is.na(tenure), "terminated", "not terminated")) %>% 
  count(depth, status) %>% 
  ggplot(aes( x = as.factor(depth), y = n, fill = status)) +
  geom_col(position = "fill") +
  labs(title = "% of employees who terminated\nat some point by level")

# c. Promotions
deskhistory_table %>% 
  select(employee_num, desk_id) %>%
  distinct() %>% 
  left_join(hierarchy_with_depth) %>% 
  select(employee_num, depth) %>% 
  distinct() %>%
  left_join(deskhistory_table) %>%
  select(employee_num, depth, promotion_flag) %>% 
  distinct() %>% 
  count(depth, promotion_flag) %>% 
  ggplot(aes( x = as.factor(depth), y = n, fill = promotion_flag)) +
  geom_col(position = "fill") +
  labs(title = "% of employees who received\na Promotion at some point by level")

# 5. Number of TMs right now
deskhistory_table %>% 
  filter(desk_id_end_date >= as.Date("2019-01-01"),
         desk_id_start_date <= as.Date("2019-01-01")) %>% 
  nrow()

# Number of desk_ids
rollup_view %>% nrow()

# 6. check job by states
state_ratios <- deskhistory_table %>% 
  filter(desk_id_end_date >= as.Date("2019-01-01")) %>% 
  left_join(employeeinfo_table) %>% 
  count(state) %>% 
  left_join(employeeinfo_table %>% 
              count(state) %>% 
              rename(state_ratio = n)) %>% 
  mutate(pct_state = n / state_ratio)  %>% 
  arrange(desc(pct_state)) 


# 7. Check promotions - see if people are getting promotions after a 1 or 2 review
deskhistory_promotions <- deskhistory_table %>% 
  mutate(endyear = year(desk_id_end_date),
         promotionyear = ifelse(promotion_flag == 1, endyear, NA)) %>% 
  filter(!is.na(promotionyear))

perf_promotions <- performancereview_table %>% 
  mutate(promotionyearcheck = year + 1) %>% 
  left_join(deskhistory_promotions, by = c("employee_num", "promotionyearcheck" = "promotionyear"))

perf_promotions %>% 
  mutate(poor_perf_review_flag = if_else(perf_review_score %in% c(1,2), "1 or 2", "3 or better")) %>% 
  count(promotion_flag,  poor_perf_review_flag) %>% 
  ggplot(aes(x = as.factor(poor_perf_review_flag), y = n, fill = promotion_flag)) +
  geom_col(position = "fill") +
  labs(title = "% of employees that promoted the year \nfollowing review score below",
       subtitle = "Maybe should be lower for 1 or 2 scores")

perf_promotions %>% 
  count(promotion_flag,  perf_review_score) %>% 
  ggplot(aes(x = as.factor(perf_review_score), y = n, fill = promotion_flag)) +
  geom_col(position = "fill") +
  labs(title = "% of employees that promoted the year \nfollowing review score below",
       subtitle = "Maybe should be lower for 1 or 2 scores")

# 8. Termination rate 
deskhistory_table %>% 
  filter(desk_id_start_date <= as.Date("2010-01-01"),
         desk_id_end_date >= as.Date("2009-01-01")) %>% 
  mutate(termination = if_else(termination_flag == 1 & (lubridate::year(desk_id_end_date) == 2009 ), "term2017", "notterm2017")) %>% 
  count(termination) %>% 
  spread(key = termination, value = n) %>% 
  mutate(pct = term2017 / (term2017 + notterm2017))





# Headcount by Year -------------------------------------------------------
hcyear <- seq(1999,2019,1)

get_end_of_year_headcount <- function (hcyear = 1999) {
  x <- deskhistory_table %>% 
    filter(desk_id_end_date >= as.Date(paste0(hcyear, "-12-31")),
           desk_id_start_date <= as.Date(paste0(hcyear, "-12-31"))) %>% 
    nrow()
  return(x)
}

hc_by_year <- tibble(hcyear = hcyear) %>% 
  rowwise() %>% 
  mutate(end_of_year_hc = get_end_of_year_headcount(hcyear)) 

hc_by_year %>% 
  ggplot(aes(hcyear, end_of_year_hc)) +
  geom_col()

source("C:\\Development\\github\\sample-hr-database\\02_variables.R")

bad_employee_table <- dbGetQuery(HRSAMPLE, "select * from bademployee")
#aaa <- dbGetQuery(HRSAMPLE, "select * from employeeinfo")

#terminations by year
terms_by_year <- deskhistory_table %>% 
  filter(termination_flag == 1) %>% 
  left_join(employeeinfo_table) %>% 
  left_join(bad_employee_table) %>% 
  mutate(year = year(desk_id_end_date)) %>% 
  count(bad_employee_flag, year) %>% # can get bad employee table
  spread((bad_employee_flag), n) %>% 
  left_join(hc_by_year, by = c("year" = "hcyear")) %>% 
  mutate(normal_emp_pct = (`0` / end_of_year_hc),
         bad_emp_pct = (`1` / (end_of_year_hc * bad_employee_ratio[1]))) # divided by 10 because 1/10 employees are bad

terms_by_year %>% 
  filter(!is.na(bad_emp_pct), year < 2020) %>% 
  ggplot(aes(year, bad_emp_pct)) +
  geom_line()


# Level changing ----------------------------------------------------------
# deskhistory job level distinct count
deskhistory_table %>% 
  select(employee_num, desk_id) %>% 
  left_join(hierarchy_with_depth %>% select(desk_id, depth)) %>% 
  select(employee_num, depth) %>% 
  distinct() %>% 
  count(employee_num) %>% 
  arrange(desc(n))


# Time between jobs -------------------------------------------------------
# Next: maybe add tenure by line of business
# Note: excludes rehires
deskhistory_table %>% 
  filter(termination_flag == 1, desk_id_end_date < as.Date("2019-01-01")) %>% 
  select(employee_num) %>%
  anti_join(rehires_reviews) %>% 
  left_join(deskhistory_table) %>% 
  group_by(employee_num) %>% 
  mutate(end_date = max(desk_id_end_date),
         start_date = min(desk_id_start_date)) %>% 
  ungroup() %>% 
  mutate(tenure = as.numeric(end_date - start_date)) %>% 
  select(employee_num, tenure) %>%
  distinct() %>% 
  left_join(bad_employee_table) %>% 
  ggplot(aes(as.factor(bad_employee_flag), tenure)) +
  geom_boxplot()

# Bad Manager analysis ----------------------------------------------------

# So get term rates for each manager
# For one: get mgr desk_ids and time periods
# get list of desk_ids that report to mgr desk_ids
# filter deskhistory for those desk_ids during those dates

# function to get term rate for a manager
#Start manually for a couple
#rollup <- dbGetQuery(HRSAMPLE, "SELECT * FROM ROLLUP") #Vies on mysql only
rollup <- rollup_view
bademployee <- dbGetQuery(HRSAMPLE, "SELECT * FROM bademployee") #Vies on mysql only


# needed below as well
manager_desk_ids <- hierarchy_with_depth %>% 
  filter(depth < 4) %>% #manager desk ids
  select(desk_id) 

bad_managers_deskhistory <- deskhistory_table %>% # their employee#s
  semi_join(manager_desk_ids) %>% 
  left_join(bademployee %>% 
              select(employee_num, bad_employee_flag)) %>% 
  filter(bad_employee_flag == 1)

deskhistory_w_parent <- deskhistory_table %>% 
  left_join(hierarchy_with_depth %>% select(desk_id, parent_id)) 

desk_history_if_bad_manager <- deskhistory_w_parent %>% 
  fuzzy_semi_join(bad_managers_deskhistory, by = c(
    "parent_id" = "desk_id",
    "desk_id_start_date" = "desk_id_end_date",
    "desk_id_start_date" = "desk_id_start_date"
  ),
  match_fun = list(`==`, `<=`, `>=`)) 

desk_history_if_not_bad_manager <- deskhistory_w_parent %>% 
  fuzzy_anti_join(bad_managers_deskhistory, by = c(
    "parent_id" = "desk_id",
    "desk_id_start_date" = "desk_id_end_date",
    "desk_id_start_date" = "desk_id_start_date"
  ),
  match_fun = list(`==`, `<=`, `>=`)) 

  
# Should be higher pct for bad manager
desk_history_if_bad_manager %>% count(termination_flag) %>% spread(termination_flag, n) %>% mutate(pct = `1` / (`1` + `0`))
desk_history_if_not_bad_manager %>% count(termination_flag)%>% spread(termination_flag, n) %>% mutate(pct = `1` / (`1` + `0`))



# Bad manager analysis ----------------------------------------------------
#remove this one?
mgr_desk_history_snapshot <- rollup_view %>% 
  select(manager_desk_id = lvl03_desk_id) %>% 
  distinct() %>% 
  left_join(deskhistory_table, by = c("manager_desk_id" = "desk_id"))
  
# Turn this into a view
# Select month
trend_start_date <- as.Date("2000-01-01")
trend_end_date <- as.Date("2018-12-31")
month_sequence <- seq(trend_start_date, trend_end_date, by = 'months')

#for (month in month_sequence) {print(as.Date(month))}

mgr_hcterms <- tibble()
for (i in (1:length(month_sequence))) {
#for (i in (1:2)) {
  print(as.Date(month_sequence[i]))
  term_month_start <- as.Date(month_sequence[i])
  term_month_end <- ceiling_date(as.Date(month_sequence[i]), "month") - 1
  trendmonth <- term_month_start
  print(term_month_start)
  
  #start here
  mgr_desk_history_snapshot <- hierarchy_table %>% 
    select(manager_desk_id = parent_id) %>% 
    distinct() %>% 
    left_join(deskhistory_table %>%
                filter(desk_id_end_date >= term_month_end, 
                       desk_id_start_date < term_month_end),
              by = c("manager_desk_id" = "desk_id")) 
   
  
  headcount_monthly <- deskhistory_table %>% 
    filter(desk_id_end_date >= term_month_end, 
           desk_id_start_date <= term_month_start) %>% 
    left_join(hierarchy_table) %>% 
    count(parent_id) %>% 
    rename(manager_desk_id = parent_id, headcount = n)
  
  # calculate # terms for each month for each manager
  termcount_monthly <- deskhistory_table %>% 
    filter(desk_id_end_date >= term_month_start, 
           desk_id_start_date <= term_month_end) %>% 
    left_join(hierarchy_table) %>% 
    mutate(terminated_this_month = ifelse(termination_flag == 1 &
                                            year(desk_id_end_date) == year(term_month_end) &
                                            month(desk_id_end_date) == month(term_month_end),
                                          1, 0)) %>% 
    group_by(parent_id) %>% 
    summarize(termcount = sum(terminated_this_month)) %>% 
    select(manager_desk_id = parent_id, termcount)
####need to find mgr desk history snapshot    
  mgr_hcterms_temp <- mgr_desk_history_snapshot %>% 
  #mgr_hcterms_temp <- manager_desk_ids %>% 
    left_join(headcount_monthly) %>% 
    left_join(termcount_monthly) %>% 
    mutate(month = trendmonth)  
  
  mgr_hcterms <- bind_rows(mgr_hcterms, mgr_hcterms_temp)
  
}

# Does not appear bad managers have higher turnover
mgr_hcterms %>%
  left_join(bad_employee_table) %>% 
  group_by(bad_employee_flag) %>% 
  summarize(headcount = sum(headcount, na.rm = T),
            termcount = sum(termcount, na.rm = T)) %>% 
  mutate(termrate = termcount/headcount)



# Phone collisions --------------------------------------------------------

contact_table %>% 
  count(contact, sort = T)


# Education ---------------------------------------------------------------

education_table %>% 
  filter(str_detect(school_name, "Santa Clara")) %>% 
  #left_join(salaryhistory_table) %>% 
  left_join(deskhistory_table) %>% 
  left_join(deskjob_table)


# Table repair ------------------------------------------------------------

desk_id_end_date_repair <- deskhistory_table %>% 
  group_by(desk_id) %>% 
  summarize(desk_id_end_date_max = max(desk_id_end_date)) %>% 
  filter(desk_id_end_date_max < as.Date("2999-01-01"))


#hopefully 0 rows
employee_repair <- deskhistory_table %>% 
  group_by(employee_num) %>% 
  mutate(desk_id_end_date_max = max(desk_id_end_date)) %>% 
  filter(desk_id_end_date_max < as.Date("2999-01-01"),
         desk_id_end_date_max == desk_id_end_date,
         termination_flag == 0)




# Active employees with bad end date - should be 0
aa <- deskhistory_table %>% 
  filter(desk_id_end_date >= as.Date("2019-01-02")) %>% 
  count(desk_id) %>% 
  arrange(desc(n)) %>% 
  filter(n>1) %>% 
  left_join(deskhistory_table)

# Show duplicates that need to be fixed
deskhistory_table %>% 
  filter(desk_id == 252) 

# # Sample remove row if needed
# library(RMariaDB)
# HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
# dbGetQuery(HRSAMPLE, "select count(*) from deskhistory")
# dbExecute(HRSAMPLE, "delete from deskhistory where desk_id = 252 and desk_id_start_date = '2999-01-02'")
# dbGetQuery(HRSAMPLE, "select count(*) from deskhistory")
