library(tidyverse)
library(hrsample)
library(lubridate)

# No rehires --------------------------------------------------------------
deskhistory_table %>% 
  mutate(endyear = year(desk_id_end_date),
         termyear = ifelse(termination_flag == 1, endyear, NA)) %>% 
  group_by(employee_num) %>% 
  mutate(mintermyear = min(termyear, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(employee_num) %>% 
  filter(endyear != termyear) # 0 rows means no rehires


# Check that TMs that left company and came back did not get a review during that period ----
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
       subtitle = "Should be higher for 1 or 2 scores")

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
hierarchy_with_depth.sql <- read_file("scripts/hierarchy_with_depth.sql")
hierarchy_with_depth <- dbGetQuery(HRSAMPLE, hierarchy_with_depth.sql)

# a. No movement between levels
deskhistory_table %>% 
  select(employee_num, desk_id) %>%
  distinct() %>% 
  left_join(hierarchy_with_depth) %>% 
  select(employee_num, depth) %>% 
  distinct() %>% 
  count(employee_num) %>% 
  rename(count_of_different_levels = n) %>% 
  arrange(desc(count_of_different_levels)) 

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

# 5. Only 692 TMs right now
deskhistory_table %>% 
  filter(desk_id_end_date >= as.Date("2019-01-01")) %>% 
  nrow()

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

# 8. Termination rate is low
deskhistory_table %>% 
  filter(desk_id_start_date <= as.Date("2018-01-01"),
         desk_id_end_date >= as.Date("2017-01-01")) %>% 
  mutate(termination = if_else(termination_flag == 1 & (year(desk_id_end_date) == 2017 ), "term2017", "notterm2017")) %>% 
  count(termination) %>% 
  spread(key = termination, value = n) %>% 
  mutate(pct = term2017 / (term2017 + notterm2017))


#terminations by year
terms_by_year <- deskhistory_table %>% filter(termination_flag == 1) %>% count(year(desk_id_end_date))


# Headcount by Year -------------------------------------------------------
hcyear <- seq(1999,2019,1)

get_end_of_year_headcount <- function (hcyear = 1999) {
  x <- deskhistory_table %>% 
    filter(desk_id_end_date >= as.Date(paste0(hcyear, "-12-31")),
           desk_id_start_date <= as.Date(paste0(hcyear, "-12-31"))) %>% 
    nrow()
  return(x)
}

tibble(hcyear = hcyear) %>% 
  rowwise() %>% 
  mutate(end_of_year_hc = get_end_of_year_headcount(hcyear)) %>% 
  ggplot(aes(hcyear, end_of_year_hc)) +
  geom_col()

# Time between jobs -------------------------------------------------------
# Next: maybe add tenure by line of business
bob <- deskhistory_table %>% 
  filter(termination_flag == 1, desk_id_end_date < as.Date("2019-01-01")) %>% 
  select(employee_num) %>%
  left_join(deskhistory_table) %>% 
  group_by(employee_num) %>% 
  mutate(end_date = max(desk_id_end_date),
         start_date = min(desk_id_start_date)) %>% 
  ungroup() %>% 
  mutate(tenure = as.numeric(end_date - start_date)) %>% 
  select(employee_num, tenure) %>%
  distinct() %>% 
  left_join(employeeinfo_table %>% 
               select(employee_num, bad_employee_flag))
#%>% 
  ggplot(aes(as.factor(bad_employee_flag), tenure)) +
  geom_boxplot()

# Table repair ------------------------------------------------------------

# Active employees
deskhistory_table %>% 
  filter(desk_id_end_date >= as.Date("2019-01-02")) %>% 
  count(desk_id) %>% 
  arrange(desc(n))

# Show duplicates that need to be fixed
deskhistory_table %>% 
  filter(desk_id == 252) 

# Sample remove row if needed
library(RMariaDB)
HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbGetQuery(HRSAMPLE, "select count(*) from deskhistory")
dbExecute(HRSAMPLE, "delete from deskhistory where desk_id = 252 and desk_id_start_date = '2999-01-02'")
dbGetQuery(HRSAMPLE, "select count(*) from deskhistory")
