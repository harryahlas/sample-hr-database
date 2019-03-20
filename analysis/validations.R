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


# 4. Check turnover, movement, and promotions for levels 1-3

# 5. check job by states

# 6. Check promotions - see if people are getting promotions after a 1 or 2 review
