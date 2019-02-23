library(RMariaDB)
library(tidyverse)
library(lubridate)

HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')


# Build performancereview table -------------------------------------------
dbExecute(HRSAMPLE, "CREATE TABLE performancereview (
          employee_num INT (11),
          year INT (4),
          perf_review INT (1),
          FOREIGN KEY (employee_num) REFERENCES employeeinfo (employee_num)  ON DELETE CASCADE ON UPDATE CASCADE
);")


# Minimum review value
min_review_value <- 1

#need to create this as import, used in 06
# First day of hierarchy, same as max(employeeinfo_table$hire_date)
hierarchy_start_date <- as.Date("1999/01/01")

#need to create this as import, used in 06
# Most recent date that a new job could be had
max_date <- as.Date("2019/01/01")

# Import deskhistory
#NOTE:::!!!update this to database
deskhistory_table <- read_csv("data/deskhistory_table.csv")

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
  left_join(hierarchy_table_with_state, by = c("lvl01_desk_id" = "parent_id")) %>% 
  rename(lvl02_desk_id = desk_id,
         lvl02_org = org) %>% 
  left_join(hierarchy_table_with_state, by = c("lvl02_desk_id" = "parent_id")) %>% 
  rename(lvl03_desk_id = desk_id,
         lvl03_org = org) %>% 
  left_join(hierarchy, by = c("lvl03_desk_id" = "parent_id"))  

# New - add level 1-3 employees since hierarchy_spread only counts level 04
hierarchy_spread_lvl01 <- hierarchy_spread %>% 
  select(lvl00_desk_id, lvl00_org, desk_id = lvl01_desk_id) %>% 
  distinct()

hierarchy_spread_lvl02 <- hierarchy_spread %>% 
  select(lvl01_desk_id, lvl01_org, desk_id = lvl02_desk_id) %>% 
  distinct()

hierarchy_spread_lvl03 <- hierarchy_spread %>% 
  select(lvl01_desk_id, lvl01_org, desk_id = lvl03_desk_id) %>% 
  distinct()

### Note this hierarchy_spread_all is different than from 04. Includes everyone except CEO
hierarchy_spread_all <- hierarchy_spread %>% 
  bind_rows(hierarchy_spread_lvl01, hierarchy_spread_lvl02, hierarchy_spread_lvl03)


# Create performance review table, employee_num, year, perf_review
# import perf_review_distributions
performance_review_ratios <- read_csv("data/performance_review_ratios.csv")

# Get list of all employees in deskhistory
employee_list <- deskhistory_table %>% 
  select(employee_num) %>% 
  distinct()


review_year_list <- tibble()

i = 100 #no promotion
i = 120 #promotion
i = 1
i = 80
#### start loop
for (i in 1:nrow(employee_list)) {
  # Pull employee's entire desk history
  deskhistory_table_temp <- deskhistory_table %>% 
    filter(employee_num == employee_list$employee_num[i])
  
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
  
  review_year_end <- case_when(company_end_month > 3 ~ company_end_year - 1, 
                               company_end_month <= 3 ~ max(company_end_year - 2, review_year_start),
                               TRUE ~ 0)
  
  if (review_year_start > review_year_end) next
  
  review_years <- seq(review_year_start, review_year_end, 1)
  review_year_list_append <- tibble(employee_numx = rep(employee_list$employee_num[i], length(review_years)),
                                                       review_year = review_years,
                                    review_date = as.Date(paste0(review_year + 1, "-03-31"))) %>% 
    fuzzy_left_join(deskhistory_table, by = c(
      "employee_numx" = "employee_num",
      "review_date" = "desk_id_start_date",
      "review_date" = "desk_id_end_date"
    ),
    match_fun = list(`==`, `>=`, `<=`)) %>% 
    select(-employee_numx) %>% 
    left_join(hierarchy_spread_all %>% select(desk_id, lvl01_org)) %>% 
    left_join(performance_review_ratios, by = c("lvl01_org" = "LOB"))
  
  ##################3next figure above
  
  # Get their current LOB for that date (april 1)
  # use that info above to assign ratios/default
  # if they have a promotion within 1 year after then double their chance for 4 or 5
  # .013 chance that they get no review at all no matter what
  sample(c(1,2,3,4,5), 100, prob=c(.01,.05,.64,.2,.1), replace=TRUE) 
  
  #make sure review is no less than minimum
  
  

  review_year_list <- bind_rows(review_year_list, review_year_list_append)

  print(i)
}


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


# Start in Feb 2000. if they were in the company from 10/1/1999-3/31/2000 then they are eligible for a review

review_year_list_append


# add laters:
# if they
# Try to leave plug for bad manager
#go back and somewhat randomly pick some bad lvl 4 managers.  They should have higher turnover rates. give their TMs a better chance of getting 1 or 2
#bad managers 

### Need to add a check that TMs that left company and came back did not get a review during that period

#update performancereview table

deskhistory_table %>% 
  group_by(employee_num) %>% 
  summarize(promotion_count = sum(promotion_flag)) %>% 
  count(promotion_count)
