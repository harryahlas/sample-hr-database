library(RMariaDB)
library(hrsample)
library(tidyverse)
library(scales)
library(lubridate)

default_color <- rgb(155/255, 186/255, 204/255)


# below borrowed from 07
# Connect to database -----------------------------------------------------
HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')

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


# Headcount ---------------------------------------------------------------
# Note this appears to be more desk count than headcount
employee_distribution <- deskhistory_table %>% 
  filter(desk_id_end_date == as.Date("2999-01-01")) %>% 
  right_join(hierarchy_spread_all %>% select(desk_id, lvl01_org)) %>% 
  filter(!is.na(lvl01_org)) %>% 
  count(lvl01_org) %>% 
  ggplot(aes(x = fct_reorder(lvl01_org, -n), y = n)) +
  geom_col(fill = default_color, width = .6) +
  labs(title = "Current Headcount by Line of Business") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(employee_distribution, filename =  "images/employee_distribution.png", device = "png")



# Merit Increases ---------------------------------------------------------


merit_increases <- salaryhistory_table %>%
  mutate(`Salary Increase Year` = lubridate::year(salary_effective_date)) %>%
  filter(salary_increase > 0,
         `Salary Increase Year` >= 2010,
         `Salary Increase Year` <= 2018) %>%
  ggplot(aes(x = salary_increase)) +
  geom_histogram(bins = 15, fill = default_color) +
  scale_x_continuous(labels = percent) +
  facet_wrap(~`Salary Increase Year`) +
  labs(x = "% Increase",
       y = "Count of Merit Increases",
       title = "Merit Increases by Year") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


ggsave(merit_increases, filename =  "images/merit_increases.png", device = "png")



#salary_distribution <- 
salary_distribution <- deskhistory_table %>% 
  filter(desk_id_end_date == as.Date("2999-01-01")) %>% 
  left_join(salaryhistory_table) %>% 
  group_by(employee_num, desk_id) %>% 
  summarise(current_salary = max(salary)) %>% arrange(desc(employee_num)) %>% 
  left_join(deskjob_table) %>% 
  ungroup() %>% 
  group_by(job_name) %>% 
  summarize(avg_salary = mean(current_salary, na.rm = TRUE),
            job_count = n()) %>% 
  filter(!grepl(pattern = "Leader|CEO", x = job_name )) %>% 
  ggplot(aes(x = fct_reorder(job_name, -avg_salary), y = avg_salary)) +
  geom_col(fill = default_color, width = .8) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Average Salary by Common Jobs") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(salary_distribution, filename =  "images/salary_distribution.png", device = "png")





# Job Distribution --------------------------------------------------------

job_distribution <- deskhistory_table %>% 
  filter(desk_id_end_date == as.Date("2999-01-01")) %>% 
  right_join(deskjob_table) %>%
  count(job_name) %>% 
  arrange(n) %>% 
  ggplot(aes(x = fct_reorder(job_name, n), y = n)) +
  geom_col(fill = default_color, width = .8) +
  labs(title = "Current Job Distribution") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1) 
  )+
  coord_flip()
 
 ggsave(job_distribution, filename =  "images/job_distribution.png", device = "png")
 
  

# Promotions --------------------------------------------------------------
#count of promotions by year
deskhistory_table %>% 
   left_join(hierarchy_spread_all) %>% 
   mutate(year = year(desk_id_start_date)) %>% 
   count(year, lvl01_org, promotion_flag) %>% 
   filter(promotion_flag == 1) %>% 
   ggplot(aes(x = year, y = n)) +
   geom_col(fill = default_color) +
   facet_wrap(~lvl01_org) +
   labs(y = "Count of Promotions",
        title = "Promotions by Business Line") +
   theme_minimal() +
   theme(panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         axis.text.x      = element_blank()) 
 
 