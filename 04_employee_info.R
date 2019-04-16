library(RMariaDB)
library(tidyverse)
library(babynames)
library(readxl)
source("01_functions.R")
source("02_variables.R")

HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbListTables(HRSAMPLE)

# Build employeeinfo table ------------------------------------------------
# dbExecute(HRSAMPLE, "DROP TABLE employeeinfo")
# dbExecute(HRSAMPLE, "DELETE from  employeeinfo")
dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS = 0;") 
dbExecute(HRSAMPLE, "DROP TABLE IF EXISTS employeeinfo;") 
dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS=1;")
dbExecute(HRSAMPLE, "CREATE TABLE employeeinfo (
                    employee_num INT (11) AUTO_INCREMENT PRIMARY KEY,
                    first_name VARCHAR (255),
                    last_name VARCHAR (255),
                    city VARCHAR (255),
                    state  VARCHAR (255),
                    bad_employee_flag INT (1))
                    ;")


# Create first names ------------------------------------------------------

firstnames_f <- babynames %>% 
  filter(sex == "F", n > 100) %>% 
  select(name) %>% 
  sample_n(round(female_male_ratio * first_name_sample_size, 0))

firstnames_m <- babynames %>% 
  filter(sex == "M", n > 100) %>% 
  select(name) %>% 
  sample_n(round((1 - female_male_ratio) * first_name_sample_size, 0))

firstnames <- firstnames_f %>% 
  bind_rows(firstnames_m) %>% 
  rename(first_name = name)

# Import surnames - source: https://github.com/smashew/NameDatabases/tree/master/NamesDatabases/surnames/us.txt
surnames <- read_csv("data/surnames.csv", col_names = "last_name" )

# Create list of cities and states - source: https://raw.githubusercontent.com/grammakov/USA-cities-and-states/master/us_cities_states_counties.csv
cities <- read_delim("data/cities.csv", delim = "|")

# State/County Info -------------------------------------------------------
#### Note: not currently used - placeholder
#https://www.ers.usda.gov/webdocs/DataFiles/48747/PopulationEstimates.xls?v=4934.5
# state_county_population <- read_excel("data/PopulationEstimates.xls", skip = 2) %>% 
#   mutate(Area_Name = trimws(toupper(gsub(pattern = "county|borough|census area", "", Area_Name, ignore.case = TRUE)))) %>% 
#  # select(Area_Name, State, POP_ESTIMATE_2017) %>% 
#   rename(COUNTY_POP_ESTIMATE_2017 = POP_ESTIMATE_2017) %>% 
#   mutate(sample_weight = COUNTY_POP_ESTIMATE_2017 / sum(COUNTY_POP_ESTIMATE_2017, na.rm = TRUE)) %>% 
#   filter(!is.na(sample_weight) & !is.na(`Rural-urban_Continuum Code_2003`))#Area_Name != "UNITED STATES")
# 
# 
# #filter for rural continuum is not na
# aa <- state_county_population %>% 
#   select(Area_Name, sample_weight) %>% 
#   distinct() %>% 
#   sample_n(size = 1000,  weight = sample_weight, replace = TRUE) %>% 
#   count(Area_Name) %>% 
#   arrange(desc(n)) 
#   
# 
# 
# cities_w_county_population <- cities %>% 
#   select(City,
#          State = `State short`,
#          Area_Name = County) %>% 
#   distinct() %>% 
#   left_join(state_county_population) %>% 
#   rename(COUNTY_POP_ESTIMATE_2017 = POP_ESTIMATE_2017) %>% 
#   mutate(sample_weight = COUNTY_POP_ESTIMATE_2017 / sum(COUNTY_POP_ESTIMATE_2017, na.rm = TRUE)) %>% 
#   filter(!is.na(sample_weight))
# 


# Create data for employeeinfo table --------------------------------------
employee_info <- tibble()

for (i in (1:number_of_employees)) {
  city_row <- sample(length(cities$City),1)
  
  employee_info_add <- tibble(
    first_name = sample(firstnames$first_name, 1),
    last_name = sample(surnames$last_name, 1),
    city = cities$City[city_row],
    state = cities$`State short`[city_row],
    bad_employee_flag = sample(c(1,0), 1, bad_employee_ratio, replace = TRUE)
  )
  
  employee_info <- bind_rows(employee_info, employee_info_add)
}


# Populate table
employeeinfo_sql <- paste(
  "INSERT INTO employeeinfo (first_name, last_name, city, state, bad_employee_flag) VALUES ",
  paste0(
    "('",
    employee_info$first_name, "','",
    employee_info$last_name, "','",
    employee_info$city, "','",
    employee_info$state, "','",
    employee_info$bad_employee_flag, "')",
    collapse = ", "),
  ";"
)

dbExecute(HRSAMPLE, employeeinfo_sql)



# View table
df <- dbGetQuery(HRSAMPLE, "SELECT *  FROM employeeinfo")
df %>% count(state) 
df %>% count(bad_employee_flag) 
