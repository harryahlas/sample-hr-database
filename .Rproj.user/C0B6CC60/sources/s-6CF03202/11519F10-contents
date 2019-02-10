library(RMariaDB)
library(tidyverse)

HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbListTables(HRSAMPLE)

# Build employeeinfo table ------------------------------------------------
# dbExecute(HRSAMPLE, "DROP TABLE employeeinfo")
dbExecute(HRSAMPLE, "CREATE TABLE employeeinfo (
                    employee_num INT (11) AUTO_INCREMENT PRIMARY KEY,
                    first_name VARCHAR (255),
                    last_name VARCHAR (255),
                    hire_date DATE,
                    city VARCHAR (255),
                    state  VARCHAR (255) )
                    ;")




# Import sample data for population ---------------------------------------

# Import surnames - source: https://github.com/smashew/NameDatabases/tree/master/NamesDatabases/surnames/us.txt
surnames <- read_csv("data/surnames.csv", col_names = "last_name" )

# Import first names - source: https://github.com/smashew/NameDatabases/tree/master/NamesDatabases/first%20names/us.txt
firstnames <- read_csv("data/firstnames.csv",col_names = "first_name")

# Create list of cities and states - source: https://raw.githubusercontent.com/grammakov/USA-cities-and-states/master/us_cities_states_counties.csv
cities <- read_delim("data/cities.csv", delim = "|")


# Function to generate random date between 2 dates
create_date <- function(start_date, end_date, number_of_dates = 1) {
  sample(seq(start_date, end_date, by="day"), number_of_dates)
}



# Create data for employeeinfo table --------------------------------------
employee_info <- tibble()

number_of_employees <- 5000
company_open_date <- as.Date('1993/07/02')
first_date_of_hierarchy <- as.Date('1999/01/01')

for (i in (1:number_of_employees)) {
  city_row <- sample(length(cities$City),1)
  
  employee_info_add <- tibble(
    first_name = sample(firstnames$first_name, 1),
    last_name = sample(surnames$last_name, 1),
    hire_date = create_date(company_open_date, first_date_of_hierarchy),
    city = cities$City[city_row],
    state = cities$`State short`[city_row]
  )
  
  employee_info <- bind_rows(employee_info, employee_info_add)
}


# Function to insert rows to employeeinfo
create_insert_employeeinfo <- function(first_name,
                                       last_name,
                                       hire_date,
                                       city,
                                       state,
                                       database = HRSAMPLE) {
  insert_employeeinfo_sql <- paste0(
    "INSERT INTO employeeinfo (first_name, last_name, hire_date, city, state) VALUES('",
    first_name, "','",
    last_name, "','",
    hire_date, "','",
    city, "','",
    state, "');")
  print(insert_employeeinfo_sql)
  dbExecute(database, insert_employeeinfo_sql)
} 

# Populate table 
for (i in (1:nrow(employee_info))) {
  create_insert_employeeinfo(employee_info$first_name[i],
                             employee_info$last_name[i],
                             employee_info$hire_date[i],
                             employee_info$city[i],
                             employee_info$state[i])
}


# View table
df <- dbGetQuery(HRSAMPLE, "SELECT *  FROM employeeinfo")
df %>% count(state)
