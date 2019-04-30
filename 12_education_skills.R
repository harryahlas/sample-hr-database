library(RMariaDB)
library(tidyverse)
library(lubridate)
source("01_functions.R")
source("02_variables.R")

HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbListTables(HRSAMPLE)

# Not used yet
#salaryhistory_table <- dbGetQuery(HRSAMPLE, "SELECT * FROM salaryhistory")
employeeinfo_table <- dbGetQuery(HRSAMPLE, "SELECT * FROM employeeinfo")

# Build contact table -----------------------------------------------------

dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS = 0;") 
dbExecute(HRSAMPLE, "DROP TABLE IF EXISTS education;") 
dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS=1;")
dbExecute(HRSAMPLE, "CREATE TABLE education (
          employee_num INT (11),
          degree VARCHAR (5),
          school_name VARCHAR (255),
          FOREIGN KEY (employee_num) REFERENCES employeeinfo (employee_num)  ON DELETE CASCADE ON UPDATE CASCADE
);")


# Import data -------------------------------------------------------------
#https://www.4icu.org/us/
colleges <- read_delim("data/colleges.txt", delim = "|")


# Create education --------------------------------------------------------

create_college <- function() {
  college <- colleges %>% 
    select(School) %>% 
    sample_n(1) %>% 
    as.character()
  return(college)
}

# For each employee, use ratios in variables to determine their education levels
# Can only have PhD if they have MA/MS. 
# Can only have MA/MS if they have a BA/BS

education_table_BABS <- employeeinfo_table %>% 
  select(employee_num) %>% 
  sample_frac(size = BA_pct + BS_pct) %>% 
  rowwise() %>% 
  mutate(degree = sample(x = c("BA", "BS"), size =  1, prob = c(BA_pct, BS_pct)),
         school_name = create_college())

education_table_MAMS <- education_table_BABS %>% 
  select(employee_num) %>% 
  sample_frac(size = MA_pct + MS_pct) %>% 
  rowwise() %>% 
  mutate(degree = sample(x = c("MA", "MS"), size =  1, prob = c(MA_pct, MS_pct)),
         school_name = create_college())

education_table_PhD <- education_table_MAMS %>% 
  select(employee_num) %>% 
  sample_frac(size = PhD_pct) %>% 
  rowwise() %>% 
  mutate(degree = "PhD",
         school_name = create_college())

education_table <- bind_rows(
  education_table_BABS,
  education_table_MAMS,
  education_table_PhD
)


# Remove bad characters ---------------------------------------------------

education_table <- education_table %>% 
  mutate(school_name = iconv(school_name, "UTF-8", "UTF-8",sub='')) %>% 
  mutate(school_name = gsub(pattern = "'",replacement = "", school_name))

# Upload data -------------------------------------------------------------

# Populate education
education_sql <- paste(
  "INSERT INTO education (employee_num, degree, school_name) VALUES ",
  paste0(
    "('",
    education_table$employee_num, "','",
    education_table$degree, "','",
    education_table$school_name, "')",
    collapse = ", "),
  ";"
)

dbExecute(HRSAMPLE, education_sql)


# Validate ----------------------------------------------------------------

df <- dbGetQuery(HRSAMPLE, "SELECT * FROM education")
skimr::skim(df)
df %>% count(school_name, sort = TRUE)
