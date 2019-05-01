library(RMariaDB)
library(tidyverse)
library(lubridate)
library(readODS)
source("01_functions.R")
source("02_variables.R")

HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbListTables(HRSAMPLE)

# Not used yet
#salaryhistory_table <- dbGetQuery(HRSAMPLE, "SELECT * FROM salaryhistory")
employeeinfo_table <- dbGetQuery(HRSAMPLE, "SELECT * FROM employeeinfo")
deskhistory_table <- dbGetQuery(HRSAMPLE, "SELECT * FROM deskhistory")
deskjob_table <- dbGetQuery(HRSAMPLE, "SELECT * FROM deskjob")

# Build education table ---------------------------------------------------

dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS = 0;") 
dbExecute(HRSAMPLE, "DROP TABLE IF EXISTS education;") 
dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS=1;")
dbExecute(HRSAMPLE, "CREATE TABLE education (
          employee_num INT (11),
          degree VARCHAR (5),
          school_name VARCHAR (255),
          FOREIGN KEY (employee_num) REFERENCES employeeinfo (employee_num)  ON DELETE CASCADE ON UPDATE CASCADE
);")

# Build skills table ------------------------------------------------------

dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS = 0;") 
dbExecute(HRSAMPLE, "DROP TABLE IF EXISTS skills;") 
dbExecute(HRSAMPLE, "SET FOREIGN_KEY_CHECKS=1;")
dbExecute(HRSAMPLE, "CREATE TABLE skills (
          employee_num INT (11),
          skill_name VARCHAR (255),
          skill_type VARCHAR (255),
          FOREIGN KEY (employee_num) REFERENCES employeeinfo (employee_num)  ON DELETE CASCADE ON UPDATE CASCADE
);")

# Import data -------------------------------------------------------------
#https://www.4icu.org/us/
colleges <- read_delim("data/colleges.txt", delim = "|")
skills <- read_ods("data/skills_worksheet.ods") %>% 
  gather(key = job_name, value = skill, -c(skill_name, skill_type)) %>% 
  select(job_name, everything())

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



# Skills ------------------------------------------------------------------
# Create potential_skills that has list of employees and all the jobs they have had
# Use create_skills() to create a randomly generated number of skills for each employee
jobs_to_exclude_from_skills <- c("CEO")

potential_skills <- deskhistory_table %>% 
  left_join(deskjob_table) %>% 
  select(employee_num, job_name) %>% 
  filter(!job_name %in% jobs_to_exclude_from_skills) %>% 
  distinct()

employee_list_for_skills <- potential_skills %>% 
  select(employee_num) %>% 
  distinct()

create_skills <- function(f_employee_num) {
  skill_count <- max(round(rgamma(1,3) ,0) -2,0)
  
  skills_temp <- potential_skills %>% 
    filter(employee_num == f_employee_num) %>% 
    left_join(skills, by = "job_name") %>% 
    filter(!is.na(skill)) %>% 
    select(-job_name, - skill)
  
  skills_temp_nrow <- nrow(skills_temp)
  
  skills_temp <- sample_n(skills_temp, min(skill_count, skills_temp_nrow))
  
  return(skills_temp)
}

skills_table <- tibble()

for (i in (1:length(employee_list_for_skills$employee_num))) {
  skills_append <- create_skills(employee_list_for_skills$employee_num[i])
  skills_table <- bind_rows(skills_table, skills_append)
}



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

# Populate skills
skills_sql <- paste(
  "INSERT INTO skills (employee_num, skill_type, skill_name) VALUES ",
  paste0(
    "('",
    skills_table$employee_num, "','",
    skills_table$skill_type, "','",
    skills_table$skill_name, "')",
    collapse = ", "),
  ";"
)

dbExecute(HRSAMPLE, skills_sql)


# Validate ----------------------------------------------------------------

df <- dbGetQuery(HRSAMPLE, "SELECT * FROM education")
skimr::skim(df)
df %>% count(school_name, sort = TRUE)
