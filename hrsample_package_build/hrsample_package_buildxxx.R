# Source: https://grasshoppermouse.github.io/2017/10/18/put-your-data-in-an-r-package/

# Run below in console and open project
devtools::create("C:\\Development\\github\\hrsample")

# Set up the data-raw directory
devtools::use_data_raw()

# Create a data processing script in the data-raw directory.
file.create("data-raw/process.R")

# This script in the R directory will contain the documentation.
file.create("R/data.R")

# Initialize git repository (optional)
devtools::use_git()

# Source data created from https://github.com/harryahlas
library(RMariaDB)
library(tidyverse)

# Connect to database stored on localhost
HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbListTables(HRSAMPLE)

# Retrieve tables
employeeinfo_table      <- dbGetQuery(HRSAMPLE, "SELECT *  FROM employeeinfo")
deskhistory_table       <- dbGetQuery(HRSAMPLE, "SELECT *  FROM deskhistory")
deskjob_table           <- dbGetQuery(HRSAMPLE, "SELECT *  FROM deskjob")
hierarchy_table         <- dbGetQuery(HRSAMPLE, "SELECT *  FROM hierarchy")
performancereview_table <- dbGetQuery(HRSAMPLE, "SELECT *  FROM performancereview")
salaryhistory_table     <- dbGetQuery(HRSAMPLE, "SELECT *  FROM salaryhistory")
recruiting_table        <- dbGetQuery(HRSAMPLE, "SELECT *  FROM recruiting")
rollup_view             <- dbGetQuery(HRSAMPLE, "SELECT *  FROM rollup")
contact_table           <- dbGetQuery(HRSAMPLE, "SELECT *  FROM contact")
education_table         <- dbGetQuery(HRSAMPLE, "SELECT *  FROM education")
skills_table            <- dbGetQuery(HRSAMPLE, "SELECT *  FROM skills")

# Put your data files into the data-raw folder
save(employeeinfo_table,       file = "data-raw/employeeinfo_table.rda")
save(deskhistory_table,        file = "data-raw/deskhistory_table.rda")
save(deskjob_table,            file = "data-raw/deskjob_table.rda")
save(hierarchy_table,          file = "data-raw/hierarchy_table.rda")
save(performancereview_table,  file = "data-raw/performancereview_table.rda")
save(salaryhistory_table,      file = "data-raw/salaryhistory_table.rda")
save(recruiting_table,         file = "data-raw/recruiting_table.rda")
save(contact_table,            file = "data-raw/contact_table.rda")
save(education_table,          file = "data-raw/education_table.rda")
save(skills_table,             file = "data-raw/skills_table.rda")

# update Process.R
load("data-raw/employeeinfo_table.rda")
load("data-raw/deskhistory_table.rda")
load("data-raw/deskjob_table.rda")
load("data-raw/hierarchy_table.rda")
load("data-raw/performancereview_table.rda")
load("data-raw/salaryhistory_table.rda")
load("data-raw/recruiting_table.rda")
load("data-raw/rollup_view.rda")
load("data-raw/contact_table.rda")
load("data-raw/education_table.rda")
load("data-raw/skills_table.rda")

devtools::use_data(employeeinfo_table,
                   deskhistory_table,
                   deskjob_table,
                   hierarchy_table,
                   performancereview_table,
                   salaryhistory_table,
                   recruiting_table,
                   rollup_view,
                   contact_table,
                   education_table,
                   skills_table,
                   overwrite = T)


# Update description file


# Edit your DESCRIPTION file as specified in Hadley Wickham’s book on R packages:
# http://r-pkgs.had.co.nz/description.html.
# You won’t need Imports or Suggests.

#install.packages("sinew")
sinew::makeOxygen(employeeinfo_table, add_fields = "source" ) %>% write(file="R/data.r",append=TRUE)
sinew::makeOxygen(deskhistory_table, add_fields = "source" ) %>% write(file="R/data.r",append=TRUE)
sinew::makeOxygen(deskjob_table, add_fields = "source" ) %>% write(file="R/data.r",append=TRUE)
sinew::makeOxygen(hierarchy_table, add_fields = "source" ) %>% write(file="R/data.r",append=TRUE)
sinew::makeOxygen(performancereview_table, add_fields = "source" ) %>% write(file="R/data.r",append=TRUE)
sinew::makeOxygen(salaryhistory_table, add_fields = "source" ) %>% write(file="R/data.r",append=TRUE)
sinew::makeOxygen(recruiting_table, add_fields = "source" ) %>% write(file="R/data.r",append=TRUE)
sinew::makeOxygen(rollup_view, add_fields = "source" ) %>% write(file="R/data.r",append=TRUE)
sinew::makeOxygen(contact_table, add_fields = "source" ) %>% write(file="R/data.r",append=TRUE)
sinew::makeOxygen(education_table, add_fields = "source" ) %>% write(file="R/data.r",append=TRUE)
sinew::makeOxygen(skills_table, add_fields = "source" ) %>% write(file="R/data.r",append=TRUE)


# Make sure Build tab is there and Build...ROxygen is checked, configured with build etc

# make sure package is public




