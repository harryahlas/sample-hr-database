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

# Put your data files into the data-raw folder
save(employeeinfo_table,       file = "C:\\Development\\github\\hrsample\\data-raw/employeeinfo_table.rda")
save(deskhistory_table,        file = "C:\\Development\\github\\hrsample\\data-raw/deskhistory_table.rda")
save(deskjob_table,            file = "C:\\Development\\github\\hrsample\\data-raw/deskjob_table.rda")
save(hierarchy_table,          file = "C:\\Development\\github\\hrsample\\data-raw/hierarchy_table.rda")
save(performancereview_table,  file = "C:\\Development\\github\\hrsample\\data-raw/performancereview_table.rda")
save(salaryhistory_table,      file = "C:\\Development\\github\\hrsample\\data-raw/salaryhistory_table.rda")


# update Process.R
load("data-raw/employeeinfo_table.rda")
load("data-raw/deskhistory_table.rda")
load("data-raw/deskjob_table.rda")
load("data-raw/hierarchy_table.rda")
load("data-raw/performancereview_table.rda")
load("data-raw/salaryhistory_table.rda")

devtools::use_data(employeeinfo_table,
                   deskhistory_table,
                   deskjob_table,
                   hierarchy_table,
                   performancereview_table,
                   salaryhistory_table,
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


# Make sure Build tab is there and Build...ROxygen is checked, configured with build etc

# make sure package is public





##############################old below

# New R project
# Version Control
# Git
# Build tab -> first item, configure it, then RStudio will restart







require(devtools)

# Example DESCRIPTION file:
writeLines(
  'Package: hrsample
  Title: What The Package Does (one line, title case required)
  Version: 0.1
  Authors@R: person("First", "Last", email = "first.last@example.com",
  role = c("aut", "cre"))
  Description: What the package does (one paragraph)
  Depends: R (>= 3.1.0)
  License: What license is it under?
  LazyData: true',
  "DESCRIPTION")


# Set up the data-raw directory
devtools::use_data_raw()

#dir.create("data-raw")
dir.create("R")

# Create a data processing script in the data-raw directory.
# You can use any name you want.
file.create("data-raw/process.R")

# This script in the R directory will contain the documentation.
# You can use any name you want.
file.create("R/data.R")


# Create data
library(RMariaDB)
library(tidyverse)

# Connect to database stored on localhost
HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbListTables(HRSAMPLE)

# Retrieve tables
employeeinfo_table <- dbGetQuery(HRSAMPLE, "SELECT *  FROM employeeinfo")
deskhistory_table  <- dbGetQuery(HRSAMPLE, "SELECT *  FROM deskhistory")

# Put your data files into the data-raw folder
save(employeeinfo_table, file = "data-raw/employeeinfo_table.rda")
save(deskhistory_table,  file = "data-raw/deskhistory_table.rda")

# update Process.R
writeLines('
           load("data-raw/employeeinfo_table.rda")
           load("data-raw/deskhistory_table.rda")
           #xxx
           devtools::use_data(employeeinfo_table,
           deskhistory_table,
           overwrite = T)',
           "data-raw/Process.R")

source("data-raw/Process.R")
# then Run Process.R

# Edit your DESCRIPTION file as specified in Hadley Wickham’s book on R packages:
# http://r-pkgs.had.co.nz/description.html.
# You won’t need Imports or Suggests.
file.create("R/data.R")

# Make sure Build tab is there and Build...ROxygen is checked, configured with build etc
