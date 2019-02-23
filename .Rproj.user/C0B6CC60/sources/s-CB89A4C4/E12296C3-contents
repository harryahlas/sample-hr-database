# First: set up MySQL Workbench here
# Second: set up tables
#https://programminghistorian.org/en/lessons/getting-started-with-mysql-using-r#downloading-and-installing-mysql

#install.packages("RMariaDB")
library(RMariaDB)
library(tidyverse)


# Create business lines ---------------------------------------------------
lob <- read_csv("data/lob.csv")

sum(lob$proportion)# should equal 1


# Alternate database ------------------------------------------------------

#http://www.sqltutorial.org/sql-sample-database/

HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbListTables(HRSAMPLE)

dbExecute(HRSAMPLE, "CREATE TABLE regions (
          region_id INT (11) AUTO_INCREMENT PRIMARY KEY,
          region_name VARCHAR (25) DEFAULT NULL
);")

#add row to table
dbWriteTable(HRSAMPLE, "regions", data.frame(region_name = 'el salvador'), append = TRUE)

rows_to_add <- tibble(region_name = c('usa','norway'))
dbWriteTable(HRSAMPLE, "regions", rows_to_add, append = TRUE)

hrsample_database_setup.sql <- read_file("scripts/hrsample_database_setup.sql")
dbExecute(HRSAMPLE, hrsample_database_setup.sql)

dbExecute(HRSAMPLE, "INSERT INTO regions VALUES(3,'greece')")

dbSendStatement(HRSAMPLE, hrsample_database_setup.sql)

dbDisconnect(HRSAMPLE)

df <- dbGetQuery(HRSAMPLE, "SELECT * FROM employees")





# Create desk_ids and hierarchy -------------------------------------------
# http://www.mysqltutorial.org/mysql-adjacency-list-tree/

#dbExecute(HRSAMPLE, "DROP TABLE HIERARCHY")

HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')
dbListTables(HRSAMPLE)

# Make desk_id/hierarchy table
dbExecute(HRSAMPLE, "CREATE TABLE hierarchy (
          desk_id int(10) unsigned NOT NULL AUTO_INCREMENT,
          org varchar(255) NOT NULL,
          parent_id int(10) unsigned DEFAULT NULL,
          PRIMARY KEY (desk_id),
          FOREIGN KEY (parent_id) REFERENCES hierarchy (desk_id) 
          ON DELETE CASCADE ON UPDATE CASCADE
);")

# Add CEO (root node, no parent)
dbExecute(HRSAMPLE, "INSERT INTO hierarchy (org, parent_id) VALUES('CEO',NULL);")

# Function to insert rows to hierarchy
create_insert_hierarchy <- function(org_name,
                                    parent_id,
                                    database = HRSAMPLE) {
  
  insert_hierarchy_sql <- paste0(
    "INSERT INTO hierarchy (org, parent_id) VALUES('",
    org_name, "',", parent_id,");")
  
  dbExecute(database, insert_hierarchy_sql)
}

# Add LOBs ----------------------------------------------------------------

for (i in (1:length(lob$lob))) {create_insert_hierarchy(lob$lob[i], 1)}

# Function to get desk_id of an orgname. Needs to be vectorized to work in dplyr chain
get_org_desk_id <- function(orgname) {
  desk_id <- dbGetQuery(HRSAMPLE,
                        paste0("SELECT desk_id FROM hierarchy WHERE org = '",
                               orgname,
                               "'", collapse = "")) %>%
    as.character()
  return(desk_id)
} 

# Vectorize 
get_org_desk_id <- Vectorize(get_org_desk_id)

# Add Departments ---------------------------------------------------------

# Get list of departments and parents
depts <- read_csv("data/departments.csv") %>% 
  mutate(dept = paste0(gsub("[aeiou]","",parent_name), " - ", dept))

# Get parent_id from database
depts <- depts %>% 
  mutate(parent_id = get_org_desk_id(parent_name)) 

# Add departmants
for (i in (1:length(depts$dept))) {
  create_insert_hierarchy(depts$dept[i], depts$parent_id[i])
}


# Add Regions -------------------------------------------------------------

# Add N, E, S, W sales regions
# Get list of regions and parents
sales_nesw_regions <- read_csv("data/sales_nesw_regions.csv") %>% 
  mutate(region = paste0(gsub("[aeiou]|","",parent_name), " - ", region))

# Get parent_id from database
sales_nesw_regions <- sales_nesw_regions %>% 
  mutate(parent_id = get_org_desk_id(parent_name)) 

# Add regions
for (i in (1:length(sales_nesw_regions$region))) {
  create_insert_hierarchy(sales_nesw_regions$region[i], sales_nesw_regions$parent_id[i])
}



# Additional Regions ------------------------------------------------------

# Get list of orgs to exclude from additional regions
op <- options("useFancyQuotes")
options(useFancyQuotes = FALSE)
orgs_to_exclude_from_region_add <- paste0(sQuote(c(lob$lob, 
                                                   unique(sales_nesw_regions$parent_name),
                                                   sales_nesw_regions$region)),
                                          collapse = ", ")
options(op)

orgs_to_include_on_region_add <- dbGetQuery(HRSAMPLE, 
                                            gsub("%ID_LIST%",
                                                 orgs_to_exclude_from_region_add,
                                                 "SELECT * FROM hierarchy WHERE org NOT IN (%ID_LIST%)")) %>% 
  filter(!is.na(parent_id))

# Add standard regions for each department. Shorten parent section in name
for (i in (1:length(orgs_to_include_on_region_add$org))) {
  org_name_short <- gsub("[aeiou]", "", orgs_to_include_on_region_add$org[i])
  create_insert_hierarchy(paste0(org_name_short, " - PSI"),
                          orgs_to_include_on_region_add$desk_id[i])
  create_insert_hierarchy(paste0(org_name_short, " - RAA"),
                          orgs_to_include_on_region_add$desk_id[i])
  create_insert_hierarchy(paste0(org_name_short, " - Project Services"),
                          orgs_to_include_on_region_add$desk_id[i])
}



# Add desk_ids to bottom nodes --------------------------------------------

# Create hierarchy and join to lob for report to count
hierarchy <- dbGetQuery(HRSAMPLE, "SELECT *  FROM hierarchy")

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
  left_join(lob, by = c("lvl01_org" = "lob")) %>% 
  select(-proportion) %>% 
  rowwise() %>% 
  mutate(
    head_count = max(round(
      rnorm(n = 1,
            mean = avg_report_to_count,
            sd =  4)
      ,0),2))


# Add level4 desk_ids -----------------------------------------------------
# For every row in hierarchy_spread
# Loop head_count times
# Insert row using "Individual Contributor" and lvl03_org as parent_id
k = 0
for (i in (1:length(hierarchy_spread$lvl03_org))) {
  for (j in (1:hierarchy_spread$head_count[i])) {
    ic_desk_name <- paste0(hierarchy_spread$lvl03_org[i]," - IC",sprintf("%02d", j) )
    create_insert_hierarchy(ic_desk_name, hierarchy_spread$lvl03_desk_id[i])
    print("running...")
    k = k + 1
  }
}
print(paste(k, "individual contributor desk_ids added"))


# sample get parent_id
get_org_desk_id("Finance")




# View table
dbGetQuery(HRSAMPLE, "SELECT *  FROM hierarchy")

# Query to get hierarchy levels of all desk_ids ---------------------------

hierarchy_with_depth.sql <- read_file("scripts/hierarchy_with_depth.sql")
hierarchy_with_depth <- dbGetQuery(HRSAMPLE, hierarchy_with_depth.sql)


