#devtools::install_github("harryahlas/hrsample")
library(hrsample)
library(tidyverse)
library(scales)
library(lubridate)
library(openxlsx)

# Tracking information
as_of_date <- Sys.Date()
report_name <- "PA73405 - Attrition by Job 2009"

glimpse(rollup_view, width = 70)

rollup_view %>% count(lvl01_org) %>% arrange(desc(n))

deskhistory_table_hierarchy <- deskhistory_table %>% 
  left_join(rollup_view %>% select(lvl01_desk_id,
                                   lvl01_org,
                                   lvl04_desk_id) %>% distinct(), 
            by = c("desk_id" = "lvl04_desk_id"))

glimpse(deskhistory_table_hierarchy[sample(1:nrow(deskhistory_table_hierarchy)),], width = 70)

LOB_list <- rollup_view %>% 
  select(lvl01_org, lvl01_desk_id) %>% 
  distinct() %>% 
  filter(lvl01_org != "CEO")

LOB_list


for (i in (1:length(LOB_list$lvl01_org))) {
  
  org_name <- LOB_list$lvl01_org[i]
  
  hcto_summary <- deskhistory_table_hierarchy %>% 
    filter(lvl01_org == LOB_list$lvl01_org[i]) %>% 
    left_join(deskjob_table) %>% 
    filter(desk_id_start_date <= as.Date("2009-12-31"),
           desk_id_end_date >= as.Date("2009-01-01")) %>% 
    arrange(desc(desk_id_end_date)) %>% 
    group_by(employee_num) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    mutate(year = "2009",
           termination_flag = if_else(termination_flag == 1 & year(desk_id_end_date) == 2009, "Terminated", "DidNotTerminate")) %>% 
    count(year, job_name, termination_flag) %>% 
    spread(termination_flag, n, fill = 0) %>% 
    mutate(Headcount =  Terminated + DidNotTerminate,
           TerminationRate = percent(Terminated / Headcount)) %>% 
    arrange(desc(Terminated / Headcount))
  
  disclaimer_info <-   data.frame(Information = 
                                    c("Source: hrsample database",
                                      paste("Data as of", as_of_date, "."),
                                      paste("Data includes all employees in", org_name ,"who were active at any point from Jan 1, 2009 through December 31, 2009."), # Added for this example
                                      "If the employee had multiple jobs during 2009, only the most recent job is counted.",
                                      "Data is confidential and should be shared on a need to know basis only.",
                                      "Do not distribute externally."))
  
  wb <- createWorkbook()
  addWorksheet(wb, report_name)
  addWorksheet(wb, "Data Disclaimer")
  writeDataTable(wb, 1, hcto_summary)
  writeDataTable(wb, 2, disclaimer_info)
  addStyle(wb, 2, style = createStyle(wrapText = TRUE), rows = 1:7, cols = 1)
  setColWidths(wb, 2, 1, widths = 50)
  saveWorkbook(wb, paste0("output/", report_name, " - ", org_name, " - ", as_of_date, ".xlsx"), TRUE) # New
}
