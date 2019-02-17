library(tidyverse)
library(scales)
library(openxlsx)

# Tracking information
as_of_date <- Sys.Date()
report_name <- "PA73405 - Attrition by Job 2009"

# Import data
deskhistory_table <- read_csv("https://raw.githubusercontent.com/harryahlas/sample-hr-database/master/data/deskhistory_table.csv")
deskjob_table <- read_csv("https://raw.githubusercontent.com/harryahlas/sample-hr-database/master/data/deskjob_table.csv")

knitr::kable(deskhistory_table[sample(nrow(deskhistory_table),5),])
knitr::kable(deskjob_table[sample(nrow(deskjob_table),5),])


# Summary data frame
hcto_summary <- deskhistory_table %>% 
  left_join(deskjob_table) %>% 
  filter(desk_id_start_date <= as.Date("2009-12-31"),
         desk_id_end_date >= as.Date("2009-01-01")) %>% 
  arrange(desc(desk_id_end_date)) %>% 
  group_by(employee_num) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  mutate(year = "2009",
         termination_flag = if_else(termination_flag == 1, "Terminated", "DidNotTerminate")) %>% 
  count(year, job_name, termination_flag) %>% 
  spread(termination_flag, n, fill = 0) %>% 
  mutate(Headcount =  Terminated + DidNotTerminate,
         TerminationRate = percent(Terminated / Headcount)) %>% 
  arrange(desc(Terminated / Headcount))

knitr::kable(hcto_summary)

# Data disclaimer
disclaimer_info <-   data.frame(Information = 
  c("Source: https://github.com/harryahlas/sample-hr-database/tree/master/data",
    paste("Data as of", as_of_date, "."),
    "Data includes all employees rolling up to CEO Tricia Avallone who were active at any point from Jan 1, 2009 through December 31, 2009.",
    "If the employee had multiple jobs during 2009, only the most recent job is counted.",
    "Data is confidential and should be shared on a need to know basis only.",
    "Do not distribute externally.")) 


# Expore to Excel
wb <- createWorkbook()
addWorksheet(wb, report_name)
addWorksheet(wb, "Data Disclaimer")
writeDataTable(wb, 1, hcto_summary)
writeDataTable(wb, 2, disclaimer_info)
addStyle(wb, 2, style = createStyle(wrapText = TRUE), rows = 1:7, cols = 1)
setColWidths(wb, 2, 1, widths = 50)
saveWorkbook(wb, paste0(report_name, as_of_date, ".xlsx"), TRUE)
