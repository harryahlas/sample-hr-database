library(RMariaDB)
library(tidyverse)

HRSAMPLE <- dbConnect(RMariaDB::MariaDB(), user='newuser', password='newuser', dbname='hrsample', host='localhost')

# Create rollup_view ------------------------------------------------------

rollup_sql <- "CREATE VIEW rollup AS  
WITH RECURSIVE cte AS
(
  SELECT desk_id, org, parent_id, 0 AS depth FROM hierarchy WHERE parent_id IS NULL
  UNION ALL
  SELECT c.desk_id, c.org, c.parent_id, cte.depth+1 FROM hierarchy c JOIN cte ON
  cte.desk_id=c.parent_id
),
  


cte3 AS (SELECT * FROM cte WHERE depth = 3),
  cte2 AS (SELECT * FROM cte WHERE depth = 2),
  cte1 AS (SELECT * FROM cte WHERE depth = 1),
  cte0 AS (SELECT * FROM cte WHERE depth = 0)
  
  SELECT 
  cte0.desk_id AS lvl00_desk_id,
  cte0.org AS lvl00_org,   
  cte1.desk_id AS lvl01_desk_id,
  cte1.org AS lvl01_org,  
  cte2.desk_id AS lvl02_desk_id,
  cte2.org AS lvl02_org, 
  cte3.desk_id AS lvl03_desk_id,
  cte3.org AS lvl03_org,
  cte.desk_id AS lvl04_desk_id,
  cte.org AS lvl04_org,  
  cte.depth
  
  FROM cte
  LEFT JOIN cte3 ON cte.parent_id = cte3.desk_id
  LEFT JOIN cte2 ON cte3.parent_id = cte2.desk_id
  LEFT JOIN cte1 ON cte2.parent_id = cte1.desk_id
  LEFT JOIN cte0 ON cte1.parent_id = cte0.desk_id
  WHERE cte.depth = 4
  
  UNION
  
  SELECT 
  cte0.desk_id AS lvl00_desk_id,
  cte0.org AS lvl00_org,   
  cte1.desk_id AS lvl01_desk_id,
  cte1.org AS lvl01_org,  
  cte2.desk_id AS lvl02_desk_id,
  cte2.org AS lvl02_org, 
  cte.desk_id AS lvl03_desk_id,
  cte.org AS lvl03_org,
  cte.desk_id AS lvl04_desk_id,
  cte.org AS lvl04_org,  
  cte.depth
  
  FROM cte
  LEFT JOIN cte2 ON cte.parent_id = cte2.desk_id
  LEFT JOIN cte1 ON cte2.parent_id = cte1.desk_id
  LEFT JOIN cte0 ON cte1.parent_id = cte0.desk_id
  WHERE cte.depth = 3
  
  UNION
  
  SELECT 
  cte0.desk_id AS lvl00_desk_id,
  cte0.org AS lvl00_org,   
  cte1.desk_id AS lvl01_desk_id,
  cte1.org AS lvl01_org,  
  cte.desk_id AS lvl02_desk_id,
  cte.org AS lvl02_org, 
  cte.desk_id AS lvl03_desk_id,
  cte.org AS lvl03_org,
  cte.desk_id AS lvl04_desk_id,
  cte.org AS lvl04_org,  
  cte.depth
  
  FROM cte
  LEFT JOIN cte1 ON cte.parent_id = cte1.desk_id
  LEFT JOIN cte0 ON cte1.parent_id = cte0.desk_id
  WHERE cte.depth = 2
  
  UNION
  
  SELECT 
  cte0.desk_id AS lvl00_desk_id,
  cte0.org AS lvl00_org,   
  cte.desk_id AS lvl01_desk_id,
  cte.org AS lvl01_org,  
  cte.desk_id AS lvl02_desk_id,
  cte.org AS lvl02_org, 
  cte.desk_id AS lvl03_desk_id,
  cte.org AS lvl03_org,
  cte.desk_id AS lvl04_desk_id,
  cte.org AS lvl04_org,  
  cte.depth
  
  FROM cte
  LEFT JOIN cte0 ON cte.parent_id = cte0.desk_id
  WHERE cte.depth = 1
  
  UNION
  
  SELECT 
  cte.desk_id AS lvl00_desk_id,
  cte.org AS lvl00_org,   
  cte.desk_id AS lvl01_desk_id,
  cte.org AS lvl01_org,  
  cte.desk_id AS lvl02_desk_id,
  cte.org AS lvl02_org, 
  cte.desk_id AS lvl03_desk_id,
  cte.org AS lvl03_org,
  cte.desk_id AS lvl04_desk_id,
  cte.org AS lvl04_org,  
  cte.depth
  
  FROM cte
  WHERE cte.depth = 0
  
  ORDER BY depth"

  
dbExecute(HRSAMPLE, "DROP VIEW IF EXISTS rollup;") 
dbExecute(HRSAMPLE, rollup_sql)

