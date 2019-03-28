CREATE VIEW `hrsample`.`rollup` AS  #ok to change this name and name of this sql file
WITH RECURSIVE cte AS
(
  SELECT desk_id, org, parent_id, 0 AS depth FROM `hrsample`.`hierarchy` WHERE parent_id IS NULL
  UNION ALL
  SELECT c.desk_id, c.org, c.parent_id, cte.depth+1 FROM `hrsample`.`hierarchy` c JOIN cte ON
    cte.desk_id=c.parent_id
),

cte3 AS (SELECT * FROM cte WHERE depth = 3),
cte2 AS (SELECT * FROM cte WHERE depth = 2),
cte1 AS (SELECT * FROM cte WHERE depth = 1),
cte0 AS (SELECT * FROM cte WHERE depth = 0)

	SELECT 
	cte0.desk_id AS DESK_ID_LEVEL_0,
	cte0.org AS HRC_LEVEL_0,   
	cte1.desk_id AS DESK_ID_LEVEL_1,
	cte1.org AS HRC_LEVEL_1,  
	cte2.desk_id AS DESK_ID_LEVEL_2,
	cte2.org AS HRC_LEVEL_2, 
	cte3.desk_id AS DESK_ID_LEVEL_3,
	cte3.org AS HRC_LEVEL_3,
	cte.desk_id AS DESK_ID_LEVEL_4,
	cte.org AS HRC_LEVEL_4,  
	cte.depth

	FROM cte
	LEFT JOIN cte3 ON cte.parent_id = cte3.desk_id
	LEFT JOIN cte2 ON cte3.parent_id = cte2.desk_id
	LEFT JOIN cte1 ON cte2.parent_id = cte1.desk_id
	LEFT JOIN cte0 ON cte1.parent_id = cte0.desk_id
	WHERE cte.depth = 4

UNION

	SELECT 
	cte0.desk_id AS DESK_ID_LEVEL_0,
	cte0.org AS HRC_LEVEL_0,   
	cte1.desk_id AS DESK_ID_LEVEL_1,
	cte1.org AS HRC_LEVEL_1,   
	cte2.desk_id AS DESK_ID_LEVEL_2,
	cte2.org AS HRC_LEVEL_2,   
	cte.desk_id AS DESK_ID_LEVEL_3,
	cte.org AS HRC_LEVEL_3,
	cte.desk_id AS DESK_ID_LEVEL_4,
	cte.org AS HRC_LEVEL_4,  
	cte.depth

	FROM cte
	LEFT JOIN cte2 ON cte.parent_id = cte2.desk_id
	LEFT JOIN cte1 ON cte2.parent_id = cte1.desk_id
	LEFT JOIN cte0 ON cte1.parent_id = cte0.desk_id
	WHERE cte.depth = 3

UNION

	SELECT 
	cte0.desk_id AS DESK_ID_LEVEL_0,
	cte0.org AS HRC_LEVEL_0,   
	cte1.desk_id AS DESK_ID_LEVEL_1,
	cte1.org AS HRC_LEVEL_1,   
	cte.desk_id AS DESK_ID_LEVEL_2,
	cte.org AS HRC_LEVEL_2,   
	cte.desk_id AS DESK_ID_LEVEL_3,
	cte.org AS HRC_LEVEL_3,
	cte.desk_id AS DESK_ID_LEVEL_4,
	cte.org AS HRC_LEVEL_4,  
	cte.depth

	FROM cte
	LEFT JOIN cte1 ON cte.parent_id = cte1.desk_id
	LEFT JOIN cte0 ON cte1.parent_id = cte0.desk_id
	WHERE cte.depth = 2

UNION

	SELECT 
	cte0.desk_id AS DESK_ID_LEVEL_0,
	cte0.org AS HRC_LEVEL_0,
	cte.desk_id AS DESK_ID_LEVEL_1,
	cte.org AS HRC_LEVEL_1,   
	cte.desk_id AS DESK_ID_LEVEL_2,
	cte.org AS HRC_LEVEL_2,   
	cte.desk_id AS DESK_ID_LEVEL_3,
	cte.org AS HRC_LEVEL_3,
	cte.desk_id AS DESK_ID_LEVEL_4,
	cte.org AS HRC_LEVEL_4,  
	cte.depth

	FROM cte
	LEFT JOIN cte0 ON cte.parent_id = cte0.desk_id
	WHERE cte.depth = 1

UNION

	SELECT 
	cte.desk_id AS DESK_ID_LEVEL_0,
	cte.org AS HRC_LEVEL_0,
	cte.desk_id AS DESK_ID_LEVEL_1,
	cte.org AS HRC_LEVEL_1,   
	cte.desk_id AS DESK_ID_LEVEL_2,
	cte.org AS HRC_LEVEL_2,   
	cte.desk_id AS DESK_ID_LEVEL_3,
	cte.org AS HRC_LEVEL_3,
	cte.desk_id AS DESK_ID_LEVEL_4,
	cte.org AS HRC_LEVEL_4, 
	cte.depth

	FROM cte
	WHERE cte.depth = 0

ORDER BY depth


