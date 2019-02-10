WITH RECURSIVE cte AS
(
  SELECT desk_id, org, parent_id, 0 AS depth FROM hierarchy WHERE parent_id IS NULL
  UNION ALL
  SELECT c.desk_id, c.org, c.parent_id, cte.depth+1 FROM hierarchy c JOIN cte ON
    cte.desk_id=c.parent_id
)
SELECT * FROM cte ORDER BY depth;