SELECT
 '2003-12-05' AS "Date of incident or notification",
 DH.employee_num,
 DH.desk_id,
 DJ.job_name
 FROM 
	deskhistory DH, 
    deskjob DJ  
 WHERE 
 DJ.desk_id = DH.desk_id
-- AND DH.employee_num = 33066 
 AND DH.employee_num in (33066, 41678, 25653, 30041, 27548, 5121, 28603, 31561, 37883, 37388, 12564, 29981, 4526, 44675, 30754, 42878, 27945, 21747, 25226, 7187, 39719, 40039, 28006, 1244, 44161) 
 AND DH.desk_id_start_date <= DATE('2003-12-05') 
 AND DH.desk_id_end_date >= DATE('2003-12-05') ;