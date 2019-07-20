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
 AND DH.employee_num = 12417 
 AND DH.desk_id_start_date <= DATE('2003-12-05') 
 AND DH.desk_id_end_date >= DATE('2003-12-05') ;