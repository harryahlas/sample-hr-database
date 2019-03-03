SELECT
 DATE('%DATE_ID%') AS "Date of incident or notification",
 DH.employee_num,
 DH.desk_id,
 DJ.job_name
 FROM 
	`hrsample`.`deskhistory` DH, 
    `hrsample`.`deskjob` DJ  
 WHERE 
 DJ.desk_id = DH.desk_id
 AND DH.employee_num = %EMP_ID% 
 AND DH.desk_id_start_date <= DATE('%DATE_ID%') 
 AND DH.desk_id_end_date >= DATE('%DATE_ID%') ;