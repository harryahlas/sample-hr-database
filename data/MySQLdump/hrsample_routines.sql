-- MySQL dump 10.13  Distrib 8.0.15, for Win64 (x86_64)
--
-- Host: localhost    Database: hrsample
-- ------------------------------------------------------
-- Server version	8.0.15

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
 SET NAMES utf8 ;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Temporary view structure for view `rollup`
--

DROP TABLE IF EXISTS `rollup`;
/*!50001 DROP VIEW IF EXISTS `rollup`*/;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8mb4;
/*!50001 CREATE VIEW `rollup` AS SELECT 
 1 AS `lvl00_desk_id`,
 1 AS `lvl00_org`,
 1 AS `lvl01_desk_id`,
 1 AS `lvl01_org`,
 1 AS `lvl02_desk_id`,
 1 AS `lvl02_org`,
 1 AS `lvl03_desk_id`,
 1 AS `lvl03_org`,
 1 AS `lvl04_desk_id`,
 1 AS `lvl04_org`,
 1 AS `depth`*/;
SET character_set_client = @saved_cs_client;

--
-- Final view structure for view `rollup`
--

/*!50001 DROP VIEW IF EXISTS `rollup`*/;
/*!50001 SET @saved_cs_client          = @@character_set_client */;
/*!50001 SET @saved_cs_results         = @@character_set_results */;
/*!50001 SET @saved_col_connection     = @@collation_connection */;
/*!50001 SET character_set_client      = utf8mb4 */;
/*!50001 SET character_set_results     = utf8mb4 */;
/*!50001 SET collation_connection      = utf8mb4_general_ci */;
/*!50001 CREATE ALGORITHM=UNDEFINED */
/*!50013 DEFINER=`newuser`@`localhost` SQL SECURITY DEFINER */
/*!50001 VIEW `rollup` AS with recursive `cte` as (select `hierarchy`.`desk_id` AS `desk_id`,`hierarchy`.`org` AS `org`,`hierarchy`.`parent_id` AS `parent_id`,0 AS `depth` from `hierarchy` where isnull(`hierarchy`.`parent_id`) union all select `c`.`desk_id` AS `desk_id`,`c`.`org` AS `org`,`c`.`parent_id` AS `parent_id`,(`cte`.`depth` + 1) AS `cte.depth+1` from (`hierarchy` `c` join `cte` on((`cte`.`desk_id` = `c`.`parent_id`)))), `cte3` as (select `cte`.`desk_id` AS `desk_id`,`cte`.`org` AS `org`,`cte`.`parent_id` AS `parent_id`,`cte`.`depth` AS `depth` from `cte` where (`cte`.`depth` = 3)), `cte2` as (select `cte`.`desk_id` AS `desk_id`,`cte`.`org` AS `org`,`cte`.`parent_id` AS `parent_id`,`cte`.`depth` AS `depth` from `cte` where (`cte`.`depth` = 2)), `cte1` as (select `cte`.`desk_id` AS `desk_id`,`cte`.`org` AS `org`,`cte`.`parent_id` AS `parent_id`,`cte`.`depth` AS `depth` from `cte` where (`cte`.`depth` = 1)), `cte0` as (select `cte`.`desk_id` AS `desk_id`,`cte`.`org` AS `org`,`cte`.`parent_id` AS `parent_id`,`cte`.`depth` AS `depth` from `cte` where (`cte`.`depth` = 0)) select `cte0`.`desk_id` AS `lvl00_desk_id`,`cte0`.`org` AS `lvl00_org`,`cte1`.`desk_id` AS `lvl01_desk_id`,`cte1`.`org` AS `lvl01_org`,`cte2`.`desk_id` AS `lvl02_desk_id`,`cte2`.`org` AS `lvl02_org`,`cte3`.`desk_id` AS `lvl03_desk_id`,`cte3`.`org` AS `lvl03_org`,`cte`.`desk_id` AS `lvl04_desk_id`,`cte`.`org` AS `lvl04_org`,`cte`.`depth` AS `depth` from ((((`cte` left join `cte3` on((`cte`.`parent_id` = `cte3`.`desk_id`))) left join `cte2` on((`cte3`.`parent_id` = `cte2`.`desk_id`))) left join `cte1` on((`cte2`.`parent_id` = `cte1`.`desk_id`))) left join `cte0` on((`cte1`.`parent_id` = `cte0`.`desk_id`))) where (`cte`.`depth` = 4) union select `cte0`.`desk_id` AS `lvl00_desk_id`,`cte0`.`org` AS `lvl00_org`,`cte1`.`desk_id` AS `lvl01_desk_id`,`cte1`.`org` AS `lvl01_org`,`cte2`.`desk_id` AS `lvl02_desk_id`,`cte2`.`org` AS `lvl02_org`,`cte`.`desk_id` AS `lvl03_desk_id`,`cte`.`org` AS `lvl03_org`,`cte`.`desk_id` AS `lvl04_desk_id`,`cte`.`org` AS `lvl04_org`,`cte`.`depth` AS `depth` from (((`cte` left join `cte2` on((`cte`.`parent_id` = `cte2`.`desk_id`))) left join `cte1` on((`cte2`.`parent_id` = `cte1`.`desk_id`))) left join `cte0` on((`cte1`.`parent_id` = `cte0`.`desk_id`))) where (`cte`.`depth` = 3) union select `cte0`.`desk_id` AS `lvl00_desk_id`,`cte0`.`org` AS `lvl00_org`,`cte1`.`desk_id` AS `lvl01_desk_id`,`cte1`.`org` AS `lvl01_org`,`cte`.`desk_id` AS `lvl02_desk_id`,`cte`.`org` AS `lvl02_org`,`cte`.`desk_id` AS `lvl03_desk_id`,`cte`.`org` AS `lvl03_org`,`cte`.`desk_id` AS `lvl04_desk_id`,`cte`.`org` AS `lvl04_org`,`cte`.`depth` AS `depth` from ((`cte` left join `cte1` on((`cte`.`parent_id` = `cte1`.`desk_id`))) left join `cte0` on((`cte1`.`parent_id` = `cte0`.`desk_id`))) where (`cte`.`depth` = 2) union select `cte0`.`desk_id` AS `lvl00_desk_id`,`cte0`.`org` AS `lvl00_org`,`cte`.`desk_id` AS `lvl01_desk_id`,`cte`.`org` AS `lvl01_org`,`cte`.`desk_id` AS `lvl02_desk_id`,`cte`.`org` AS `lvl02_org`,`cte`.`desk_id` AS `lvl03_desk_id`,`cte`.`org` AS `lvl03_org`,`cte`.`desk_id` AS `lvl04_desk_id`,`cte`.`org` AS `lvl04_org`,`cte`.`depth` AS `depth` from (`cte` left join `cte0` on((`cte`.`parent_id` = `cte0`.`desk_id`))) where (`cte`.`depth` = 1) union select `cte`.`desk_id` AS `lvl00_desk_id`,`cte`.`org` AS `lvl00_org`,`cte`.`desk_id` AS `lvl01_desk_id`,`cte`.`org` AS `lvl01_org`,`cte`.`desk_id` AS `lvl02_desk_id`,`cte`.`org` AS `lvl02_org`,`cte`.`desk_id` AS `lvl03_desk_id`,`cte`.`org` AS `lvl03_org`,`cte`.`desk_id` AS `lvl04_desk_id`,`cte`.`org` AS `lvl04_org`,`cte`.`depth` AS `depth` from `cte` where (`cte`.`depth` = 0) order by `depth` */;
/*!50001 SET character_set_client      = @saved_cs_client */;
/*!50001 SET character_set_results     = @saved_cs_results */;
/*!50001 SET collation_connection      = @saved_col_connection */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2019-05-05 19:03:37
