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
-- Table structure for table `deskjob`
--

DROP TABLE IF EXISTS `deskjob`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
 SET character_set_client = utf8mb4 ;
CREATE TABLE `deskjob` (
  `desk_id` int(10) unsigned NOT NULL,
  `job_name` varchar(255) DEFAULT NULL,
  KEY `desk_id` (`desk_id`),
  CONSTRAINT `deskjob_ibfk_1` FOREIGN KEY (`desk_id`) REFERENCES `hierarchy` (`desk_id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `deskjob`
--

LOCK TABLES `deskjob` WRITE;
/*!40000 ALTER TABLE `deskjob` DISABLE KEYS */;
INSERT INTO `deskjob` VALUES (1,'CEO'),(2,'Business Leader'),(3,'Business Leader'),(4,'Business Leader'),(5,'Business Leader'),(6,'Business Leader'),(7,'Business Leader'),(8,'Business Leader'),(9,'Department Leader'),(10,'Department Leader'),(11,'Department Leader'),(12,'Department Leader'),(13,'Department Leader'),(14,'Department Leader'),(15,'Department Leader'),(16,'Department Leader'),(17,'Department Leader'),(18,'Department Leader'),(19,'Department Leader'),(20,'Department Leader'),(21,'Department Leader'),(22,'Department Leader'),(23,'Department Leader'),(24,'Department Leader'),(25,'Department Leader'),(26,'Department Leader'),(27,'Department Leader'),(28,'Department Leader'),(29,'Department Leader'),(30,'Department Leader'),(31,'Department Leader'),(32,'Department Leader'),(33,'Department Leader'),(34,'Department Leader'),(35,'Department Leader'),(36,'Department Leader'),(37,'Department Leader'),(38,'Department Leader'),(39,'Department Leader'),(40,'Department Leader'),(41,'Department Leader'),(42,'Regional Leader'),(43,'Regional Leader'),(44,'Regional Leader'),(45,'Regional Leader'),(46,'Regional Leader'),(47,'Regional Leader'),(48,'Regional Leader'),(49,'Regional Leader'),(50,'Regional Leader'),(51,'Regional Leader'),(52,'Regional Leader'),(53,'Regional Leader'),(54,'Regional Leader'),(55,'Regional Leader'),(56,'Regional Leader'),(57,'Regional Leader'),(58,'Regional Leader'),(59,'Regional Leader'),(60,'Regional Leader'),(61,'Regional Leader'),(62,'Regional Leader'),(63,'Regional Leader'),(64,'Regional Leader'),(65,'Regional Leader'),(66,'Regional Leader'),(67,'Regional Leader'),(68,'Regional Leader'),(69,'Regional Leader'),(70,'Regional Leader'),(71,'Regional Leader'),(72,'Regional Leader'),(73,'Regional Leader'),(74,'Regional Leader'),(75,'Regional Leader'),(76,'Regional Leader'),(77,'Regional Leader'),(78,'Regional Leader'),(79,'Regional Leader'),(80,'Regional Leader'),(81,'Regional Leader'),(82,'Regional Leader'),(83,'Regional Leader'),(84,'Regional Leader'),(85,'Regional Leader'),(86,'Regional Leader'),(87,'Regional Leader'),(88,'Regional Leader'),(89,'Regional Leader'),(90,'Regional Leader'),(91,'Regional Leader'),(92,'Regional Leader'),(93,'Regional Leader'),(94,'Regional Leader'),(95,'Regional Leader'),(96,'Regional Leader'),(97,'Regional Leader'),(98,'Regional Leader'),(99,'Regional Leader'),(100,'Regional Leader'),(101,'Regional Leader'),(102,'Regional Leader'),(103,'Regional Leader'),(104,'Regional Leader'),(105,'Regional Leader'),(106,'Regional Leader'),(107,'Regional Leader'),(108,'Regional Leader'),(109,'Regional Leader'),(110,'Regional Leader'),(111,'Regional Leader'),(112,'Regional Leader'),(113,'Regional Leader'),(114,'Regional Leader'),(115,'Regional Leader'),(116,'Regional Leader'),(117,'Regional Leader'),(118,'Regional Leader'),(119,'Regional Leader'),(120,'Regional Leader'),(121,'Regional Leader'),(122,'Regional Leader'),(123,'Regional Leader'),(124,'Regional Leader'),(125,'Regional Leader'),(126,'Regional Leader'),(127,'Regional Leader'),(128,'Regional Leader'),(129,'Regional Leader'),(130,'Regional Leader'),(131,'Regional Leader'),(132,'Regional Leader'),(133,'Regional Leader'),(134,'Regional Leader'),(135,'Regional Leader'),(136,'Regional Leader'),(137,'Regional Leader'),(138,'Regional Leader'),(139,'Regional Leader'),(140,'Regional Leader'),(141,'Regional Leader'),(142,'Regional Leader'),(143,'Regional Leader'),(144,'Regional Leader'),(145,'Regional Leader'),(146,'Regional Leader'),(147,'Regional Leader'),(148,'Regional Leader'),(149,'Regional Leader'),(150,'Regional Leader'),(151,'Regional Leader'),(152,'Regional Leader'),(153,'Regional Leader'),(154,'Regional Leader'),(155,'Regional Leader'),(156,'Regional Leader'),(157,'Regional Leader'),(158,'Regional Leader'),(159,'Regional Leader'),(160,'Regional Leader'),(161,'Regional Leader'),(162,'Regional Leader'),(163,'Regional Leader'),(164,'Regional Leader'),(165,'Regional Leader'),(166,'Regional Leader'),(167,'Regional Leader'),(168,'Regional Leader'),(169,'Regional Leader'),(170,'Regional Leader'),(171,'Regional Leader'),(172,'Regional Leader'),(173,'Regional Leader'),(174,'Regional Leader'),(175,'Regional Leader'),(176,'Regional Leader'),(177,'Regional Leader'),(178,'Regional Leader'),(179,'Consultant'),(180,'Analyst'),(181,'Administrative Assistant'),(182,'Product Manager'),(183,'Project Manager'),(184,'Project Manager'),(185,'Analyst'),(186,'Administrative Assistant'),(187,'Project Manager'),(188,'Consultant'),(189,'Administrative Assistant'),(190,'Project Manager'),(191,'Analyst'),(192,'Consultant'),(193,'Administrative Assistant'),(194,'Product Manager'),(195,'Analyst'),(196,'Analyst'),(197,'Analyst'),(198,'Consultant'),(199,'Product Manager'),(200,'Project Manager'),(201,'Consultant'),(202,'Analyst'),(203,'Administrative Assistant'),(204,'Product Manager'),(205,'Project Manager'),(206,'Product Manager'),(207,'Project Manager'),(208,'Administrative Assistant'),(209,'Analyst'),(210,'Administrative Assistant'),(211,'Consultant'),(212,'Administrative Assistant'),(213,'Project Manager'),(214,'Analyst'),(215,'Project Manager'),(216,'Consultant'),(217,'Project Manager'),(218,'Project Manager'),(219,'Product Manager'),(220,'Consultant'),(221,'Product Manager'),(222,'Consultant'),(223,'Product Manager'),(224,'Consultant'),(225,'Consultant'),(226,'Administrative Assistant'),(227,'Analyst'),(228,'Consultant'),(229,'Analyst'),(230,'Product Manager'),(231,'Administrative Assistant'),(232,'Product Manager'),(233,'Administrative Assistant'),(234,'Product Manager'),(235,'Consultant'),(236,'Project Manager'),(237,'Consultant'),(238,'Consultant'),(239,'Project Manager'),(240,'Product Manager'),(241,'Consultant'),(242,'Analyst'),(243,'Consultant'),(244,'Consultant'),(245,'Consultant'),(246,'Administrative Assistant'),(247,'Administrative Assistant'),(248,'Consultant'),(249,'Consultant'),(250,'Administrative Assistant'),(251,'Consultant'),(252,'Product Manager'),(253,'Project Manager'),(254,'Project Manager'),(255,'Administrative Assistant'),(256,'Consultant'),(257,'Project Manager'),(258,'Administrative Assistant'),(259,'Analyst'),(260,'Consultant'),(261,'Consultant'),(262,'Consultant'),(263,'Project Manager'),(264,'Administrative Assistant'),(265,'Analyst'),(266,'Project Manager'),(267,'Consultant'),(268,'Consultant'),(269,'Consultant'),(270,'Administrative Assistant'),(271,'Project Manager'),(272,'Administrative Assistant'),(273,'Project Manager'),(274,'Administrative Assistant'),(275,'Project Manager'),(276,'Administrative Assistant'),(277,'Product Manager'),(278,'Product Manager'),(279,'Project Manager'),(280,'Product Manager'),(281,'Administrative Assistant'),(282,'Analyst'),(283,'Product Manager'),(284,'Product Manager'),(285,'Analyst'),(286,'Project Manager'),(287,'Consultant'),(288,'Product Manager'),(289,'Product Manager'),(290,'Administrative Assistant'),(291,'Project Manager'),(292,'Consultant'),(293,'Consultant'),(294,'Analyst'),(295,'Product Manager'),(296,'Analyst'),(297,'Analyst'),(298,'Product Manager'),(299,'Administrative Assistant'),(300,'Consultant'),(301,'Project Manager'),(302,'Analyst'),(303,'Consultant'),(304,'Analyst'),(305,'Consultant'),(306,'Administrative Assistant'),(307,'Administrative Assistant'),(308,'Project Manager'),(309,'Consultant'),(310,'Analyst'),(311,'Project Manager'),(312,'Analyst'),(313,'Product Manager'),(314,'Project Manager'),(315,'Project Manager'),(316,'Administrative Assistant'),(317,'Administrative Assistant'),(318,'Consultant'),(319,'Product Manager'),(320,'Consultant'),(321,'Product Manager'),(322,'Administrative Assistant'),(323,'Product Manager'),(324,'Administrative Assistant'),(325,'Product Manager'),(326,'Analyst'),(327,'Project Manager'),(328,'Product Manager'),(329,'Product Manager'),(330,'Administrative Assistant'),(331,'Product Manager'),(332,'Project Manager'),(333,'Product Manager'),(334,'Product Manager'),(335,'Administrative Assistant'),(336,'Project Manager'),(337,'Product Manager'),(338,'Analyst'),(339,'Administrative Assistant'),(340,'Consultant'),(341,'Administrative Assistant'),(342,'Product Manager'),(343,'Consultant'),(344,'Salesperson'),(345,'Salesperson'),(346,'Salesperson'),(347,'Salesperson'),(348,'Salesperson'),(349,'Salesperson'),(350,'Salesperson'),(351,'Salesperson'),(352,'Salesperson'),(353,'Salesperson'),(354,'Salesperson'),(355,'Salesperson'),(356,'Salesperson'),(357,'Salesperson'),(358,'Salesperson'),(359,'Salesperson'),(360,'Salesperson'),(361,'Salesperson'),(362,'Salesperson'),(363,'Salesperson'),(364,'Salesperson'),(365,'Salesperson'),(366,'Salesperson'),(367,'Salesperson'),(368,'Salesperson'),(369,'Salesperson'),(370,'Salesperson'),(371,'Salesperson'),(372,'Salesperson'),(373,'Salesperson'),(374,'Salesperson'),(375,'Salesperson'),(376,'Salesperson'),(377,'Salesperson'),(378,'Salesperson'),(379,'Salesperson'),(380,'Salesperson'),(381,'Salesperson'),(382,'Salesperson'),(383,'Salesperson'),(384,'Salesperson'),(385,'Salesperson'),(386,'Salesperson'),(387,'Salesperson'),(388,'Salesperson'),(389,'Salesperson'),(390,'Salesperson'),(391,'Salesperson'),(392,'Salesperson'),(393,'Salesperson'),(394,'Salesperson'),(395,'Salesperson'),(396,'Salesperson'),(397,'Salesperson'),(398,'Salesperson'),(399,'Salesperson'),(400,'Salesperson'),(401,'Salesperson'),(402,'Salesperson'),(403,'Salesperson'),(404,'Salesperson'),(405,'Salesperson'),(406,'Salesperson'),(407,'Salesperson'),(408,'Salesperson'),(409,'Salesperson'),(410,'Salesperson'),(411,'Salesperson'),(412,'Salesperson'),(413,'Salesperson'),(414,'Salesperson'),(415,'Salesperson'),(416,'Salesperson'),(417,'Salesperson'),(418,'Salesperson'),(419,'Salesperson'),(420,'Salesperson'),(421,'Salesperson'),(422,'Salesperson'),(423,'Salesperson'),(424,'Salesperson'),(425,'Salesperson'),(426,'Salesperson'),(427,'Salesperson'),(428,'Salesperson'),(429,'Salesperson'),(430,'Salesperson'),(431,'Salesperson'),(432,'Salesperson'),(433,'Salesperson'),(434,'Salesperson'),(435,'Salesperson'),(436,'Salesperson'),(437,'Salesperson'),(438,'Salesperson'),(439,'Salesperson'),(440,'Salesperson'),(441,'Salesperson'),(442,'Salesperson'),(443,'Salesperson'),(444,'Salesperson'),(445,'Salesperson'),(446,'Salesperson'),(447,'Salesperson'),(448,'Salesperson'),(449,'Salesperson'),(450,'Salesperson'),(451,'Salesperson'),(452,'Salesperson'),(453,'Salesperson'),(454,'Salesperson'),(455,'Salesperson'),(456,'Salesperson'),(457,'Salesperson'),(458,'Salesperson'),(459,'Salesperson'),(460,'Salesperson'),(461,'Salesperson'),(462,'Salesperson'),(463,'Salesperson'),(464,'Salesperson'),(465,'Salesperson'),(466,'Salesperson'),(467,'Salesperson'),(468,'Salesperson'),(469,'Salesperson'),(470,'Salesperson'),(471,'Salesperson'),(472,'Salesperson'),(473,'Salesperson'),(474,'Salesperson'),(475,'Salesperson'),(476,'Salesperson'),(477,'Salesperson'),(478,'Salesperson'),(479,'Salesperson'),(480,'Salesperson'),(481,'Salesperson'),(482,'Salesperson'),(483,'Salesperson'),(484,'Salesperson'),(485,'Salesperson'),(486,'Salesperson'),(487,'Salesperson'),(488,'Salesperson'),(489,'Salesperson'),(490,'Salesperson'),(491,'Salesperson'),(492,'Salesperson'),(493,'Salesperson'),(494,'Salesperson'),(495,'Salesperson'),(496,'Salesperson'),(497,'Salesperson'),(498,'Salesperson'),(499,'Salesperson'),(500,'Salesperson'),(501,'Salesperson'),(502,'Salesperson'),(503,'Salesperson'),(504,'Salesperson'),(505,'Salesperson'),(506,'Salesperson'),(507,'Salesperson'),(508,'Salesperson'),(509,'Salesperson'),(510,'Salesperson'),(511,'Salesperson'),(512,'Salesperson'),(513,'Salesperson'),(514,'Salesperson'),(515,'Salesperson'),(516,'Salesperson'),(517,'Salesperson'),(518,'Salesperson'),(519,'Salesperson'),(520,'Salesperson'),(521,'Salesperson'),(522,'Salesperson'),(523,'Salesperson'),(524,'Salesperson'),(525,'Salesperson'),(526,'Salesperson'),(527,'Salesperson'),(528,'Salesperson'),(529,'Salesperson'),(530,'Salesperson'),(531,'Salesperson'),(532,'Salesperson'),(533,'Salesperson'),(534,'Salesperson'),(535,'Salesperson'),(536,'Salesperson'),(537,'Salesperson'),(538,'Salesperson'),(539,'Salesperson'),(540,'Salesperson'),(541,'Salesperson'),(542,'Salesperson'),(543,'Salesperson'),(544,'Salesperson'),(545,'Salesperson'),(546,'Salesperson'),(547,'Salesperson'),(548,'Salesperson'),(549,'Salesperson'),(550,'Salesperson'),(551,'Salesperson'),(552,'Salesperson'),(553,'Salesperson'),(554,'Salesperson'),(555,'Salesperson'),(556,'Salesperson'),(557,'Salesperson'),(558,'Salesperson'),(559,'Salesperson'),(560,'Salesperson'),(561,'Salesperson'),(562,'Salesperson'),(563,'Salesperson'),(564,'Salesperson'),(565,'Salesperson'),(566,'Salesperson'),(567,'Salesperson'),(568,'Salesperson'),(569,'Salesperson'),(570,'Salesperson'),(571,'Salesperson'),(572,'Salesperson'),(573,'Salesperson'),(574,'Salesperson'),(575,'Salesperson'),(576,'Salesperson'),(577,'Salesperson'),(578,'Salesperson'),(579,'Salesperson'),(580,'Salesperson'),(581,'Salesperson'),(582,'Salesperson'),(583,'Salesperson'),(584,'Salesperson'),(585,'Salesperson'),(586,'Salesperson'),(587,'Salesperson'),(588,'Salesperson'),(589,'Salesperson'),(590,'Salesperson'),(591,'Salesperson'),(592,'Salesperson'),(593,'Salesperson'),(594,'Salesperson'),(595,'Salesperson'),(596,'Salesperson'),(597,'Salesperson'),(598,'Salesperson'),(599,'Salesperson'),(600,'Salesperson'),(601,'Salesperson'),(602,'Salesperson'),(603,'Salesperson'),(604,'Salesperson'),(605,'Salesperson'),(606,'Salesperson'),(607,'Salesperson'),(608,'Salesperson'),(609,'Salesperson'),(610,'Salesperson'),(611,'Salesperson'),(612,'Salesperson'),(613,'Salesperson'),(614,'Salesperson'),(615,'Salesperson'),(616,'Salesperson'),(617,'Salesperson'),(618,'Salesperson'),(619,'Salesperson'),(620,'Salesperson'),(621,'Salesperson'),(622,'Salesperson'),(623,'Salesperson'),(624,'Salesperson'),(625,'Salesperson'),(626,'Salesperson'),(627,'Salesperson'),(628,'Salesperson'),(629,'Salesperson'),(630,'Salesperson'),(631,'Salesperson'),(632,'Salesperson'),(633,'Salesperson'),(634,'Salesperson'),(635,'Salesperson'),(636,'Salesperson'),(637,'Salesperson'),(638,'Salesperson'),(639,'Salesperson'),(640,'Salesperson'),(641,'Salesperson'),(642,'Salesperson'),(643,'Salesperson'),(644,'Salesperson'),(645,'Salesperson'),(646,'Salesperson'),(647,'Salesperson'),(648,'Salesperson'),(649,'Salesperson'),(650,'Salesperson'),(651,'Salesperson'),(652,'Salesperson'),(653,'Salesperson'),(654,'Salesperson'),(655,'Salesperson'),(656,'Salesperson'),(657,'Salesperson'),(658,'Salesperson'),(659,'Salesperson'),(660,'Salesperson'),(661,'Salesperson'),(662,'Salesperson'),(663,'Salesperson'),(664,'Salesperson'),(665,'Salesperson'),(666,'Salesperson'),(667,'Salesperson'),(668,'Salesperson'),(669,'Salesperson'),(670,'Salesperson'),(671,'Salesperson'),(672,'Salesperson'),(673,'Salesperson'),(674,'Salesperson'),(675,'Salesperson'),(676,'Salesperson'),(677,'Salesperson'),(678,'Salesperson'),(679,'Salesperson'),(680,'Salesperson'),(681,'Salesperson'),(682,'Salesperson'),(683,'Salesperson'),(684,'Salesperson'),(685,'Salesperson'),(686,'Salesperson'),(687,'Salesperson'),(688,'Salesperson'),(689,'Salesperson'),(690,'Salesperson'),(691,'Salesperson'),(692,'Salesperson'),(693,'Salesperson'),(694,'Salesperson'),(695,'Salesperson'),(696,'Salesperson'),(697,'Salesperson'),(698,'Salesperson'),(699,'Salesperson'),(700,'Salesperson'),(701,'Salesperson'),(702,'Salesperson'),(703,'Salesperson'),(704,'Salesperson'),(705,'Salesperson'),(706,'Salesperson'),(707,'Salesperson'),(708,'Salesperson'),(709,'Salesperson'),(710,'Salesperson'),(711,'Salesperson'),(712,'Salesperson'),(713,'Salesperson'),(714,'Salesperson'),(715,'Salesperson'),(716,'Salesperson'),(717,'Salesperson'),(718,'Salesperson'),(719,'Salesperson'),(720,'Salesperson'),(721,'Salesperson'),(722,'Salesperson'),(723,'Salesperson'),(724,'Salesperson'),(725,'Salesperson'),(726,'Salesperson'),(727,'Salesperson'),(728,'Salesperson'),(729,'Salesperson'),(730,'Salesperson'),(731,'Salesperson'),(732,'Salesperson'),(733,'Salesperson'),(734,'Salesperson'),(735,'Salesperson'),(736,'Salesperson'),(737,'Salesperson'),(738,'Salesperson'),(739,'Salesperson'),(740,'Salesperson'),(741,'Salesperson'),(742,'Salesperson'),(743,'Salesperson'),(744,'Salesperson'),(745,'Salesperson'),(746,'Salesperson'),(747,'Salesperson'),(748,'Salesperson'),(749,'Salesperson'),(750,'Salesperson'),(751,'Salesperson'),(752,'Salesperson'),(753,'Salesperson'),(754,'Salesperson'),(755,'Salesperson'),(756,'Salesperson'),(757,'Salesperson'),(758,'Salesperson'),(759,'Salesperson'),(760,'Salesperson'),(761,'Salesperson'),(762,'Salesperson'),(763,'Salesperson'),(764,'Salesperson'),(765,'Salesperson'),(766,'Salesperson'),(767,'Salesperson'),(768,'Salesperson'),(769,'Salesperson'),(770,'Salesperson'),(771,'Salesperson'),(772,'Salesperson'),(773,'Salesperson'),(774,'Salesperson'),(775,'Salesperson'),(776,'Salesperson'),(777,'Salesperson'),(778,'Salesperson'),(779,'Salesperson'),(780,'Salesperson'),(781,'Salesperson'),(782,'Salesperson'),(783,'Salesperson'),(784,'Salesperson'),(785,'Salesperson'),(786,'Salesperson'),(787,'Salesperson'),(788,'Salesperson'),(789,'Salesperson'),(790,'Salesperson'),(791,'Salesperson'),(792,'Salesperson'),(793,'Salesperson'),(794,'Salesperson'),(795,'Salesperson'),(796,'Salesperson'),(797,'Salesperson'),(798,'Salesperson'),(799,'Salesperson'),(800,'Salesperson'),(801,'Salesperson'),(802,'Salesperson'),(803,'Salesperson'),(804,'Salesperson'),(805,'Salesperson'),(806,'Salesperson'),(807,'Salesperson'),(808,'Salesperson'),(809,'Salesperson'),(810,'Salesperson'),(811,'Salesperson'),(812,'Salesperson'),(813,'Salesperson'),(814,'Salesperson'),(815,'Salesperson'),(816,'Salesperson'),(817,'Salesperson'),(818,'Salesperson'),(819,'Salesperson'),(820,'Salesperson'),(821,'Salesperson'),(822,'Salesperson'),(823,'Salesperson'),(824,'Salesperson'),(825,'Salesperson'),(826,'Salesperson'),(827,'Salesperson'),(828,'Salesperson'),(829,'Salesperson'),(830,'Salesperson'),(831,'Salesperson'),(832,'Salesperson'),(833,'Salesperson'),(834,'Salesperson'),(835,'Salesperson'),(836,'Salesperson'),(837,'Salesperson'),(838,'Salesperson'),(839,'Salesperson'),(840,'Salesperson'),(841,'Salesperson'),(842,'Salesperson'),(843,'Salesperson'),(844,'Salesperson'),(845,'Salesperson'),(846,'Salesperson'),(847,'Salesperson'),(848,'Salesperson'),(849,'Salesperson'),(850,'Salesperson'),(851,'Salesperson'),(852,'Salesperson'),(853,'Salesperson'),(854,'Salesperson'),(855,'Salesperson'),(856,'Salesperson'),(857,'Salesperson'),(858,'Salesperson'),(859,'Salesperson'),(860,'Salesperson'),(861,'Salesperson'),(862,'Salesperson'),(863,'Salesperson'),(864,'Salesperson'),(865,'Salesperson'),(866,'Salesperson'),(867,'Salesperson'),(868,'Salesperson'),(869,'Salesperson'),(870,'Salesperson'),(871,'Salesperson'),(872,'Salesperson'),(873,'Salesperson'),(874,'Salesperson'),(875,'Salesperson'),(876,'Salesperson'),(877,'Salesperson'),(878,'Salesperson'),(879,'Salesperson'),(880,'Salesperson'),(881,'Salesperson'),(882,'Salesperson'),(883,'Salesperson'),(884,'Salesperson'),(885,'Salesperson'),(886,'Salesperson'),(887,'Salesperson'),(888,'Salesperson'),(889,'Salesperson'),(890,'Salesperson'),(891,'Salesperson'),(892,'Salesperson'),(893,'Salesperson'),(894,'Salesperson'),(895,'Salesperson'),(896,'Salesperson'),(897,'Salesperson'),(898,'Salesperson'),(899,'Salesperson'),(900,'Salesperson'),(901,'Salesperson'),(902,'Salesperson'),(903,'Salesperson'),(904,'Salesperson'),(905,'Salesperson'),(906,'Salesperson'),(907,'Salesperson'),(908,'Salesperson'),(909,'Salesperson'),(910,'Salesperson'),(911,'Salesperson'),(912,'Salesperson'),(913,'Salesperson'),(914,'Salesperson'),(915,'Salesperson'),(916,'Salesperson'),(917,'Salesperson'),(918,'Salesperson'),(919,'Salesperson'),(920,'Salesperson'),(921,'Salesperson'),(922,'Salesperson'),(923,'Product Manager'),(924,'Consultant'),(925,'Product Manager'),(926,'Analyst'),(927,'Project Manager'),(928,'Analyst'),(929,'Project Manager'),(930,'Consultant'),(931,'Administrative Assistant'),(932,'Administrative Assistant'),(933,'Analyst'),(934,'Administrative Assistant'),(935,'Project Manager'),(936,'Administrative Assistant'),(937,'Project Manager'),(938,'Project Manager'),(939,'Administrative Assistant'),(940,'Product Manager'),(941,'Administrative Assistant'),(942,'Product Manager'),(943,'Administrative Assistant'),(944,'Consultant'),(945,'Project Manager'),(946,'Product Manager'),(947,'Consultant'),(948,'Consultant'),(949,'Consultant'),(950,'Project Manager'),(951,'Consultant'),(952,'Administrative Assistant'),(953,'Analyst'),(954,'Administrative Assistant'),(955,'Product Manager'),(956,'Consultant'),(957,'Analyst'),(958,'Product Manager'),(959,'Analyst'),(960,'Project Manager'),(961,'Product Manager'),(962,'Analyst'),(963,'Administrative Assistant'),(964,'Project Manager'),(965,'Product Manager'),(966,'Consultant'),(967,'Product Manager'),(968,'Consultant'),(969,'Administrative Assistant'),(970,'Product Manager'),(971,'Administrative Assistant'),(972,'Analyst'),(973,'Consultant'),(974,'Product Manager'),(975,'Project Manager'),(976,'Administrative Assistant'),(977,'Analyst'),(978,'Consultant'),(979,'Analyst'),(980,'Administrative Assistant'),(981,'Project Manager'),(982,'Analyst'),(983,'Consultant'),(984,'Consultant'),(985,'Consultant'),(986,'Attorney'),(987,'Attorney'),(988,'Attorney'),(989,'Paralegal'),(990,'Project Manager'),(991,'Attorney'),(992,'Paralegal'),(993,'Attorney'),(994,'Product Manager'),(995,'Consultant'),(996,'Paralegal'),(997,'Paralegal'),(998,'Attorney'),(999,'Paralegal'),(1000,'Attorney'),(1001,'Attorney'),(1002,'Attorney'),(1003,'Paralegal'),(1004,'Paralegal'),(1005,'Attorney'),(1006,'Paralegal'),(1007,'Attorney'),(1008,'Attorney'),(1009,'Attorney'),(1010,'Paralegal'),(1011,'Attorney'),(1012,'Paralegal'),(1013,'Paralegal'),(1014,'Paralegal'),(1015,'Paralegal'),(1016,'Paralegal'),(1017,'Paralegal'),(1018,'Attorney'),(1019,'Paralegal'),(1020,'Attorney'),(1021,'Attorney'),(1022,'Paralegal'),(1023,'Attorney'),(1024,'Paralegal'),(1025,'Attorney'),(1026,'Attorney'),(1027,'Paralegal'),(1028,'Attorney'),(1029,'Attorney'),(1030,'Paralegal'),(1031,'Attorney'),(1032,'Attorney'),(1033,'Paralegal'),(1034,'Paralegal'),(1035,'Attorney'),(1036,'Attorney'),(1037,'Analyst'),(1038,'Project Manager'),(1039,'Developer'),(1040,'Developer'),(1041,'Developer'),(1042,'Developer'),(1043,'Administrative Assistant'),(1044,'Developer'),(1045,'Developer'),(1046,'Developer'),(1047,'Developer'),(1048,'Developer'),(1049,'Developer'),(1050,'Administrative Assistant'),(1051,'Analyst'),(1052,'Developer'),(1053,'Developer'),(1054,'Project Manager'),(1055,'Developer'),(1056,'Developer'),(1057,'Developer'),(1058,'Developer'),(1059,'Developer'),(1060,'Project Manager'),(1061,'Developer'),(1062,'Developer'),(1063,'Developer'),(1064,'Developer'),(1065,'Developer'),(1066,'Developer'),(1067,'Developer'),(1068,'Product Manager'),(1069,'Developer'),(1070,'Developer'),(1071,'Developer'),(1072,'Developer'),(1073,'Developer'),(1074,'Developer'),(1075,'Developer'),(1076,'Developer'),(1077,'Developer'),(1078,'Administrative Assistant'),(1079,'Analyst'),(1080,'Developer'),(1081,'Developer'),(1082,'Developer'),(1083,'Developer'),(1084,'Developer'),(1085,'Product Manager'),(1086,'Developer'),(1087,'Developer'),(1088,'Analyst'),(1089,'Developer'),(1090,'Developer'),(1091,'Developer'),(1092,'Analyst'),(1093,'Analyst'),(1094,'Product Manager'),(1095,'Analyst'),(1096,'Developer'),(1097,'Product Manager'),(1098,'Developer'),(1099,'Developer'),(1100,'Developer'),(1101,'Developer'),(1102,'Developer'),(1103,'Developer'),(1104,'Developer'),(1105,'Developer'),(1106,'Consultant'),(1107,'Developer'),(1108,'Developer'),(1109,'Developer'),(1110,'Developer'),(1111,'Developer'),(1112,'Developer'),(1113,'Developer'),(1114,'Developer'),(1115,'Developer'),(1116,'Developer'),(1117,'Developer'),(1118,'Developer'),(1119,'Developer'),(1120,'Project Manager'),(1121,'Developer'),(1122,'Administrative Assistant'),(1123,'Developer'),(1124,'Developer'),(1125,'Developer'),(1126,'Developer'),(1127,'Developer'),(1128,'Administrative Assistant'),(1129,'Developer'),(1130,'Product Manager'),(1131,'Project Manager'),(1132,'Analyst'),(1133,'Developer'),(1134,'Developer'),(1135,'Developer'),(1136,'Developer'),(1137,'Developer'),(1138,'Developer'),(1139,'Consultant'),(1140,'Developer'),(1141,'Developer'),(1142,'Analyst'),(1143,'Developer'),(1144,'Developer'),(1145,'Developer'),(1146,'Project Manager'),(1147,'Developer'),(1148,'Developer'),(1149,'Product Manager'),(1150,'Project Manager'),(1151,'Developer'),(1152,'Developer'),(1153,'Developer'),(1154,'Product Manager'),(1155,'Project Manager'),(1156,'Product Manager'),(1157,'Developer'),(1158,'Developer'),(1159,'Developer'),(1160,'Developer'),(1161,'Developer'),(1162,'Administrative Assistant'),(1163,'Analyst'),(1164,'Consultant'),(1165,'Project Manager'),(1166,'Analyst'),(1167,'Product Manager'),(1168,'Consultant'),(1169,'Project Manager'),(1170,'Administrative Assistant'),(1171,'Analyst'),(1172,'Consultant'),(1173,'Consultant'),(1174,'Analyst'),(1175,'Consultant'),(1176,'Project Manager'),(1177,'Analyst'),(1178,'Analyst'),(1179,'Product Manager'),(1180,'Analyst'),(1181,'Analyst'),(1182,'Administrative Assistant'),(1183,'Administrative Assistant'),(1184,'Analyst'),(1185,'Product Manager'),(1186,'Product Manager'),(1187,'Project Manager'),(1188,'Consultant'),(1189,'Product Manager'),(1190,'Product Manager'),(1191,'Project Manager'),(1192,'Product Manager'),(1193,'Analyst'),(1194,'Analyst'),(1195,'Administrative Assistant'),(1196,'Analyst'),(1197,'Analyst'),(1198,'Project Manager'),(1199,'Analyst'),(1200,'Consultant'),(1201,'Administrative Assistant'),(1202,'Consultant'),(1203,'Administrative Assistant'),(1204,'Administrative Assistant'),(1205,'Product Manager'),(1206,'Product Manager'),(1207,'Analyst'),(1208,'Product Manager'),(1209,'Product Manager'),(1210,'Analyst'),(1211,'Consultant'),(1212,'Administrative Assistant');
/*!40000 ALTER TABLE `deskjob` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2019-03-01 16:29:51
