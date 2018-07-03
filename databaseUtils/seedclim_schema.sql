-- MySQL dump 10.13  Distrib 5.5.59, for debian-linux-gnu (x86_64)
--
-- Host: localhost    Database: seedclimComm
-- ------------------------------------------------------
-- Server version	5.5.59-0ubuntu0.14.04.1


--
-- Table structure for table `blocks`
--

CREATE TABLE `blocks` (
  `blockID` varchar(20) NOT NULL DEFAULT '',
  `siteID` varchar(40) DEFAULT NULL,
  `slope` float DEFAULT NULL,
  `aspect` float DEFAULT NULL,
  `comment` tinytext,
  PRIMARY KEY (`blockID`),
  KEY `siteID` (`siteID`),
  CONSTRAINT `blocks_ibfk_1` FOREIGN KEY (`siteID`) REFERENCES `sites` (`siteID`),
  CONSTRAINT `blocks_ibfk_2` FOREIGN KEY (`siteID`) REFERENCES `sites` (`siteID`),
  CONSTRAINT `blocks_ibfk_3` FOREIGN KEY (`siteID`) REFERENCES `sites` (`siteID`)
) 

--
-- Table structure for table `mergedictionary`
--

CREATE TABLE `mergedictionary` (
  `oldID` varchar(510) NOT NULL DEFAULT '',
  `newID` varchar(510) DEFAULT NULL,
  PRIMARY KEY (`oldID`)
) 

--
-- Table structure for table `metaData`
--

CREATE TABLE `metaData` (
  `tableName` varchar(100) DEFAULT NULL,
  `column` varchar(100) DEFAULT NULL,
  `units` varchar(100) DEFAULT NULL
) 

--
-- Table structure for table `moreTraits`
--

CREATE TABLE `moreTraits` (
  `species` varchar(510) DEFAULT NULL,
  `Norwegian name` varchar(510) DEFAULT NULL,
  `Polyploid (2n)` float DEFAULT NULL,
  `Flowering start` varchar(510) DEFAULT NULL,
  `F-Vår` float DEFAULT NULL,
  `F-Fso` float DEFAULT NULL,
  `F-Mso` float DEFAULT NULL,
  `F-Sso` float DEFAULT NULL,
  `F-Hø` float DEFAULT NULL,
  `Flowering finish` varchar(510) DEFAULT NULL,
  `Min height` float DEFAULT NULL,
  `Max height` float DEFAULT NULL,
  `Lower` varchar(510) DEFAULT NULL,
  `Nem` float DEFAULT NULL,
  `BNem` float DEFAULT NULL,
  `SBor` float DEFAULT NULL,
  `MBor` float DEFAULT NULL,
  `Nbor` float DEFAULT NULL,
  `LAlp` float DEFAULT NULL,
  `MAlp` float DEFAULT NULL,
  `HAlp` varchar(510) DEFAULT NULL,
  `Upper` varchar(510) DEFAULT NULL,
  `Habitat` varchar(510) DEFAULT NULL,
  `grassland` float DEFAULT NULL,
  `forest` float DEFAULT NULL,
  `rocky` float DEFAULT NULL,
  `wetland` varchar(510) DEFAULT NULL,
  `Soil type` varchar(510) DEFAULT NULL,
  `Common-rear` varchar(510) DEFAULT NULL,
  `Lids page` float DEFAULT NULL,
  `Mossberg page` varchar(510) DEFAULT NULL,
  `comment` varchar(510) DEFAULT NULL
)

--
-- Table structure for table `plots`
--

CREATE TABLE `plots` (
  `plotID` int(11) NOT NULL DEFAULT '0',
  `blockID` varchar(40) DEFAULT NULL,
  `aspect` float DEFAULT NULL,
  `slope` float DEFAULT NULL,
  PRIMARY KEY (`plotID`),
  KEY `blockID` (`blockID`),
  CONSTRAINT `plots_ibfk_1` FOREIGN KEY (`blockID`) REFERENCES `blocks` (`blockID`),
  CONSTRAINT `plots_ibfk_2` FOREIGN KEY (`blockID`) REFERENCES `blocks` (`blockID`)
);

--
-- Table structure for table `sites`
--

CREATE TABLE `sites` (
  `siteID` varchar(40) NOT NULL DEFAULT '',
  `norwegianName` varchar(40) DEFAULT NULL,
  `siteCode` varchar(12) DEFAULT NULL,
  `latitude` float DEFAULT NULL,
  `longitude` float DEFAULT NULL,
  `x_UTM33_North` float DEFAULT NULL,
  `y_UTM33_north` int(11) DEFAULT NULL,
  `altitude(DEM)` int(11) DEFAULT NULL,
  `Annualprecipitation_gridded` float DEFAULT NULL,
  `Temperature_level` int(11) DEFAULT NULL,
  `SummerTemperature_gridded` float DEFAULT NULL,
  `Precipitation_level` int(11) DEFAULT NULL,
  `geology` varchar(100) DEFAULT NULL,
  `landUse` varchar(100) DEFAULT NULL,
  `aspect` float DEFAULT NULL,
  `solar_radiation(DEM)(mw/hour/m^2)` float DEFAULT NULL,
  `Total N (red+oxi)                           (mg N/m2 yr)` int(11) DEFAULT NULL,
  `slope` float DEFAULT NULL,
  PRIMARY KEY (`siteID`)
) 

--
-- Table structure for table `soil_data`
--

CREATE TABLE `soil_data` (
  `originPlotID` float DEFAULT NULL,
  `destinationPlotID` float DEFAULT NULL,
  `water_cont` float DEFAULT NULL,
  `LOI` float DEFAULT NULL
) 

--
-- Table structure for table `soil_moist`
--

CREATE TABLE `soil_moist` (
  `originPlotID` int(11) DEFAULT NULL,
  `Year` float DEFAULT NULL,
  `moisture` float DEFAULT NULL
) 

--
-- Table structure for table `subTurfCommunity`
--

CREATE TABLE `subTurfCommunity` (
  `turfID` varchar(40) NOT NULL DEFAULT '',
  `subTurf` int(11) NOT NULL DEFAULT '0',
  `year` int(11) NOT NULL DEFAULT '0',
  `species` varchar(100) NOT NULL DEFAULT '',
  `seedlings` int(11) DEFAULT NULL,
  `juvenile` int(11) DEFAULT NULL,
  `adult` char(1) NOT NULL,
  `fertile` char(1) NOT NULL,
  `vegetative` char(1) NOT NULL,
  `dominant` char(1) NOT NULL,
  `cf` char(1) NOT NULL,
  PRIMARY KEY (`turfID`,`subTurf`,`year`,`species`),
  CONSTRAINT `subTurfCommunity_ibfk_1` FOREIGN KEY (`turfID`) REFERENCES `turfs` (`turfID`),
  CONSTRAINT `subTurfCommunity_ibfk_2` FOREIGN KEY (`turfID`) REFERENCES `turfs` (`turfID`)
)

--
-- Table structure for table `subTurfEnvironment`
--

CREATE TABLE `subTurfEnvironment` (
  `turfID` varchar(100) NOT NULL DEFAULT '',
  `subTurf` int(11) NOT NULL DEFAULT '0',
  `year` int(11) NOT NULL DEFAULT '0',
  `pleuro` int(11) DEFAULT NULL,
  `acro` int(11) DEFAULT NULL,
  `liver` int(11) DEFAULT NULL,
  `lichen` int(11) DEFAULT NULL,
  `litter` int(11) DEFAULT NULL,
  `soil` int(11) DEFAULT NULL,
  `rock` int(11) DEFAULT NULL,
  `comment` varchar(510) DEFAULT NULL,
  `bad` varchar(2) DEFAULT NULL,
  PRIMARY KEY (`turfID`,`subTurf`,`year`),
  CONSTRAINT `subTurfEnvironment_ibfk_1` FOREIGN KEY (`turfID`) REFERENCES `turfs` (`turfID`),
  CONSTRAINT `subTurfEnvironment_ibfk_2` FOREIGN KEY (`turfID`) REFERENCES `turfs` (`turfID`)
) 

--
-- Table structure for table `taxon`
--

CREATE TABLE `taxon` (
  `species` varchar(510) NOT NULL DEFAULT '',
  `speciesName` varchar(510) DEFAULT NULL,
  `family` varchar(510) DEFAULT NULL,
  `functionalGroup` varchar(510) DEFAULT NULL,
  `lifeSpan` varchar(510) DEFAULT NULL,
  `comment` varchar(510) DEFAULT NULL,
  `height` float DEFAULT NULL,
  `leafSize` float DEFAULT NULL,
  `seedMass` float DEFAULT NULL,
  `SLA` float DEFAULT NULL,
  PRIMARY KEY (`species`)
)

--
-- Table structure for table `turfCommunity`
--

CREATE TABLE `turfCommunity` (
  `turfID` varchar(40) NOT NULL DEFAULT '',
  `year` int(11) NOT NULL DEFAULT '0',
  `species` varchar(100) NOT NULL DEFAULT '',
  `cover` float DEFAULT NULL,
  `cf` char(1) NOT NULL,
  PRIMARY KEY (`turfID`,`year`,`species`),
  CONSTRAINT `turfCommunity_ibfk_1` FOREIGN KEY (`turfID`) REFERENCES `turfs` (`turfID`),
  CONSTRAINT `turfCommunity_ibfk_2` FOREIGN KEY (`turfID`) REFERENCES `turfs` (`turfID`)
)
--
-- Table structure for table `turfEnvironment`
--

CREATE TABLE `turfEnvironment` (
  `turfID` varchar(100) NOT NULL DEFAULT '',
  `year` int(11) NOT NULL DEFAULT '0',
  `pleuro` float DEFAULT NULL,
  `acro` float DEFAULT NULL,
  `liver` float DEFAULT NULL,
  `lichen` float DEFAULT NULL,
  `litter` float DEFAULT NULL,
  `soil` float DEFAULT NULL,
  `rock` float DEFAULT NULL,
  `totalVascular` float DEFAULT NULL,
  `totalBryophytes` float DEFAULT NULL,
  `totalLichen` float DEFAULT NULL,
  `vegetationHeight` float DEFAULT NULL,
  `mossHeight` float DEFAULT NULL,
  `comment` varchar(510) DEFAULT NULL,
  `recorder` varchar(100) DEFAULT NULL,
  `date` varchar(100) DEFAULT NULL,
  PRIMARY KEY (`turfID`,`year`),
  CONSTRAINT `turfEnvironment_ibfk_1` FOREIGN KEY (`turfID`) REFERENCES `turfs` (`turfID`),
  CONSTRAINT `turfEnvironment_ibfk_2` FOREIGN KEY (`turfID`) REFERENCES `turfs` (`turfID`)
)

--
-- Table structure for table `turfs`
--

CREATE TABLE `turfs` (
  `turfID` varchar(40) NOT NULL DEFAULT '',
  `TTtreat` varchar(10) DEFAULT NULL,
  `RTtreat` varchar(10) DEFAULT NULL,
  `GRtreat` varchar(10) DEFAULT NULL,
  `originPlotID` int(11) DEFAULT NULL,
  `destinationPlotID` int(11) DEFAULT NULL,
  PRIMARY KEY (`turfID`),
  KEY `originPlotID` (`originPlotID`),
  KEY `destinationPlotID` (`destinationPlotID`),
  CONSTRAINT `turfs_ibfk_1` FOREIGN KEY (`originPlotID`) REFERENCES `plots` (`plotID`),
  CONSTRAINT `turfs_ibfk_2` FOREIGN KEY (`destinationPlotID`) REFERENCES `plots` (`plotID`),
  CONSTRAINT `turfs_ibfk_3` FOREIGN KEY (`originPlotID`) REFERENCES `plots` (`plotID`),
  CONSTRAINT `turfs_ibfk_4` FOREIGN KEY (`destinationPlotID`) REFERENCES `plots` (`plotID`)
) 
