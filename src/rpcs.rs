use std::collections::BTreeMap;

lazy_static! {
    pub(crate) static ref SERVICE_INFO: BTreeMap<&'static str, BTreeMap<&'static str, &'static [&'static str]>> = {
        let mut services = BTreeMap::new();
        services.insert("AirQuality", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("aqi", ["latitude", "longitude"].as_slice());
            rpcs.insert("qualityIndex", ["latitude", "longitude"].as_slice());
            rpcs.insert("qualityIndexByZipCode", ["zipCode"].as_slice());
            rpcs
        });
        services.insert("Alcohol Consumption", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAllCountryValues", [].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getValueLitresPerCapita15", ["Country"].as_slice());
            rpcs.insert("getYEA", ["Country"].as_slice());
            rpcs.insert("getYear", ["Country"].as_slice());
            rpcs
        });
        services.insert("Alexa", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("createSkill", ["configuration"].as_slice());
            rpcs.insert("deleteSkill", ["ID"].as_slice());
            rpcs.insert("getSkill", ["ID"].as_slice());
            rpcs.insert("getSkillCategories", [].as_slice());
            rpcs.insert("getSlotTypes", [].as_slice());
            rpcs.insert("invokeSkill", ["ID", "utterance"].as_slice());
            rpcs.insert("listSkills", [].as_slice());
            rpcs.insert("updateSkill", ["ID", "configuration"].as_slice());
            rpcs
        });
        services.insert("Autograders", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("createAutograder", ["configuration"].as_slice());
            rpcs.insert("getAutograderConfig", ["name"].as_slice());
            rpcs.insert("getAutograders", [].as_slice());
            rpcs
        });
        services.insert("Avoidable Death", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAllVariableValues", [].as_slice());
            rpcs.insert("getCountry", ["Variable"].as_slice());
            rpcs.insert("getDeathPer100000", ["Variable"].as_slice());
            rpcs.insert("getMeasure", ["Variable"].as_slice());
            rpcs.insert("getYear", ["Variable"].as_slice());
            rpcs
        });
        services.insert("BaseX", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("command", ["url", "command", "username", "password"].as_slice());
            rpcs.insert("query", ["url", "database", "query", "username", "password"].as_slice());
            rpcs
        });
        services.insert("Battleship", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("allShips", [].as_slice());
            rpcs.insert("fire", ["row", "column"].as_slice());
            rpcs.insert("placeShip", ["ship", "row", "column", "facing"].as_slice());
            rpcs.insert("remainingShips", ["roleID"].as_slice());
            rpcs.insert("reset", [].as_slice());
            rpcs.insert("shipLength", ["ship"].as_slice());
            rpcs.insert("start", [].as_slice());
            rpcs
        });
        services.insert("CARES data", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("get1222021", ["Well A"].as_slice());
            rpcs.insert("get172", ["Well A"].as_slice());
            rpcs.insert("getAllWellAValues", [].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs
        });
        services.insert("COVID-19", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getConfirmedCounts", ["country", "state", "city"].as_slice());
            rpcs.insert("getDeathCounts", ["country", "state", "city"].as_slice());
            rpcs.insert("getLocationCoordinates", ["country", "state", "city"].as_slice());
            rpcs.insert("getLocationsWithData", [].as_slice());
            rpcs.insert("getRecoveredCounts", ["country", "state", "city"].as_slice());
            rpcs.insert("getVaccinationCategories", [].as_slice());
            rpcs.insert("getVaccinationCountries", [].as_slice());
            rpcs.insert("getVaccinationData", ["country", "state", "category", "startDate", "endDate"].as_slice());
            rpcs.insert("getVaccinationStates", [].as_slice());
            rpcs
        });
        services.insert("COVID-19 Estimated ICU Beds", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAllStateValues", [].as_slice());
            rpcs.insert("getCollectionDate", ["state"].as_slice());
            rpcs.insert("getCountLL", ["state"].as_slice());
            rpcs.insert("getCountUL", ["state"].as_slice());
            rpcs.insert("getGeocodedStateForState", ["state"].as_slice());
            rpcs.insert("getPercentageLL", ["state"].as_slice());
            rpcs.insert("getPercentageOfStaffedAdultICUBedsOccupiedEstimated", ["state"].as_slice());
            rpcs.insert("getPercentageUL", ["state"].as_slice());
            rpcs.insert("getStaffedAdultICUBedsOccupiedEstimated", ["state"].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getTotalLL", ["state"].as_slice());
            rpcs.insert("getTotalStaffedAdultICUBeds", ["state"].as_slice());
            rpcs.insert("getTotalUL", ["state"].as_slice());
            rpcs
        });
        services.insert("COVID-19 ICU Bed", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAllStateValues", [].as_slice());
            rpcs.insert("getCollectionDate", ["state"].as_slice());
            rpcs.insert("getPercentageOfStaffedAdultICUBedsOccupiedEstimated", ["state"].as_slice());
            rpcs.insert("getStaffedAdultICUBedsOccupiedEstimated", ["state"].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getTotalStaffedAdultICUBeds", ["state"].as_slice());
            rpcs
        });
        services.insert("CS PhDs By Ethnicity", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAsianByYear", [].as_slice());
            rpcs.insert("getAsianColumn", [].as_slice());
            rpcs.insert("getBlackByYear", [].as_slice());
            rpcs.insert("getBlackColumn", [].as_slice());
            rpcs.insert("getHispanicByYear", [].as_slice());
            rpcs.insert("getHispanicColumn", [].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getValue", ["Year", "column name"].as_slice());
            rpcs.insert("getWhiteByYear", [].as_slice());
            rpcs.insert("getWhiteColumn", [].as_slice());
            rpcs.insert("getYearColumn", [].as_slice());
            rpcs
        });
        services.insert("Cape Grim CO2", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAllCO2PpmValues", [].as_slice());
            rpcs.insert("getAllDATEValues", [].as_slice());
            rpcs.insert("getCO2Ppm", ["DATE"].as_slice());
            rpcs.insert("getRecord", ["DATE"].as_slice());
            rpcs
        });
        services.insert("Chart", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("defaultOptions", [].as_slice());
            rpcs.insert("draw", ["lines", "options"].as_slice());
            rpcs.insert("drawBarChart", ["dataset", "xAxisTag", "yAxisTag", "datasetTag", "title"].as_slice());
            rpcs.insert("drawLineChart", ["dataset", "xAxisTag", "yAxisTag", "datasetTag", "title"].as_slice());
            rpcs
        });
        services.insert("Climate Forcings", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAerosolsByYear", [].as_slice());
            rpcs.insert("getAerosolsColumn", [].as_slice());
            rpcs.insert("getAllByYear", [].as_slice());
            rpcs.insert("getAllColumn", [].as_slice());
            rpcs.insert("getAnnualMeanByYear", [].as_slice());
            rpcs.insert("getAnnualMeanColumn", [].as_slice());
            rpcs.insert("getGreenhouseGasByYear", [].as_slice());
            rpcs.insert("getGreenhouseGasColumn", [].as_slice());
            rpcs.insert("getHumanByYear", [].as_slice());
            rpcs.insert("getHumanColumn", [].as_slice());
            rpcs.insert("getLandUseByYear", [].as_slice());
            rpcs.insert("getLandUseColumn", [].as_slice());
            rpcs.insert("getNaturalByYear", [].as_slice());
            rpcs.insert("getNaturalColumn", [].as_slice());
            rpcs.insert("getOrbitByYear", [].as_slice());
            rpcs.insert("getOrbitColumn", [].as_slice());
            rpcs.insert("getOzoneByYear", [].as_slice());
            rpcs.insert("getOzoneColumn", [].as_slice());
            rpcs.insert("getSolarByYear", [].as_slice());
            rpcs.insert("getSolarColumn", [].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getValue", ["Year", "column name"].as_slice());
            rpcs.insert("getVolcanismByYear", [].as_slice());
            rpcs.insert("getVolcanismColumn", [].as_slice());
            rpcs.insert("getYearColumn", [].as_slice());
            rpcs
        });
        services.insert("CloudVariables", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("deleteUserVariable", ["name"].as_slice());
            rpcs.insert("deleteVariable", ["name", "password"].as_slice());
            rpcs.insert("getUserVariable", ["name"].as_slice());
            rpcs.insert("getVariable", ["name", "password"].as_slice());
            rpcs.insert("lockVariable", ["name", "password"].as_slice());
            rpcs.insert("setUserVariable", ["name", "value"].as_slice());
            rpcs.insert("setVariable", ["name", "value", "password"].as_slice());
            rpcs.insert("unlockVariable", ["name", "password"].as_slice());
            rpcs
        });
        services.insert("ConnectN", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("isFullBoard", [].as_slice());
            rpcs.insert("isGameOver", [].as_slice());
            rpcs.insert("newGame", ["row", "column", "numDotsToConnect"].as_slice());
            rpcs.insert("play", ["row", "column"].as_slice());
            rpcs
        });
        services.insert("Coral Growth Anomaly", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getCoralGrowthAnomalyByYEAR", [].as_slice());
            rpcs.insert("getCoralGrowthAnomalyColumn", [].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getValue", ["YEAR", "column name"].as_slice());
            rpcs.insert("getYEARColumn", [].as_slice());
            rpcs
        });
        services.insert("CoreNLP", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("annotate", ["text", "annotators"].as_slice());
            rpcs.insert("getAnnotators", [].as_slice());
            rpcs
        });
        services.insert("EarthOrbit", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getEccentricity", ["startyear", "endyear"].as_slice());
            rpcs.insert("getInsolation", ["startyear", "endyear"].as_slice());
            rpcs.insert("getLongitude", ["startyear", "endyear"].as_slice());
            rpcs.insert("getObliquity", ["startyear", "endyear"].as_slice());
            rpcs.insert("getPrecession", ["startyear", "endyear"].as_slice());
            rpcs
        });
        services.insert("Earthquakes", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("byRegion", ["minLatitude", "maxLatitude", "minLongitude", "maxLongitude", "startTime", "endTime", "minMagnitude", "maxMagnitude"].as_slice());
            rpcs.insert("stop", [].as_slice());
            rpcs
        });
        services.insert("Eclipse2017", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("availableStations", ["maxReadingMedian", "maxDistanceFromCenter", "latitude", "longitude", "maxDistanceFromPoint"].as_slice());
            rpcs.insert("condition", ["stationId"].as_slice());
            rpcs.insert("conditionHistory", ["stationId", "limit"].as_slice());
            rpcs.insert("conditionHistoryRange", ["stationId", "startTime", "endTime"].as_slice());
            rpcs.insert("eclipsePath", [].as_slice());
            rpcs.insert("pastCondition", ["stationId", "time"].as_slice());
            rpcs.insert("pastTemperature", ["stationId", "time"].as_slice());
            rpcs.insert("selectPointBased", [].as_slice());
            rpcs.insert("selectSectionBased", ["numSections", "perSection"].as_slice());
            rpcs.insert("stationInfo", ["stationId"].as_slice());
            rpcs.insert("stations", [].as_slice());
            rpcs.insert("stationsInfo", [].as_slice());
            rpcs.insert("temperature", ["stationId"].as_slice());
            rpcs.insert("temperatureHistory", ["stationId", "limit"].as_slice());
            rpcs.insert("temperatureHistoryRange", ["stationId", "startTime", "endTime"].as_slice());
            rpcs
        });
        services.insert("Execute", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("call", ["fn"].as_slice());
            rpcs
        });
        services.insert("Fairbanks Weather", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAllDATEValues", [].as_slice());
            rpcs.insert("getAllPRCPValues", [].as_slice());
            rpcs.insert("getAllSNOWValues", [].as_slice());
            rpcs.insert("getAllTMAXValues", [].as_slice());
            rpcs.insert("getAllTMINValues", [].as_slice());
            rpcs.insert("getAllTOBSValues", [].as_slice());
            rpcs.insert("getPRCP", ["DATE"].as_slice());
            rpcs.insert("getRecord", ["DATE"].as_slice());
            rpcs.insert("getSNOW", ["DATE"].as_slice());
            rpcs.insert("getTMAX", ["DATE"].as_slice());
            rpcs.insert("getTMIN", ["DATE"].as_slice());
            rpcs.insert("getTOBS", ["DATE"].as_slice());
            rpcs
        });
        services.insert("Franklin Temperatures", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAllDATEValues", [].as_slice());
            rpcs.insert("getAllTMAXValues", [].as_slice());
            rpcs.insert("getAllTMINValues", [].as_slice());
            rpcs.insert("getAllTOBSValues", [].as_slice());
            rpcs.insert("getRecord", ["DATE"].as_slice());
            rpcs.insert("getTMAX", ["DATE"].as_slice());
            rpcs.insert("getTMIN", ["DATE"].as_slice());
            rpcs.insert("getTOBS", ["DATE"].as_slice());
            rpcs
        });
        services.insert("GDPTrend", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getChinaByYear", [].as_slice());
            rpcs.insert("getChinaColumn", [].as_slice());
            rpcs.insert("getGermanyByYear", [].as_slice());
            rpcs.insert("getGermanyColumn", [].as_slice());
            rpcs.insert("getJapanByYear", [].as_slice());
            rpcs.insert("getJapanColumn", [].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getUnitedStatesByYear", [].as_slice());
            rpcs.insert("getUnitedStatesColumn", [].as_slice());
            rpcs.insert("getValue", ["Year", "column name"].as_slice());
            rpcs.insert("getYearColumn", [].as_slice());
            rpcs
        });
        services.insert("Genius", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getArtist", ["ID"].as_slice());
            rpcs.insert("getSong", ["ID"].as_slice());
            rpcs.insert("getSongLyrics", ["ID"].as_slice());
            rpcs.insert("getSongsByArtist", ["ID"].as_slice());
            rpcs.insert("searchSongs", ["query"].as_slice());
            rpcs
        });
        services.insert("Geolocation", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("city", ["latitude", "longitude"].as_slice());
            rpcs.insert("country", ["latitude", "longitude"].as_slice());
            rpcs.insert("countryCode", ["latitude", "longitude"].as_slice());
            rpcs.insert("county*", ["latitude", "longitude"].as_slice());
            rpcs.insert("geolocate", ["address"].as_slice());
            rpcs.insert("info", ["latitude", "longitude"].as_slice());
            rpcs.insert("nearbySearch", ["latitude", "longitude", "keyword", "radius"].as_slice());
            rpcs.insert("state*", ["latitude", "longitude"].as_slice());
            rpcs.insert("stateCode*", ["latitude", "longitude"].as_slice());
            rpcs.insert("streetAddress", ["address"].as_slice());
            rpcs.insert("timezone", ["address"].as_slice());
            rpcs
        });
        services.insert("GlobalLandTemperature", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("get10YearAverage", ["Date"].as_slice());
            rpcs.insert("getAll10YearAverageValues", [].as_slice());
            rpcs.insert("getAllAnnualValues", [].as_slice());
            rpcs.insert("getAllDateValues", [].as_slice());
            rpcs.insert("getAllMonthlyValues", [].as_slice());
            rpcs.insert("getAnnual", ["Date"].as_slice());
            rpcs.insert("getMonthly", ["Date"].as_slice());
            rpcs.insert("getRecord", ["Date"].as_slice());
            rpcs
        });
        services.insert("GoogleMaps", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getDistance", ["startLatitude", "startLongitude", "endLatitude", "endLongitude"].as_slice());
            rpcs.insert("getEarthCoordinates", ["x", "y"].as_slice());
            rpcs.insert("getImageCoordinates", ["latitude", "longitude"].as_slice());
            rpcs.insert("getLatitude", ["y"].as_slice());
            rpcs.insert("getLatitudeFromY", ["y"].as_slice());
            rpcs.insert("getLongitude", ["x"].as_slice());
            rpcs.insert("getLongitudeFromX", ["x"].as_slice());
            rpcs.insert("getMap", ["latitude", "longitude", "width", "height", "zoom"].as_slice());
            rpcs.insert("getSatelliteMap", ["latitude", "longitude", "width", "height", "zoom"].as_slice());
            rpcs.insert("getTerrainMap", ["latitude", "longitude", "width", "height", "zoom"].as_slice());
            rpcs.insert("getXFromLongitude", ["longitude"].as_slice());
            rpcs.insert("getYFromLatitude", ["latitude"].as_slice());
            rpcs.insert("maxLatitude", [].as_slice());
            rpcs.insert("maxLongitude", [].as_slice());
            rpcs.insert("minLatitude", [].as_slice());
            rpcs.insert("minLongitude", [].as_slice());
            rpcs
        });
        services.insert("GoogleStreetView", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getInfo", ["latitude", "longitude", "fieldofview", "heading", "pitch"].as_slice());
            rpcs.insert("getInfoFromAddress", ["location", "fieldofview", "heading", "pitch"].as_slice());
            rpcs.insert("getView", ["latitude", "longitude", "width", "height", "fieldofview", "heading", "pitch"].as_slice());
            rpcs.insert("getViewFromAddress", ["location", "width", "height", "fieldofview", "heading", "pitch"].as_slice());
            rpcs.insert("getViewFromLatLong", ["latitude", "longitude", "width", "height", "fieldofview", "heading", "pitch"].as_slice());
            rpcs.insert("isAvailable", ["latitude", "longitude", "fieldofview", "heading", "pitch"].as_slice());
            rpcs.insert("isAvailableFromAddress", ["location", "fieldofview", "heading", "pitch"].as_slice());
            rpcs
        });
        services.insert("HPV Vaccination", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAllSexValues", [].as_slice());
            rpcs.insert("getAnnualPercentChangeAPC", ["Sex"].as_slice());
            rpcs.insert("getAverageAnnualPercentChangeAAPC", ["Sex"].as_slice());
            rpcs.insert("getHealthyPeople2020TargetForSex", ["Sex"].as_slice());
            rpcs.insert("getPercent", ["Sex"].as_slice());
            rpcs.insert("getPercentTrendLine", ["Sex"].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getYear", ["Sex"].as_slice());
            rpcs
        });
        services.insert("Hangman", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getCurrentlyKnownWord", [].as_slice());
            rpcs.insert("getWrongCount", [].as_slice());
            rpcs.insert("guess", ["letter"].as_slice());
            rpcs.insert("isWordGuessed", [].as_slice());
            rpcs.insert("setWord", ["word"].as_slice());
            rpcs
        });
        services.insert("HistoricalTemperature", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("annualAnomaly", ["region"].as_slice());
            rpcs.insert("fiveYearAnomaly", ["region"].as_slice());
            rpcs.insert("monthlyAnomaly", ["region"].as_slice());
            rpcs.insert("tenYearAnomaly", ["region"].as_slice());
            rpcs.insert("twentyYearAnomaly", ["region"].as_slice());
            rpcs
        });
        services.insert("HumanMortalityDatabase", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAllData", [].as_slice());
            rpcs.insert("getAllDataForCountry", ["country"].as_slice());
            rpcs.insert("getCategories", [].as_slice());
            rpcs.insert("getCountries", [].as_slice());
            rpcs.insert("getGenders", [].as_slice());
            rpcs.insert("getTimeSeries", ["country", "gender", "category"].as_slice());
            rpcs
        });
        services.insert("HurricaneData", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getHurricaneData", ["name", "year"].as_slice());
            rpcs.insert("getHurricanesInYear", ["year"].as_slice());
            rpcs.insert("getYearsWithHurricaneNamed", ["name"].as_slice());
            rpcs
        });
        services.insert("IceCoreData", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getCarbonDioxideData", ["core", "startyear", "endyear"].as_slice());
            rpcs.insert("getDataAvailability", [].as_slice());
            rpcs.insert("getDelta18OData", ["core", "startyear", "endyear"].as_slice());
            rpcs.insert("getDeuteriumData", ["core", "startyear", "endyear"].as_slice());
            rpcs.insert("getIceCoreMetadata", ["core"].as_slice());
            rpcs.insert("getIceCoreNames", [].as_slice());
            rpcs.insert("getTemperatureData", ["core", "startyear", "endyear"].as_slice());
            rpcs
        });
        services.insert("KeyValueStore", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("child", ["key", "password"].as_slice());
            rpcs.insert("delete", ["key", "password"].as_slice());
            rpcs.insert("get", ["key", "password"].as_slice());
            rpcs.insert("parent", ["key"].as_slice());
            rpcs.insert("put", ["key", "value", "password"].as_slice());
            rpcs
        });
        services.insert("LandOceanTemperatureIndex", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAllAprValues", [].as_slice());
            rpcs.insert("getAllAugValues", [].as_slice());
            rpcs.insert("getAllDJFValues", [].as_slice());
            rpcs.insert("getAllDNValues", [].as_slice());
            rpcs.insert("getAllDecValues", [].as_slice());
            rpcs.insert("getAllFebValues", [].as_slice());
            rpcs.insert("getAllJDValues", [].as_slice());
            rpcs.insert("getAllJJAValues", [].as_slice());
            rpcs.insert("getAllJanValues", [].as_slice());
            rpcs.insert("getAllJulValues", [].as_slice());
            rpcs.insert("getAllJunValues", [].as_slice());
            rpcs.insert("getAllMAMValues", [].as_slice());
            rpcs.insert("getAllMarValues", [].as_slice());
            rpcs.insert("getAllMayValues", [].as_slice());
            rpcs.insert("getAllNovValues", [].as_slice());
            rpcs.insert("getAllOctValues", [].as_slice());
            rpcs.insert("getAllSONValues", [].as_slice());
            rpcs.insert("getAllSepValues", [].as_slice());
            rpcs.insert("getAllYearValues", [].as_slice());
            rpcs.insert("getApr", ["Year"].as_slice());
            rpcs.insert("getAug", ["Year"].as_slice());
            rpcs.insert("getDJF", ["Year"].as_slice());
            rpcs.insert("getDN", ["Year"].as_slice());
            rpcs.insert("getDec", ["Year"].as_slice());
            rpcs.insert("getFeb", ["Year"].as_slice());
            rpcs.insert("getJD", ["Year"].as_slice());
            rpcs.insert("getJJA", ["Year"].as_slice());
            rpcs.insert("getJan", ["Year"].as_slice());
            rpcs.insert("getJul", ["Year"].as_slice());
            rpcs.insert("getJun", ["Year"].as_slice());
            rpcs.insert("getMAM", ["Year"].as_slice());
            rpcs.insert("getMar", ["Year"].as_slice());
            rpcs.insert("getMay", ["Year"].as_slice());
            rpcs.insert("getNov", ["Year"].as_slice());
            rpcs.insert("getOct", ["Year"].as_slice());
            rpcs.insert("getRecord", ["Year"].as_slice());
            rpcs.insert("getSON", ["Year"].as_slice());
            rpcs.insert("getSep", ["Year"].as_slice());
            rpcs
        });
        services.insert("MaunaLoaCO2Data", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getCO2Trend", ["startyear", "endyear"].as_slice());
            rpcs.insert("getRawCO2", ["startyear", "endyear"].as_slice());
            rpcs
        });
        services.insert("MetMuseum", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("advancedSearch", ["field", "query", "skip", "limit"].as_slice());
            rpcs.insert("fields", [].as_slice());
            rpcs.insert("getImageUrls", ["id"].as_slice());
            rpcs.insert("getInfo", ["id"].as_slice());
            rpcs.insert("searchByArtistDisplayBio", ["query"].as_slice());
            rpcs.insert("searchByArtistDisplayName", ["query"].as_slice());
            rpcs.insert("searchByClassification", ["query"].as_slice());
            rpcs.insert("searchByCountry", ["query"].as_slice());
            rpcs.insert("searchByCreditLine", ["query"].as_slice());
            rpcs.insert("searchByDepartment", ["query"].as_slice());
            rpcs.insert("searchByDimensions", ["query"].as_slice());
            rpcs.insert("searchByIsHighlight", ["query"].as_slice());
            rpcs.insert("searchByMedium", ["query"].as_slice());
            rpcs.insert("searchByObjectDate", ["query"].as_slice());
            rpcs.insert("searchByObjectName", ["query"].as_slice());
            rpcs.insert("searchByRepository", ["query"].as_slice());
            rpcs.insert("searchByTitle", ["query"].as_slice());
            rpcs
        });
        services.insert("MovieDB", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getImage", ["path"].as_slice());
            rpcs.insert("movieBackdropPath", ["id"].as_slice());
            rpcs.insert("movieBudget", ["id"].as_slice());
            rpcs.insert("movieCastCharacters", ["id"].as_slice());
            rpcs.insert("movieCastNames", ["id"].as_slice());
            rpcs.insert("movieCastPersonIDs", ["id"].as_slice());
            rpcs.insert("movieCastProfilePaths", ["id"].as_slice());
            rpcs.insert("movieCrewJobs", ["id"].as_slice());
            rpcs.insert("movieCrewNames", ["id"].as_slice());
            rpcs.insert("movieCrewPersonIDs", ["id"].as_slice());
            rpcs.insert("movieCrewProfilePaths", ["id"].as_slice());
            rpcs.insert("movieGenres", ["id"].as_slice());
            rpcs.insert("movieOriginalLanguage", ["id"].as_slice());
            rpcs.insert("movieOriginalTitle", ["id"].as_slice());
            rpcs.insert("movieOverview", ["id"].as_slice());
            rpcs.insert("moviePopularity", ["id"].as_slice());
            rpcs.insert("moviePosterPath", ["id"].as_slice());
            rpcs.insert("movieProductionCompanies", ["id"].as_slice());
            rpcs.insert("movieProductionCountries", ["id"].as_slice());
            rpcs.insert("movieReleaseDate", ["id"].as_slice());
            rpcs.insert("movieRevenue", ["id"].as_slice());
            rpcs.insert("movieRuntime", ["id"].as_slice());
            rpcs.insert("movieSpokenLanguages", ["id"].as_slice());
            rpcs.insert("movieTagline", ["id"].as_slice());
            rpcs.insert("movieTitle", ["id"].as_slice());
            rpcs.insert("movieVoteAverage", ["id"].as_slice());
            rpcs.insert("movieVoteCount", ["id"].as_slice());
            rpcs.insert("personBiography", ["id"].as_slice());
            rpcs.insert("personBirthday", ["id"].as_slice());
            rpcs.insert("personCastCharacters", ["id"].as_slice());
            rpcs.insert("personCastMovieIDs", ["id"].as_slice());
            rpcs.insert("personCastOriginalTitles", ["id"].as_slice());
            rpcs.insert("personCastPosterPaths", ["id"].as_slice());
            rpcs.insert("personCastReleaseDates", ["id"].as_slice());
            rpcs.insert("personCastTitles", ["id"].as_slice());
            rpcs.insert("personCrewJobs", ["id"].as_slice());
            rpcs.insert("personCrewMovieIDs", ["id"].as_slice());
            rpcs.insert("personCrewOriginalTitles", ["id"].as_slice());
            rpcs.insert("personCrewPosterPaths", ["id"].as_slice());
            rpcs.insert("personCrewReleaseDates", ["id"].as_slice());
            rpcs.insert("personCrewTitles", ["id"].as_slice());
            rpcs.insert("personDeathday", ["id"].as_slice());
            rpcs.insert("personGender", ["id"].as_slice());
            rpcs.insert("personImageAspectRatios", ["id"].as_slice());
            rpcs.insert("personImageFilePaths", ["id"].as_slice());
            rpcs.insert("personImageHeights", ["id"].as_slice());
            rpcs.insert("personImageVoteCounts", ["id"].as_slice());
            rpcs.insert("personImageWidths", ["id"].as_slice());
            rpcs.insert("personName", ["id"].as_slice());
            rpcs.insert("personPlaceOfBirth", ["id"].as_slice());
            rpcs.insert("personPopularity", ["id"].as_slice());
            rpcs.insert("personProfilePath", ["id"].as_slice());
            rpcs.insert("searchMovie", ["title"].as_slice());
            rpcs.insert("searchPerson", ["name"].as_slice());
            rpcs
        });
        services.insert("NASA", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("apod", [].as_slice());
            rpcs.insert("apodDetails", [].as_slice());
            rpcs.insert("apodMedia", [].as_slice());
            rpcs.insert("marsHighTemp", [].as_slice());
            rpcs.insert("marsLowTemp", [].as_slice());
            rpcs.insert("marsWeather", [].as_slice());
            rpcs
        });
        services.insert("NPlayer", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("endTurn", ["next"].as_slice());
            rpcs.insert("getActive", [].as_slice());
            rpcs.insert("getN", [].as_slice());
            rpcs.insert("getNext", [].as_slice());
            rpcs.insert("getPrevious", [].as_slice());
            rpcs.insert("start", [].as_slice());
            rpcs
        });
        services.insert("NewYorkPublicLibrary", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getDetails", ["uuid"].as_slice());
            rpcs.insert("getImage", ["itemID"].as_slice());
            rpcs.insert("search", ["term", "perPage", "page"].as_slice());
            rpcs
        });
        services.insert("NewYorkTimes", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getArticleSections", [].as_slice());
            rpcs.insert("getArticlesWithConcept", ["concept"].as_slice());
            rpcs.insert("getBestSellerLists", [].as_slice());
            rpcs.insert("getBestSellers", ["list", "date"].as_slice());
            rpcs.insert("getConceptInfo", ["concept"].as_slice());
            rpcs.insert("getConceptTypes", [].as_slice());
            rpcs.insert("getCriticsPicks", ["offset"].as_slice());
            rpcs.insert("getLatestArticles", ["section"].as_slice());
            rpcs.insert("getMostEmailedArticles", ["period"].as_slice());
            rpcs.insert("getMostSharedArticles", ["period"].as_slice());
            rpcs.insert("getMostViewedArticles", ["period"].as_slice());
            rpcs.insert("getMovieCriticInfo", ["name"].as_slice());
            rpcs.insert("getMovieCritics", [].as_slice());
            rpcs.insert("getMovieReviews", ["offset"].as_slice());
            rpcs.insert("getMovieReviewsByCritic", ["critic", "offset"].as_slice());
            rpcs.insert("getTopBestSellers", ["date"].as_slice());
            rpcs.insert("getTopStories", ["section"].as_slice());
            rpcs.insert("searchArticles", ["query", "offset"].as_slice());
            rpcs.insert("searchBestSellers", ["title", "author", "offset"].as_slice());
            rpcs.insert("searchConcepts", ["query"].as_slice());
            rpcs.insert("searchMovieReviews", ["query", "offset"].as_slice());
            rpcs
        });
        services.insert("NexradRadar", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("listRadars", ["latitude", "longitude", "width", "height", "zoom"].as_slice());
            rpcs.insert("plotRadarImages", ["latitude", "longitude", "width", "height", "zoom", "mapType", "radars"].as_slice());
            rpcs
        });
        services.insert("OceanData", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getDeepOceanTemp", ["startYear", "endYear"].as_slice());
            rpcs.insert("getOxygenRatio", ["startYear", "endYear"].as_slice());
            rpcs.insert("getSeaLevel", ["startYear", "endYear"].as_slice());
            rpcs.insert("getSurfaceTemp", ["startYear", "endYear"].as_slice());
            rpcs
        });
        services.insert("PaleoceanOxygenIsotopes", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAverageSedimentationRates", ["startyear", "endyear"].as_slice());
            rpcs.insert("getDelta18O", ["startyear", "endyear"].as_slice());
            rpcs.insert("getDelta18OError", ["startyear", "endyear"].as_slice());
            rpcs.insert("getNormalizedSedimentationRates", ["startyear", "endyear"].as_slice());
            rpcs
        });
        services.insert("ParallelDots", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAbuse", ["text"].as_slice());
            rpcs.insert("getEmotion", ["text"].as_slice());
            rpcs.insert("getIntent", ["text"].as_slice());
            rpcs.insert("getKeywords", ["text"].as_slice());
            rpcs.insert("getNamedEntities", ["text"].as_slice());
            rpcs.insert("getSarcasmProbability", ["text"].as_slice());
            rpcs.insert("getSentiment", ["text"].as_slice());
            rpcs.insert("getSimilarity", ["text1", "text2"].as_slice());
            rpcs.insert("getTaxonomy", ["text"].as_slice());
            rpcs
        });
        services.insert("People", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAgeByName", [].as_slice());
            rpcs.insert("getAgeColumn", [].as_slice());
            rpcs.insert("getNameColumn", [].as_slice());
            rpcs.insert("getSexByName", [].as_slice());
            rpcs.insert("getSexColumn", [].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getValue", ["name", "column name"].as_slice());
            rpcs
        });
        services.insert("PhoneIoT", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("addButton", ["device", "x", "y", "width", "height", "text", "options"].as_slice());
            rpcs.insert("addImageDisplay", ["device", "x", "y", "width", "height", "options"].as_slice());
            rpcs.insert("addJoystick", ["device", "x", "y", "width", "options"].as_slice());
            rpcs.insert("addLabel", ["device", "x", "y", "text", "options"].as_slice());
            rpcs.insert("addRadioButton", ["device", "x", "y", "text", "options"].as_slice());
            rpcs.insert("addSlider", ["device", "x", "y", "width", "options"].as_slice());
            rpcs.insert("addTextField", ["device", "x", "y", "width", "height", "options"].as_slice());
            rpcs.insert("addToggle", ["device", "x", "y", "text", "options"].as_slice());
            rpcs.insert("addTouchpad", ["device", "x", "y", "width", "height", "options"].as_slice());
            rpcs.insert("authenticate", ["device"].as_slice());
            rpcs.insert("clearControls", ["device"].as_slice());
            rpcs.insert("getAccelerometer", ["device"].as_slice());
            rpcs.insert("getAltitude", ["device"].as_slice());
            rpcs.insert("getBearing", ["device"].as_slice());
            rpcs.insert("getColor", ["red", "green", "blue", "alpha"].as_slice());
            rpcs.insert("getCompassCardinalDirection", ["device"].as_slice());
            rpcs.insert("getCompassDirection", ["device"].as_slice());
            rpcs.insert("getCompassHeading", ["device"].as_slice());
            rpcs.insert("getFacingDirection", ["device"].as_slice());
            rpcs.insert("getGameRotation", ["device"].as_slice());
            rpcs.insert("getGravity", ["device"].as_slice());
            rpcs.insert("getGyroscope", ["device"].as_slice());
            rpcs.insert("getImage", ["device", "id"].as_slice());
            rpcs.insert("getJoystickVector", ["device", "id"].as_slice());
            rpcs.insert("getLevel", ["device", "id"].as_slice());
            rpcs.insert("getLightLevel", ["device"].as_slice());
            rpcs.insert("getLinearAcceleration", ["device"].as_slice());
            rpcs.insert("getLocation", ["device"].as_slice());
            rpcs.insert("getMagneticField", ["device"].as_slice());
            rpcs.insert("getMicrophoneLevel", ["device"].as_slice());
            rpcs.insert("getOrientation", ["device"].as_slice());
            rpcs.insert("getPosition", ["device", "id"].as_slice());
            rpcs.insert("getProximity", ["device"].as_slice());
            rpcs.insert("getRotation", ["device"].as_slice());
            rpcs.insert("getSensors", [].as_slice());
            rpcs.insert("getStepCount", ["device"].as_slice());
            rpcs.insert("getText", ["device", "id"].as_slice());
            rpcs.insert("getToggleState", ["device", "id"].as_slice());
            rpcs.insert("isPressed", ["device", "id"].as_slice());
            rpcs.insert("listenToGUI", ["device"].as_slice());
            rpcs.insert("listenToSensors", ["device", "sensors"].as_slice());
            rpcs.insert("magnitude", ["vec"].as_slice());
            rpcs.insert("normalize", ["vec"].as_slice());
            rpcs.insert("removeControl", ["device", "id"].as_slice());
            rpcs.insert("setCredentials", ["device", "password"].as_slice());
            rpcs.insert("setImage", ["device", "id", "img"].as_slice());
            rpcs.insert("setLevel", ["device", "id", "value"].as_slice());
            rpcs.insert("setText", ["device", "id", "text"].as_slice());
            rpcs.insert("setToggleState", ["device", "id", "state"].as_slice());
            rpcs
        });
        services.insert("Pixabay", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getImage", ["url"].as_slice());
            rpcs.insert("searchAll", ["keywords", "maxHeight", "minHeight"].as_slice());
            rpcs.insert("searchIllustration", ["keywords", "maxHeight", "minHeight"].as_slice());
            rpcs.insert("searchPhoto", ["keywords", "maxHeight", "minHeight"].as_slice());
            rpcs
        });
        services.insert("PositionSensor", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getDevices", [].as_slice());
            rpcs.insert("getHeading", ["id"].as_slice());
            rpcs.insert("getPosition", ["id"].as_slice());
            rpcs.insert("getX", ["id"].as_slice());
            rpcs.insert("getY", ["id"].as_slice());
            rpcs.insert("getZ", ["id"].as_slice());
            rpcs.insert("listen", ["id"].as_slice());
            rpcs
        });
        services.insert("ProjectGutenberg", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getInfo", ["ID"].as_slice());
            rpcs.insert("getText", ["ID"].as_slice());
            rpcs.insert("search", ["field", "text"].as_slice());
            rpcs
        });
        services.insert("PublicRoles", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getPublicRoleId", [].as_slice());
            rpcs.insert("requestPublicRoleId", [].as_slice());
            rpcs
        });
        services.insert("RoboScape", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("eavesdrop", ["robots"].as_slice());
            rpcs.insert("getRobots", [].as_slice());
            rpcs.insert("listen", ["robots"].as_slice());
            rpcs.insert("send", ["robot", "command"].as_slice());
            rpcs
        });
        services.insert("S1", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("get1222021", ["Well A"].as_slice());
            rpcs.insert("get172", ["Well A"].as_slice());
            rpcs.insert("getAllWellAValues", [].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs
        });
        services.insert("ServiceCreation", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("createServiceFromTable", ["name", "data", "options"].as_slice());
            rpcs.insert("deleteService", ["name"].as_slice());
            rpcs.insert("getCreateFromTableOptions", ["data"].as_slice());
            rpcs
        });
        services.insert("SimpleHangman", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getCurrentlyKnownWord", [].as_slice());
            rpcs.insert("getWrongCount", [].as_slice());
            rpcs.insert("guess", ["letter"].as_slice());
            rpcs.insert("isWordGuessed", [].as_slice());
            rpcs.insert("restart", ["word"].as_slice());
            rpcs
        });
        services.insert("Smithsonian", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getImage", ["id"].as_slice());
            rpcs.insert("search", ["term", "count", "skip"].as_slice());
            rpcs.insert("searchImageContent", ["term", "count", "skip"].as_slice());
            rpcs
        });
        services.insert("StarMap", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("arcHourMinSecToDeg", ["arcHour", "arcMin", "arcSec"].as_slice());
            rpcs.insert("findObject", ["name"].as_slice());
            rpcs.insert("getImage", ["right_ascension", "declination", "arcseconds_per_pixel", "options", "width", "height"].as_slice());
            rpcs
        });
        services.insert("StudentPerformance", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAllGenderValues", [].as_slice());
            rpcs.insert("getLunch", ["gender"].as_slice());
            rpcs.insert("getMathScore", ["gender"].as_slice());
            rpcs.insert("getParentalLevelOfEducation", ["gender"].as_slice());
            rpcs.insert("getRaceEthnicity", ["gender"].as_slice());
            rpcs.insert("getReadingScore", ["gender"].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getTestPreparationCourse", ["gender"].as_slice());
            rpcs.insert("getWritingScore", ["gender"].as_slice());
            rpcs
        });
        services.insert("SummerOlympicMedals", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAllYearValues", [].as_slice());
            rpcs.insert("getAthlete", ["Year"].as_slice());
            rpcs.insert("getCityForYear", ["Year"].as_slice());
            rpcs.insert("getCountry", ["Year"].as_slice());
            rpcs.insert("getDiscipline", ["Year"].as_slice());
            rpcs.insert("getEvent", ["Year"].as_slice());
            rpcs.insert("getGender", ["Year"].as_slice());
            rpcs.insert("getMedal", ["Year"].as_slice());
            rpcs.insert("getSport", ["Year"].as_slice());
            rpcs
        });
        services.insert("TemperatureDatabase", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAerosolsByYear", [].as_slice());
            rpcs.insert("getAerosolsColumn", [].as_slice());
            rpcs.insert("getAllByYear", [].as_slice());
            rpcs.insert("getAllColumn", [].as_slice());
            rpcs.insert("getAnnualMeanByYear", [].as_slice());
            rpcs.insert("getAnnualMeanColumn", [].as_slice());
            rpcs.insert("getGreenhouseGasByYear", [].as_slice());
            rpcs.insert("getGreenhouseGasColumn", [].as_slice());
            rpcs.insert("getHumanByYear", [].as_slice());
            rpcs.insert("getHumanColumn", [].as_slice());
            rpcs.insert("getLandUseByYear", [].as_slice());
            rpcs.insert("getLandUseColumn", [].as_slice());
            rpcs.insert("getNaturalByYear", [].as_slice());
            rpcs.insert("getNaturalColumn", [].as_slice());
            rpcs.insert("getOrbitByYear", [].as_slice());
            rpcs.insert("getOrbitColumn", [].as_slice());
            rpcs.insert("getOzoneByYear", [].as_slice());
            rpcs.insert("getOzoneColumn", [].as_slice());
            rpcs.insert("getSolarByYear", [].as_slice());
            rpcs.insert("getSolarColumn", [].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getValue", ["Year", "column name"].as_slice());
            rpcs.insert("getVolcanismByYear", [].as_slice());
            rpcs.insert("getVolcanismColumn", [].as_slice());
            rpcs.insert("getYearColumn", [].as_slice());
            rpcs
        });
        services.insert("TheMapotakes's First Service", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAwayScoreSum", [].as_slice());
            rpcs.insert("getDateColumn", [].as_slice());
            rpcs.insert("getHomeScoreSum", [].as_slice());
            rpcs
        });
        services.insert("Thingspeak", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("channelDetails", ["id"].as_slice());
            rpcs.insert("channelFeed", ["id", "numResult"].as_slice());
            rpcs.insert("privateChannelFeed", ["id", "numResult", "apiKey"].as_slice());
            rpcs.insert("searchByLocation", ["latitude", "longitude", "distance", "limit"].as_slice());
            rpcs.insert("searchByTag", ["tag", "limit"].as_slice());
            rpcs.insert("searchByTagAndLocation", ["tag", "latitude", "longitude", "distance", "limit"].as_slice());
            rpcs
        });
        services.insert("ThisXDoesNotExist", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getArtwork", [].as_slice());
            rpcs.insert("getCat", [].as_slice());
            rpcs.insert("getCongressPerson", [].as_slice());
            rpcs.insert("getFursona", [].as_slice());
            rpcs.insert("getHomeInterior", [].as_slice());
            rpcs.insert("getHorse", [].as_slice());
            rpcs.insert("getPerson", [].as_slice());
            rpcs.insert("getPony", [].as_slice());
            rpcs.insert("getWaifu", [].as_slice());
            rpcs
        });
        services.insert("Tobacco Consumption", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAllMeasureValues", [].as_slice());
            rpcs.insert("getCountry", ["Measure"].as_slice());
            rpcs.insert("getValue", ["Measure"].as_slice());
            rpcs.insert("getYEA", ["Measure"].as_slice());
            rpcs.insert("getYear", ["Measure"].as_slice());
            rpcs
        });
        services.insert("Traffic", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("search", ["westLongitude", "northLatitude", "eastLongitude", "southLatitude"].as_slice());
            rpcs.insert("stop", [].as_slice());
            rpcs
        });
        services.insert("Translation", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("detectLanguage", ["text"].as_slice());
            rpcs.insert("getSupportedLanguages", [].as_slice());
            rpcs.insert("toEnglish", ["text"].as_slice());
            rpcs.insert("translate", ["text", "from", "to"].as_slice());
            rpcs
        });
        services.insert("Trivia", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getRandomQuestion", [].as_slice());
            rpcs.insert("random", [].as_slice());
            rpcs
        });
        services.insert("TwentyQuestions", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("answer", ["answer"].as_slice());
            rpcs.insert("gameStarted", [].as_slice());
            rpcs.insert("guess", ["guess"].as_slice());
            rpcs.insert("restart", [].as_slice());
            rpcs.insert("start", ["answer"].as_slice());
            rpcs
        });
        services.insert("Twitter", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("favorites", ["screenName", "count"].as_slice());
            rpcs.insert("favoritesCount", ["screenName"].as_slice());
            rpcs.insert("followers", ["screenName"].as_slice());
            rpcs.insert("recentTweets", ["screenName", "count"].as_slice());
            rpcs.insert("search", ["keyword", "count"].as_slice());
            rpcs.insert("tweets", ["screenName"].as_slice());
            rpcs.insert("tweetsPerDay", ["screenName"].as_slice());
            rpcs
        });
        services.insert("Vaping", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAllMeasureValues", [].as_slice());
            rpcs.insert("getCountry", ["Measure"].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getValue", ["Measure"].as_slice());
            rpcs.insert("getYear", ["Measure"].as_slice());
            rpcs
        });
        services.insert("Vostok Temperatures", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAllTemperatureValues", [].as_slice());
            rpcs.insert("getAllYearValues", [].as_slice());
            rpcs.insert("getRecord", ["Year"].as_slice());
            rpcs.insert("getTemperature", ["Year"].as_slice());
            rpcs
        });
        services.insert("Water Quality", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAllWellValues", [].as_slice());
            rpcs.insert("getDate", ["Well"].as_slice());
            rpcs.insert("getQuality", ["Well"].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs
        });
        services.insert("WaterWatch", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("gageHeight", ["minLatitude", "maxLatitude", "minLongitude", "maxLongitude"].as_slice());
            rpcs.insert("stop", [].as_slice());
            rpcs.insert("streamFlow", ["minLatitude", "maxLatitude", "minLongitude", "maxLongitude"].as_slice());
            rpcs.insert("waterTemp", ["minLatitude", "maxLatitude", "minLongitude", "maxLongitude"].as_slice());
            rpcs
        });
        services.insert("Weather", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("description", ["latitude", "longitude"].as_slice());
            rpcs.insert("humidity", ["latitude", "longitude"].as_slice());
            rpcs.insert("icon", ["latitude", "longitude"].as_slice());
            rpcs.insert("temp", ["latitude", "longitude"].as_slice());
            rpcs.insert("temperature", ["latitude", "longitude"].as_slice());
            rpcs.insert("windAngle", ["latitude", "longitude"].as_slice());
            rpcs.insert("windSpeed", ["latitude", "longitude"].as_slice());
            rpcs
        });
        services.insert("Well Data", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("get1222021", ["Well A"].as_slice());
            rpcs.insert("get172", ["Well A"].as_slice());
            rpcs.insert("getAllWellAValues", [].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs
        });
        services.insert("brian's First Service", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAwayScoreByDate", [].as_slice());
            rpcs.insert("getAwayScoreColumn", [].as_slice());
            rpcs.insert("getAwayTeamByDate", [].as_slice());
            rpcs.insert("getAwayTeamColumn", [].as_slice());
            rpcs.insert("getCityByDate", [].as_slice());
            rpcs.insert("getCityColumn", [].as_slice());
            rpcs.insert("getCountryColumn", [].as_slice());
            rpcs.insert("getDateColumn", [].as_slice());
            rpcs.insert("getHomeScoreByDate", [].as_slice());
            rpcs.insert("getHomeScoreColumn", [].as_slice());
            rpcs.insert("getHomeTeamByDate", [].as_slice());
            rpcs.insert("getHomeTeamColumn", [].as_slice());
            rpcs
        });
        services.insert("brian's soccer service", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAwayScoreByDate", [].as_slice());
            rpcs.insert("getAwayScoreColumn", [].as_slice());
            rpcs.insert("getAwayTeamByDate", [].as_slice());
            rpcs.insert("getAwayTeamColumn", [].as_slice());
            rpcs.insert("getCityByDate", [].as_slice());
            rpcs.insert("getCityColumn", [].as_slice());
            rpcs.insert("getCountryByDate", [].as_slice());
            rpcs.insert("getCountryColumn", [].as_slice());
            rpcs.insert("getDateColumn", [].as_slice());
            rpcs.insert("getHomeScoreByDate", [].as_slice());
            rpcs.insert("getHomeScoreColumn", [].as_slice());
            rpcs.insert("getHomeTeamByDate", [].as_slice());
            rpcs.insert("getHomeTeamColumn", [].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getValue", ["date", "column name"].as_slice());
            rpcs
        });
        services.insert("cbradyisTestingFromCSV", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAreaByIndex", [].as_slice());
            rpcs.insert("getAreaColumn", [].as_slice());
            rpcs.insert("getIndexColumn", [].as_slice());
            rpcs.insert("getPerimeterByIndex", [].as_slice());
            rpcs.insert("getPerimeterColumn", [].as_slice());
            rpcs.insert("getRadiusByIndex", [].as_slice());
            rpcs.insert("getRadiusColumn", [].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getValue", ["index", "column name"].as_slice());
            rpcs
        });
        services.insert("coreys-soccer-wtih-options", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAwayScoreByDate", [].as_slice());
            rpcs.insert("getAwayScoreColumn", [].as_slice());
            rpcs.insert("getAwayTeamByDate", [].as_slice());
            rpcs.insert("getAwayTeamColumn", [].as_slice());
            rpcs.insert("getCityByDate", [].as_slice());
            rpcs.insert("getCityColumn", [].as_slice());
            rpcs.insert("getCountryByDate", [].as_slice());
            rpcs.insert("getCountryColumn", [].as_slice());
            rpcs.insert("getDateColumn", [].as_slice());
            rpcs.insert("getHomeScoreByDate", [].as_slice());
            rpcs.insert("getHomeScoreColumn", [].as_slice());
            rpcs.insert("getHomeTeamByDate", [].as_slice());
            rpcs.insert("getHomeTeamColumn", [].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getValue", ["date", "column name"].as_slice());
            rpcs
        });
        services.insert("dummy", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("get12ByA", [].as_slice());
            rpcs.insert("get12Column", [].as_slice());
            rpcs.insert("getAColumn", [].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getValue", ["A", "column name"].as_slice());
            rpcs
        });
        services.insert("empty service", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("get0By0", [].as_slice());
            rpcs.insert("get0Column", [].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getValue", ["0", "column name"].as_slice());
            rpcs
        });
        services.insert("myservice", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAColumn", [].as_slice());
            rpcs.insert("getBByA", [].as_slice());
            rpcs.insert("getBColumn", [].as_slice());
            rpcs.insert("getCByA", [].as_slice());
            rpcs.insert("getCColumn", [].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getValue", ["a", "column name"].as_slice());
            rpcs
        });
        services.insert("myservice1", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("get1ByA", [].as_slice());
            rpcs.insert("get1Column", [].as_slice());
            rpcs.insert("getAColumn", [].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getValue", ["a", "column name"].as_slice());
            rpcs
        });
        services.insert("soccer", {
            let mut rpcs = BTreeMap::new();
            rpcs.insert("getAwayScoreByDate", [].as_slice());
            rpcs.insert("getAwayScoreColumn", [].as_slice());
            rpcs.insert("getAwayTeamByDate", [].as_slice());
            rpcs.insert("getAwayTeamColumn", [].as_slice());
            rpcs.insert("getCityByDate", [].as_slice());
            rpcs.insert("getCityColumn", [].as_slice());
            rpcs.insert("getCountryByDate", [].as_slice());
            rpcs.insert("getCountryColumn", [].as_slice());
            rpcs.insert("getDateColumn", [].as_slice());
            rpcs.insert("getHomeScoreByDate", [].as_slice());
            rpcs.insert("getHomeScoreColumn", [].as_slice());
            rpcs.insert("getHomeTeamByDate", [].as_slice());
            rpcs.insert("getHomeTeamColumn", [].as_slice());
            rpcs.insert("getTable", [].as_slice());
            rpcs.insert("getValue", ["date", "column name"].as_slice());
            rpcs
        });
        services
    };
}
