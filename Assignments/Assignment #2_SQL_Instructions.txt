--Select objectid and geometry
--Split crash_date to year, month, day, and time format
--Filter by pedestrian crashes and Salt Lake County
SELECT objectid, geom, substr(crash_date,7,4) AS Year, substr(crash_date,1,2) AS Month, substr(crash_date,4,2) As Day, substr(crash_date,12,11) As Time 
FROM Utah_Crash_2015_2019_3566
WHERE pedestrian = 'true' AND COUNTY_NAM = 'SALT LAKE';
--Load Layer as: SLCo_PedCrash_3566

--Count the Number of Crashes by years and months
SELECT Year, Month, Count(objectid)
FROM SLCo_PedCrash_2015_2019
GROUP BY Year, Month;

--Filter by year 2015
SELECT *
FROM SLCo_PedCrash_2015_2019
WHERE Year = '2015';
--Load as new layer: SLCo_PedCrash_2015
--Filter by year 2016
SELECT *
FROM SLCo_PedCrash_3566
WHERE Year = '2016';
--Load as new layer: SLCo_PedCrash_2016
--Filter by year 2017
SELECT *
FROM SLCo_PedCrash_3566
WHERE Year = '2017';
--Load as new layer: SLCo_PedCrash_2017
--Filter by year 2018
SELECT *
FROM SLCo_PedCrash_3566
WHERE Year = '2018';
--Load as new layer: SLCo_PedCrash_2018
--Filter by year 2019
SELECT *
FROM SLCo_PedCrash_2015_2019
WHERE Year = '2019' AND geom NOT NULL;
--Load as new layer: SLCo_PedCrash_2019