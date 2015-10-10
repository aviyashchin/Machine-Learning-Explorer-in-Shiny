show databases;
use test;	
show tables;
SELECT `COLUMN_NAME` 
FROM `INFORMATION_SCHEMA`.`COLUMNS` 
WHERE `TABLE_SCHEMA`='test' 
    AND `TABLE_NAME`='120 - no exercise adults (percent) - national';

select `Locale County FIPS Code`,`Numeric Value` as `no_exercise_percent` from `120 - no exercise adults (percent) - national` where Timeframe=2009 and `Numeric Value` is NOT NULL
select `Locale County FIPS Code`,`Numeric Value` as `few_fruits_percent` from `121 - few fruitsvegetables adults (percent) - national` where Timeframe=2009 and `Numeric Value` is NOT NULL

select * from `aminno_member`
select * from `bankruptcy`
select * from `county business patterns from factfinder.census.gov1`
select * from `education1`
select * from `household income disparity1`
select * from `member_details`
select * from `populationestimates`
select * from `povertyestimates`
select * from `social_capital`
select * from `unemployment`
select * from `ziptofips`
