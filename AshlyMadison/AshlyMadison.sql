show databases;
use test;	
show tables;
SELECT `COLUMN_NAME` 
FROM `INFORMATION_SCHEMA`.`COLUMNS` 
WHERE `TABLE_SCHEMA`='test' 
    AND `TABLE_NAME`='120 - no exercise adults (percent) - national';

select `Locale County FIPS Code` as FIPS,`Numeric Value` as `no_exercise_percent` from `120 - no exercise adults (percent) - national` where Timeframe=2009 and `Numeric Value` is NOT NULL
select `Locale County FIPS Code`,`Numeric Value` as `few_fruits_percent` from `121 - few fruitsvegetables adults (percent) - national` where Timeframe=2009 and `Numeric Value` is NOT NULL

select * from ziptofips

--select zip,count(nickname),avg(dob),group_concat(keywords) from `aminno_member` where zip  in (select ZCTA5 from ziptofips) group by zip limit 10

--All Accounts
--select count(nickname) from `aminno_member` where zip in (select ZCTA5 from ziptofips) group by zip limit 10

--real Accounts
select * from `aminno_member` where gender=2 and approved=1 and zip is not NULL

--select * from `aminno_member` limit 10
--SELECT COUNT(*) FROM aminno_member;

--select ZCTA5 from ziptofips

select zip,count(nickname),avg(dob) from `aminno_member` where zip in (select ZCTA5 from ziptofips) group by zip


select `Circ/Dist and County`,`County Code`,`All FilingsTotal`,`Business Filings Total`,`Nonbusiness Filings Total` from  `bankruptcy`

-- select * from `aminno_member`
select `FIPS`,`Number of establishments`, `Paid employees for pay period including March 12 (number)`, `First-quarter payroll ($1,000)`, `Annual payroll ($1,000)` from `county business patterns from factfinder.census.gov1` where year=2013 and `Meaning of 2012 NAICS code`=`Total for all sectors`

select `FIPS Code` as FIPS,
`State` as `State`,
`Area name` as `Area_name`,
`2013 Rural-urban Continuum Code` as `2013_Rural-urban_Continuum_Code`,
`2013 Urban Influence Code` as `2013_Urban_Influence_Code`,
`Prc of adults with less than high school diploma, 2009-2013` as `Prc_of_adults_with_less_than_high_school_diploma_2009-2013`,
`Prc of adults with a high school diploma only, 2009-2013` as `Prc_of_adults_with_a_high_school_diploma_only_2009-2013`,
`Prc of adults compl some college or asc deg, 2009-2013` as `Prc_of_adults_compl_some_college_or_asc_deg_2009-2013`,
`Prc of adults with a bachelor's deg or higher, 2009-2013` as `Prc_of_adults_with_a_bachelor's_deg_or_higher_2009-2013` from `education1`

select * from `household income disparity1`
--drop table `member_details`

--select * from `populationestimates`
select FIPStxt as FIPS, State, Area_Name, `Rural-urban_Continuum Code_2013` as `Rural_urban_Continuum_Code_2013`, Urban_Influence_Code_2013, CENSUS_2010_POP, ESTIMATES_BASE_2010, POP_ESTIMATE_2014, N_POP_CHG_2014, Births_2014, Deaths_2014, NATURAL_INC_2014, INTERNATIONAL_MIG_2014, DOMESTIC_MIG_2014, NET_MIG_2014, RESIDUAL_2014, GQ_ESTIMATES_2014, R_birth_2014, R_death_2014, R_NATURAL_INC_2014, R_INTERNATIONAL_MIG_2014, R_DOMESTIC_MIG_2014, R_NET_MIG_2014 from `populationestimates`

select * from `povertyestimates`
select * from `social_capital`

select FIPS_Code, State, Area_name, Rural_urban_continuum_code_2013, Urban_influence_code_2013, Civilian_labor_force_2014, Employed_2014, Unemployed_2014, Unemployment_rate_2014, Median_Household_Income_2013 from `unemployment`
 