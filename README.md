Replication Kit for “Mandatory Retirement and Age, Race, and Gender Diversity of University Faculties” 

## Introduction

The code in this repository was used to create the tables and figures in the main body of Daniel E. Ho, Oluchi Mbonu & Anne McDonough, “Mandatory Retirement and Age, Race, and Gender Diversity of University Faculties,” American Law and Economics Review (2021).  

## Requirements
R version 3.4.3 or higher. The scripts will use the following R packages (and their dependencies) from CRAN. The scripts were tested on the denoted versions of the packages.
●	survival version 2.44-1.1
●	survminer version 0.4.7
●	gridExtra version 2.3
●	plotrix version 3.7-6
●	data.table version 1.12.2
●	dplyr version 0.8.3

## Naming Conventions 
Each script is named for the figure or table it creates. The naming convention is [item type]_[item number]_[item description].

## Published Data
The data used in this paper is from the annual Directories of Law Teachers published by The American Association of Law Schools (AALS). In this repository we only make available anonymized aggregate data due to privacy concerns. See the data dictionary below for descriptions of these public datasets.  

## Data Dictionary

|Table name |Description|
|:---------- |:------------|
|faculty	|Faculty level biographical data |
|school	|School level data |
|facultySchool	|Faculty/school/year level data detailing faculty service |
|schoolYear	|Data aggregated to the school/year level|
|year	|Data aggregated to the year level|
|ageCensus	|Year level U.S. population age data from U.S. Census|
|schoolMetaData	|School/year level data from U.S. News and World Reports (USNWR)|
|------------|----------|
|*Published Data*||
|year |Selected variables aggregated to the year level for all AALS member schools (including HBCUs and non-continental Law Schools) |
|year_pwi_continental |Selected variables aggregated to the year level for PWI and continental AALS member schools |
|schoolYear| Selected variables aggregated to the school/year level (including HBCUs and non-continental Law Schools) |

### Variable Definitions
**faculty**
|Variable name	|Description|
|:---| :---|
|facID|	Unique faculty ID
name|	Faculty full name
byear|	Faculty birth year 
female|	Female indicator
minority|	Minority indicator
collegeDeg|	Undergraduate degree earned
collegeYear|	Year undergraduate degree was earned
lawDeg|	Law Degree earned
lawYear|	Year law degree was earned
advLawDeg|	Advanced law degree earned
advLawYear|	Year advanced law degree was earned
gradDeg|	Graduate degree earned
gradYear|	Year graduate degree was earned
first.year|	Year of first appearance in the directory
last.year|	Year of last appearance in the directory
private|	Private indicator for last school of appearance (tenured/tenure track faculty only)
first.year.tenured|	First year as a tenured faculty member (tenured faculty only)


**school**
|Variable name|	Description|
|:---|:---|
|schoolID|	Unique School identifier
Name|	School Name
Address|	School Address
State|	State where school is located
Uncapped.Year|	Year school uncapped mandatory retirement
Canadian|	Indicator for whether the school is Canadian
Continental.USA|	Indicator for whether the school is located in the continental USA
Coverage.Period|	Indicator for whether the school has data for at least one year before and after the school specific uncapping of mandatory retirement (mergers and splits are exceptions and still considered within the coverage period).
HBCU|	Indicator for whether the school is an HBCU
Private|	Indicator for whether the school is private
First.Appearance|	Year in which the school first appears in AALS volumes
Last.Appearance|	Year in which the school last appears in AALS volumes
rank2010|	Rank of the school in 2010 as reported by the U.S. News and World Report
T14|	Indicator for whether a school is T14 (top 10 consistently in the past 14 years)
foundedPost1994|	Indicator for whether a school was founded post 1994



**facultySchool**
|Variable name|	Description|
|:---|:---|
year|	Year of affiliation
schoolID|	Unique school identifier
facID|	Unique faculty identifier
schoolName|	School Name
facName|	Faculty Name
age|	Faculty age
title|	Faculty Title 
tenure.tenureTrack|	Indicator for whether faculty member is either tenured or on the tenure track
tenured|	Indicator for whether faculty member is tenured
emeritus|	Indicator for whether faculty member is emeritus
librarian|	Indicator for whether faculty member is a librarian
clinical|	Indicator for whether faculty member is Clinical (including dir. Of clinics) faculty
emeritusNextYear|	Indicator for whether a faculty member became emeritus in the next year
leavesSchool|	Indicator for whether a year was the last year of appearance of the faculty member in that school given that they are older than 49 and tenured
leavesDirectory|	Indicator for whether a year was the last year of appearance of the faculty in the database given that they are older than 49 and tenured
mandatoryRetirement|	Indicator for whether a faculty member would have been subject to mandatory retirement (older than 70 by the schools uncapped year and started at the school before the uncapped year)

**schoolYear/year**
|Variable name|	Description|
|:---|:---|
schoolID|	Unique school identifier
nFaculty|	Number of faculty members
nFacultyClinical|	Number of faculty members including clinical faculty
nClinical|	Number of clinical faculty members
propClinical|	Proportion of faculty members that are clinical
nJunior|	Number of junior faculty members (excluding Clinical)
propJunior|	Proportion of faculty members that are junior faculty members (excluding Clinical)
nSenior|	Number of senior faculty members (excluding clinical)
propSenior|	Proportion of faculty members that are senior faculty members (excluding Clinical)
nOver70|	Number of faculty members older than 70 years (excluding clinical)
over70|	Proportion of faculty members older than 70 years (excluding clinical)
over70Clinical|	Proportion of faculty members older than 70 years (ncluding clinical faculty)
under40|	Proportion of faculty members younger than 40 years old (excluding clinical)
under40Clinical|	Proportion of faculty members younger than 40 years old including clinical faculty)
female|	Proportion of faculty members that are female (excluding clinical)
femaleClinical| Proportion of faculty members that are female (including clinical faculty)
femaleClinicalOnly| Proportion of clinical faculty members that are female
nFemale|	Number of faculty members that are female (excluding clinical)
minority|	Proportion of faculty members that are minority (excluding clinical)
minorityClinical| Proportion of faculty members that are minority including clinical faculty
minorityCliicalOnly | Proportion of clinical faculty that are minority
nMinority|	Number of faculty members that are minority (excluding clinical)
minorityFemale|	Proportion of faculty members that are minority and female (excluding clinical)
nMinorityFemale|	Number of faculty members that are minority and female
minorityMale | Proportion of faculty members that are minority and male (excluding clinical)
age|	Average age of faculty members
propCollegeDeg|	Proportion of faculty members with a recorded college degree
collegeDeg|	Number of faculty members with a recorded college degree 
propLawDeg| Proportion of faculty members with a recorded law degree
lawDeg|	Number of faculty members with a recorded law degree
propAdvLawDeg | Proportion of faculty members with a recorded advanced law degree
advLawDeg|	Number of faculty members with a recorded advanced law degree
propGradDeg | Proportion of faculty members with a graduate degree
gradDeg|	Number of faculty members with a graduate degree
leavesDirectory|	Proportion of faculty that retired
leavesSchool|	Proportion of faculty that retired from one school
juniorHires|	Number of junior hires - seniority based on title parsing. A faculty member is considered as 'hired' in the first year in which they appear in that school
lateralHires|	Number of lateral hires - seniority based on title parsing. A faculty member is considered as 'hired' in the first year in which they appear in that school
minorityJuniorHires|	Proportion of junior hires that are minority - minority based on AALS recording, seniority based on title parsing
minorityLateralHires|	Proportion of lateral hires that are minority - minority based on AALS recording, seniority based on title parsing
minorityHires|	Proportion of all hires that are minority
femaleJuniorHires|	Proportion of junior hires that are female - seniority based on title parsing
femaleLateralHires|	Proportion of lateral hires that are female - seniority based on title parsing
femaleHires|	Proportion of all hires that are female
minorityFemaleHires|	Proportion of ALL hires that are minority female
nOver70Female|	Number of faculty over70 and female
nDepartures|	Number of faculty that left in the following year (and are over 49 y/o)

**ageCensus**
|Variable| name	Description|
|:---|:---|
propOver70|	Proportion of the U.S. population over 70 years old
year|	Year

**schoolMetaData**
|Variable| name	Description|
|:---|:---|
AALS.Member|	Indicator for whether the school is an AALS member
reportingYear|	Year of USNWR rankings
rank|	USNWR law school rank

