# Deaf-Latinx-Repository

**[Overview](#overview)** | **[How To Start](#start)** | **[Why the 2020 PUMS Dataset Excludes](#whyfiles)**

## Overview <a id = 'overview'></a>

The National Deaf Center on Postsecondary Outcomes' 2021 Deaf Latinx report aims to understand which aspects of employment experiences might be unique, or not, to the deaf Latinx population. The report's analyses are based on this repository, which contains the R syntax for all statistical assessments, thereby enabling the public to reproduce or update the results. However, in so doing, 13 Public Use Microdata Sample (PUMS) datasets from 1-year estimates and 2 datasets of 5-year estimates from American Community Survey (ACS) are required.

## How To Start <a id = 'start'></a>

The prerequisite for conducting all assessments is at least 51 GB of memory space. The computer needs to either have this amount of storage available or create additional space. Without sufficient memory, these assessments cannot be performed.

1. The first step in this process is to clone the repository by using the ```git clone``` command.

2. All of these datasets can be obtained from the [U.S. Census Bureau's File Transfer Protocol](https://www2.census.gov/programs-surveys/acs/data/pums/).

3. The most recent PUMS csv_hus.zip and csv_pus.zip datasets of 5-year estimates need to be downloaded and placed in the [Assets](Assets) folder on the desktop where the repository has been cloned.
   
4. Excluding the 2020 dataset, the 13 consecutive PUMS csv_pus.zip datasets of 1-year estimates need to be downloaded and placed in the [Time Assets](Time%20Assets) folder on the desktop.

5. The ```DeafLatinx.Rproj``` needs to be opened on the desktop, followed by the```RScript.R```. This script needs to be able to run without any issues. If problems arise, there may be missing dependencies (such as ```dplyr```, ```tidyr```, or ```tidycensus```) or the coding may need refactoring.

6. After this first script is completed, the ```TimeSeries.R``` script needs to run.

## Why the 2020 PUMS Dataset Excludes <a id = 'whyfiles'></a>

The 2020 PUMS dataset of 1-year estimates is largely unusable due to nonresponse bias. [Census Bureau](https://www.census.gov/newsroom/press-releases/2021/changes-2020-acs-1-year.html) staff found a high nonresponse rate among people with lower income, lower educational attainment, and those less likely to own their homes. The implication is that this dataset could compromise the quality of assessments and potentially misrepresent deaf postsecondary outcomes.

## Reference

U.S. Census Bureau. (2021). *Census Bureau Announces Changes for 20202 American Community Survey 1-Year Estimates.* https://www.census.gov/newsroom/press-releases/2021/changes-2020-acs-1-year.html
