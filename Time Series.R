# Libraries and data------------------------------------------------
# Necessary libraries
library(readr)
library(data.table)
library(textreadr)
library(dplyr)
library(lubridate)
library(Dict)
library(jsonlite)
library(tidycensus)
library(stringr)
library(gtools)

# Fetch Statistics
library(tidyr)
library(Hmisc)
dir <- paste(getwd(),'Statistics.R',sep='/')
source(dir)

# List of PWGTP1-PWGTP80
pwgtps <- c()

for(i in seq(1,80,1)){
  pwgtps[i] <- paste('pwgtp',i,sep='')
}

# Prepare Time Series Analysis--------------------------------------------------
# Obtain datasets from 13 years before most recent year to most recent year
range <- 13
year_n <- 2021

for(i in 0:range){
  destination<-paste(getwd(),'/Time Assets/',year_n-i,'csv_pus.zip', sep='')
  if(!file.exists(destination) & ((year_n - i) != 2020)){
    urlpath<-paste0('https://www2.census.gov/programs-surveys/acs/data/pums/',
                    year_n-i,'/1-Year/csv_pus.zip')
    download.file(urlpath, destfile = destination, method = 'curl')
  }
}

## Create the grand calculator--------------------------------------------------
time_series<-function(year_n){
  filepath<-paste(getwd(),'/Time Assets/',year_n,'csv_pus.zip', sep='')
  unzipped <- tryCatch(unzip(filepath),error = function(e){
    "error"})
  if(!any(grep('.csv',unzipped))){
    return(data.frame(attribution = NA, status = NA, percentage = NA, 
               margin_errors = NA, variable = NA, type = NA, state = NA, 
               year = year_n))
  }else{
    unzipped<-unzipped[grep('.csv',unzipped)]
    
    if(year_n < 2019){
      df<-data.frame()
      for(j in 1:length(unzipped)){
        part_df<-fread(unzipped[j])
        names(part_df)<-toupper(names(part_df))
        part_df<-select(part_df, 
                        toupper(c('dear','agep','rac1p','hisp','sex',
                                  'ddrs','deye','dout',
                                  'dphy','drem','schl','schg',
                                  'esr','pernp','wkhp','wkw',
                                  'naicsp','occp','indp',
                                  'cow','pwgtp',pwgtps)))
        df<-bind_rows(df,part_df)
      }
      
      df$HISP<-as.numeric(df$HISP)
      employment<-df%>%
        mutate(DEAR = ifelse(DEAR == '1', "deaf", "hearing"))%>%# DEAR recode
        mutate(RAC1P = recode(RAC1P, '1' = 'White',             # RAC1P recode
                              '2' = 'Black',
                              '3' = 'Native American',
                              '6' = 'Asian/Pacific Islander',
                              '7' = 'Asian/Pacific Islander',
                              '8' = 'Other Race/Multiracial',
                              '9' = 'Other Race/Multiracial',
                              .default = 'Native American'))%>%
        mutate(HISP = ifelse(HISP %in% c(1,23), 'Not Latinx','Latinx'))%>% # HISP recode
        mutate(SEX = ifelse(SEX == '1','male','female'))%>%   # SEX recode
        mutate(DEYE = recode(DEYE, '1' = 'Blind',             # DEYE recode
                             '2' = 'Sighted'))%>%
        mutate(DDRS = recode(DDRS, '1' = 'Yes',               # DDRS recode
                             '2' = 'No'))%>%
        mutate(DOUT = recode(DOUT, '1' = 'Yes',               # DOUT recode
                             '2' = 'No'))%>%
        mutate(DPHY = recode(DPHY, '1' = 'Yes',               # DPHY recode
                             '2' = 'No'))%>%
        mutate(DREM = recode(DREM, '1' = 'Yes',               # DREM recode
                             '2' = 'No'))%>%
        mutate(ESR = recode(ESR, '1' = 'employed',
                            '3' = 'unemployed',
                            '6' = 'notinLF', 
                            .default = 'employed'))%>%
        mutate(RACETH=ifelse(HISP == 'Latinx','Latinx',      # Create RACETH
                             ifelse(RAC1P == 'Black','Black',
                                    ifelse(RAC1P == 'Asian/Pacific Islander',
                                           'Asian/Pacific Islander',
                                           ifelse(RAC1P == 'Native American',
                                                  'Native American',
                                                  ifelse(RAC1P == 'White','White',
                                                         'Other Race/Multiracial'))))))%>%
        mutate(SCH = ifelse(SCHG > 14, 'Enrolled',
                            'Not Enrolled'))%>%
        mutate(SCHL = ifelse(SCHL < 16,'no HS diploma', 
                             ifelse(SCHL <= 17,'HS diploma',
                                    ifelse(SCHL < 20,'some college',
                                           ifelse(SCHL == 20,'associate',
                                                  ifelse(SCHL == 21,'bachelor', 
                                                         ifelse(SCHL <= 22,'master',
                                                                ifelse(SCHL > 22, 'phd/dr',NA))))))))
      
      # Filter to Ages 16 - 64
      employment <- employment%>%filter(AGEP > 15 & AGEP < 65)
      
      ### Employment Rate---------------------------------------------------------
      employment<-employment%>%mutate(AGE_group = 
                                        ifelse(AGEP >= 16 & AGEP <= 24, '16-24',
                                               ifelse(AGEP >= 25 & AGEP <= 34, '25-34',
                                                      ifelse(AGEP >= 35 & AGEP <= 44, '35-44',
                                                             ifelse(AGEP >= 45 & AGEP <= 54, '45-54',
                                                                    '55-64')))))
      
      # General
      emp_rate<-employment%>%group_by(DEAR,ESR)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      emp_rate<-emp_rate%>%filter(ESR %in% c('unemployed', 'employed'))%>%
        group_by(DEAR)%>%summarise_at(toupper(c('pwgtp',pwgtps)), sum)%>%
        mutate(ESR = 'inLF')%>%relocate(ESR,.after = DEAR)%>%bind_rows(emp_rate)
      
      for_N<-employment%>%
        group_by(DEAR)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      fullmatrix<-left_join(emp_rate,for_N, by = c('DEAR'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){emp_rate[,2+i] <- fullmatrix[,2+i]/fullmatrix[,83+i]}
      
      emp_rate<-bind_cols(select(emp_rate, DEAR, ESR, PWGTP),
                          calculate_me(emp_rate,start = 4, end = 83, alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))%>%
        select(DEAR, ESR, percentage,margin_errors)%>%
        mutate(variable = 'overall', type = 'employment', state = 'United States')%>%
        rename('attribution' = 'DEAR', 'status' = 'ESR')
      
      # Age
      emp_rate_by_age<-employment%>%group_by(DEAR,AGE_group,ESR)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      emp_rate_by_age<-emp_rate_by_age%>%
        filter(ESR %in% c('unemployed', 'employed'))%>%
        group_by(DEAR,AGE_group)%>%summarise_at(toupper(c('pwgtp',pwgtps)), sum)%>%
        mutate(ESR = 'inLF')%>%relocate(ESR,.after = AGE_group)%>%
        bind_rows(emp_rate_by_age)
      
      for_N<-employment%>%
        group_by(DEAR,AGE_group)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      fullmatrix<-left_join(emp_rate_by_age,for_N, by = c('DEAR','AGE_group'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){emp_rate_by_age[,3+i] <- fullmatrix[,3+i]/fullmatrix[,84+i]}
      
      emp_rate_by_age<-bind_cols(select(emp_rate_by_age, DEAR, AGE_group, ESR, PWGTP),
                                 calculate_me(emp_rate_by_age,start = 5, end = 84, alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))
      
      num_obs<-employment%>%group_by(DEAR,AGE_group,ESR)%>%
        summarise(n=n())
      
      emp_rate_by_age<-left_join(emp_rate_by_age,num_obs, 
                                 by=c('DEAR','AGE_group','ESR'))
      
      emp_rate_by_age<-emp_rate_by_age%>%
        mutate(attribution = paste(DEAR,AGE_group,sep=': ages '))%>%ungroup(DEAR,AGE_group)%>%
        select(attribution, ESR, percentage,margin_errors,n)%>%
        mutate(variable = 'age', type = 'employment', state = 'United States')%>%
        rename('status' = 'ESR')
      
      # Gender
      emp_rate_by_gender<-employment%>%group_by(DEAR,SEX,ESR)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      emp_rate_by_gender<-emp_rate_by_gender%>%
        filter(ESR %in% c('unemployed', 'employed'))%>%
        group_by(DEAR,SEX)%>%summarise_at(toupper(c('pwgtp',pwgtps)), sum)%>%
        mutate(ESR = 'inLF')%>%relocate(ESR,.after = SEX)%>%
        bind_rows(emp_rate_by_gender)
      
      for_N<-employment%>%
        group_by(DEAR,SEX)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      fullmatrix<-left_join(emp_rate_by_gender,for_N, by = c('DEAR','SEX'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){emp_rate_by_gender[,3+i] <- fullmatrix[,3+i]/fullmatrix[,84+i]}
      
      emp_rate_by_gender<-bind_cols(select(emp_rate_by_gender, DEAR, SEX, ESR, PWGTP),
                                    calculate_me(emp_rate_by_gender,start = 5, end = 84, alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))%>%
        mutate(SEX = recode(SEX,'female'='women','male'='men'))%>%
        mutate(attribution = paste(DEAR,SEX,sep=' '))%>%ungroup(DEAR)%>%
        select(attribution, ESR, percentage,margin_errors)%>%
        mutate(variable = 'gender', type = 'employment', state = 'United States')%>%
        rename('status' = 'ESR')
      
      # Race
      emp_rate_by_race<-employment%>%group_by(DEAR,RACETH,ESR)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      emp_rate_by_race<-emp_rate_by_race%>%
        filter(ESR %in% c('unemployed', 'employed'))%>%
        group_by(DEAR,RACETH)%>%summarise_at(toupper(c('pwgtp',pwgtps)), sum)%>%
        mutate(ESR = 'inLF')%>%relocate(ESR,.after = RACETH)%>%
        bind_rows(emp_rate_by_race)
      
      for_N<-employment%>%
        group_by(DEAR,RACETH)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      fullmatrix<-left_join(emp_rate_by_race,for_N, by = c('DEAR','RACETH'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){emp_rate_by_race[,3+i] <- fullmatrix[,3+i]/fullmatrix[,84+i]}
      
      emp_rate_by_race<-bind_cols(select(emp_rate_by_race, DEAR, RACETH, ESR, PWGTP),
                                  calculate_me(emp_rate_by_race,start = 5, end = 84, alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))%>%
        mutate(RACETH = recode(RACETH,'Asian/Pacific Islander'='Asian',
                               'Other Race/Multiracial'='multiracial',
                               'White'='white'))%>%
        mutate(attribution = paste(DEAR,RACETH,sep=' '))%>%ungroup(DEAR)%>%
        select(attribution, ESR, percentage,margin_errors)%>%
        mutate(variable = 'race', type = 'employment', state = 'United States')%>%
        rename('status' = 'ESR')
      
      # Disability
      emp_rate_by_disability<-employment %>%      #employed,unemployed,not in LF
        mutate(PLUS = ifelse((DDRS == 'Yes' |     # self-care difficulty
                                DOUT == 'Yes' |   # independent living difficulty
                                DPHY == 'Yes' |   # ambulatory difficulty
                                DREM == 'Yes') &  # cognitive difficulty
                               DEYE == 'Sighted',# deafdisabled only
                             'disabled',
                             ifelse((DDRS == 'No' |    # deaf only
                                       DOUT == 'No' |
                                       DPHY == 'No' |
                                       DREM == 'No') & 
                                      DEYE == 'Sighted',
                                    'no disability',
                                    ifelse(DEYE == 'Blind',
                                           'blind',NA))))%>%
        group_by(DEAR,PLUS,ESR)%>%summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      emp_rate_by_disability<-emp_rate_by_disability%>%
        filter(ESR %in% c('unemployed', 'employed'))%>%
        group_by(DEAR,PLUS)%>%summarise_at(toupper(c('pwgtp',pwgtps)), sum)%>%
        mutate(ESR = 'inLF')%>%relocate(ESR,.after = PLUS)%>%
        bind_rows(emp_rate_by_disability)
      
      for_N<-emp_rate_by_disability%>%
        filter(ESR != 'inLF')%>%
        group_by(DEAR,PLUS)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      fullmatrix<-left_join(emp_rate_by_disability,for_N, by = c('DEAR','PLUS'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){emp_rate_by_disability[,3+i] <- fullmatrix[,3+i]/fullmatrix[,84+i]}
      
      emp_rate_by_disability<-bind_cols(select(emp_rate_by_disability, DEAR, PLUS, ESR, PWGTP),
                                        calculate_me(emp_rate_by_disability,start = 5, end = 84, alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))%>%
        mutate(attribution = paste(DEAR,PLUS,sep=' '))%>%
        mutate(attribution = recode(attribution,
                                    'deaf blind'='deafblind',
                                    'deaf disabled'='deafdisabled',
                                    'deaf no disability'='deaf with no additional disabilities',
                                    'hearing no disability' = 'hearing with no additional disabilities'))%>%
        ungroup(DEAR)%>%
        select(attribution, ESR, percentage,margin_errors)%>%
        mutate(variable = 'disability', type = 'employment', state = 'United States')%>%
        rename('status' = 'ESR')
      
      emp_result <- bind_rows(emp_rate,emp_rate_by_age,emp_rate_by_race,
                              emp_rate_by_gender,emp_rate_by_disability,)
      
      ### Education Attainment----------------------------------------------------------
      education<-employment%>%filter(AGEP > 24)
      
      # General
      edu_att<-education%>%group_by(DEAR,SCHL)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      edu_att<-edu_att%>%
        arrange(DEAR,match(SCHL, 
                           c("phd/dr", "master",
                             "bachelor","associate",
                             "some college","HS diploma", 
                             "no HS diploma")))%>%
        mutate_at(toupper(c('pwgtp',pwgtps)), cumsum)
      
      for_N <- edu_att %>% filter(SCHL == 'no HS diploma')
      
      fullmatrix<-left_join(edu_att,for_N, by = c('DEAR'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){edu_att[,2+i] <- fullmatrix[,2+i]/fullmatrix[,84+i]}
      
      edu_att<-bind_cols(select(edu_att,DEAR,SCHL,PWGTP),
                         calculate_me(edu_att,start = 4, end = 83, 
                                      alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))%>%
        ungroup(DEAR, SCHL)%>%
        rename('status' = 'SCHL', 'attribution' = 'DEAR')%>%
        select(attribution, status, percentage, margin_errors)%>%
        mutate(variable = 'overall', type = 'education', state = 'United States')
      
      # Age
      education<-education%>%mutate(AGE_group = 
                                      ifelse(AGEP >= 25 & AGEP <= 34, '25-34',
                                             ifelse(AGEP >= 35 & AGEP <= 44, '35-44',
                                                    ifelse(AGEP >= 45 & AGEP <= 54, '45-54',
                                                           '55-64'))))
      
      edu_att_by_age<-education%>%group_by(DEAR,AGE_group,SCHL)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      edu_att_by_age<-edu_att_by_age%>%
        arrange(DEAR,AGE_group,match(SCHL, 
                                     c("phd/dr", "master",
                                       "bachelor","associate",
                                       "some college","HS diploma", 
                                       "no HS diploma")))%>%
        mutate_at(toupper(c('pwgtp',pwgtps)), cumsum)
      
      for_N <- edu_att_by_age %>% filter(SCHL == 'no HS diploma')
      
      fullmatrix<-left_join(edu_att_by_age,for_N, by = c('DEAR','AGE_group'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){edu_att_by_age[,3+i] <- fullmatrix[,3+i]/fullmatrix[,85+i]}
      
      edu_att_by_age<-bind_cols(select(edu_att_by_age,DEAR,AGE_group,SCHL,PWGTP),
                                calculate_me(edu_att_by_age,start = 5, end = 84, 
                                             alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))%>%
        mutate(attribution = paste(DEAR,AGE_group,sep=': ages '))%>%ungroup(DEAR,AGE_group)%>%
        select(attribution, SCHL, percentage, margin_errors)%>%
        mutate(variable = 'age', type = 'education', state = 'United States')%>%
        rename('status' = 'SCHL')
      
      # Gender
      edu_att_by_sex<-education%>%group_by(DEAR,SEX,SCHL)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      edu_att_by_sex<-edu_att_by_sex%>%
        arrange(DEAR,SEX,match(SCHL, 
                               c("phd/dr", "master",
                                 "bachelor","associate",
                                 "some college","HS diploma", 
                                 "no HS diploma")))%>%
        mutate_at(toupper(c('pwgtp',pwgtps)), cumsum)
      
      for_N <- edu_att_by_sex %>% filter(SCHL == 'no HS diploma')
      
      fullmatrix<-left_join(edu_att_by_sex,for_N, by = c('DEAR','SEX'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){edu_att_by_sex[,3+i] <- fullmatrix[,3+i]/fullmatrix[,85+i]}
      
      edu_att_by_sex<-bind_cols(select(edu_att_by_sex,DEAR,SEX,SCHL,PWGTP),
                                calculate_me(edu_att_by_sex,start = 5, end = 84, 
                                             alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))%>%
        mutate(SEX = recode(SEX,'female'='women','male'='men'))%>%
        mutate(attribution = paste(DEAR,SEX,sep=' '))%>%ungroup(DEAR,SEX)%>%
        select(attribution, SCHL, percentage, margin_errors)%>%
        mutate(variable = 'gender', type = 'education', state = 'United States')%>%
        rename('status' = 'SCHL')
      
      # Race
      edu_att_by_race<-education%>%group_by(DEAR,RACETH,SCHL)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      edu_att_by_race<-edu_att_by_race%>%
        arrange(DEAR,RACETH,match(SCHL, 
                                  c("phd/dr", "master",
                                    "bachelor","associate",
                                    "some college","HS diploma", 
                                    "no HS diploma")))%>%
        mutate_at(toupper(c('pwgtp',pwgtps)), cumsum)
      
      for_N <- edu_att_by_race %>% filter(SCHL == 'no HS diploma')
      
      fullmatrix<-left_join(edu_att_by_race,for_N, by = c('DEAR','RACETH'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){edu_att_by_race[,3+i] <- fullmatrix[,3+i]/fullmatrix[,85+i]}
      
      edu_att_by_race<-bind_cols(select(edu_att_by_race,DEAR,RACETH,SCHL,PWGTP),
                                 calculate_me(edu_att_by_race,start = 5, end = 84, 
                                              alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))%>%
        mutate(RACETH = recode(RACETH,'Asian/Pacific Islander'='Asian',
                               'Other Race/Multiracial'='multiracial',
                               'White'='white'))%>%
        mutate(attribution = paste(DEAR,RACETH,sep=' '))%>%ungroup(DEAR,RACETH)%>%
        select(attribution, SCHL, percentage, margin_errors)%>%
        mutate(variable = 'race', type = 'education', state = 'United States')%>%
        rename('status' = 'SCHL')
      
      # Disability
      edu_att_by_dis<-education%>%
        mutate(PLUS = ifelse((DDRS == 'Yes' |     # self-care difficulty
                                DOUT == 'Yes' |   # independent living difficulty
                                DPHY == 'Yes' |   # ambulatory difficulty
                                DREM == 'Yes') &  # cognitive difficulty
                               DEYE == 'Sighted',# deafdisabled only
                             'disabled',
                             ifelse((DDRS == 'No' |    # deaf only
                                       DOUT == 'No' |
                                       DPHY == 'No' |
                                       DREM == 'No') & 
                                      DEYE == 'Sighted',
                                    'no disability',
                                    ifelse(DEYE == 'Blind',
                                           'blind',NA))))%>%
        group_by(DEAR,PLUS,SCHL)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      edu_att_by_dis<-edu_att_by_dis%>%
        arrange(DEAR,PLUS,match(SCHL, 
                                c("phd/dr", "master",
                                  "bachelor","associate",
                                  "some college","HS diploma", 
                                  "no HS diploma")))%>%
        mutate_at(toupper(c('pwgtp',pwgtps)), cumsum)
      
      for_N <- edu_att_by_dis %>% filter(SCHL == 'no HS diploma')
      
      fullmatrix<-left_join(edu_att_by_dis,for_N, by = c('DEAR','PLUS'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){edu_att_by_dis[,3+i] <- fullmatrix[,3+i]/fullmatrix[,85+i]}
      
      edu_att_by_dis<-bind_cols(select(edu_att_by_dis,DEAR,PLUS,SCHL,PWGTP),
                                calculate_me(edu_att_by_dis,start = 5, end = 84, 
                                             alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))%>%
        mutate(attribution = paste(DEAR,PLUS,sep=' '))%>%
        mutate(attribution = recode(attribution,
                                    'deaf blind'='deafblind',
                                    'deaf disabled'='deafdisabled',
                                    'deaf no disability'='deaf with no additional disabilities',
                                    'hearing no disability' = 'hearing with no additional disabilities'))%>%
        ungroup(DEAR,PLUS)%>%
        select(attribution, SCHL, percentage, margin_errors)%>%
        mutate(variable = 'disability', type = 'education', state = 'United States')%>%
        rename('status' = 'SCHL')
      
      edu_result <- bind_rows(edu_att,edu_att_by_sex,edu_att_by_race,edu_att_by_dis,
                              edu_att_by_age)
      # Salary Income-----------------------------------------------------------------
      # Recategorize Age Group
      employment<-employment%>% # full-time
        mutate(fullTime = ifelse(WKW == 1 & WKHP >= 35,'yes','any'))
      
      employment<-employment%>%mutate(AGE_group = 
                                        ifelse(AGEP >= 16 & AGEP <= 24, '16-24',
                                               ifelse(AGEP >= 25 & AGEP <= 34, '25-34',
                                                      ifelse(AGEP >= 35 & AGEP <= 44, '35-44',
                                                             ifelse(AGEP >= 45 & AGEP <= 54, '45-54',
                                                                    '55-64')))))
      # General Salary Range
      general_income<-employment%>%filter(fullTime == 'yes')%>%
        group_by(DEAR)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), # Apply weighted median
                     list(~ wtd.quantile(PERNP, weights = ., probs = 0.5)))
      
      num_obs<-employment%>%filter(fullTime == 'yes')%>%
        group_by(DEAR)%>%
        summarise(n = n())
      
      general_income<-left_join(general_income,num_obs, 
                                by=c('DEAR'))
      
      general_income<-calculate_regular_me(general_income,start = 3, end = 82, 
                                           alpha = 0.05)%>%
        bind_cols(general_income)%>%
        select(DEAR,PWGTP,margin_errors,n)
      
      general_income<-general_income%>%rename('attribution' = 'DEAR')%>%
        mutate(median_income = PWGTP)%>%
        mutate(status = 'earning', type = 'salary-range',variable = 'overall',
               state = 'United States')%>%
        select(attribution,status,median_income,margin_errors,variable,type,state,n)
      
      # Age Salary Range
      age_income<-employment%>%filter(fullTime == 'yes')%>%
        group_by(DEAR,AGE_group)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), # Apply weighted median
                     list(~ wtd.quantile(PERNP, weights = ., probs = 0.5)))
      
      num_obs<-employment%>%filter(fullTime == 'yes')%>%
        group_by(DEAR,AGE_group)%>%
        summarise(n = n())
      
      age_income<-left_join(age_income,num_obs, 
                            by=c('DEAR','AGE_group'))
      
      age_income<-calculate_regular_me(age_income,start = 4, end = 83, 
                                       alpha = 0.05)%>%
        bind_cols(age_income)%>%
        select(DEAR,AGE_group,PWGTP,margin_errors,n)
      
      age_income<-age_income%>%
        mutate(attribution = paste(DEAR,AGE_group,sep=': ages '))%>%
        mutate(median_income = PWGTP)%>%
        mutate(status = 'earning', type = 'salary-range',variable = 'age',
               state = 'United States')%>%
        select(attribution,status,median_income,margin_errors,variable,type,state,n)
      
      # Gender Salary Range
      gender_income<-employment%>%filter(fullTime == 'yes')%>%
        group_by(DEAR,SEX)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), # Apply weighted median
                     list(~ wtd.quantile(PERNP, weights = ., probs = 0.5)))
      
      num_obs<-employment%>%filter(fullTime == 'yes')%>%
        group_by(DEAR,SEX)%>%
        summarise(n = n())
      
      gender_income<-left_join(gender_income,num_obs, 
                               by=c('DEAR','SEX'))
      
      gender_income<-calculate_regular_me(gender_income,start = 4, end = 83, 
                                          alpha = 0.05)%>%
        bind_cols(gender_income)%>%
        select(DEAR,SEX,PWGTP,margin_errors,n)
      
      gender_income<-gender_income%>%
        mutate(SEX = recode(SEX,'female'='women','male'='men'))%>%
        mutate(attribution = paste(DEAR,SEX,sep=' '),median_income = PWGTP)%>%
        select(attribution,median_income,margin_errors,n)%>%
        mutate(status = 'earning', type = 'salary-range',variable = 'gender',
               state = 'United States')%>%
        select(attribution,status,median_income,margin_errors,variable,type,state,n)
      
      # Race
      race_income<-employment%>%filter(fullTime == 'yes')%>%
        group_by(DEAR,RACETH)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), # Apply weighted median
                     list(~ wtd.quantile(PERNP, weights = ., probs = 0.5)))
      
      num_obs<-employment%>%filter(fullTime == 'yes')%>%
        group_by(DEAR,RACETH)%>%
        summarise(n = n())
      
      race_income<-left_join(race_income,num_obs, 
                             by=c('DEAR','RACETH'))
      
      race_income<-calculate_regular_me(race_income,start = 4, end = 83, 
                                        alpha = 0.05)%>%
        bind_cols(race_income)%>%
        select(DEAR,RACETH,PWGTP,margin_errors,n)
      
      race_income<-race_income%>%
        mutate(RACETH = recode(RACETH,'Asian/Pacific Islander'='Asian',
                               'Other Race/Multiracial'='multiracial',
                               'White'='white'))%>%
        mutate(attribution = paste(DEAR,RACETH,sep=' '), median_income = PWGTP)%>%
        select(attribution,median_income,margin_errors,n)%>%
        mutate(status = 'earning', variable = 'race', type = 'salary-range', 
               state = 'United States')
      
      # Disability
      dis_income<-employment%>%
        mutate(PLUS = ifelse((DDRS == 'Yes' |     # self-care difficulty
                                DOUT == 'Yes' |   # independent living difficulty
                                DPHY == 'Yes' |   # ambulatory difficulty
                                DREM == 'Yes') &  # cognitive difficulty
                               DEYE == 'Sighted',# deafdisabled only
                             'disabled',
                             ifelse((DDRS == 'No' |    # deaf only
                                       DOUT == 'No' |
                                       DPHY == 'No' |
                                       DREM == 'No') & 
                                      DEYE == 'Sighted',
                                    'no disability',
                                    ifelse(DEYE == 'Blind',
                                           'blind',NA))))%>%
        filter(fullTime == 'yes')%>%
        group_by(DEAR,PLUS)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), # Apply weighted median
                     list(~ wtd.quantile(PERNP, weights = ., probs = 0.5)))
      
      num_obs<-employment%>%filter(fullTime == 'yes')%>%
        mutate(PLUS = ifelse((DDRS == 'Yes' |     # self-care difficulty
                                DOUT == 'Yes' |   # independent living difficulty
                                DPHY == 'Yes' |   # ambulatory difficulty
                                DREM == 'Yes') &  # cognitive difficulty
                               DEYE == 'Sighted',# deafdisabled only
                             'disabled',
                             ifelse((DDRS == 'No' |    # deaf only
                                       DOUT == 'No' |
                                       DPHY == 'No' |
                                       DREM == 'No') & 
                                      DEYE == 'Sighted',
                                    'no disability',
                                    ifelse(DEYE == 'Blind',
                                           'blind',NA))))%>%
        group_by(DEAR,PLUS)%>%
        summarise(n = n())
      
      dis_income<-left_join(dis_income,num_obs, 
                            by=c('DEAR','PLUS'))
      
      dis_income<-calculate_regular_me(dis_income,start = 4, end = 83, 
                                       alpha = 0.05)%>%
        bind_cols(dis_income)%>%
        select(DEAR,PLUS,PWGTP,margin_errors,n)
      
      dis_income<-dis_income%>%
        mutate(attribution = paste(DEAR,PLUS,sep=' '),median_income = PWGTP)%>%
        mutate(attribution = recode(attribution,
                                    'deaf blind'='deafblind',
                                    'deaf disabled'='deafdisabled',
                                    'deaf no disability'='deaf with no additional disabilities',
                                    'hearing no disability' = 'hearing with no additional disabilities'))%>%
        select(attribution,median_income,margin_errors,n)%>%
        mutate(status = 'earning', variable = 'disability', type = 'salary-range', 
               state = 'United States')
      
      incomes<-bind_rows(general_income,gender_income,race_income,dis_income,
                         age_income)
      
      return(bind_rows(emp_result,edu_result,incomes))
    }else{
      df<-data.frame()
      for(j in 1:length(unzipped)){
        part_df<-fread(unzipped[j])
        names(part_df)<-toupper(names(part_df))
        part_df<-select(part_df, 
                        toupper(c('dear','agep','rac1p','hisp','sex',
                                  'ddrs','deye','dout',
                                  'dphy','drem','schl','schg',
                                  'esr','pernp','wkhp','wkwn',
                                  'naicsp','occp','indp',
                                  'cow','pwgtp',pwgtps)))
        df<-bind_rows(df,part_df)
      }
      
      df$HISP<-as.numeric(df$HISP)
      employment<-df%>%
        mutate(DEAR = ifelse(DEAR == '1', "deaf", "hearing"))%>%# DEAR recode
        mutate(RAC1P = recode(RAC1P, '1' = 'White',             # RAC1P recode
                              '2' = 'Black',
                              '3' = 'Native American',
                              '6' = 'Asian/Pacific Islander',
                              '7' = 'Asian/Pacific Islander',
                              '8' = 'Other Race/Multiracial',
                              '9' = 'Other Race/Multiracial',
                              .default = 'Native American'))%>%
        mutate(HISP = ifelse(HISP %in% c(1,23), 'Not Latinx','Latinx'))%>% # HISP recode
        mutate(SEX = ifelse(SEX == '1','male','female'))%>%   # SEX recode
        mutate(DEYE = recode(DEYE, '1' = 'Blind',             # DEYE recode
                             '2' = 'Sighted'))%>%
        mutate(DDRS = recode(DDRS, '1' = 'Yes',               # DDRS recode
                             '2' = 'No'))%>%
        mutate(DOUT = recode(DOUT, '1' = 'Yes',               # DOUT recode
                             '2' = 'No'))%>%
        mutate(DPHY = recode(DPHY, '1' = 'Yes',               # DPHY recode
                             '2' = 'No'))%>%
        mutate(DREM = recode(DREM, '1' = 'Yes',               # DREM recode
                             '2' = 'No'))%>%
        mutate(ESR = recode(ESR, '1' = 'employed',
                            '3' = 'unemployed',
                            '6' = 'notinLF', 
                            .default = 'employed'))%>%
        mutate(RACETH=ifelse(HISP == 'Latinx','Latinx',      # Create RACETH
                             ifelse(RAC1P == 'Black','Black',
                                    ifelse(RAC1P == 'Asian/Pacific Islander',
                                           'Asian/Pacific Islander',
                                           ifelse(RAC1P == 'Native American',
                                                  'Native American',
                                                  ifelse(RAC1P == 'White','White',
                                                         'Other Race/Multiracial'))))))%>%
        mutate(SCH = ifelse(SCHG > 14, 'Enrolled',
                            'Not Enrolled'))%>%
        mutate(SCHL = ifelse(SCHL < 16,'no HS diploma', 
                             ifelse(SCHL <= 17,'HS diploma',
                                    ifelse(SCHL < 20,'some college',
                                           ifelse(SCHL == 20,'associate',
                                                  ifelse(SCHL == 21,'bachelor', 
                                                         ifelse(SCHL <= 22,'master',
                                                                ifelse(SCHL > 22, 'phd/dr',NA))))))))
      
      # Filter to Ages 16 - 64
      employment <- employment%>%filter(AGEP > 15 & AGEP < 65)
      
      ### Employment Rate---------------------------------------------------------
      employment<-employment%>%mutate(AGE_group = 
                                        ifelse(AGEP >= 16 & AGEP <= 24, '16-24',
                                               ifelse(AGEP >= 25 & AGEP <= 34, '25-34',
                                                      ifelse(AGEP >= 35 & AGEP <= 44, '35-44',
                                                             ifelse(AGEP >= 45 & AGEP <= 54, '45-54',
                                                                    '55-64')))))
      
      # General
      emp_rate<-employment%>%group_by(DEAR,ESR)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      emp_rate<-emp_rate%>%filter(ESR %in% c('unemployed', 'employed'))%>%
        group_by(DEAR)%>%summarise_at(toupper(c('pwgtp',pwgtps)), sum)%>%
        mutate(ESR = 'inLF')%>%relocate(ESR,.after = DEAR)%>%bind_rows(emp_rate)
      
      for_N<-employment%>%
        group_by(DEAR)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      fullmatrix<-left_join(emp_rate,for_N, by = c('DEAR'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){emp_rate[,2+i] <- fullmatrix[,2+i]/fullmatrix[,83+i]}
      
      emp_rate<-bind_cols(select(emp_rate, DEAR, ESR, PWGTP),
                          calculate_me(emp_rate,start = 4, end = 83, alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))%>%
        select(DEAR, ESR, percentage,margin_errors)%>%
        mutate(variable = 'overall', type = 'employment', state = 'United States')%>%
        rename('attribution' = 'DEAR', 'status' = 'ESR')
      
      # Age
      emp_rate_by_age<-employment%>%group_by(DEAR,AGE_group,ESR)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      emp_rate_by_age<-emp_rate_by_age%>%
        filter(ESR %in% c('unemployed', 'employed'))%>%
        group_by(DEAR,AGE_group)%>%summarise_at(toupper(c('pwgtp',pwgtps)), sum)%>%
        mutate(ESR = 'inLF')%>%relocate(ESR,.after = AGE_group)%>%
        bind_rows(emp_rate_by_age)
      
      for_N<-employment%>%
        group_by(DEAR,AGE_group)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      fullmatrix<-left_join(emp_rate_by_age,for_N, by = c('DEAR','AGE_group'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){emp_rate_by_age[,3+i] <- fullmatrix[,3+i]/fullmatrix[,84+i]}
      
      emp_rate_by_age<-bind_cols(select(emp_rate_by_age, DEAR, AGE_group, ESR, PWGTP),
                                 calculate_me(emp_rate_by_age,start = 5, end = 84, alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))
      
      num_obs<-employment%>%group_by(DEAR,AGE_group,ESR)%>%
        summarise(n=n())
      
      emp_rate_by_age<-left_join(emp_rate_by_age,num_obs, 
                                 by=c('DEAR','AGE_group','ESR'))
      
      emp_rate_by_age<-emp_rate_by_age%>%
        mutate(attribution = paste(DEAR,AGE_group,sep=': ages '))%>%ungroup(DEAR,AGE_group)%>%
        select(attribution, ESR, percentage,margin_errors,n)%>%
        mutate(variable = 'age', type = 'employment', state = 'United States')%>%
        rename('status' = 'ESR')
      
      # Gender
      emp_rate_by_gender<-employment%>%group_by(DEAR,SEX,ESR)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      emp_rate_by_gender<-emp_rate_by_gender%>%
        filter(ESR %in% c('unemployed', 'employed'))%>%
        group_by(DEAR,SEX)%>%summarise_at(toupper(c('pwgtp',pwgtps)), sum)%>%
        mutate(ESR = 'inLF')%>%relocate(ESR,.after = SEX)%>%
        bind_rows(emp_rate_by_gender)
      
      for_N<-employment%>%
        group_by(DEAR,SEX)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      fullmatrix<-left_join(emp_rate_by_gender,for_N, by = c('DEAR','SEX'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){emp_rate_by_gender[,3+i] <- fullmatrix[,3+i]/fullmatrix[,84+i]}
      
      emp_rate_by_gender<-bind_cols(select(emp_rate_by_gender, DEAR, SEX, ESR, PWGTP),
                                    calculate_me(emp_rate_by_gender,start = 5, end = 84, alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))%>%
        mutate(SEX = recode(SEX,'female'='women','male'='men'))%>%
        mutate(attribution = paste(DEAR,SEX,sep=' '))%>%ungroup(DEAR)%>%
        select(attribution, ESR, percentage,margin_errors)%>%
        mutate(variable = 'gender', type = 'employment', state = 'United States')%>%
        rename('status' = 'ESR')
      
      # Race
      emp_rate_by_race<-employment%>%group_by(DEAR,RACETH,ESR)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      emp_rate_by_race<-emp_rate_by_race%>%
        filter(ESR %in% c('unemployed', 'employed'))%>%
        group_by(DEAR,RACETH)%>%summarise_at(toupper(c('pwgtp',pwgtps)), sum)%>%
        mutate(ESR = 'inLF')%>%relocate(ESR,.after = RACETH)%>%
        bind_rows(emp_rate_by_race)
      
      for_N<-employment%>%
        group_by(DEAR,RACETH)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      fullmatrix<-left_join(emp_rate_by_race,for_N, by = c('DEAR','RACETH'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){emp_rate_by_race[,3+i] <- fullmatrix[,3+i]/fullmatrix[,84+i]}
      
      emp_rate_by_race<-bind_cols(select(emp_rate_by_race, DEAR, RACETH, ESR, PWGTP),
                                  calculate_me(emp_rate_by_race,start = 5, end = 84, alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))%>%
        mutate(RACETH = recode(RACETH,'Asian/Pacific Islander'='Asian',
                               'Other Race/Multiracial'='multiracial',
                               'White'='white'))%>%
        mutate(attribution = paste(DEAR,RACETH,sep=' '))%>%ungroup(DEAR)%>%
        select(attribution, ESR, percentage,margin_errors)%>%
        mutate(variable = 'race', type = 'employment', state = 'United States')%>%
        rename('status' = 'ESR')
      
      # Disability
      emp_rate_by_disability<-employment %>%      #employed,unemployed,not in LF
        mutate(PLUS = ifelse((DDRS == 'Yes' |     # self-care difficulty
                                DOUT == 'Yes' |   # independent living difficulty
                                DPHY == 'Yes' |   # ambulatory difficulty
                                DREM == 'Yes') &  # cognitive difficulty
                               DEYE == 'Sighted',# deafdisabled only
                             'disabled',
                             ifelse((DDRS == 'No' |    # deaf only
                                       DOUT == 'No' |
                                       DPHY == 'No' |
                                       DREM == 'No') & 
                                      DEYE == 'Sighted',
                                    'no disability',
                                    ifelse(DEYE == 'Blind',
                                           'blind',NA))))%>%
        group_by(DEAR,PLUS,ESR)%>%summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      emp_rate_by_disability<-emp_rate_by_disability%>%
        filter(ESR %in% c('unemployed', 'employed'))%>%
        group_by(DEAR,PLUS)%>%summarise_at(toupper(c('pwgtp',pwgtps)), sum)%>%
        mutate(ESR = 'inLF')%>%relocate(ESR,.after = PLUS)%>%
        bind_rows(emp_rate_by_disability)
      
      for_N<-emp_rate_by_disability%>%
        filter(ESR != 'inLF')%>%
        group_by(DEAR,PLUS)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      fullmatrix<-left_join(emp_rate_by_disability,for_N, by = c('DEAR','PLUS'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){emp_rate_by_disability[,3+i] <- fullmatrix[,3+i]/fullmatrix[,84+i]}
      
      emp_rate_by_disability<-bind_cols(select(emp_rate_by_disability, DEAR, PLUS, ESR, PWGTP),
                                        calculate_me(emp_rate_by_disability,start = 5, end = 84, alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))%>%
        mutate(attribution = paste(DEAR,PLUS,sep=' '))%>%
        mutate(attribution = recode(attribution,
                                    'deaf blind'='deafblind',
                                    'deaf disabled'='deafdisabled',
                                    'deaf no disability'='deaf with no additional disabilities',
                                    'hearing no disability' = 'hearing with no additional disabilities'))%>%
        ungroup(DEAR)%>%
        select(attribution, ESR, percentage,margin_errors)%>%
        mutate(variable = 'disability', type = 'employment', state = 'United States')%>%
        rename('status' = 'ESR')
      
      emp_result <- bind_rows(emp_rate,emp_rate_by_age,emp_rate_by_race,
                              emp_rate_by_gender,emp_rate_by_disability,)
      
      ### Education Attainment----------------------------------------------------------
      education<-employment%>%filter(AGEP > 24)
      
      # General
      edu_att<-education%>%group_by(DEAR,SCHL)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      edu_att<-edu_att%>%
        arrange(DEAR,match(SCHL, 
                           c("phd/dr", "master",
                             "bachelor","associate",
                             "some college","HS diploma", 
                             "no HS diploma")))%>%
        mutate_at(toupper(c('pwgtp',pwgtps)), cumsum)
      
      for_N <- edu_att %>% filter(SCHL == 'no HS diploma')
      
      fullmatrix<-left_join(edu_att,for_N, by = c('DEAR'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){edu_att[,2+i] <- fullmatrix[,2+i]/fullmatrix[,84+i]}
      
      edu_att<-bind_cols(select(edu_att,DEAR,SCHL,PWGTP),
                         calculate_me(edu_att,start = 4, end = 83, 
                                      alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))%>%
        ungroup(DEAR, SCHL)%>%
        rename('status' = 'SCHL', 'attribution' = 'DEAR')%>%
        select(attribution, status, percentage, margin_errors)%>%
        mutate(variable = 'overall', type = 'education', state = 'United States')
      
      # Age
      education<-education%>%mutate(AGE_group = 
                                      ifelse(AGEP >= 25 & AGEP <= 34, '25-34',
                                             ifelse(AGEP >= 35 & AGEP <= 44, '35-44',
                                                    ifelse(AGEP >= 45 & AGEP <= 54, '45-54',
                                                           '55-64'))))
      
      edu_att_by_age<-education%>%group_by(DEAR,AGE_group,SCHL)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      edu_att_by_age<-edu_att_by_age%>%
        arrange(DEAR,AGE_group,match(SCHL, 
                                     c("phd/dr", "master",
                                       "bachelor","associate",
                                       "some college","HS diploma", 
                                       "no HS diploma")))%>%
        mutate_at(toupper(c('pwgtp',pwgtps)), cumsum)
      
      for_N <- edu_att_by_age %>% filter(SCHL == 'no HS diploma')
      
      fullmatrix<-left_join(edu_att_by_age,for_N, by = c('DEAR','AGE_group'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){edu_att_by_age[,3+i] <- fullmatrix[,3+i]/fullmatrix[,85+i]}
      
      edu_att_by_age<-bind_cols(select(edu_att_by_age,DEAR,AGE_group,SCHL,PWGTP),
                                calculate_me(edu_att_by_age,start = 5, end = 84, 
                                             alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))%>%
        mutate(attribution = paste(DEAR,AGE_group,sep=': ages '))%>%ungroup(DEAR,AGE_group)%>%
        select(attribution, SCHL, percentage, margin_errors)%>%
        mutate(variable = 'age', type = 'education', state = 'United States')%>%
        rename('status' = 'SCHL')
      
      # Gender
      edu_att_by_sex<-education%>%group_by(DEAR,SEX,SCHL)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      edu_att_by_sex<-edu_att_by_sex%>%
        arrange(DEAR,SEX,match(SCHL, 
                               c("phd/dr", "master",
                                 "bachelor","associate",
                                 "some college","HS diploma", 
                                 "no HS diploma")))%>%
        mutate_at(toupper(c('pwgtp',pwgtps)), cumsum)
      
      for_N <- edu_att_by_sex %>% filter(SCHL == 'no HS diploma')
      
      fullmatrix<-left_join(edu_att_by_sex,for_N, by = c('DEAR','SEX'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){edu_att_by_sex[,3+i] <- fullmatrix[,3+i]/fullmatrix[,85+i]}
      
      edu_att_by_sex<-bind_cols(select(edu_att_by_sex,DEAR,SEX,SCHL,PWGTP),
                                calculate_me(edu_att_by_sex,start = 5, end = 84, 
                                             alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))%>%
        mutate(SEX = recode(SEX,'female'='women','male'='men'))%>%
        mutate(attribution = paste(DEAR,SEX,sep=' '))%>%ungroup(DEAR,SEX)%>%
        select(attribution, SCHL, percentage, margin_errors)%>%
        mutate(variable = 'gender', type = 'education', state = 'United States')%>%
        rename('status' = 'SCHL')
      
      # Race
      edu_att_by_race<-education%>%group_by(DEAR,RACETH,SCHL)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      edu_att_by_race<-edu_att_by_race%>%
        arrange(DEAR,RACETH,match(SCHL, 
                                  c("phd/dr", "master",
                                    "bachelor","associate",
                                    "some college","HS diploma", 
                                    "no HS diploma")))%>%
        mutate_at(toupper(c('pwgtp',pwgtps)), cumsum)
      
      for_N <- edu_att_by_race %>% filter(SCHL == 'no HS diploma')
      
      fullmatrix<-left_join(edu_att_by_race,for_N, by = c('DEAR','RACETH'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){edu_att_by_race[,3+i] <- fullmatrix[,3+i]/fullmatrix[,85+i]}
      
      edu_att_by_race<-bind_cols(select(edu_att_by_race,DEAR,RACETH,SCHL,PWGTP),
                                 calculate_me(edu_att_by_race,start = 5, end = 84, 
                                              alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))%>%
        mutate(RACETH = recode(RACETH,'Asian/Pacific Islander'='Asian',
                               'Other Race/Multiracial'='multiracial',
                               'White'='white'))%>%
        mutate(attribution = paste(DEAR,RACETH,sep=' '))%>%ungroup(DEAR,RACETH)%>%
        select(attribution, SCHL, percentage, margin_errors)%>%
        mutate(variable = 'race', type = 'education', state = 'United States')%>%
        rename('status' = 'SCHL')
      
      # Disability
      edu_att_by_dis<-education%>%
        mutate(PLUS = ifelse((DDRS == 'Yes' |     # self-care difficulty
                                DOUT == 'Yes' |   # independent living difficulty
                                DPHY == 'Yes' |   # ambulatory difficulty
                                DREM == 'Yes') &  # cognitive difficulty
                               DEYE == 'Sighted',# deafdisabled only
                             'disabled',
                             ifelse((DDRS == 'No' |    # deaf only
                                       DOUT == 'No' |
                                       DPHY == 'No' |
                                       DREM == 'No') & 
                                      DEYE == 'Sighted',
                                    'no disability',
                                    ifelse(DEYE == 'Blind',
                                           'blind',NA))))%>%
        group_by(DEAR,PLUS,SCHL)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), sum)
      
      edu_att_by_dis<-edu_att_by_dis%>%
        arrange(DEAR,PLUS,match(SCHL, 
                                c("phd/dr", "master",
                                  "bachelor","associate",
                                  "some college","HS diploma", 
                                  "no HS diploma")))%>%
        mutate_at(toupper(c('pwgtp',pwgtps)), cumsum)
      
      for_N <- edu_att_by_dis %>% filter(SCHL == 'no HS diploma')
      
      fullmatrix<-left_join(edu_att_by_dis,for_N, by = c('DEAR','PLUS'), 
                            suffix = c('','_N'))
      
      for(i in seq(1,81,1)){edu_att_by_dis[,3+i] <- fullmatrix[,3+i]/fullmatrix[,85+i]}
      
      edu_att_by_dis<-bind_cols(select(edu_att_by_dis,DEAR,PLUS,SCHL,PWGTP),
                                calculate_me(edu_att_by_dis,start = 5, end = 84, 
                                             alpha = 0.05))%>%
        mutate(percentage = round(PWGTP*100,1), 
               margin_errors = round(margin_errors*100,2))%>%
        mutate(attribution = paste(DEAR,PLUS,sep=' '))%>%
        mutate(attribution = recode(attribution,
                                    'deaf blind'='deafblind',
                                    'deaf disabled'='deafdisabled',
                                    'deaf no disability'='deaf with no additional disabilities',
                                    'hearing no disability' = 'hearing with no additional disabilities'))%>%
        ungroup(DEAR,PLUS)%>%
        select(attribution, SCHL, percentage, margin_errors)%>%
        mutate(variable = 'disability', type = 'education', state = 'United States')%>%
        rename('status' = 'SCHL')
      
      edu_result <- bind_rows(edu_att,edu_att_by_sex,edu_att_by_race,edu_att_by_dis,
                              edu_att_by_age)
      # Salary Income-----------------------------------------------------------------
      # Recategorize Age Group
      employment<-employment%>% # full-time
        mutate(fullTime = ifelse(WKWN %in% c(50,51,52) & WKHP >= 35,'yes','any'))
      
      employment<-employment%>%mutate(AGE_group = 
                                        ifelse(AGEP >= 16 & AGEP <= 24, '16-24',
                                               ifelse(AGEP >= 25 & AGEP <= 34, '25-34',
                                                      ifelse(AGEP >= 35 & AGEP <= 44, '35-44',
                                                             ifelse(AGEP >= 45 & AGEP <= 54, '45-54',
                                                                    '55-64')))))
      # General Salary Range
      general_income<-employment%>%filter(fullTime == 'yes')%>%
        group_by(DEAR)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), # Apply weighted median
                     list(~ wtd.quantile(PERNP, weights = ., probs = 0.5)))
      
      num_obs<-employment%>%filter(fullTime == 'yes')%>%
        group_by(DEAR)%>%
        summarise(n = n())
      
      general_income<-left_join(general_income,num_obs, 
                                by=c('DEAR'))
      
      general_income<-calculate_regular_me(general_income,start = 3, end = 82, 
                                           alpha = 0.05)%>%
        bind_cols(general_income)%>%
        select(DEAR,PWGTP,margin_errors,n)
      
      general_income<-general_income%>%rename('attribution' = 'DEAR')%>%
        mutate(median_income = PWGTP)%>%
        mutate(status = 'earning', type = 'salary-range',variable = 'overall',
               state = 'United States')%>%
        select(attribution,status,median_income,margin_errors,variable,type,state,n)
      
      # Age Salary Range
      age_income<-employment%>%filter(fullTime == 'yes')%>%
        group_by(DEAR,AGE_group)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), # Apply weighted median
                     list(~ wtd.quantile(PERNP, weights = ., probs = 0.5)))
      
      num_obs<-employment%>%filter(fullTime == 'yes')%>%
        group_by(DEAR,AGE_group)%>%
        summarise(n = n())
      
      age_income<-left_join(age_income,num_obs, 
                            by=c('DEAR','AGE_group'))
      
      age_income<-calculate_regular_me(age_income,start = 4, end = 83, 
                                       alpha = 0.05)%>%
        bind_cols(age_income)%>%
        select(DEAR,AGE_group,PWGTP,margin_errors,n)
      
      age_income<-age_income%>%
        mutate(attribution = paste(DEAR,AGE_group,sep=': ages '))%>%
        mutate(median_income = PWGTP)%>%
        mutate(status = 'earning', type = 'salary-range',variable = 'age',
               state = 'United States')%>%
        select(attribution,status,median_income,margin_errors,variable,type,state,n)
      
      # Gender Salary Range
      gender_income<-employment%>%filter(fullTime == 'yes')%>%
        group_by(DEAR,SEX)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), # Apply weighted median
                     list(~ wtd.quantile(PERNP, weights = ., probs = 0.5)))
      
      num_obs<-employment%>%filter(fullTime == 'yes')%>%
        group_by(DEAR,SEX)%>%
        summarise(n = n())
      
      gender_income<-left_join(gender_income,num_obs, 
                               by=c('DEAR','SEX'))
      
      gender_income<-calculate_regular_me(gender_income,start = 4, end = 83, 
                                          alpha = 0.05)%>%
        bind_cols(gender_income)%>%
        select(DEAR,SEX,PWGTP,margin_errors,n)
      
      gender_income<-gender_income%>%
        mutate(SEX = recode(SEX,'female'='women','male'='men'))%>%
        mutate(attribution = paste(DEAR,SEX,sep=' '),median_income = PWGTP)%>%
        select(attribution,median_income,margin_errors,n)%>%
        mutate(status = 'earning', type = 'salary-range',variable = 'gender',
               state = 'United States')%>%
        select(attribution,status,median_income,margin_errors,variable,type,state,n)
      
      # Race
      race_income<-employment%>%filter(fullTime == 'yes')%>%
        group_by(DEAR,RACETH)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), # Apply weighted median
                     list(~ wtd.quantile(PERNP, weights = ., probs = 0.5)))
      
      num_obs<-employment%>%filter(fullTime == 'yes')%>%
        group_by(DEAR,RACETH)%>%
        summarise(n = n())
      
      race_income<-left_join(race_income,num_obs, 
                             by=c('DEAR','RACETH'))
      
      race_income<-calculate_regular_me(race_income,start = 4, end = 83, 
                                        alpha = 0.05)%>%
        bind_cols(race_income)%>%
        select(DEAR,RACETH,PWGTP,margin_errors,n)
      
      race_income<-race_income%>%
        mutate(RACETH = recode(RACETH,'Asian/Pacific Islander'='Asian',
                               'Other Race/Multiracial'='multiracial',
                               'White'='white'))%>%
        mutate(attribution = paste(DEAR,RACETH,sep=' '), median_income = PWGTP)%>%
        select(attribution,median_income,margin_errors,n)%>%
        mutate(status = 'earning', variable = 'race', type = 'salary-range', 
               state = 'United States')
      
      # Disability
      dis_income<-employment%>%
        mutate(PLUS = ifelse((DDRS == 'Yes' |     # self-care difficulty
                                DOUT == 'Yes' |   # independent living difficulty
                                DPHY == 'Yes' |   # ambulatory difficulty
                                DREM == 'Yes') &  # cognitive difficulty
                               DEYE == 'Sighted',# deafdisabled only
                             'disabled',
                             ifelse((DDRS == 'No' |    # deaf only
                                       DOUT == 'No' |
                                       DPHY == 'No' |
                                       DREM == 'No') & 
                                      DEYE == 'Sighted',
                                    'no disability',
                                    ifelse(DEYE == 'Blind',
                                           'blind',NA))))%>%
        filter(fullTime == 'yes')%>%
        group_by(DEAR,PLUS)%>%
        summarise_at(toupper(c('pwgtp',pwgtps)), # Apply weighted median
                     list(~ wtd.quantile(PERNP, weights = ., probs = 0.5)))
      
      num_obs<-employment%>%filter(fullTime == 'yes')%>%
        mutate(PLUS = ifelse((DDRS == 'Yes' |     # self-care difficulty
                                DOUT == 'Yes' |   # independent living difficulty
                                DPHY == 'Yes' |   # ambulatory difficulty
                                DREM == 'Yes') &  # cognitive difficulty
                               DEYE == 'Sighted',# deafdisabled only
                             'disabled',
                             ifelse((DDRS == 'No' |    # deaf only
                                       DOUT == 'No' |
                                       DPHY == 'No' |
                                       DREM == 'No') & 
                                      DEYE == 'Sighted',
                                    'no disability',
                                    ifelse(DEYE == 'Blind',
                                           'blind',NA))))%>%
        group_by(DEAR,PLUS)%>%
        summarise(n = n())
      
      dis_income<-left_join(dis_income,num_obs, 
                            by=c('DEAR','PLUS'))
      
      dis_income<-calculate_regular_me(dis_income,start = 4, end = 83, 
                                       alpha = 0.05)%>%
        bind_cols(dis_income)%>%
        select(DEAR,PLUS,PWGTP,margin_errors,n)
      
      dis_income<-dis_income%>%
        mutate(attribution = paste(DEAR,PLUS,sep=' '),median_income = PWGTP)%>%
        mutate(attribution = recode(attribution,
                                    'deaf blind'='deafblind',
                                    'deaf disabled'='deafdisabled',
                                    'deaf no disability'='deaf with no additional disabilities',
                                    'hearing no disability' = 'hearing with no additional disabilities'))%>%
        select(attribution,median_income,margin_errors,n)%>%
        mutate(status = 'earning', variable = 'disability', type = 'salary-range', 
               state = 'United States')
      
      incomes<-bind_rows(general_income,gender_income,race_income,dis_income,
                         age_income)
      
      return(bind_rows(emp_result,edu_result,incomes))
    }
  }
}

# Analyze 13 years before most recent year- most recent year -------------------

thetime<-data.frame()
for(i in 0:range){
  time_result<-time_series(year_n-i)
  time_result$year <- year_n-i
  
  thetime<-bind_rows(thetime,time_result)
  print(paste('Done with',year_n-i,'data analysis'))
}
#Write time series and save file in JSON syntax for .../assets------------------
dir <- paste(getwd(),'time.csv',sep='/')
write.csv(thetime,dir)
#------