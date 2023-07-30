# Data Preparation--------------------------------------------------------------
library(data.table)
library(dplyr)
library(tidyr)
library(tidycensus)
library(readr)

# Get data directory and read pus and hus
destfile <- paste(getwd(),'Assets','2021_5yr_csv_pus.zip',sep='/')
hdestfile <- paste(getwd(),'Assets','2021_5yr_csv_hus.zip',sep='/')
unzipped <- unzip(destfile)
hunzipped <- unzip(hdestfile)

vector<-grep('.csv',unzipped)
hvector<-grep('.csv',hunzipped)

# Quicker way to write pwgtps
pwgtps<-c()
for(i in 1:80){
  pwgtps[i]<-paste0('PWGTP',i)
}

# Merge four datasets by rows
mega_df <- data.frame()
for(i in vector){
  df<-fread(unzipped[i],
            select = c('SERIALNO',
                       'AGEP','DEAR','SCH','SCHL',
                       'SCHG','ESR','RAC1P','DEYE',
                       'DOUT','DDRS','DPHY','DREM',
                       'LANX','MSP','CIT','SEX',
                       'WKWN','WKHP','SSIP','SSP',
                       'PWGTP','HISP','MAR','PERNP',
                       'COW','INDP','FOD1P','LAPTOP',
                       'FER'
            )
  )
  mega_df<-bind_rows(mega_df,df)
}

hmega_df <- data.frame()
for(i in hvector){
  df<-fread(hunzipped[i],
            select = c('SERIALNO',
                       'LAPTOP','BROADBND','PARTNER','FS',
                       'COMPOTHX','TABLET','FPARC','MULTG',
                       'HUPAC','HUPAOC','ACCESSINET','NRC',
                       'NP','HINCP'
            )
  )
  hmega_df<-bind_rows(hmega_df,df)
}

df<-mega_df%>%left_join(hmega_df, by = 'SERIALNO')
remove(mega_df,hmega_df)

# Data Recoding-----------------------------------------------------------------
# Recode
df$HISPexact <- df$HISP
df<-df%>%
  mutate(DEAR = ifelse(DEAR == 1, "deaf", "hearing"))%>%# DEAR recode
  mutate(RAC1P = recode(RAC1P, '1' = 'White',             # RAC1P recode
                        '2' = 'Black',
                        '3' = 'Native American',
                        '6' = 'Asian/Pacific Islander',
                        '7' = 'Asian/Pacific Islander',
                        '8' = 'Other Race',
                        '9' = 'Multiracial',
                        .default = 'Native American'))%>%
  mutate(HISP = ifelse(HISP %in% c(1,23), 'Not Latinx','Latinx'))%>% # HISP recode
  mutate(SEX = ifelse(SEX == 1,'male','female'))%>%   # SEX recode
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
  mutate(PLUS = ifelse((DDRS == 'Yes' |     # self-care difficulty
                          DOUT == 'Yes' |   # independent living difficulty
                          DPHY == 'Yes' |   # ambulatory difficulty
                          DREM == 'Yes') &  # cognitive difficulty
                         DEYE == 'Sighted',# deafdisabled only
                       'disabled',
                       ifelse((DDRS == 'No' &    # deaf only
                                 DOUT == 'No' &
                                 DPHY == 'No' &
                                 DREM == 'No') & 
                                DEYE == 'Sighted',
                              'no disability',
                              ifelse(DEYE == 'Blind',
                                     'blind',NA))))%>%
  mutate(origin = ifelse(
    CIT == 1,
    'born in US',
    'not born in US')
  )%>%
  mutate(fullTime = ifelse(WKWN %in% c(50,51,52) & WKHP >= 35,'yes','any'))

df<-df%>%
  mutate(disability = ifelse(
    PLUS == 'no disability',
    'No',"Yes"
  ))

dfHISP<-df%>%filter(HISP == 'Latinx')

HISPrecoded<-pums_variables%>%filter(var_code == 'HISP' & year == 2021)%>%
  select(val_min, val_label)
HISPrecoded$val_min<-as.integer(HISPrecoded$val_min)

df<-df%>%left_join(HISPrecoded, by = c('HISPexact'='val_min'))
dfHISP<-dfHISP%>%left_join(HISPrecoded, by = c('HISPexact'='val_min'))

# Demographics Tables: 1, 1.1, 2, 2.2-------------------------------------------
# Table 1
resultRAC<-dfHISP %>% group_by(DEAR,RAC1P)%>%
  summarise(n=n())%>%
  mutate(n = ifelse(n >= 390, n,0))%>%
  group_by(DEAR,RAC1P)%>%
  summarise(n = sum(n))%>%
  filter(DEAR == 'deaf')%>%
  mutate(percentage = paste0(round(n/sum(n)*100,2),'%'))%>%
  arrange(RAC1P)%>%
  mutate(RAC1P = paste0(RAC1P,' Latinx'))

# Table 1.1
result<-dfHISP %>% group_by(DEAR,val_label)%>%
  summarise(n=n())%>%
  mutate(val_label = ifelse(n >= 390, val_label,'All Other Spanish/Hispanic/Latino'))%>%
  group_by(DEAR,val_label)%>%
  summarise(n = sum(n))%>%
  filter(DEAR == 'deaf')%>%
  mutate(percentage = paste0(round(n/sum(n)*100,2),'%'))%>%
  arrange(desc(n))%>%
  unite(col = 'Hispanic group', c('DEAR','val_label'),sep = ' ')

# Table 2
threeGroupCalculator <- function(x,z){
  result<-df%>%
    group_by(DEAR,HISP,{{x}})%>%
    summarise(n = n())
  
  result<-na.omit(result)
  
  HISPN <- result%>%
    group_by(DEAR,HISP)%>%
    summarise(N = sum(n))
  
  result<-result%>%
    left_join(HISPN, by = c('DEAR','HISP'))%>%
    mutate(percentage = paste0(round(n/N*100,2),'%'))%>%
    rename('Category' = z)%>%
    unite('Group',c('DEAR','HISP'),sep = ' ')%>%
    select(-n,-N)%>%
    pivot_wider(names_from = Group, values_from = percentage)
  
  return(result)
}

df<-df%>% # Create SSI variable
  mutate(SSI = ifelse(is.na(SSIP) & is.na(SSP),NA,
                      ifelse(SSIP > 0 | SSIP > 0,'Receives SSI',
                             'Does not receive SSI')),
         language = ifelse(LANX == 1, 'Speak another language at home',
                    ifelse(LANX == 2, 'Speak English only',NA)))

resultPLUS<-threeGroupCalculator(PLUS,'PLUS')           # Disability
resultOrigin <- threeGroupCalculator(origin,'origin')   # origin
resultLanx <- threeGroupCalculator(language,'language') # language
resultSSI <- threeGroupCalculator(SSI,'SSI')            # SSI


result2<-bind_rows(resultPLUS,resultSSI,resultOrigin,resultLanx)

# Employment Rates and Median Full-time earning---------------------------------
# Employment status
employed_df <- df%>%
  filter(AGEP > 15 & AGEP < 65)

resultESR<-employed_df%>%
  group_by(DEAR,HISP,ESR)%>%
  summarise(n = n(), PWGTP = sum(PWGTP))

ESRN<-resultESR%>%
  na.omit()%>%
  group_by(DEAR,HISP)%>%
  summarise(N = n(), NPWGTP = sum(PWGTP))

resultESR<-resultESR%>%
  left_join(ESRN, by = c('DEAR','HISP'))%>%
  na.omit()%>%
  mutate(rates = paste0(round(PWGTP/NPWGTP*100,2),'%'))%>%
  mutate(HISP = ifelse(HISP == 'Not Latinx', 'non-Latinx',HISP))%>%
  unite(col = 'Attribution', c('DEAR','HISP'), sep = ' ')%>%
  select(-n,-N,-PWGTP,-NPWGTP)%>%
  pivot_wider(names_from = Attribution, values_from = rates)

# Earning
resultEarning<-employed_df%>%
  filter(fullTime == 'yes')%>%
  group_by(DEAR,HISP)%>%
  summarise(earning = median(PERNP))%>%
  mutate(HISP = ifelse(HISP == 'Not Latinx', 'non-Latinx',HISP))%>%
  unite(col = 'Attribution', c('DEAR','HISP'), sep = ' ')%>%
  pivot_wider(names_from = Attribution, values_from = earning)

# Employment status by gender
resultESRbyGender<-employed_df%>%
  group_by(DEAR,HISP,SEX,ESR)%>%
  summarise(n = n(), PWGTP = sum(PWGTP))

ESRN<-resultESRbyGender%>%
  na.omit()%>%
  group_by(DEAR,HISP,SEX)%>%
  summarise(N = sum(n), NPWGTP = sum(PWGTP))

resultESRbyGender<-resultESRbyGender%>%
  left_join(ESRN, by = c('DEAR','HISP','SEX'))%>%
  na.omit()%>%
  mutate(rates = paste0(round(PWGTP/NPWGTP*100,2),'%'))%>%
  mutate(HISP = ifelse(HISP == 'Not Latinx', 'non-Latinx',HISP))%>%
  unite(col = 'Attribution', c('DEAR','HISP'), sep = ' ')%>%
  select(-n,-N,-PWGTP,-NPWGTP)%>%
  pivot_wider(names_from = Attribution, values_from = rates)

# Earning by Gender
resultEarningbyGender<-employed_df%>%
  filter(fullTime == 'yes')%>%
  group_by(DEAR,HISP,SEX)%>%
  summarise(earning = median(PERNP))%>%
  mutate(HISP = ifelse(HISP == 'Not Latinx', 'non-Latinx',HISP))%>%
  unite(col = 'Attribution', c('DEAR','HISP'), sep = ' ')%>%
  pivot_wider(names_from = Attribution, values_from = earning)

# Household earning
resultEarninginHH<-employed_df%>%
  na.omit()%>%
  group_by(DEAR,HISP)%>%
  summarise(earning = as.character(median(HINCP)))%>%
  mutate(HISP = ifelse(HISP == 'Not Latinx', 'non-Latinx',HISP))%>%
  unite(col = 'Attribution', c('DEAR','HISP'), sep = ' ')%>%
  pivot_wider(names_from = Attribution, values_from = earning)

# Multigenerational
resultMULTGinHH<-df%>%
  mutate(MULTG = ifelse(MULTG == 2, 'multig','single'))%>%
  na.omit()%>%
  group_by(DEAR,HISP,MULTG)%>%
  summarise(n = sum(PWGTP))

resultMULTGinHH<-resultMULTGinHH%>%
  group_by(DEAR,HISP)%>%
  summarise(N = sum(n))%>%
  left_join(resultMULTGinHH, by = c('DEAR','HISP'))%>%
  mutate(HISP = ifelse(HISP == 'Not Latinx', 'non-Latinx',HISP))%>%
  mutate(percentage = paste0(round(n/N*100,2),'%'))%>%
  unite(col = 'Attribution', c('DEAR','HISP'), sep = ' ')%>%
  filter(MULTG == 'multig')%>%select(-n,-N,-MULTG)%>%
  pivot_wider(names_from = Attribution, values_from = percentage)

# Average number of people in HH
resultPersonsinHH<-df%>%
  group_by(DEAR,HISP)%>%
  summarise(people = as.character(round(mean(NP),1)))%>%
  mutate(HISP = ifelse(HISP == 'Not Latinx', 'non-Latinx',HISP))%>%
  unite(col = 'Attribution', c('DEAR','HISP'), sep = ' ')%>%
  pivot_wider(names_from = Attribution, values_from = people)

resultEarninginHH$category <- 'Median earning in household'
resultMULTGinHH$category <- 'Percentage of multigenerational household'
resultPersonsinHH$category <- 'Average number of people in household'

bind_rows(resultEarninginHH,resultMULTGinHH,resultPersonsinHH)

# Employment Tables: 7, 7.1, 8--------------------------------------------------
# Table 7
resultPLUS<-employed_df%>%
  group_by(DEAR,HISP,PLUS,ESR)%>%
  summarise(n = n(), PWGTP = sum(PWGTP))

ESRN<-resultPLUS%>%
  na.omit()%>%
  group_by(DEAR,HISP,PLUS)%>%
  summarise(NPWGTP = sum(PWGTP))

resultPLUS<-employed_df%>%
  filter(fullTime == 'yes')%>%
  group_by(DEAR,HISP,PLUS)%>%
  summarise(earning = median(PERNP))%>%
  left_join(resultPLUS, by = c('DEAR','HISP','PLUS'))

resultPLUS<-resultPLUS%>%
  filter(ESR == 'employed')%>%
  left_join(ESRN, by = c('DEAR','HISP','PLUS'))%>%
  mutate(emp_rate = paste0(round(PWGTP/NPWGTP*100,2),'%'))%>%
  rename('Category' = 'PLUS')

# Birth Origin
resultOrigin <- employed_df%>%
  group_by(DEAR,HISP,origin,ESR)%>%
  summarise(n=n(), PWGTP = sum(PWGTP))

ESRN<-resultOrigin%>%
  na.omit()%>%
  group_by(DEAR,HISP,origin)%>%
  summarise(NPWGTP = sum(PWGTP))

resultOrigin<-employed_df%>%
  filter(fullTime == 'yes')%>%
  group_by(DEAR,HISP,origin)%>%
  summarise(earning = median(PERNP))%>%
  left_join(resultOrigin, by = c('DEAR','HISP','origin'))%>%
  filter(ESR == 'employed')%>%
  left_join(ESRN, by = c('DEAR','HISP','origin'))%>%
  mutate(emp_rate = paste0(round(PWGTP/NPWGTP*100,2),'%'))%>%
  rename('Category' = 'origin')

# Language
resultLanx <- employed_df%>%
  group_by(DEAR,HISP,LANX,ESR)%>%
  summarise(n=n(), PWGTP = sum(PWGTP))

ESRN<-resultLanx%>%
  na.omit()%>%
  group_by(DEAR,HISP,LANX)%>%
  summarise(NPWGTP = sum(PWGTP))

resultLanx<-employed_df%>%
  filter(fullTime == 'yes')%>%
  group_by(DEAR,HISP,LANX)%>%
  summarise(earning = median(PERNP))%>%
  left_join(resultLanx, by = c('DEAR','HISP','LANX'))%>%
  filter(ESR == 'employed')%>%
  left_join(ESRN, by = c('DEAR','HISP','LANX'))%>%
  mutate(emp_rate = paste0(round(PWGTP/NPWGTP*100,2),'%'),
         LANX = ifelse(LANX == 1, 'Speak another language at home',
                       ifelse(is.na(LANX),NA,'Speak English only')))%>%
  rename('Category' = 'LANX')

# Race
resultRace <- employed_df%>%
  group_by(DEAR,HISP,RAC1P,ESR)%>%
  summarise(n=n(),PWGTP = sum(PWGTP))

ESRN <- resultRace%>%
  group_by(DEAR,HISP,RAC1P)%>%
  summarise(N=sum(n),NPWGTP = sum(PWGTP))%>%
  mutate(NPWGTP = ifelse(N < 390, NA, NPWGTP))

resultRace<-employed_df%>%
  filter(fullTime == 'yes')%>%
  group_by(DEAR,HISP,RAC1P)%>%
  summarise(earning = median(PERNP),n = n())%>%
  mutate(earning = ifelse(n < 390, 0, earning))%>%
  select(-n)%>%
  left_join(resultRace, by = c('DEAR','HISP','RAC1P'))%>%
  filter(ESR == 'employed')%>%
  left_join(ESRN, by = c('DEAR','HISP','RAC1P'))%>%
  mutate(emp_rate = paste0(round(PWGTP/NPWGTP*100,2),'%'))%>%
  rename('Category' = 'RAC1P')%>%
  na.omit()%>%select(-n,-N)

result7<-bind_rows(resultPLUS,resultOrigin,resultLanx,resultRace)

result7<-result7%>%
  filter(HISP == 'Latinx' & DEAR == 'deaf')%>%
  ungroup()%>%
  select(-n,-HISP,-DEAR)

# Table 7.1
unwanted<-c('Not Spanish/Hispanic/Latino','Spaniard')

resultHispExact <- employed_df%>%
  group_by(DEAR,val_label,ESR)%>%
  summarise(n=n(),PWGTP = sum(PWGTP))%>%
  filter(!(val_label %in% unwanted))

ESRN <- resultHispExact%>%
  group_by(DEAR,val_label)%>%
  summarise(N = sum(n), NPWGTP = sum(PWGTP))%>%
  mutate(NPWGTP = ifelse(N < 390, NA, NPWGTP))
  

resultHispExact<-employed_df%>%
  filter(fullTime == 'yes')%>%
  group_by(DEAR,val_label)%>%
  summarise(earning = median(PERNP),n = n())%>%
  mutate(earning = ifelse(n < 390, 0, earning))%>%
  select(-n)%>%
  left_join(resultHispExact, by = c('DEAR','val_label'))%>%
  filter(ESR == 'employed')%>%
  left_join(ESRN, by = c('DEAR','val_label'))%>%
  filter(DEAR == 'deaf' & !(val_label %in% unwanted))%>%
  mutate(emp_rate = paste0(round(PWGTP/NPWGTP*100,2),'%'))%>%
  rename('Category' = 'val_label')%>%
  na.omit()

# Table 8
resultFTPT <- employed_df%>%
  filter(ESR == 'employed')%>%
  group_by(DEAR,HISP,fullTime)%>%
  summarise(n=n(),PWGTP = sum(PWGTP))

ESRN <- resultFTPT%>%
  group_by(DEAR,HISP)%>%
  summarise(N = sum(n), NPWGTP = sum(PWGTP))%>%
  mutate(NPWGTP = ifelse(N < 390, NA, NPWGTP))

resultFTPT<-resultFTPT%>%
  left_join(ESRN, by = c('DEAR','HISP'))%>%
  mutate(emp_rate = paste0(round(PWGTP/NPWGTP*100,2),'%'))%>%
  na.omit()%>%select(-n,-N,-PWGTP,-NPWGTP)%>%
  unite('Group',c('DEAR','HISP'),sep = ' ')%>%
  pivot_wider(names_from = 'fullTime', values_from = 'emp_rate')


# Employment Table 9, 10--------------------------------------------------------
employed<-employed_df%>%
  filter(ESR == 'employed')

employed<-employed%>% # self employee and bizowner
  mutate(selfEmp = ifelse(COW %in% c(6,7), 'Y', 'N'),
         bizOwner = ifelse(COW == 7, 'Y','N'))

# Self Employed and Business Owners
resultSelfEmp<-employed%>%
  group_by(DEAR,HISP,selfEmp)%>%
  summarise(n = n(), PWGTP = sum(PWGTP))

resultbizOwner<-employed%>%
  group_by(DEAR,HISP,bizOwner)%>%
  summarise(nBiz = n(), PWGTPBiz = sum(PWGTP))%>%
  filter(bizOwner == 'Y')
  
ESRN<-resultSelfEmp%>%
  na.omit()%>%
  group_by(DEAR,HISP)%>%
  summarise(NPWGTP = sum(PWGTP))

resultSelfEmp<-resultSelfEmp%>%
  filter(selfEmp == 'Y')%>%
  left_join(resultbizOwner,c('DEAR','HISP'))%>%
  left_join(ESRN, by = c('DEAR','HISP'))%>%
  mutate(selfEmpRate = paste0(round(PWGTP/NPWGTP*100,2),'%'),
         selfBizRate = paste0(round(PWGTPBiz/NPWGTP*100,2),'%'))%>%
  unite('Group',c('DEAR','HISP'),sep = ' ')%>%
  ungroup()%>%select(Group,selfEmpRate,selfBizRate)

# Table 10
recoded_industry<-pums_variables%>%
  filter(var_code == 'INDP' & year == 2021)%>%
  select(val_min,val_label)%>%
  mutate(val_label = gsub('-.*','',val_label))

recoded_industry<-recoded_industry%>%
  mutate(complete_category = recode(val_label,
  "ADM" = "Public Administration",
  "AGR" = "Agriculture",
  "CON" = "Construction and Extraction",
  "EDU" = "Education",
  "ENT" = "Entertainment, Food Services and Accommodation",
  "EXT" = "Construction and Extraction",
  "FIN" = "Finance, Real Estate and Rental",
  "INF" = "Information",
  "MED" = "Healthcare",
  "MFG" = "Manufacturing",
  "MIL" = "Military",
  "PRF" = "Scientific, and Management, and Waste Management",
  "RET" = "Retail Trade",
  "SCA" = "Social Assistance",
  "SRV" = "Other Services, Except Public Administration",
  "TRN" = "Transportation",
  "UTL" = "Utilities",
  "WHL" = "Wholesale Trade"))

recoded_industry$val_min<-as.integer(recoded_industry$val_min)
  
employed<-employed%>%
  left_join(recoded_industry, by = c('INDP' = 'val_min'))

ESRN<-employed%>%
  group_by(DEAR,HISP)%>%
  summarise(NPWGTP = sum(PWGTP))

resultIndp<-employed%>%
  group_by(DEAR,HISP,complete_category)%>%
  summarise(PWGTP = sum(PWGTP))%>%
  left_join(ESRN, by = c('DEAR','HISP'))%>%
  mutate(PWGTP = paste0(round(PWGTP/NPWGTP*100,2),'%'))%>%
  select(-NPWGTP)%>%
  pivot_wider(names_from = c('DEAR','HISP'), values_from = 'PWGTP')

# Employment and earning by educational level by Latinx-------------------------
# Employment Rate
employed_df<-employed_df%>%
  mutate(SCHL = ifelse(SCHL < 16,'no HS diploma', 
                ifelse(SCHL <= 17,'HS diploma',
                ifelse(SCHL < 20,'some college',
                ifelse(SCHL == 20,'associate',
                ifelse(SCHL == 21,'bachelor', 
                ifelse(SCHL <= 22,'master',
                ifelse(SCHL > 22, 'phd/dr',NA))))))))

resultEdESR<-employed_df%>%
  group_by(DEAR,HISP,SCHL,ESR)%>%
  summarise(n = n(), PWGTP = sum(PWGTP))

ESRN <- resultEdESR%>%
  group_by(DEAR,HISP,SCHL)%>%
  summarise(N = sum(n), NPWGTP = sum(PWGTP))

resultEdESR<-resultEdESR%>%
  left_join(ESRN, by = c('DEAR','HISP','SCHL'))%>%
  mutate(emp_rate = paste0(round(PWGTP/NPWGTP*100,2),'%'))%>%
  select(-n,-N,-NPWGTP,-PWGTP)%>%
  filter(ESR == 'employed')%>%
  arrange(DEAR,HISP,match(SCHL, 
                          rev(c("phd/dr", "master",
                            "bachelor","associate",
                            "some college","HS diploma", 
                            "no HS diploma"))))%>%
  unite(col = 'Category', c('DEAR','HISP'), sep = ' ')%>%
  pivot_wider(names_from = SCHL, values_from = 'emp_rate')

# Median Earning
employed<-employed%>%
  mutate(SCHL = ifelse(SCHL < 16,'no HS diploma', 
                ifelse(SCHL <= 17,'HS diploma',
                ifelse(SCHL < 20,'some college',
                ifelse(SCHL == 20,'associate',
                ifelse(SCHL == 21,'bachelor', 
                ifelse(SCHL <= 22,'master',
                ifelse(SCHL > 22, 'phd/dr',NA))))))))
resultEdEarn<-employed%>%
  filter(fullTime == 'yes')%>%
  group_by(DEAR,HISP,SCHL)%>%
  summarise(n = n(),earning = median(PERNP))%>%
  arrange(DEAR,HISP,match(SCHL, 
               rev(c("phd/dr", "master",
                     "bachelor","associate",
                     "some college","HS diploma", 
                     "no HS diploma"))))%>%
  mutate(earning = ifelse(n < 390, NA, earning))%>%
  select(-n)%>%  unite(col = 'Category', c('DEAR','HISP'), sep = ' ')%>%
  pivot_wider(names_from = SCHL, values_from = 'earning')

# Education Attainment----------------------------------------------------------
education<-employed_df%>%
  filter(AGEP > 24)

# General
resultEd<-education%>%
  group_by(DEAR,HISP,SCHL)%>%
  summarise(n = n(), PWGTP= sum(PWGTP))%>%
  arrange(DEAR,match(SCHL, 
                     c("phd/dr", "master",
                       "bachelor","associate",
                       "some college","HS diploma", 
                       "no HS diploma")))%>%
  mutate(PWGTP = cumsum(PWGTP), n = cumsum(n))


resultEd<-resultEd%>%
  filter(SCHL == 'no HS diploma')%>%
  mutate(NPWGTP = PWGTP)%>%
  select(-n,-PWGTP,-SCHL)%>%
  left_join(resultEd, by = c('DEAR','HISP'))%>%
  mutate(percentage = paste0(round(PWGTP/NPWGTP*100,2),'%'))%>%
  select(-n,-PWGTP,-NPWGTP)%>%
  unite(col = 'Category', c('DEAR','HISP'), sep = ' ')%>%
  filter(SCHL != 'no HS diploma')%>%
  pivot_wider(names_from = SCHL, values_from = 'percentage')

education<-education%>%mutate(AGE_group = 
                              ifelse(AGEP >= 25 & AGEP <= 34, '25-34',
                              ifelse(AGEP >= 35 & AGEP <= 44, '35-44',
                              ifelse(AGEP >= 45 & AGEP <= 54, '45-54',
                                                              '55-64'))))

# Age
resultEdAge<-education%>%
  group_by(DEAR,HISP,AGE_group,SCHL)%>%
  summarise(n = n(), PWGTP= sum(PWGTP))%>%
  arrange(DEAR,match(SCHL, 
                     c("phd/dr", "master",
                       "bachelor","associate",
                       "some college","HS diploma", 
                       "no HS diploma")))%>%
  mutate(PWGTP = cumsum(PWGTP), n = cumsum(n))


resultEdAge<-resultEdAge%>%
  filter(SCHL == 'no HS diploma')%>%
  mutate(NPWGTP = ifelse(n < 390, NA, PWGTP))%>%
  select(-n,-PWGTP,-SCHL)%>%
  left_join(resultEdAge, by = c('DEAR','HISP','AGE_group'))%>%
  mutate(percentage = paste0(round(PWGTP/NPWGTP*100,2),'%'))%>%
  select(-n,-PWGTP,-NPWGTP)%>%
  unite(col = 'Category', c('DEAR','HISP'), sep = ' ')%>%
  filter(Category %in% c('deaf Latinx','hearing Latinx'))%>%
  filter(SCHL != 'no HS diploma')%>%
  pivot_wider(names_from = c('AGE_group','Category'), values_from = 'percentage')

resultEdAge<-education%>%
  group_by(DEAR,HISP,AGE_group,SCHL)%>%
  summarise(n = n(), PWGTP= sum(PWGTP))%>%
  arrange(DEAR,match(SCHL, 
                     c("phd/dr", "master",
                       "bachelor","associate",
                       "some college","HS diploma", 
                       "no HS diploma")))%>%
  mutate(PWGTP = cumsum(PWGTP), n = cumsum(n))


resultEdAge<-resultEdAge%>%
  filter(SCHL == 'no HS diploma')%>%
  mutate(NPWGTP = ifelse(n < 390, NA, PWGTP))%>%
  select(-n,-PWGTP,-SCHL)%>%
  left_join(resultEdAge, by = c('DEAR','HISP','AGE_group'))%>%
  mutate(percentage = paste0(round(PWGTP/NPWGTP*100,2),'%'))%>%
  select(-n,-PWGTP,-NPWGTP)%>%
  unite(col = 'Category', c('DEAR','HISP'), sep = ' ')%>%
  filter(Category %in% c('deaf Not Latinx','hearing Not Latinx'))%>%
  filter(SCHL != 'no HS diploma')%>%
  pivot_wider(names_from = c('AGE_group','Category'), values_from = 'percentage')

# Gender
resultEdSex<-education%>%
  group_by(DEAR,HISP,SEX,SCHL)%>%
  summarise(n = n(), PWGTP= sum(PWGTP))%>%
  arrange(DEAR,match(SCHL, 
                     c("phd/dr", "master",
                       "bachelor","associate",
                       "some college","HS diploma", 
                       "no HS diploma")))%>%
  mutate(PWGTP = cumsum(PWGTP), n = cumsum(n))


resultEdSex<-resultEdSex%>%
  filter(SCHL == 'no HS diploma')%>%
  mutate(NPWGTP = ifelse(n < 390, NA, PWGTP))%>%
  select(-n,-PWGTP,-SCHL)%>%
  left_join(resultEdSex, by = c('DEAR','HISP','SEX'))%>%
  mutate(percentage = paste0(round(PWGTP/NPWGTP*100,2),'%'))%>%
  select(-n,-PWGTP,-NPWGTP)%>%
  unite(col = 'Category', c('DEAR','HISP'), sep = ' ')%>%
  filter(Category %in% c('deaf Latinx','hearing Latinx'))%>%
  filter(SCHL != 'no HS diploma')%>%
  pivot_wider(names_from = 'SCHL', values_from = 'percentage')

# Education Attainment Table 6--------------------------------------------------
# Disability
resultEdPlus<-education%>%
  group_by(DEAR,HISP,PLUS,SCHL)%>%
  summarise(n = n(), PWGTP= sum(PWGTP))%>%
  arrange(DEAR,match(SCHL, 
                     c("phd/dr", "master",
                       "bachelor","associate",
                       "some college","HS diploma", 
                       "no HS diploma")))%>%
  mutate(PWGTP = cumsum(PWGTP), n = cumsum(n))


resultEdPlus<-resultEdPlus%>%
  filter(SCHL == 'no HS diploma')%>%
  mutate(NPWGTP = ifelse(n < 390, NA, PWGTP))%>%
  select(-n,-PWGTP,-SCHL)%>%
  left_join(resultEdPlus, by = c('DEAR','HISP','PLUS'))%>%
  mutate(percentage = paste0(round(PWGTP/NPWGTP*100,2),'%'))%>%
  select(-n,-PWGTP,-NPWGTP)%>%
  filter(HISP == 'Latinx' & DEAR == 'deaf')%>%
  filter(SCHL != 'no HS diploma')%>%
  ungroup()%>%select(-DEAR,-HISP)%>%rename('Category'= 'PLUS')%>%
  pivot_wider(names_from = 'SCHL', values_from = 'percentage')

# Birth Origin
resultEdOrigin<-education%>%
  group_by(DEAR,HISP,origin,SCHL)%>%
  summarise(n = n(), PWGTP= sum(PWGTP))%>%
  arrange(DEAR,match(SCHL, 
                     c("phd/dr", "master",
                       "bachelor","associate",
                       "some college","HS diploma", 
                       "no HS diploma")))%>%
  mutate(PWGTP = cumsum(PWGTP), n = cumsum(n))


resultEdOrigin<-resultEdOrigin%>%
  filter(SCHL == 'no HS diploma')%>%
  mutate(NPWGTP = ifelse(n < 390, NA, PWGTP))%>%
  select(-n,-PWGTP,-SCHL)%>%
  left_join(resultEdOrigin, by = c('DEAR','HISP','origin'))%>%
  mutate(percentage = paste0(round(PWGTP/NPWGTP*100,2),'%'))%>%
  select(-n,-PWGTP,-NPWGTP)%>%
  filter(HISP == 'Latinx' & DEAR == 'deaf')%>%
  filter(SCHL != 'no HS diploma')%>%
  ungroup()%>%select(-DEAR,-HISP)%>%rename('Category'= 'origin')%>%
  pivot_wider(names_from = 'SCHL', values_from = 'percentage')

# Language
resultEdLang<-education%>%
  group_by(DEAR,HISP,LANX,SCHL)%>%
  summarise(n = n(), PWGTP= sum(PWGTP))%>%
  arrange(DEAR,match(SCHL, 
                     c("phd/dr", "master",
                       "bachelor","associate",
                       "some college","HS diploma", 
                       "no HS diploma")))%>%
  mutate(PWGTP = cumsum(PWGTP), n = cumsum(n))


resultEdLang<-resultEdLang%>%
  filter(SCHL == 'no HS diploma')%>%
  mutate(NPWGTP = ifelse(n < 390, NA, PWGTP))%>%
  select(-n,-PWGTP,-SCHL)%>%
  left_join(resultEdLang, by = c('DEAR','HISP','LANX'))%>%
  mutate(percentage = paste0(round(PWGTP/NPWGTP*100,2),'%'),
         LANX = ifelse(LANX == 1, 'Speak another language at home',
                       ifelse(is.na(LANX),NA,'Speak English only')))%>%
  select(-n,-PWGTP,-NPWGTP)%>%
  filter(HISP == 'Latinx' & DEAR == 'deaf')%>%
  filter(SCHL != 'no HS diploma')%>%
  ungroup()%>%select(-DEAR,-HISP)%>%rename('Category'= 'LANX')%>%
  pivot_wider(names_from = 'SCHL', values_from = 'percentage')

# Race
resultEdRace<-education%>%
  group_by(DEAR,HISP,RAC1P,SCHL)%>%
  summarise(n = n(), PWGTP= sum(PWGTP))%>%
  arrange(DEAR,match(SCHL, 
                     c("phd/dr", "master",
                       "bachelor","associate",
                       "some college","HS diploma", 
                       "no HS diploma")))%>%
  mutate(PWGTP = cumsum(PWGTP), n = cumsum(n))


resultEdRace<-resultEdRace%>%
  filter(SCHL == 'no HS diploma')%>%
  mutate(NPWGTP = ifelse(n < 390, NA, PWGTP))%>%
  select(-n,-PWGTP,-SCHL)%>%
  left_join(resultEdRace, by = c('DEAR','HISP','RAC1P'))%>%
  mutate(percentage = paste0(round(PWGTP/NPWGTP*100,2),'%'))%>%
  select(-n,-PWGTP,-NPWGTP)%>%
  filter(HISP == 'Latinx' & DEAR == 'deaf')%>%
  filter(SCHL != 'no HS diploma')%>%
  ungroup()%>%select(-DEAR,-HISP)%>%rename('Category'= 'RAC1P')%>%
  pivot_wider(names_from = 'SCHL', values_from = 'percentage')

# Bind rows
result16 <- bind_rows(resultEdPlus,resultEdOrigin,resultEdLang,resultEdRace)


# Disability
resultEdExact<-education%>%
  group_by(DEAR,HISPexact,SCHL)%>%
  summarise(n = n(), PWGTP= sum(PWGTP))%>%
  arrange(DEAR,match(SCHL, 
                     c("phd/dr", "master",
                       "bachelor","associate",
                       "some college","HS diploma", 
                       "no HS diploma")))%>%
  mutate(PWGTP = cumsum(PWGTP), n = cumsum(n))


resultEdExact<-resultEdExact%>%
  filter(SCHL == 'no HS diploma')%>%
  mutate(NPWGTP = ifelse(n < 390, NA, PWGTP))%>%
  select(-n,-PWGTP,-SCHL)%>%
  left_join(resultEdExact, by = c('DEAR','HISPexact'))%>%
  mutate(percentage = paste0(round(PWGTP/NPWGTP*100,2),'%'))%>%
  select(-n,-PWGTP,-NPWGTP)%>%
  filter(DEAR == 'deaf')%>%
  filter(SCHL != 'no HS diploma')%>%
  ungroup()%>%select(-DEAR)%>%
  pivot_wider(names_from = 'SCHL', values_from = 'percentage')%>%
  left_join(HISPrecoded, by = c('HISPexact'='val_min'))%>%
  select(-HISPexact)
  

# Field of Degree---------------------------------------------------------------
filepath<-paste(getwd(),'Assets/fod_recode.csv',sep='/')
fod_recode<-read_csv(filepath)
education$FOD1P<-as.character(education$FOD1P)
fod_recode$Code<-as.character(fod_recode$Code)

education<-education%>%
  left_join(fod_recode, by = c('FOD1P'='Code'))%>%
  filter(!is.na(FOD))

resultFOD<-education%>%
  filter(SCHL %in% c('bachelor','master','phd/dr'))%>%
  group_by(DEAR,HISP,Subgroup)%>%
  summarise(PWGTP = sum(PWGTP), n = n())

resultFOD<-resultFOD%>%
  mutate(Subgroup = ifelse(n < 30, 'Other', Subgroup))%>%
  group_by(DEAR,HISP,Subgroup)%>%
  summarise(PWGTP = sum(PWGTP))

resultFOD<-resultFOD%>%
  group_by(DEAR,HISP)%>%
  summarise(N = sum(PWGTP))%>%
  left_join(resultFOD, by = c('DEAR','HISP'))%>%
  mutate(percentage = paste0(round(PWGTP/N*100,2),'%'))%>%
  select(-N,-PWGTP)%>%unite('Category',c('DEAR','HISP'), sep = ' ')%>%
  pivot_wider(names_from = Category, values_from = percentage)
  
  

# Final remaining tables--------------------------------------------------------
education<-education%>%
  mutate(SCH = ifelse(SCHG > 14,'Enrolled','Not Enrolled'))

# General Enrollment
resultEnroll<-education%>%
  group_by(DEAR,HISP,SCH)%>%
  summarise(PWGTP = sum(PWGTP), n = n())

resultEnroll<-resultEnroll%>%
  group_by(DEAR,HISP)%>%
  summarise(N = sum(PWGTP))%>%
  left_join(resultEnroll, by = c('DEAR','HISP'))%>%
  mutate(percentage = paste0(round(PWGTP/N*100,2),'%'))%>%
  select(-n,-N,-PWGTP)%>%filter(SCH == 'Enrolled')%>%
  unite('Category', c('DEAR','HISP'),sep = ' ')

# Gender Enrollment
resultEnrollbySex<-education%>%
  group_by(DEAR,HISP,SEX,SCH)%>%
  summarise(PWGTP = sum(PWGTP), n = n())

resultEnrollbySex<-resultEnrollbySex%>%
  group_by(DEAR,HISP,SEX)%>%
  summarise(N = sum(PWGTP))%>%
  left_join(resultEnrollbySex, by = c('DEAR','HISP','SEX'))%>%
  mutate(percentage = paste0(round(PWGTP/N*100,2),'%'))%>%
  select(-n,-N,-PWGTP)%>%filter(SCH == 'Enrolled')%>%
  unite('Category', c('DEAR','HISP'),sep = ' ')%>%
  pivot_wider(names_from = SEX, values_from = percentage)


# Time Series-------------------------------------------------------------------
library(ggplot2)

thetime<-read.csv(paste0(getwd(),'/time.csv'))

# HS/GED Deaf and Hearing Latinx
resultHSGED<-thetime%>%
  filter(status == 'HS diploma' & attribution %in% c("deaf Latinx","hearing Latinx"))

write.csv(resultHSGED%>%
  select(attribution,percentage,year)%>%
  pivot_wider(names_from = year, values_from = percentage),file = 'output.csv')

resultHSGED$attribution <- factor(resultHSGED$attribution, levels = c("hearing Latinx",
                                                 "deaf Latinx"))

ggplot(data = resultHSGED, aes(x = year, y = percentage/100, col = attribution))+
  geom_line()+geom_point()+
  geom_ribbon(aes(ymin=percentage/100-margin_errors/100,
                  ymax=percentage/100+margin_errors/100,fill=attribution),
              alpha=0.3,size = 0.2)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(breaks = round(seq(min(resultHSGED$year), max(resultHSGED$year), by = 1),1))+
  scale_fill_manual(values = c('#414042','#00A79D'))+
  scale_color_manual(values = c('#414042','#00A79D'))+
  xlab('')+ylab('')+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white"),
        text = element_text(size = 12, family ='Roboto'),
        axis.text = element_text(size = 12, family ='Roboto'),
        plot.caption = element_text(face = "italic"),
        panel.spacing.x = unit(6, "mm"),
        legend.position = 'top',
        legend.title = element_blank())

ggsave(
  paste('Charts/hsged.png',sep='/'),
  width = 3000,
  height = 1500,
  units = 'px',
  dpi = 300
)

# Bachelor Deaf and Hearing Latinx
resultBA<-thetime%>%
  filter(status == 'bachelor' & attribution %in% c("deaf Latinx","hearing Latinx"))

write.csv(resultBA%>%
            select(attribution,percentage,year)%>%
            pivot_wider(names_from = year, values_from = percentage),file = 'output.csv')

resultBA$attribution <- factor(resultBA$attribution, levels = c("hearing Latinx",
                                                                      "deaf Latinx"))

ggplot(data = resultBA, aes(x = year, y = percentage/100, col = attribution))+
  geom_line()+geom_point()+
  geom_ribbon(aes(ymin=percentage/100-margin_errors/100,
                  ymax=percentage/100+margin_errors/100,fill=attribution),
              alpha=0.3,size = 0.2)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(breaks = round(seq(min(resultBA$year), max(resultBA$year), by = 1),1))+
  scale_fill_manual(values = c('#414042','#00A79D'))+
  scale_color_manual(values = c('#414042','#00A79D'))+
  xlab('')+ylab('')+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white"),
        text = element_text(size = 12, family ='Roboto'),
        axis.text = element_text(size = 12, family ='Roboto'),
        plot.caption = element_text(face = "italic"),
        panel.spacing.x = unit(6, "mm"),
        legend.position = 'top',
        legend.title = element_blank())

ggsave(
  paste('Charts/bachelor.png',sep='/'),
  width = 3000,
  height = 1500,
  units = 'px',
  dpi = 300
)

# Employment Rate: Deaf and Hearing Latinx
resultEmployed<-thetime%>%
  filter(status == 'employed' & attribution %in% c("deaf Latinx","hearing Latinx"))

write.csv(resultEmployed%>%
            select(attribution,percentage,year)%>%
            pivot_wider(names_from = year, values_from = percentage),file = 'output.csv')

resultEmployed$attribution <- factor(resultEmployed$attribution, levels = c("hearing Latinx",
                                                                "deaf Latinx"))

ggplot(data = resultEmployed, aes(x = year, y = percentage/100, col = attribution))+
  geom_line()+geom_point()+
  geom_ribbon(aes(ymin=percentage/100-margin_errors/100,
                  ymax=percentage/100+margin_errors/100,fill=attribution),
              alpha=0.3,size = 0.2)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(breaks = round(seq(min(resultEmployed$year), max(resultEmployed$year), by = 1),1))+
  scale_fill_manual(values = c('#414042','#00A79D'))+
  scale_color_manual(values = c('#414042','#00A79D'))+
  xlab('')+ylab('')+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white"),
        text = element_text(size = 12, family ='Roboto'),
        axis.text = element_text(size = 12, family ='Roboto'),
        plot.caption = element_text(face = "italic"),
        panel.spacing.x = unit(6, "mm"),
        legend.position = 'top',
        legend.title = element_blank())

ggsave(
  paste('Charts/employed.png',sep='/'),
  width = 3000,
  height = 1500,
  units = 'px',
  dpi = 300
)

# Earning: Deaf and Hearing Latinx
resultEarning<-thetime%>%
  filter(status == 'earning' & attribution %in% c("deaf Latinx","hearing Latinx"))

monetize<-scales::dollar_format()

write.csv(resultEarning%>%
            select(attribution,median_income,year)%>%
            mutate(median_income = monetize(median_income))%>%
            pivot_wider(names_from = year, values_from = median_income),file = 'output.csv')

resultEarning$attribution <- factor(resultEarning$attribution, levels = c("deaf Latinx",
                                                                            "hearing Latinx"))

ggplot(data = resultEarning, aes(x = year, y = median_income, col = attribution))+
  geom_line()+geom_point()+
  geom_ribbon(aes(ymin=median_income-margin_errors,
                  ymax=median_income+margin_errors,fill=attribution),
              alpha=0.3,size = 0.2)+
  scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 0),
                     breaks = round(seq(min(resultEarning$median_income), max(resultEarning$median_income), by = 2000)))+
  scale_x_continuous(breaks = round(seq(min(resultEarning$year), max(resultEarning$year), by = 1),1))+
  scale_fill_manual(values = c('#00A79D','#414042'))+
  scale_color_manual(values = c('#00A79D','#414042'))+
  xlab('')+ylab('')+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white"),
        text = element_text(size = 12, family ='Roboto'),
        axis.text = element_text(size = 12, family ='Roboto'),
        plot.caption = element_text(face = "italic"),
        panel.spacing.x = unit(6, "mm"),
        legend.position = 'top',
        legend.title = element_blank())

ggsave(
  paste('Charts/median_income.png',sep='/'),
  width = 3000,
  height = 1500,
  units = 'px',
  dpi = 300
)
  
