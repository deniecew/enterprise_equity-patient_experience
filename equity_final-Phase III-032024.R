#Phase 3 - Clinic breakdown

#Load Packages----
library(tidyverse)
library(janitor)
library(tidyverse)
library(lme4)
library(purrr)
library(broom)


#Load Data----
load("//Hlm.ad.moffitt.usf.edu/data/dept/Dept_Patient_Experience/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Phase II (b) - Nov2023/data/deidata.Rdata")
#load("//Hlm.ad.moffitt.usf.edu/data/dept/Dept_Patient_Experience/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Phase II (b) - Nov2023/data/cleandeidata.Rdata")

#check demographics
unique<-distinct(deidata,mrn,.keep_all=TRUE)
dim(unique)
# table(unique$pg_unit) #since same MRN can be used for different units these can not be used as an accurate count of patients per unit.
# table(unique$race, unique$pg_unit)
# table(unique$ethnicity, unique$pg_unit)
# table(unique$payor, unique$pg_unit)
# table(unique$language, unique$pg_unit)
# table(unique$sex, unique$pg_unit)


#Clean Data----
cleandeidata<-deidata %>%
  rename(question = question_text_latest)%>%
  rename(unit = pg_unit)%>%
  #rename(gender = sex)%>%
  mutate(gender=case_when(sex =='Female'~ 'Female',
                          sex =='Male'~'Male',
                          sex == '0'~ 'Missing'))%>%
  mutate(age_group=case_when(age<25 ~ '15-24',
                             age<35 ~ '25-34',
                             age<45 ~ '35-44',
                             age<55 ~ '45-54',
                             age<65 ~ '55-64',
                             age<75 ~ '65-74',
                             age<85 ~ '75-84',
                             age>84 ~ '85+',))%>%
  mutate(topbox=case_when(value==5 ~1, value<5 ~0))%>%
  mutate(race_ethnicity = case_when(
    ethnicity =='Hispanic/Latino' ~"Hispanic",
    ethnicity ==''~ "Missing", #this line was missing from Phase 2b code
    race == 'Am Ind/Alas Nat' ~ "More than one race/Other",
    race == 'Nat Haw/Other PI' ~ "More than one race/Other",
    race == 'Two or More' ~ "More than one race/Other",
    race == 'Asian' ~ "Asian",
    race == 'Black/Afr Amer'~"Black",
    race == 'Not Specified' ~ "Missing",
    race == 'Prefer Not Answ' ~ "Missing",
    race == 'White'~"White",
    race == ''~ "Missing"))%>%
  mutate(payor2 = case_when(
    payor %in% c('Charity','Exec Override Charity','Florida Breast and Cervical Cancer Early','Scrn and Prev Svcs','Unfunded Lung Screen','Unfunded Prostate CC'
    )~"Charity",
    payor %in% c('ACH Stem Cell Processing','Aetna','Aetna HMO','Aetna Qualified Health Plans QHP','All Savers','Allegiance Benefit Plan Cigna Network','American Postal Workers Union','American Postal Workers Union Cigna Netw','AvMed','BCBS BlueSelect','BCBS BlueSelect INN','BCBS Florida BlueCare','BCBS Florida BlueChoice','BCBS Florida BlueOptions','BCBS Florida Traditional','BCBS MyBlue','BCBS Netblue OutOfState','BCBS PPO OOS Publix','BCBS PPO OutOfState','BCBS Traditional Out Of State','Blue Cross Federal','Capital Health Plan','Cigna','Cigna Healthcare Baycare Employee','Cigna Pathwell','Florida Health Care Plan','GEHA United Network','Generic Aetna Network','Generic Cigna NAC PrePost BMT Network','Generic Cigna Network','Generic Evolutions Prime Network','Generic Evolutions Select Network','Generic First Health Network','Generic Multiplan PHCS Network','Generic United Network','Generic United Options Network','Golden Rule United Network','Harvard Pilgrim United Options PPO Netwo','Health First Health Plans','Healthsmart Aetna Network','Hillsborough County Health Care Plan','Hospice Gulfside Regional','Humana','Mail Handlers Aetna Network','Medica Insurance United Network','Medical Mutual Aetna Network','Meritain Health Aetna Network','MVP Healthcare Cigna Network','NALC Cigna Network','Oxford Health Plan United Network','Samba FEBA Cigna Network','Tricare Prime','UMR United Network','UMR United Options PPO Network','Unicare Multiplan PHCS Network','United','United Healthcare Optum CRS','United HMO ACA OON','United Options PPO','UPMC Health Plan Multiplan PHCS Network','Web TPA Aetna Network'
    )~"HMOPPO",
    payor %in% c('Aetna Medicaid','Amerihealth Caritas Florida Medicaid','Humana Medicaid','Humana Medicaid INN','Medicaid','Medically Needy','Molina Healthcare Medicaid','Non Contracted Medicaid','Simply Healthcare Medicaid','Sunshine State Health Plan Medicaid','United Healthcare Medicaid'
    )~"Medicaid",
    payor %in% c('Aetna Medicare Advantage','Aetna Medicare Advantage HMO','BayCare Medicare Advantage OON','BayCare Medicare Advantage OON LOA','BayCare Medicare Advantage OON LOA Prof','BCBS BlueCard Medicare Advantage','BCBS Florida Medicare Advantage','BCBS Medicare Supplement','Capital Health Plan Medicare Advantage','CarePlus Health Plan Medicare Advantage','Cigna HealthSpring Medicare Advantage','Devoted Medicare Advantage','Donor Medicare BMT','Freedom Health Medicare Advantage','Health First Medicare Advantage','Humana Medicare Advantage HMO','Humana Medicare Advantage PPO','Medicare A','Medicare Railroad Retirement Part A','Molina MCR ADV','Non Contracted Medicare Advantage','Optimum Healthcare Medicare Advantage','Priority Health Medicare Advantage','Simply Healthcare Medicare','Ultimate Medicare Advantage','United Healthcare Medicare Advantage','United Healthcare Optum Medicare BMT','United MCR HMO OON','Wellcare Medicare Advantage','WellMed Network of Florida Inc'
    )~"Medicare",
    payor %in% c('Aetna BMT Global','Aetna Lakeland Regional Medical Center','Blue Cross Indemnity','CHAMPVA','Cigna Global BMT','Cigna Global CAR T','Cigna Healthcare Pre and Post BMT','Cigna Indemnity','Cigna PrePost CAR T','Energy Employees OIEP','Evolution HealthCare System Employer','Generic Cigna PrePost BMT Network','Generic Commercial','Golden Rule Optum PrePost BMT','Humana Global BMT','Humana Medicare Advantage BMT','Key Benefits Administrators Beechstreet','National Marrow Donor Program','Tricare For Life Medicare Supplement','Tricare Select','UMR Optum CRS','UMR Optum Global BMT','UMR Optum PrePost BMT','United Healthcare Indemnity','United Healthcare Optum Global BMT','United Healthcare Optum PrePost BMT','Veterans Administration Bay Pines','Veterans Administration Gainesville','Veterans Administration Global','Veterans Administration Orlando','Veterans Administration Puerto Rico','Veterans Administration Tampa','Veterans Administration West Palm Beach','World Trade Center NPN'
    )~"OtherIns",
    payor %in% c('Self Pay','Self Pay International Patient','Self Pay Tier'
    )~"SelfPay",
    payor =='Pending Funding Medicaid'~"Unknown"
  )
  )%>%
  subset(payor2 != 'Unknown')%>%
  subset(gender != 'Missing')

#check that coded values are correct
table(cleandeidata$payor2)
table(cleandeidata$gender)
table(cleandeidata$age_group)
table(cleandeidata$race_ethnicity)

# uniquely<-distinct(cleandeidata,mrn,.keep_all=TRUE)
# dim(uniquely)
# table(uniquely$payor2, uniquely$unit)
# table(uniquely$race_ethnicity, uniquely$unit)
# table(uniquely$age_group, uniquely$unit)
# table(uniquely$gender, uniquely$unit)

#collapse data levels
unitdata<-cleandeidata%>%
  mutate(ethnic2=case_when(
    race_ethnicity %in% c('Asian','Black','More than one race/Other ')~ "ABO", 
    race_ethnicity==  'White'~ "White", 
    race_ethnicity==  'Hispanic'~"Hispanic", 
    race_ethnicity==  'Missing'~"Missing"))%>%
  mutate(age2=case_when(age<65 ~ 'age<65',
                        age>64 ~ '65+'))%>%
  subset(unit!='CSMK'& unit !='MKC 6FL')%>%
  subset(ethnic2 !='Missing')

unitdata<-unitdata %>%
  mutate(age2 = factor(age2,levels=c('65+', 'age<65')))%>%
  mutate(gender = factor(gender,levels=c('Female','Male')))%>%
  mutate(ethnic2 = factor(ethnic2,levels=c('White','ABO', 'Hispanic')))

head(unitdata) 

unitdata<-unitdata %>%
  mutate(age2 = factor(age2,levels=c('65+', 'age<65')))%>%
  mutate(gender = factor(gender,levels=c('Female','Male')))%>%
  mutate(ethnic2 = factor(ethnic2,levels=c('White','ABO', 'Hispanic')))


#Descriptive Stats----
#patients by unit

#top box by race_ethnicity,unit
descript<-function(x){
  tmp1<-subset(unitdata,unit==x)
  return(addmargins(table(tmp1$ethnic2,tmp1$topbox)))
}

allunits<-c(unique(unitdata$unit))
tabls<-map(allunits,descript)
names(tabls)<-allunits
tabls
write.csv(tabls,'tabls.csv')
#unique MRNs,ethnic counts by unit
demographs<-function(y){
  tmp2<-subset(unitdata,unit==y)
  uniqunit<-distinct(tmp2,mrn,.keep_all=TRUE)
  return(addmargins(table(uniqunit$ethnic2)))
}

tabls2<-map(allunits,demographs)
names(tabls2)<-allunits
tabls2
write.csv(tabls2,'tabls2.csv')


#Modelling the Data----
  #Full Model
glmerfit <- function(unitname){
  tmp_model<-glmer(topbox ~ age2+ ethnic2 +gender +(1|mrn), unitdata,
                   family = 'binomial',control=glmerControl
                   (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)),
                   nAGQ = 10,subset=unit==unitname )
  
  tmp_se<- sqrt(diag(vcov(tmp_model)))
  tmp_tab <- cbind(Est = fixef(tmp_model), LL = fixef(tmp_model) - 1.96 * 
                     tmp_se, UL = fixef(tmp_model) + 1.96 *tmp_se)
  return(  exp(tmp_tab) #OR estimates and 95% confidence intervals
           
  )
}

  #Reduced Model
glmerfit2 <- function(unitname){
  tmp_model2<-glmer(topbox ~ age2+ ethnic2 +(1|mrn), unitdata,
                   family = 'binomial',control=glmerControl
                   (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)),
                   nAGQ = 10,subset=unit==unitname )
  
  tmp_se2<- sqrt(diag(vcov(tmp_model2)))
  tmp_tab2<- cbind(Est = fixef(tmp_model2), LL = fixef(tmp_model2) - 1.96 * 
                     tmp_se2, UL = fixef(tmp_model2) + 1.96 *tmp_se2)
  return(  exp(tmp_tab2) #OR estimates and 95% confidence intervals
           
  )
}
# outputAIM<-glmerfit(unitname="AIM")
# outputAIM

mostunits<- subset(allunits, allunits != 'WDC')
output<-map(mostunits,glmerfit)
names(output)<-mostunits
#str(output,1,0)
output
write.csv(output,"output.csv")
# (output[[1]])
# output$AIM


output2<-map(allunits,glmerfit2)
names(output2)<-allunits
output2$WDC




