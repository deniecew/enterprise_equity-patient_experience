#load packages
library(tidyverse)
library(janitor)

#load data
deidata<-read.csv("G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/DEI_data.csv")

#rename columns to fit r nomenclature and store as deidata.r
deidata<-clean_names(deidata)
glimpse(deidata)
save(deidata,file="G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/deidata.Rdata")

#Descriptive Statistics on Raw Data 'deidata'----

#store unique mrns and run summary statistics
uniquedei<-distinct(deidata,mrn,.keep_all = TRUE)
dim(deidata)
dim(uniquedei)
view(uniquedei)
save(uniquedei,file="G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/uniquedei.Rdata")
table(uniquedei$race)
table(uniquedei$ethnicity)
table(uniquedei$ethnicity,uniquedei$race)
table(uniquedei$payor)

#create new grouping variables payor2,race_ethnicity,topbox,and age_group

#Cleaning Data----
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
    )

# Remove rows with missing data for payor2 and gender
cleandeidata<-subset(cleandeidata,payor2 != 'Unknown')
cleandeidata<-subset(cleandeidata, gender != 'Missing')

table(cleandeidata$payor2)
table(cleandeidata$race_ethnicity)
table(cleandeidata$topbox)
table(cleandeidata$age_group)
table(cleandeidata$gender)


save(cleandeidata,file="G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/cleandeidata.Rdata")
write.csv(cleandeidata,file="G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/cleandeidata.csv")

#Descriptive Statistics on Clean Data----
unique<-distinct(cleandeidata,mrn,.keep_all=TRUE)
dim(unique)
table(unique$payor2)
table(unique$race_ethnicity)
table(unique$topbox)
table(unique$age_group)
table(unique$gender)

#Data Analysis on Clean Data 'cleandeidata'----

#Model the data-
library(tidyverse)
library(here)
library(lme4)

#model1, simple logistic regression model-
model1<-glm(topbox ~ age_group + gender + race_ethnicity + payor2 , 
            family = 'binomial', cleandeidata)
summary(model1)

#set reference categories

cleandeidata<-cleandeidata %>%
  
  mutate(age_group = factor(age_group,levels=c('65-74','15-24','25-34','35-44',
                                               '45-54','55-64','75-84','85+')))%>%
  mutate(gender = factor(gender,levels=c('Female','Male')))%>%
  mutate(race_ethnicity = factor(race_ethnicity,levels=c('White','Asian', 
                                                         'Black', 
                                                         'Hispanic',
                                                         'More than one race/Other','Missing')))%>%
  mutate(payor2 = factor(payor2,levels=c('Medicare','HMOPPO', 'OtherIns', 
                                         'Medicaid','Charity', 'SelfPay'
  )))

#model2, group by mrn (i.e mrn is a random effect)- 
model2<-glmer(topbox ~ age_group + gender + race_ethnicity + payor2 +(1|mrn), cleandeidata, 
              family = 'binomial',control=glmerControl
              (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
              nAGQ = 10, )
summary(model2)
save(model2,file="G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/model2.Rdata")


#subset data by question (use VARNAME)
CP23<-subset(cleandeidata, question == "Care providers' efforts to include you in decisions about your treatment")
DCP5<-subset(cleandeidata, question == "Care providers' discussion of your treatment options")
I3<-subset(cleandeidata, question == "Staff's sensitivity to the difficulties that your condition and treatment can cause")
I4<-subset(cleandeidata, question == "Efforts to include you in decisions about your treatment")
MED3<-subset(cleandeidata, question == "How well the care providers kept you informed about your condition")
MED5<-subset(cleandeidata, question == "Care providers' concern for your questions and worries")
MED6<-subset(cleandeidata, question == "Your trust in the skill of the care providers")
MED23<-subset(cleandeidata, question == "Caring manner of the nurses")
MED24<-subset(cleandeidata, question == "Nurses answer to your questions")
O1<-subset(cleandeidata, question == "How well the staff worked together to care for you")
O3<-subset(cleandeidata, question == "Likelihood of your recommending this facility to others")

save(CP23,file="G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/data/CP23.Rdata")
save(DCP5,file="G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/data/DCP5.Rdata")
save(I3,file="G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/data/I3.Rdata")
save(I4,file="G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/data/I4.Rdata")
save(MED3,file="G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/data/MED3.Rdata")
save(MED5,file="G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/data/MED5.Rdata")
save(MED6,file="G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/data/MED6.Rdata")
save(MED23,file="G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/data/MED23.Rdata")
save(MED24,file="G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/data/MED24.Rdata")
save(O1,file="G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/data/O1.Rdata")
save(O3,file="G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/data/O3.Rdata")



#modelCP23, group by mrn (i.e mrn is a random effect)
modelCP23<-glmer(topbox ~ age_group + gender + race_ethnicity + payor2 +(1|mrn), CP23, 
              family = 'binomial',control=glmerControl
              (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
              nAGQ = 10, )
summary(modelCP23)
save(modelCP23,file="modelCP23.Rdata")

#modelDCP5, group by mrn (i.e mrn is a random effect)- 
modelDCP5<-glmer(topbox ~ age_group + gender + race_ethnicity + payor2 +(1|mrn), DCP5, 
                 family = 'binomial',control=glmerControl
                 (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                 nAGQ = 10, )
summary(modelDCP5)
save(modelDCP5,file="modelDCP5.Rdata")

#modelI3, group by mrn (i.e mrn is a random effect)- 
modelI3<-glmer(topbox ~ age_group + gender + race_ethnicity + payor2 +(1|mrn), I3, 
               family = 'binomial',control=glmerControl
               (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
               nAGQ = 10, )
summary(modelI3)
save(modelI3,file="modelI3.Rdata")

#modelI4, group by mrn (i.e mrn is a random effect)- 
modelI4<-glmer(topbox ~ age_group + gender + race_ethnicity + payor2 +(1|mrn), I4, 
               family = 'binomial',control=glmerControl
               (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
               nAGQ = 10, )
summary(modelI4)
save(modelI4,file="modelI4.Rdata")

#modelMED3, group by mrn (i.e mrn is a random effect)- 
modelMED3<-glmer(topbox ~ age_group + gender + race_ethnicity + payor2 +(1|mrn), MED3, 
                 family = 'binomial',control=glmerControl
                 (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                 nAGQ = 10, )
summary(modelMED3)
save(modelMED3,file="modelMED3.Rdata")

#modelMED5, group by mrn (i.e mrn is a random effect)- 
modelMED5<-glmer(topbox ~ age_group + gender + race_ethnicity + payor2 +(1|mrn), MED5, 
                 family = 'binomial',control=glmerControl
                 (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                 nAGQ = 10, )
summary(modelMED5)
save(modelMED5,file="modelMED5.Rdata")

#modelMED6, group by mrn (i.e mrn is a random effect)- 
modelMED6<-glmer(topbox ~ age_group + gender + race_ethnicity + payor2 +(1|mrn), MED6, 
                 family = 'binomial',control=glmerControl
                 (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                 nAGQ = 10, )
summary(modelMED6)
save(modelMED6,file="modelMED6.Rdata")

#modelMED23, group by mrn (i.e mrn is a random effect)- 
modelMED23<-glmer(topbox ~ age_group + gender + race_ethnicity + payor2 +(1|mrn), MED23, 
                  family = 'binomial',control=glmerControl
                  (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                  nAGQ = 10, )
summary(modelMED23)
save(modelMED23,file="modelMED23.Rdata")

#modelMED24, group by mrn (i.e mrn is a random effect)- 
modelMED24<-glmer(topbox ~ age_group + gender + race_ethnicity + payor2 +(1|mrn), MED24, 
                  family = 'binomial',control=glmerControl
                  (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                  nAGQ = 10, )
summary(modelMED24)
save(modelMED24,file="modelMED24.Rdata")

#modelO1, group by mrn (i.e mrn is a random effect)- 
modelO1<-glmer(topbox ~ age_group + gender + race_ethnicity + payor2 +(1|mrn), O1, 
               family = 'binomial',control=glmerControl
               (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
               nAGQ = 10, )
summary(modelO1)
save(modelO1,file="modelO1.Rdata")

#modelO3, group by mrn (i.e mrn is a random effect)- 
modelO3<-glmer(topbox ~ age_group + gender + race_ethnicity + payor2 +(1|mrn), O3, 
               family = 'binomial',control=glmerControl
               (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
               nAGQ = 10, )
summary(modelO3)
save(modelO3,file="modelO3.Rdata")

#investigate errors with modelO3
table(O3$payor2)
table(O3$age_group)
uniqueO3<-distinct(O3,mrn,.keep_all=TRUE)
dim(uniqueO3)
table(uniqueO3$payor2)
table(uniqueO3$race_ethnicity)
table(uniqueO3$topbox)
table(uniqueO3$age_group)
table(uniqueO3$gender)

# O3a<-select(O3,topbox,payor2,age_group,race_ethnicity,gender)
# O3a
# print(O3a,correlation=TRUE)

#try to calculate odds ratio
# install.packages('epitools')
# library(epitools)
# oddsratio(O3)

seCP23 <- sqrt(diag(vcov(modelCP23)))
seDCP5 <- sqrt(diag(vcov(modelDCP5)))
seI3 <- sqrt(diag(vcov(modelI3)))
seI4 <- sqrt(diag(vcov(modelI4)))
seMED3 <- sqrt(diag(vcov(modelMED3)))
seMED5 <- sqrt(diag(vcov(modelMED5)))
seMED6 <- sqrt(diag(vcov(modelMED6)))
seMED23 <- sqrt(diag(vcov(modelMED23)))
seMED24 <- sqrt(diag(vcov(modelMED24)))
seO1 <- sqrt(diag(vcov(modelO1)))
seO3 <- sqrt(diag(vcov(modelO3)))


#estimates 95% confidence intervals
(tabCP23 <- cbind(Est = fixef(modelCP23), LL = fixef(modelCP23) - 1.96 * 
                    seCP23, UL = fixef(modelCP23) + 1.96 *seCP23))
(tabDCP5 <- cbind(Est = fixef(modelDCP5), LL = fixef(modelDCP5) - 1.96 * 
                    seDCP5, UL = fixef(modelDCP5) + 1.96 *seDCP5))
(tabI3 <- cbind(Est = fixef(modelI3), LL = fixef(modelI3) - 1.96 * 
                  seI3, UL = fixef(modelI3) + 1.96 *seI3))
(tabI4 <- cbind(Est = fixef(modelI4), LL = fixef(modelI4) - 1.96 * 
                  seI4, UL = fixef(modelI4) + 1.96 *seI4))
(tabMED3 <- cbind(Est = fixef(modelMED3), LL = fixef(modelMED3) - 1.96 * 
                    seMED3, UL = fixef(modelMED3) + 1.96 *seMED3))
(tabMED5 <- cbind(Est = fixef(modelMED5), LL = fixef(modelMED5) - 1.96 * 
                    seMED5, UL = fixef(modelMED5) + 1.96 *seMED5))
(tabMED6 <- cbind(Est = fixef(modelMED6), LL = fixef(modelMED6) - 1.96 * 
                    seMED6, UL = fixef(modelMED6) + 1.96 *seMED6))
(tabMED23 <- cbind(Est = fixef(modelMED23), LL = fixef(modelMED23) - 1.96 * 
                     seMED23, UL = fixef(modelMED23) + 1.96 *seMED23))
(tabMED24 <- cbind(Est = fixef(modelMED24), LL = fixef(modelMED24) - 1.96 * 
                     seMED24, UL = fixef(modelMED24) + 1.96 *seMED24))
(tabO1 <- cbind(Est = fixef(modelO1), LL = fixef(modelO1) - 1.96 * 
                  seO1, UL = fixef(modelO1) + 1.96 *seO1))
(tabO3 <- cbind(Est = fixef(modelO3), LL = fixef(modelO3) - 1.96 * 
                  seO3, UL = fixef(modelO3) + 1.96 *seO3))
#odds ratio 95% confidence intervals
exp(tabCP23)
exp(tabDCP5)
exp(tabI3)
exp(tabI4)
exp(tabMED3)
exp(tabMED5)
exp(tabMED6)
exp(tabMED23)
exp(tabMED24)
exp(tabO1)
exp(tabO3)
