##Data Cleaning ##----

#load packages

library(tidyverse)
library(here)
library(janitor)

ltr<-read_csv(here("data","final_ltr01.csv"))
swt<-read_csv(here("data","final_swt01.csv"))
glimpse(ltr)
glimpse(swt)

#rename columns to fit r nomenclature ans store as 'clean~.csv'

cleanltr<-clean_names(ltr)
write_csv(cleanltr,here("data","cleanltr.csv"))
names(ltr)#verify names look goodt

cleanswt<-clean_names(swt)
write_csv(cleanswt,here("data","cleanswt.csv"))

#rename variables,select predictors and set reference levels and 
#store as 'cleaner~.csv' using pipe %>%

cleanerltr<-cleanltr %>%
  rename(topboxltr = top_box_ltr)%>%
  rename(age_gp = age_2)%>%
  rename(ethnicity = ethnicity_2)%>%
  rename(race = race_2)%>%
  rename(insurance = payer_2)%>%
  rename(language = preflang_2)%>%
  rename(sexor = sexor_2)%>%
  rename(genid = genid_2)%>%
  select(mrn, topboxltr, service,age_gp, age, sex, ethnicity, race, insurance, 
         language, sexor, genid )
 
view(cleanerltr)
write_csv(cleanerltr,here("data","cleanerltr.csv"))

cleanerswt<-cleanswt %>%
  rename(topboxswt = top_box_swt)%>%
  rename(age_gp = age_2)%>%
  rename(ethnicity = ethnicity_2)%>%
  rename(race = race_2)%>%
  rename(insurance = payer_2)%>%
  rename(language = preflang_2)%>%
  rename(sexor = sexor_2)%>%
  rename(genid = genid_2)%>%
  select(mrn, topboxswt, service,age_gp, age, sex, ethnicity, race, insurance, 
         language, sexor, genid )

view(cleanerswt)
write_csv(cleanerswt,here("data","cleanerswt.csv"))

#remove subset of data that is 'Unknown' store files as 'cleanest~.csv'
cleanestltr<-
  subset(cleanerltr, ethnicity !='Unknown' & race!='Unknown'
         & insurance !='Unknown'& language !='Unknown'& sexor!= 'Unknown' )
write_csv(cleanestltr,here("data", "cleanestltr.csv"))

cleanestswt<-
  subset(cleanerswt, ethnicity !='Unknown' & race!='Unknown'
         & insurance !='Unknown'& language !='Unknown'& sexor!= 'Unknown' )
write_csv(cleanestswt,here("data", "cleanestswt.csv"))

#Descriptive Statistics----
#store unique mrns as 'unique_~.csv'and run summary statistics
library(tidyverse)

unique_ltr<-distinct(cleanestltr, mrn, .keep_all = TRUE) #remove duplicate MRNs
dim(cleanestltr)
dim(unique_ltr)
view(unique_ltr)
write_csv(unique_ltr,here("data", "unique_ltr.csv")) 

unique_swt<-distinct(cleanestswt, mrn, .keep_all = TRUE) #remove duplicate MRNs
dim(cleanestswt)
dim(unique_swt)
view(unique_swt)
write_csv(unique_swt,here("data", "unique_swt.csv")) 


library(plyr)

l_age<-count(unique_ltr,'age_gp')  
l_sex<-count(unique_ltr,'sex')
l_ethnic<-count(unique_ltr,'ethnicity')
l_race<-count(unique_ltr,'race')
l_payer<-count(unique_ltr,'insurance')
l_preflang<-count(unique_ltr,'language')
l_sexor<-count(unique_ltr,'sexor')
l_genid<-count(unique_ltr,'genid')

s_age<-count(unique_swt,'age_gp')  
s_sex<-count(unique_swt,'sex')
s_ethnic<-count(unique_swt,'ethnicity')
s_race<-count(unique_swt,'race')
s_payer<-count(unique_swt,'insurance')
s_preflang<-count(unique_swt,'language')
s_sexor<-count(unique_swt,'sexor')
s_genid<-count(unique_swt,'genid')

library (reshape2)

melt(l_age,na.rm=FALSE, value.name='freq')
melt(l_sex,na.rm=FALSE, value.name='freq')
melt(l_ethnic,na.rm=FALSE, value.name='freq')
melt(l_race,na.rm=FALSE, value.name='freq')
melt(l_payer,na.rm=FALSE, value.name='freq')
melt(l_preflang,na.rm=FALSE, value.name='freq')
melt(l_sexor,na.rm=FALSE, value.name='freq')
melt(l_genid,na.rm=FALSE, value.name='freq')

melt(s_age,na.rm=FALSE, value.name='freq')
melt(s_sex,na.rm=FALSE, value.name='freq')
melt(s_ethnic,na.rm=FALSE, value.name='freq')
melt(s_race,na.rm=FALSE, value.name='freq')
melt(s_payer,na.rm=FALSE, value.name='freq')
melt(s_preflang,na.rm=FALSE, value.name='freq')
melt(s_sexor,na.rm=FALSE, value.name='freq')
melt(s_genid,na.rm=FALSE, value.name='freq')

##Model the Data ----
##using the 'cleanest~.csv'data
##install packages----
library(tidyverse)
library(here)
library(lme4)

#run the models on *complete* data----
#set reference categories----
cleanerltr<-cleanerltr %>%
  mutate(age_3=case_when(age<65 ~"<65", age>=65 ~">=65"))%>%
  mutate(age_3 = factor(age_3,levels=c('>=65','<65')))%>%
  mutate(service = factor(service,levels=c('ON','MD','OU','IN','UC',
                                           'AS')))%>%
  mutate(age_gp = factor(age_gp,levels=c('65-74','15-24','25-34','35-44',
                                         '45-54','55-64','75-84','>=85')))%>%
  mutate(sex = factor(sex,levels=c('Female','Male')))%>%
  mutate(ethnicity = factor(ethnicity,levels=
                              c('Non-Spanish ;Non Hispanic',
                                'Spanish;Hispanic','Unknown')))%>%
  mutate(race = factor(race,levels=c('White','Black/African American', 
                                     'More Than One Race/Other', 
                                     'Asian/Pacific Islander',
                                     'American Indian or Alaska Native',
                                     'Unknown')))%>%
  mutate(insurance = factor(insurance,levels=c('Medicare','HMOPPO', 'OtherIns', 
                                               'Medicaid','Charity', 'SelfPay',
                                               'Unknown')))%>%
  mutate(language = factor(language,levels=c('English','Spanish', 'Other', 
                                             'Unknown')))%>%
  mutate(sexor = factor(sexor,levels=c('Heterosexual','Unknown','Homosexual', 
                                       'Something else', 'Bisexual')))%>%
  mutate(genid = factor(genid,levels=c('Female','Male','Unknown', 'Transgender',
                                       'Genderqueer Nonbinary')))

cleanerswt<-cleanerswt %>%
  mutate(age_3=case_when(age<65 ~"<65", age>=65 ~">=65"))%>%
  mutate(age_3 = factor(age_3,levels=c('>=65','<65')))%>%
  mutate(service = factor(service,levels=c('ON','MD','OU','IN','UC',
                                           'AS')))%>%
  mutate(age_gp = factor(age_gp,levels=c('65-74','15-24','25-34','35-44',
                                         '45-54','55-64','75-84','>=85')))%>%
  mutate(sex = factor(sex,levels=c('Female','Male')))%>%
  mutate(ethnicity = factor(ethnicity,levels=
                              c('Non-Spanish ;Non Hispanic',
                                'Spanish;Hispanic','Unknown')))%>%
  mutate(race = factor(race,levels=c('White','Black/African American', 
                                     'More Than One Race/Other', 
                                     'Asian/Pacific Islander',
                                     'American Indian or Alaska Native',
                                     'Unknown')))%>%
  mutate(insurance = factor(insurance,levels=c('Medicare','HMOPPO', 'OtherIns', 
                                               'Medicaid','Charity', 'SelfPay',
                                               'Unknown')))%>%
  mutate(language = factor(language,levels=c('English','Spanish', 'Other', 
                                             'Unknown')))%>%
  mutate(sexor = factor(sexor,levels=c('Heterosexual','Unknown','Homosexual', 
                                       'Something else', 'Bisexual')))%>%
  mutate(genid = factor(genid,levels=c('Female','Male','Unknown', 'Transgender',
                                       'Genderqueer Nonbinary')))

#simple logistic regression model on 'cleaner~.csv' data
#model 1----
model_ltr1<-glm(topboxltr ~ age + ethnicity + race + insurance + 
                 language + sexor, family = 'binomial', cleanerltr)
summary(model_ltr1)

model_swt1<-glm(topboxswt ~ age + ethnicity + race + insurance + 
                  language + sexor, family = 'binomial', cleanerswt)
summary(model_swt1)

#model 2----
#use mrn as a random effect on 'cleaner~.csv' data and store as 'model_~2.rdata'
#this takes a lot of time and bandwidth
model_ltr2<-glmer(topboxltr ~ age_gp + sex + ethnicity + race + insurance + 
                    language + sexor + genid +(1|mrn), cleanerltr, 
                  family = 'binomial',control=glmerControl
                  (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                  nAGQ = 10, )
summary(model_ltr2)
save(model_ltr2, file=here("model","model_ltr2.Rdata"))

model_swt2<-glmer(topboxswt ~ age_gp + sex + ethnicity + race + insurance + 
                 language + sexor + genid +(1|mrn), cleanerswt, 
               family = 'binomial',control=glmerControl
               (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
               nAGQ = 10, )
summary(model_swt2)
save(model_swt2, file=here("model","model_swt2.Rdata"))

3#model 3----
#remove genid variable on 'cleaner~.csv' data and store as 'model_~3.rdata'
model_ltr3<-glmer(topboxltr ~ age_gp + sex + ethnicity + race + insurance + 
                    language + sexor +(1|mrn), cleanerltr, 
                  family = 'binomial',control=glmerControl
                  (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                  nAGQ = 10, )
summary(model_ltr3)
save(model_ltr3, file=here("model","model_ltr3.Rdata"))

model_swt3<-glmer(topboxswt ~ age_gp + sex + ethnicity + race + insurance + 
                    language + sexor +(1|mrn), cleanerswt, 
                  family = 'binomial',control=glmerControl
                  (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                  nAGQ = 10, )
summary(model_swt3)
save(model_swt3, file=here("model","model_swt3.Rdata"))

#run models on *subset* data: 'cleanest~.csv'----

#set reference categories----
cleanestltr<-cleanestltr %>%
  mutate(age_3=case_when(age<65 ~"<65", age>=65 ~">=65"))%>%
  mutate(age_3 = factor(age_3,levels=c('>=65','<65')))%>%
  mutate(service = factor(service,levels=c('ON','MD','OU','IN','UC',
                                           'AS')))%>%
  mutate(age_gp = factor(age_gp,levels=c('65-74','15-24','25-34','35-44',
                                         '45-54','55-64','75-84','>=85')))%>%
  mutate(sex = factor(sex,levels=c('Female','Male')))%>%
  mutate(ethnicity = factor(ethnicity,levels=
                              c('Non-Spanish ;Non Hispanic',
                                'Spanish;Hispanic','Unknown')))%>%
  mutate(race = factor(race,levels=c('White','Black/African American', 
                                     'More Than One Race/Other', 
                                     'Asian/Pacific Islander',
                                     'American Indian or Alaska Native',
                                     'Unknown')))%>%
  mutate(insurance = factor(insurance,levels=c('Medicare','HMOPPO', 'OtherIns', 
                                               'Medicaid','Charity', 'SelfPay',
                                               'Unknown')))%>%
  mutate(language = factor(language,levels=c('English','Spanish', 'Other', 
                                             'Unknown')))%>%
  mutate(sexor = factor(sexor,levels=c('Heterosexual','Unknown','Homosexual', 
                                       'Something else', 'Bisexual')))%>%
  mutate(genid = factor(genid,levels=c('Female','Male','Unknown', 'Transgender',
                                       'Genderqueer Nonbinary')))

cleanestswt<-cleanestswt %>%
  mutate(age_3=case_when(age<65 ~"<65", age>=65 ~">=65"))%>%
  mutate(age_3 = factor(age_3,levels=c('>=65','<65')))%>%
  mutate(service = factor(service,levels=c('ON','MD','OU','IN','UC',
                                           'AS')))%>%
  mutate(age_gp = factor(age_gp,levels=c('65-74','15-24','25-34','35-44',
                                         '45-54','55-64','75-84','>=85')))%>%
  mutate(sex = factor(sex,levels=c('Female','Male')))%>%
  mutate(ethnicity = factor(ethnicity,levels=
                              c('Non-Spanish ;Non Hispanic',
                                'Spanish;Hispanic','Unknown')))%>%
  mutate(race = factor(race,levels=c('White','Black/African American', 
                                     'More Than One Race/Other', 
                                     'Asian/Pacific Islander',
                                     'American Indian or Alaska Native',
                                     'Unknown')))%>%
  mutate(insurance = factor(insurance,levels=c('Medicare','HMOPPO', 'OtherIns', 
                                               'Medicaid','Charity', 'SelfPay',
                                               'Unknown')))%>%
  mutate(language = factor(language,levels=c('English','Spanish', 'Other', 
                                             'Unknown')))%>%
  mutate(sexor = factor(sexor,levels=c('Heterosexual','Unknown','Homosexual', 
                                       'Something else', 'Bisexual')))%>%
  mutate(genid = factor(genid,levels=c('Female','Male','Unknown', 'Transgender',
                                       'Genderqueer Nonbinary')))
#model 4----
#simple logistic regression model on 'cleanest~.csv' data
model_ltr4<-glm(topboxltr ~ age + ethnicity + race + insurance + 
                  language + sexor, family = 'binomial', cleanestltr)
summary(model_ltr4)

model_swt4<-glm(topboxswt ~ age + service + ethnicity + race + insurance + 
                  language + sexor, family = 'binomial', cleanestswt)
summary(model_swt4)                         

#model 5----
#run model w/o missing data:'cleanest~.csv' and store as 'model_~5.rdata'
model_ltr5<-glmer(topboxltr ~ age_gp + sex + ethnicity + race + insurance + 
                    language + sexor +(1|mrn), cleanestltr, 
                  family = 'binomial',control=glmerControl
                  (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                  nAGQ = 10, )
summary(model_ltr5)
save(model_ltr5, file=here("model","model_ltr5.Rdata"))

model_swt5<-glmer(topboxswt ~ age_gp + sex + ethnicity + race + insurance + 
                    language + sexor +(1|mrn), cleanestswt, 
                  family = 'binomial',control=glmerControl
                  (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                  nAGQ = 10, )
summary(model_swt5)
save(model_swt5, file=here("model","model_swt5.Rdata"))

#model 6-----
#run model treating service line as a fixed effect
model_ltr6<-glmer(topboxltr ~ service + age_gp + sex + ethnicity + race + insurance + 
                    language + sexor +(1|mrn), cleanestltr, 
                  family = 'binomial',control=glmerControl
                  (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                  nAGQ = 10, )
summary(model_ltr6)
save(model_ltr6, file=here("model","model_ltr6.Rdata"))

model_swt6<-glmer(topboxswt ~ service + age_gp + sex + ethnicity + race + 
                    insurance + language + sexor +(1|mrn), cleanestswt, 
                  family = 'binomial',control=glmerControl
                  (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                  nAGQ = 10, )
summary(model_swt6)
save(model_swt6, file=here("model","model_swt6.Rdata"))

#model 7-----
#run model treating service line as a fixed effect and age as continuous
model_ltr7<-glmer(topboxltr ~ service + age + sex + ethnicity + race + 
                    insurance + language + sexor +(1|mrn), cleanestltr, 
                  family = 'binomial',control=glmerControl
                  (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                  nAGQ = 10, )
summary(model_ltr7)
save(model_ltr7, file=here("model","model_ltr7.Rdata"))

model_swt7<-glmer(topboxswt ~ service + age + sex + ethnicity + race + insurance + 
                    language + sexor +(1|mrn), cleanestswt, 
                  family = 'binomial',control=glmerControl
                  (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                  nAGQ = 10, )
summary(model_swt7)
save(model_swt7, file=here("model","model_swt7.Rdata"))

#model 8-----
#run model treating service line as a random effect
model_ltr8<-glmer(topboxltr ~ age_gp + sex + ethnicity + race + insurance + 
                    language + sexor +(1|mrn) + (1|service), cleanestltr, 
                  family = 'binomial',control=glmerControl
                  (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                  nAGQ = 1, )
summary(model_ltr8)
save(model_ltr8, file=here("model","model_ltr8.Rdata"))

#Confidence Intervals----

se_ltr <- sqrt(diag(vcov(model_ltr6)))
se_swt <- sqrt(diag(vcov(model_swt6)))
#estimates 95% confidence intervals
(tab_ltr <- cbind(Est = fixef(model_ltr6), LL = fixef(model_ltr6) - 1.96 * 
                    se_ltr, UL = fixef(model_ltr6) + 1.96 *se_ltr))

(tab_swt <- cbind(Est = fixef(model_swt6), LL = fixef(model_swt6) - 1.96 * 
                    se_swt, UL = fixef(model_swt6) + 1.96 *se_swt))

#odds ratio 95% confidence intervals
exp(tab_ltr)
exp(tab_swt)

##Visualize the Data----
##forest plots for results from exp(tab_~) OR CI data

library(ggplot2)
library(ggpubr)
library(here)
#forest plots for LTR data----
#OR, LL, UL values
service_ltr<-data.frame(label=c('Virtual Health',	'Outpatient Services',	
                                'Inpatient','Urgent Care'),
                    index=1:4,
                    OR=c(0.53, 1.27, 0.58, 0.16),
                    LL=c(0.46, 1.03, 0.45, 0.12),
                    UL=c(0.60, 1.57, 0.74, 0.20))

age_ltr<-data.frame(label=c('15-24',	'25-34',	'35-44','45-54',	'55-64',	
                        '75-84',	'>=85'),
                index=1:7,
                OR=c(0.32, 0.34, 0.44, 0.60, 0.72, 1.05, 0.86),
                LL=c(0.11, 0.18, 0.30, 0.45, 0.57, 0.88, 0.57),
                UL=c(0.88, 0.62, 0.63, 0.81, 0.92, 1.24, 1.30))

race_ltr<-data.frame(label=c('Black/African American','More Than One Race/Other',
                         'Asian/Pacific Islander',
                         'American Indian or Alaska Native'),
                 index=1:4,
                 OR=c(0.70,	0.45, 0.49, 0.72),
                 LL=c(0.53, 0.30, 0.31, 0.20),
                 UL=c(0.92, 0.68, 0.76, 2.66))

lang_ltr<-data.frame(label=c('Spanish', 'Other'),
                 index=1:2,
                 OR=c(1.73,	0.70),
                 LL=c(0.89,	0.23),
                 UL=c(3.37,	2.15))

sexor_ltr<-data.frame(label=c('Homosexual', 'Something else', 'Bisexual'),
                  index=1:3,
                  OR=c(0.72,	0.71,	0.92),
                  LL=c(0.48,	0.34,	0.38),
                  UL=c(1.08,	1.48,	2.27))

insurance_ltr<-data.frame(label=c('HMOPPO', 'OtherIns', 'Medicaid', 'Charity', 
                              'SelfPay'),
                      index=1:5,
                      OR=c(1.09,	0.89,	0.98,	2.41,	0.69),
                      LL=c(0.87,	0.62,	0.59,	1.26,	0.34),
                      UL=c(1.37,	1.29,	1.63,	4.62,	1.39))
#forest plots
p0<-ggplot(data=service_ltr, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(service_ltr), labels=service_ltr$label) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25))+
  labs(title='Likelihood to Recommend by Service Line', 
       subtitle= 'Comparison Group: Outpatient Oncology', x='OR', 
       y = 'Service Line') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.75, y = nrow(service_ltr)+1,
           label = "TopBox Less Likely") +
  annotate("text", x = 1.15, y = nrow(service_ltr)+1,
           label = "TopBox More Likely") +
  theme_minimal()
p1<-ggplot(data=age_ltr, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(age_ltr), labels=age_ltr$label) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25))+
  labs(title='Likelihood to Recommend by Age Group', 
       subtitle= 'Comparison Group: 65-74 yr olds', x='OR', y = 'Age Group') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.75, y = nrow(age_ltr)+1, 
           label = "TopBox Less Likely") +
  annotate("text", x = 1.15, y = nrow(age_ltr)+1, 
           label = "TopBox More Likely") +
  theme_minimal()

p2<-ggplot(data=race_ltr, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(race_ltr), labels=race_ltr$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0))+
  labs(title='Likelihood to Recommend by Race', 
       subtitle='Comparison Group: White',
       x='OR', y = 'Race') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(race_ltr)+1, 
           label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(race_ltr)+1, 
           label = "TopBox More Likely") +
  theme_minimal()

p3<-ggplot(data=lang_ltr, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(lang_ltr), labels=lang_ltr$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5))+
  labs(title='Likelihood to Recommend by Language', 
       subtitle ='Comparison Group: English', x='OR', 
       y = 'Language') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(lang_ltr)+1, 
           label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(lang_ltr)+1, 
           label = "TopBox More Likely") +
  theme_minimal()

p4<-ggplot(data=sexor_ltr, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(sexor_ltr), labels=sexor_ltr$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0))+
  labs(title='Likelihood to Recommend by Sexual Orientation',
       subtitle = 'Comparison Group: Heterosexual', 
       x='OR', y = 'Sexual Orientation') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(sexor_ltr)+1, 
           label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(sexor_ltr)+1, 
           label = "TopBox More Likely") +
  theme_minimal()

p5<-ggplot(data=insurance_ltr, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(insurance_ltr), labels=insurance_ltr$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5))+
  labs(title='Likelihood to Recommend by Payer',
       subtitle='Comparison Group: Medicare', x='OR', y = 'Insurance') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(insurance_ltr)+1,
           label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(insurance_ltr)+1, 
           label = "TopBox More Likely") +
  theme_minimal()

#arrange plots then export files
p0
ggsave(here("plots","forest-plot00.png"))#exports the last plot
p1
ggsave(here("plots","forest-plot01.png"))#exports the last plot
#ggarrange(p2,p3, nrow=2,ncol=1)
p2
ggsave(here("plots","forest-plot02.png"))#exports the last plot
#ggarrange(p4,p5, nrow=2,ncol=1)
p3
ggsave(here("plots","forest-plot03.png"))#exports the last plot
p4
ggsave(here("plots","forest-plot04.png"))#exports the last plot
p5
ggsave(here("plots","forest-plot05.png"))#exports the last plot

#forest plots for SWT data----
#Enter Odds Ratio estimate and CI
service_swt<-data.frame(label=c('Virtual Health',	'Outpatient Services',	
                                'Inpatient','Urgent Care'),
                        index=1:4,
                        OR=c(1.09, 1.93, 0.67, 0.32),
                        LL=c(0.96, 1.59, 0.54, 0.26),
                        UL=c(1.24, 2.34, 0.83, 0.41))

age_swt<-data.frame(label=c('15-24',	'25-34',	'35-44','45-54',	'55-64',	
                        '75-84',	'>=85'),
                index=1:7,
                OR=c(0.89, 0.44, 0.54, 0.75, 0.76, 0.96, 0.81),
                LL=c(0.30, 0.25, 0.38, 0.57, 0.62, 0.83, 0.56),
                UL=c(2.63, 0.78, 0.76, 0.97, 0.95, 1.11, 1.17))

race_swt<-data.frame(label=c('Black/African American','More Than One Race/Other',
                         'Asian/Pacific Islander',
                         'American Indian or Alaska Native'),
                 index=1:4,
                 OR=c(0.77, 0.45, 0.53, 0.99),
                 LL=c(0.60, 0.31, 0.35, 0.27),
                 UL=c(1.00, 0.65, 0.81, 3.59))

insurance_swt<-data.frame(label=c('HMOPPO', 'OtherIns', 'Medicaid', 'Charity', 
                              'SelfPay'),
                      index=1:5,
                      OR=c(1.20, 0.81, 0.73, 1.68, 0.70),
                      LL=c(0.98, 0.59, 0.46, 0.97, 0.38),
                      UL=c(1.47, 1.12, 1.14, 2.92, 1.32))

lang_swt<-data.frame(label=c('Spanish', 'Other'),
                 index=1:2,
                 OR=c(1.56, 0.86),
                 LL=c(0.87, 0.30),
                 UL=c(2.80, 2.49))

sexor_swt<-data.frame(label=c('Homosexual', 'Something else', 'Bisexual'),
                  index=1:3,
                  OR=c(0.75, 0.64, 1.04),
                  LL=c(0.52, 0.33, 0.45),
                  UL=c(1.09, 1.23, 2.38))
#Forest plots
p6<-ggplot(data=service_swt, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(service_swt), labels=service_swt$label) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25))+
  labs(title='Staff Worked Together by Service Line', 
       subtitle= 'Comparison Group: Outpatient Oncology', x='OR', 
       y = 'Service Line') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.75, y = nrow(service_swt)+1, label = "TopBox Less Likely") +
  annotate("text", x = 1.15, y = nrow(service_swt)+1, label = "TopBox More Likely") +
  theme_minimal()

p7<-ggplot(data=age_swt, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(age_swt), labels=age_swt$label) +
  scale_x_continuous(breaks=c(0.0,0.25,0.5,0.75,1.0,1.25))+
  labs(title='Staff Worked Together by Age Group', 
       subtitle= 'Comparison Group: 65-74 yr olds', x='OR', y = 'Age Group') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(age_swt)+1, label = "TopBox Less Likely") +
  annotate("text", x = 1.25, y = nrow(age_swt)+1, label = "TopBox More Likely") +
  theme_minimal()

p8<-ggplot(data=race_swt, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(race_swt), labels=race_swt$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5))+
  labs(title='Staff Worked Together by Race', subtitle='Comparison Group: White',
       x='OR', y = 'Race') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(race_swt)+1, label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(race_swt)+1, label = "TopBox More Likely") +
  theme_minimal()

p9<-ggplot(data=lang_swt, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(lang_swt), labels=lang_swt$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0))+
  labs(title='Staff Worked Together by Language', 
       subtitle ='Comparison Group: English', x='OR', 
       y = 'Language') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(lang_swt)+1, label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(lang_swt)+1, label = "TopBox More Likely") +
  theme_minimal()

p10<-ggplot(data=sexor_swt, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(sexor_swt), labels=sexor_swt$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0))+
  labs(title='Staff Worked Together by Sexual Orientation',
       subtitle = 'Comparison Group: Heterosexual', 
       x='OR', y = 'Sexual Orientation') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(sexor_swt)+1, label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(sexor_swt)+1, label = "TopBox More Likely") +
  theme_minimal()

p11<-ggplot(data=insurance_swt, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(insurance_swt), labels=insurance_swt$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0))+
  labs(title='Staff Worked Together by Payer',
       subtitle='Comparison Group: Medicare', x='OR', y = 'Insurance') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(insurance_swt)+1, label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(insurance_swt)+1, label = "TopBox More Likely") +
  theme_minimal()
p6
ggsave(here("plots","forest-plot06.png"))#exports the last plot
p7
ggsave(here("plots","forest-plot07.png"))#exports the last plot
#ggarrange(p8,p9, nrow=2,ncol=1)
p8
ggsave(here("plots","forest-plot08.png"))#exports the last plot
#ggarrange(p10,p11, nrow=2,ncol=1)
p9
ggsave(here("plots","forest-plot09.png"))#exports the last plot
p10
ggsave(here("plots","forest-plot10.png"))#exports the last plot
p11
ggsave(here("plots","forest-plot11.png"))#exports the last plot