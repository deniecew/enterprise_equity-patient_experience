library(dplyr)
library(janitor)

#upload data

data_long<-DEI_Data
#will remove MRN missing values
library(tidyr)
###Clean Data----
  #remove the dates and language columns
  #add columns grouping age, other mutations done in Excel

deidata<-data_long %>%
  rename(question = question_text_latest)%>%
  rename(unit = pg_unit)%>%
  mutate(age_gp=case_when(age<25 ~ '15-24',
                          age<35 ~ '25-34',
                          age<45 ~ '35-44',
                          age<55 ~ '45-54',
                          age<65 ~ '55-64',
                          age<75 ~ '65-74',
                          age<85 ~ '75-84',
                          age>84 ~ '85+',))%>%
  mutate(topbox=case_when(value==5 ~1, value<5 ~0))%>%
     select(-recdate, -race, -ethnicity, -race_ethnicity, -payor, -disdate, -language)

save(deidata,file="dei_long_data.Rdata")
view(deidata)
#write.csv(deidata,file="dei_long_data.csv")


###Descriptive statistics on unique MRNs----
library(tidyverse)
uniquedei<-distinct(deidata, mrn, .keep_all = TRUE) #remove duplicate MRNs
dim(deidata)
dim(uniquedei)
view(uniquedei)
save(uniquedei,file="dei_unique_mrn.Rdata")
write.csv(uniquedei,file="dei_unique_mrn.csv")

fivenum(uniquedei$age) #calculate five number summary of age

table<-table(uniquedei$age_gp) # freq table age_group
table

table1<-table(uniquedei$race_ethnicity2,uniquedei$sex)
table1

table2<-table(uniquedei$race_ethnicity2)
table2

table3<-table(uniquedei$sex)
table3

table4<-table(uniquedei$payor2)
table4


###Data Analysis----

###set reference categories----

deidata<-deidata %>%
 
  mutate(age_gp = factor(age_gp,levels=c('65-74','15-24','25-34','35-44',
                                         '45-54','55-64','75-84','85+')))%>%
  mutate(sex = factor(sex,levels=c('Female','Male')))%>%
  mutate(race_ethnicity2 = factor(race_ethnicity2,levels=c('White','Asian', 
                                     'Black', 
                                     'Hispanic',
                                     'More than one race/Other')))%>%
  mutate(payor2 = factor(payor2,levels=c('MEDICARE','HMOPPO', 'OTHINS', 
                                               'MEDICAID','CHARITY', 'SELFPAY',
                                               'UNKNOWN')))
 
###modelling the data----
library(tidyverse)
library(here)
library(lme4)


#simple logistic regression model
#model 1----
model1<-glm(topbox ~ age_gp + sex + race_ethnicity2 + payor2 , 
            family = 'binomial', deidata)
summary(model1)

#remove rows where payor2=="UNKNOWN" store as deidata2
deidata2<-subset(deidata, payor2 != "UNKNOWN")
#model 1a----
model1a<-glm(topbox ~ age_gp + sex + race_ethnicity2 + payor2 , 
            family = 'binomial', deidata2)
summary(model1a)

#model 2----
#use mrn as a random effect 
model2<-glmer(topbox ~ age_gp + sex + race_ethnicity2 + 
                payor2 +(1|mrn), deidata2, 
                  family = 'binomial',control=glmerControl
                  (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                  nAGQ = 10, )
summary(model2)
save(model2, file=here("model","model2.Rdata"))
save(model2,file="model2.Rdata")


#subset data by question
Q1<-subset(deidata2, question == "Care providers' concern for your questions and worries")
Q2<-subset(deidata2, question == "Care providers' discussion of your treatment options")
Q3<-subset(deidata2, question == "Care providers' efforts to include you in decisions about your treatment")
Q4<-subset(deidata2, question == "Caring manner of the nurses")
Q5<-subset(deidata2, question == "Efforts to include you in decisions about your treatment")
Q6<-subset(deidata2, question == "How well the care providers kept you informed about your condition")
Q7<-subset(deidata2, question == "How well the staff worked together to care for you")
Q8<-subset(deidata2, question == "Likelihood of your recommending this facility to others")
Q9<-subset(deidata2, question == "Nurses answer to your questions")
Q10<-subset(deidata2, question == "Staff's sensitivity to the difficulties that your condition and treatment can cause")
Q11<-subset(deidata2, question == "Your trust in the skill of the care providers")

#Q1
model1Q<-glm(topbox ~ age_gp + sex + race_ethnicity2 + payor2 , 
            family = 'binomial', Q1)
summary(model1Q)
modelQ1<-glmer(topbox ~ age_gp + sex + race_ethnicity2 + payor2 + (1|mrn), Q1, 
                                     family = 'binomial',control=glmerControl
                                      (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                                     nAGQ = 10, )
summary(modelQ1)
save(modelQ1,file="modelQ1.Rdata")

modelQ2<-glmer(topbox ~ age_gp + sex + race_ethnicity2 + payor2 + (1|mrn), Q2, 
               family = 'binomial',control=glmerControl
               (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
               nAGQ = 10, )
summary(modelQ2)
save(modelQ2,file="modelQ2.Rdata")

modelQ3<-glmer(topbox ~ age_gp + sex + race_ethnicity2 + payor2 + (1|mrn), Q3, 
               family = 'binomial',control=glmerControl
               (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
               nAGQ = 10, )
summary(modelQ3)
save(modelQ3,file="modelQ3.Rdata")

modelQ4<-glmer(topbox ~ age_gp + sex + race_ethnicity2 + payor2 + (1|mrn), Q4, 
               family = 'binomial',control=glmerControl
               (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
               nAGQ = 10, )
summary(modelQ4)
save(modelQ4,file="modelQ4.Rdata")

modelQ5<-glmer(topbox ~ age_gp + sex + race_ethnicity2 + payor2 + (1|mrn), Q5, 
               family = 'binomial',control=glmerControl
               (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
               nAGQ = 10, )
summary(modelQ5)
save(modelQ5,file="modelQ5.Rdata")

modelQ6<-glmer(topbox ~ age_gp + sex + race_ethnicity2 + payor2 + (1|mrn), Q6, 
               family = 'binomial',control=glmerControl
               (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
               nAGQ = 10, )
summary(modelQ6)
save(modelQ6,file="modelQ6.Rdata")

modelQ7<-glmer(topbox ~ age_gp + sex + race_ethnicity2 + payor2 + (1|mrn), Q7, 
               family = 'binomial',control=glmerControl
               (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
               nAGQ = 10, )
summary(modelQ7)
save(modelQ7,file="modelQ7.Rdata")

modelQ8<-glmer(topbox ~ age_gp + sex + race_ethnicity2 + payor2 + (1|mrn), Q8, 
               family = 'binomial',control=glmerControl
               (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
               nAGQ = 10, )
summary(modelQ8)
save(modelQ8,file="modelQ8.Rdata")

#Run logistic regression with age group and random effect:MRN ONLY
modelQ8a<-glmer(topbox ~ age_gp + (1|mrn), Q8, 
               family = 'binomial',control=glmerControl
               (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
               nAGQ = 10, )
summary(modelQ8a)
save(modelQ8a,file="modelQ8a.Rdata")


#Run logistic regression with age group ONLY, no random effect
modelQ8b<-glm(topbox ~ age_gp, Q8, 
                family = 'binomial' )
summary(modelQ8b)
save(modelQ8b,file="modelQ8b.Rdata")

modelQ9<-glmer(topbox ~ age_gp + sex + race_ethnicity2 + payor2 + (1|mrn), Q9, 
               family = 'binomial',control=glmerControl
               (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
               nAGQ = 10, )
summary(modelQ9)
save(modelQ9,file="modelQ9.Rdata")

modelQ10<-glmer(topbox ~ age_gp + sex + race_ethnicity2 + payor2 + (1|mrn), Q10, 
               family = 'binomial',control=glmerControl
               (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
               nAGQ = 10, )
summary(modelQ10)
save(modelQ10,file="modelQ10.Rdata")

modelQ11<-glmer(topbox ~ age_gp + sex + race_ethnicity2 + payor2 + (1|mrn), Q11, 
                family = 'binomial',control=glmerControl
                (optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), 
                nAGQ = 10, )
summary(modelQ11)
save(modelQ11,file="modelQ11.Rdata")

#Confidence Intervals----

seQ1 <- sqrt(diag(vcov(modelQ1)))
seQ2 <- sqrt(diag(vcov(modelQ2)))
seQ3 <- sqrt(diag(vcov(modelQ3)))
seQ4 <- sqrt(diag(vcov(modelQ4)))
seQ5 <- sqrt(diag(vcov(modelQ5)))
seQ6 <- sqrt(diag(vcov(modelQ6)))
seQ7 <- sqrt(diag(vcov(modelQ7)))
seQ8 <- sqrt(diag(vcov(modelQ8)))
seQ9 <- sqrt(diag(vcov(modelQ9)))
seQ10 <- sqrt(diag(vcov(modelQ10)))
seQ11 <- sqrt(diag(vcov(modelQ11)))
seQ8a <- sqrt(diag(vcov(modelQ8a)))

#estimates 95% confidence intervals
(tabQ1 <- cbind(Est = fixef(modelQ1), LL = fixef(modelQ1) - 1.96 * 
                    seQ1, UL = fixef(modelQ1) + 1.96 *seQ1))
(tabQ2 <- cbind(Est = fixef(modelQ2), LL = fixef(modelQ2) - 1.96 * 
                  seQ2, UL = fixef(modelQ2) + 1.96 *seQ2))
(tabQ3 <- cbind(Est = fixef(modelQ3), LL = fixef(modelQ3) - 1.96 * 
                  seQ3, UL = fixef(modelQ3) + 1.96 *seQ3))
(tabQ4 <- cbind(Est = fixef(modelQ4), LL = fixef(modelQ4) - 1.96 * 
                  seQ4, UL = fixef(modelQ4) + 1.96 *seQ4))
(tabQ5 <- cbind(Est = fixef(modelQ5), LL = fixef(modelQ5) - 1.96 * 
                  seQ5, UL = fixef(modelQ5) + 1.96 *seQ5))
(tabQ6 <- cbind(Est = fixef(modelQ6), LL = fixef(modelQ6) - 1.96 * 
                  seQ6, UL = fixef(modelQ6) + 1.96 *seQ6))
(tabQ7 <- cbind(Est = fixef(modelQ7), LL = fixef(modelQ7) - 1.96 * 
                  seQ7, UL = fixef(modelQ7) + 1.96 *seQ7))
(tabQ8 <- cbind(Est = fixef(modelQ8), LL = fixef(modelQ8) - 1.96 * 
                  seQ8, UL = fixef(modelQ8) + 1.96 *seQ8))
(tabQ9 <- cbind(Est = fixef(modelQ9), LL = fixef(modelQ9) - 1.96 * 
                  seQ9, UL = fixef(modelQ9) + 1.96 *seQ9))
(tabQ10 <- cbind(Est = fixef(modelQ10), LL = fixef(modelQ10) - 1.96 * 
                   seQ10, UL = fixef(modelQ10) + 1.96 *seQ10))
(tabQ11 <- cbind(Est = fixef(modelQ11), LL = fixef(modelQ11) - 1.96 * 
                   seQ11, UL = fixef(modelQ11) + 1.96 *seQ11))
(tabQ8a <- cbind(Est = fixef(modelQ8a), LL = fixef(modelQ8a) - 1.96 * 
                  seQ8a, UL = fixef(modelQ8a) + 1.96 *seQ8a))
#odds ratio 95% confidence intervals
exp(tabQ1)
exp(tabQ2)
exp(tabQ3)
exp(tabQ4)
exp(tabQ5)
exp(tabQ6)
exp(tabQ7)
exp(tabQ8)
exp(tabQ9)
exp(tabQ10)
exp(tabQ11)
exp(tabQ8a)

##Visualize the Data----
##forest plots for results from exp(tab_~) OR CI data

library(ggplot2)
library(ggpubr)
library(here)
#forest plots for data----
#OR, LL, UL values

#Q1.CP concern for your questions and worries----
ageQ1<-data.frame(label=c('15-24',	'25-34',	'35-44', '45-54',	'55-64',	
                          '75-84',	'85+'),
                  index=1:7,
                  OR=c(0.50, 0.62, 0.57, 0.56, 0.78, 0.94, 0.78),
                  LL=c(0.19, 0.37, 0.41, 0.44, 0.65, 0.83, 0.58),
                  UL=c(1.28, 1.06, 0.79, 0.70, 0.94, 1.08, 1.05 ))

p1Q1<-ggplot(data=ageQ1, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(ageQ1), labels=ageQ1$label) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25))+
  labs(title='"CP concern for your questions and worries" by Age Group', 
       subtitle= 'Comparison Group: 65-74 yr olds', 
       x='OR', y = 'Age Group') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.75, y = nrow(ageQ1)+1, 
           label = "TopBox Less Likely") +
  annotate("text", x = 1.15, y = nrow(ageQ1)+1, 
           label = "TopBox More Likely") +
  theme_minimal()
p1Q1
ggsave(p1Q1,file="p1Q1.png")
raceQ1<-data.frame(label=c('Asian','Black',
                           'Hispanic',
                           'More than one race/Other'),
                   index=1:4,
                   OR=c(0.63,0.79,1.13,0.50),
                   LL=c(0.39,0.64,0.93,0.27),
                   UL=c(0.99,0.99,1.37,0.91))

p2Q1<-ggplot(data=raceQ1, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(raceQ1), labels=raceQ1$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5))+
  labs(title='"CP concern for your questions and worries" by Race', 
       subtitle='Comparison Group: White',
       x='OR', y = 'Race') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(raceQ1)+1, label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(raceQ1)+1, label = "TopBox More Likely") +
  theme_minimal()
p2Q1
ggsave(p2Q1,file="p2Q1.png")

payorQ1<-data.frame(label=c('HMOPPO', 'OtherIns', 'Medicaid', 'Charity', 
                            'SelfPay'),
                    index=1:5,
                    OR=c(1.24,0.82,0.79,1.04,0.71),
                    LL=c(1.04,0.61,0.53,0.67,0.42),
                    UL=c(1.49,1.11,1.17,1.64,1.23))

p3Q1<-ggplot(data=payorQ1, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(payorQ1), labels=payorQ1$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5))+
  labs(title='"CP concern for your questions and worries" by Payer',
       subtitle='Comparison Group: Medicare', x='OR', y = 'Insurance') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(payorQ1)+1,
           label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(payorQ1)+1, 
           label = "TopBox More Likely") +
  theme_minimal()

p3Q1
ggsave(p3Q1,file="p3Q1.png")
#Q2.CP Discussion of your Treatment Options----
ageQ2<-data.frame(label=c('15-24',	'25-34',	'35-44', '45-54',	'55-64',	
                          '75-84',	'85+'),
                  index=1:7,
                  OR=c(0.50, 0.62, 0.57, 0.56, 0.78, 0.94, 0.78),
                  LL=c(0.19, 0.37, 0.41, 0.44, 0.65, 0.83, 0.58),
                  UL=c(1.28, 1.06, 0.79, 0.70, 0.94, 1.08, 1.05 ))

p1Q2<-ggplot(data=ageQ2, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(ageQ2), labels=ageQ2$label) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25))+
  labs(title='"CP discussion of your treatment options" by Age Group', 
       subtitle= 'Comparison Group: 65-74 yr olds', x='OR', y = 'Age Group') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.75, y = nrow(ageQ2)+1, 
           label = "TopBox Less Likely") +
  annotate("text", x = 1.15, y = nrow(ageQ2)+1, 
           label = "TopBox More Likely") +
  theme_minimal()
p1Q2
ggsave(p1Q2,file="p1Q2.png")

raceQ2<-data.frame(label=c('Asian','Black',
                           'Hispanic',
                           'More than one race/Other'),
                   index=1:4,
                   OR=c(0.67,0.78,1.22,0.38),
                   LL=c(.43,0.63,1.01,0.22),
                   UL=c(1.06,0.96,1.48,0.68))

p2Q2<-ggplot(data=raceQ2, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(raceQ2), labels=raceQ2$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5))+
  labs(title='"CP discussion of your treatment options" by Race', 
       subtitle='Comparison Group: White',
       x='OR', y = 'Race') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(raceQ2)+1, label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(raceQ2)+1, label = "TopBox More Likely") +
  theme_minimal()
p2Q2
ggsave(p2Q2,file="p2Q2.png")

payorQ2<-data.frame(label=c('HMOPPO', 'OtherIns', 'Medicaid', 'Charity', 
                            'SelfPay'),
                    index=1:5,
                    OR=c(1.19,1.00,0.87,1.25,0.70),
                    LL=c(1.00,0.74,0.58,0.76,0.41),
                    UL=c(1.42,1.34,1.28,1.95,1.19))

p3Q2<-ggplot(data=payorQ2, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(payorQ2), labels=payorQ2$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5))+
  labs(title='"CP discussion of your treatment options" by Payer',
       subtitle='Comparison Group: Medicare', x='OR', y = 'Insurance') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(payorQ2)+1,
           label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(payorQ2)+1, 
           label = "TopBox More Likely") +
  theme_minimal()

p3Q2
ggsave(p3Q2,file="p3Q2.png")

#Q3.CP Efforts to include you in decisions about your treatment----
ageQ3<-data.frame(label=c('15-24',	'25-34',	'35-44', '45-54',	'55-64',	
                          '75-84',	'85+'),
                  index=1:7,
                  OR=c(0.79,0.61,0.57,0.71,0.85,0.89,0.92),
                  LL=c(0.29,0.36,0.41,0.56,0.71,0.78,0.67),
                  UL=c(2.18,1.04,0.80,0.90,1.03,1.02,1.25 ))

p1Q3<-ggplot(data=ageQ3, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(ageQ3), labels=ageQ3$label) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25))+
  labs(title='"CP efforts to include you in decisions about your treatment" by Age Group', 
       subtitle= 'Comparison Group: 65-74 yr olds', x='OR', y = 'Age Group') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.75, y = nrow(ageQ3)+1, 
           label = "TopBox Less Likely") +
  annotate("text", x = 1.15, y = nrow(ageQ3)+1, 
           label = "TopBox More Likely") +
  theme_minimal()
p1Q3
ggsave(p1Q3,file="p1Q3.png")

raceQ3<-data.frame(label=c('Asian','Black',
                           'Hispanic',
                           'More than one race/Other'),
                   index=1:4,
                   OR=c(0.68,0.87,1.20,0.48),
                   LL=c(0.43,0.70,0.99,0.26),
                   UL=c(1.10,1.09,1.46,0.89))

p2Q3<-ggplot(data=raceQ3, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(raceQ3), labels=raceQ3$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5))+
  labs(title='"CP efforts to include you in decisions about your treatment" by Race', 
       subtitle='Comparison Group: White',
       x='OR', y = 'Race') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(raceQ3)+1, label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(raceQ3)+1, label = "TopBox More Likely") +
  theme_minimal()
p2Q3
ggsave(p2Q3,file="p2Q3.png")

payorQ3<-data.frame(label=c('HMOPPO', 'OtherIns', 'Medicaid', 'Charity', 
                            'SelfPay'),
                    index=1:5,
                    OR=c(1.24,1.02,0.75,1.21,0.62),
                    LL=c(1.03,0.75,0.50,0.76,0.36),
                    UL=c(1.49,1.39,1.13,1.94,1.08))

p3Q3<-ggplot(data=payorQ3, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(payorQ3), labels=payorQ3$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5))+
  labs(title='"CP efforts to include you in decisions about your treatment" by Payer',
       subtitle='Comparison Group: Medicare', x='OR', y = 'Insurance') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(payorQ3)+1,
           label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(payorQ3)+1, 
           label = "TopBox More Likely") +
  theme_minimal()

p3Q3
ggsave(p3Q3,file="p3Q3.png")


#Q4.Caring Manner of the Nurses----
ageQ4<-data.frame(label=c('15-24',	'25-34',	'35-44', '45-54',	'55-64',	
                          '75-84',	'85+'),
                  index=1:7,
                  OR=c(0.88,0.74,0.81,0.99,0.89,1.03,0.81),
                  LL=c(0.31,0.42,0.57,0.77,0.73,0.90,0.59),
                  UL=c(2.54,1.30,1.17,1.29,1.09,1.19,1.12 ))

p1Q4<-ggplot(data=ageQ4, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(ageQ4), labels=ageQ4$label) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25))+
  labs(title='"Caring manner of the nurses" by Age Group', 
       subtitle= 'Comparison Group: 65-74 yr olds', x='OR', y = 'Age Group') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.75, y = nrow(ageQ4)+1, 
           label = "TopBox Less Likely") +
  annotate("text", x = 1.15, y = nrow(ageQ4)+1, 
           label = "TopBox More Likely") +
  theme_minimal()
p1Q4
ggsave(p1Q4,file="p1Q4.png")

raceQ4<-data.frame(label=c('Asian','Black',
                           'Hispanic',
                           'More than one race/Other'),
                   index=1:4,
                   OR=c(0.54,0.69,0.95,0.32),
                   LL=c(0.33,0.55,0.77,0.17),
                   UL=c(0.88,0.87,1.16,0.59))

p2Q4<-ggplot(data=raceQ4, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(raceQ4), labels=raceQ4$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5))+
  labs(title='"Caring manner of the nurses" by Race', 
       subtitle='Comparison Group: White',
       x='OR', y = 'Race') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(raceQ4)+1, label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(raceQ4)+1, label = "TopBox More Likely") +
  theme_minimal()
p2Q4
ggsave(p2Q4,file="p2Q4.png")

payorQ4<-data.frame(label=c('HMOPPO', 'OtherIns', 'Medicaid', 'Charity', 
                            'SelfPay'),
                    index=1:5,
                    OR=c(1.14,0.88,0.82,1.36,0.63),
                    LL=c(0.94,0.64,0.53,0.82,0.36),
                    UL=c(1.38,1.22,1.26,2.24,1.13))

p3Q4<-ggplot(data=payorQ4, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(payorQ4), labels=payorQ4$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5))+
  labs(title='"Caring manner of the nurses" by Payer',
       subtitle='Comparison Group: Medicare', x='OR', y = 'Insurance') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(payorQ4)+1,
           label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(payorQ4)+1, 
           label = "TopBox More Likely") +
  theme_minimal()

p3Q4
ggsave(p3Q4,file="p3Q4.png")


#Q5.Efforts to include you in decisions about your treatment----
ageQ5<-data.frame(label=c('15-24','25-34','35-44', '45-54','55-64', '75-84','85+'),
                  index=1:7,
                  OR=c(0.43,0.58,0.80,0.71,0.84,1.03,1.12),
                  LL=c(0.17,0.35,0.57,0.56,0.69,0.90,0.82),
                  UL=c(1.08,0.98,1.12,0.91,1.01,1.18,1.53))

p1Q5<-ggplot(data=ageQ5, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(ageQ5), labels=ageQ5$label) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25))+
  labs(title='"Efforts to include you in decisions about your treatment" by Age Group', 
       subtitle= 'Comparison Group: 65-74 yr olds', x='OR', y = 'Age Group') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.75, y = nrow(ageQ5)+1, 
           label = "TopBox Less Likely") +
  annotate("text", x = 1.15, y = nrow(ageQ5)+1, 
           label = "TopBox More Likely") +
  theme_minimal()
p1Q5
ggsave(p1Q5,file="p1Q5.png")

raceQ5<-data.frame(label=c('Asian','Black',
                           'Hispanic',
                           'More than one race/Other'),
                   index=1:4,
                   OR=c(0.62,0.68,1.00,0.37),
                   LL=c(0.38,0.55,0.83,0.20),
                   UL=c(1.00,0.85,1.21,0.67))

p2Q5<-ggplot(data=raceQ5, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(raceQ5), labels=raceQ5$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5))+
  labs(title='"Efforts to include you in decisions about your treatment" by Race', 
       subtitle='Comparison Group: White',
       x='OR', y = 'Race') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(raceQ5)+1, label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(raceQ5)+1, label = "TopBox More Likely") +
  theme_minimal()
p2Q5
ggsave(p2Q5,file="p2Q5.png")

payorQ5<-data.frame(label=c('HMOPPO', 'OtherIns', 'Medicaid', 'Charity', 
                            'SelfPay'),
                    index=1:5,
                    OR=c(1.46,0.86,1.03,1.57,0.65),
                    LL=c(1.21,0.65,0.69,0.99,0.37),
                    UL=c(1.75,1.15,1.54,2.49,1.13))

p3Q5<-ggplot(data=payorQ5, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(payorQ5), labels=payorQ5$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5))+
  labs(title='"Efforts to include you in decisions about your treatment" by Payer',
       subtitle='Comparison Group: Medicare', x='OR', y = 'Insurance') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(payorQ5)+1,
           label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(payorQ5)+1, 
           label = "TopBox More Likely") +
  theme_minimal()

p3Q5
ggsave(p3Q5,file="p3Q5.png")


#Q6.How well the CP kept you informed about your condition----
ageQ6<-data.frame(label=c('15-24','25-34','35-44', '45-54','55-64', '75-84','85+'),
                  index=1:7,
                  OR=c(0.4,0.56,0.55,0.61,0.83,0.95,0.82),
                  LL=c(0.16,0.33,0.39,0.48,0.68,0.83,0.61),
                  UL=c(1.02,0.95,0.77,0.77,1.00,1.08,1.10))

p1Q6<-ggplot(data=ageQ6, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(ageQ6), labels=ageQ6$label) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25))+
  labs(title='"How well the CPs kept you informed about your condition" by Age Group', 
       subtitle= 'Comparison Group: 65-74 yr olds', x='OR', y = 'Age Group') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.75, y = nrow(ageQ6)+1, 
           label = "TopBox Less Likely") +
  annotate("text", x = 1.15, y = nrow(ageQ6)+1, 
           label = "TopBox More Likely") +
  theme_minimal()
p1Q6
ggsave(p1Q6,file="p1Q6.png")


raceQ6<-data.frame(label=c('Asian','Black',
                           'Hispanic',
                           'More than one race/Other'),
                   index=1:4,
                   OR=c(0.87,0.90,1.21,0.41),
                   LL=c(0.54,0.72,1.00,0.23),
                   UL=c(1.41,1.13,1.47,0.74))

p2Q6<-ggplot(data=raceQ6, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(raceQ6), labels=raceQ6$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5))+
  labs(title='"How well the CPs kept you informed about your condition" by Race', 
       subtitle='Comparison Group: White',
       x='OR', y = 'Race') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(raceQ6)+1, label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(raceQ6)+1, label = "TopBox More Likely") +
  theme_minimal()
p2Q6
ggsave(p2Q6,file="p2Q6.png")


payorQ6<-data.frame(label=c('HMOPPO', 'OtherIns', 'Medicaid', 'Charity', 
                            'SelfPay'),
                    index=1:5,
                    OR=c(1.22,0.98,1.15,1.15,0.65),
                    LL=c(1.02,0.72,0.76,0.73,0.38),
                    UL=c(1.46,1.33,1.75,1.81,1.11))

p3Q6<-ggplot(data=payorQ6, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(payorQ6), labels=payorQ6$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5))+
  labs(title='"How well the CPs kept you informed about your condition" by Payer',
       subtitle='Comparison Group: Medicare', x='OR', y = 'Insurance') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(payorQ6)+1,
           label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(payorQ6)+1, 
           label = "TopBox More Likely") +
  theme_minimal()

p3Q6
ggsave(p3Q6,file="p3Q6.png")

#Q7.How well the staff worked together to care for you----
ageQ7<-data.frame(label=c('15-24','25-34','35-44', '45-54','55-64', '75-84','85+'),
                  index=1:7,
                  OR=c(0.47,0.52,0.67,0.78,0.80,1.07,0.98),
                  LL=c(0.18,0.31,0.48,0.61,0.66,0.93,0.72),
                  UL=c(1.24,0.88,0.94,0.99,0.97,1.22,1.33))

p1Q7<-ggplot(data=ageQ7, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(ageQ7), labels=ageQ7$label) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25))+
  labs(title='"How well the staff worked together to care for you" by Age Group', 
       subtitle= 'Comparison Group: 65-74 yr olds', x='OR', y = 'Age Group') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.75, y = nrow(ageQ7)+1, 
           label = "TopBox Less Likely") +
  annotate("text", x = 1.15, y = nrow(ageQ7)+1, 
           label = "TopBox More Likely") +
  theme_minimal()
p1Q7
ggsave(p1Q7,file="p1Q7.png")

raceQ7<-data.frame(label=c('Asian','Black',
                           'Hispanic',
                           'More than one race/Other'),
                   index=1:4,
                   OR=c(0.56,0.69,1.06,0.25),
                   LL=c(0.35,0.55,0.87,0.14),
                   UL=c(0.90,0.86,1.29,0.44))

p2Q7<-ggplot(data=raceQ7, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(raceQ7), labels=raceQ7$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5))+
  labs(title='"How well the staff worked together to care for you" by Race', 
       subtitle='Comparison Group: White',
       x='OR', y = 'Race') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(raceQ7)+1, label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(raceQ7)+1, label = "TopBox More Likely") +
  theme_minimal()
p2Q7
ggsave(p2Q7,file="p2Q7.png")


payorQ7<-data.frame(label=c('HMOPPO', 'OtherIns', 'Medicaid', 'Charity', 
                            'SelfPay'),
                    index=1:5,
                    OR=c(1.29,0.97,0.95,1.17,1.15),
                    LL=c(1.07,0.72,0.63,0.74,0.63),
                    UL=c(1.55,1.31,1.43,1.85,2.10))

p3Q7<-ggplot(data=payorQ7, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(payorQ7), labels=payorQ7$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5))+
  labs(title='"How well the staff worked together to care for you" by Payer',
       subtitle='Comparison Group: Medicare', x='OR', y = 'Insurance') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(payorQ7)+1,
           label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(payorQ7)+1, 
           label = "TopBox More Likely") +
  theme_minimal()

p3Q7
ggsave(p3Q7,file="p3Q7.png")

#Q8.Likelihood of Recommending this facility to others-----
ageQ8<-data.frame(label=c('15-24','25-34','35-44', '45-54','55-64', '75-84','85+'),
                  index=1:7,
                  OR=c(0.09,11.07,1.63,1.97,1.26,1.69,3.90),
                  LL=c(0.03,0.71,0.74,1.20,1.26,1.20,1.20),
                  UL=c(0.31,172,3.59,3.25,1.26,2.37,12.7))

p1Q8<-ggplot(data=ageQ8, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(ageQ8), labels=ageQ8$label) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25))+
  labs(title='"Likelihood of your recommending this facility to others" by Age Group', 
       subtitle= 'Comparison Group: 65-74 yr olds', x='OR', y = 'Age Group') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.75, y = nrow(ageQ8)+1, 
           label = "TopBox Less Likely") +
  annotate("text", x = 1.15, y = nrow(ageQ8)+1, 
           label = "TopBox More Likely") +
  theme_minimal()
p1Q8
ggsave(p1Q8,file="p1Q8.png")

raceQ8<-data.frame(label=c('Asian','Black',
                           'Hispanic',
                           'More than one race/Other'),
                   index=1:4,
                   OR=c(0.16,1.05,1.38,0.05),
                   LL=c(0.08,0.58,0.79,0.02),
                   UL=c(0.36,1.89,2.39,0.14))

p2Q8<-ggplot(data=raceQ8, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(raceQ8), labels=raceQ8$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5))+
  labs(title='"Likelihood of your recommending this facility to others" by Race', 
       subtitle='Comparison Group: White',
       x='OR', y = 'Race') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(raceQ8)+1, label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(raceQ8)+1, label = "TopBox More Likely") +
  theme_minimal()
p2Q8
ggsave(p2Q8,file="p2Q8.png")


payorQ8<-data.frame(label=c('HMOPPO', 'OtherIns', 'Medicaid', 'Charity', 
                            'SelfPay'),
                    index=1:5,
                    OR=c(0.57,0.70,0.17,11.24,0.96),
                    LL=c(0.57,0.69,0.08,0.80,0.20),
                    UL=c(0.57,1.36,0.34,157.78,4.59))

p3Q8<-ggplot(data=payorQ8, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(payorQ8), labels=payorQ8$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5))+
  labs(title='"Likelihood of your recommending this facility to others" by Payer',
       subtitle='Comparison Group: Medicare', x='OR', y = 'Insurance') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(payorQ8)+1,
           label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(payorQ8)+1, 
           label = "TopBox More Likely") +
  theme_minimal()

p3Q8
ggsave(p3Q8,file="p3Q8.png")

#Q9.Nurses answer to your questions----
ageQ9<-data.frame(label=c('15-24','25-34','35-44', '45-54','55-64', '75-84','85+'),
                  index=1:7,
                  OR=c(0.83,0.68,0.85,0.88,0.96,1.01,0.86),
                  LL=c(0.32,0.41,0.61,0.70,0.80,0.89,0.65),
                  UL=c(2.19,1.14,1.18,1.12,1.15,1.14,1.16))

p1Q9<-ggplot(data=ageQ9, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(ageQ9), labels=ageQ9$label) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25))+
  labs(title='"Nurses answer to your questions" by Age Group', 
       subtitle= 'Comparison Group: 65-74 yr olds', x='OR', y = 'Age Group') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.75, y = nrow(ageQ9)+1, 
           label = "TopBox Less Likely") +
  annotate("text", x = 1.15, y = nrow(ageQ9)+1, 
           label = "TopBox More Likely") +
  theme_minimal()
p1Q9
ggsave(p1Q9,file="p1Q9.png")

raceQ9<-data.frame(label=c('Asian','Black',
                           'Hispanic',
                           'More than one race/Other'),
                   index=1:4,
                   OR=c(0.69,0.64,1.03,0.50),
                   LL=c(0.43,0.52,0.85,0.28),
                   UL=c(1.10,0.78,1.23,0.91))

p2Q9<-ggplot(data=raceQ9, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(raceQ9), labels=raceQ9$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5))+
  labs(title='"Nurses answer to your questions" by Race', 
       subtitle='Comparison Group: White',
       x='OR', y = 'Race') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(raceQ9)+1, label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(raceQ9)+1, label = "TopBox More Likely") +
  theme_minimal()
p2Q9
ggsave(p2Q9,file="p2Q9.png")

payorQ9<-data.frame(label=c('HMOPPO', 'OtherIns', 'Medicaid', 'Charity', 
                            'SelfPay'),
                    index=1:5,
                    OR=c(1.09,0.84,0.83,1.27,0.61),
                    LL=c(0.91,0.63,0.57,0.82,0.36),
                    UL=c(1.30,1.12,1.23,1.98,1.03))

p3Q9<-ggplot(data=payorQ9, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(payorQ9), labels=payorQ9$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5))+
  labs(title='"Nurses answer to your questions" by Payer',
       subtitle='Comparison Group: Medicare', x='OR', y = 'Insurance') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(payorQ9)+1,
           label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(payorQ9)+1, 
           label = "TopBox More Likely") +
  theme_minimal()

p3Q9
ggsave(p3Q9,file="p3Q9.png")

#Q10.Staff sensitivity to the difficulties that your condition and treatment can cause----
ageQ10<-data.frame(label=c('15-24','25-34','35-44', '45-54','55-64', '75-84','85+'),
                   index=1:7,
                   OR=c(0.30,0.71,0.85,0.84,0.92,1.01,0.98),
                   LL=c(0.12,0.42,0.61,0.66,0.77,0.88,0.72),
                   UL=c(0.73,1.20,1.18,1.06,1.11,1.15,1.33))

p1Q10<-ggplot(data=ageQ10, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(ageQ10), labels=ageQ10$label) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25))+
  labs(title='"Staff sensitivity to the difficulties that your condition and treatment can cause" by Age Group', 
       subtitle= 'Comparison Group: 65-74 yr olds', x='OR', y = 'Age Group') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.75, y = nrow(ageQ10)+1, 
           label = "TopBox Less Likely") +
  annotate("text", x = 1.15, y = nrow(ageQ10)+1, 
           label = "TopBox More Likely") +
  theme_minimal()
p1Q10
ggsave(p1Q10,file="p1Q10.png")

raceQ10<-data.frame(label=c('Asian','Black',
                            'Hispanic',
                            'More than one race/Other'),
                    index=1:4,
                    OR=c(0.80,0.67,1.16,0.43),
                    LL=c(0.49,0.54,0.96,0.23),
                    UL=c(1.30,0.83,1.40,0.79))

p2Q10<-ggplot(data=raceQ10, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(raceQ10), labels=raceQ10$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5))+
  labs(title='"Staff sensitivity to the difficulties that your condition and treatment can cause" by Race', 
       subtitle='Comparison Group: White',
       x='OR', y = 'Race') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(raceQ10)+1, label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(raceQ10)+1, label = "TopBox More Likely") +
  theme_minimal()
p2Q10
ggsave(p2Q10,file="p2Q10.png")

payorQ10<-data.frame(label=c('HMOPPO', 'OtherIns', 'Medicaid', 'Charity', 
                             'SelfPay'),
                     index=1:5,
                     OR=c(1.32,0.87,1.02,1.38,0.70),
                     LL=c(1.11,0.65,0.68,0.88,0.40),
                     UL=c(1.58,1.16,1.51,2.15,1.22))

p3Q10<-ggplot(data=payorQ10, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(payorQ10), labels=payorQ10$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5))+
  labs(title='"Staff sensitivity to the difficulties that your condition and treatment can cause" by Payer',
       subtitle='Comparison Group: Medicare', x='OR', y = 'Insurance') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(payorQ10)+1,
           label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(payorQ10)+1, 
           label = "TopBox More Likely") +
  theme_minimal()

p3Q10
ggsave(p3Q10,file="p3Q10.png")

#Q11.Your Trust in the Skill of the CP----
ageQ11<-data.frame(label=c('15-24','25-34','35-44', '45-54','55-64', '75-84','85+'),
                   index=1:7,
                   OR=c(0.67,0.58,0.43,0.57,0.79,1.16,0.88),
                   LL=c(0.23,0.33,0.30,0.44,0.64,1.00,0.63),
                   UL=c(1.97,1.03,0.61,0.73,0.97,1.34,1.22))

p1Q11<-ggplot(data=ageQ11, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(ageQ11), labels=ageQ11$label) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25))+
  labs(title='"Your trust in the skill of the CP" by Age Group', 
       subtitle= 'Comparison Group: 65-74 yr olds', x='OR', y = 'Age Group') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.75, y = nrow(ageQ11)+1, 
           label = "TopBox Less Likely") +
  annotate("text", x = 1.15, y = nrow(ageQ11)+1, 
           label = "TopBox More Likely") +
  theme_minimal()
p1Q11
ggsave(p1Q11,file="p1Q11.png")
raceQ11<-data.frame(label=c('Asian','Black',
                            'Hispanic',
                            'More than one race/Other'),
                    index=1:4,
                    OR=c(0.52,0.63,0.95,0.33),
                    LL=c(0.32,0.50,0.78,0.18),
                    UL=c(0.86,0.79,1.17,0.62))

p2Q11<-ggplot(data=raceQ11, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(raceQ11), labels=raceQ11$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5))+
  labs(title='"Your trust in the skill of the CP" by Race', 
       subtitle='Comparison Group: White',
       x='OR', y = 'Race') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(raceQ11)+1, label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(raceQ11)+1, label = "TopBox More Likely") +
  theme_minimal()
p2Q11
ggsave(p2Q11,file="p2Q11.png")

payorQ11<-data.frame(label=c('HMOPPO', 'OtherIns', 'Medicaid', 'Charity', 
                             'SelfPay'),
                     index=1:5,
                     OR=c(1.23,1.07,0.89,1.32,0.64),
                     LL=c(1.01,0.76,0.58,0.80,0.36),
                     UL=c(1.50,1.49,1.38,2.17,1.14))

p3Q11<-ggplot(data=payorQ11, aes(y=index, x=OR, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(payorQ11), labels=payorQ11$label) +
  scale_x_continuous(breaks=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5))+
  labs(title='"Your trust in the skill of the CP" by Payer',
       subtitle='Comparison Group: Medicare', x='OR', y = 'Insurance') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.50, y = nrow(payorQ11)+1,
           label = "TopBox Less Likely") +
  annotate("text", x = 1.50, y = nrow(payorQ11)+1, 
           label = "TopBox More Likely") +
  theme_minimal()

p3Q11
ggsave(p3Q11,file="p3Q11.png")
