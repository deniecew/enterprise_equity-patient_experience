##Visualize the Data----
#load packages
library(tidyverse)
library(janitor)

#load data
deioutput<-read.csv("G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/Model Estimates.csv")
clean_names(deioutput)
glimpse(deioutput)
save(deioutput,file="G:/Press Ganey II/Reports/Ad Hoc/Deniece/Enterprise Equity/Nov2023_Reproduction/deioutput.Rdata")

##forest plots for results from exp(tab_~) OR CI data

library(ggplot2)
library(ggpubr)
library(here)

#forest plots for data----
# CP23<-subset(deioutput,VarName=='CP23'& Fixed.Effect=='Age Group')
# fplot<-ggplot(data=CP23, aes(y=1:nrow(CP23), x=Odds.Estimate, xmin=LL, xmax=UL)) +
#   geom_point() +
#   geom_errorbarh(height=.1) +
#   scale_y_continuous(breaks=1:nrow(CP23), labels=CP23$Level) +
#   scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0))+
#   labs(title='"CPs efforts to include you in decisions about your treatment" by Age Group', 
#        subtitle= 'Comparison Group: 65-74 yr olds', 
#        x='OR', y = 'Age Group') +
#   geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
#   annotate("text", x = 0.5, y = nrow(CP23)+1, 
#            label = "Less Likely") +
#   annotate("text", x = 1.5, y = nrow(CP23)+1, 
#            label = "More Likely") +
#   theme_minimal()
# fplot

CP23<-subset(deioutput,VarName=='CP23'& Fixed.Effect !='Intercept')
CP23plot<-ggplot(data=CP23, aes(y=1:nrow(CP23), x=Odds.Estimate, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(CP23), labels=CP23$Level) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0))+
  labs(title='CPs efforts to include you in \ndecisions about your treatment', 
      x='OR', y = '') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.5, y = nrow(CP23)+1, 
           label = "Less Likely") +
  annotate("text", x = 1.5, y = nrow(CP23)+1, 
           label = "More Likely") +
  theme_minimal()

DCP5<-subset(deioutput,VarName=='DCP5'& Fixed.Effect !='Intercept')
DCP5plot<-ggplot(data=DCP5, aes(y=1:nrow(DCP5), x=Odds.Estimate, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(DCP5), labels=DCP5$Level) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0))+
  labs(title='CPs discussion of your \ntreatment options', 
       x='OR', y = '') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.5, y = nrow(DCP5)+1, 
           label = "Less Likely") +
  annotate("text", x = 1.5, y = nrow(DCP5)+1, 
           label = "More Likely") +
  theme_minimal()

I3<-subset(deioutput,VarName=='I3'& Fixed.Effect !='Intercept')
I3plot<-ggplot(data=I3, aes(y=1:nrow(I3), x=Odds.Estimate, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(I3), labels=I3$Level) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0))+
  labs(title='Staff sensitivity to the difficulties \nthat your condition and treatment \ncan cause', 
       x='OR', y = '') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.5, y = nrow(I3)+1, 
           label = "Less Likely") +
  annotate("text", x = 1.5, y = nrow(I3)+1, 
           label = "More Likely") +
  theme_minimal()

I4<-subset(deioutput,VarName=='I4'& Fixed.Effect !='Intercept')
I4plot<-ggplot(data=I4, aes(y=1:nrow(I4), x=Odds.Estimate, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(I4), labels=I4$Level) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0))+
  labs(title='Efforts to include you in \ndecisions about your treatment', 
       x='OR', y = '') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.5, y = nrow(I4)+1, 
           label = "Less Likely") +
  annotate("text", x = 1.5, y = nrow(I4)+1, 
           label = "More Likely") +
  theme_minimal()

MED3<-subset(deioutput,VarName=='MED3'& Fixed.Effect !='Intercept')
MED3plot<-ggplot(data=MED3, aes(y=1:nrow(MED3), x=Odds.Estimate, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(MED3), labels=MED3$Level) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0))+
  labs(title='How well the CPs kept you \ninformed about your condition', 
       x='OR', y = '') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.5, y = nrow(MED3)+1, 
           label = "Less Likely") +
  annotate("text", x = 1.5, y = nrow(MED3)+1, 
           label = "More Likely") +
  theme_minimal()

MED5<-subset(deioutput,VarName=='MED5'& Fixed.Effect !='Intercept')
MED5plot<-ggplot(data=MED5, aes(y=1:nrow(MED5), x=Odds.Estimate, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(MED5), labels=MED5$Level) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0))+
  labs(title='CPs concern for your \nquestions and worries', 
       x='OR', y = '') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.5, y = nrow(MED5)+1, 
           label = "Less Likely") +
  annotate("text", x = 1.5, y = nrow(MED5)+1, 
           label = "More Likely") +
  theme_minimal()

MED6<-subset(deioutput,VarName=='MED6'& Fixed.Effect !='Intercept')
MED6plot<-ggplot(data=MED6, aes(y=1:nrow(MED6), x=Odds.Estimate, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(MED6), labels=MED6$Level) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0))+
  labs(title='Your trust in the skill of the CPs', 
       x='OR', y = '') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.5, y = nrow(MED6)+1, 
           label = "Less Likely") +
  annotate("text", x = 1.5, y = nrow(MED6)+1, 
           label = "More Likely") +
  theme_minimal()

MED23<-subset(deioutput,VarName=='MED23'& Fixed.Effect !='Intercept')
MED23plot<-ggplot(data=MED23, aes(y=1:nrow(MED23), x=Odds.Estimate, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(MED23), labels=MED23$Level) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0))+
  labs(title='Caring manner of the nurses', 
       x='OR', y = '') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.5, y = nrow(MED23)+1, 
           label = "Less Likely") +
  annotate("text", x = 1.5, y = nrow(MED23)+1, 
           label = "More Likely") +
  theme_minimal()

MED24<-subset(deioutput,VarName=='MED24'& Fixed.Effect !='Intercept')
MED24plot<-ggplot(data=MED24, aes(y=1:nrow(MED24), x=Odds.Estimate, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(MED24), labels=MED24$Level) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0))+
  labs(title='Nurses answer to your questions', 
       x='OR', y = '') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.5, y = nrow(MED24)+1, 
           label = "Less Likely") +
  annotate("text", x = 1.5, y = nrow(MED24)+1, 
           label = "More Likely") +
  theme_minimal()

O1<-subset(deioutput,VarName=='O1'& Fixed.Effect !='Intercept')
O1plot<-ggplot(data=O1, aes(y=1:nrow(O1), x=Odds.Estimate, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(O1), labels=O1$Level) +
  scale_x_continuous(breaks=c(0.00,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0))+
  labs(title='How well the staff worked together\nto care for you', 
       x='OR', y = '') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.5, y = nrow(O1)+1, 
           label = "Less Likely") +
  annotate("text", x = 1.5, y = nrow(O1)+1, 
           label = "More Likely") +
  theme_minimal()

O3<-subset(deioutput,VarName=='O3'& Fixed.Effect !='Intercept'&  Level != 'Self-Pay'& Level != '25-34')
O3plot<-ggplot(data=O3, aes(y=1:nrow(O3), x=Odds.Estimate, xmin=LL, xmax=UL)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(O3), labels=O3$Level) +
  scale_x_continuous(breaks=c(0.00,1.0,2.0,3.0,4.0))+
  labs(title='Likelihood of your recommending \nthis facility to others', 
       x='OR', y = '') +
  geom_vline(xintercept=1, color='red', linetype='dashed', alpha=.5) +
  annotate("text", x = 0.5, y = nrow(O3)+1, 
           label = "Less \nLikely") +
  annotate("text", x = 1.5, y = nrow(O3)+1, 
           label = "More \nLikely") +
  theme_minimal()

CP23plot
DCP5plot
I3plot
I4plot
MED3plot
MED5plot
MED6plot
MED23plot
MED24plot
O1plot
O3plot