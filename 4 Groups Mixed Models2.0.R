####Using lme4 for Mixed Models####
#library(dplyr)
#library(tidyr)
library(lme4)
library(lmerTest) # show the results,  p-values change in Mixed Models (DO THEY?)

#Simplified Dataset#
all4sems=read.csv("C:/Users/Jcolton/Documents/Jesse's Documents/Documents/Education/Data/4Semesters3.0.csv",
                  header=TRUE, stringsAsFactors=FALSE)
attach(all4sems)

esl=factor(esl)
bioclass=factor(bioclass)
gender=factor(gender)
race=factor(race)
race=relevel(race,ref="3")
semester=factor(semester)
id=factor(id)


#Dataset with all timepoints combined#
all4semstimes=read.csv("C:/Users/Jcolton/Documents/Jesse's Documents/Documents/Education/Data/4SemestersAllTime3.0.csv",
                       header=TRUE, stringsAsFactors=FALSE)
attach(all4semstimes)

alltime.esl=factor(alltime.esl)
alltime.bioclass=factor(alltime.bioclass)
alltime.gender=factor(alltime.gender)
alltime.race=factor(alltime.race)
alltime.race=relevel(alltime.race,ref="3")
alltime.semester=factor(alltime.semester)
alltime.id=factor(alltime.id)
timepoint=factor(timepoint,levels=c("pre","post","delpost"))





#Dataset with all 3 times and both types combined#
all4semstimestypes=read.csv("C:/Users/Jcolton/Documents/Jesse's Documents/Documents/Education/Data/4SemestersAllTimeandType3.0.csv",
                            header=TRUE, stringsAsFactors=FALSE)
attach(all4semstimestypes)


alltime.allesl=factor(alltime.allesl)
alltime.allbioclass=factor(alltime.allbioclass)
alltime.allgender=factor(alltime.allgender)
alltime.allrace=relevel(factor(alltime.allrace),ref="3")
alltime.allsemester=factor(alltime.allsemester)
alltime.type=relevel(factor(alltime.type),ref="P")
all.timepoint=factor(all.timepoint,levels=c("pre","post","delpost"))
alltime.allid=factor(alltime.allid)





####Mixed Models####

#All times, Animal/Plant separate#

#Animal KC#
m1 = lmer(alltime.skc ~ alltime.semester*timepoint + (1|alltime.id))  #Random Intercept
anova(m1)
summary(m1)

m2 = lmer(alltime.skc ~ alltime.semester*timepoint + alltime.esl + alltime.reading + alltime.writing +
            alltime.bioclass + alltime.gender + alltime.age + alltime.race + alltime.cins.pre +
            alltime.isea.pre + (1|alltime.id))  #Random Intercept
anova(m2)
summary(m2)


m3 = lmer(alltime.skc ~ alltime.semester*timepoint + (timepoint|alltime.id))  #Random Intercept + Slope
anova(m3)   #ERROR*** number obs<=number random effects
summary(m3)

#Plant KC#
m1 = lmer(alltime.rkc ~ alltime.semester*timepoint + (1|alltime.id))  #Random Intercept
anova(m1)
summary(m1)

m2 = lmer(alltime.rkc ~ alltime.semester*timepoint + alltime.esl + alltime.reading + alltime.writing +
            alltime.bioclass + alltime.gender+ alltime.age + alltime.race + alltime.cins.pre +
            alltime.isea.pre + (1|alltime.id))  #Random Intercept
anova(m2)
summary(m2)

#Animal NI#
m1 = lmer(alltime.snaive ~ alltime.semester*timepoint + (1|alltime.id))  #Random Intercept
anova(m1)
summary(m1)

m2 = lmer(alltime.snaive ~ alltime.semester*timepoint + alltime.esl + alltime.reading + 
            alltime.writing + alltime.bioclass + alltime.gender+ alltime.age + alltime.race +
            alltime.cins.pre + alltime.isea.pre + (1|alltime.id))  #Random Intercept
anova(m2)
summary(m2)

#Plant NI#
m1 = lmer(alltime.rnaive ~ alltime.semester*timepoint + (1|alltime.id))  #Random Intercept
anova(m1)
summary(m1)

m2 = lmer(alltime.rnaive ~ alltime.semester*timepoint + alltime.esl + alltime.reading + 
            alltime.writing + alltime.bioclass + alltime.gender+ alltime.age + alltime.race +
            alltime.cins.pre + alltime.isea.pre + (1|alltime.id))  #Random Intercept
anova(m2)
summary(m2)




#All times and Animal/Plant combined    ######WHEN YOU HAVE TIME, REWRITE. RANDOM SLOPE AND INTERCEPT (TIME|ID)
#Key Concepts
s=alltime.allsemester
alltime.allsemester=relevel(alltime.allsemester,ref="a")
all.timepoint=relevel(all.timepoint,1)
all.timepoint=ordered(all.timepoint)
alltime.allsemester=ordered(alltime.allsemester)   #ONLY RUN THIS STUFF FOR QUAD CUBIC QUARTIC POLYNOMIALS
g=alltime.allgender
e=alltime.allesl
alltime.allgender[alltime.allgender==3]=NA #Run to get rid of option 3 for Gender.  Put it back after
alltime.allesl[alltime.allesl==3]=NA       #Run to get rid of option 3 for ESL.   Put it back after



m.kc.1 = lmer(alltime.allkc ~ alltime.allsemester*all.timepoint + (1|alltime.allid))
anova(m.kc.1)
summary(m.kc.1)

m.kc.2 = lmer(alltime.allkc ~ alltime.allsemester*all.timepoint + alltime.type + alltime.allesl +
                alltime.allreading + alltime.allwriting + alltime.allbioclass + alltime.allgender +
                alltime.allage + alltime.allrace + alltime.alllevel + alltime.allplan +
                alltime.allcins.pre + alltime.allisea.pre + 
                (1|alltime.allid))  #Random Intercept for ID *****USE THIS*****
anova(m.kc.2)
summary(m.kc.2)

m.kc.3 = lmer(alltime.allkc ~ alltime.allsemester*all.timepoint + alltime.type + alltime.allesl +
                alltime.allreading + alltime.allwriting + alltime.allbioclass + alltime.allgender +
                alltime.allage + alltime.allrace + alltime.alllevel + alltime.allplan +
                alltime.allcins.pre + alltime.allisea.pre + 
                (1|alltime.type/alltime.allid))  #Random Intercept for Type, and ID within type
anova(m.kc.3)                     ###With type in random, should I remove from fixed?
summary(m.kc.3)

m.kc.4 = lmer(alltime.allkc ~ alltime.allsemester*all.timepoint + alltime.type + alltime.allesl +
                alltime.allreading + alltime.allwriting + alltime.allbioclass + alltime.allgender +
                alltime.allage + alltime.allrace + alltime.alllevel + alltime.allplan +
                alltime.allcins.pre + alltime.allisea.pre + 
                (1|alltime.allid/alltime.type))  #Random Intercept for ID, and Type within ID
anova(m.kc.4)
summary(m.kc.4)

m.kc.5 = lmer(alltime.allkc ~ alltime.allsemester*all.timepoint + alltime.type + alltime.allesl +
                alltime.allreading + alltime.allwriting + alltime.allbioclass + alltime.allgender +
                alltime.allage + alltime.allrace + alltime.alllevel + alltime.allplan +
                alltime.allcins.pre + alltime.allisea.pre + 
                (alltime.type|alltime.allid))  #Random Intercept for ID and random Type slope for ID
anova(m.kc.5)
summary(m.kc.5)

m.kc.6 = lmer(alltime.allkc ~ alltime.allsemester*all.timepoint + alltime.type + alltime.allesl +
                alltime.allreading + alltime.allwriting + alltime.allbioclass + alltime.allgender +
                alltime.allage + alltime.allrace + alltime.alllevel + alltime.allplan +
                alltime.allcins.pre + alltime.allisea.pre + 
                (all.timepoint|alltime.allid))  #Random Intercept and slope model used in Thesis********
anova(m.kc.6)
summary(m.kc.6)



#Naive Ideas
m.ni.2 = lmer(alltime.allnaive ~ alltime.allsemester*all.timepoint + alltime.type + alltime.allesl +
                alltime.allreading + alltime.allwriting + alltime.allbioclass + alltime.allgender +
                alltime.allage + alltime.allrace + alltime.alllevel + alltime.allplan +
                alltime.allcins.pre + alltime.allisea.pre + 
                (1|alltime.allid))  #Random Intercept for ID *****USE THIS*****
anova(m.ni.2)
summary(m.ni.2)

m.ni.6 = lmer(alltime.allnaive ~ alltime.allsemester*all.timepoint + alltime.type + alltime.allesl +
                alltime.allreading + alltime.allwriting + alltime.allbioclass + alltime.allgender +
                alltime.allage + alltime.allrace + alltime.alllevel + alltime.allplan +
                alltime.allcins.pre + alltime.allisea.pre + 
                (all.timepoint|alltime.allid))  #Random Intercept and slope model used in Thesis********
anova(m.ni.6)
summary(m.ni.6)


#Scientific Model Type   #Both A and P in it  #NOT WORKING FOR SOME REASON
m.sci.2 = glmer(alltime.allmodel.sci ~ alltime.allsemester*all.timepoint + alltime.type + alltime.allesl +
                alltime.allreading + alltime.allwriting + alltime.allbioclass + alltime.allgender +
                alltime.allage + alltime.allrace + alltime.alllevel + alltime.allplan +
                alltime.allcins.pre + alltime.allisea.pre + 
                (1|alltime.allid),             #Random Intercept for ID *****USE THIS*****
                family=binomial(link='logit'),
                control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=100000)))
anova(m.sci.2)
summary(m.sci.2)


#Scientific Model Consistency
#Problem converging.  First do regular glm model.  then use those coefs to initialize glmer
#m.sci.2 is the best one
m.sci = glm(alltime.tmodel.sci ~ alltime.semester*timepoint + alltime.esl +
                  alltime.reading + alltime.writing + alltime.bioclass + alltime.gender +
                  alltime.age + alltime.race + alltime.level + alltime.plan +
                  alltime.cins.pre + alltime.isea.pre, family=binomial(link='logit'))  
anova(m.sci)
summary(m.sci)

m.sci.2 = glmer(alltime.tmodel.sci ~ alltime.semester*timepoint + alltime.esl +
                  alltime.reading + alltime.writing + alltime.bioclass + alltime.gender +
                  alltime.age + alltime.race + alltime.level + alltime.plan +
                  alltime.cins.pre + alltime.isea.pre + 
                  (1|alltime.id),             #Random Intercept for ID *****USE THIS*****
                start = list(coef(m.sci)),
                family=binomial(link='logit'))
                #control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=100000)))  
anova(m.sci.2)
summary(m.sci.2)

m.sci.3 = glmer(alltime.tmodel.sci ~ alltime.semester*timepoint + alltime.esl +
                  alltime.reading + alltime.writing + alltime.bioclass + alltime.gender +
                  alltime.age + alltime.race + alltime.level + alltime.plan +
                  alltime.cins.pre + alltime.isea.pre + 
                  (1|alltime.id),             #Random Intercept for ID *****USE THIS*****
                family=binomial(link='logit'))
#control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=100000)))  
anova(m.sci.3)
summary(m.sci.3)

m.sci.4 = glmer(alltime.tmodel.sci ~ alltime.semester*timepoint + alltime.esl +
                  alltime.reading + alltime.writing + alltime.bioclass + alltime.gender +
                  alltime.age + alltime.race + alltime.level + alltime.plan +
                  alltime.cins.pre + alltime.isea.pre + 
                  (timepoint|alltime.id),             #Random Slope and Intercept for ID *****NOT WORKING*****
                family=binomial(link='logit'))
#control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=100000)))  
anova(m.sci.4)
summary(m.sci.4)


alltime.semester=relevel(alltime.semester,ref="c")


#Testing glmer
y=factor(rbinom(100,size=1,prob=0.2))
x=y
x[c(1,5,10,24,55,88)]=0
x[c(8,23,43,66,92,98)]=1
table(x,y)
summary(glm(y~x,family=binomial(link='logit')))
z=factor(rep(1:50,2))
m1=glmer(y~x+(1|z),family=binomial(link='logit'))
summary(m1)
ranef(m1)


####TESTING SOMETHING OUT####
#Random intercept for subject, and random time-slope for each subject
m.kc = lmer(alltime.allkc ~ alltime.allsemester*all.timepoint + alltime.type + alltime.allesl +
                alltime.allreading + alltime.allwriting + alltime.allbioclass + alltime.allgender +
                alltime.allage + alltime.allrace + alltime.alllevel + alltime.allplan +
                alltime.allcins.pre + alltime.allisea.pre + 
                (1+all.timepoint|alltime.allid))  #Random Intercept for ID
anova(m.kc)
summary(m.kc)







######PLOTS USING COMBINED ANIMAL/PLANT KC AND NI######  
##MUST RUN "4 GROUPS ANALYSIS FIXED S17 CONSENT" FIRST##

akc.pre=c(skc.pre,rkc.pre)
akc.ex1=c(skc.ex1,rkc.ex1)
akc.post=c(skc.post,rkc.post)
anaive.pre=c(snaive.pre,rnaive.pre)
anaive.ex1=c(snaive.ex1,rnaive.ex1)
anaive.post=c(snaive.post,rnaive.post)

sn0=c(length(na.omit(skc.spring15.pre)),length(na.omit(skc.spring16.pre)),
      length(na.omit(skc.spring17.pre)),length(na.omit(skc.fall17.pre)))
sn1=c(length(na.omit(skc.spring15.ex1)),length(na.omit(skc.spring16.ex1)),
      length(na.omit(skc.spring17.ex1)),length(na.omit(skc.fall17.ex1)))
sn2=c(length(na.omit(skc.spring15.post)),length(na.omit(skc.spring16.post)),
      length(na.omit(skc.spring17.post)),length(na.omit(skc.fall17.post)))
sn=c(sn0,sn1,sn2)
rn0=c(length(na.omit(rkc.spring15.pre)),length(na.omit(rkc.spring16.pre)),
      length(na.omit(rkc.spring17.pre)),length(na.omit(rkc.fall17.pre)))
rn1=c(length(na.omit(rkc.spring15.ex1)),length(na.omit(rkc.spring16.ex1)),
      length(na.omit(rkc.spring17.ex1)),length(na.omit(rkc.fall17.ex1)))
rn2=c(length(na.omit(rkc.spring15.post)),length(na.omit(rkc.spring16.post)),
      length(na.omit(rkc.spring17.post)),length(na.omit(rkc.fall17.post)))
rn=c(rn0,rn1,rn2)
tn0=c(length(na.omit(tmodel.sci0[sem=="a"])),length(na.omit(tmodel.sci0[sem=="b"])),
      length(na.omit(tmodel.sci0[sem=="c"])),length(na.omit(tmodel.sci0[sem=="d"])))
tn1=c(length(na.omit(tmodel.sci1[sem=="a"])),length(na.omit(tmodel.sci1[sem=="b"])),
      length(na.omit(tmodel.sci1[sem=="c"])),length(na.omit(tmodel.sci1[sem=="d"])))
tn2=c(length(na.omit(tmodel.sci2[sem=="a"])),length(na.omit(tmodel.sci2[sem=="b"])),
      length(na.omit(tmodel.sci2[sem=="c"])),length(na.omit(tmodel.sci2[sem=="d"])))
tn=c(tn0,tn1,tn2)
an=sn+rn
sem=c(sem,sem) ##Dont run this for MC, only for KC NI



#KEY CONCEPTS
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(tapply(akc.pre, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(akc.ex1, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(akc.post, sem, mean, na.rm=TRUE))),
                 se=c(as.numeric(tapply(akc.pre, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(akc.ex1, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(akc.post, sem, sd, na.rm=TRUE))),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.1=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.5) +
  geom_errorbar(aes(ymin=val-1.96*se/sqrt(an),ymax=val+1.96*se/sqrt(an),colour=Treatment),
                size=1,width=.1) + 
  xlab("") + ylab("Number of KC")  + ylim(0.8,3.0) +
  ggtitle("Key Concepts") +
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20),
        legend.text=element_text(size=18),plot.title = element_text(size=25,hjust = 0.5),
        legend.title=element_text(size=18),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))


#NAIVE IDEAS
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(tapply(anaive.pre, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(anaive.ex1, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(anaive.post, sem, mean, na.rm=TRUE))),
                 se=c(as.numeric(tapply(anaive.pre, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(anaive.ex1, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(anaive.post, sem, sd, na.rm=TRUE))),
                 Treatment=c("CG","EG1","EG2","EG3"))
p2.1=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.5) +
  geom_errorbar(aes(ymin=val-1.96*se/sqrt(an),ymax=val+1.96*se/sqrt(an),colour=Treatment), size=1,width=.1) + 
  xlab("") + ylab("Number of NI")  + ggtitle("Naive Ideas") +
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20),
        legend.text=element_text(size=18),plot.title = element_text(size=25,hjust = 0.5),
        legend.title=element_text(size=18),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))


#Model Consistency#
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(tapply(tmodel.sci0, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(tmodel.sci1, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(tmodel.sci2, sem, mean, na.rm=TRUE))),
                 se=c(as.numeric(tapply(tmodel.sci0, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(tmodel.sci1, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(tmodel.sci2, sem, sd, na.rm=TRUE))),
                 Treatment=c("CG","EG1","EG2","EG3"))
p9.1=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  geom_errorbar(aes(ymin=val-1.96*se/sqrt(tn),ymax=val+1.96*se/sqrt(tn),colour=Treatment),size=1,width=.1) + 
  xlab("") + ylab("Proportion")  + ggtitle("Scientific Model Consistency") +
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20),
        legend.text=element_text(size=18),plot.title = element_text(size=25,hjust = 0.5),
        legend.title=element_text(size=18),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))
