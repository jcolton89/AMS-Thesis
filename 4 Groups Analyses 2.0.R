spring15 <- read.csv("~/Jesse's Documents/Documents/Education/Data/Spring2015AllJesse5.0.csv",
                     header=FALSE, stringsAsFactors=FALSE)

spring16 <- read.csv("~/Jesse's Documents/Documents/Education/Data/Spring2016AllJesse8.0.csv",
                     header=FALSE, stringsAsFactors=FALSE)

spring17 <- read.csv("~/Jesse's Documents/Documents/Education/Data/Spring2017AllJesse4.consent.csv", header=FALSE)
spring17=spring17[spring17[,312]=="I consent.",]


fall17 <- read.csv("~/Jesse's Documents/Documents/Education/Data/Spring2018AllJesse3.0.csv", header=FALSE)
#####THIS ONE IS REALLY SPRING18#### did this to not recode all data!!!####



###Create Spring15 variables
#Pre
skc.spring15.pre=spring15[,11]
snaive.spring15.pre=spring15[,12]
smodel.spring15.pre=spring15[,13]
rkc.spring15.pre=spring15[,23]
rnaive.spring15.pre=spring15[,24]
rmodel.spring15.pre=spring15[,25]
cins.spring15.pre=spring15[,46]
percept.spring15.pre=spring15[,57]
#Take Short version of ISEA
isea.spring15.pre.long=spring15[,82]
isea.spring15.pre.short=spring15[,c(61,62,64,65,66,69,70,72,74,77,78,80)]
isea.spring15.pre=rowSums(isea.spring15.pre.short)
cor.test(isea.spring15.pre,isea.spring15.pre.long) ##Very Correlated!! Short can be used!! p<e-16
#Exam1 ACORNS
skc.spring15.ex1=spring15[,111]
snaive.spring15.ex1=spring15[,112]
smodel.spring15.ex1=spring15[,113]
rkc.spring15.ex1=spring15[,99]
rnaive.spring15.ex1=spring15[,100]
rmodel.spring15.ex1=spring15[,101]
#Exam2 ACORNS
skc.spring15.ex2=spring15[,135]
snaive.spring15.ex2=spring15[,136]
smodel.spring15.ex2=spring15[,137]
rkc.spring15.ex2=spring15[,123]
rnaive.spring15.ex2=spring15[,124]
rmodel.spring15.ex2=spring15[,125]
#Acorns Final only Orchid
rkc.spring15.final=spring15[,147]
rnaive.spring15.final=spring15[,148]
rmodel.spring15.final=spring15[,149]
#Post
skc.spring15.post=spring15[,159]
snaive.spring15.post=spring15[,160]
smodel.spring15.post=spring15[,161]
#rkc.spring15.post=spring15[,171]
#rnaive.spring15.post=spring15[,172]
#rmodel.spring15.post=spring15[,173]
rkc.spring15.post=rkc.spring15.final
rnaive.spring15.post=rnaive.spring15.final
rmodel.spring15.post=rmodel.spring15.final
cins.spring15.post=spring15[,194]
percept.spring15.post=spring15[,205]
#Take Short version of ISEA
isea.spring15.post.long=spring15[,230]
isea.spring15.post.short=spring15[,c(209,210,212,213,214,217,218,220,222,225,226,228)]
isea.spring15.post=rowSums(isea.spring15.post.short)
cor.test(isea.spring15.post,isea.spring15.post.long)  ##Very Correlated!! Short can be used!! p<e-16
#Demographic
esl.spring15=spring15[,83]
reading.spring15=spring15[,84]
writing.spring15=spring15[,85]
bio.spring15=spring15[,86]
gender.spring15=spring15[,87]
age.spring15=spring15[,88]
race.spring15=spring15[,89]
level.spring15=spring15[,237]
plan.spring15=spring15[,238]
##Are Final and Post Rose the same?  No!
#t.test(rkc.spring15.final,rkc.spring15.post,paired=TRUE)  #p=e-10
#t.test(rnaive.spring15.final,rnaive.spring15.post,paired=TRUE) #p=.03
#t.test(rmodel.spring15.final,rmodel.spring15.post,paired=TRUE) #p=e-06




###Create Spring16 variables
#Pre
skc.spring16.pre=spring16[,11]
snaive.spring16.pre=spring16[,12]
smodel.spring16.pre=spring16[,13]
rkc.spring16.pre=spring16[,23]
rnaive.spring16.pre=spring16[,24]
rmodel.spring16.pre=spring16[,25]
cins.spring16.pre=spring16[,46]
percept.spring16.pre=spring16[,57]
#Take Short version of ISEA
isea.spring16.pre.long=spring16[,82]
isea.spring16.pre.short=spring16[,c(61,62,64,65,66,69,70,72,74,77,78,80)]
isea.spring16.pre=rowSums(isea.spring16.pre.short)
cor.test(isea.spring16.pre,isea.spring16.pre.long)  #Correlation Strong, Short can be used
#Exam1 ACORNS
skc.spring16.ex1=spring16[,111]
snaive.spring16.ex1=spring16[,112]
smodel.spring16.ex1=spring16[,113]
rkc.spring16.ex1=spring16[,99]
rnaive.spring16.ex1=spring16[,100]
rmodel.spring16.ex1=spring16[,101]
#Exam2 ACORNS
skc.spring16.ex2=spring16[,135]
snaive.spring16.ex2=spring16[,136]
smodel.spring16.ex2=spring16[,137]
rkc.spring16.ex2=spring16[,123]
rnaive.spring16.ex2=spring16[,124]
rmodel.spring16.ex2=spring16[,125]
#Post   ###ACORNS TAKEN FROM FINAL EXAM, NO POST TEST ACORNS###
orchidkc=spring16[,147]
orchidnaive=spring16[,148]
orchidmodel=spring16[,149]
lillykc=spring16[,159]
lillynaive=spring16[,160]
lillymodel=spring16[,161]
animalgainkc=spring16[,171]
animalgainnaive=spring16[,172]
animalgainmodel=spring16[,173]
animallosskc=spring16[,183]
animallossnaive=spring16[,184]
animallossmodel=spring16[,185]
which(is.na(orchidkc)==FALSE&is.na(lillykc)==FALSE)
which(is.na(animalgainkc)==FALSE&is.na(animallosskc)==FALSE)
skc.spring16.post=pmin(animalgainkc,animallosskc,na.rm=TRUE)
snaive.spring16.post=pmin(animalgainnaive,animallossnaive,na.rm=TRUE)
smodel.spring16.post=pmin(animalgainmodel,animallossmodel,na.rm=TRUE)
rkc.spring16.post=pmin(orchidkc,lillykc,na.rm=TRUE)
rnaive.spring16.post=pmin(orchidnaive,lillynaive,na.rm=TRUE)
rmodel.spring16.post=pmin(orchidmodel,lillymodel,na.rm=TRUE)
cins.spring16.post=spring16[,206]
percept.spring16.post=spring16[,217]
#Take Short version of ISEA
isea.spring16.post.long=spring16[,242]
isea.spring16.post.short=spring16[,c(221,222,224,225,226,229,230,232,234,237,238,240)]
isea.spring16.post=rowSums(isea.spring16.post.short)
cor.test(isea.spring16.post,isea.spring16.post.long)  ##Very Correlated!! Short can be used!! p<e-16
#Demographic
esl.spring16=spring16[,83]
reading.spring16=spring16[,84]
writing.spring16=spring16[,85]
bio.spring16=spring16[,86]
gender.spring16=spring16[,87]
age.spring16=spring16[,88]
race.spring16=spring16[,89]
rank.spring16=spring16[,249]
level.spring16=spring16[,250]
plan.spring16=spring16[,251]
#race.spring16=replace(race.spring16,race.spring16==4|race.spring16==6,7)
#race.spring16[race.spring16==0]=NA
##Are Final Orchid and Lille Petal the same?  Yes!
t.test(orchidkc,lillykc,paired=FALSE)  #p=0.68
t.test(orchidnaive,lillynaive,paired=FALSE) #p=0.21
t.test(orchidmodel,lillymodel,paired=FALSE) #p=0.08




###Create Spring17 variables
#Pre
skc.spring17.pre=spring17[,11]
snaive.spring17.pre=spring17[,12]
smodel.spring17.pre=spring17[,13]
rkc.spring17.pre=spring17[,23]
rnaive.spring17.pre=spring17[,24]
rmodel.spring17.pre=spring17[,25]
cins.spring17.pre=spring17[,46]
cans.spring17.pre=spring17[,71]
##percept.spring17.pre=spring17[,57]  Does not Exist
#Take Short version of ISEA
isea.spring17.pre.long=spring17[,96]
isea.spring17.pre.short=spring17[,c(75,76,78,79,80,83,84,86,88,91,92,94)]
isea.spring17.pre=rowSums(isea.spring17.pre.short)
cor.test(isea.spring17.pre,isea.spring17.pre.long) ##Very Correlated!! Short can be used!! p<e-16
#Exam1 ACORNS
skc.spring17.ex1=spring17[,142]
snaive.spring17.ex1=spring17[,143]
smodel.spring17.ex1=spring17[,144]
rkc.spring17.ex1=spring17[,130]
rnaive.spring17.ex1=spring17[,131]
rmodel.spring17.ex1=spring17[,132]
##Exam2 ACORNS
skc.spring17.ex2=spring17[,166]
snaive.spring17.ex2=spring17[,167]
smodel.spring17.ex2=spring17[,168]
rkc.spring17.ex2=spring17[,154]
rnaive.spring17.ex2=spring17[,155]
rmodel.spring17.ex2=spring17[,156]
##Final    ########WORK ON THIS########
spring17[,182][which(spring17[,182]=="strepsirrhini tapetum lucidum loss")]="Strepsirrhini tapetum lucidum loss"
animalgainkc1.spring17=ifelse(spring17[,169]=="Strepsirrhini tapetum lucidum gain",
                              spring17[,179],NA)
animalgainkc2.spring17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum gain",
                              spring17[,192],NA)
animalgainkc.spring17=pmin(animalgainkc1.spring17,animalgainkc2.spring17,na.rm=TRUE)
animalgainnaive1.spring17=ifelse(spring17[,169]=="Strepsirrhini tapetum lucidum gain",
                                 spring17[,180],NA)
animalgainnaive2.spring17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum gain",
                                 spring17[,193],NA)
animalgainnaive.spring17=pmin(animalgainnaive1.spring17,animalgainnaive2.spring17,na.rm=TRUE)
animalgainmodel1.spring17=ifelse(spring17[,169]=="Strepsirrhini tapetum lucidum gain",
                                 spring17[,181],NA)
animalgainmodel2.spring17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum gain",
                                 spring17[,194],NA)
animalgainmodel.spring17=pmin(animalgainmodel1.spring17,animalgainmodel2.spring17,na.rm=TRUE)

animallosskc.spring17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum loss",
                             spring17[,192],NA)
animallossnaive.spring17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum loss",
                                spring17[,193],NA)
animallossmodel.spring17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum loss",
                                spring17[,194],NA)

lillykc.spring17=ifelse(spring17[,169]=="Lily petal loss",
                        spring17[,179],NA)
lillynaive.spring17=ifelse(spring17[,169]=="Lily petal loss",
                           spring17[,180],NA)
lillymodel.spring17=ifelse(spring17[,169]=="Lily petal loss",
                           spring17[,181],NA)

orchidkc1.spring17=ifelse(spring17[,169]=="Orchid leaf loss",
                          spring17[,179],NA)
orchidkc2.spring17=ifelse(spring17[,182]=="Orchid leaf loss",
                          spring17[,192],NA)
orchidkc.spring17=pmin(orchidkc1.spring17,orchidkc2.spring17,na.rm=TRUE)
orchidnaive1.spring17=ifelse(spring17[,169]=="Orchid leaf loss",
                             spring17[,180],NA)
orchidnaive2.spring17=ifelse(spring17[,182]=="Orchid leaf loss",
                             spring17[,193],NA)
orchidnaive.spring17=pmin(orchidnaive1.spring17,orchidnaive2.spring17,na.rm=TRUE)
orchidmodel1.spring17=ifelse(spring17[,169]=="Orchid leaf loss",
                             spring17[,181],NA)
orchidmodel2.spring17=ifelse(spring17[,182]=="Orchid leaf loss",
                             spring17[,194],NA)
orchidmodel.spring17=pmin(orchidmodel1.spring17,orchidmodel2.spring17,na.rm=TRUE)

skc.spring17.final=pmin(animalgainkc.spring17,animallosskc.spring17,na.rm=TRUE)
snaive.spring17.final=pmin(animalgainnaive.spring17,animallossnaive.spring17,na.rm=TRUE)
smodel.spring17.final=pmin(animalgainmodel.spring17,animallossmodel.spring17,na.rm=TRUE)
rkc.spring17.final=pmin(orchidkc.spring17,lillykc.spring17,na.rm=TRUE)
rnaive.spring17.final=pmin(orchidnaive.spring17,lillynaive.spring17,na.rm=TRUE)
rmodel.spring17.final=pmin(orchidmodel.spring17,lillymodel.spring17,na.rm=TRUE)
##Post
#skc.spring17.post=spring17[,204]
#snaive.spring17.post=spring17[,205]
#smodel.spring17.post=spring17[,206]
#rkc.spring17.post=spring17[,216]
#rnaive.spring17.post=spring17[,217]
#rmodel.spring17.post=spring17[,218]
skc.spring17.post=skc.spring17.final
snaive.spring17.post=snaive.spring17.final
smodel.spring17.post=smodel.spring17.final
rkc.spring17.post=rkc.spring17.final
rnaive.spring17.post=rnaive.spring17.final
rmodel.spring17.post=rmodel.spring17.final
cins.spring17.post=spring17[,239]
cans.spring17.post=spring17[,264]
#percept.spring17.post=spring17[,241]   #Does not exist
##Take Short version of ISEA
isea.spring17.post.long=spring17[,289]
isea.spring17.post.short=spring17[,c(268,269,271,272,273,276,277,279,281,284,285,287)]
isea.spring17.post=rowSums(isea.spring17.post.short)
cor.test(isea.spring17.post,isea.spring17.post.long)  ##Very Correlated!! Short can be used!! p<e-16
##Demographic
esl.spring17=spring17[,111]
reading.spring17=spring17[,112]
writing.spring17=spring17[,113]
bio.spring17=spring17[,114]
gender.spring17=spring17[,115]
age.spring17=spring17[,116]
race.spring17=spring17[,117]
level.spring17=spring17[,119]
plan.spring17=spring17[,120]
wordcount.spring17.spre=spring17[,304]
wordcount.spring17.rpre=spring17[,305]
wordcount.spring17.sex1=spring17[,306]
wordcount.spring17.rex1=spring17[,307]
wordcount.spring17.spost=spring17[,308]
wordcount.spring17.rpost=spring17[,309]



###Create Spring18 variables      ####Just to make things easier, I will name it Fall17####
#Pre
skc.fall17.pre=fall17[,15]
snaive.fall17.pre=fall17[,16]
smodel.fall17.pre=fall17[,17]
rkc.fall17.pre=fall17[,27]
rnaive.fall17.pre=fall17[,28]
rmodel.fall17.pre=fall17[,29]
cins.fall17.pre=rowSums(fall17[,30:49])
cans.fall17.pre=rowSums(fall17[,50:73])
#Take Short version of ISEA
isea.fall17.pre.long=rowSums(fall17[,74:97])
isea.fall17.pre.short=fall17[,c(77,78,80,81,82,85,86,88,90,93,94,96)]
isea.fall17.pre=rowSums(isea.fall17.pre.short)
cor.test(isea.fall17.pre,isea.fall17.pre.long)  #Correlation Strong, Short can be used
#Exam1 ACORNS
skc.fall17.ex1=fall17[,126]
snaive.fall17.ex1=fall17[,127]
smodel.fall17.ex1=fall17[,128]
rkc.fall17.ex1=fall17[,114]
rnaive.fall17.ex1=fall17[,115]
rmodel.fall17.ex1=fall17[,116]
#Final/Post
fall17[,143][fall17[,143]=="strepsirrhini tapetum lucidum gain"]="strepsirrhini tapetum lucidum loss"
animalgainkc.fall17=ifelse(fall17[,129]=="strepsirrhini tapetum lucidum gain",
                           fall17[,139],NA)
animalgainnaive.fall17=ifelse(fall17[,129]=="strepsirrhini tapetum lucidum gain",
                              fall17[,140],NA)
animalgainmodel.fall17=ifelse(fall17[,129]=="strepsirrhini tapetum lucidum gain",
                              fall17[,141],NA)
animallosskc.fall17=ifelse(fall17[,143]=="strepsirrhini tapetum lucidum loss",
                           fall17[,153],NA)
animallossnaive.fall17=ifelse(fall17[,143]=="strepsirrhini tapetum lucidum loss",
                              fall17[,154],NA)
animallossmodel.fall17=ifelse(fall17[,143]=="strepsirrhini tapetum lucidum loss",
                              fall17[,155],NA)
lillykc.fall17=ifelse(fall17[,129]=="lily petal loss",fall17[,139],NA)
lillynaive.fall17=ifelse(fall17[,129]=="lily petal loss",fall17[,140],NA)
lillymodel.fall17=ifelse(fall17[,129]=="lily petal loss",fall17[,141],NA)
orchidkc.fall17=ifelse(fall17[,143]=="orchid leaf loss",fall17[,153],NA)
orchidnaive.fall17=ifelse(fall17[,143]=="orchid leaf loss",fall17[,154],NA)
orchidmodel.fall17=ifelse(fall17[,143]=="orchid leaf loss",fall17[,155],NA)
skc.fall17.post=pmin(animalgainkc.fall17,animallosskc.fall17,na.rm=TRUE)
snaive.fall17.post=pmin(animalgainnaive.fall17,animallossnaive.fall17,na.rm=TRUE)
smodel.fall17.post=pmin(animalgainmodel.fall17,animallossmodel.fall17,na.rm=TRUE)
rkc.fall17.post=pmin(orchidkc.fall17,lillykc.fall17,na.rm=TRUE)
rnaive.fall17.post=pmin(orchidnaive.fall17,lillynaive.fall17,na.rm=TRUE)
rmodel.fall17.post=pmin(orchidmodel.fall17,lillymodel.fall17,na.rm=TRUE)
which(is.na(orchidkc.fall17)==FALSE&is.na(lillykc.fall17)==FALSE)
which(is.na(animalgainkc.fall17)==FALSE&is.na(animallosskc.fall17)==FALSE)
cins.fall17.post=rowSums(fall17[,181:200])
cans.fall17.post=rowSums(fall17[,201:224])
#Take Short version of ISEA
isea.fall17.post.long=rowSums(fall17[,225:248])
isea.fall17.post.short=fall17[,c(228,229,231,232,233,236,237,239,241,244,245,247)]
isea.fall17.post=rowSums(isea.fall17.post.short)
cor.test(isea.fall17.post,isea.fall17.post.long)  ##Very Correlated!! Short can be used!! p<e-16
#Demographic
esl.fall17=fall17[,98]
reading.fall17=fall17[,99]
writing.fall17=fall17[,100]
bio.fall17=fall17[,101]
gender.fall17=fall17[,102]
age.fall17=fall17[,103]
race.fall17=fall17[,104]
level.fall17=fall17[,2]
plan.fall17=fall17[,3]
section.fall17=fall17[,4]
group.fall17=fall17[,5]
#race.fall17=replace(race.fall17,race.fall17==4|race.fall17==6,7)
#race.fall17[race.fall17==0]=NA
##Are Final Orchid and Lilly Petal the same?  Yes!
##Are Animal Gain and Loss the same? Yes!
t.test(orchidkc.fall17,lillykc.fall17,paired=FALSE)  #p=0.27
t.test(orchidnaive.fall17,lillynaive.fall17,paired=FALSE) #p=0.25
t.test(orchidmodel.fall17,lillymodel.fall17,paired=FALSE) #p=0.21
t.test(animalgainkc.fall17,animallosskc.fall17,paired=FALSE)  #p=0.04
t.test(animalgainnaive.fall17,animallossnaive.fall17,paired=FALSE) #p=0.05
t.test(animalgainmodel.fall17,animallossmodel.fall17,paired=FALSE) #p=0.016








###################################################
##This section will be for individual KC NI stuff##
###################################################






#####Compare the 4 semesters

esl=c(esl.spring15,esl.spring16,esl.spring17,esl.fall17)
esl=factor(esl)
reading=c(reading.spring15,reading.spring16,reading.spring17,reading.fall17)
writing=c(writing.spring15,writing.spring16,writing.spring17,writing.fall17)
bioclass=c(bio.spring15,bio.spring16,bio.spring17,bio.fall17)
bioclass=factor(bioclass)
gender=c(gender.spring15,gender.spring16,gender.spring17,gender.fall17)
gender=factor(gender)
age=c(age.spring15,age.spring16,age.spring17,age.fall17)
race=c(race.spring15,race.spring16,race.spring17,race.fall17)
race=replace(race,race==4|race==6,7) ##Change American Indian & Other Pacific to "Other"
race=replace(race,race==2|race==5,7) ##Change African American & Hispanic to "Other"
race=factor(race)
race=relevel(race,ref="3")
level=c(level.spring15,level.spring16,level.spring17,level.fall17)
level=factor(level)
plan=c(plan.spring15,plan.spring16,plan.spring17,plan.fall17)
plan=factor(plan)

semester=c(rep("a",length(cins.spring15.pre)),rep("b",length(cins.spring16.pre)),
           rep("c",length(cins.spring17.pre)),rep("d",length(cans.fall17.pre)))
sem=semester


###Demographics Comparisons
round(prop.table(table(esl,sem,useNA="always"),2)*100,1)

##ESL
table(esl,semester)
chisq.test(esl,semester)                #p=0.9949   #No Difference
chisq.test(esl[esl!=3],semester[esl!=3])  #p=0.9042 #No Difference

##Reading  (Ordinal, so go with the Kruskal Wallis test)
table(reading,semester)
chisq.test(reading,semester)            #p=e-05
kruskal.test(reading,factor(semester))  #p=0.003   #Sig Difference

##Writing
table(writing,semester)
chisq.test(writing,semester)            #p=e-05
kruskal.test(writing,factor(semester))  #p=0.0006   #Sig Difference

##Bio Class
table(bioclass,semester)
chisq.test(bioclass,semester)           #p=e-15
kruskal.test(bioclass,factor(semester)) #p=0.011    #No Difference

##Gender
table(gender,semester)
chisq.test(gender,semester)             #p=0.1013   #No Difference
chisq.test(gender[gender!=3],semester[gender!=3]) #p=0.4871  #No Diff

##Age
anova(lm(age~semester))                 #p=0.0778   #No Difference
tapply(age,semester,mean,na.rm=TRUE)

##Race
table(race,semester)
chisq.test(race,semester)               #p=0.0385   #No Difference
table(replace(race,race==2|race==5,7),sem,exclude=NULL)

##Level
table(level,semester)
round(prop.table(table(level,semester),2),4)
chisq.test(level,semester)              #p=0.1035    #No Difference
kruskal.test(level,factor(semester))    #p=0.494

##Plan
table(plan,semester)
round(prop.table(table(plan,semester),2),4)
chisq.test(plan,semester)               #p=0.1185  #No Difference
kruskal.test(plan,factor(semester))     #p=0.07263  #No Difference



###Instrument Comparisons at 3 Time Points

##skc
skc.pre=c(skc.spring15.pre,skc.spring16.pre,skc.spring17.pre,skc.fall17.pre)
anova(lm(skc.pre~semester))      #p=0.035   NOT SIGNIFICANTLY DIFFERENT PRE
summary(skc.spring15.pre)
summary(skc.spring16.pre) 
summary(skc.spring17.pre) 
summary(skc.fall17.pre)

skc.ex1=c(skc.spring15.ex1,skc.spring16.ex1,skc.spring17.ex1,skc.fall17.ex1)
anova(lm(skc.ex1~semester))      #p=e-07    SIGNIFICANTLY DIFFERENT POST

skc.post=c(skc.spring15.post,skc.spring16.post,skc.spring17.post,skc.fall17.post)
anova(lm(skc.post~semester))     #p=e-16   SIGNIFICANTLY DIFFERENT DELAYED-POST


##snaive
snaive.pre=c(snaive.spring15.pre,snaive.spring16.pre,snaive.spring17.pre,snaive.fall17.pre)
anova(lm(snaive.pre~semester))      #p=0.1808   NOT SIGNIFICANTLY DIFFERENT PRE
summary(snaive.spring15.pre)
summary(snaive.spring16.pre) 
summary(snaive.spring17.pre) 
summary(snaive.fall17.pre)

snaive.ex1=c(snaive.spring15.ex1,snaive.spring16.ex1,snaive.spring17.ex1,snaive.fall17.ex1)
anova(lm(snaive.ex1~semester))      #p=e-14    SIGNIFICANTLY DIFFERENT POST

snaive.post=c(snaive.spring15.post,snaive.spring16.post,snaive.spring17.post,snaive.fall17.post)
anova(lm(snaive.post~semester))     #p=e-10   SIGNIFICANTLY DIFFERENT DELAYED-POST


##smodel (ANOVA may not be ideal.  Did Chi-Square and Kruskal-Wallis)
smodel.pre=c(smodel.spring15.pre,smodel.spring16.pre,smodel.spring17.pre,smodel.fall17.pre)
anova(lm(smodel.pre~semester))      #p=0.0387   NOT SIGNIFICANTLY DIFFERENT PRE
summary(smodel.spring15.pre)
summary(smodel.spring16.pre) 
summary(smodel.spring17.pre) 
summary(smodel.fall17.pre)

smodel.ex1=c(smodel.spring15.ex1,smodel.spring16.ex1,smodel.spring17.ex1,smodel.fall17.ex1)
anova(lm(smodel.ex1~semester))      #p=e-10    SIGNIFICANTLY DIFFERENT POST

smodel.post=c(smodel.spring15.post,smodel.spring16.post,smodel.spring17.post,smodel.fall17.post)
anova(lm(smodel.post~semester))     #p=e-11   SIGNIFICANTLY DIFFERENT DELAYED-POST
chisq.test(smodel.pre,semester) #p=0.0546
kruskal.test(smodel.pre,factor(semester)) #p=0.02373   PRE NOT DIFFERENT


##rkc
rkc.pre=c(rkc.spring15.pre,rkc.spring16.pre,rkc.spring17.pre,rkc.fall17.pre)
anova(lm(rkc.pre~semester))      #p=0.0623   NOT SIGNIFICANTLY DIFFERENT PRE
summary(rkc.spring15.pre)
summary(rkc.spring16.pre) 
summary(rkc.spring17.pre) 
summary(rkc.fall17.pre)

rkc.ex1=c(rkc.spring15.ex1,rkc.spring16.ex1,rkc.spring17.ex1,rkc.fall17.ex1)
anova(lm(rkc.ex1~semester))      #p=e-16    SIGNIFICANTLY DIFFERENT POST

rkc.post=c(rkc.spring15.post,rkc.spring16.post,rkc.spring17.post,rkc.fall17.post)
anova(lm(rkc.post~semester))     #p=e-16   SIGNIFICANTLY DIFFERENT DELAYED-POST


##rnaive
rnaive.pre=c(rnaive.spring15.pre,rnaive.spring16.pre,rnaive.spring17.pre,rnaive.fall17.pre)
anova(lm(rnaive.pre~semester))      #p=0.6502   NOT SIGNIFICANTLY DIFFERENT PRE
summary(rnaive.spring15.pre)
summary(rnaive.spring16.pre) 
summary(rnaive.spring17.pre) 
summary(rnaive.fall17.pre)

rnaive.ex1=c(rnaive.spring15.ex1,rnaive.spring16.ex1,rnaive.spring17.ex1,rnaive.fall17.ex1)
anova(lm(rnaive.ex1~semester))      #p=e-16    SIGNIFICANTLY DIFFERENT POST

rnaive.post=c(rnaive.spring15.post,rnaive.spring16.post,rnaive.spring17.post,rnaive.fall17.post)
anova(lm(rnaive.post~semester))     #p=e-14   SIGNIFICANTLY DIFFERENT DELAYED-POST


##rmodel (ANOVA may not be ideal.  Did Chi-Square and Kruskal-Wallis)
rmodel.pre=c(rmodel.spring15.pre,rmodel.spring16.pre,rmodel.spring17.pre,rmodel.fall17.pre)
anova(lm(rmodel.pre~semester))      #p=0.1124   NOT DIFFERENT PRE
summary(rmodel.spring15.pre)
summary(rmodel.spring16.pre) 
summary(rmodel.spring17.pre) 
summary(rmodel.fall17.pre)

rmodel.ex1=c(rmodel.spring15.ex1,rmodel.spring16.ex1,rmodel.spring17.ex1,rmodel.fall17.ex1)
anova(lm(rmodel.ex1~semester))      #p=e-16    SIGNIFICANTLY DIFFERENT POST

rmodel.post=c(rmodel.spring15.post,rmodel.spring16.post,rmodel.spring17.post,rmodel.fall17.post)
anova(lm(rmodel.post~semester))     #p=e-13   SIGNIFICANTLY DIFFERENT DELAYED-POST
chisq.test(rmodel.pre,semester) #p=0.1109     NOT DIFFERENT PRE
kruskal.test(rmodel.pre,factor(semester)) #p=0.1406 NOT DIFFERENT PRE


##CINS
cins.pre=c(cins.spring15.pre,cins.spring16.pre,cins.spring17.pre,cins.fall17.pre)
anova(lm(cins.pre~semester))  #p=0.00085  Significantly Different Pre
summary(cins.spring15.pre)
summary(cins.spring16.pre)
summary(cins.spring17.pre)
summary(cins.fall17.pre)
TukeyHSD(aov(lm(cins.pre~semester)))      ##Post-Hoc test since ANOVA was significant

cins.post=c(cins.spring15.post,cins.spring16.post,cins.spring17.post,cins.fall17.post)
anova(lm(cins.post~semester))  #p=e-16   SIGNIFICANTLY DIFFERENT POST


##CANS
cans.pre=c(rep(NA,length(cins.spring15.pre)),rep(NA,length(cins.spring16.pre)),
           cans.spring17.pre,cans.fall17.pre)
anova(lm(cans.pre~semester))  #p=0.9483  No Diff
summary(cans.spring17.pre)
summary(cans.fall17.pre)

cans.post=c(rep(NA,length(cins.spring15.post)),rep(NA,length(cins.spring16.post)),
            cans.spring17.post,cans.fall17.post)
anova(lm(cans.post~semester))  #p=0.384  No Diff


##ISEA
isea.pre=c(isea.spring15.pre,isea.spring16.pre,isea.spring17.pre,isea.fall17.pre)
anova(lm(isea.pre~semester))  #p=0.00028  SIGNIFICANTLY DIFFERENT PRE
summary(isea.spring15.pre)
summary(isea.spring16.pre)
summary(isea.spring17.pre)
summary(isea.fall17.pre)

TukeyHSD(aov(lm(isea.pre~semester)))      ##Post-Hoc test since ANOVA was significant

isea.post=c(isea.spring15.post,isea.spring16.post,isea.spring17.post,isea.fall17.post)
anova(lm(isea.post~semester))  #p=0.0004871 SIGNIFICANTLY DIFFERENT POST


###Model Types - Logistic regressions###
smodel.sci0=replace(smodel.pre,smodel.pre==2|smodel.pre==3|smodel.pre==4,0)
smodel.sci1=replace(smodel.ex1,smodel.ex1==2|smodel.ex1==3|smodel.ex1==4,0)
smodel.sci2=replace(smodel.post,smodel.post==2|smodel.post==3|smodel.post==4,0)
rmodel.sci0=replace(rmodel.pre,rmodel.pre==2|rmodel.pre==3|rmodel.pre==4,0)
rmodel.sci1=replace(rmodel.ex1,rmodel.ex1==2|rmodel.ex1==3|rmodel.ex1==4,0)
rmodel.sci2=replace(rmodel.post,rmodel.post==2|rmodel.post==3|rmodel.post==4,0)
#Total Model 0,1,2#
tmodel0=smodel.sci0+rmodel.sci0
tmodel1=smodel.sci1+rmodel.sci1
tmodel2=smodel.sci2+rmodel.sci2
#Total Model binary#
tmodel.sci0=replace(tmodel0,tmodel0==1,0)
tmodel.sci0=replace(tmodel.sci0,tmodel.sci0==2,1)
tmodel.sci1=replace(tmodel1,tmodel1==1,0)
tmodel.sci1=replace(tmodel.sci1,tmodel.sci1==2,1)
tmodel.sci2=replace(tmodel2,tmodel2==1,0)
tmodel.sci2=replace(tmodel.sci2,tmodel.sci2==2,1)




###Table with all means,sd's,proportions### #I turn proportions to percentages in Excel, not here#

#PRE
x=matrix(0,nrow=23,ncol=4)
x[1,]=tapply(skc.pre,semester,mean,na.rm=T)
x[2,]=tapply(skc.pre,semester,sd,na.rm=T)
x[3,]=tapply(rkc.pre,semester,mean,na.rm=T)
x[4,]=tapply(rkc.pre,semester,sd,na.rm=T)
x[5,]=tapply(snaive.pre,semester,mean,na.rm=T)
x[6,]=tapply(snaive.pre,semester,sd,na.rm=T)
x[7,]=tapply(rnaive.pre,semester,mean,na.rm=T)
x[8,]=tapply(rnaive.pre,semester,sd,na.rm=T)
x[9:12,]=prop.table(table(smodel.pre,semester),2)
x[13:16,]=prop.table(table(rmodel.pre,semester),2)
x[17,]=tapply(tmodel.sci0,semester,mean,na.rm=T)
x[18,]=tapply(cins.pre,semester,mean,na.rm=T)
x[19,]=tapply(cins.pre,semester,sd,na.rm=T)
x[20,]=tapply(cans.pre,semester,mean,na.rm=T)
x[21,]=tapply(cans.pre,semester,sd,na.rm=T)
x[22,]=tapply(isea.pre,semester,mean,na.rm=T)
x[23,]=tapply(isea.pre,semester,sd,na.rm=T)
round(x,2)

#Post
x=matrix(0,nrow=17,ncol=4)
x[1,]=tapply(skc.ex1,semester,mean,na.rm=T)
x[2,]=tapply(skc.ex1,semester,sd,na.rm=T)
x[3,]=tapply(rkc.ex1,semester,mean,na.rm=T)
x[4,]=tapply(rkc.ex1,semester,sd,na.rm=T)
x[5,]=tapply(snaive.ex1,semester,mean,na.rm=T)
x[6,]=tapply(snaive.ex1,semester,sd,na.rm=T)
x[7,]=tapply(rnaive.ex1,semester,mean,na.rm=T)
x[8,]=tapply(rnaive.ex1,semester,sd,na.rm=T)
x[9:12,]=prop.table(table(smodel.ex1,semester),2)
x[13:16,]=prop.table(table(rmodel.ex1,semester),2)
x[17,]=tapply(tmodel.sci1,semester,mean,na.rm=T)
round(x,2)

#Delayed-post
x=matrix(0,nrow=23,ncol=4)
x[1,]=tapply(skc.post,semester,mean,na.rm=T)
x[2,]=tapply(skc.post,semester,sd,na.rm=T)
x[3,]=tapply(rkc.post,semester,mean,na.rm=T)
x[4,]=tapply(rkc.post,semester,sd,na.rm=T)
x[5,]=tapply(snaive.post,semester,mean,na.rm=T)
x[6,]=tapply(snaive.post,semester,sd,na.rm=T)
x[7,]=tapply(rnaive.post,semester,mean,na.rm=T)
x[8,]=tapply(rnaive.post,semester,sd,na.rm=T)
x[9:12,]=prop.table(table(smodel.post,semester),2)
x[13:16,]=prop.table(table(rmodel.post,semester),2)
x[17,]=tapply(tmodel.sci2,semester,mean,na.rm=T)
x[18,]=tapply(cins.post,semester,mean,na.rm=T)
x[19,]=tapply(cins.post,semester,sd,na.rm=T)
x[20,]=tapply(cans.post,semester,mean,na.rm=T)
x[21,]=tapply(cans.post,semester,sd,na.rm=T)
x[22,]=tapply(isea.post,semester,mean,na.rm=T)
x[23,]=tapply(isea.post,semester,sd,na.rm=T)
round(x,2)




##Models to compare Post, Pre as predictor.  Using demographics##
#(More advanced modeling below in document)#
summary(lm(skc.ex1~skc.pre+semester+esl+reading+writing+bioclass+gender+age+race+level+plan))
summary(lm(snaive.ex1~snaive.pre+semester+esl+reading+writing+bioclass+gender+age+race+level+plan))
summary(lm(rkc.ex1~rkc.pre+semester+esl+reading+writing+bioclass+gender+age+race+level+plan))
summary(lm(rnaive.ex1~rnaive.pre+semester+esl+reading+writing+bioclass+gender+age+race+level+plan))
summary(lm(isea.post~isea.pre+semester+esl+reading+writing+bioclass+gender+age+race+level+plan))
summary(glm(smodel.sci1~smodel.sci0+semester+esl+reading+writing+bioclass+gender+age+race+level+plan,
            family=binomial(link='logit')))
summary(glm(rmodel.sci1~rmodel.sci0+semester+esl+reading+writing+bioclass+gender+age+race+level+plan,
            family=binomial(link='logit')))
summary(glm(tmodel.sci1~tmodel.sci0+semester+esl+reading+writing+bioclass+gender+age+race+level+plan,
            family=binomial(link='logit')))

#Run this to change reference group, then redo above regressions
#semester=relevel(factor(semester),ref="c")



#####Plots to compare them.  With error bars#####
#I use skc.pre in this instead of skc0 like in some other documents#

require(ggplot2)
require(gridExtra)
require(grid)

###Error Bars###
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
cn0=c(length(na.omit(cins.spring15.pre)),length(na.omit(cins.spring16.pre)),
      length(na.omit(cins.spring17.pre)),length(na.omit(cins.fall17.pre)))
cn1=c(length(na.omit(cins.spring15.post)),length(na.omit(cins.spring16.post)),
      length(na.omit(cins.spring17.post)),length(na.omit(cins.fall17.post)))
cn=c(cn0,cn1)
isn0=c(length(na.omit(isea.spring15.pre)),length(na.omit(isea.spring16.pre)),
       length(na.omit(isea.spring17.pre)),length(na.omit(isea.fall17.pre)))
isn1=c(length(na.omit(isea.spring15.post)),length(na.omit(isea.spring16.post)),
       length(na.omit(isea.spring17.post)),length(na.omit(isea.fall17.post)))
isn=c(isn0,isn1)
can0=c(NA,NA,length(na.omit(cans.spring17.pre)),length(na.omit(cans.fall17.pre)))
can1=c(NA,NA,length(na.omit(cans.spring17.post)),length(na.omit(cans.fall17.post)))
can=c(can0,can1)
cincan=c(cn0,can0,cn1,can1)
snrn=c(sn0,rn0,sn1,rn1,sn2,rn2)  #For animal/plant combined



#SKC
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(tapply(skc.pre, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(skc.ex1, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(skc.post, sem, mean, na.rm=TRUE))),
                 se=c(as.numeric(tapply(skc.pre, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(skc.ex1, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(skc.post, sem, sd, na.rm=TRUE))),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.1=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  geom_errorbar(aes(ymin=val-1.96*se/sqrt(sn),ymax=val+1.96*se/sqrt(sn),colour=Treatment),
                size=1,width=.1) + 
  xlab("") + ylab("# of KC")  + ylim(0.8,3.0) +
  ggtitle("Animal Key Concepts") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))


#SNAIVE
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(tapply(snaive.pre, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(snaive.ex1, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(snaive.post, sem, mean, na.rm=TRUE))),
                 se=c(as.numeric(tapply(snaive.pre, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(snaive.ex1, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(snaive.post, sem, sd, na.rm=TRUE))),
                 Treatment=c("CG","EG1","EG2","EG3"))
p2.1=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  geom_errorbar(aes(ymin=val-1.96*se/sqrt(sn),ymax=val+1.96*se/sqrt(sn),colour=Treatment), size=1,width=.1) + 
  xlab("") + ylab("# of NI")  + ggtitle("Animal Naive Ideas") + ylim(0,0.55) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))


#RKC
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(tapply(rkc.pre, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(rkc.ex1, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(rkc.post, sem, mean, na.rm=TRUE))),
                 se=c(as.numeric(tapply(rkc.pre, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(rkc.ex1, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(rkc.post, sem, sd, na.rm=TRUE))),
                 Treatment=c("CG","EG1","EG2","EG3"))
p3.1=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  geom_errorbar(aes(ymin=val-1.96*se/sqrt(rn),ymax=val+1.96*se/sqrt(rn),colour=Treatment),
                size=1,width=.1) + 
  xlab("") + ylab("# of KC")  + ylim(0.8,3.0) +
  ggtitle("Plant Key Concepts") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))


#RNAIVE
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(tapply(rnaive.pre, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(rnaive.ex1, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(rnaive.post, sem, mean, na.rm=TRUE))),
                 se=c(as.numeric(tapply(rnaive.pre, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(rnaive.ex1, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(rnaive.post, sem, sd, na.rm=TRUE))),
                 Treatment=c("CG","EG1","EG2","EG3"))
p4.1=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  geom_errorbar(aes(ymin=val-1.96*se/sqrt(rn),ymax=val+1.96*se/sqrt(rn),colour=Treatment),
                size=1,width=.1) + 
  xlab("") + ylab("# of NI")  + ggtitle("Plant Naive Ideas") + ylim(0,0.55) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))


#CINS  THERE IS NO FALL17 CINS#  #I AM USING SPRING18 THOUGH#
df <- data.frame(x=c(rep(0,4),rep(2,4)),
                 val=c(as.numeric(tapply(cins.pre, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(cins.post, sem, mean, na.rm=TRUE))),
                 se=c(as.numeric(tapply(cins.pre, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(cins.post, sem, sd, na.rm=TRUE))),
                 Treatment=c("CG","EG1","EG2","EG3"))
p5.1=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  geom_errorbar(aes(ymin=val-1.96*se/sqrt(cn),ymax=val+1.96*se/sqrt(cn),colour=Treatment),
                size=1,width=.1) + 
  xlab("") + ylab("Score")  + ggtitle("CINS") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,2),
                     labels=c("Pre", "Delayed-Post"))


#CANS
df <- data.frame(x=c(rep(0,4),rep(2,4)),
                 val=c(as.numeric(tapply(cans.pre, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(cans.post, sem, mean, na.rm=TRUE))),
                 se=c(as.numeric(tapply(cans.pre, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(cans.post, sem, sd, na.rm=TRUE))),
                 Treatment=c("CG","EG1","EG2","EG3"))
p6.1=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  geom_errorbar(aes(ymin=val-1.96*se/sqrt(can),ymax=val+1.96*se/sqrt(can),colour=Treatment),
                size=1,width=.1) + 
  xlab("") + ylab("Score")  + ggtitle("CANS") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,2),
                     labels=c("Pre", "Delayed-Post"))


#CINS/CANS Combined
df <- data.frame(x=c(rep(0,8),rep(2,8)),
                 val=c(as.numeric(tapply(cins.pre/20*100, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(cans.pre/24*100, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(cins.post/20*100, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(cans.post/24*100, sem, mean, na.rm=TRUE))),
                 se=c(as.numeric(tapply(cins.pre/20*100, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(cans.pre/24*100, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(cins.post/20*100, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(cans.pre/24*100, sem, sd, na.rm=TRUE))),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("CINS",4),rep("CANS",4)))
p7.1=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  geom_errorbar(aes(ymin=val-1.96*se/sqrt(cincan),ymax=val+1.96*se/sqrt(cincan),colour=Treatment),
                size=1,width=.1) + 
  xlab("") + ylab("Avg Score (%)")  + ggtitle("CINS/CANS") + ylim(26,93) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,2),
                     labels=c("Pre", "Delayed-Post"))


#ISEA
df <- data.frame(x=c(rep(0,4),rep(2,4)),
                 val=c(as.numeric(tapply(isea.pre, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(isea.post, sem, mean, na.rm=TRUE))),
                 se=c(as.numeric(tapply(isea.pre, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(isea.post, sem, sd, na.rm=TRUE))),
                 Treatment=c("CG","EG1","EG2","EG3"))
p8.1=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  geom_errorbar(aes(ymin=val-1.96*se/sqrt(isn),ymax=val+1.96*se/sqrt(isn),colour=Treatment),
                size=1,width=.1) + 
  xlab("") + ylab("Score")  + ggtitle("ISEA") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,2),
                     labels=c("Pre", "Delayed-Post"))


#Model Consistency#
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(tapply(tmodel.sci0*100, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(tmodel.sci1*100, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(tmodel.sci2*100, sem, mean, na.rm=TRUE))),
                 se=c(as.numeric(tapply(tmodel.sci0*100, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(tmodel.sci1*100, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(tmodel.sci2*100, sem, sd, na.rm=TRUE))),
                 Treatment=c("CG","EG1","EG2","EG3"))
p9.1=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  geom_errorbar(aes(ymin=val-1.96*se/sqrt(tn),ymax=val+1.96*se/sqrt(tn),colour=Treatment),size=1,width=.1) + 
  xlab("") + ylab("% of Class")  + ggtitle("Scientific Model Consistency") + ylim(26,93) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))



grid.arrange(p1.1,p3.1,p2.1,p4.1,p5.1,p9.1)
grid.arrange(p1.1,p3.1,p2.1,p4.1,p7.1,p9.1)




#Model Type, Animal/Plant combined#
df <- data.frame(x=c(rep(0,8),rep(1,8),rep(3,8)),
                 val=c(as.numeric(tapply(smodel.sci0, semester, mean, na.rm=TRUE)),
                       as.numeric(tapply(rmodel.sci0, semester, mean, na.rm=TRUE)),
                       as.numeric(tapply(smodel.sci1, semester, mean, na.rm=TRUE)),
                       as.numeric(tapply(rmodel.sci1, semester, mean, na.rm=TRUE)),
                       as.numeric(tapply(smodel.sci2, semester, mean, na.rm=TRUE)),
                       as.numeric(tapply(rmodel.sci2, semester, mean, na.rm=TRUE))),
                 se=c(as.numeric(tapply(smodel.sci0, semester, sd, na.rm=TRUE)),
                      as.numeric(tapply(rmodel.sci0, semester, sd, na.rm=TRUE)),
                      as.numeric(tapply(smodel.sci1, semester, sd, na.rm=TRUE)),
                      as.numeric(tapply(rmodel.sci1, semester, sd, na.rm=TRUE)),
                      as.numeric(tapply(smodel.sci2, semester, sd, na.rm=TRUE)),
                      as.numeric(tapply(rmodel.sci2, semester, sd, na.rm=TRUE))),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",4),rep("Plant",4)))
p10.1=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  geom_errorbar(aes(ymin=val-1.96*se/sqrt(snrn),ymax=val+1.96*se/sqrt(snrn),colour=Treatment),
                size=1,width=.1) + 
  xlab("") + ylab("Proportion")  + ggtitle("Scientific Model") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,2),
                     labels=c("Pre", "Delayed-Post"))




#Key Concepts, Animal/Plant combined#
df <- data.frame(x=c(rep(0,8),rep(1,8),rep(3,8)),
                 val=c(as.numeric(tapply(skc.pre, semester, mean, na.rm=TRUE)),
                       as.numeric(tapply(rkc.pre, semester, mean, na.rm=TRUE)),
                       as.numeric(tapply(skc.ex1, semester, mean, na.rm=TRUE)),
                       as.numeric(tapply(rkc.ex1, semester, mean, na.rm=TRUE)),
                       as.numeric(tapply(skc.post, semester, mean, na.rm=TRUE)),
                       as.numeric(tapply(rkc.post, semester, mean, na.rm=TRUE))),
                 se=c(as.numeric(tapply(skc.pre, semester, sd, na.rm=TRUE)),
                      as.numeric(tapply(rkc.pre, semester, sd, na.rm=TRUE)),
                      as.numeric(tapply(skc.ex1, semester, sd, na.rm=TRUE)),
                      as.numeric(tapply(rkc.ex1, semester, sd, na.rm=TRUE)),
                      as.numeric(tapply(skc.post, semester, sd, na.rm=TRUE)),
                      as.numeric(tapply(rkc.post, semester, sd, na.rm=TRUE))),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",4),rep("Plant",4)))
p11.1=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  geom_errorbar(aes(ymin=val-1.96*se/sqrt(snrn),ymax=val+1.96*se/sqrt(snrn),colour=Treatment),
                size=1,width=.1) + 
  xlab("") + ylab("Key Concepts(Mean)")  + ggtitle("Key Concepts") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,2),
                     labels=c("Pre", "Delayed-Post"))



#Naive Ideas, Animal/Plant combined#
df <- data.frame(x=c(rep(0,8),rep(1,8),rep(3,8)),
                 val=c(as.numeric(tapply(snaive.pre, semester, mean, na.rm=TRUE)),
                       as.numeric(tapply(rnaive.pre, semester, mean, na.rm=TRUE)),
                       as.numeric(tapply(snaive.ex1, semester, mean, na.rm=TRUE)),
                       as.numeric(tapply(rnaive.ex1, semester, mean, na.rm=TRUE)),
                       as.numeric(tapply(snaive.post, semester, mean, na.rm=TRUE)),
                       as.numeric(tapply(rnaive.post, semester, mean, na.rm=TRUE))),
                 se=c(as.numeric(tapply(snaive.pre, semester, sd, na.rm=TRUE)),
                      as.numeric(tapply(rnaive.pre, semester, sd, na.rm=TRUE)),
                      as.numeric(tapply(snaive.ex1, semester, sd, na.rm=TRUE)),
                      as.numeric(tapply(rnaive.ex1, semester, sd, na.rm=TRUE)),
                      as.numeric(tapply(snaive.post, semester, sd, na.rm=TRUE)),
                      as.numeric(tapply(rnaive.post, semester, sd, na.rm=TRUE))),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",4),rep("Plant",4)))
p12.1=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  geom_errorbar(aes(ymin=val-1.96*se/sqrt(snrn),ymax=val+1.96*se/sqrt(snrn),colour=Treatment),
                size=1,width=.1) + 
  xlab("") + ylab("Naive Ideas (Mean)")  + ggtitle("Naive Ideas") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,2),
                     labels=c("Pre", "Delayed-Post"))










#####REGRESSION MODELS IN ORDER OF PAPER#####
#1) Model Consistency
#2) Model Types
#3) Key Concepts
#4) Naive Ideas
#5) Individual KC/NI





####1) Model Consistency
#Pre-test
m0.0=glm(tmodel.sci0~semester,family=binomial(link='logit'))
summary(m0.0)

m0.1=glm(tmodel.sci0~semester+esl+reading+writing+bioclass+gender+age+race+level+plan+isea.pre,
         family=binomial(link='logit'))
summary(m0.1)


#A) Post-test
m1.0=glm(tmodel.sci1~semester,family=binomial(link='logit'))
summary(m1.0)

m1.1=glm(tmodel.sci1~semester+tmodel.sci0+esl+reading+writing+bioclass+gender+age+race+level+plan+isea.pre,
         family=binomial(link='logit'))
summary(m1.1)   ##This is the important one##

m1.1.1=glm(tmodel.sci1~relevel(factor(semester),ref=3)+tmodel.sci0+esl+reading+writing+bioclass+
             gender+age+race+level+plan+isea.pre, family=binomial(link='logit'))
summary(m1.1.1)   ##Relevel to EG2, highest one##


#B) Delayed-post test, with and without post-test as covariate
m2.0=glm(tmodel.sci2~semester,family=binomial(link='logit'))
summary(m2.0)

m2.1=glm(tmodel.sci2~semester+tmodel.sci0+esl+reading+writing+bioclass+gender+age+race+level+plan+
           isea.pre, family=binomial(link='logit'))
summary(m2.1)

m2.2=glm(tmodel.sci2~semester+tmodel.sci0+tmodel.sci1+esl+reading+writing+bioclass+gender+age+race+level+plan+
           isea.pre, family=binomial(link='logit'))
summary(m2.2) ##This is the important one##

m2.2.1=glm(tmodel.sci2~relevel(factor(semester),ref=4)+tmodel.sci0+tmodel.sci1+esl+reading+writing+
             bioclass+gender+age+race+level+plan+ isea.pre, family=binomial(link='logit'))
summary(m2.2.1) ##Relevel to EG2, highest one##


#C) All 3 time points in one model
alltime.tmodel.sci=c(tmodel.sci0,tmodel.sci1,tmodel.sci2)
alltime.semester=rep(semester,3)
alltime.esl=factor(rep(esl,3))
alltime.reading=rep(reading,3)
alltime.writing=rep(writing,3)
alltime.bioclass=factor(rep(bioclass,3))
alltime.gender=factor(rep(gender,3))
alltime.age=rep(age,3)
alltime.race=factor(rep(race,3))
alltime.isea.pre=rep(isea.pre,3)
timepoint=c(rep("pre",length(esl)),rep("post",length(esl)),rep("delpost",length(esl)))
timepoint=factor(timepoint,levels=c("pre","post","delpost"))

m.all=glm(alltime.tmodel.sci~alltime.semester+alltime.esl+alltime.reading+alltime.writing+
            alltime.bioclass+alltime.gender+alltime.age+alltime.race+alltime.isea.pre+
            timepoint*alltime.semester, family=binomial(link='logit'))
summary(m.all)

m.all.1=glm(alltime.tmodel.sci~relevel(factor(alltime.semester),ref=3)+alltime.esl+alltime.reading+
              alltime.writing+alltime.bioclass+alltime.gender+alltime.age+alltime.race+alltime.isea.pre+
              timepoint*relevel(factor(alltime.semester),ref=3), family=binomial(link='logit'))
summary(m.all.1)



####2) Model Type
##A) Animal
#i) Post-test Animal Model Type
m1.1=glm(smodel.sci1~semester+smodel.sci0+esl+reading+writing+bioclass+gender+age+race+level+plan+isea.pre,
         family=binomial(link='logit'))
summary(m1.1)   ##This is the important one##

m1.1.1=glm(smodel.sci1~relevel(factor(semester),ref=2)+smodel.sci0+esl+reading+writing+bioclass+
             gender+age+race+level+plan+isea.pre, family=binomial(link='logit'))
summary(m1.1.1)   ##Relevel to EG2, highest one##

#ii) Delayed-Post-Test Animal Model Type
m2.2=glm(smodel.sci2~semester+smodel.sci0+smodel.sci1+esl+reading+writing+bioclass+gender+age+race+level+plan+
           isea.pre, family=binomial(link='logit'))
summary(m2.2) ##This is the important one##

m2.2.1=glm(smodel.sci2~relevel(factor(semester),ref=3)+smodel.sci0+smodel.sci1+esl+reading+writing+
             bioclass+gender+age+race+level+plan+ isea.pre, family=binomial(link='logit'))
summary(m2.2.1) ##Relevel to EG2, highest one##

##B) Plant
#i) Post-test Plant Model Type
m1.1=glm(rmodel.sci1~semester+rmodel.sci0+esl+reading+writing+bioclass+gender+age+race+level+plan+isea.pre,
         family=binomial(link='logit'))
summary(m1.1)   ##This is the important one##

m1.1.1=glm(rmodel.sci1~relevel(factor(semester),ref=3)+rmodel.sci0+esl+reading+writing+bioclass+
             gender+age+race+level+plan+isea.pre, family=binomial(link='logit'))
summary(m1.1.1)   ##Relevel to EG2, highest one##

#ii) Delayed-Post-Test Plant Model Type
m2.2=glm(rmodel.sci2~semester+rmodel.sci0+rmodel.sci1+esl+reading+writing+bioclass+gender+age+race+level+plan+
           isea.pre, family=binomial(link='logit'))
summary(m2.2) ##This is the important one##

m2.2.1=glm(rmodel.sci2~relevel(factor(semester),ref=3)+rmodel.sci0+rmodel.sci1+esl+reading+writing+
             bioclass+gender+age+race+level+plan+ isea.pre, family=binomial(link='logit'))
summary(m2.2.1) ##Relevel to EG2, highest one##


##C) Animal & Plant Combined
allmodel.sci0=c(smodel.sci0,rmodel.sci0)
allmodel.sci1=c(smodel.sci1,rmodel.sci1)
allmodel.sci2=c(smodel.sci2,rmodel.sci2)
allsemester=c(semester,semester)
allesl=factor(c(esl,esl))
allreading=c(reading,reading)
allwriting=c(writing,writing)
allbioclass=factor(c(bioclass,bioclass))
allgender=factor(c(gender,gender))
allage=c(age,age)
allrace=factor(c(race,race))
allisea.pre=c(isea.pre,isea.pre)
type=c(rep("A",length(esl)),rep("P",length(esl)))
type=relevel(factor(type),ref="P")

#i) Post-test Model Type
m1.1=glm(allmodel.sci1~type+allsemester+allmodel.sci0+allesl+allreading+allwriting+allbioclass+
           allgender+allage+allrace+allisea.pre, family=binomial(link='logit'))
summary(m1.1)   ##This is the important one##

m1.1.1=glm(allmodel.sci1~type+relevel(factor(allsemester),ref=3)+allmodel.sci0+allesl+allreading+
             allwriting+allbioclass+allgender+allage+allrace+allisea.pre, family=binomial(link='logit'))
summary(m1.1.1)   ##Relevel to EG2, highest one##

#ii) Delayed-Post-Test Model Type
m2.2=glm(allmodel.sci2~type+allsemester+allmodel.sci0+allmodel.sci1+allesl+allreading+allwriting+
           allbioclass+allgender+allage+allrace+allisea.pre, family=binomial(link='logit'))
summary(m2.2) ##This is the important one##

m2.2.1=glm(allmodel.sci2~type+relevel(factor(allsemester),ref=3)+allmodel.sci0+allmodel.sci1+allesl+allreading+
             allwriting+allbioclass+allgender+allage+allrace+allisea.pre, family=binomial(link='logit'))
summary(m2.2.1) ##Relevel to EG2, highest one##


##D) Animal & Plant Combined, All 3 Time Points Combined
alltime.allmodel.sci=c(allmodel.sci0,allmodel.sci1,allmodel.sci2)
alltime.allsemester=rep(allsemester,3)
alltime.allesl=factor(rep(allesl,3))
alltime.allreading=rep(allreading,3)
alltime.allwriting=rep(allwriting,3)
alltime.allbioclass=factor(rep(allbioclass,3))
alltime.allgender=factor(rep(allgender,3))
alltime.allage=rep(allage,3)
alltime.allrace=factor(rep(allrace,3))
alltime.allisea.pre=rep(allisea.pre,3)
alltime.type=rep(type,3)
alltime.type=relevel(factor(alltime.type),ref="P")
all.timepoint=c(rep("pre",length(type)),rep("post",length(type)),rep("delpost",length(type)))
all.timepoint=factor(all.timepoint,levels=c("pre","post","delpost"))

m.all=glm(alltime.allmodel.sci~alltime.type+alltime.allsemester+alltime.allesl+
            alltime.allreading+alltime.allwriting+alltime.allbioclass+alltime.allgender+
            alltime.allage+alltime.allrace+alltime.allisea.pre+all.timepoint*alltime.allsemester,
          family=binomial(link='logit'))
summary(m.all)

m.all.1=glm(alltime.allmodel.sci~alltime.type+relevel(factor(alltime.allsemester),ref=3)+
              alltime.allesl+alltime.allreading+alltime.allwriting+alltime.allbioclass+
              alltime.allgender+alltime.allage+alltime.allrace+alltime.allisea.pre+
              all.timepoint*relevel(factor(alltime.allsemester),ref=3), family=binomial(link='logit'))
summary(m.all.1)




####Plots are in "Compare F17"####



####3) Key Concepts
##A) Animal
m1=lm(alltime.skc~alltime.semester+alltime.esl+alltime.reading+alltime.writing+
        alltime.bioclass+alltime.gender+alltime.age+alltime.race+alltime.isea.pre+
        timepoint*alltime.semester)
summary(m1)

m1.1=lm(alltime.skc~relevel(factor(alltime.semester),ref=2)+alltime.esl+alltime.reading+alltime.writing+
          alltime.bioclass+alltime.gender+alltime.age+alltime.race+alltime.isea.pre+
          timepoint*relevel(factor(alltime.semester),ref=2))
summary(m1.1)

##B) Plant
m1=lm(alltime.rkc~alltime.semester+alltime.esl+alltime.reading+alltime.writing+
        alltime.bioclass+alltime.gender+alltime.age+alltime.race+alltime.isea.pre+
        timepoint*alltime.semester)
summary(m1)

m1.1=lm(alltime.rkc~relevel(factor(alltime.semester),ref=2)+alltime.esl+alltime.reading+alltime.writing+
          alltime.bioclass+alltime.gender+alltime.age+alltime.race+alltime.isea.pre+
          timepoint*relevel(factor(alltime.semester),ref=2))
summary(m1.1)

##C) Animal/Plant combined
m1=lm(alltime.allkc~alltime.type+alltime.allsemester+alltime.allesl+
        alltime.allreading+alltime.allwriting+alltime.allbioclass+alltime.allgender+
        alltime.allage+alltime.allrace+alltime.allisea.pre+all.timepoint*alltime.allsemester)
summary(m1)

m1.1=lm(alltime.allkc~alltime.type+relevel(factor(alltime.allsemester),ref=2)+alltime.allesl+
          alltime.allreading+alltime.allwriting+alltime.allbioclass+alltime.allgender+
          alltime.allage+alltime.allrace+alltime.allisea.pre+
          all.timepoint*relevel(factor(alltime.allsemester),ref=2))
summary(m1.1)


###4) Naive ideas
##A) Animal
m1=lm(alltime.snaive~alltime.semester+alltime.esl+alltime.reading+alltime.writing+
        alltime.bioclass+alltime.gender+alltime.age+alltime.race+alltime.isea.pre+
        timepoint*alltime.semester)
summary(m1)

m1.1=lm(alltime.snaive~relevel(factor(alltime.semester),ref=2)+alltime.esl+alltime.reading+alltime.writing+
          alltime.bioclass+alltime.gender+alltime.age+alltime.race+alltime.isea.pre+
          timepoint*relevel(factor(alltime.semester),ref=2))
summary(m1.1)

##B) Plant
m1=lm(alltime.rnaive~alltime.semester+alltime.esl+alltime.reading+alltime.writing+
        alltime.bioclass+alltime.gender+alltime.age+alltime.race+alltime.isea.pre+
        timepoint*alltime.semester)
summary(m1)

m1.1=lm(alltime.rnaive~relevel(factor(alltime.semester),ref=2)+alltime.esl+alltime.reading+alltime.writing+
          alltime.bioclass+alltime.gender+alltime.age+alltime.race+alltime.isea.pre+
          timepoint*relevel(factor(alltime.semester),ref=2))
summary(m1.1)

##C) Animal/Plant Combined
m1=lm(alltime.allnaive~alltime.type+alltime.allsemester+alltime.allesl+
        alltime.allreading+alltime.allwriting+alltime.allbioclass+alltime.allgender+
        alltime.allage+alltime.allrace+alltime.allisea.pre+all.timepoint*alltime.allsemester)
summary(m1)

m1.1=lm(alltime.allnaive~alltime.type+relevel(factor(alltime.allsemester),ref=2)+alltime.allesl+
          alltime.allreading+alltime.allwriting+alltime.allbioclass+alltime.allgender+
          alltime.allage+alltime.allrace+alltime.allisea.pre+
          all.timepoint*relevel(factor(alltime.allsemester),ref=2))
summary(m1.1)
