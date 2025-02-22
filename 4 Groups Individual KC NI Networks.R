spring15 <- read.csv("~/Education/Data/Spring2015AllJesse5.0.csv",
                     header=FALSE, stringsAsFactors=FALSE)
spring16 <- read.csv("~/Education/Data/Spring2016AllJesse8.0.csv",
                     header=FALSE, stringsAsFactors=FALSE)
spring17 <- read.csv("~/Education/Data/Spring2017AllJesse4.consent.csv", header=FALSE)
spring17=spring17[spring17[,312]=="I consent.",]

fall17 <- read.csv("~/Education/Data/Fall2017AllJesse2.0.csv", header=FALSE)
#fall17 <- read.csv("~/Education/Data/Fall2017AllJesse.csv", header=FALSE)#OLD

fall17 <- read.csv("~/Education/Data/Spring2018AllJesse3.0.csv", header=FALSE)
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




###Create Fall17 variables  ####DO NOT USE FALL17.  USE SPRING18 (I CALL FALL17 FOR SIMPLICITY)
#Pre
skc.fall17.pre=fall17[,11]
snaive.fall17.pre=fall17[,12]
smodel.fall17.pre=fall17[,13]
rkc.fall17.pre=fall17[,23]
rnaive.fall17.pre=fall17[,24]
rmodel.fall17.pre=fall17[,25]
cans.fall17.pre=fall17[,50]
##percept.fall17.pre=fall17[,57]  Does not Exist
#Take Short version of ISEA
isea.fall17.pre.long=fall17[,75]
isea.fall17.pre.short=fall17[,c(54,55,57,58,59,62,63,65,67,70,71,73)]
isea.fall17.pre=rowSums(isea.fall17.pre.short)
cor.test(isea.fall17.pre,isea.fall17.pre.long) ##Very Correlated!! Short can be used!! p<e-16
#Exam1 ACORNS
skc.fall17.ex1=fall17[,107]
snaive.fall17.ex1=fall17[,108]
smodel.fall17.ex1=fall17[,109]
rkc.fall17.ex1=fall17[,95]
rnaive.fall17.ex1=fall17[,96]
rmodel.fall17.ex1=fall17[,97]
##Final/Post    ########WORK ON THIS########
animalgainkc.fall17=ifelse(fall17[,110]=="strepsirrhini tapetum lucidum gain",
                           fall17[,120],NA)
animalgainnaive.fall17=ifelse(fall17[,110]=="strepsirrhini tapetum lucidum gain",
                              fall17[,121],NA)
animalgainmodel.fall17=ifelse(fall17[,110]=="strepsirrhini tapetum lucidum gain",
                              fall17[,122],NA)
animallosskc.fall17=ifelse(fall17[,123]=="strepsirrhini tapetum lucidum loss",
                           fall17[,133],NA)
animallossnaive.fall17=ifelse(fall17[,123]=="strepsirrhini tapetum lucidum loss",
                              fall17[,134],NA)
animallossmodel.fall17=ifelse(fall17[,123]=="strepsirrhini tapetum lucidum loss",
                              fall17[,135],NA)
lillykc.fall17=ifelse(fall17[,110]=="lilly petal loss",fall17[,120],NA)
lillynaive.fall17=ifelse(fall17[,110]=="lilly petal loss",fall17[,121],NA)
lillymodel.fall17=ifelse(fall17[,110]=="lilly petal loss",fall17[,122],NA)
orchidkc.fall17=ifelse(fall17[,123]=="orchid leaf loss",fall17[,133],NA)
orchidnaive.fall17=ifelse(fall17[,123]=="orchid leaf loss",fall17[,134],NA)
orchidmodel.fall17=ifelse(fall17[,123]=="orchid leaf loss",fall17[,135],NA)
skc.fall17.post=pmin(animalgainkc.fall17,animallosskc.fall17,na.rm=TRUE)
snaive.fall17.post=pmin(animalgainnaive.fall17,animallossnaive.fall17,na.rm=TRUE)
smodel.fall17.post=pmin(animalgainmodel.fall17,animallossmodel.fall17,na.rm=TRUE)
rkc.fall17.post=pmin(orchidkc.fall17,lillykc.fall17,na.rm=TRUE)
rnaive.fall17.post=pmin(orchidnaive.fall17,lillynaive.fall17,na.rm=TRUE)
rmodel.fall17.post=pmin(orchidmodel.fall17,lillymodel.fall17,na.rm=TRUE)
cans.fall17.post=fall17[,160]
#percept.fall17.post=fall17[,241]   #Does not exist
##Take Short version of ISEA
isea.fall17.post.long=fall17[,185]
isea.fall17.post.short=fall17[,c(164,165,167,168,169,172,173,175,177,180,181,183)]
isea.fall17.post=rowSums(isea.fall17.post.short)
cor.test(isea.fall17.post,isea.fall17.post.long)  ##Very Correlated!! Short can be used!! p<e-16
##Demographic
section.fall17=fall17[,1]
rank.fall17=fall17[,76]
level.fall17=fall17[,77]
plan.fall17=fall17[,78]
esl.fall17=fall17[,79]
reading.fall17=fall17[,80]
writing.fall17=fall17[,81]
bio.fall17=fall17[,82]
gender.fall17=fall17[,83]
age.fall17=fall17[,84]
race.fall17=fall17[,85]
#wordcount.fall17.spre=fall17[,304]
#wordcount.fall17.rpre=fall17[,305]
#wordcount.fall17.sex1=fall17[,306]
#wordcount.fall17.rex1=fall17[,307]
#wordcount.fall17.spost=fall17[,308]
#wordcount.fall17.rpost=fall17[,309]



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
cor.test(isea.fall17.post,isea.fall17.post.long)  ##Very Correlated!! Short can be used!! p<e-18
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
t.test(orchidkc.fall17,lillykc.fall17,paired=FALSE)  #p=0.28
t.test(orchidnaive.fall17,lillynaive.fall17,paired=FALSE) #p=0.27
t.test(orchidmodel.fall17,lillymodel.fall17,paired=FALSE) #p=0.51
t.test(animalgainkc.fall17,animallosskc.fall17,paired=FALSE)  #p=0.51
t.test(animalgainnaive.fall17,animallossnaive.fall17,paired=FALSE) #p=0.04
t.test(animalgainmodel.fall17,animallossmodel.fall17,paired=FALSE) #p=0.05





####Individual KC and NI####

##Spring 15
a.kc.s15.pre=spring15[,2:7]
a.ni.s15.pre=spring15[,8:10]
p.kc.s15.pre=spring15[,14:19]
p.ni.s15.pre=spring15[,20:22]
a.kc.s15.ex1=spring15[,102:107]
a.ni.s15.ex1=spring15[,108:110]
p.kc.s15.ex1=spring15[,90:95]
p.ni.s15.ex1=spring15[,96:98]
a.kc.s15.post=spring15[,150:155]
a.ni.s15.post=spring15[,156:158]
p.kc.s15.post=spring15[,138:143]
p.ni.s15.post=spring15[,144:146]



##Spring 16
a.kc.s16.pre=spring16[,2:7]
a.ni.s16.pre=spring16[,8:10]
p.kc.s16.pre=spring16[,14:19]
p.ni.s16.pre=spring16[,20:22]
a.kc.s16.ex1=spring16[,102:107]
a.ni.s16.ex1=spring16[,108:110]
p.kc.s16.ex1=spring16[,90:95]
p.ni.s16.ex1=spring16[,96:98]
o.kc.s16=spring16[,138:143]   #Question Split
o.ni.s16=spring16[,144:146]
l.kc.s16=spring16[,150:155]
l.ni.s16=spring16[,156:158]
ag.kc.s16=spring16[,162:167]
ag.ni.s16=spring16[,168:170]
al.kc.s16=spring16[,174:179]
al.ni.s16=spring16[,180:182]
which(is.na(o.kc.s16)==FALSE&is.na(l.kc.s16)==FALSE)
which(is.na(ag.kc.s16)==FALSE&is.na(al.kc.s16)==FALSE)
a.kc.s16.post=ag.kc.s16
a.kc.s16.post[!is.na(al.kc.s16)]=al.kc.s16[!is.na(al.kc.s16)]  #Combine
a.ni.s16.post=ag.ni.s16
a.ni.s16.post[!is.na(al.ni.s16)]=al.ni.s16[!is.na(al.ni.s16)]
p.kc.s16.post=o.kc.s16
p.kc.s16.post[!is.na(l.kc.s16)]=l.kc.s16[!is.na(l.kc.s16)]
p.ni.s16.post=o.ni.s16
p.ni.s16.post[!is.na(l.ni.s16)]=l.ni.s16[!is.na(l.ni.s16)]



##Spring 17
a.kc.s17.pre=spring17[,2:7]
a.ni.s17.pre=spring17[,8:10]
p.kc.s17.pre=spring17[,14:19]
p.ni.s17.pre=spring17[,20:22]
a.kc.s17.ex1=spring17[,133:138]
a.ni.s17.ex1=spring17[,139:141]
p.kc.s17.ex1=spring17[,121:126]
p.ni.s17.ex1=spring17[,127:129]
spring17[,182][which(spring17[,182]=="strepsirrhini tapetum lucidum loss")]="Strepsirrhini tapetum lucidum loss"
#Animal Gain Matrices
againkc1.1.s17=ifelse(spring17[,169]=="Strepsirrhini tapetum lucidum gain",
                      spring17[,170],NA)
againkc1.2.s17=ifelse(spring17[,169]=="Strepsirrhini tapetum lucidum gain",
                      spring17[,171],NA)
againkc1.3.s17=ifelse(spring17[,169]=="Strepsirrhini tapetum lucidum gain",
                      spring17[,172],NA)
againkc1.4.s17=ifelse(spring17[,169]=="Strepsirrhini tapetum lucidum gain",
                      spring17[,173],NA)
againkc1.5.s17=ifelse(spring17[,169]=="Strepsirrhini tapetum lucidum gain",
                      spring17[,174],NA)
againkc1.6.s17=ifelse(spring17[,169]=="Strepsirrhini tapetum lucidum gain",
                      spring17[,175],NA)
againkc1.s17=matrix(c(againkc1.1.s17,againkc1.2.s17,againkc1.3.s17,againkc1.4.s17,
                      againkc1.5.s17,againkc1.6.s17),ncol=6)

againkc2.1.s17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum gain",
                      spring17[,183],NA)
againkc2.2.s17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum gain",
                      spring17[,184],NA)
againkc2.3.s17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum gain",
                      spring17[,185],NA)
againkc2.4.s17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum gain",
                      spring17[,186],NA)
againkc2.5.s17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum gain",
                      spring17[,187],NA)
againkc2.6.s17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum gain",
                      spring17[,188],NA)
againkc2.s17=matrix(c(againkc2.1.s17,againkc2.2.s17,againkc2.3.s17,againkc2.4.s17,
                      againkc2.5.s17,againkc2.6.s17),ncol=6)

againkc.s17=againkc1.s17
againkc.s17[!is.na(againkc2.s17)]=againkc2.s17[!is.na(againkc2.s17)]

againni1.1.s17=ifelse(spring17[,169]=="Strepsirrhini tapetum lucidum gain",
                      spring17[,176],NA)
againni1.2.s17=ifelse(spring17[,169]=="Strepsirrhini tapetum lucidum gain",
                      spring17[,177],NA)
againni1.3.s17=ifelse(spring17[,169]=="Strepsirrhini tapetum lucidum gain",
                      spring17[,178],NA)
againni1.s17=matrix(c(againni1.1.s17,againni1.2.s17,againni1.3.s17),ncol=3)

againni2.1.s17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum gain",
                      spring17[,189],NA)
againni2.2.s17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum gain",
                      spring17[,190],NA)
againni2.3.s17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum gain",
                      spring17[,191],NA)
againni2.s17=matrix(c(againni2.1.s17,againni2.2.s17,againni2.3.s17),ncol=3)

againni.s17=againni1.s17
againni.s17[!is.na(againni2.s17)]=againni2.s17[!is.na(againni2.s17)]

#Animal Loss Matrices
alosskc1.1.s17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum loss",
                      spring17[,183],NA)
alosskc1.2.s17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum loss",
                      spring17[,184],NA)
alosskc1.3.s17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum loss",
                      spring17[,185],NA)
alosskc1.4.s17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum loss",
                      spring17[,186],NA)
alosskc1.5.s17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum loss",
                      spring17[,187],NA)
alosskc1.6.s17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum loss",
                      spring17[,188],NA)
alosskc.s17=matrix(c(alosskc1.1.s17,alosskc1.2.s17,alosskc1.3.s17,alosskc1.4.s17,
                     alosskc1.5.s17,alosskc1.6.s17),ncol=6)

alossni1.1.s17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum loss",
                      spring17[,189],NA)
alossni1.2.s17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum loss",
                      spring17[,190],NA)
alossni1.3.s17=ifelse(spring17[,182]=="Strepsirrhini tapetum lucidum loss",
                      spring17[,191],NA)
alossni.s17=matrix(c(alossni1.1.s17,alossni1.2.s17,alossni1.3.s17),ncol=3)

#Orchid Loss Matrices
orchidkc1.1.s17=ifelse(spring17[,169]=="Orchid leaf loss",
                       spring17[,170],NA)
orchidkc1.2.s17=ifelse(spring17[,169]=="Orchid leaf loss",
                       spring17[,171],NA)
orchidkc1.3.s17=ifelse(spring17[,169]=="Orchid leaf loss",
                       spring17[,172],NA)
orchidkc1.4.s17=ifelse(spring17[,169]=="Orchid leaf loss",
                       spring17[,173],NA)
orchidkc1.5.s17=ifelse(spring17[,169]=="Orchid leaf loss",
                       spring17[,174],NA)
orchidkc1.6.s17=ifelse(spring17[,169]=="Orchid leaf loss",
                       spring17[,175],NA)
orchidkc1.s17=matrix(c(orchidkc1.1.s17,orchidkc1.2.s17,orchidkc1.3.s17,orchidkc1.4.s17,
                       orchidkc1.5.s17,orchidkc1.6.s17),ncol=6)

orchidkc2.1.s17=ifelse(spring17[,182]=="Orchid leaf loss",
                       spring17[,183],NA)
orchidkc2.2.s17=ifelse(spring17[,182]=="Orchid leaf loss",
                       spring17[,184],NA)
orchidkc2.3.s17=ifelse(spring17[,182]=="Orchid leaf loss",
                       spring17[,185],NA)
orchidkc2.4.s17=ifelse(spring17[,182]=="Orchid leaf loss",
                       spring17[,186],NA)
orchidkc2.5.s17=ifelse(spring17[,182]=="Orchid leaf loss",
                       spring17[,187],NA)
orchidkc2.6.s17=ifelse(spring17[,182]=="Orchid leaf loss",
                       spring17[,188],NA)
orchidkc2.s17=matrix(c(orchidkc2.1.s17,orchidkc2.2.s17,orchidkc2.3.s17,orchidkc2.4.s17,
                       orchidkc2.5.s17,orchidkc2.6.s17),ncol=6)

orchidkc.s17=orchidkc1.s17
orchidkc.s17[!is.na(orchidkc2.s17)]=orchidkc2.s17[!is.na(orchidkc2.s17)]

orchidni1.1.s17=ifelse(spring17[,169]=="Orchid leaf loss",
                       spring17[,176],NA)
orchidni1.2.s17=ifelse(spring17[,169]=="Orchid leaf loss",
                       spring17[,177],NA)
orchidni1.3.s17=ifelse(spring17[,169]=="Orchid leaf loss",
                       spring17[,178],NA)
orchidni1.s17=matrix(c(orchidni1.1.s17,orchidni1.2.s17,orchidni1.3.s17),ncol=3)

orchidni2.1.s17=ifelse(spring17[,182]=="Orchid leaf loss",
                       spring17[,189],NA)
orchidni2.2.s17=ifelse(spring17[,182]=="Orchid leaf loss",
                       spring17[,190],NA)
orchidni2.3.s17=ifelse(spring17[,182]=="Orchid leaf loss",
                       spring17[,191],NA)
orchidni2.s17=matrix(c(orchidni2.1.s17,orchidni2.2.s17,orchidni2.3.s17),ncol=3)

orchidni.s17=orchidni1.s17
orchidni.s17[!is.na(orchidni2.s17)]=orchidni2.s17[!is.na(orchidni2.s17)]

#Lilly Loss Matrices
lillykc1.1.s17=ifelse(spring17[,169]=="Lily petal loss",
                      spring17[,170],NA)
lillykc1.2.s17=ifelse(spring17[,169]=="Lily petal loss",
                      spring17[,171],NA)
lillykc1.3.s17=ifelse(spring17[,169]=="Lily petal loss",
                      spring17[,172],NA)
lillykc1.4.s17=ifelse(spring17[,169]=="Lily petal loss",
                      spring17[,173],NA)
lillykc1.5.s17=ifelse(spring17[,169]=="Lily petal loss",
                      spring17[,174],NA)
lillykc1.6.s17=ifelse(spring17[,169]=="Lily petal loss",
                      spring17[,175],NA)
lillykc.s17=matrix(c(lillykc1.1.s17,lillykc1.2.s17,lillykc1.3.s17,lillykc1.4.s17,
                     lillykc1.5.s17,lillykc1.6.s17),ncol=6)

lillyni1.1.s17=ifelse(spring17[,169]=="Lily petal loss",
                      spring17[,176],NA)
lillyni1.2.s17=ifelse(spring17[,169]=="Lily petal loss",
                      spring17[,177],NA)
lillyni1.3.s17=ifelse(spring17[,169]=="Lily petal loss",
                      spring17[,178],NA)
lillyni.s17=matrix(c(lillyni1.1.s17,lillyni1.2.s17,lillyni1.3.s17),ncol=3)

#Combine Gain/Loss Lilly/Orchid
a.kc.s17.post=againkc.s17
a.kc.s17.post[!is.na(alosskc.s17)]=alosskc.s17[!is.na(alosskc.s17)]
###a.kc.s17.post=as.matrix(a.kc.s17.post)    #########JWLHHKWHKJWHJKWHJWHJK######
a.ni.s17.post=againni.s17
a.ni.s17.post[!is.na(alossni.s17)]=alossni.s17[!is.na(alossni.s17)]
p.kc.s17.post=orchidkc.s17
p.kc.s17.post[!is.na(lillykc.s17)]=lillykc.s17[!is.na(lillykc.s17)]
p.ni.s17.post=orchidni.s17
p.ni.s17.post[!is.na(lillyni.s17)]=lillyni.s17[!is.na(lillyni.s17)]



##Fall 17       ####DO NOT USE THIS, USE SPRING18 BELOW####
a.kc.f17.pre=fall17[,2:7]
a.ni.f17.pre=fall17[,8:10]
p.kc.f17.pre=fall17[,14:19]
p.ni.f17.pre=fall17[,20:22]
a.kc.f17.ex1=fall17[,98:103]
a.ni.f17.ex1=fall17[,104:106]
p.kc.f17.ex1=fall17[,86:91]
p.ni.f17.ex1=fall17[,92:94]
table(fall17[,110])
table(fall17[,123])
#Animal Gain Matrices
againkc.1.f17=ifelse(fall17[,110]=="strepsirrhini tapetum lucidum gain",
                      fall17[,111],NA)
againkc.2.f17=ifelse(fall17[,110]=="strepsirrhini tapetum lucidum gain",
                      fall17[,112],NA)
againkc.3.f17=ifelse(fall17[,110]=="strepsirrhini tapetum lucidum gain",
                      fall17[,113],NA)
againkc.4.f17=ifelse(fall17[,110]=="strepsirrhini tapetum lucidum gain",
                      fall17[,114],NA)
againkc.5.f17=ifelse(fall17[,110]=="strepsirrhini tapetum lucidum gain",
                      fall17[,115],NA)
againkc.6.f17=ifelse(fall17[,110]=="strepsirrhini tapetum lucidum gain",
                      fall17[,116],NA)
againkc.f17=matrix(c(againkc.1.f17,againkc.2.f17,againkc.3.f17,againkc.4.f17,
                      againkc.5.f17,againkc.6.f17),ncol=6)

againni.1.f17=ifelse(fall17[,110]=="strepsirrhini tapetum lucidum gain",
                      fall17[,117],NA)
againni.2.f17=ifelse(fall17[,110]=="strepsirrhini tapetum lucidum gain",
                      fall17[,118],NA)
againni.3.f17=ifelse(fall17[,110]=="strepsirrhini tapetum lucidum gain",
                      fall17[,119],NA)
againni.f17=matrix(c(againni.1.f17,againni.2.f17,againni.3.f17),ncol=3)


#Animal Loss Matrices
alosskc.1.f17=ifelse(fall17[,123]=="strepsirrhini tapetum lucidum loss",
                      fall17[,124],NA)
alosskc.2.f17=ifelse(fall17[,123]=="strepsirrhini tapetum lucidum loss",
                      fall17[,125],NA)
alosskc.3.f17=ifelse(fall17[,123]=="strepsirrhini tapetum lucidum loss",
                      fall17[,126],NA)
alosskc.4.f17=ifelse(fall17[,123]=="strepsirrhini tapetum lucidum loss",
                      fall17[,127],NA)
alosskc.5.f17=ifelse(fall17[,123]=="strepsirrhini tapetum lucidum loss",
                      fall17[,128],NA)
alosskc.6.f17=ifelse(fall17[,123]=="strepsirrhini tapetum lucidum loss",
                      fall17[,129],NA)
alosskc.f17=matrix(c(alosskc.1.f17,alosskc.2.f17,alosskc.3.f17,alosskc.4.f17,
                     alosskc.5.f17,alosskc.6.f17),ncol=6)

alossni.1.f17=ifelse(fall17[,123]=="strepsirrhini tapetum lucidum loss",
                      fall17[,130],NA)
alossni.2.f17=ifelse(fall17[,123]=="strepsirrhini tapetum lucidum loss",
                      fall17[,131],NA)
alossni.3.f17=ifelse(fall17[,123]=="strepsirrhini tapetum lucidum loss",
                      fall17[,132],NA)
alossni.f17=matrix(c(alossni.1.f17,alossni.2.f17,alossni.3.f17),ncol=3)

#Orchid Loss Matrices
orchidkc.1.f17=ifelse(fall17[,123]=="orchid leaf loss",
                       fall17[,124],NA)
orchidkc.2.f17=ifelse(fall17[,123]=="orchid leaf loss",
                       fall17[,125],NA)
orchidkc.3.f17=ifelse(fall17[,123]=="orchid leaf loss",
                       fall17[,126],NA)
orchidkc.4.f17=ifelse(fall17[,123]=="orchid leaf loss",
                       fall17[,127],NA)
orchidkc.5.f17=ifelse(fall17[,123]=="orchid leaf loss",
                       fall17[,128],NA)
orchidkc.6.f17=ifelse(fall17[,123]=="orchid leaf loss",
                       fall17[,129],NA)
orchidkc.f17=matrix(c(orchidkc.1.f17,orchidkc.2.f17,orchidkc.3.f17,orchidkc.4.f17,
                       orchidkc.5.f17,orchidkc.6.f17),ncol=6)

orchidni.1.f17=ifelse(fall17[,123]=="orchid leaf loss",
                       fall17[,130],NA)
orchidni.2.f17=ifelse(fall17[,123]=="orchid leaf loss",
                       fall17[,131],NA)
orchidni.3.f17=ifelse(fall17[,123]=="orchid leaf loss",
                       fall17[,132],NA)
orchidni.f17=matrix(c(orchidni.1.f17,orchidni.2.f17,orchidni.3.f17),ncol=3)


#Lilly Loss Matrices
lillykc.1.f17=ifelse(fall17[,110]=="lilly petal loss",
                      fall17[,111],NA)
lillykc.2.f17=ifelse(fall17[,110]=="lilly petal loss",
                      fall17[,112],NA)
lillykc.3.f17=ifelse(fall17[,110]=="lilly petal loss",
                      fall17[,113],NA)
lillykc.4.f17=ifelse(fall17[,110]=="lilly petal loss",
                      fall17[,114],NA)
lillykc.5.f17=ifelse(fall17[,110]=="lilly petal loss",
                      fall17[,115],NA)
lillykc.6.f17=ifelse(fall17[,110]=="lilly petal loss",
                      fall17[,116],NA)
lillykc.f17=matrix(c(lillykc.1.f17,lillykc.2.f17,lillykc.3.f17,lillykc.4.f17,
                     lillykc.5.f17,lillykc.6.f17),ncol=6)

lillyni.1.f17=ifelse(fall17[,110]=="lilly petal loss",
                      fall17[,117],NA)
lillyni.2.f17=ifelse(fall17[,110]=="lilly petal loss",
                      fall17[,118],NA)
lillyni.3.f17=ifelse(fall17[,110]=="lilly petal loss",
                      fall17[,119],NA)
lillyni.f17=matrix(c(lillyni.1.f17,lillyni.2.f17,lillyni.3.f17),ncol=3)

#Combine Gain/Loss Lilly/Orchid
a.kc.f17.post=againkc.f17
a.kc.f17.post[!is.na(alosskc.f17)]=alosskc.f17[!is.na(alosskc.f17)]
###a.kc.f17.post=as.matrix(a.kc.f17.post)    #########JWLHHKWHKJWHJKWHJWHJK######
a.ni.f17.post=againni.f17
a.ni.f17.post[!is.na(alossni.f17)]=alossni.f17[!is.na(alossni.f17)]
p.kc.f17.post=orchidkc.f17
p.kc.f17.post[!is.na(lillykc.f17)]=lillykc.f17[!is.na(lillykc.f17)]
p.ni.f17.post=orchidni.f17
p.ni.f17.post[!is.na(lillyni.f17)]=lillyni.f17[!is.na(lillyni.f17)]



##Spring18   #### I CALL THIS FALL17 TO SAVE FROM WRITING CODE ####
a.kc.f17.pre=fall17[,6:11]
a.ni.f17.pre=fall17[,12:14]
p.kc.f17.pre=fall17[,18:23]
p.ni.f17.pre=fall17[,24:26]
a.kc.f17.ex1=fall17[,117:122]
a.ni.f17.ex1=fall17[,123:125]
p.kc.f17.ex1=fall17[,105:110]
p.ni.f17.ex1=fall17[,111:113]
table(fall17[,129])
table(fall17[,143])
#Animal Gain Matrices
againkc.1.f17=ifelse(fall17[,129]=="strepsirrhini tapetum lucidum gain",
                     fall17[,130],NA)
againkc.2.f17=ifelse(fall17[,129]=="strepsirrhini tapetum lucidum gain",
                     fall17[,131],NA)
againkc.3.f17=ifelse(fall17[,129]=="strepsirrhini tapetum lucidum gain",
                     fall17[,132],NA)
againkc.4.f17=ifelse(fall17[,129]=="strepsirrhini tapetum lucidum gain",
                     fall17[,133],NA)
againkc.5.f17=ifelse(fall17[,129]=="strepsirrhini tapetum lucidum gain",
                     fall17[,134],NA)
againkc.6.f17=ifelse(fall17[,129]=="strepsirrhini tapetum lucidum gain",
                     fall17[,135],NA)
againkc.f17=matrix(c(againkc.1.f17,againkc.2.f17,againkc.3.f17,againkc.4.f17,
                     againkc.5.f17,againkc.6.f17),ncol=6)

againni.1.f17=ifelse(fall17[,129]=="strepsirrhini tapetum lucidum gain",
                     fall17[,136],NA)
againni.2.f17=ifelse(fall17[,129]=="strepsirrhini tapetum lucidum gain",
                     fall17[,137],NA)
againni.3.f17=ifelse(fall17[,129]=="strepsirrhini tapetum lucidum gain",
                     fall17[,138],NA)
againni.f17=matrix(c(againni.1.f17,againni.2.f17,againni.3.f17),ncol=3)


#Animal Loss Matrices
alosskc.1.f17=ifelse(fall17[,143]=="strepsirrhini tapetum lucidum loss",
                     fall17[,144],NA)
alosskc.2.f17=ifelse(fall17[,143]=="strepsirrhini tapetum lucidum loss",
                     fall17[,145],NA)
alosskc.3.f17=ifelse(fall17[,143]=="strepsirrhini tapetum lucidum loss",
                     fall17[,146],NA)
alosskc.4.f17=ifelse(fall17[,143]=="strepsirrhini tapetum lucidum loss",
                     fall17[,147],NA)
alosskc.5.f17=ifelse(fall17[,143]=="strepsirrhini tapetum lucidum loss",
                     fall17[,148],NA)
alosskc.6.f17=ifelse(fall17[,143]=="strepsirrhini tapetum lucidum loss",
                     fall17[,149],NA)
alosskc.f17=matrix(c(alosskc.1.f17,alosskc.2.f17,alosskc.3.f17,alosskc.4.f17,
                     alosskc.5.f17,alosskc.6.f17),ncol=6)

alossni.1.f17=ifelse(fall17[,143]=="strepsirrhini tapetum lucidum loss",
                     fall17[,150],NA)
alossni.2.f17=ifelse(fall17[,143]=="strepsirrhini tapetum lucidum loss",
                     fall17[,151],NA)
alossni.3.f17=ifelse(fall17[,143]=="strepsirrhini tapetum lucidum loss",
                     fall17[,152],NA)
alossni.f17=matrix(c(alossni.1.f17,alossni.2.f17,alossni.3.f17),ncol=3)

#Orchid Loss Matrices
orchidkc.1.f17=ifelse(fall17[,143]=="orchid leaf loss",
                      fall17[,144],NA)
orchidkc.2.f17=ifelse(fall17[,143]=="orchid leaf loss",
                      fall17[,145],NA)
orchidkc.3.f17=ifelse(fall17[,143]=="orchid leaf loss",
                      fall17[,146],NA)
orchidkc.4.f17=ifelse(fall17[,143]=="orchid leaf loss",
                      fall17[,147],NA)
orchidkc.5.f17=ifelse(fall17[,143]=="orchid leaf loss",
                      fall17[,148],NA)
orchidkc.6.f17=ifelse(fall17[,143]=="orchid leaf loss",
                      fall17[,149],NA)
orchidkc.f17=matrix(c(orchidkc.1.f17,orchidkc.2.f17,orchidkc.3.f17,orchidkc.4.f17,
                      orchidkc.5.f17,orchidkc.6.f17),ncol=6)

orchidni.1.f17=ifelse(fall17[,143]=="orchid leaf loss",
                      fall17[,150],NA)
orchidni.2.f17=ifelse(fall17[,143]=="orchid leaf loss",
                      fall17[,151],NA)
orchidni.3.f17=ifelse(fall17[,143]=="orchid leaf loss",
                      fall17[,152],NA)
orchidni.f17=matrix(c(orchidni.1.f17,orchidni.2.f17,orchidni.3.f17),ncol=3)


#Lilly Loss Matrices
lillykc.1.f17=ifelse(fall17[,129]=="lily petal loss",
                     fall17[,130],NA)
lillykc.2.f17=ifelse(fall17[,129]=="lily petal loss",
                     fall17[,131],NA)
lillykc.3.f17=ifelse(fall17[,129]=="lily petal loss",
                     fall17[,132],NA)
lillykc.4.f17=ifelse(fall17[,129]=="lily petal loss",
                     fall17[,133],NA)
lillykc.5.f17=ifelse(fall17[,129]=="lily petal loss",
                     fall17[,134],NA)
lillykc.6.f17=ifelse(fall17[,129]=="lily petal loss",
                     fall17[,135],NA)
lillykc.f17=matrix(c(lillykc.1.f17,lillykc.2.f17,lillykc.3.f17,lillykc.4.f17,
                     lillykc.5.f17,lillykc.6.f17),ncol=6)

lillyni.1.f17=ifelse(fall17[,129]=="lily petal loss",
                     fall17[,136],NA)
lillyni.2.f17=ifelse(fall17[,129]=="lily petal loss",
                     fall17[,137],NA)
lillyni.3.f17=ifelse(fall17[,129]=="lily petal loss",
                     fall17[,138],NA)
lillyni.f17=matrix(c(lillyni.1.f17,lillyni.2.f17,lillyni.3.f17),ncol=3)

#Combine Gain/Loss Lilly/Orchid
a.kc.f17.post=againkc.f17
a.kc.f17.post[!is.na(alosskc.f17)]=alosskc.f17[!is.na(alosskc.f17)]
###a.kc.f17.post=as.matrix(a.kc.f17.post)    #########JWLHHKWHKJWHJKWHJWHJK######
a.ni.f17.post=againni.f17
a.ni.f17.post[!is.na(alossni.f17)]=alossni.f17[!is.na(alossni.f17)]
p.kc.f17.post=orchidkc.f17
p.kc.f17.post[!is.na(lillykc.f17)]=lillykc.f17[!is.na(lillykc.f17)]
p.ni.f17.post=orchidni.f17
p.ni.f17.post[!is.na(lillyni.f17)]=lillyni.f17[!is.na(lillyni.f17)]



semester=c(rep("a",length(cins.spring15.pre)),rep("b",length(cins.spring16.pre)),
           rep("c",length(cins.spring17.pre)),rep("d",length(cans.fall17.pre)))
sem=factor(semester)



#Combine the Individual KC NI from all 3 semesters
skc0.ind=rbind(as.matrix(a.kc.s15.pre),as.matrix(a.kc.s16.pre),as.matrix(a.kc.s17.pre),
               as.matrix(a.kc.f17.pre))
skc1.ind=rbind(as.matrix(a.kc.s15.ex1),as.matrix(a.kc.s16.ex1),as.matrix(a.kc.s17.ex1),
               as.matrix(a.kc.f17.ex1))
skc2.ind=rbind(as.matrix(a.kc.s15.post),as.matrix(a.kc.s16.post),as.matrix(a.kc.s17.post),
               as.matrix(a.kc.f17.post))
skc.all.ind=rbind(as.matrix(skc0.ind),as.matrix(skc1.ind),as.matrix(skc2.ind))

rkc0.ind=rbind(as.matrix(p.kc.s15.pre),as.matrix(p.kc.s16.pre),as.matrix(p.kc.s17.pre),
               as.matrix(p.kc.f17.pre))
rkc1.ind=rbind(as.matrix(p.kc.s15.ex1),as.matrix(p.kc.s16.ex1),as.matrix(p.kc.s17.ex1),
               as.matrix(p.kc.f17.ex1))
rkc2.ind=rbind(as.matrix(p.kc.s15.post),as.matrix(p.kc.s16.post),as.matrix(p.kc.s17.post),
               as.matrix(p.kc.f17.post))
rkc.all.ind=rbind(as.matrix(rkc0.ind),as.matrix(rkc1.ind),as.matrix(rkc2.ind))

snaive0.ind=rbind(as.matrix(a.ni.s15.pre),as.matrix(a.ni.s16.pre),as.matrix(a.ni.s17.pre),
                  as.matrix(a.ni.f17.pre))
snaive1.ind=rbind(as.matrix(a.ni.s15.ex1),as.matrix(a.ni.s16.ex1),as.matrix(a.ni.s17.ex1),
                  as.matrix(a.ni.f17.ex1))
snaive2.ind=rbind(as.matrix(a.ni.s15.post),as.matrix(a.ni.s16.post),as.matrix(a.ni.s17.post),
                  as.matrix(a.ni.f17.post))
snaive.all.ind=rbind(as.matrix(snaive0.ind),as.matrix(snaive1.ind),as.matrix(snaive2.ind))

rnaive0.ind=rbind(as.matrix(p.ni.s15.pre),as.matrix(p.ni.s16.pre),as.matrix(p.ni.s17.pre),
                  as.matrix(p.ni.f17.pre))
rnaive1.ind=rbind(as.matrix(p.ni.s15.ex1),as.matrix(p.ni.s16.ex1),as.matrix(p.ni.s17.ex1),
                  as.matrix(p.ni.f17.ex1))
rnaive2.ind=rbind(as.matrix(p.ni.s15.post),as.matrix(p.ni.s16.post),as.matrix(p.ni.s17.post),
                  as.matrix(p.ni.f17.post))
rnaive.all.ind=rbind(as.matrix(rnaive0.ind),as.matrix(rnaive1.ind),as.matrix(rnaive2.ind))

time=c(rep(0,length(skc0.ind[,1])),rep(1,length(skc1.ind[,1])),rep(2,length(skc2.ind[,1])))



#Demographics
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
cins.pre=c(cins.spring15.pre,cins.spring16.pre,cins.spring17.pre,cins.fall17.pre)
isea.pre=c(isea.spring15.pre,isea.spring16.pre,isea.spring17.pre,isea.fall17.pre)


###STOP HERE FOR MOST STUFF, GO ON TO DOC2.0 FOR INDIVID/CLASS KC/NI COMBINES NETWORKS
###ANIMAL PLANT SEPARATE FOR NOW
###ALSO STOP HERE FOR NEWEST (2019) JACCARD SIMILARITY AND GO TO "4 Groups Network Similarity"





###REPORT PROPORTIONS OF EACH KC AND NI BY SEMESTER###
#Animal KC
x=matrix(rep(0,24),nrow=6)     ##Loop to create matrix of all KC##
for(i in 1:6){
  x[i,]=tapply(skc0.ind[,i],semester,mean,na.rm=T)
}
x1.0=x

x=matrix(rep(0,24),nrow=6)
for(i in 1:6){
  x[i,]=tapply(skc1.ind[,i],semester,mean,na.rm=T)
}
x1.1=x

x=matrix(rep(0,24),nrow=6)
for(i in 1:6){
  x[i,]=tapply(skc2.ind[,i],semester,mean,na.rm=T)
}
x1.2=x

#Plant KC
x=matrix(rep(0,24),nrow=6)     ##Loop to create matrix of all KC##
for(i in 1:6){
  x[i,]=tapply(rkc0.ind[,i],semester,mean,na.rm=T)
}
x2.0=x

x=matrix(rep(0,24),nrow=6)
for(i in 1:6){
  x[i,]=tapply(rkc1.ind[,i],semester,mean,na.rm=T)
}
x2.1=x

x=matrix(rep(0,24),nrow=6)
for(i in 1:6){
  x[i,]=tapply(rkc2.ind[,i],semester,mean,na.rm=T)
}
x2.2=x

#Animal NI
x=matrix(rep(0,12),nrow=3)     ##Loop to create matrix of all NI##
for(i in 1:3){
  x[i,]=tapply(snaive0.ind[,i],semester,mean,na.rm=T)
}
x3.0=x

x=matrix(rep(0,12),nrow=3)
for(i in 1:3){
  x[i,]=tapply(snaive1.ind[,i],semester,mean,na.rm=T)
}
x3.1=x

x=matrix(rep(0,12),nrow=3)
for(i in 1:3){
  x[i,]=tapply(snaive2.ind[,i],semester,mean,na.rm=T)
}
x3.2=x

#Plant NI
x=matrix(rep(0,12),nrow=3)     ##Loop to create matrix of all NI##
for(i in 1:3){
  x[i,]=tapply(rnaive0.ind[,i],semester,mean,na.rm=T)
}
x4.0=x

x=matrix(rep(0,12),nrow=3)
for(i in 1:3){
  x[i,]=tapply(rnaive1.ind[,i],semester,mean,na.rm=T)
}
x4.1=x

x=matrix(rep(0,12),nrow=3)
for(i in 1:3){
  x[i,]=tapply(rnaive2.ind[,i],semester,mean,na.rm=T)
}
x4.2=x


#Combined Table
x.akc=cbind(x1.0,x1.1,x1.2)
x.pkc=cbind(x2.0,x2.1,x2.2)
x.ani=cbind(x3.0,x3.1,x3.2)
x.pni=cbind(x4.0,x4.1,x4.2)
x.all=rbind(x.akc,x.pkc,x.ani,x.pni)
round(x.all,4)


#################################################
###### NETWORK STATISTICS ######

##Clustering stuff##

#CODE FROM STACK EXCHANGE#
library(cluster)
data(flower)
str(flower)
flower <- flower[, 1:6] # just to keep only categorial variables
# Dissimilarity matrix matrix
distm <- daisy(flower, metric = "gower", stand = FALSE)
distm

# Hierarchical agglomerative clustering (HAC)  (THIS ONE ONLY USES EUCLIDEAN OR MANHATTAN. DONT USE)
hac <- agnes(distm, diss = TRUE)
hac$order
plot(hac)

# Partial clustering algorithm with automatic estimation of the number of 
# clusters and identification of outliers
library(CrossClustering)
cross.clust <- CrossClustering(distm, k.w.min = 2, k.w.max = 5,
                               k.c.max = 6, out = TRUE)
cross.clust$Cluster.list


##My attempt.  Spring 15 pre-test Animal Key Concepts

#Create dissimilarity matrix using daisy()
#"This is typically the input for the functions pam, fanny, agnes or diana"
dismat <- daisy(na.omit(a.kc.s15.pre), metric = "gower", type = list(asymm=c(1,2,3,4,5,6)),
                stand = FALSE)

#Create distance matrix using dist()
dismat2=dist(na.omit(a.kc.s15.pre), method = "binary", diag = FALSE, upper = FALSE)

#cluster using the dissimilatiry matrix from daisy or dist,
#between 2 and 5 clusters  (NOT WORKING for daisy ,NA's)
cross.clust.akc=CrossClustering(dismat2, k.w.min = 2, k.w.max = 5,
                                k.c.max = 6, out = TRUE)
cross.clust.akc$Cluster.list

#Examine the people in the clusters
na.omit(a.kc.s15.pre)[unlist(cross.clust.akc$Cluster.list[1]),]
na.omit(a.kc.s15.pre)[unlist(cross.clust.akc$Cluster.list[2]),]
na.omit(a.kc.s15.pre)[unlist(cross.clust.akc$Cluster.list[3]),]
na.omit(a.kc.s15.pre)[unlist(cross.clust.akc$Cluster.list[4]),]
na.omit(a.kc.s15.pre)[-unlist(cross.clust.akc$Cluster.list),]


###############################
##Co-occurrence network stuff##
library(igraph)


##Just Spring15 Animal Key Concepts##

#Create co-occurrence frequency table#
n=length(a.kc.s15.pre[,1])
mymat=matrix(0,nrow=6,ncol=6)
for(k in 1:n){
  for(i in 1:6){
    for(j in 1:6){  
      if (sum(a.kc.s15.pre[k,i],a.kc.s15.pre[k,j],na.rm=T)==2){
      mymat[i,j]=mymat[i,j]+1
      }
    }
  }
}
colnames(mymat)=c("Var","Her","Comp","Ltd","Diff","Non")
rownames(mymat)=colnames(mymat)
  
mynet=graph_from_adjacency_matrix(mymat, mode = "lower", weighted = NULL, diag = TRUE,
                            add.colnames = NULL, add.rownames = NA)
mynet2=graph_from_adjacency_matrix(mymat, mode = "lower", weighted = NULL, diag = FALSE,
                                  add.colnames = NULL, add.rownames = NA)

betweenness(mynet, directed=FALSE)
centr_betw(mynet)
closeness(mynet)
vertex_connectivity(mynet)
diameter(mynet)
degree(mynet)  #and mynet2



##Just Spring15 Animal Key Concepts and Naive Ideas##

animal.s15.pre=cbind(a.kc.s15.pre,a.ni.s15.pre)
colnames(animal.s15.pre)=c("Var","Her","Comp","Ltd","Diff","Non","Ada","Need","Use")

#animal.s15.pre[,7:9]=1-animal.s15.pre[,7:9]   #for reverse coding reasons

#Create co-occurrence frequency table#
n=length(animal.s15.pre[,1])
mymat=matrix(0,nrow=9,ncol=9)
for(k in 1:n){
  for(i in 1:9){
    for(j in 1:9){  
      if (sum(animal.s15.pre[k,i],animal.s15.pre[k,j],na.rm=T)==2){
        mymat[i,j]=mymat[i,j]+1
      }
    }
  }
}
colnames(mymat)=c("Var","Her","Comp","Ltd","Diff","Non","Ada","Need","Use")
rownames(mymat)=colnames(mymat)

mynet=graph_from_adjacency_matrix(mymat, mode = "lower", weighted = NULL, diag = TRUE,
                                  add.colnames = NULL, add.rownames = NA)
mynet2=graph_from_adjacency_matrix(mymat, mode = "lower", weighted = NULL, diag = FALSE,
                                   add.colnames = NULL, add.rownames = NA)

degree(mynet2)
betweenness(mynet, directed=FALSE)
centr_betw(mynet)
closeness(mynet)
vertex_connectivity(mynet)
diameter(mynet)
degree(mynet)  #and mynet2

dismat <- daisy(na.omit(animal.s15.pre), metric = "gower", type = list(asymm=c(1:9)),
                stand = FALSE)

dismat2=dist(na.omit(animal.s15.pre), method = "binary", diag = FALSE, upper = FALSE)

cross.clust.akc=CrossClustering(dismat2, k.w.min = 2, k.w.max = 5,
                                k.c.max = 6, out = TRUE)
cross.clust.akc$Cluster.list

#Examine the people in the clusters
na.omit(animal.s15.pre)[unlist(cross.clust.akc$Cluster.list[1]),]
na.omit(animal.s15.pre)[unlist(cross.clust.akc$Cluster.list[2]),]
na.omit(animal.s15.pre)[unlist(cross.clust.akc$Cluster.list[3]),]
na.omit(animal.s15.pre)[-unlist(cross.clust.akc$Cluster.list),]




##Just Spring15. Both Animal and Plant, Key Concepts and Naive Ideas##

all.s15.pre=cbind(a.kc.s15.pre,p.kc.s15.pre,a.ni.s15.pre,p.ni.s15.pre)
colnames(all.s15.pre)=c("Var","Her","Comp","Ltd","Diff","Non","Var","Her","Comp","Ltd","Diff","Non",
                           "Ada","Need","Use","Ada","Need","Use")

#animal.s15.pre[,7:9]=1-animal.s15.pre[,7:9]   #for reverse coding reasons

#Create co-occurrence frequency table#
n=length(all.s15.pre[,1])
mymat=matrix(0,nrow=18,ncol=18)
for(k in 1:n){
  for(i in 1:18){
    for(j in 1:18){  
      if (sum(all.s15.pre[k,i],all.s15.pre[k,j],na.rm=T)==2){
        mymat[i,j]=mymat[i,j]+1
      }
    }
  }
}
colnames(mymat)=c("Var","Her","Comp","Ltd","Diff","Non","Var","Her","Comp","Ltd","Diff","Non",
                  "Ada","Need","Use","Ada","Need","Use")
rownames(mymat)=colnames(mymat)

mynet=graph_from_adjacency_matrix(mymat, mode = "lower", weighted = NULL, diag = TRUE,
                                  add.colnames = NULL, add.rownames = NA)
mynet2=graph_from_adjacency_matrix(mymat, mode = "lower", weighted = NULL, diag = FALSE,
                                   add.colnames = NULL, add.rownames = NA)

degree(mynet2)
betweenness(mynet, directed=FALSE)
centr_betw(mynet)
closeness(mynet)
vertex_connectivity(mynet)
diameter(mynet)
degree(mynet)  #and mynet2

dismat <- daisy(na.omit(all.s15.pre), metric = "gower", type = list(asymm=c(1:18)),
                stand = FALSE)

dismat2=dist(na.omit(all.s15.pre), method = "binary", diag = FALSE, upper = FALSE)

cross.clust.akc=CrossClustering(dismat2, k.w.min = 2, k.w.max = 5,
                                k.c.max = 6, out = TRUE)
cross.clust.akc$Cluster.list

#Examine the people in the clusters
na.omit(all.s15.pre)[unlist(cross.clust.akc$Cluster.list[1]),]
na.omit(all.s15.pre)[unlist(cross.clust.akc$Cluster.list[2]),]
na.omit(all.s15.pre)[unlist(cross.clust.akc$Cluster.list[3]),]
na.omit(all.s15.pre)[-unlist(cross.clust.akc$Cluster.list),]




##Just Spring15.  Animal and Plant Key Concepts##
ap.kc.s15.pre=cbind(a.kc.s15.pre,p.kc.s15.pre)
colnames(ap.kc.s15.pre)=c("Var","Her","Comp","Ltd","Diff","Non","Var","Her","Comp","Ltd","Diff","Non")

#animal.s15.pre[,7:9]=1-animal.s15.pre[,7:9]   #for reverse coding reasons

#Create co-occurrence frequency table#
n=length(ap.kc.s15.pre[,1])
mymat=matrix(0,nrow=12,ncol=12)
for(k in 1:n){
  for(i in 1:12){
    for(j in 1:12){  
      if (sum(ap.kc.s15.pre[k,i],ap.kc.s15.pre[k,j],na.rm=T)==2){
        mymat[i,j]=mymat[i,j]+1
      }
    }
  }
}
colnames(mymat)=c("Var","Her","Comp","Ltd","Diff","Non","Var","Her","Comp","Ltd","Diff","Non")
rownames(mymat)=colnames(mymat)

mynet=graph_from_adjacency_matrix(mymat, mode = "lower", weighted = NULL, diag = TRUE,
                                  add.colnames = NULL, add.rownames = NA)
mynet2=graph_from_adjacency_matrix(mymat, mode = "lower", weighted = NULL, diag = FALSE,
                                   add.colnames = NULL, add.rownames = NA)

degree(mynet2)
betweenness(mynet, directed=FALSE)
centr_betw(mynet)
closeness(mynet)
vertex_connectivity(mynet)
diameter(mynet)

dismat <- daisy(na.omit(ap.kc.s15.pre), metric = "gower", type = list(asymm=c(1:12)),
                stand = FALSE)

dismat2=dist(na.omit(ap.kc.s15.pre), method = "binary", diag = FALSE, upper = FALSE)

cross.clust.akc=CrossClustering(dismat2, k.w.min = 2, k.w.max = 6,
                                k.c.max = 7, out = TRUE)
cross.clust.akc$Cluster.list

#Examine the people in the clusters
na.omit(ap.kc.s15.pre)[unlist(cross.clust.akc$Cluster.list[1]),]
na.omit(ap.kc.s15.pre)[unlist(cross.clust.akc$Cluster.list[2]),]
na.omit(ap.kc.s15.pre)[unlist(cross.clust.akc$Cluster.list[3]),]
na.omit(ap.kc.s15.pre)[-unlist(cross.clust.akc$Cluster.list),]





##Just Spring15.  Animal and Plant Naive Ideas##
ap.ni.s15.pre=cbind(a.ni.s15.pre,p.ni.s15.pre)
colnames(ap.ni.s15.pre)=c("Ada","Need","Use","Ada","Need","Use")

#animal.s15.pre[,7:9]=1-animal.s15.pre[,7:9]   #for reverse coding reasons

#Create co-occurrence frequency table#
n=length(ap.ni.s15.pre[,1])
mymat=matrix(0,nrow=6,ncol=6)
for(k in 1:n){
  for(i in 1:6){
    for(j in 1:6){  
      if (sum(ap.ni.s15.pre[k,i],ap.ni.s15.pre[k,j],na.rm=T)==2){
        mymat[i,j]=mymat[i,j]+1
      }
    }
  }
}
colnames(mymat)=c("Ada","Need","Use","Ada","Need","Use")
rownames(mymat)=colnames(mymat)

mynet=graph_from_adjacency_matrix(mymat, mode = "lower", weighted = NULL, diag = TRUE,
                                  add.colnames = NULL, add.rownames = NA)
mynet2=graph_from_adjacency_matrix(mymat, mode = "lower", weighted = NULL, diag = FALSE,
                                   add.colnames = NULL, add.rownames = NA)

degree(mynet2)
betweenness(mynet, directed=FALSE)
centr_betw(mynet)
closeness(mynet)
vertex_connectivity(mynet)
diameter(mynet)

dismat <- daisy(na.omit(ap.ni.s15.pre), metric = "gower", type = list(asymm=c(1:6)),
                stand = FALSE)

dismat2=dist(na.omit(ap.ni.s15.pre), method = "binary", diag = FALSE, upper = FALSE)

cross.clust.akc=CrossClustering(dismat2, k.w.min = 2, k.w.max = 6,
                                k.c.max = 7, out = TRUE)
cross.clust.akc$Cluster.list

#Examine the people in the clusters
na.omit(ap.ni.s15.pre)[unlist(cross.clust.akc$Cluster.list[1]),]
na.omit(ap.ni.s15.pre)[unlist(cross.clust.akc$Cluster.list[2]),]
na.omit(ap.ni.s15.pre)[unlist(cross.clust.akc$Cluster.list[3]),]
na.omit(ap.ni.s15.pre)[-unlist(cross.clust.akc$Cluster.list),]






########Newest Network Attempt########
##This document uses KC only network.  document 2.0 uses KC NI combined
#Here I will use closeness as the measure of interest.
#I will try it for individuals as well as aggregation over the whole class

library(igraph)
library(GraphAlignment)

#Create co-occurrence frequency table# ##Code further below calculated Harmonic and Closeness
n=length(a.kc.s15.pre[,1])
mymat=matrix(0,nrow=6,ncol=6)
for(k in 1:n){
  for(i in 1:6){
    for(j in 1:6){  
      if (sum(a.kc.s15.pre[k,i],a.kc.s15.pre[k,j],na.rm=T)==2){
        mymat[i,j]=mymat[i,j]+1
      }
    }
  }
}
colnames(mymat)=c("Var","Her","Comp","Ltd","Diff","Non")
rownames(mymat)=colnames(mymat)

mynet=graph_from_adjacency_matrix(mymat, mode = "lower", weighted = NULL, diag = TRUE,
                                  add.colnames = NULL, add.rownames = NA)
mynet2=graph_from_adjacency_matrix(mymat, mode = "lower", weighted = NULL, diag = FALSE,
                                   add.colnames = NULL, add.rownames = NA) #No diagonal
mynet3=graph_from_adjacency_matrix(mymat, mode = "lower", weighted = T, diag = TRUE,
                                   add.colnames = NULL, add.rownames = NA) #Weighted
mynet4=graph_from_adjacency_matrix(mymat, mode = "lower", weighted = T, diag = FALSE,
                                   add.colnames = NULL, add.rownames = NA) #Weighted, no diag
mynet5=graph_from_adjacency_matrix(1/mymat, mode = "lower", weighted = T, diag = TRUE,
                                   add.colnames = NULL, add.rownames = NA) #Weighted, diag
mynet6=graph_from_adjacency_matrix(1/mymat, mode = "lower", weighted = T, diag = FALSE,
                                   add.colnames = NULL, add.rownames = NA) #Weighted, no diag


closeness(mynet)
shortest.paths(mynet)
mean_distance(mynet) #Average distance of shortest paths
plot(mynet,layout=layout_in_circle(mynet))





###Playing around with it###
#The best way is 1/freq#
#1)All students use all concepts
x=matrix(1,nrow=6,ncol=6)
y=2*x
z=1/y
w=1/(8*x)   #I conclude inverse co-occurrence as best distance matrix

#2)All students use same 2 concepts
x=matrix(0,nrow=6,ncol=6)
x[2,1]=10 ; x[1,2]=10
y=1/x

#3)All students use concept 1, half use concept 2, half use concept 3
x=matrix(0,nrow=6,ncol=6)
x[2,1]=10 ; x[1,2]=10 ; x[3,1]=10 ; x[1,3]=10 ; x[3,2]=10 ; x[2,3]=10
y=1/x
z=2/x

#4)All 10 students use 3 concepts
   #This is equivalent to many scenarios.  need to figure out scaling
   #This is same as 10 only using 1,2; 10 only using 1,3; 10 only using 2,3: 30 students
x=matrix(0,nrow=6,ncol=6)
x[2,1]=10 ; x[1,2]=10 ; x[3,1]=10 ; x[1,3]=10 ; x[3,2]=10 ; x[2,3]=10

#5)All 10 students use all 6 concepts
    #This is equivalent to many scenarios (look at matrix).  need to figure out scaling
x=matrix(10,nrow=6,ncol=6)
diag(x)=0
y=1/x

net=graph_from_adjacency_matrix(z, mode = "lower", weighted = NULL, diag = TRUE,
                                  add.colnames = NULL, add.rownames = NA)
net2=graph_from_adjacency_matrix(z, mode = "lower", weighted = NULL, diag = FALSE,
                                   add.colnames = NULL, add.rownames = NA) #No diagonal
net3=graph_from_adjacency_matrix(z, mode = "lower", weighted = T, diag = TRUE,
                                   add.colnames = NULL, add.rownames = NA) #Weighted
net4=graph_from_adjacency_matrix(z, mode = "lower", weighted = T, diag = FALSE,
                                   add.colnames = NULL, add.rownames = NA) #Weighted, no diag
closeness(net)
betweenness(net)
shortest.paths(net)






###########START HERE FOR HARMONIC AND CLOSENESS CENTRALITYT MEASURE STUFF############ THESIS USES Doc 2.0

#Create function to calculate harmonic closeness
harmonic=function(mat){
  net=graph_from_adjacency_matrix(1/mat, mode = "lower", weighted = T, diag = FALSE,
                              add.colnames = NULL, add.rownames = NA)
  dij=shortest.paths(net)
  diag(dij)=Inf
  rowSums(1/dij)
}


#Create function to calculate closeness centrality
closecent=function(mat){
  net=graph_from_adjacency_matrix(1/mat, mode = "lower", weighted = T, diag = FALSE,
                                  add.colnames = NULL, add.rownames = NA)
  closeness(net)
}



########Loop to create co-occurrence matrices, and matrices for Harmonic & Closeness Centrality
#Pre
mymat=matrix(0,nrow=6,ncol=6); mymat2=mymat; mymat3=mymat; mymat4=mymat
sem=c(rep(1,length(cins.spring15.pre)),rep(2,length(cins.spring16.pre)),
           rep(3,length(cins.spring17.pre)),rep(4,length(cans.fall17.pre)))
M.pre=list(mymat,mymat2,mymat3,mymat4)
H.pre=matrix(0,nrow=4,ncol=6)
C.pre=matrix(0,nrow=4,ncol=6)
for(m in 1:4){
  n=length(skc0.ind[sem==m,1])
  for(k in 1:n){
    for(i in 1:6){
      for(j in 1:6){  
        if (sum(skc0.ind[sem==m,][k,i],skc0.ind[sem==m,][k,j],na.rm=T)==2){
          M.pre[[m]][i,j]=M.pre[[m]][i,j]+1
        }
      }
    }
  }
  colnames(M.pre[[m]])=c("Var","Her","Comp","Ltd","Diff","Non")
  rownames(M.pre[[m]])=colnames(M.pre[[m]])
  H.pre[m,]=harmonic(M.pre[[m]])/(length(na.omit(skc0.ind[sem==m,1]))*5)
  C.pre[m,]=closecent(M.pre[[m]])*5/length(na.omit(skc0.ind[sem==m,1]))
}
colnames(H.pre)=c("Var","Her","Comp","Ltd","Diff","Non")
colnames(C.pre)=colnames(H.pre)
M.pre
round(H.pre,6)
round(C.pre,6)

#Post
mymat=matrix(0,nrow=6,ncol=6); mymat2=mymat; mymat3=mymat; mymat4=mymat
sem=c(rep(1,length(cins.spring15.pre)),rep(2,length(cins.spring16.pre)),
      rep(3,length(cins.spring17.pre)),rep(4,length(cans.fall17.pre)))
M.post=list(mymat,mymat2,mymat3,mymat4)
H.post=matrix(0,nrow=4,ncol=6)
C.post=matrix(0,nrow=4,ncol=6)
for(m in 1:4){
  n=length(skc1.ind[sem==m,1])
  for(k in 1:n){
    for(i in 1:6){
      for(j in 1:6){  
        if (sum(skc1.ind[sem==m,][k,i],skc1.ind[sem==m,][k,j],na.rm=T)==2){
          M.post[[m]][i,j]=M.post[[m]][i,j]+1
        }
      }
    }
  }
  colnames(M.post[[m]])=c("Var","Her","Comp","Ltd","Diff","Non")
  rownames(M.post[[m]])=colnames(M.post[[m]])
  H.post[m,]=harmonic(M.post[[m]])/(length(na.omit(skc1.ind[sem==m,1]))*5)
  C.post[m,]=closecent(M.post[[m]])*5/length(na.omit(skc1.ind[sem==m,1]))
}
colnames(H.post)=c("Var","Her","Comp","Ltd","Diff","Non")
colnames(C.post)=colnames(H.post)
M.post
round(H.post,6)
round(C.post,6)

#Delayed-Post
mymat=matrix(0,nrow=6,ncol=6); mymat2=mymat; mymat3=mymat; mymat4=mymat
sem=c(rep(1,length(cins.spring15.pre)),rep(2,length(cins.spring16.pre)),
      rep(3,length(cins.spring17.pre)),rep(4,length(cans.fall17.pre)))
M.dpost=list(mymat,mymat2,mymat3,mymat4)
H.dpost=matrix(0,nrow=4,ncol=6)
C.dpost=matrix(0,nrow=4,ncol=6)
for(m in 1:4){
  n=length(skc2.ind[sem==m,1])
  for(k in 1:n){
    for(i in 1:6){
      for(j in 1:6){  
        if (sum(skc2.ind[sem==m,][k,i],skc2.ind[sem==m,][k,j],na.rm=T)==2){
          M.dpost[[m]][i,j]=M.dpost[[m]][i,j]+1
        }
      }
    }
  }
  colnames(M.dpost[[m]])=c("Var","Her","Comp","Ltd","Diff","Non")
  rownames(M.dpost[[m]])=colnames(M.dpost[[m]])
  H.dpost[m,]=harmonic(M.dpost[[m]])/(length(na.omit(skc2.ind[sem==m,1]))*5)
  C.dpost[m,]=closecent(M.dpost[[m]])*5/length(na.omit(skc2.ind[sem==m,1]))
}
colnames(H.dpost)=c("Var","Her","Comp","Ltd","Diff","Non")
colnames(C.dpost)=colnames(H.dpost)
M.dpost
round(H.dpost,6)
round(C.dpost,6)


#run these to avoid extra code for plant.  this also assigns animal values to M,H,C
skc0.ind=rkc0.ind ; skc1.ind=rkc1.ind ; skc2.ind=rkc2.ind
H.pre.a=H.pre ; H.post.a=H.post ; H.dpost.a=H.dpost
C.pre.a=C.pre ; C.post.a=C.post ; C.dpost.a=C.dpost
M.pre.a=M.pre ; M.post.a=M.post ; M.dpost.a=M.dpost





#######Loop to create co-occurrence matrices at the individual level
#####And matrices for Harmonic Centrality.  No Closeness Centrality because disconnected
#Pre
sem=c(rep(1,length(cins.spring15.pre)),rep(2,length(cins.spring16.pre)),
      rep(3,length(cins.spring17.pre)),rep(4,length(cans.fall17.pre)))
n=length(skc0.ind[,1])
H.all.pre=matrix(0,nrow=n,ncol=6)
for(k in 1:n){
  mat.ind=matrix(0,nrow=6,ncol=6)
  for(i in 1:6){
    for(j in 1:6){  
      if (sum(skc0.ind[k,i],skc0.ind[k,j],na.rm=T)==2){
        mat.ind[i,j]=mat.ind[i,j]+1
      }
    }
  }
  H.all.pre[k,]=harmonic(mat.ind)/5
}
colnames(H.all.pre)=c("Var","Her","Comp","Ltd","Diff","Non")

H.sem.pre=matrix(0,nrow=4,ncol=7)
for(i in 1:4){
H.sem.pre[i,1:6]=colMeans(H.all.pre[sem==i,])
}
H.sem.pre[,7]=rowMeans(H.sem.pre)
round(H.sem.pre,6)

#Post
sem=c(rep(1,length(cins.spring15.pre)),rep(2,length(cins.spring16.pre)),
      rep(3,length(cins.spring17.pre)),rep(4,length(cans.fall17.pre)))
n=length(skc1.ind[,1])
H.all.post=matrix(0,nrow=n,ncol=6)
for(k in 1:n){
  mat.ind=matrix(0,nrow=6,ncol=6)
  for(i in 1:6){
    for(j in 1:6){  
      if (sum(skc1.ind[k,i],skc1.ind[k,j],na.rm=T)==2){
        mat.ind[i,j]=mat.ind[i,j]+1
      }
    }
  }
  H.all.post[k,]=harmonic(mat.ind)/5
}
colnames(H.all.post)=c("Var","Her","Comp","Ltd","Diff","Non")

H.sem.post=matrix(0,nrow=4,ncol=7)
for(i in 1:4){
  H.sem.post[i,1:6]=colMeans(H.all.post[sem==i,])
}
H.sem.post[,7]=rowMeans(H.sem.post)
round(H.sem.post,6)

#Delayed-Post
sem=c(rep(1,length(cins.spring15.post)),rep(2,length(cins.spring16.post)),
      rep(3,length(cins.spring17.post)),rep(4,length(cans.fall17.post)))
n=length(skc2.ind[,1])
H.all.dpost=matrix(0,nrow=n,ncol=6)
for(k in 1:n){
  mat.ind=matrix(0,nrow=6,ncol=6)
  for(i in 1:6){
    for(j in 1:6){  
      if (sum(skc2.ind[k,i],skc2.ind[k,j],na.rm=T)==2){
        mat.ind[i,j]=mat.ind[i,j]+1
      }
    }
  }
  H.all.dpost[k,]=harmonic(mat.ind)/5
}
colnames(H.all.dpost)=c("Var","Her","Comp","Ltd","Diff","Non")

H.sem.dpost=matrix(0,nrow=4,ncol=7)
for(i in 1:4){
  H.sem.dpost[i,1:6]=colMeans(H.all.dpost[sem==i,])
}
H.sem.dpost[,7]=rowMeans(H.sem.dpost)
round(H.sem.dpost,6)

#run these to avoid extra code for plant.  this also assigns animal values to M,H,C
skc0.ind=rkc0.ind ; skc1.ind=rkc1.ind ; skc2.ind=rkc2.ind
H.sem.pre.a=H.sem.pre ; H.sem.post.a=H.sem.post ; H.sem.dpost.a=H.sem.dpost
H.all.pre.a=H.all.pre ; H.all.post.a=H.all.post ; H.all.dpost.a=H.all.dpost




##Compare Node Centrality Ordering by Semester##
for(i in 1:4){
  print(sort(H.dpost.a[i,],decreasing = T))
}



##Use the individual student's centralities to compare semesters##
colnames(H.all.pre.a)=c("Var","Her","Comp","Ltd","Diff","Non")
colnames(H.all.post.a)=colnames(H.all.pre.a);  colnames(H.all.dpost.a)=colnames(H.all.pre.a)

m1=lm(cbind(H.all.pre,H.all.pre.a)~semester)
anova(m1)
summary(m1)

m2=lm(cbind(H.all.post,H.all.post.a)~cbind(H.all.pre,H.all.pre.a)+semester)
anova(m2)
summary(m2)

m3=lm(cbind(H.all.dpost,H.all.dpost.a)~cbind(H.all.pre,H.all.pre.a)+semester)
anova(m3)
summary(m3)

m1.1=lm(rowSums(H.all.pre+H.all.pre.a)/2~semester)
anova(m1.1)
m2.1=lm(rowSums(H.all.post+H.all.post.a)/2~rowSums(H.all.pre)+semester)
anova(m2.1)
summary(m2.1)

summary(m1.1)

m3.1=lm(rowSums(H.all.dpost+H.all.dpost.a)/2~rowSums(H.all.pre)+semester)
anova(m3.1)
summary(m3.1)



######Compute Jaccard and Simple Matching Similarities between the Animal/Plant networks
##Remember to get rid of rounding to go actual statistical tests

#Pre
mymat=matrix(0,nrow=6,ncol=6); 
rownames(mymat)=c("Var","Her","Comp","Ltd","Diff","Non"); colnames(mymat)=rownames(mymat)
M00.pre=list(mymat,mymat,mymat,mymat); M01.pre=M00.pre; M10.pre=M00.pre; M11.pre=M00.pre
for(m in 1:4){
  n=length(skc0.ind[sem==m,][,1])
  for(k in 1:n){
    for(i in 1:6){
      for(j in 1:6){  
        if(sum(skc0.ind[sem==m,][k,i],skc0.ind[sem==m,][k,j],na.rm=T)==2 &&
           sum(rkc0.ind[sem==m,][k,i],rkc0.ind[sem==m,][k,j],na.rm=T)==2) {
          M11.pre[[m]][i,j]=M11.pre[[m]][i,j]+1
        }
        else if(sum(skc0.ind[sem==m,][k,i],skc0.ind[sem==m,][k,j],na.rm=T)==2 &&
                sum(rkc0.ind[sem==m,][k,i],rkc0.ind[sem==m,][k,j],na.rm=T)!=2) {
          M10.pre[[m]][i,j]=M10.pre[[m]][i,j]+1
        }
        else if(sum(skc0.ind[sem==m,][k,i],skc0.ind[sem==m,][k,j],na.rm=T)!=2 &&
                sum(rkc0.ind[sem==m,][k,i],rkc0.ind[sem==m,][k,j],na.rm=T)==2) {
          M01.pre[[m]][i,j]=M01.pre[[m]][i,j]+1
        }
        else if(sum(skc0.ind[sem==m,][k,i],skc0.ind[sem==m,][k,j],na.rm=T)!=2 &&
                sum(rkc0.ind[sem==m,][k,i],rkc0.ind[sem==m,][k,j],na.rm=T)!=2) {
          M00.pre[[m]][i,j]=M00.pre[[m]][i,j]+1
        }
      }
    }
  }
}
Jacc.pre=list(mymat,mymat,mymat,mymat)
Simp.pre=list(mymat,mymat,mymat,mymat)
for(m in 1:4){
Jacc.pre[[m]]=round(M11.pre[[m]]/(M01.pre[[m]]+M10.pre[[m]]+M11.pre[[m]]),4)
Simp.pre[[m]]=round((M00.pre[[m]]+M11.pre[[m]])/(M00.pre[[m]]+M01.pre[[m]]+M10.pre[[m]]+M11.pre[[m]]),4)
}
Jacc.pre
Simp.pre

#Post
mymat=matrix(0,nrow=6,ncol=6); 
rownames(mymat)=c("Var","Her","Comp","Ltd","Diff","Non"); colnames(mymat)=rownames(mymat)
M00.post=list(mymat,mymat,mymat,mymat); M01.post=M00.post; M10.post=M00.post; M11.post=M00.post
for(m in 1:4){
  n=length(skc1.ind[sem==m,][,1])
  for(k in 1:n){
    for(i in 1:6){
      for(j in 1:6){  
        if(sum(skc1.ind[sem==m,][k,i],skc1.ind[sem==m,][k,j],na.rm=T)==2 &&
           sum(rkc1.ind[sem==m,][k,i],rkc1.ind[sem==m,][k,j],na.rm=T)==2) {
          M11.post[[m]][i,j]=M11.post[[m]][i,j]+1
        }
        else if(sum(skc1.ind[sem==m,][k,i],skc1.ind[sem==m,][k,j],na.rm=T)==2 &&
                sum(rkc1.ind[sem==m,][k,i],rkc1.ind[sem==m,][k,j],na.rm=T)!=2) {
          M10.post[[m]][i,j]=M10.post[[m]][i,j]+1
        }
        else if(sum(skc1.ind[sem==m,][k,i],skc1.ind[sem==m,][k,j],na.rm=T)!=2 &&
                sum(rkc1.ind[sem==m,][k,i],rkc1.ind[sem==m,][k,j],na.rm=T)==2) {
          M01.post[[m]][i,j]=M01.post[[m]][i,j]+1
        }
        else if(sum(skc1.ind[sem==m,][k,i],skc1.ind[sem==m,][k,j],na.rm=T)!=2 &&
                sum(rkc1.ind[sem==m,][k,i],rkc1.ind[sem==m,][k,j],na.rm=T)!=2) {
          M00.post[[m]][i,j]=M00.post[[m]][i,j]+1
        }
      }
    }
  }
}
Jacc.post=list(mymat,mymat,mymat,mymat)
Simp.post=list(mymat,mymat,mymat,mymat)
for(m in 1:4){
  Jacc.post[[m]]=round(M11.post[[m]]/(M01.post[[m]]+M10.post[[m]]+M11.post[[m]]),4)
  Simp.post[[m]]=round((M00.post[[m]]+M11.post[[m]])/(M00.post[[m]]+M01.post[[m]]+M10.post[[m]]+M11.post[[m]]),4)
}
Jacc.post
Simp.post

#D-Post
mymat=matrix(0,nrow=6,ncol=6); 
rownames(mymat)=c("Var","Her","Comp","Ltd","Diff","Non"); colnames(mymat)=rownames(mymat)
M00.dpost=list(mymat,mymat,mymat,mymat); M01.dpost=M00.dpost; M10.dpost=M00.dpost; M11.dpost=M00.dpost
for(m in 1:4){
  n=length(skc2.ind[sem==m,][,1])
  for(k in 1:n){
    for(i in 1:6){
      for(j in 1:6){  
        if(sum(skc2.ind[sem==m,][k,i],skc2.ind[sem==m,][k,j],na.rm=T)==2 &&
           sum(rkc2.ind[sem==m,][k,i],rkc2.ind[sem==m,][k,j],na.rm=T)==2) {
          M11.dpost[[m]][i,j]=M11.dpost[[m]][i,j]+1
        }
        else if(sum(skc2.ind[sem==m,][k,i],skc2.ind[sem==m,][k,j],na.rm=T)==2 &&
                sum(rkc2.ind[sem==m,][k,i],rkc2.ind[sem==m,][k,j],na.rm=T)!=2) {
          M10.dpost[[m]][i,j]=M10.dpost[[m]][i,j]+1
        }
        else if(sum(skc2.ind[sem==m,][k,i],skc2.ind[sem==m,][k,j],na.rm=T)!=2 &&
                sum(rkc2.ind[sem==m,][k,i],rkc2.ind[sem==m,][k,j],na.rm=T)==2) {
          M01.dpost[[m]][i,j]=M01.dpost[[m]][i,j]+1
        }
        else if(sum(skc2.ind[sem==m,][k,i],skc2.ind[sem==m,][k,j],na.rm=T)!=2 &&
                sum(rkc2.ind[sem==m,][k,i],rkc2.ind[sem==m,][k,j],na.rm=T)!=2) {
          M00.dpost[[m]][i,j]=M00.dpost[[m]][i,j]+1
        }
      }
    }
  }
}
Jacc.dpost=list(mymat,mymat,mymat,mymat)
Simp.dpost=list(mymat,mymat,mymat,mymat)
for(m in 1:4){
  Jacc.dpost[[m]]=round(M11.dpost[[m]]/(M01.dpost[[m]]+M10.dpost[[m]]+M11.dpost[[m]]),4)
  Simp.dpost[[m]]=round((M00.dpost[[m]]+M11.dpost[[m]])/(M00.dpost[[m]]+M01.dpost[[m]]+M10.dpost[[m]]+M11.dpost[[m]]),4)
}
Jacc.dpost
Simp.dpost


##List of all of them##
Jacc=list(Jacc.pre,Jacc.post,Jacc.dpost)
Simp=list(Simp.pre,Simp.post,Simp.dpost)
M00=list(M00.pre,M00.post,M00.dpost)
M01=list(M01.pre,M01.post,M01.dpost)
M10=list(M10.pre,M10.post,M10.dpost)
M11=list(M11.pre,M11.post,M11.dpost)


####Output the means of Node similarities####
nodeJacc=matrix(0,nrow=4,ncol=3); nodeSimp=nodeJacc
for(w in 1:3){
  for(m in 1:4){
    nodeJacc[m,w]=mean(diag(Jacc[[w]][[m]]))
  }
}
for(w in 1:3){
  for(m in 1:4){
    nodeSimp[m,w]=mean(diag(Simp[[w]][[m]]))
  }
}

####Output the means of Edge similarities####
edgeJacc=matrix(0,nrow=4,ncol=3); edgeSimp=edgeJacc
for(w in 1:3){
  for(m in 1:4){
    nodeJacc[m,w]=mean(Jacc[[w]][[m]][lower.tri(mymat)],na.rm=T)
  }
}
for(w in 1:3){
  for(m in 1:4){
    nodeSimp[m,w]=mean(Simp[[w]][[m]][lower.tri(mymat)],na.rm=T)
  }
}


##Count the overall M00, M10, M01, M11 for entire network, compute Similarities##
M00.node=matrix(0,nrow=4,ncol=3); M01.node=M00.node; M10.node=M00.node; M11.node=M00.node
for(w in 1:3){
  for(m in 1:4){
    M00.node[m,w]=sum(diag(M00[[w]][[m]]))
    M01.node[m,w]=sum(diag(M01[[w]][[m]]))
    M10.node[m,w]=sum(diag(M10[[w]][[m]]))
    M11.node[m,w]=sum(diag(M11[[w]][[m]]))
  }
}
M11.node/(M01.node+M10.node+M11.node) ##Jaccard Similarity
(M00.node+M11.node)/(M00.node+M01.node+M10.node+M11.node) ##Simple Matching Similarity

M00.edge=matrix(0,nrow=4,ncol=3); M01.edge=M00.edge; M10.edge=M00.edge; M11.edge=M00.edge
for(w in 1:3){
  for(m in 1:4){
    M00.edge[m,w]=sum(M00[[w]][[m]][lower.tri(mymat)])
    M01.edge[m,w]=sum(M01[[w]][[m]][lower.tri(mymat)])
    M10.edge[m,w]=sum(M10[[w]][[m]][lower.tri(mymat)])
    M11.edge[m,w]=sum(M11[[w]][[m]][lower.tri(mymat)])
  }
}
M11.edge/(M01.edge+M10.edge+M11.edge) ##Jaccard Similarity
(M00.edge+M11.edge)/(M00.edge+M01.edge+M10.edge+M11.edge) ##Simple Matching Similarity





#####Calculate Euclidean Distance between Plant/Animal Co-Occurrence Matrices#####
M.p=list(M.pre,M.post,M.dpost);  M.a=list(M.pre.a,M.post.a,M.dpost.a)
euc_dist_withnode <- function(p,q){
  sqrt(sum((p[!upper.tri(mymat)] - q[!upper.tri(mymat)])^2))
}
euc_dist_nonode <- function(p,q){
  sqrt(sum((p[lower.tri(mymat)] - q[lower.tri(mymat)])^2))
}
Euc.1=matrix(0,nrow=4,ncol=3)
Euc.2=Euc.1
for(w in 1:3){
  for(m in 1:4){
    Euc.1[m,w]=euc_dist_withnode(M.a[[w]][[m]]/length(na.omit(skc.all.ind[time==w-1,1][sem==m])),
                                 M.p[[w]][[m]]/length(na.omit(rkc.all.ind[time==w-1,1][sem==m])))
    Euc.2[m,w]=euc_dist_nonode(M.a[[w]][[m]]/length(na.omit(skc.all.ind[time==w-1,1][sem==m])),
                               M.p[[w]][[m]]/length(na.omit(rkc.all.ind[time==w-1,1][sem==m])))
  }
}

#first index time, 2nd index semester




###########PLOT THE NETWORKS FROM CO-OCCURRENCE MATRICES############
par(mfrow=c(1,3))

offdiagsums=function(A){
  rowSums(A)-diag(A)
}

#Animal
n1.1=graph_from_adjacency_matrix(M.pre.a[[1]],weighted = T, mode='lower',diag=F)
plot(n1.1,vertex.label.color="black", layout=layout_in_circle(n1.1), edge.width=E(n1.1)$weight/10,
     vertex.size=offdiagsums(M.pre.a[[1]])/10)
title("Pre")
#add edge.label=E(n1)$weight to put numbers on edges

n2.1=graph_from_adjacency_matrix(M.post.a[[1]],weighted = T, mode='lower',diag=F)
plot(n2.1,vertex.label.color="black", layout=layout_in_circle(n2.1), edge.width=E(n2.1)$weight/10,
     vertex.size=offdiagsums(M.post.a[[1]])/10)
title("Post")

n3.1=graph_from_adjacency_matrix(M.dpost.a[[1]],weighted = T, mode='lower',diag=F)
plot(n3.1,vertex.label.color="black", layout=layout_in_circle(n3.1), edge.width=E(n3.1)$weight/10,
     vertex.size=offdiagsums(M.pre.a[[1]])/10)
title("D-Post")


n1.2=graph_from_adjacency_matrix(M.pre.a[[2]],weighted = T, mode='lower',diag=F)
plot(n3.1,vertex.label.color="black", layout=layout_in_circle(n1.2), edge.width=E(n1.2)$weight/10,
     vertex.size=offdiagsums(M.pre.a[[2]])/10)
#title("Pre")

n2.2=graph_from_adjacency_matrix(M.post.a[[2]],weighted = T, mode='lower',diag=F)
plot(n1,vertex.label.color="black", layout=layout_in_circle(n2.2), edge.width=E(n2.2)$weight/10,
     vertex.size=offdiagsums(M.post.a[[2]])/10)
#title("Post")

n3.2=graph_from_adjacency_matrix(M.dpost.a[[2]],weighted = T, mode='lower',diag=F)
plot(n1,vertex.label.color="black", layout=layout_in_circle(n3.2), edge.width=E(n3.2)$weight/10,
     vertex.size=offdiagsums(M.pre.a[[2]])/10)
#title("D-Post")


n1.3=graph_from_adjacency_matrix(M.pre.a[[3]],weighted = T, mode='lower',diag=F)
plot(n1.3,vertex.label.color="black", layout=layout_in_circle(n1.3), edge.width=E(n1.3)$weight/10,
     vertex.size=offdiagsums(M.pre.a[[3]])/10)
#title("Pre")

n2.3=graph_from_adjacency_matrix(M.post.a[[3]],weighted = T, mode='lower',diag=F)
plot(n2.3,vertex.label.color="black", layout=layout_in_circle(n2.3), edge.width=E(n2.3)$weight/10,
     vertex.size=offdiagsums(M.post.a[[3]])/10)
#title("Post")

n3.3=graph_from_adjacency_matrix(M.dpost.a[[3]],weighted = T, mode='lower',diag=F)
plot(n3.3,vertex.label.color="black", layout=layout_in_circle(n3.3), edge.width=E(n3.3)$weight/10,
     vertex.size=offdiagsums(M.pre.a[[3]])/10)
#title("D-Post")


n1.4=graph_from_adjacency_matrix(M.pre.a[[4]],weighted = T, mode='lower',diag=F)
plot(n1.4,vertex.label.color="black", layout=layout_in_circle(n1.4), edge.width=E(n1.4)$weight/10,
     vertex.size=offdiagsums(M.pre.a[[4]])/10)
#title("Pre")

n2.4=graph_from_adjacency_matrix(M.post.a[[4]],weighted = T, mode='lower',diag=F)
plot(n2.4,vertex.label.color="black", layout=layout_in_circle(n2.4), edge.width=E(n2.4)$weight/10,
     vertex.size=offdiagsums(M.post.a[[4]])/10)
#title("Post")

n3.4=graph_from_adjacency_matrix(M.dpost.a[[4]],weighted = T, mode='lower',diag=F)
plot(n3.4,vertex.label.color="black", layout=layout_in_circle(n3.4), edge.width=E(n3.4)$weight/10,
     vertex.size=offdiagsums(M.pre.a[[4]])/10)
#title("D-Post")


#Plant
n1.1=graph_from_adjacency_matrix(M.pre[[1]],weighted = T, mode='lower',diag=F)
plot(n1,vertex.label.color="black", layout=layout_in_circle(n1.1), edge.width=E(n1.1)$weight/10,
     vertex.size=offdiagsums(M.pre[[1]])/10)
title("Pre")
#add edge.label=E(n1)$weight to put numbers on edges

n2.1=graph_from_adjacency_matrix(M.post[[1]],weighted = T, mode='lower',diag=F)
plot(n1,vertex.label.color="black", layout=layout_in_circle(n1.1), edge.width=E(n2.1)$weight/10,
     vertex.size=offdiagsums(M.post[[1]])/10)
title("Post")

n3.1=graph_from_adjacency_matrix(M.dpost[[1]],weighted = T, mode='lower',diag=F)
plot(n1,vertex.label.color="black", layout=layout_in_circle(n1.1), edge.width=E(n3.1)$weight/10,
     vertex.size=offdiagsums(M.pre[[1]])/10)
title("D-Post")


n1.2=graph_from_adjacency_matrix(M.pre[[2]],weighted = T, mode='lower',diag=F)
plot(n1,vertex.label.color="black", layout=layout_in_circle(n1.2), edge.width=E(n1.2)$weight/10,
     vertex.size=offdiagsums(M.pre[[2]])/10)
#title("Pre")

n2.2=graph_from_adjacency_matrix(M.post[[2]],weighted = T, mode='lower',diag=F)
plot(n1,vertex.label.color="black", layout=layout_in_circle(n1.2), edge.width=E(n2.2)$weight/10,
     vertex.size=offdiagsums(M.post[[2]])/10)
#title("Post")

n3.2=graph_from_adjacency_matrix(M.dpost[[2]],weighted = T, mode='lower',diag=F)
plot(n1,vertex.label.color="black", layout=layout_in_circle(n1.2), edge.width=E(n3.2)$weight/10,
     vertex.size=offdiagsums(M.pre[[2]])/10)
#title("D-Post")


n1.3=graph_from_adjacency_matrix(M.pre[[3]],weighted = T, mode='lower',diag=F)
plot(n1,vertex.label.color="black", layout=layout_in_circle(n1.3), edge.width=E(n1.3)$weight/10,
     vertex.size=offdiagsums(M.pre[[3]])/10)
#title("Pre")

n2.3=graph_from_adjacency_matrix(M.post[[3]],weighted = T, mode='lower',diag=F)
plot(n1,vertex.label.color="black", layout=layout_in_circle(n1.3), edge.width=E(n2.3)$weight/10,
     vertex.size=offdiagsums(M.post[[3]])/10)
#title("Post")

n3.3=graph_from_adjacency_matrix(M.dpost[[3]],weighted = T, mode='lower',diag=F)
plot(n1,vertex.label.color="black", layout=layout_in_circle(n1.3), edge.width=E(n3.3)$weight/10,
     vertex.size=offdiagsums(M.pre[[3]])/10)
#title("D-Post")


n1.4=graph_from_adjacency_matrix(M.pre[[4]],weighted = T, mode='lower',diag=F)
plot(n1,vertex.label.color="black", layout=layout_in_circle(n1.4), edge.width=E(n1.4)$weight/10,
     vertex.size=offdiagsums(M.pre[[4]])/10)
#title("Pre")

n2.4=graph_from_adjacency_matrix(M.post[[4]],weighted = T, mode='lower',diag=F)
plot(n1,vertex.label.color="black", layout=layout_in_circle(n1.4), edge.width=E(n2.4)$weight/10,
     vertex.size=offdiagsums(M.post[[4]])/10)
#title("Post")

n3.4=graph_from_adjacency_matrix(M.dpost[[4]],weighted = T, mode='lower',diag=F)
plot(n1,vertex.label.color="black", layout=layout_in_circle(n1.4), edge.width=E(n3.4)$weight/10,
     vertex.size=offdiagsums(M.pre[[4]])/10)
#title("D-Post")









#plot(n1,vertex.label.color="black",vertex.color=my_color,layout=layout_in_circle(n1),
 #    vertex.size=25,edge.width=ifelse(abs(E(n1)$weight)<0.3,1,ifelse(abs(E(n1)$weight)<0.5,3,6)))
#title("CG",line=-10)






######Network Similarity#######
#I have not used the following stuff
library(GraphAlignment)

### convert the adjacency matrices into an igraph object using graph.adjacency functio
g.pre.a1 <- graph.adjacency(M.pre.a[[1]], mode="undirected", weighted=NULL, diag=TRUE) #T or F?
g.pre.p1 <- graph.adjacency(M.pre[[1]], mode="undirected", weighted=NULL, diag=TRUE) #mode=lower


### Compute graph similarity (intersection of vertices and edges between the graphs)
g.pre.sim <- graph.intersection(g.pre.a1,g.pre.p1, byname = "auto", keep.all.vertices = TRUE) #T or F?


### Compute adjacency matrix of the g.sim graph
adj.sim <- get.adjacency(g.pre.sim, type="both", sparse=F)







####CODE FROM OTHER DOCUMENT####

#Numbers for Animal/Plant KC/NI, to make nodes different colors#
mycolor=c(rep(1,6),rep(2,6),rep(3,3),rep(4,3))
library(RColorBrewer)
#coul = brewer.pal(nlevels(as.factor(mycolor)), "Set2")
coul = c("lightsalmon","seagreen3","pink3","burlywood3")
my_color=coul[as.numeric(as.factor(mycolor))]

#Vector to reorder the A/P KC/NI to put same concepts across 180º#
x=c(1,2,3,4,5,6,13,14,15,7,8,9,10,11,12,16,17,18)
my_color=my_color[x]


plot(n1,vertex.label.color="black",vertex.color=my_color,layout=layout_in_circle(n1),
     vertex.size=25,edge.width=ifelse(abs(E(n1)$weight)<0.3,1,ifelse(abs(E(n1)$weight)<0.5,3,6)))
title("CG",line=-10)