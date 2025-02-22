##First run all 4 semester stuff##

####RACE DIFFERENCES####

#Post~Pre models for AKC PKC ANI PNI      PART 1
#Mixed Models for ALLKC ALLNI             PART 2


#Run this rather than rewrite a lot of code, to do post-test analysis instead of delayed-post
#skc.post=skc.ex1
#rkc.post=rkc.ex1
#snaive.post=snaive.ex1
#rnaive.post=rnaive.ex1
#race=relevel(race,2)

#Animal KC
r0=lm(skc.post~skc.pre+semester+esl+reading+writing+bioclass+gender+age+race+level+plan+cins.pre+isea.pre)
anova(r0)
summary(r0)   #Model with no interaction

r0.1=lm(skc.post~skc.pre+relevel(factor(semester),3)+esl+reading+writing+bioclass+gender+age+race+level+plan+cins.pre+isea.pre)
anova(r0.1)
summary(r0.1) #Model with no interaction, change reference group

r1=lm(skc.post~skc.pre+semester+esl+reading+writing+bioclass+gender+age+race*semester+level+plan+cins.pre+isea.pre)
anova(r1)
summary(r1)   #Model with interaction.  I dont think this is the best way

r1.1=lm(skc.post~skc.pre+relevel(factor(semester),3)+esl+reading+writing+bioclass+gender+age+
          race*relevel(factor(semester),3)+level+plan+cins.pre+isea.pre)
anova(r1.1)
summary(r1.1) #Model with interaction, change semester reference group.  Not the best way

r1.2=lm(skc.post~skc.pre+esl+reading+writing+bioclass+gender+age+relevel(race,2)*semester+level+plan+cins.pre+isea.pre)
anova(r1.2)
summary(r1.2) #Model with interaction, change race reference group.  Not the best way


r1.3=lm(skc.post~skc.pre+esl+reading+writing+bioclass+gender+age+race+race:semester+level+plan+cins.pre+isea.pre)
anova(r1.3)
summary(r1.3)  #Race term and interaction term, no regular semester term.  Compares semesters by race

r1.4=lm(skc.post~skc.pre+esl+reading+writing+bioclass+gender+age+race+
          race:relevel(factor(semester),2)+level+plan+cins.pre+isea.pre)
anova(r1.4)    #Race term and interaction term, no regular semester term.
summary(r1.4)  #Compares semesters by race.  Changes reference group


r1.5=lm(skc.post~skc.pre+esl+reading+writing+bioclass+gender+age+semester+race:semester+level+plan+cins.pre+isea.pre)
anova(r1.5)
summary(r1.5)  #Semester term and interaction term, no regular race term.  Compares races by semester

r1.6=lm(skc.post~skc.pre+esl+reading+writing+bioclass+gender+age+semester+
          relevel(race,2):semester+level+plan+cins.pre+isea.pre)
anova(r1.6)    #Semester term and interaction term, no regular race term.
summary(r1.6)  #Compares races by semester. Changes reference group



#Plant KC
r0=lm(rkc.post~rkc.pre+semester+esl+reading+writing+bioclass+gender+age+race+level+plan+cins.pre+isea.pre)
anova(r0)
summary(r0)   #Model with no interaction

r0.1=lm(rkc.post~rkc.pre+relevel(factor(semester),3)+esl+reading+writing+bioclass+gender+age+race+level+plan+cins.pre+isea.pre)
anova(r0.1)
summary(r0.1) #Model with no interaction, change reference group

r1=lm(rkc.post~rkc.pre+semester+esl+reading+writing+bioclass+gender+age+race*semester+level+plan+cins.pre+isea.pre)
anova(r1)
summary(r1)   #Model with interaction.  I dont think this is the best way

r1.1=lm(rkc.post~rkc.pre+relevel(factor(semester),3)+esl+reading+writing+bioclass+gender+age+
          race*relevel(factor(semester),3)+level+plan+cins.pre+isea.pre)
anova(r1.1)
summary(r1.1) #Model with interaction, change semester reference group.  Not the best way

r1.2=lm(rkc.post~rkc.pre+esl+reading+writing+bioclass+gender+age+relevel(race,2)*semester+level+plan+cins.pre+isea.pre)
anova(r1.2)
summary(r1.2) #Model with interaction, change race reference group.  Not the best way


r1.3=lm(rkc.post~rkc.pre+esl+reading+writing+bioclass+gender+age+race+race:semester+level+plan+cins.pre+isea.pre)
anova(r1.3)
summary(r1.3)  #Race term and interaction term, no regular semester term.  Compares semesters by race

r1.4=lm(rkc.post~rkc.pre+esl+reading+writing+bioclass+gender+age+race+
          race:relevel(factor(semester),2)+level+plan+cins.pre+isea.pre)
anova(r1.4)    #Race term and interaction term, no regular semester term.
summary(r1.4)  #Compares semesters by race.  Changes reference group


r1.5=lm(rkc.post~rkc.pre+esl+reading+writing+bioclass+gender+age+semester+race:semester+level+plan+cins.pre+isea.pre)
anova(r1.5)
summary(r1.5)  #Semester term and interaction term, no regular race term.  Compares races by semester

r1.6=lm(rkc.post~rkc.pre+esl+reading+writing+bioclass+gender+age+semester+
          relevel(race,2):semester+level+plan+cins.pre+isea.pre)
anova(r1.6)    #Semester term and interaction term, no regular race term.
summary(r1.6)  #Compares races by semester. Changes reference group



#Animal NI
r0=lm(snaive.post~snaive.pre+semester+esl+reading+writing+bioclass+gender+age+race+level+plan+cins.pre+isea.pre)
anova(r0)
summary(r0)   #Model with no interaction

r0.1=lm(snaive.post~snaive.pre+relevel(factor(semester),3)+esl+reading+writing+bioclass+gender+age+race+level+plan+cins.pre+isea.pre)
anova(r0.1)
summary(r0.1) #Model with no interaction, change reference group

r1=lm(snaive.post~snaive.pre+semester+esl+reading+writing+bioclass+gender+age+race*semester+level+plan+cins.pre+isea.pre)
anova(r1)
summary(r1)   #Model with interaction.  I dont think this is the best way

r1.1=lm(snaive.post~snaive.pre+relevel(factor(semester),3)+esl+reading+writing+bioclass+gender+age+
          race*relevel(factor(semester),3)+level+plan+cins.pre+isea.pre)
anova(r1.1)
summary(r1.1) #Model with interaction, change semester reference group.  Not the best way

r1.2=lm(snaive.post~snaive.pre+esl+reading+writing+bioclass+gender+age+relevel(race,2)*semester+level+plan+cins.pre+isea.pre)
anova(r1.2)
summary(r1.2) #Model with interaction, change race reference group.  Not the best way


r1.3=lm(snaive.post~snaive.pre+esl+reading+writing+bioclass+gender+age+race+race:semester+level+plan+cins.pre+isea.pre)
anova(r1.3)
summary(r1.3)  #Race term and interaction term, no regular semester term.  Compares semesters by race

r1.4=lm(snaive.post~snaive.pre+esl+reading+writing+bioclass+gender+age+race+
          race:relevel(factor(semester),3)+level+plan+cins.pre+isea.pre)
anova(r1.4)    #Race term and interaction term, no regular semester term.
summary(r1.4)  #Compares semesters by race.  Changes reference group


r1.5=lm(snaive.post~snaive.pre+esl+reading+writing+bioclass+gender+age+semester+race:semester+level+plan+cins.pre+isea.pre)
anova(r1.5)
summary(r1.5)  #Semester term and interaction term, no regular race term.  Compares races by semester

r1.6=lm(snaive.post~snaive.pre+esl+reading+writing+bioclass+gender+age+semester+
          relevel(race,2):semester+level+plan+cins.pre+isea.pre)
anova(r1.6)    #Semester term and interaction term, no regular race term.
summary(r1.6)  #Compares races by semester. Changes reference group



#Plant NI
r0=lm(rnaive.post~rnaive.pre+semester+esl+reading+writing+bioclass+gender+age+race+level+plan+cins.pre+isea.pre)
anova(r0)
summary(r0)   #Model with no interaction

r0.1=lm(rnaive.post~rnaive.pre+relevel(factor(semester),2)+esl+reading+writing+bioclass+gender+age+race+level+plan+cins.pre+isea.pre)
anova(r0.1)
summary(r0.1) #Model with no interaction, change reference group

r1=lm(rnaive.post~rnaive.pre+semester+esl+reading+writing+bioclass+gender+age+race*semester+level+plan+cins.pre+isea.pre)
anova(r1)
summary(r1)   #Model with interaction.  I dont think this is the best way

r1.1=lm(rnaive.post~rnaive.pre+relevel(factor(semester),3)+esl+reading+writing+bioclass+gender+age+
          race*relevel(factor(semester),3)+level+plan+cins.pre+isea.pre)
anova(r1.1)
summary(r1.1) #Model with interaction, change semester reference group.  Not the best way

r1.2=lm(rnaive.post~rnaive.pre+esl+reading+writing+bioclass+gender+age+relevel(race,2)*semester+level+plan+cins.pre+isea.pre)
anova(r1.2)
summary(r1.2) #Model with interaction, change race reference group.  Not the best way


r1.3=lm(rnaive.post~rnaive.pre+esl+reading+writing+bioclass+gender+age+race+race:semester+level+plan+cins.pre+isea.pre)
anova(r1.3)
summary(r1.3)  #Race term and interaction term, no regular semester term.  Compares semesters by race

r1.4=lm(rnaive.post~rnaive.pre+esl+reading+writing+bioclass+gender+age+race+
          race:relevel(factor(semester),3)+level+plan+cins.pre+isea.pre)
anova(r1.4)    #Race term and interaction term, no regular semester term.
summary(r1.4)  #Compares semesters by race.  Changes reference group


r1.5=lm(rnaive.post~rnaive.pre+esl+reading+writing+bioclass+gender+age+semester+race:semester+level+plan+cins.pre+isea.pre)
anova(r1.5)
summary(r1.5)  #Semester term and interaction term, no regular race term.  Compares races by semester

r1.6=lm(rnaive.post~rnaive.pre+esl+reading+writing+bioclass+gender+age+semester+
          relevel(race,2):semester+level+plan+cins.pre+isea.pre)
anova(r1.6)    #Semester term and interaction term, no regular race term.
summary(r1.6)  #Compares races by semester. Changes reference group




####GENDER DIFFERENCES?####

#gender[gender==3]=NA

#Animal KC
r0=lm(skc.post~skc.pre+semester+esl+reading+writing+bioclass+gender+age+race+level+plan+cins.pre+isea.pre)
anova(r0)
summary(r0)   #Model with no interaction

r0.1=lm(skc.post~skc.pre+semester+esl+reading+writing+bioclass+relevel(gender,2)+age+race+level+plan+cins.pre+isea.pre)
anova(r0.1)
summary(r0.1) #Model with no interaction, change reference group


r1.3=lm(skc.post~skc.pre+esl+reading+writing+bioclass+gender+age+race+gender:semester+level+plan+cins.pre+isea.pre)
anova(r1.3)
summary(r1.3)  #Gender term and interaction term, no regular semester term.  Compares semesters by gender

r1.4=lm(skc.post~skc.pre+esl+reading+writing+bioclass+gender+age+race+
          gender:relevel(factor(semester),3)+level+plan+cins.pre+isea.pre)
anova(r1.4)    #Gender term and interaction term, no regular semester term.
summary(r1.4)  #Compares semesters by Gender  Changes reference group


r1.5=lm(skc.post~skc.pre+esl+reading+writing+bioclass+age+race+semester+gender:semester+level+plan+cins.pre+isea.pre)
anova(r1.5)
summary(r1.5)  #Semester term and interaction term, no regular race term.  Compares races by semester



#Plant KC
r0=lm(rkc.post~rkc.pre+semester+esl+reading+writing+bioclass+gender+age+race+level+plan+cins.pre+isea.pre)
anova(r0)
summary(r0)   #Model with no interaction

r0.1=lm(rkc.post~rkc.pre+semester+esl+reading+writing+bioclass+relevel(gender,2)+age+race+level+plan+cins.pre+isea.pre)
anova(r0.1)
summary(r0.1) #Model with no interaction, change reference group


r1.3=lm(rkc.post~rkc.pre+esl+reading+writing+bioclass+gender+age+race+gender:semester+level+plan+cins.pre+isea.pre)
anova(r1.3)
summary(r1.3)  #Gender term and interaction term, no regular semester term.  Compares semesters by gender

r1.4=lm(rkc.post~rkc.pre+esl+reading+writing+bioclass+gender+age+race+
          gender:relevel(factor(semester),3)+level+plan+cins.pre+isea.pre)
anova(r1.4)    #Gender term and interaction term, no regular semester term.
summary(r1.4)  #Compares semesters by Gender  Changes reference group


r1.5=lm(rkc.post~rkc.pre+esl+reading+writing+bioclass+age+race+semester+gender:semester+level+plan+cins.pre+isea.pre)
anova(r1.5)
summary(r1.5)  #Semester term and interaction term, no regular race term.  Compares races by semester



#Animal NI
r0=lm(snaive.post~snaive.pre+semester+esl+reading+writing+bioclass+gender+age+race+level+plan+cins.pre+isea.pre)
anova(r0)
summary(r0)   #Model with no interaction

r0.1=lm(snaive.post~snaive.pre+semester+esl+reading+writing+bioclass+relevel(gender,2)+age+race+level+plan+cins.pre+isea.pre)
anova(r0.1)
summary(r0.1) #Model with no interaction, change reference group


r1.3=lm(snaive.post~snaive.pre+esl+reading+writing+bioclass+gender+age+race+gender:semester+level+plan+cins.pre+isea.pre)
anova(r1.3)
summary(r1.3)  #Gender term and interaction term, no regular semester term.  Compares semesters by gender

r1.4=lm(snaive.post~snaive.pre+esl+reading+writing+bioclass+gender+age+race+
          gender:relevel(factor(semester),3)+level+plan+cins.pre+isea.pre)
anova(r1.4)    #Gender term and interaction term, no regular semester term.
summary(r1.4)  #Compares semesters by Gender  Changes reference group


r1.5=lm(snaive.post~snaive.pre+esl+reading+writing+bioclass+age+race+semester+gender:semester+level+plan+cins.pre+isea.pre)
anova(r1.5)
summary(r1.5)  #Semester term and interaction term, no regular race term.  Compares races by semester



#Plant NI
r0=lm(rnaive.post~rnaive.pre+semester+esl+reading+writing+bioclass+gender+age+race+level+plan+cins.pre+isea.pre)
anova(r0)
summary(r0)   #Model with no interaction

r0.1=lm(rnaive.post~rnaive.pre+semester+esl+reading+writing+bioclass+relevel(gender,2)+age+race+level+plan+cins.pre+isea.pre)
anova(r0.1)
summary(r0.1) #Model with no interaction, change reference group


r1.3=lm(rnaive.post~rnaive.pre+esl+reading+writing+bioclass+gender+age+race+gender:semester+level+plan+cins.pre+isea.pre)
anova(r1.3)
summary(r1.3)  #Gender term and interaction term, no regular semester term.  Compares semesters by gender

r1.4=lm(rnaive.post~rnaive.pre+esl+reading+writing+bioclass+gender+age+race+
          gender:relevel(factor(semester),3)+level+plan+cins.pre+isea.pre)
anova(r1.4)    #Gender term and interaction term, no regular semester term.
summary(r1.4)  #Compares semesters by Gender  Changes reference group


r1.5=lm(rnaive.post~rnaive.pre+esl+reading+writing+bioclass+age+race+semester+gender:semester+level+plan+cins.pre+isea.pre)
anova(r1.5)
summary(r1.5)  #Semester term and interaction term, no regular race term.  Compares races by semester



####FULL INTERACTION EFFECT RACE####
r1=lm(skc.post~skc.pre+esl+reading+writing+bioclass+gender+age+race*semester+level+plan+cins.pre+isea.pre)
anova(r1)
summary(r1)


r2=lm(rkc.post~rkc.pre+esl+reading+writing+bioclass+gender+age+race*semester+level+plan+cins.pre+isea.pre)
anova(r2)
summary(r2)


r3=lm(snaive.post~snaive.pre+esl+reading+writing+bioclass+gender+age+race*semester+level+plan+cins.pre+isea.pre)
anova(r3)
summary(r3)


r4=lm(rnaive.post~rnaive.pre+esl+reading+writing+bioclass+gender+age+race*semester+level+plan+cins.pre+isea.pre)
anova(r4)
summary(r4)



####Full interaction gender effect####
r1=lm(skc.post~skc.pre+esl+reading+writing+bioclass+race+age+gender*semester+level+plan+cins.pre+isea.pre)
anova(r1)
summary(r1)


r2=lm(rkc.post~rkc.pre+esl+reading+writing+bioclass+race+age+gender*semester+level+plan+cins.pre+isea.pre)
anova(r2)
summary(r2)


r3=lm(snaive.post~snaive.pre+esl+reading+writing+bioclass+race+age+gender*semester+level+plan+cins.pre+isea.pre)
anova(r3)
summary(r3)


r4=lm(rnaive.post~rnaive.pre+esl+reading+writing+bioclass+race+age+gender*semester+level+plan+cins.pre+isea.pre)
anova(r4)
summary(r4)


race=relevel(race,2)
semester=relevel(semester,ref="c")
levels(semester)=c("a","b","c","d")

##BASELINES##
r1=lm(skc.pre~esl+reading+writing+bioclass+gender+age+race+relevel(factor(semester),1)+level+plan+cins.pre+isea.pre)
anova(r1)
summary(r1)

r2=lm(rkc.pre~esl+reading+writing+bioclass+gender+age+race+relevel(factor(semester),3)+level+plan+cins.pre+isea.pre)
anova(r2)
summary(r2)

r3=lm(snaive.pre~esl+reading+writing+bioclass+gender+age+race+relevel(factor(semester),1)+level+plan+cins.pre+isea.pre)
anova(r3)
summary(r3)

r4=lm(rnaive.pre~esl+reading+writing+bioclass+gender+age+race+relevel(factor(semester),3)+level+plan+cins.pre+isea.pre)
anova(r4)
summary(r4)

#race=relevel(race,2)



###Model Consistency###
#tmodel.sci2=tmodel.sci1  #Run this rather than rewrite code

r0=glm(tmodel.sci2~semester+esl+reading+writing+bioclass+gender+age+race+level+plan+cins.pre+isea.pre,
       family=binomial(link='logit'))
anova(r0)
summary(r0)   #Model with no interaction
exp(coef(r0))

r0.1=glm(tmodel.sci2~tmodel.sci0+relevel(factor(semester),3)+esl+reading+writing+bioclass+gender+age+
           race+level+plan+cins.pre+isea.pre,
         family=binomial(link='logit'))
anova(r0.1)
summary(r0.1)   #Model with no interaction, change reference group


r1.3=glm(tmodel.sci2~tmodel.sci0+esl+reading+writing+bioclass+gender+age+race+race:semester+level+plan+cins.pre+isea.pre,
         family=binomial(link='logit'))
anova(r1.3)
summary(r1.3)   #Race term and interaction term, no regular semester term.  Compares semesters by race
exp(coef(r1.3))

r1.4=glm(tmodel.sci2~tmodel.sci0+esl+reading+writing+bioclass+gender+age+race+
           race:relevel(factor(semester),3)+level+plan+cins.pre+isea.pre,
         family=binomial(link='logit'))
anova(r1.4)    #Race term and interaction term, no regular semester term.
summary(r1.4)  #Compares semesters by race.  Changes reference group
exp(coef(r1.4))

r1.5=glm(tmodel.sci2~tmodel.sci0+esl+reading+writing+bioclass+gender+age+semester+race:semester+level+plan+cins.pre+isea.pre,
         family=binomial(link='logit'))
anova(r1.5)
summary(r1.5)  #Semester term and interaction term, no regular race term.  Compares races by semester

r1.6=glm(tmodel.sci2~tmodel.sci0+esl+reading+writing+bioclass+gender+age+semester+
           relevel(race,2):semester+level+plan+cins.pre+isea.pre,
         family=binomial(link='logit'))
anova(r1.6)    #Semester term and interaction term, no regular race term.
summary(r1.6)  #Compares races by semester. Changes reference group


r1.7=glm(tmodel.sci2~tmodel.sci0+esl+reading+writing+bioclass+gender+age+race*semester+level+plan+cins.pre+isea.pre,
         family=binomial(link='logit'))
anova(r1.7)
summary(r1.7)     #Full interaction effect


#Pretest
r1.8=glm(tmodel.sci0~esl+reading+writing+bioclass+gender+age+race+relevel(factor(semester),3)+level+plan+cins.pre+isea.pre,
         family=binomial(link='logit'))
anova(r1.8)
summary(r1.8)



###CINS/CANS and race###
r0=lm(cins.post~cins.pre+skc.pre+rkc.pre+snaive.pre+rnaive.pre+esl+reading+writing+bioclass+
        gender+age+race+semester+level+plan+cins.pre+isea.pre)
summary(r0)     #Model with no interaction

r0.1=lm(cins.post~cins.pre+skc.pre+rkc.pre+snaive.pre+rnaive.pre+esl+reading+writing+bioclass+
          gender+age+relevel(factor(race),2)+semester+level+plan+cins.pre+isea.pre)
summary(r0.1)     #Model with no interaction, change reference group


r1.0=lm(cins.post~cins.pre+skc.pre+rkc.pre+snaive.pre+rnaive.pre+esl+reading+writing+bioclass+
          gender+age+race+race:semester+level+plan+cins.pre+isea.pre)
summary(r1.0)     #Race term and interaction term, no regular semester term.  Compares semesters by race

r1.1=lm(cins.post~cins.pre+skc.pre+rkc.pre+snaive.pre+rnaive.pre+esl+reading+writing+bioclass+
          gender+age+race+race:relevel(factor(semester),3)+level+plan+cins.pre+isea.pre)
summary(r1.1)     #Compares semesters by race, change reference group

r2.0=lm(cins.post~cins.pre+skc.pre+rkc.pre+snaive.pre+rnaive.pre+esl+reading+writing+bioclass+
          gender+age+semester+race:semester+level+plan+cins.pre+isea.pre)
summary(r2.0)     #Semester term and interaction term, no regular race term.  Compares races by semester

r2.1=lm(cins.post~cins.pre+skc.pre+rkc.pre+snaive.pre+rnaive.pre+esl+reading+writing+bioclass+
          gender+age+semester+relevel(race,2):semester+level+plan+cins.pre+isea.pre)
summary(r2.1)     #Compares races by semester, change reference group

r3.0=lm(cins.post~cins.pre+skc.pre+rkc.pre+snaive.pre+rnaive.pre+esl+reading+writing+bioclass+
          gender+age+race*semester+level+plan+cins.pre+isea.pre)
summary(r3.0)     #Full Interaction Effect


#sem=semester
race=relevel(race,2)
semester=relevel(semester,ref="c")
semester=sem



#Pretest#
r4=lm(cins.pre~esl+reading+skc.pre+rkc.pre+snaive.pre+rnaive.pre+writing+bioclass+gender+age+
        race+relevel(factor(semester),1)+level+plan+cins.pre+isea.pre)
summary(r4)

#cins.pre=cans.pre   #Just run this to do CANS, rather than rewrite all code
#cins.post=cans.post

#Imputing F17 CINS
cins.fall17.pre = 6.07203 + 0.50892*cans.fall17.pre
cins.fall17.post = 6.65200 + 0.55926*cans.fall17.post
cins.pre=c(cins.spring15.pre,cins.spring16.pre,cins.spring17.pre,cins.fall17.pre)
cins.post=c(cins.spring15.post,cins.spring16.post,cins.spring17.post,cins.fall17.post)
##################################################################bottom






######Animal/Plant combined, but time points separate.  MIXED MODELS######
#Run "4 Groups Mixed Models2.0" first, or just run code below#
library(lme4)
library(lmerTest) # show the results,  p-values change in Mixed Models (DO THEY?)

#Simplified Dataset#
all4sems=read.csv("C:/Users/jcolton/Documents/Jesse's Documents/Documents/Education/Data/4Semesters3.0.csv",
                  header=TRUE, stringsAsFactors=FALSE)
attach(all4sems)

esl=factor(esl)
bioclass=factor(bioclass)
gender=factor(gender)
race=factor(race)
race=relevel(race,ref="3")
semester=factor(semester)

all.kc.pre=c(skc.pre,rkc.pre)
all.kc.ex1=c(skc.ex1,rkc.ex1)
all.kc.post=c(skc.post,rkc.post)
all.ni.pre=c(snaive.pre,rnaive.pre)
all.ni.ex1=c(snaive.ex1,rnaive.ex1)
all.ni.post=c(snaive.post,rnaive.post)
all.type=c(rep("A",length(skc.pre)),rep("P",length(rkc.pre)))
all.esl=factor(c(esl,esl))
all.reading=c(reading,reading)
all.writing=c(writing,writing)
all.bioclass=factor(c(bioclass,bioclass))
all.gender=factor(c(gender,gender))
all.age=c(age,age)
all.race=factor(rep(race,2))
all.level=c(level,level)
 all.level=as.numeric(all.level)
all.plan=c(plan,plan)
 all.plan=as.numeric(all.plan)
all.cins.pre=c(cins.pre,cins.pre)
all.isea.pre=c(isea.pre,isea.pre)
all.id=factor(c(id,id))
all.semester=rep(semester,2)


#####Mixed Models#####

###Race###

##KC
r0=lmer(all.kc.post~all.kc.pre+all.semester+all.type+all.esl+all.reading+all.writing+all.bioclass+
          all.gender+all.age+all.race+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
summary(r0)   #Model with no interaction

r0.1=lmer(all.kc.post~all.kc.pre+all.semester+all.type+all.esl+all.reading+all.writing+all.bioclass+
            all.gender+all.age+relevel(all.race,2)+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
summary(r0.1)   #Model with no interaction, change reference group


r1=lmer(all.kc.post~all.kc.pre+all.type+all.esl+all.reading+all.writing+all.bioclass+all.gender+
          all.age+all.race+all.race:all.semester+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
summary(r1)   #Race term and interaction term, no regular semester term.  Compares semesters by race

r1.1=lmer(all.kc.post~all.kc.pre+all.type+all.esl+all.reading+all.writing+all.bioclass+all.gender+
          all.age+all.race+all.race:relevel(factor(all.semester),1)+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
anova(r1.1)     #Race term and interaction term, no regular semester term.  
summary(r1.1)   #Compares semesters by race. Changes reference group


r2=lmer(all.kc.post~all.kc.pre+all.type+all.esl+all.reading+all.writing+all.bioclass+all.gender+
          all.age+all.semester+all.race:all.semester+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
summary(r2)   #Semester term and interaction term, no regular race term.  Compares races by semester

r2.1=lmer(all.kc.post~all.kc.pre+all.type+all.esl+all.reading+all.writing+all.bioclass+all.gender+
            all.age+all.semester+relevel(all.race,2):all.semester+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
anova(r2.1)     #Race term and interaction term, no regular race term.  
summary(r2.1)   #Compares races by semester Changes reference group


r3=lmer(all.kc.post~all.kc.pre+all.type+all.esl+all.reading+all.writing+all.bioclass+all.gender+
          all.age+all.race*all.semester+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
summary(r3)    #Full Interaction Effect

#Use when needing to relevel
all.race=relevel(all.race,2)
all.semester=relevel(all.semester,ref="c")

all.semester=rep(semester,2)


##NI
r0=lmer(all.ni.post~all.ni.pre+all.semester+all.type+all.esl+all.reading+all.writing+all.bioclass+
          all.gender+all.age+all.race+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
summary(r0)   #Model with no interaction

r0.1=lmer(all.ni.post~all.ni.pre+all.semester+all.type+all.esl+all.reading+all.writing+all.bioclass+
            all.gender+all.age+relevel(all.race,2)+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
summary(r0.1)   #Model with no interaction, change reference group


r1=lmer(all.ni.post~all.ni.pre+all.type+all.esl+all.reading+all.writing+all.bioclass+all.gender+
          all.age+all.race+all.race:all.semester+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
summary(r1)   #Race term and interaction term, no regular semester term.  Compares semesters by race

r1.1=lmer(all.ni.post~all.ni.pre+all.type+all.esl+all.reading+all.writing+all.bioclass+all.gender+
            all.age+all.race+all.race:relevel(factor(all.semester),1)+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
anova(r1.1)     #Race term and interaction term, no regular semester term.  
summary(r1.1)   #Compares semesters by race. Changes reference group


r2=lmer(all.ni.post~all.ni.pre+all.type+all.esl+all.reading+all.writing+all.bioclass+all.gender+
          all.age+all.semester+all.race:all.semester+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
summary(r2)   #Semester term and interaction term, no regular race term.  Compares races by semester

r2.1=lmer(all.ni.post~all.ni.pre+all.type+all.esl+all.reading+all.writing+all.bioclass+all.gender+
            all.age+all.semester+relevel(all.race,2):all.semester+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
anova(r2.1)     #Race term and interaction term, no regular race term.  
summary(r2.1)   #Compares races by semester Changes reference group


r3=lmer(all.ni.post~all.ni.pre+all.type+all.esl+all.reading+all.writing+all.bioclass+all.gender+
          all.age+all.race*all.semester+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
summary(r3)    #Full Interaction Effect

#Use when needing to relevel
all.race=relevel(all.race,2)
all.semester=relevel(all.semester,ref="c")

all.semester=rep(semester,2)


##Model Consistency
r0=glm(tmodel.sci2~tmodel.sci0+semester+esl+reading+writing+bioclass+gender+age+race+level+plan+
        cins.pre+isea.pre,family = binomial(link='logit'))
summary(r0)   #Model with no interaction

r0.1=glm(tmodel.sci2~tmodel.sci0+semester+esl+reading+writing+bioclass+gender+age+relevel(race,2)+level+plan+
          cins.pre+isea.pre,family = binomial(link='logit'))
summary(r0.1)   #Model with no interaction, change reference group


r1=glm(tmodel.sci2~tmodel.sci0+esl+reading+writing+bioclass+gender+age+race+race:semester+level+plan+
          cins.pre+isea.pre,family = binomial(link='logit'))
summary(r1)   #Race term and interaction term, no regular semester term.  Compares semesters by race

r1.1=glm(tmodel.sci2~tmodel.sci0+esl+reading+writing+bioclass+gender+age+race+race:relevel(factor(semester),1)+
          level+plan+cins.pre+isea.pre,family = binomial(link='logit'))
anova(r1.1)     #Race term and interaction term, no regular semester term.  
summary(r1.1)   #Compares semesters by race. Changes reference group


r2=glm(tmodel.sci2~tmodel.sci0+esl+reading+writing+bioclass+gender+age+semester+race:semester+level+plan+
        cins.pre+isea.pre,family = binomial(link='logit'))
summary(r2)   #Semester term and interaction term, no regular race term.  Compares races by semester

r2.1=glm(tmodel.sci2~tmodel.sci0+esl+reading+writing+bioclass+gender+age+semester+relevel(race,1):semester+
          level+plan+cins.pre+isea.pre,family = binomial(link='logit'))
anova(r2.1)     #Race term and interaction term, no regular race term.  
summary(r2.1)   #Compares races by semester Changes reference group


r3=glm(tmodel.sci2~tmodel.sci0+esl+reading+writing+bioclass+gender+age+race*semester+level+plan+
        cins.pre+isea.pre,family = binomial(link='logit'))
summary(r3)    #Full Interaction Effect

#Use when needing to relevel
sem=semester
race=relevel(race,2)
semester=relevel(semester,ref="c")

semester=sem


#Use to avoid doubling code#
all.kc.post=all.kc.ex1
all.ni.post=all.ni.ex1
tmodel.sci2=tmodel.sci1




###Gender###    #STILL WORK IN PROGRESS, COPY/PASTE FROM ABOVE>  BE CAREFUL

gender.restore=gender   #Save it so I can restore if needed
gender[gender==3]=NA      #Get rid of the "none of the above" students
all.gender=factor(c(gender,gender))

##KC
r0=lmer(all.kc.post~all.kc.pre+all.semester+all.type+all.esl+all.reading+all.writing+all.bioclass+
          all.gender+all.age+all.race+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
summary(r0)   #Model with no interaction


r1=lmer(all.kc.post~all.kc.pre+all.type+all.esl+all.reading+all.writing+all.bioclass+all.gender+
          all.age+all.race+all.gender:all.semester+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
summary(r1)   #Gender term and interaction term, no regular semester term.  Compares semesters by gender

r1.1=lmer(all.kc.post~all.kc.pre+all.type+all.esl+all.reading+all.writing+all.bioclass+all.gender+
            all.age+all.race+all.gender:relevel(factor(all.semester),1)+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
anova(r1.1)     #Gender term and interaction term, no regular semester term.  
summary(r1.1)   #Compares semesters by gender Changes reference group


r2=lmer(all.kc.post~all.kc.pre+all.type+all.esl+all.reading+all.writing+all.bioclass+all.age+all.race+
          all.semester+all.gender:all.semester+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
summary(r2)   #Semester term and interaction term, no regular gender term.  Compares genders by semester


r3=lmer(all.kc.post~all.kc.pre+all.type+all.esl+all.reading+all.writing+all.bioclass+all.age+all.race+
          all.gender*all.semester+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
summary(r3)    #Full Interaction Effect

#Use when needing to relevel
all.semester=relevel(all.semester,ref="c")

all.semester=rep(semester,2)


##NI
r0=lmer(all.ni.post~all.ni.pre+all.semester+all.type+all.esl+all.reading+all.writing+all.bioclass+
          all.gender+all.age+all.race+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
summary(r0)   #Model with no interaction


r1=lmer(all.ni.post~all.ni.pre+all.type+all.esl+all.reading+all.writing+all.bioclass+all.gender+
          all.age+all.race+all.gender:all.semester+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
summary(r1)   #Gender term and interaction term, no regular semester term.  Compares semesters by gender

r1.1=lmer(all.ni.post~all.ni.pre+all.type+all.esl+all.reading+all.writing+all.bioclass+all.gender+
            all.age+all.race+all.gender:relevel(factor(all.semester),1)+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
anova(r1.1)     #Gender term and interaction term, no regular semester term.  
summary(r1.1)   #Compares semesters by gender Changes reference group


r2=lmer(all.ni.post~all.ni.pre+all.type+all.esl+all.reading+all.writing+all.bioclass+all.age+all.race+
          all.semester+all.gender:all.semester+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
summary(r2)   #Semester term and interaction term, no regular gender term.  Compares genders by semester


r3=lmer(all.ni.post~all.ni.pre+all.type+all.esl+all.reading+all.writing+all.bioclass+all.age+all.race+
          all.gender*all.semester+all.level+all.plan+all.cins.pre+all.isea.pre + (1|all.id))
summary(r3)    #Full Interaction Effect

#Use when needing to relevel
all.semester=relevel(all.semester,ref="c")

all.semester=rep(semester,2)


##Model Consistency
r0=glm(tmodel.sci2~tmodel.sci0+semester+esl+reading+writing+bioclass+gender+age+race+level+plan+
         cins.pre+isea.pre,family = binomial(link='logit'))
summary(r0)   #Model with no interaction


r1=glm(tmodel.sci2~tmodel.sci0+esl+reading+writing+bioclass+gender+age+race+gender:semester+level+plan+
         cins.pre+isea.pre,family = binomial(link='logit'))
summary(r1)   #Gender term and interaction term, no regular semester term.  Compares semesters by gender

r1.1=glm(tmodel.sci2~tmodel.sci0+esl+reading+writing+bioclass+gender+age+race+gender:relevel(factor(semester),1)+
           level+plan+cins.pre+isea.pre,family = binomial(link='logit'))
anova(r1.1)     #Gender term and interaction term, no regular semester term.  
summary(r1.1)   #Compares semesters by gender Changes reference group


r2=glm(tmodel.sci2~tmodel.sci0+esl+reading+writing+bioclass+age+race+semester+gender:semester+level+plan+
         cins.pre+isea.pre,family = binomial(link='logit'))
summary(r2)   #Semester term and interaction term, no regular gender term.  Compares gender by semester


r3=glm(tmodel.sci2~tmodel.sci0+esl+reading+writing+bioclass+age+race+gender*semester+level+plan+
         cins.pre+isea.pre,family = binomial(link='logit'))
summary(r3)    #Full Interaction Effect

#Use when needing to relevel
sem=semester
semester=relevel(semester,ref="c")

semester=sem


#Use to avoid doubling code#
all.kc.post=all.kc.ex1
all.ni.post=all.ni.ex1
tmodel.sci2=tmodel.sci1

