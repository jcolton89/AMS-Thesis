#RUN 4 GROUPS MIXED MODELS

#Effect Sizes (Beta's) For Mixed Models#
#First Section is overall effects#
#Second Section is by race#
 
###Key Concepts###
m.kc.beta = lmer(scale(alltime.allkc) ~ alltime.allsemester*all.timepoint + alltime.type + alltime.allesl +
                alltime.allreading + alltime.allwriting + alltime.allbioclass + alltime.allgender +
                alltime.allage + alltime.allrace + alltime.alllevel + alltime.allplan +
                alltime.allcins.pre + alltime.allisea.pre + 
                (1|alltime.allid))  ##Model that uses standardized Y, for standardized Betas
summary(m.kc.beta)

x=c(fixef(m.kc.beta)[5],fixef(m.kc.beta)[24:26]+fixef(m.kc.beta)[5],fixef(m.kc.beta)[6],
    fixef(m.kc.beta)[27:29]+fixef(m.kc.beta)[6])   #Creates Betas (Using CG = reference)
names(x)=NULL

group=rep(c("CG","EG1","EG2","EG3"))
time=factor(c(rep("Post",4),rep("Delayed-Post",4)),levels=c("Post","Delayed-Post"))
levels(time)=c("Post","D-Post")
#order(time)
df=data.frame(x,group,time)

dotchart(df$x,labels=df$group,groups=df$time,xlim=c(0,1.4),#pch=c(19,17,15,13),
         cex=1.4,pch=16,xlab="Standardized Betas",main="Effect Size (Key Concepts)",
         color=c("red","darkgreen","blue","purple"))
abline(v=0.45,col="red",lty=2)



###Naive Ideas###
m.ni.beta = lmer(scale(alltime.allnaive) ~ alltime.allsemester*all.timepoint + alltime.type + alltime.allesl +
                   alltime.allreading + alltime.allwriting + alltime.allbioclass + alltime.allgender +
                   alltime.allage + alltime.allrace + alltime.alllevel + alltime.allplan +
                   alltime.allcins.pre + alltime.allisea.pre + 
                   (1|alltime.allid))  ##Model that uses standardized Y, for standardized Betas
summary(m.ni.beta)

x=c(fixef(m.ni.beta)[5],fixef(m.ni.beta)[24:26]+fixef(m.ni.beta)[5],fixef(m.ni.beta)[6],
    fixef(m.ni.beta)[27:29]+fixef(m.ni.beta)[6])   #Creates Betas (Using CG = reference)
names(x)=NULL
x=abs(x)

group=rep(c("CG","EG1","EG2","EG3"))
time=factor(c(rep("Post",4),rep("Delayed-Post",4)),levels=c("Post","Delayed-Post"))
levels(time)=c("Post","D-Post")
#order(time)
df1=data.frame(x,group,time)

dotchart(df1$x,labels=df1$group,groups=df1$time,xlim=c(0,1.4),#pch=c(19,17,15,13),
         cex=1.4,pch=16,xlab="Standardized Betas",main="Effect Size (Naive Ideas)",
         color=c("red","darkgreen","blue","purple"))
abline(v=0.45,col="red",lty=2)



###CINS###
cins.both=c(cins.pre,cins.post)
cans.both=c(cans.pre,cans.post)
skc.both.pre=rep(skc.pre,2)
rkc.both.pre=rep(rkc.pre,2)
snaive.both.pre=rep(snaive.pre,2)
rnaive.both.pre=rep(rnaive.pre,2)
semester.both=rep(semester,2)
esl.both=rep(esl,2)
reading.both=rep(reading,2)
writing.both=rep(writing,2)
bioclass.both=rep(bioclass,2)
gender.both=rep(gender,2)
age.both=rep(age,2)
race.both=rep(race,2)
level.both=rep(level,2)
plan.both=rep(plan,2)
isea.both.pre=rep(isea.pre,2)
id.both=factor(rep(id,2))
time.both=c(rep("pre",length(esl)),rep("delpost",length(esl)))
time.both=factor(time.both,levels=c("pre","delpost"))
#semester.both=relevel(semester.both,ref="a")

m.cins.beta = lmer(scale(cins.both) ~ semester.both*time.both + skc.both.pre + rkc.both.pre +
                   snaive.both.pre + rnaive.both.pre + esl.both + reading.both + writing.both + 
                   bioclass.both + gender.both + age.both + race.both + level.both + plan.both +
                   isea.both.pre+(1|id.both)) ##Model that uses standardized Y, for standardized Betas
summary(m.cins.beta)

x=c(fixef(m.cins.beta)[5],fixef(m.cins.beta)[25:27]+fixef(m.cins.beta)[5]) #Creates Betas (Using CG = reference)
names(x)=NULL

group=c("CG","EG1","EG2","EG3")
time=factor(rep("D-Post",4))
df2=data.frame(x,group,time)

#dotchart(df2$x,labels=df2$group,xlim=c(0,1.4), groups=df2$time,#pch=c(19,17,15,13),
#         cex=1.8,pch=16,xlab="Standardized Betas",main="Effect Size (CINS)",
#         color=c("red","darkgreen","blue","purple"))
#abline(v=0.45,col="red",lty=2)
#Using CINS/CANS combined chart.  Just making sure not to accidentally run this for now

###CANS###
m.cans.beta = lmer(scale(cans.both) ~ semester.both*time.both + skc.both.pre + rkc.both.pre +
                     snaive.both.pre + rnaive.both.pre + esl.both + reading.both + writing.both + 
                     bioclass.both + gender.both + age.both + race.both + level.both + plan.both +
                     isea.both.pre+(1|id.both)) ##Model that uses standardized Y, for standardized Betas
summary(m.cans.beta)

x=c(fixef(m.cans.beta)[3],fixef(m.cans.beta)[23]+fixef(m.cans.beta)[3]) #Creates Betas (Using CG = reference)
names(x)=NULL

group=c("EG2","EG3")
df3=data.frame(x,group)

#dotchart(df3$x,labels=df3$group,xlim=c(0,1.4),#pch=c(19,17,15,13),
#         cex=1.8,pch=16,xlab="Standardized Betas",main="Effect Size (CANS)",
#         color=c("blue","purple"))
#Using CINS/CANS combined chart.  Just making sure not to accidentally run this for now


###CINS/CANS COMBINED###
df4=rbind(df2,df3)
test=factor(c(rep("CINS",4),rep("CANS",2)),levels = c("CINS","CANS"))
df4=data.frame(df4,test)

dotchart(df4$x,labels=df4$group,groups=df4$test,xlim=c(0,1.4),#pch=c(19,17,15,13),
         cex=1.4,pch=16,xlab="Standardized Betas",main="Effect Size (CANS/CINS)",
         color=c("red","darkgreen","blue","purple","blue","purple"))



###Creating all 3 plots in one###
par(mar=c(2,2,2,2))  #margins
par(mfrow=c(2,2))    #2x2 plot
par(mfrow=c(3,1))    #3x1 plot

#bty="y" is the box around;  pt.cex for the point size;  xaxt yaxt is for axis text
#mtext("Species", at=0.2, cex=2)   adds text

#I have been playing around with cex and pt.cex size, and other stuff, so be careful with parameters.

par(xaxt = "s",xlab="")    #change "s" to "n" to remove x axis
dotchart(df$x,labels=df$group,groups=df$time,xlim=c(0,1.4),xaxt="n",xlab="",#pch=c(19,17,15,13),
         cex=1.6,pt.cex = 1.8,pch=16,main="Key Concepts",
         color=c("red","darkgreen","blue","purple"))
dotchart(df1$x,labels=df1$group,groups=df1$time,xlim=c(0,1.4),#pch=c(19,17,15,13),
         cex=1.6,pt.cex = 1.8,pch=16,xlab="Standardized Betas",main="Naive Ideas",
         color=c("red","darkgreen","blue","purple"))
dotchart(df4$x,labels=df4$group,groups=df4$test,xlim=c(0,1.4),#pch=c(19,17,15,13),
            cex=1.6,pt.cex = 1.8,pch=16,xlab="Standardized Betas",main="CANS/CINS",
            color=c("red","darkgreen","blue","purple","blue","purple"))
plot(NULL) #,xaxt='n',yaxt='n',bty='n',ylab='',xlab='') #bty is the box around
legend("center", col=c("purple","blue", "darkgreen", "red") , c("EG3","EG2", "EG1", "CG"),bty="n",
       text.col = c("purple","blue", "darkgreen", "red"),pch=16,cex=1.4,pt.cex = 1.8)




###Model Consistency###   #Odds ratio is effect size here
m.mc.beta = glmer(alltime.tmodel.sci ~ alltime.semester*timepoint + alltime.esl +
                  alltime.reading + alltime.writing + alltime.bioclass + alltime.gender +
                  alltime.age + alltime.race + alltime.level + alltime.plan +
                  alltime.cins.pre + alltime.isea.pre + 
                  (1|alltime.id),             #Random Intercept for ID *****USE THIS*****
                family=binomial(link='logit'))
summary(m.mc.beta)

x=c(fixef(m.mc.beta)[5],fixef(m.mc.beta)[23:25]+fixef(m.mc.beta)[5],fixef(m.mc.beta)[6],
    fixef(m.mc.beta)[26:28]+fixef(m.mc.beta)[6])   #Creates Betas (Using CG = reference)
names(x)=NULL
x=exp(x)


group=rep(c("CG","EG1","EG2","EG3"))
time=factor(c(rep("Post",4),rep("Delayed-Post",4)),levels=c("Post","Delayed-Post"))
levels(time)=c("Post","D-Post")
#order(time)
df=data.frame(x,group,time)

dotchart(df$x,labels=df$group,groups=df$time,xlim=c(0,35),#pch=c(19,17,15,13),
         cex=1.4,pch=16,xlab="Odds Ratios",main="Scientific Model Consistency",
         color=c("red","darkgreen","blue","purple"))


#Log Scale Plot
plot(1, type="n", xlab="", ylab="", xlim=c(1, 35),xaxt="n",log="x")
axis(1, at = seq(5, 35, by = 5), las=1)
dotchart(log(df$x),labels=df$group,groups=df$time,xlim=c(0,log(35)),#pch=c(19,17,15,13),
        cex=1.4,pch=16,xlab="Odds Ratios",main="Scientific Model Consistency",
        color=c("red","darkgreen","blue","purple"))


##Test, Model Consistency without covariates#
m.test.3 = glmer(alltime.tmodel.sci ~ alltime.semester*timepoint+
                     (1|alltime.id),             #Random Intercept for ID
                 family=binomial(link='logit'))
summary(m.test.3)





###########ETHNICITY############


###Key Concepts###
m.kc.race = lmer(scale(alltime.allkc) ~ alltime.allsemester*all.timepoint + alltime.type + alltime.allesl +
                   alltime.allreading + alltime.allwriting + alltime.allbioclass + alltime.allgender +
                   alltime.allage  + alltime.alllevel + alltime.allplan + alltime.allcins.pre + 
                   alltime.allisea.pre + alltime.allrace:alltime.allsemester:all.timepoint +
                   (1|alltime.allid))  ##Model that uses standardized Y, for standardized Betas
summary(m.kc.race)  ###Compares races by semester.  #Sem effect,time,sem:time, then sem:race:time, no race 1st order

x=c(fixef(m.kc.race)[5], fixef(m.kc.race)[5]+fixef(m.kc.race)[22:24],  #White.  Start with Post
    fixef(m.kc.race)[5]+fixef(m.kc.race)[32],fixef(m.kc.race)[5]+fixef(m.kc.race)[22:24]+fixef(m.kc.race)[33:35], #Asian
    fixef(m.kc.race)[5]+fixef(m.kc.race)[44],fixef(m.kc.race)[5]+fixef(m.kc.race)[22:24]+fixef(m.kc.race)[45:47], #Other
    fixef(m.kc.race)[6], fixef(m.kc.race)[6]+fixef(m.kc.race)[25:27],  #White.  Delayed Post
    fixef(m.kc.race)[6]+fixef(m.kc.race)[36],fixef(m.kc.race)[6]+fixef(m.kc.race)[25:27]+fixef(m.kc.race)[37:39], #Asian
    fixef(m.kc.race)[6]+fixef(m.kc.race)[48],fixef(m.kc.race)[6]+fixef(m.kc.race)[25:27]+fixef(m.kc.race)[49:51] #Other
    )   
#Creates Betas (Using CG = reference)
names(x)=NULL

group=rep(c("CG","EG1","EG2","EG3"),6)
group=factor(group,levels=c("EG3","EG2","EG1","CG"))
which=rep(c(rep("White",4),rep("Asian",4),rep("URM",4)),2)
which=factor(which,levels=c("Asian","White","URM"))
time=factor(c(rep("Post",12),rep("Delayed-Post",12)),levels=c("Post","Delayed-Post"))
levels(time)=c("Post","Delayed-Post")
#order(time)
mycolor=rep(c(rep("Dark Green",4),rep("Blue",4),rep("Red",4)),2)
df=data.frame(x,group,time,which,mycolor)
ord=c(12:9,4:1,8:5,24:21,16:13,20:17)
df1=df[ord,]

##Grouped by semester, then race

#Post
dotchart(df1$x[time=="Post"],labels=df1$which[time=="Post"],groups=df1$group[time=="Post"],xlim=c(0,1.5),
         color=c("purple","blue","darkgreen","red"),
         pch=c(17,18,15),cex=0.8,pt.cex=1.5,xlab="Standardized Beta",main="Effect Size Key Concepts (Pre->Post)")
abline(v=0.45,col="red",lty=2)

#Delayed-Post 
dotchart(df1$x[time=="Delayed-Post"],labels=df1$which[time=="Delayed-Post"],groups=df1$group[time=="Delayed-Post"],
         xlim=c(0,1.5), color=c("purple","blue","darkgreen","red"),pch=c(17,18,15),cex=0.8,pt.cex=1.5,
         xlab="Standardized Beta",main="Effect Size Key Concepts (Pre->Delayed-Post)")
abline(v=0.45,col="red",lty=2)

#Both
#group1=c(rep(c("CG ","EG1 ","EG2 ","EG3 "),3),rep(c("CG  ","EG1  ","EG2  ","EG3  "),3))
#group1=factor(group1,levels = c("CG ","EG1 ","EG2 ","EG3 ","CG  ","EG1  ","EG2  ","EG3  "))
#df1=cbind(df,group1)
#dotchart(df1$x,labels=df1$which,groups=df1$group1,xlim=c(0,1.5),
#         pch=c(17,18,15),cex=0.8,pt.cex=1.5,xlab="Standardized Beta",main="Effect Size Key Concepts")
#abline(h=19.5,col="red")

#Add color=mycolor  to get the colors. Different color for each race.  Necessary?


##Grouped by race, then semester
#Post
dotchart(df$x[time=="Post"],labels=df$group[time=="Post"],groups=df$which[time=="Post"],xlim=c(0,1.5),
         color=c("red","darkgreen","blue","purple"),pch=c(rep(17,4),rep(18,4),rep(15,4)),
         cex=0.8,pt.cex=1.5,xlab="Standardized Beta",main="Effect Size Key Concepts (Pre->Post)")
abline(v=0.45,col="red",lty=2)

#Delayed-Post 
dotchart(df$x[time=="Delayed-Post"],labels=df$group[time=="Delayed-Post"],groups=df$which[time=="Delayed-Post"],
         xlim=c(0,1.5), color=c("red","darkgreen","blue","purple"),pch=c(rep(17,4),rep(18,4),rep(15,4)),cex=0.8,
         pt.cex=1.5,xlab="Standardized Beta",main="Effect Size Key Concepts (Pre->Delayed-Post)")
abline(v=0.45,col="red",lty=2)

#par(mfrow=c(1,2))



###Naive Ideas###
m.ni.race = lmer(scale(alltime.allnaive) ~ alltime.allsemester*all.timepoint + alltime.type + alltime.allesl +
                   alltime.allreading + alltime.allwriting + alltime.allbioclass + alltime.allgender +
                   alltime.allage  + alltime.alllevel + alltime.allplan + alltime.allcins.pre + 
                   alltime.allisea.pre + alltime.allrace:alltime.allsemester:all.timepoint +
                   (1|alltime.allid))  ##Model that uses standardized Y, for standardized Betas
summary(m.ni.race)  ###Compares races by semester.  #Sem effect,time,sem:time, then sem:race:time, no race 1st order

x=c(fixef(m.ni.race)[5], fixef(m.ni.race)[5]+fixef(m.ni.race)[22:24],  #White.  Start with Post
    fixef(m.ni.race)[5]+fixef(m.ni.race)[32],fixef(m.ni.race)[5]+fixef(m.ni.race)[22:24]+fixef(m.ni.race)[33:35], #Asian
    fixef(m.ni.race)[5]+fixef(m.ni.race)[44],fixef(m.ni.race)[5]+fixef(m.ni.race)[22:24]+fixef(m.ni.race)[45:47], #Other
    fixef(m.ni.race)[6], fixef(m.ni.race)[6]+fixef(m.ni.race)[25:27],  #White.  Delayed Post
    fixef(m.ni.race)[6]+fixef(m.ni.race)[36],fixef(m.ni.race)[6]+fixef(m.ni.race)[25:27]+fixef(m.ni.race)[37:39], #Asian
    fixef(m.ni.race)[6]+fixef(m.ni.race)[48],fixef(m.ni.race)[6]+fixef(m.ni.race)[25:27]+fixef(m.ni.race)[49:51] #Other
)   
#Creates Betas (Using CG = reference)
names(x)=NULL
x=abs(x)

group=rep(c("CG","EG1","EG2","EG3"),6)
group=factor(group,levels=c("EG3","EG2","EG1","CG"))
which=rep(c(rep("White",4),rep("Asian",4),rep("URM",4)),2)
which=factor(which,levels=c("Asian","White","URM"))
time=factor(c(rep("Post",12),rep("Delayed-Post",12)),levels=c("Post","Delayed-Post"))
levels(time)=c("Post","Delayed-Post")
#order(time)
mycolor=rep(c(rep("Dark Green",4),rep("Blue",4),rep("Red",4)),2)
df=data.frame(x,group,time,which,mycolor)
ord=c(12:9,4:1,8:5,24:21,16:13,20:17)
df1=df[ord,]

##Grouped by semester, then race

#Post
dotchart(df1$x[time=="Post"],labels=df1$which[time=="Post"],groups=df1$group[time=="Post"],xlim=c(0,1.5),
         color=c("purple","blue","darkgreen","red"),
         pch=c(17,18,15),cex=0.8,pt.cex=1.5,xlab="Standardized Beta",main="Effect Size Naive Ideas (Pre->Post)")
abline(v=0.45,col="red",lty=2)

#Delayed-Post 
dotchart(df1$x[time=="Delayed-Post"],labels=df1$which[time=="Delayed-Post"],groups=df1$group[time=="Delayed-Post"],
         xlim=c(0,1.5), color=c("purple","blue","darkgreen","red"),pch=c(17,18,15),cex=0.8,pt.cex=1.5,
         xlab="Standardized Beta",main="Effect Size Naive Ideas (Pre->Delayed-Post)")
abline(v=0.45,col="red",lty=2)

#Both
#group1=c(rep(c("CG ","EG1 ","EG2 ","EG3 "),3),rep(c("CG  ","EG1  ","EG2  ","EG3  "),3))
#group1=factor(group1,levels = c("CG ","EG1 ","EG2 ","EG3 ","CG  ","EG1  ","EG2  ","EG3  "))
#df1=cbind(df,group1)
#dotchart(df1$x,labels=df1$which,groups=df1$group1,xlim=c(0,1.5),
#         pch=c(17,18,15),cex=0.8,pt.cex=1.5,xlab="Standardized Beta",main="Effect Size Key Concepts")
#abline(h=19.5,col="red")

#Add color=mycolor  to get the colors. Different color for each race.  Necessary?


##Grouped by race, then semester
#Post
dotchart(df$x[time=="Post"],labels=df$group[time=="Post"],groups=df$which[time=="Post"],xlim=c(0,1.5),
         color=c("red","darkgreen","blue","purple"),pch=c(rep(17,4),rep(18,4),rep(15,4)),
         cex=0.8,pt.cex=1.5,xlab="Standardized Beta",main="Effect Size Naive Ideas (Pre->Post)")
abline(v=0.45,col="red",lty=2)

#Delayed-Post 
dotchart(df$x[time=="Delayed-Post"],labels=df$group[time=="Delayed-Post"],groups=df$which[time=="Delayed-Post"],
         xlim=c(0,1.5), color=c("red","darkgreen","blue","purple"),pch=c(rep(17,4),rep(18,4),rep(15,4)),cex=0.8,
         pt.cex=1.5,xlab="Standardized Beta",main="Effect Size Naive Ideas (Pre->Delayed-Post)")
abline(v=0.45,col="red",lty=2)

#par(mfrow=c(1,2))



###CINS/CANS###LOOK CAREFULLY.  NOT FINISHED YET###  PROBLEM WITH THIS, NOT ALL PARAMETERS
m.cins.race = lmer(scale(cins.both) ~ semester.both*time.both + skc.both.pre + rkc.both.pre +
                     snaive.both.pre + rnaive.both.pre + esl.both + reading.both + writing.both + 
                     bioclass.both + gender.both + age.both + race.both + level.both + plan.both + isea.both.pre+
                     race.both:semester.both:time.both+(1|id.both)) ##Model that uses standardized Y, for standardized Betas
summary(m.cins.race)###Compares races by semester.  #Sem effect,time,sem:time, then sem:race:time, no race 1st order


x=c(fixef(m.cins.beta)[5],fixef(m.cins.beta)[5]+fixef(m.cins.beta)[25:27], #White.
    fixef(m.cins.beta)[5]+fixef(m.cins.beta)[20],fixef(m.cins.beta)[5]+fixef(m.cins.beta)[25:27]+fixef(m.cins.beta)[5]
    ) #Creates Betas (Using CG = reference)
names(x)=NULL

x=c(fixef(m.ni.race)[5], fixef(m.ni.race)[5]+fixef(m.ni.race)[22:24],  #White.  Start with Post
    fixef(m.ni.race)[5]+fixef(m.ni.race)[32],fixef(m.ni.race)[5]+fixef(m.ni.race)[22:24]+fixef(m.ni.race)[33:35], #Asian
    fixef(m.ni.race)[5]+fixef(m.ni.race)[44],fixef(m.ni.race)[5]+fixef(m.ni.race)[22:24]+fixef(m.ni.race)[45:47], #Other
    fixef(m.ni.race)[6], fixef(m.ni.race)[6]+fixef(m.ni.race)[25:27],  #White.  Delayed Post
    fixef(m.ni.race)[6]+fixef(m.ni.race)[36],fixef(m.ni.race)[6]+fixef(m.ni.race)[25:27]+fixef(m.ni.race)[37:39], #Asian
    fixef(m.ni.race)[6]+fixef(m.ni.race)[48],fixef(m.ni.race)[6]+fixef(m.ni.race)[25:27]+fixef(m.ni.race)[49:51] #Other
)   
#Creates Betas (Using CG = reference)
names(x)=NULL
x=abs(x)

group=rep(c("CG","EG1","EG2","EG3"),6)
group=factor(group,levels=c("EG3","EG2","EG1","CG"))
which=rep(c(rep("White",4),rep("Asian",4),rep("URM",4)),2)
which=factor(which,levels=c("Asian","White","URM"))
time=factor(c(rep("Post",12),rep("Delayed-Post",12)),levels=c("Post","Delayed-Post"))
levels(time)=c("Post","Delayed-Post")
#order(time)
mycolor=rep(c(rep("Dark Green",4),rep("Blue",4),rep("Red",4)),2)
df=data.frame(x,group,time,which,mycolor)
ord=c(12:9,4:1,8:5,24:21,16:13,20:17)
df1=df[ord,]

#Post
dotchart(df1$x[time=="Post"],labels=df1$which[time=="Post"],groups=df1$group[time=="Post"],xlim=c(0,1.5),
         color=c("purple","blue","darkgreen","red"),
         pch=c(17,18,15),cex=0.8,pt.cex=1.5,xlab="Standardized Beta",main="Effect Size Naive Ideas (Pre->Post)")

#Delayed-Post 
dotchart(df1$x[time=="Delayed-Post"],labels=df1$which[time=="Delayed-Post"],groups=df1$group[time=="Delayed-Post"],
         xlim=c(0,1.5), color=c("purple","blue","darkgreen","red"),pch=c(17,18,15),cex=0.8,pt.cex=1.5,
         xlab="Standardized Beta",main="Effect Size Naive Ideas (Pre->Delayed-Post)")

#Both
#group1=c(rep(c("CG ","EG1 ","EG2 ","EG3 "),3),rep(c("CG  ","EG1  ","EG2  ","EG3  "),3))
#group1=factor(group1,levels = c("CG ","EG1 ","EG2 ","EG3 ","CG  ","EG1  ","EG2  ","EG3  "))
#df1=cbind(df,group1)
#dotchart(df1$x,labels=df1$which,groups=df1$group1,xlim=c(0,1.5),
#         pch=c(17,18,15),cex=0.8,pt.cex=1.5,xlab="Standardized Beta",main="Effect Size Key Concepts")
#abline(h=19.5,col="red")

#Add color=mycolor  to get the colors. Different color for each race.  Necessary?


##Grouped by race, then semester
#Post
dotchart(df$x[time=="Post"],labels=df$group[time=="Post"],groups=df$which[time=="Post"],xlim=c(0,1.5),
         color=c("red","darkgreen","blue","purple"),pch=c(rep(17,4),rep(18,4),rep(15,4)),
         cex=0.8,pt.cex=1.5,xlab="Standardized Beta",main="Effect Size Naive Ideas (Pre->Post)")

#Delayed-Post 
dotchart(df$x[time=="Delayed-Post"],labels=df$group[time=="Delayed-Post"],groups=df$which[time=="Delayed-Post"],
         xlim=c(0,1.5), color=c("red","darkgreen","blue","purple"),pch=c(rep(17,4),rep(18,4),rep(15,4)),cex=0.8,
         pt.cex=1.5,xlab="Standardized Beta",main="Effect Size Naive Ideas (Pre->Delayed-Post)")

#par(mfrow=c(1,2))



###MODEL CONSISTENCY###
m.mc.race = glmer(alltime.tmodel.sci ~ alltime.semester*timepoint + alltime.esl +
                    alltime.reading + alltime.writing + alltime.bioclass + alltime.gender +
                    alltime.age + alltime.level + alltime.plan + alltime.cins.pre + alltime.isea.pre + 
                    alltime.race:alltime.semester:timepoint +
                    (1|alltime.id),             #Random Intercept for ID *****USE THIS*****
                  family=binomial(link='logit'))
summary(m.mc.race)  ###Compares races by semester.  #Sem effect,time,sem:time, then sem:race:time, no race 1st order

x=c(fixef(m.mc.race)[5], fixef(m.mc.race)[5]+fixef(m.mc.race)[21:23],  #White.  Start with Post
    fixef(m.mc.race)[5]+fixef(m.mc.race)[31],fixef(m.mc.race)[5]+fixef(m.mc.race)[21:23]+fixef(m.mc.race)[32:34], #Asian
    fixef(m.mc.race)[5]+fixef(m.mc.race)[43],fixef(m.mc.race)[5]+fixef(m.mc.race)[21:23]+fixef(m.mc.race)[44:46], #Other
    fixef(m.mc.race)[6], fixef(m.mc.race)[6]+fixef(m.mc.race)[24:26],  #White.  Delayed Post
    fixef(m.mc.race)[6]+fixef(m.mc.race)[35],fixef(m.mc.race)[6]+fixef(m.mc.race)[24:26]+fixef(m.mc.race)[36:38], #Asian
    fixef(m.mc.race)[6]+fixef(m.mc.race)[47],fixef(m.mc.race)[6]+fixef(m.mc.race)[24:26]+fixef(m.mc.race)[48:50] #Other
)   
#Creates Betas (Using CG = reference)
names(x)=NULL
x=exp(x)

group=rep(c("CG","EG1","EG2","EG3"),6)
group=factor(group,levels=c("EG3","EG2","EG1","CG"))
which=rep(c(rep("White",4),rep("Asian",4),rep("URM",4)),2)
which=factor(which,levels=c("Asian","White","URM"))
time=factor(c(rep("Post",12),rep("Delayed-Post",12)),levels=c("Post","Delayed-Post"))
levels(time)=c("Post","Delayed-Post")
#order(time)
mycolor=rep(c(rep("Dark Green",4),rep("Blue",4),rep("Red",4)),2)
df=data.frame(x,group,time,which,mycolor)
ord=c(12:9,4:1,8:5,24:21,16:13,20:17)
df1=df[ord,]

##Grouped by semester, then race

#Post
dotchart(df1$x[time=="Post"],labels=df1$which[time=="Post"],groups=df1$group[time=="Post"],xlim=c(0,55),
         color=c("purple","blue","darkgreen","red"),
         pch=c(17,18,15),cex=0.8,pt.cex=1.5,xlab="Odds Ratios",main="Model Consistency")

#Delayed-Post 
dotchart(df1$x[time=="Delayed-Post"],labels=df1$which[time=="Delayed-Post"],groups=df1$group[time=="Delayed-Post"],
         xlim=c(0,55), color=c("purple","blue","darkgreen","red"),pch=c(17,18,15),cex=0.8,pt.cex=1.5,
         xlab="Odds Ratios",main="Model Consistency")

#Both
#group1=c(rep(c("CG ","EG1 ","EG2 ","EG3 "),3),rep(c("CG  ","EG1  ","EG2  ","EG3  "),3))
#group1=factor(group1,levels = c("CG ","EG1 ","EG2 ","EG3 ","CG  ","EG1  ","EG2  ","EG3  "))
#df1=cbind(df,group1)
#dotchart(df1$x,labels=df1$which,groups=df1$group1,xlim=c(0,1.5),
#         pch=c(17,18,15),cex=0.8,pt.cex=1.5,xlab="Standardized Beta",main="Effect Size Key Concepts")
#abline(h=19.5,col="red")

#Add color=mycolor  to get the colors. Different color for each race.  Necessary?


##Grouped by race, then semester
#Post
dotchart(df$x[time=="Post"],labels=df$group[time=="Post"],groups=df$which[time=="Post"],xlim=c(0,55),
         color=c("red","darkgreen","blue","purple"),pch=c(rep(17,4),rep(18,4),rep(15,4)),
         cex=0.8,pt.cex=1.5,xlab="Odds Ratios",main="Model Consistency")

#Delayed-Post 
dotchart(df$x[time=="Delayed-Post"],labels=df$group[time=="Delayed-Post"],groups=df$which[time=="Delayed-Post"],
         xlim=c(0,55), color=c("red","darkgreen","blue","purple"),pch=c(rep(17,4),rep(18,4),rep(15,4)),cex=0.8,
         pt.cex=1.5,xlab="Odds Ratios",main="Model Consistency")

#Log Scale Plot
plot(1, type="n", xlab="", ylab="", xlim=c(1, 55),xaxt="n",log="x")
axis(1, at = seq(5, 55, by = 5), las=1)
dotchart(log(df$x[time=="Post"]),labels=df$group[time=="Post"],groups=df$which[time=="Post"],xlim=c(0,log(55)),
         color=c("red","darkgreen","blue","purple"),pch=c(rep(17,4),rep(18,4),rep(15,4)),
         cex=0.8,pt.cex=1.5,xlab="Odds Ratios",main="Scientific Model Consistency")

dotchart(log(df$x[time=="Delayed-Post"]),labels=df$group[time=="Delayed-Post"],groups=df$which[time=="Delayed-Post"],
         xlim=c(0,log(55)), color=c("red","darkgreen","blue","purple"),pch=c(rep(17,4),rep(18,4),rep(15,4)),cex=0.8,
         pt.cex=1.5,xlab="Odds Ratios",main="Scientific Model Consistency")



###########GENDER############

alltime.allgender[alltime.allgender==3]=NA
alltime.gender[alltime.gender==3]=NA


###Key Concepts###
m.kc.gender = lmer(scale(alltime.allkc) ~ alltime.allsemester*all.timepoint + alltime.type + alltime.allesl +
                   alltime.allreading + alltime.allwriting + alltime.allbioclass + alltime.allrace +
                   alltime.allage  + alltime.alllevel + alltime.allplan + alltime.allcins.pre + 
                   alltime.allisea.pre + alltime.allgender:alltime.allsemester:all.timepoint +
                   (1|alltime.allid))  ##Model that uses standardized Y, for standardized Betas
summary(m.kc.gender)  ###Compares genders by semester.  #Sem effect,time,sem:time, then sem:gender:time, no gender 1st order

x=c(fixef(m.kc.gender)[5], fixef(m.kc.gender)[5]+fixef(m.kc.gender)[22:24],  #Male.  Start with Post
    fixef(m.kc.gender)[5]+fixef(m.kc.gender)[32],fixef(m.kc.gender)[5]+fixef(m.kc.gender)[22:24]+fixef(m.kc.gender)[33:35], #Female
    fixef(m.kc.gender)[6], fixef(m.kc.gender)[6]+fixef(m.kc.gender)[25:27],  #Male.  Delayed Post
    fixef(m.kc.gender)[6]+fixef(m.kc.gender)[36],fixef(m.kc.gender)[6]+fixef(m.kc.gender)[25:27]+fixef(m.kc.gender)[37:39] #Female
)   
#Creates Betas (Using CG = reference)
names(x)=NULL

group=rep(c("CG","EG1","EG2","EG3"),4)
group=factor(group,levels=c("EG3","EG2","EG1","CG"))
which=rep(c(rep("Male",4),rep("Female",4)),2)
which=factor(which,levels=c("Female","Male"))
time=factor(c(rep("Post",8),rep("Delayed-Post",8)),levels=c("Post","Delayed-Post"))
levels(time)=c("Post","Delayed-Post")
df=data.frame(x,group,time,which)


##Grouped by semester, then gender

#Post
dotchart(df$x[time=="Post"],labels=df$which[time=="Post"],groups=df$group[time=="Post"],xlim=c(0,1.5),
         color=c("red","darkgreen","blue","purple"),
         pch=c(17,15),cex=1,pt.cex=1.6,xlab="Standardized Beta",main="Effect Size Key Concepts (Pre->Post)")
abline(v=0.45,col="red",lty=2)

#Delayed-Post 
dotchart(df$x[time=="Delayed-Post"],labels=df$which[time=="Delayed-Post"],groups=df$group[time=="Delayed-Post"],
         xlim=c(0,1.5), color=c("red","darkgreen","blue","purple"),pch=c(17,15),cex=1,pt.cex=1.6,
         xlab="Standardized Beta",main="Effect Size Key Concepts (Pre->Delayed-Post)")
abline(v=0.45,col="red",lty=2)

#Both
#group1=c(rep(c("CG ","EG1 ","EG2 ","EG3 "),3),rep(c("CG  ","EG1  ","EG2  ","EG3  "),3))
#group1=factor(group1,levels = c("CG ","EG1 ","EG2 ","EG3 ","CG  ","EG1  ","EG2  ","EG3  "))
#df1=cbind(df,group1)
#dotchart(df1$x,labels=df1$which,groups=df1$group1,xlim=c(0,1.5),
#         pch=c(17,18,15),cex=0.8,pt.cex=1.5,xlab="Standardized Beta",main="Effect Size Key Concepts")
#abline(h=19.5,col="red")

#Add color=mycolor  to get the colors. Different color for each race.  Necessary?


##Grouped by gender, then semester
#Post
dotchart(df$x[time=="Post"],labels=df$group[time=="Post"],groups=df$which[time=="Post"],xlim=c(0,1.5),
         color=c("red","darkgreen","blue","purple"),pch=c(rep(17,4),rep(15,4)),
         cex=1,pt.cex=1.6,xlab="Standardized Beta",main="Effect Size Key Concepts (Pre->Post)")
abline(v=0.45,col="red",lty=2)

#Delayed-Post 
dotchart(df$x[time=="Delayed-Post"],labels=df$group[time=="Delayed-Post"],groups=df$which[time=="Delayed-Post"],
         xlim=c(0,1.5), color=c("red","darkgreen","blue","purple"),pch=c(rep(17,4),rep(15,4)),cex=1,
         pt.cex=1.6,xlab="Standardized Beta",main="Effect Size Key Concepts (Pre->Delayed-Post)")
abline(v=0.45,col="red",lty=2)

#par(mfrow=c(1,2))



###Naive Ideas###
m.ni.gender = lmer(scale(alltime.allnaive) ~ alltime.allsemester*all.timepoint + alltime.type + alltime.allesl +
                     alltime.allreading + alltime.allwriting + alltime.allbioclass + alltime.allrace +
                     alltime.allage  + alltime.alllevel + alltime.allplan + alltime.allcins.pre + 
                     alltime.allisea.pre + alltime.allgender:alltime.allsemester:all.timepoint +
                     (1|alltime.allid))  ##Model that uses standardized Y, for standardized Betas
summary(m.ni.gender)  ###Compares genders by semester.  #Sem effect,time,sem:time, then sem:gender:time, no gender 1st order

x=c(fixef(m.ni.gender)[5], fixef(m.ni.gender)[5]+fixef(m.ni.gender)[22:24],  #Male.  Start with Post
    fixef(m.ni.gender)[5]+fixef(m.ni.gender)[32],fixef(m.ni.gender)[5]+fixef(m.ni.gender)[22:24]+fixef(m.ni.gender)[33:35], #Female
    fixef(m.ni.gender)[6], fixef(m.ni.gender)[6]+fixef(m.ni.gender)[25:27],  #Male.  Delayed Post
    fixef(m.ni.gender)[6]+fixef(m.ni.gender)[36],fixef(m.ni.gender)[6]+fixef(m.ni.gender)[25:27]+fixef(m.ni.gender)[37:39] #Female
)   
#Creates Betas (Using CG = reference)
names(x)=NULL
x=abs(x)

group=rep(c("CG","EG1","EG2","EG3"),4)
group=factor(group,levels=c("EG3","EG2","EG1","CG"))
which=rep(c(rep("Male",4),rep("Female",4)),2)
which=factor(which,levels=c("Female","Male"))
time=factor(c(rep("Post",8),rep("Delayed-Post",8)),levels=c("Post","Delayed-Post"))
levels(time)=c("Post","Delayed-Post")
df=data.frame(x,group,time,which)


##Grouped by semester, then gender

#Post
dotchart(df$x[time=="Post"],labels=df$which[time=="Post"],groups=df$group[time=="Post"],xlim=c(0,1.5),
         color=c("red","darkgreen","blue","purple"),
         pch=c(17,15),cex=1,pt.cex=1.6,xlab="Standardized Beta",main="Effect Size Naive Ideas (Pre->Post)")
abline(v=0.45,col="red",lty=2)

#Delayed-Post 
dotchart(df$x[time=="Delayed-Post"],labels=df$which[time=="Delayed-Post"],groups=df$group[time=="Delayed-Post"],
         xlim=c(0,1.5), color=c("red","darkgreen","blue","purple"),pch=c(17,15),cex=1,pt.cex=1.6,
         xlab="Standardized Beta",main="Effect Size Naive Ideas (Pre->Delayed-Post)")
abline(v=0.45,col="red",lty=2)

#Both
#group1=c(rep(c("CG ","EG1 ","EG2 ","EG3 "),3),rep(c("CG  ","EG1  ","EG2  ","EG3  "),3))
#group1=factor(group1,levels = c("CG ","EG1 ","EG2 ","EG3 ","CG  ","EG1  ","EG2  ","EG3  "))
#df1=cbind(df,group1)
#dotchart(df1$x,labels=df1$which,groups=df1$group1,xlim=c(0,1.5),
#         pch=c(17,18,15),cex=0.8,pt.cex=1.5,xlab="Standardized Beta",main="Effect Size Key Concepts")
#abline(h=19.5,col="red")

#Add color=mycolor  to get the colors. Different color for each race.  Necessary?


##Grouped by gender, then semester
#Post
dotchart(df$x[time=="Post"],labels=df$group[time=="Post"],groups=df$which[time=="Post"],xlim=c(0,1.5),
         color=c("red","darkgreen","blue","purple"),pch=c(rep(17,4),rep(15,4)),
         cex=1,pt.cex=1.6,xlab="Standardized Beta",main="Effect Size Naive Ideas (Pre->Post)")
abline(v=0.45,col="red",lty=2)

#Delayed-Post 
dotchart(df$x[time=="Delayed-Post"],labels=df$group[time=="Delayed-Post"],groups=df$which[time=="Delayed-Post"],
         xlim=c(0,1.5), color=c("red","darkgreen","blue","purple"),pch=c(rep(17,4),rep(15,4)),cex=1,
         pt.cex=1.6,xlab="Standardized Beta",main="Effect Size Naive Ideas (Pre->Delayed-Post)")
abline(v=0.45,col="red",lty=2)

#par(mfrow=c(1,2))



###Model Consistency###
m.mc.gender = glmer(alltime.tmodel.sci ~ alltime.semester*timepoint + alltime.esl +
                    alltime.reading + alltime.writing + alltime.bioclass + alltime.race +
                    alltime.age + alltime.level + alltime.plan + alltime.cins.pre + alltime.isea.pre + 
                    alltime.gender:alltime.semester:timepoint +
                    (1|alltime.id),             #Random Intercept for ID *****USE THIS*****
                  family=binomial(link='logit'))
summary(m.mc.gender)  ###Compares genders by semester.  #Sem effect,time,sem:time, then sem:gender:time, no gender 1st order


x=c(fixef(m.mc.gender)[5], fixef(m.mc.gender)[5]+fixef(m.mc.gender)[21:23],  #Male.  Start with Post
    fixef(m.mc.gender)[5]+fixef(m.mc.gender)[31],fixef(m.mc.gender)[5]+fixef(m.mc.gender)[21:23]+fixef(m.mc.gender)[32:34], #Female
    fixef(m.mc.gender)[6], fixef(m.mc.gender)[6]+fixef(m.mc.gender)[24:26],  #Male.  Delayed Post
    fixef(m.mc.gender)[6]+fixef(m.mc.gender)[35],fixef(m.mc.gender)[6]+fixef(m.mc.gender)[24:26]+fixef(m.mc.gender)[36:38] #Female
)   
#Creates Betas (Using CG = reference)
names(x)=NULL
x=exp(x)

group=rep(c("CG","EG1","EG2","EG3"),4)
group=factor(group,levels=c("EG3","EG2","EG1","CG"))
which=rep(c(rep("Male",4),rep("Female",4)),2)
which=factor(which,levels=c("Female","Male"))
time=factor(c(rep("Post",8),rep("Delayed-Post",8)),levels=c("Post","Delayed-Post"))
levels(time)=c("Post","Delayed-Post")
df=data.frame(x,group,time,which)


##Grouped by semester, then gender

#Post
dotchart(df$x[time=="Post"],labels=df$which[time=="Post"],groups=df$group[time=="Post"],xlim=c(0,50),
         color=c("red","darkgreen","blue","purple"),
         pch=c(17,15),cex=1,pt.cex=1.6,xlab="Odds Ratios",main="Model Consistency")

#Delayed-Post 
dotchart(df$x[time=="Delayed-Post"],labels=df$which[time=="Delayed-Post"],groups=df$group[time=="Delayed-Post"],
         xlim=c(0,50), color=c("red","darkgreen","blue","purple"),pch=c(17,15),cex=1,pt.cex=1.6,
         xlab="Odds Ratios",main="Model Consistency")

#Both
#group1=c(rep(c("CG ","EG1 ","EG2 ","EG3 "),3),rep(c("CG  ","EG1  ","EG2  ","EG3  "),3))
#group1=factor(group1,levels = c("CG ","EG1 ","EG2 ","EG3 ","CG  ","EG1  ","EG2  ","EG3  "))
#df1=cbind(df,group1)
#dotchart(df1$x,labels=df1$which,groups=df1$group1,xlim=c(0,1.5),
#         pch=c(17,18,15),cex=0.8,pt.cex=1.5,xlab="Standardized Beta",main="Effect Size Key Concepts")
#abline(h=19.5,col="red")

#Add color=mycolor  to get the colors. Different color for each race.  Necessary?


##Grouped by gender, then semester
#Post
dotchart(df$x[time=="Post"],labels=df$group[time=="Post"],groups=df$which[time=="Post"],xlim=c(0,50),
         color=c("red","darkgreen","blue","purple"),pch=c(rep(17,4),rep(15,4)),
         cex=1,pt.cex=1.6,xlab="Odds Ratios",main="Model Consistency")

#Delayed-Post 
dotchart(df$x[time=="Delayed-Post"],labels=df$group[time=="Delayed-Post"],groups=df$which[time=="Delayed-Post"],
         xlim=c(0,50), color=c("red","darkgreen","blue","purple"),pch=c(rep(17,4),rep(15,4)),cex=1,
         pt.cex=1.6,xlab="Odds Ratios",main="Model Consistency")

#Log Scale Plot
plot(1, type="n", xlab="", ylab="", xlim=c(1, 50),xaxt="n",log="x")
axis(1, at = seq(5, 50, by = 5), las=1)

dotchart(log(df$x[time=="Post"]),labels=df$group[time=="Post"],groups=df$which[time=="Post"],xlim=c(0,log(50)),
         color=c("red","darkgreen","blue","purple"),pch=c(rep(17,4),rep(15,4)),
         cex=1,pt.cex=1.6,xlab="Odds Ratios",main="Scientific Model Consistency")

dotchart(log(df$x[time=="Delayed-Post"]),labels=df$group[time=="Delayed-Post"],groups=df$which[time=="Delayed-Post"],
         xlim=c(0,log(50)), color=c("red","darkgreen","blue","purple"),pch=c(rep(17,4),rep(15,4)),cex=1,
         pt.cex=1.6,xlab="Odds Ratios",main="Scientific Model Consistency")















##Cohen's D and Standardized Beta in one chart##
library(lattice)

dotplot(df$which ~ df$x, groups = df$group)


library(ggplot2)

qplot(df$x,df$which)




(tapply(skc.post,semester,mean,na.rm=T)-tapply(skc.pre,semester,mean,na.rm=T))/
  ((tapply(skc.post,semester,sd,na.rm=T)+tapply(skc.pre,semester,sd,na.rm=T))/2)





library(effsize)
###Effect Sizes of each Treatment group individually###
x=0
for(i in c("a","b","c","d")){
  x[i]=cohen.d(skc.post[semester==i],skc.pre[semester==i],paired=T,na.rm=T)$estimate
}

d1.0=cohen.d(skc.ex1[semester=="a"],skc.pre[semester=="a"],paired=T,na.rm=T)$estimate



x=0
for(i in c("a","b","c","d")){
  x[i]=mean(skc.post[semester==i]-skc.pre[semester==i],na.rm=T)/
    sd(skc.post[semester==i]-skc.pre[semester==i],na.rm=T)
}



###Cohen's D to compare to Std Beta###
(tapply(alltime.allkc[all.timepoint=="post"&&alltime.allrace==3],
       alltime.allsemester[all.timepoint=="post"&&alltime.allrace==3],mean,na.rm=T) - 
  tapply(alltime.allkc[all.timepoint=="pre"&&alltime.allrace==3],
         alltime.allsemester[all.timepoint=="pre"&&alltime.allrace==3],mean,na.rm=T)) / 
  (tapply(alltime.allkc[all.timepoint=="post"&&alltime.allrace==3]-
           alltime.allkc[all.timepoint=="pre"&&alltime.allrace==3],sd,na.rm=T))

