####RUN ALL CODE FROM 4 GROUPS INDIVIDUAL KC NI NETWORKS.R FIRST####

#Previous document only looked at KC network.  This one will look at KC&NI combined networks
library(igraph)
library(lme4)
library(lmerTest)

animal.all.ind=cbind(skc.all.ind,snaive.all.ind)
animal0.ind=cbind(skc0.ind,snaive0.ind)
animal1.ind=cbind(skc1.ind,snaive1.ind)
animal2.ind=cbind(skc2.ind,snaive2.ind)
plant.all.ind=cbind(rkc.all.ind,rnaive.all.ind)
plant0.ind=cbind(rkc0.ind,rnaive0.ind)
plant1.ind=cbind(rkc1.ind,rnaive1.ind)
plant2.ind=cbind(rkc2.ind,rnaive2.ind)


###ONLY RUN THIS FOR REVERSE CODING NAIVE IDEAS
#animal0.ind[,7:9]=1-animal0.ind[,7:9]
#animal1.ind[,7:9]=1-animal1.ind[,7:9]
#animal2.ind[,7:9]=1-animal2.ind[,7:9]
#plant0.ind[,7:9]=1-plant0.ind[,7:9]
#plant1.ind[,7:9]=1-plant1.ind[,7:9]
#plant2.ind[,7:9]=1-plant2.ind[,7:9]




#Create function to calculate Harmonic Centrality
harmonic=function(mat){
  net=graph_from_adjacency_matrix(1/mat, mode = "lower", weighted = T, diag = FALSE,
                                  add.colnames = NULL, add.rownames = NA)
  dij=shortest.paths(net)
  diag(dij)=Inf
  rowSums(1/dij)
}


#Create function to calculate Closeness Centrality
closecent=function(mat){
  net=graph_from_adjacency_matrix(1/mat, mode = "lower", weighted = T, diag = FALSE,
                                  add.colnames = NULL, add.rownames = NA)
  closeness(net)
}



####Class-Level Networks#### Animal/Plant Separate (Only coded once, see below)


#Pre
mymat=matrix(0,nrow=9,ncol=9); mymat2=mymat; mymat3=mymat; mymat4=mymat
sem=c(rep(1,length(cins.spring15.pre)),rep(2,length(cins.spring16.pre)),
      rep(3,length(cins.spring17.pre)),rep(4,length(cans.fall17.pre)))
M.pre=list(mymat,mymat2,mymat3,mymat4)
H.pre=matrix(0,nrow=4,ncol=9)
C.pre=matrix(0,nrow=4,ncol=9)
for(m in 1:4){
  n=length(skc0.ind[sem==m,1])
  for(k in 1:n){
    for(i in 1:9){
      for(j in 1:9){  
        if (sum(animal0.ind[sem==m,][k,i],animal0.ind[sem==m,][k,j],na.rm=T)==2){
          M.pre[[m]][i,j]=M.pre[[m]][i,j]+1
        }
      }
    }
  }
  colnames(M.pre[[m]])=c("Var","Her","Comp","Ltd","Diff","Non","Ada","Need","Use")
  rownames(M.pre[[m]])=colnames(M.pre[[m]])
  H.pre[m,]=harmonic(M.pre[[m]])/(length(na.omit(animal0.ind[sem==m,1]))*8)
  C.pre[m,]=closecent(M.pre[[m]])*8/length(na.omit(animal0.ind[sem==m,1]))
}
colnames(H.pre)=c("Var","Her","Comp","Ltd","Diff","Non","Ada","Need","Use")
colnames(C.pre)=colnames(H.pre)
M.pre
round(H.pre,4)
round(C.pre,4)

#Post
mymat=matrix(0,nrow=9,ncol=9); mymat2=mymat; mymat3=mymat; mymat4=mymat
sem=c(rep(1,length(cins.spring15.pre)),rep(2,length(cins.spring16.pre)),
      rep(3,length(cins.spring17.pre)),rep(4,length(cans.fall17.pre)))
M.post=list(mymat,mymat2,mymat3,mymat4)
H.post=matrix(0,nrow=4,ncol=9)
C.post=matrix(0,nrow=4,ncol=9)
for(m in 1:4){
  n=length(animal1.ind[sem==m,1])
  for(k in 1:n){
    for(i in 1:9){
      for(j in 1:9){  
        if (sum(animal1.ind[sem==m,][k,i],animal1.ind[sem==m,][k,j],na.rm=T)==2){
          M.post[[m]][i,j]=M.post[[m]][i,j]+1
        }
      }
    }
  }
  colnames(M.post[[m]])=c("Var","Her","Comp","Ltd","Diff","Non","Ada","Need","Use")
  rownames(M.post[[m]])=colnames(M.post[[m]])
  H.post[m,]=harmonic(M.post[[m]])/(length(na.omit(animal1.ind[sem==m,1]))*8)
  C.post[m,]=closecent(M.post[[m]])*8/length(na.omit(animal1.ind[sem==m,1]))
}
colnames(H.post)=c("Var","Her","Comp","Ltd","Diff","Non","Ada","Need","Use")
colnames(C.post)=colnames(H.post)
M.post
round(H.post,4)
round(C.post,4)

#Delayed-Post
mymat=matrix(0,nrow=9,ncol=9); mymat2=mymat; mymat3=mymat; mymat4=mymat
sem=c(rep(1,length(cins.spring15.pre)),rep(2,length(cins.spring16.pre)),
      rep(3,length(cins.spring17.pre)),rep(4,length(cans.fall17.pre)))
M.dpost=list(mymat,mymat2,mymat3,mymat4)
H.dpost=matrix(0,nrow=4,ncol=9)
C.dpost=matrix(0,nrow=4,ncol=9)
for(m in 1:4){
  n=length(animal2.ind[sem==m,1])
  for(k in 1:n){
    for(i in 1:9){
      for(j in 1:9){  
        if (sum(animal2.ind[sem==m,][k,i],animal2.ind[sem==m,][k,j],na.rm=T)==2){
          M.dpost[[m]][i,j]=M.dpost[[m]][i,j]+1
        }
      }
    }
  }
  colnames(M.dpost[[m]])=c("Var","Her","Comp","Ltd","Diff","Non","Ada","Need","Use")
  rownames(M.dpost[[m]])=colnames(M.dpost[[m]])
  H.dpost[m,]=harmonic(M.dpost[[m]])/(length(na.omit(animal2.ind[sem==m,1]))*8)
  C.dpost[m,]=closecent(M.dpost[[m]])*8/length(na.omit(animal2.ind[sem==m,1]))
}
colnames(H.dpost)=c("Var","Her","Comp","Ltd","Diff","Non","Ada","Need","Use")
colnames(C.dpost)=colnames(H.dpost)
M.dpost
round(H.dpost,4)
round(C.dpost,4)


##Output Mean Centrality by class and by Animal/Plant
round(rowMeans(H.pre[,1:6]),4)
round(rowMeans(H.pre[,7:9]),4)
round(rowMeans(H.post[,1:6]),4)
round(rowMeans(H.post[,7:9]),4)
round(rowMeans(H.dpost[,1:6]),4)
round(rowMeans(H.dpost[,7:9]),4)
round(rowMeans(C.pre[,1:6]),4)
round(rowMeans(C.pre[,7:9]),4)
round(rowMeans(C.post[,1:6]),4)
round(rowMeans(C.post[,7:9]),4)
round(rowMeans(C.dpost[,1:6]),4)
round(rowMeans(C.dpost[,7:9]),4)


#run these to avoid extra code for plant.  this also assigns animal values to M,H,C
animal0.ind=plant0.ind ; animal1.ind=plant1.ind ; animal2.ind=plant2.ind
H.pre.a=H.pre ; H.post.a=H.post ; H.dpost.a=H.dpost
C.pre.a=C.pre ; C.post.a=C.post ; C.dpost.a=C.dpost
M.pre.a=M.pre ; M.post.a=M.post ; M.dpost.a=M.dpost





########MANOVA to compare centralities across semesters######## 



####Individual Level (Reimport everything below since I made animal=plant above)
animal.all.ind=cbind(skc.all.ind,snaive.all.ind)
animal0.ind=cbind(skc0.ind,snaive0.ind)
animal1.ind=cbind(skc1.ind,snaive1.ind)
animal2.ind=cbind(skc2.ind,snaive2.ind)
plant.all.ind=cbind(rkc.all.ind,rnaive.all.ind)
plant0.ind=cbind(rkc0.ind,rnaive0.ind)
plant1.ind=cbind(rkc1.ind,rnaive1.ind)
plant2.ind=cbind(rkc2.ind,rnaive2.ind)

###ONLY RUN THIS FOR REVERSE CODING NAIVE IDEAS
animal0.ind[,7:9]=1-animal0.ind[,7:9]
animal1.ind[,7:9]=1-animal1.ind[,7:9]
animal2.ind[,7:9]=1-animal2.ind[,7:9]
plant0.ind[,7:9]=1-plant0.ind[,7:9]
plant1.ind[,7:9]=1-plant1.ind[,7:9]
plant2.ind[,7:9]=1-plant2.ind[,7:9]


#######Loop to create co-occurrence matrices at the individual level
#####And matrices for Harmonic Centrality.  No Closeness Centrality because disconnected
#Pre
sem=c(rep(1,length(cins.spring15.pre)),rep(2,length(cins.spring16.pre)),
      rep(3,length(cins.spring17.pre)),rep(4,length(cans.fall17.pre)))
n=length(animal0.ind[,1])
H.all.pre=matrix(0,nrow=n,ncol=9)
for(k in 1:n){
  mat.ind=matrix(0,nrow=9,ncol=9)
  for(i in 1:9){
    for(j in 1:9){  
      if (sum(animal0.ind[k,i],animal0.ind[k,j],na.rm=T)==2){
        mat.ind[i,j]=mat.ind[i,j]+1
      } else if(is.na(sum(animal0.ind[k,i],animal0.ind[k,j]))){
        mat.ind[i,j]=NA
      }
    }
  }
  H.all.pre[k,]=harmonic(mat.ind)/8
}
colnames(H.all.pre)=c("Var","Her","Comp","Ltd","Diff","Non","Ada","Need","Use")


#Post
sem=c(rep(1,length(cins.spring15.pre)),rep(2,length(cins.spring16.pre)),
      rep(3,length(cins.spring17.pre)),rep(4,length(cans.fall17.pre)))
n=length(animal1.ind[,1])
H.all.post=matrix(0,nrow=n,ncol=9)
for(k in 1:n){
  mat.ind=matrix(0,nrow=9,ncol=9)
  for(i in 1:9){
    for(j in 1:9){  
      if (sum(animal1.ind[k,i],animal1.ind[k,j],na.rm=T)==2){
        mat.ind[i,j]=mat.ind[i,j]+1
      } else if(is.na(sum(animal1.ind[k,i],animal1.ind[k,j]))){
        mat.ind[i,j]=NA
      }
    }
  }
  H.all.post[k,]=harmonic(mat.ind)/8
}
colnames(H.all.post)=c("Var","Her","Comp","Ltd","Diff","Non","Ada","Need","Use")


#Delayed-Post
sem=c(rep(1,length(cins.spring15.post)),rep(2,length(cins.spring16.post)),
      rep(3,length(cins.spring17.post)),rep(4,length(cans.fall17.post)))
n=length(animal2.ind[,1])
H.all.dpost=matrix(0,nrow=n,ncol=9)
for(k in 1:n){
  mat.ind=matrix(0,nrow=9,ncol=9)
  for(i in 1:9){
    for(j in 1:9){  
      if (sum(animal2.ind[k,i],animal2.ind[k,j],na.rm=T)==2){
        mat.ind[i,j]=mat.ind[i,j]+1
      } else if(is.na(sum(animal2.ind[k,i],animal2.ind[k,j]))){
        mat.ind[i,j]=NA
      }
    }
  }
  H.all.dpost[k,]=harmonic(mat.ind)/8
}
colnames(H.all.dpost)=c("Var","Her","Comp","Ltd","Diff","Non","Ada","Need","Use")



##Output Mean Centrality by class/time. Repeat for Animal/Plant
#Each KC NI
avg.har.pre=matrix(0,nrow=4,ncol=9); avg.har.post=avg.har.pre; avg.har.dpost=avg.har.pre
for(i in 1:4){
  avg.har.pre[i,]=colMeans(H.all.pre[sem==i,],na.rm=T)
  avg.har.post[i,]=colMeans(H.all.post[sem==i,],na.rm=T)
  avg.har.dpost[i,]=colMeans(H.all.dpost[sem==i,],na.rm=T)
}

sd.har.pre=matrix(0,nrow=4,ncol=9); sd.har.post=sd.har.pre; sd.har.dpost=sd.har.pre
for(i in 1:4){
  sd.har.pre[i,]=apply(H.all.pre[sem==i,],2,sd,na.rm=T)
  sd.har.post[i,]=apply(H.all.post[sem==i,],2,sd,na.rm=T)
  sd.har.dpost[i,]=apply(H.all.dpost[sem==i,],2,sd,na.rm=T)
}

round(avg.har.pre,4)
round(avg.har.post,4)
round(avg.har.dpost,4)

round(sd.har.pre,4)
round(sd.har.post,4)
round(sd.har.dpost,4)



#Average KC, Average NI, and SD's
tapply(rowMeans(H.all.pre[,1:6]),sem,mean,na.rm=T)  #KC
tapply(rowMeans(H.all.pre[,7:9]),sem,mean,na.rm=T)  #NI
tapply(rowMeans(H.all.post[,1:6]),sem,mean,na.rm=T)  #KC
tapply(rowMeans(H.all.post[,7:9]),sem,mean,na.rm=T)  #NI
tapply(rowMeans(H.all.dpost[,1:6]),sem,mean,na.rm=T)  #KC
tapply(rowMeans(H.all.dpost[,7:9]),sem,mean,na.rm=T)  #NI
tapply(rowMeans(H.all.pre[,1:6]),sem,sd,na.rm=T)  #KC
tapply(rowMeans(H.all.pre[,7:9]),sem,sd,na.rm=T)  #NI
tapply(rowMeans(H.all.post[,1:6]),sem,sd,na.rm=T)  #KC
tapply(rowMeans(H.all.post[,7:9]),sem,sd,na.rm=T)  #NI
tapply(rowMeans(H.all.dpost[,1:6]),sem,sd,na.rm=T)  #KC
tapply(rowMeans(H.all.dpost[,7:9]),sem,sd,na.rm=T)  #NI



#run these to avoid extra code for plant.  this also assigns animal values to M,H,C
animal0.ind=plant0.ind ; animal1.ind=plant1.ind ; animal2.ind=plant2.ind
H.all.pre.a=H.all.pre ; H.all.post.a=H.all.post ; H.all.dpost.a=H.all.dpost
avg.har.pre.a=avg.har.pre ; avg.har.post.a=avg.har.post ; avg.har.dpost.a=avg.har.dpost
sd.har.pre.a=sd.har.pre ; sd.har.post.a=sd.har.post ; sd.har.dpost.a=sd.har.dpost





###Use the individual student's centralities to compare semesters###
colnames(H.all.pre.a)=c("Var","Her","Comp","Ltd","Diff","Non","Ada","Need","Use")
colnames(H.all.post.a)=colnames(H.all.pre.a);  colnames(H.all.dpost.a)=colnames(H.all.pre.a)
level=as.numeric(level)
plan=as.numeric(plan)


#A) Manova on 18 variables for each time point
m1=lm(cbind(H.all.pre,H.all.pre.a)~semester)
anova(m1)
summary(m1)

m2=lm(cbind(H.all.post,H.all.post.a)~cbind(H.all.pre,H.all.pre.a)+semester)
anova(m2)
summary(m2)

m3=lm(cbind(H.all.dpost,H.all.dpost.a)~cbind(H.all.pre,H.all.pre.a)+semester)
anova(m3)
summary(m3)


#B1) Manova on 2 variables, mean KC & mean NI, for all 3 time points.  (Animal/Plant combined)
#This is the one in my document
H.allkc.mean.pre=rowSums(H.all.pre[,1:6]+H.all.pre.a[,1:6])/12
H.allni.mean.pre=rowSums(H.all.pre[,7:9]+H.all.pre.a[,7:9])/6
H.allkc.mean.post=rowSums(H.all.post[,1:6]+H.all.post.a[,1:6])/12
H.allni.mean.post=rowSums(H.all.post[,7:9]+H.all.post.a[,7:9])/6
H.allkc.mean.dpost=rowSums(H.all.dpost[,1:6]+H.all.dpost.a[,1:6])/12
H.allni.mean.dpost=rowSums(H.all.dpost[,7:9]+H.all.dpost.a[,7:9])/6

m1.1=lm(cbind(H.allkc.mean.pre,H.allni.mean.pre)~semester)
anova(m1.1)
summary(m1.1)

m2.1=lm(cbind(H.allkc.mean.post,H.allni.mean.post)~cbind(H.allkc.mean.pre,H.allni.mean.pre)+semester)
anova(m2.1)
summary(m2.1)

m3.1=lm(cbind(H.allkc.mean.dpost,H.allni.mean.dpost)~cbind(H.allkc.mean.pre,H.allni.mean.pre)+semester)
anova(m3.1)
summary(m3.1)


#B2) Anova/Regression on separately for mean KC, mean NI, for all time points. Post~Pre; DP~Pre
#Not the one in my document at the moment
m1.2=lm(H.allkc.mean.pre~semester)
anova(m1.2)
summary(m1.2)

m2.2=lm(H.allkc.mean.post~H.allkc.mean.pre+semester)
anova(m2.2)
summary(m2.2)

m3.2=lm(H.allkc.mean.dpost~H.allkc.mean.pre+semester)
anova(m3.2)
summary(m3.2)

m4.2=lm(H.allni.mean.pre~semester)
anova(m4.2)
summary(m4.2)

m5.2=lm(H.allni.mean.post~H.allni.mean.pre+semester)
anova(m5.2)
summary(m5.2)

m6.2=lm(H.allni.mean.dpost~H.allni.mean.pre+semester)
anova(m6.2)
summary(m6.2)


#C) Same as above, but with covariates. Post~Pre; DPost~Pre.  Separate models, not multivariate
m7.2=lm(H.allkc.mean.post~H.allkc.mean.pre+semester+esl+reading+writing+bioclass+gender+age+race+
          level+plan+cins.pre+isea.pre)
anova(m7.2)
summary(m7.2)

m8.2=lm(H.allkc.mean.dpost~H.allkc.mean.pre+semester+esl+reading+writing+bioclass+gender+age+race+
          level+plan+cins.pre+isea.pre)
anova(m8.2)
summary(m8.2)

m9.2=lm(H.allni.mean.post~H.allni.mean.pre+semester+esl+reading+writing+bioclass+gender+age+race+
          level+plan+cins.pre+isea.pre)
anova(m9.2)
summary(m9.2)

m10.2=lm(H.allni.mean.dpost~H.allni.mean.pre+semester+esl+reading+writing+bioclass+gender+age+race+
          level+plan+cins.pre+isea.pre)
anova(m10.2)
summary(m10.2)


#D) Mixed Model. Mean KC, Mean NI, combined Animal/Plant
H.alltime.kc.mean=c(H.allkc.mean.pre,H.allkc.mean.post,H.allkc.mean.dpost)
H.alltime.ni.mean=c(H.allni.mean.pre,H.allni.mean.post,H.allni.mean.dpost)
semester.all=rep(semester,3)
esl.all=rep(esl,3)
reading.all=rep(reading,3)
writing.all=rep(writing,3)
bioclass.all=rep(bioclass,3)
gender.all=rep(gender,3)
age.all=rep(age,3)
race.all=rep(race,3)
level.all=rep(level,3)
plan.all=rep(plan,3)
cins.pre.all=rep(cins.pre,3)
isea.pre.all=rep(isea.pre,3)
time=c(rep(0,length(cins.pre)),rep(1,length(cins.pre)),rep(2,length(cins.pre)))
time=factor(time,labels = c("pre","post","delpost"))
id=factor(rep(1:length(cins.pre),3))

mixed.kc.1=lmer(H.alltime.kc.mean ~ semester.all*time + esl.all + reading.all + writing.all +
                bioclass.all + gender.all + age.all + race.all + level.all + plan.all +
                  cins.pre.all + isea.pre.all +  (1|id))  #Random Intercept model
summary(mixed.kc.1)


mixed.ni.1=lmer(H.alltime.ni.mean ~ semester.all*time + esl.all + reading.all + writing.all +
                  bioclass.all + gender.all + age.all + race.all + level.all + plan.all +
                  cins.pre.all + isea.pre.all +  (1|id))  #Random Intercept model
summary(mixed.ni.1)


#E) Mixed Model. Mean KC, Mean NI, Animal/Plant separate mean calculations, added covariate for TYPE
#Reusing some of same code. be careful. rerun stuff at (B1) to do (B-D) over  ***USING FOR THESIS***
H.alltime.akc.mean=c(rowMeans(H.all.pre.a[,1:6]),rowMeans(H.all.post.a[,1:6]),
                     rowMeans(H.all.dpost.a[,1:6]))
H.alltime.ani.mean=c(rowMeans(H.all.pre.a[,7:9]),rowMeans(H.all.post.a[,7:9]),
                     rowMeans(H.all.dpost.a[,7:9]))
H.alltime.pkc.mean=c(rowMeans(H.all.pre[,1:6]),rowMeans(H.all.post[,1:6]),rowMeans(H.all.dpost[,1:6]))
H.alltime.pni.mean=c(rowMeans(H.all.pre[,7:9]),rowMeans(H.all.post[,7:9]),rowMeans(H.all.dpost[,7:9]))
H.alltime.allkc.mean=c(H.alltime.akc.mean,H.alltime.pkc.mean)#Use these as dependent
H.alltime.allni.mean=c(H.alltime.ani.mean,H.alltime.pni.mean)#
semester.all=rep(semester,6)
esl.all=rep(esl,6)
reading.all=rep(reading,6)
writing.all=rep(writing,6)
bioclass.all=rep(bioclass,6)
gender.all=rep(gender,6)
age.all=rep(age,6)
race.all=rep(race,6)
level.all=rep(level,6)
plan.all=rep(plan,6)
cins.pre.all=rep(cins.pre,6)
isea.pre.all=rep(isea.pre,6)
time=c(rep(0,length(cins.pre)),rep(1,length(cins.pre)),rep(2,length(cins.pre)))
time=factor(time,labels = c("pre","post","delpost"))
time=rep(time,2)
id=factor(rep(1:length(cins.pre),6))
type=c(rep("a",length(H.alltime.akc.mean)),rep("p",length(H.alltime.pkc.mean)))


mixed.kc.2=lmer(H.alltime.allkc.mean ~ semester.all*time + type + esl.all + reading.all + writing.all +
                  bioclass.all + gender.all + age.all + race.all + level.all + plan.all +
                  cins.pre.all + isea.pre.all +  (time|id))  #Random Intercept model
summary(mixed.kc.2)


mixed.ni.2=lmer(H.alltime.allni.mean ~ semester.all*time + type + esl.all + reading.all + writing.all +
                  bioclass.all + gender.all + age.all + race.all + level.all + plan.all +
                  cins.pre.all + isea.pre.all +  (time|id))  #Random Intercept model
summary(mixed.ni.2)


#sem.all=semester.all   #only run this at first before all the releveling
semester.all=relevel(factor(semester.all),ref="c")
#semester.all=sem.all   #run this after the releveling done


###############Network Cohesion (Marcus Paper)###################

animal.all.ind=cbind(skc.all.ind,snaive.all.ind)
animal0.ind=cbind(skc0.ind,snaive0.ind)
animal1.ind=cbind(skc1.ind,snaive1.ind)
animal2.ind=cbind(skc2.ind,snaive2.ind)
plant.all.ind=cbind(rkc.all.ind,rnaive.all.ind)
plant0.ind=cbind(rkc0.ind,rnaive0.ind)
plant1.ind=cbind(rkc1.ind,rnaive1.ind)
plant2.ind=cbind(rkc2.ind,rnaive2.ind)


###ONLY RUN THIS FOR REVERSE CODING NAIVE IDEAS
#animal0.ind[,7:9]=1-animal0.ind[,7:9]
#animal1.ind[,7:9]=1-animal1.ind[,7:9]
#animal2.ind[,7:9]=1-animal2.ind[,7:9]
#plant0.ind[,7:9]=1-plant0.ind[,7:9]
#plant1.ind[,7:9]=1-plant1.ind[,7:9]
#plant2.ind[,7:9]=1-plant2.ind[,7:9]


##Here, each student's network is their combined animal and plant network. (Max of 2 co-occurs)

#Pre
sem=c(rep(1,length(cins.spring15.pre)),rep(2,length(cins.spring16.pre)),
      rep(3,length(cins.spring17.pre)),rep(4,length(cans.fall17.pre)))
n=length(animal0.ind[,1])
H.all.pre=matrix(0,nrow=n,ncol=9)
for(k in 1:n){
  mat.ind=matrix(0,nrow=9,ncol=9)
  for(i in 1:9){
    for(j in 1:9){  
      if (sum(animal0.ind[k,i],animal0.ind[k,j],na.rm=T)==2){
        mat.ind[i,j]=mat.ind[i,j]+1
      } else if(is.na(sum(animal0.ind[k,i],animal0.ind[k,j]))){
        mat.ind[i,j]=NA
      }
      if (sum(plant0.ind[k,i],plant0.ind[k,j],na.rm=T)==2){
        mat.ind[i,j]=mat.ind[i,j]+1
      } else if(is.na(sum(plant0.ind[k,i],plant0.ind[k,j]))){
        mat.ind[i,j]=NA
      }
    }
  }
  H.all.pre[k,]=harmonic(mat.ind)/16
}
colnames(H.all.pre)=c("Var","Her","Comp","Ltd","Diff","Non","Ada","Need","Use")


#Post
sem=c(rep(1,length(cins.spring15.pre)),rep(2,length(cins.spring16.pre)),
      rep(3,length(cins.spring17.pre)),rep(4,length(cans.fall17.pre)))
n=length(animal1.ind[,1])
H.all.post=matrix(0,nrow=n,ncol=9)
for(k in 1:n){
  mat.ind=matrix(0,nrow=9,ncol=9)
  for(i in 1:9){
    for(j in 1:9){  
      if (sum(animal1.ind[k,i],animal1.ind[k,j],na.rm=T)==2){
        mat.ind[i,j]=mat.ind[i,j]+1
      } else if(is.na(sum(animal1.ind[k,i],animal1.ind[k,j]))){
        mat.ind[i,j]=NA
      }
      if (sum(plant1.ind[k,i],plant1.ind[k,j],na.rm=T)==2){
        mat.ind[i,j]=mat.ind[i,j]+1
      } else if(is.na(sum(plant1.ind[k,i],plant1.ind[k,j]))){
        mat.ind[i,j]=NA
      }
    }
  }
  H.all.post[k,]=harmonic(mat.ind)/16
}
colnames(H.all.post)=c("Var","Her","Comp","Ltd","Diff","Non","Ada","Need","Use")


#Delayed-Post
sem=c(rep(1,length(cins.spring15.pre)),rep(2,length(cins.spring16.pre)),
      rep(3,length(cins.spring17.pre)),rep(4,length(cans.fall17.pre)))
n=length(animal2.ind[,1])
H.all.dpost=matrix(0,nrow=n,ncol=9)
for(k in 1:n){
  mat.ind=matrix(0,nrow=9,ncol=9)
  for(i in 1:9){
    for(j in 1:9){  
      if (sum(animal2.ind[k,i],animal2.ind[k,j],na.rm=T)==2){
        mat.ind[i,j]=mat.ind[i,j]+1
      } else if(is.na(sum(animal2.ind[k,i],animal2.ind[k,j]))){
        mat.ind[i,j]=NA
      }
      if (sum(plant2.ind[k,i],plant2.ind[k,j],na.rm=T)==2){
        mat.ind[i,j]=mat.ind[i,j]+1
      } else if(is.na(sum(plant2.ind[k,i],plant2.ind[k,j]))){
        mat.ind[i,j]=NA
      }
    }
  }
  H.all.dpost[k,]=harmonic(mat.ind)/16
}
colnames(H.all.dpost)=c("Var","Her","Comp","Ltd","Diff","Non","Ada","Need","Use")


avg.har.pre=matrix(0,nrow=4,ncol=9); avg.har.post=avg.har.pre; avg.har.dpost=avg.har.pre
for(i in 1:4){
  avg.har.pre[i,]=colMeans(H.all.pre[sem==i,],na.rm=T)
  avg.har.post[i,]=colMeans(H.all.post[sem==i,],na.rm=T)
  avg.har.dpost[i,]=colMeans(H.all.dpost[sem==i,],na.rm=T)
}

round(avg.har.pre,4)
round(avg.har.post,4)
round(avg.har.dpost,4)


#Mixed Model with covariates
H.kc.mean=c(rowMeans(H.all.pre[,1:6]),rowMeans(H.all.post[,1:6]),rowMeans(H.all.dpost[,1:6]))
H.ni.mean=c(rowMeans(H.all.pre[,7:9]),rowMeans(H.all.post[,7:9]),rowMeans(H.all.dpost[,7:9]))
semester.all=rep(semester,3)
esl.all=rep(esl,3)
reading.all=rep(reading,3)
writing.all=rep(writing,3)
bioclass.all=rep(bioclass,3)
gender.all=rep(gender,3)
age.all=rep(age,3)
race.all=rep(race,3)
level.all=rep(level,3)
plan.all=rep(plan,3)
cins.pre.all=rep(cins.pre,3)
isea.pre.all=rep(isea.pre,3)
time=c(rep(0,length(cins.pre)),rep(1,length(cins.pre)),rep(2,length(cins.pre)))
time=factor(time,labels = c("pre","post","delpost"))
id=factor(rep(1:length(cins.pre),3))

mixed.kc.3=lmer(H.kc.mean ~ semester.all*time + esl.all + reading.all + writing.all +
                  bioclass.all + gender.all + age.all + race.all + level.all + plan.all +
                  cins.pre.all + isea.pre.all +  (1|id))  #Random Intercept model
summary(mixed.kc.3)


mixed.ni.3=lmer(H.ni.mean ~ semester.all*time + esl.all + reading.all + writing.all +
                  bioclass.all + gender.all + age.all + race.all + level.all + plan.all +
                  cins.pre.all + isea.pre.all +  (1|id))  #Random Intercept model
summary(mixed.ni.3)















###Network Plots###
#Animal
n1.1=graph_from_adjacency_matrix(M.pre.a[[1]],weighted = T, mode='lower',diag=F)
plot(n1.1,vertex.label.color="black", layout=layout_in_circle(n1.1), edge.width=E(n1.1)$weight/10,
     vertex.size=offdiagsums(M.pre.a[[1]])/10)
title("Pre")
plot(n1.1)
