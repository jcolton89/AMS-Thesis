####Run top part of "4 Groups Individual KC NI Networks.R####

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
sem=c(rep(1,length(cins.spring15.pre)),rep(2,length(cins.spring16.pre)),
      rep(3,length(cins.spring17.pre)),rep(4,length(cins.fall17.pre)))


###ONLY RUN THIS FOR REVERSE CODING NAIVE IDEAS
#animal0.ind[,7:9]=1-animal0.ind[,7:9]
#animal1.ind[,7:9]=1-animal1.ind[,7:9]
#animal2.ind[,7:9]=1-animal2.ind[,7:9]
#plant0.ind[,7:9]=1-plant0.ind[,7:9]
#plant1.ind[,7:9]=1-plant1.ind[,7:9]
#plant2.ind[,7:9]=1-plant2.ind[,7:9]



######Compute Jaccard and Simple Matching Similarities between the Animal/Plant networks
###PART 1:  TAKEN AT THE CLASS LEVEL###

#Pre
mymat=matrix(0,nrow=9,ncol=9); 
rownames(mymat)=c("Var","Her","Comp","Ltd","Diff","Non","Ada","Need","Use"); 
colnames(mymat)=rownames(mymat)
M00.pre=list(mymat,mymat,mymat,mymat); M01.pre=M00.pre; M10.pre=M00.pre; M11.pre=M00.pre
for(m in 1:4){
  n=length(animal0.ind[sem==m,][,1])
  for(k in 1:n){
    for(i in 1:9){
      for(j in 1:9){  
        if(sum(animal0.ind[sem==m,][k,i],animal0.ind[sem==m,][k,j],na.rm=T)==2 &&
           sum(plant0.ind[sem==m,][k,i],plant0.ind[sem==m,][k,j],na.rm=T)==2) {
          M11.pre[[m]][i,j]=M11.pre[[m]][i,j]+1
        }
        else if(sum(animal0.ind[sem==m,][k,i],animal0.ind[sem==m,][k,j],na.rm=T)==2 &&
                sum(plant0.ind[sem==m,][k,i],plant0.ind[sem==m,][k,j],na.rm=T)!=2) {
          M10.pre[[m]][i,j]=M10.pre[[m]][i,j]+1
        }
        else if(sum(animal0.ind[sem==m,][k,i],animal0.ind[sem==m,][k,j],na.rm=T)!=2 &&
                sum(plant0.ind[sem==m,][k,i],plant0.ind[sem==m,][k,j],na.rm=T)==2) {
          M01.pre[[m]][i,j]=M01.pre[[m]][i,j]+1
        }
        else if(sum(animal0.ind[sem==m,][k,i],animal0.ind[sem==m,][k,j],na.rm=T)!=2 &&
                sum(plant0.ind[sem==m,][k,i],plant0.ind[sem==m,][k,j],na.rm=T)!=2) {
          M00.pre[[m]][i,j]=M00.pre[[m]][i,j]+1
        }
      }
    }
  }
}
Jacc.pre=list(mymat,mymat,mymat,mymat)
for(m in 1:4){
  Jacc.pre[[m]]=M11.pre[[m]]/(M01.pre[[m]]+M10.pre[[m]]+M11.pre[[m]])
}
lapply(Jacc.pre,round,4) #Round to 4 digits, each element of list


#Post
mymat=matrix(0,nrow=9,ncol=9); 
rownames(mymat)=c("Var","Her","Comp","Ltd","Diff","Non","Ada","Need","Use"); 
colnames(mymat)=rownames(mymat)
M00.post=list(mymat,mymat,mymat,mymat); M01.post=M00.post; M10.post=M00.post; M11.post=M00.post
for(m in 1:4){
  n=length(animal1.ind[sem==m,][,1])
  for(k in 1:n){
    for(i in 1:9){
      for(j in 1:9){  
        if(sum(animal1.ind[sem==m,][k,i],animal1.ind[sem==m,][k,j],na.rm=T)==2 &&
           sum(plant1.ind[sem==m,][k,i],plant1.ind[sem==m,][k,j],na.rm=T)==2) {
          M11.post[[m]][i,j]=M11.post[[m]][i,j]+1
        }
        else if(sum(animal1.ind[sem==m,][k,i],animal1.ind[sem==m,][k,j],na.rm=T)==2 &&
                sum(plant1.ind[sem==m,][k,i],plant1.ind[sem==m,][k,j],na.rm=T)!=2) {
          M10.post[[m]][i,j]=M10.post[[m]][i,j]+1
        }
        else if(sum(animal1.ind[sem==m,][k,i],animal1.ind[sem==m,][k,j],na.rm=T)!=2 &&
                sum(plant1.ind[sem==m,][k,i],plant1.ind[sem==m,][k,j],na.rm=T)==2) {
          M01.post[[m]][i,j]=M01.post[[m]][i,j]+1
        }
        else if(sum(animal1.ind[sem==m,][k,i],animal1.ind[sem==m,][k,j],na.rm=T)!=2 &&
                sum(plant1.ind[sem==m,][k,i],plant1.ind[sem==m,][k,j],na.rm=T)!=2) {
          M00.post[[m]][i,j]=M00.post[[m]][i,j]+1
        }
      }
    }
  }
}
Jacc.post=list(mymat,mymat,mymat,mymat)
for(m in 1:4){
  Jacc.post[[m]]=M11.post[[m]]/(M01.post[[m]]+M10.post[[m]]+M11.post[[m]])
}
lapply(Jacc.post,round,4)  #Round to 4, each element of list


#D-Post
mymat=matrix(0,nrow=9,ncol=9); 
rownames(mymat)=c("Var","Her","Comp","Ltd","Diff","Non","Ada","Need","Use"); 
colnames(mymat)=rownames(mymat)
M00.dpost=list(mymat,mymat,mymat,mymat); M01.dpost=M00.dpost; M10.dpost=M00.dpost; M11.dpost=M00.dpost
for(m in 1:4){
  n=length(animal2.ind[sem==m,][,1])
  for(k in 1:n){
    for(i in 1:9){
      for(j in 1:9){  
        if(sum(animal2.ind[sem==m,][k,i],animal2.ind[sem==m,][k,j],na.rm=T)==2 &&
           sum(plant2.ind[sem==m,][k,i],plant2.ind[sem==m,][k,j],na.rm=T)==2) {
          M11.dpost[[m]][i,j]=M11.dpost[[m]][i,j]+1
        }
        else if(sum(animal2.ind[sem==m,][k,i],animal2.ind[sem==m,][k,j],na.rm=T)==2 &&
                sum(plant2.ind[sem==m,][k,i],plant2.ind[sem==m,][k,j],na.rm=T)!=2) {
          M10.dpost[[m]][i,j]=M10.dpost[[m]][i,j]+1
        }
        else if(sum(animal2.ind[sem==m,][k,i],animal2.ind[sem==m,][k,j],na.rm=T)!=2 &&
                sum(plant2.ind[sem==m,][k,i],plant2.ind[sem==m,][k,j],na.rm=T)==2) {
          M01.dpost[[m]][i,j]=M01.dpost[[m]][i,j]+1
        }
        else if(sum(animal2.ind[sem==m,][k,i],animal2.ind[sem==m,][k,j],na.rm=T)!=2 &&
                sum(plant2.ind[sem==m,][k,i],plant2.ind[sem==m,][k,j],na.rm=T)!=2) {
          M00.dpost[[m]][i,j]=M00.dpost[[m]][i,j]+1
        }
      }
    }
  }
}
Jacc.dpost=list(mymat,mymat,mymat,mymat)
for(m in 1:4){
  Jacc.dpost[[m]]=round(M11.dpost[[m]]/(M01.dpost[[m]]+M10.dpost[[m]]+M11.dpost[[m]]),4)
}
lapply(Jacc.dpost,round,4)  #Round to 4, each element of list


##List of all of them##
Jacc=list(Jacc.pre,Jacc.post,Jacc.dpost)
M00=list(M00.pre,M00.post,M00.dpost)
M01=list(M01.pre,M01.post,M01.dpost)
M10=list(M10.pre,M10.post,M10.dpost)
M11=list(M11.pre,M11.post,M11.dpost)


####Output the means of Node similarities####
nodeJacc=matrix(0,nrow=4,ncol=3)
for(w in 1:3){
  for(m in 1:4){
    nodeJacc[m,w]=mean(diag(Jacc[[w]][[m]]))
  }
}


####Output the means of Edge similarities####
edgeJacc=matrix(0,nrow=4,ncol=3)
for(w in 1:3){
  for(m in 1:4){
    edgeJacc[m,w]=mean(Jacc[[w]][[m]][lower.tri(mymat)],na.rm=T)
  }
}


####Output the means of combined Node/Edge similarities####
bothJacc=matrix(0,nrow=4,ncol=3)
for(w in 1:3){
  for(m in 1:4){
    bothJacc[m,w]=mean(Jacc[[w]][[m]][!lower.tri(mymat)],na.rm=T)
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
Jacc.node=M11.node/(M01.node+M10.node+M11.node) ##Jaccard Similarity

M00.edge=matrix(0,nrow=4,ncol=3); M01.edge=M00.edge; M10.edge=M00.edge; M11.edge=M00.edge
for(w in 1:3){
  for(m in 1:4){
    M00.edge[m,w]=sum(M00[[w]][[m]][lower.tri(mymat)])
    M01.edge[m,w]=sum(M01[[w]][[m]][lower.tri(mymat)])
    M10.edge[m,w]=sum(M10[[w]][[m]][lower.tri(mymat)])
    M11.edge[m,w]=sum(M11[[w]][[m]][lower.tri(mymat)])
  }
}
Jacc.edge=M11.edge/(M01.edge+M10.edge+M11.edge) ##Jaccard Similarity

M00.both=matrix(0,nrow=4,ncol=3); M01.both=M00.both; M10.both=M00.both; M11.both=M00.both
for(w in 1:3){
  for(m in 1:4){
    M00.both[m,w]=sum(M00[[w]][[m]][!lower.tri(mymat)])
    M01.both[m,w]=sum(M01[[w]][[m]][!lower.tri(mymat)])
    M10.both[m,w]=sum(M10[[w]][[m]][!lower.tri(mymat)])
    M11.both[m,w]=sum(M11[[w]][[m]][!lower.tri(mymat)])
  }
}
Jacc.both=M11.both/(M01.both+M10.both+M11.both) ##Jaccard Similarity





###PART 2:  TAKEN AT THE INDIVIDUAL LEVEL###

#Pre
m00.n=rep(0,length(animal0.ind[,1])); m01.n=m00.n; m10.n=m00.n; m11.n=m00.n;
m00.e=m00.n; m01.e=m00.n; m10.e=m00.n; m11.e=m00.n; m00.b=m00.n; m01.b=m00.n; m10.b=m00.n; m11.b=m00.n;
for(i in 1:length(animal0.ind[,1])){
  if(is.na(animal0.ind[i,1]) || is.na(plant0.ind[i,1])){
    m11.n[i]=NA;m10.n[i]=NA;m01.n[i]=NA;m00.n[i]=NA;
  }
  else{
    for(j in 1:9){
      if(animal0.ind[i,j]==1 && plant0.ind[i,j]==1){
        m11.n[i]=m11.n[i]+1
      }
      else if(animal0.ind[i,j]==1 && plant0.ind[i,j]==0){
        m10.n[i]=m10.n[i]+1
      }
      else if(animal0.ind[i,j]==0 && plant0.ind[i,j]==1){
        m01.n[i]=m01.n[i]+1
      }
      else if(animal0.ind[i,j]==0 && plant0.ind[i,j]==0){
        m00.n[i]=m00.n[i]+1
      }
    }
  }
}
for(i in 1:length(animal0.ind[,1])){
  if(is.na(animal0.ind[i,1]) || is.na(plant0.ind[i,1])){
    m11.e[i]=NA;m10.e[i]=NA;m01.e[i]=NA;m00.e[i]=NA;
  }
  else{
    for(j in 1:9){
      for(k in 1:9){
        if(j==k){
          break;
        }
        else if(sum(animal0.ind[i,j],animal0.ind[i,k],na.rm=T)==2 &&
                sum(plant0.ind[i,j],plant0.ind[i,k],na.rm=T)==2) {
          m11.e[i]=m11.e[i]+1
        }
        else if(sum(animal0.ind[i,j],animal0.ind[i,k],na.rm=T)==2 &&
                sum(plant0.ind[i,j],plant0.ind[i,k],na.rm=T)!=2) {
          m10.e[i]=m10.e[i]+1
        }
        else if(sum(animal0.ind[i,j],animal0.ind[i,k],na.rm=T)!=2 &&
                sum(plant0.ind[i,j],plant0.ind[i,k],na.rm=T)==2) {
          m01.e[i]=m01.e[i]+1
        }
        else if(sum(animal0.ind[i,j],animal0.ind[i,k],na.rm=T)!=2 &&
                sum(plant0.ind[i,j],plant0.ind[i,k],na.rm=T)!=2) {
          m00.e[i]=m00.e[i]+1
        }
      }
    }
  }
}
Jacc.ind.n.pre=m11.n/(m10.n+m01.n+m11.n)
Jacc.ind.e.pre=m11.e/(m10.e+m01.e+m11.e)
Jacc.ind.b.pre=(m11.n+m11.e)/(m10.n+m10.e+m01.n+m01.e+m11.n+m11.e)
Simp.ind.pre=(m11.n+m11.e+m00.n+m00.e)/(m10.n+m10.e+m01.n+m01.e+m11.n+m11.e+m00.n+m00.e)

#Post
m00.n=rep(0,length(animal1.ind[,1])); m01.n=m00.n; m10.n=m00.n; m11.n=m00.n;
m00.e=m00.n; m01.e=m00.n; m10.e=m00.n; m11.e=m00.n; m00.b=m00.n; m01.b=m00.n; m10.b=m00.n; m11.b=m00.n;
for(i in 1:length(animal1.ind[,1])){
  if(is.na(animal1.ind[i,1]) || is.na(plant1.ind[i,1])){
    m11.n[i]=NA;m10.n[i]=NA;m01.n[i]=NA;m00.n[i]=NA;
  }
  else{
    for(j in 1:9){
      if(animal1.ind[i,j]==1 && plant1.ind[i,j]==1){
        m11.n[i]=m11.n[i]+1
      }
      else if(animal1.ind[i,j]==1 && plant1.ind[i,j]==0){
        m10.n[i]=m10.n[i]+1
      }
      else if(animal1.ind[i,j]==0 && plant1.ind[i,j]==1){
        m01.n[i]=m01.n[i]+1
      }
      else if(animal1.ind[i,j]==0 && plant1.ind[i,j]==0){
        m00.n[i]=m00.n[i]+1
      }
    }
  }
}
for(i in 1:length(animal1.ind[,1])){
  if(is.na(animal1.ind[i,1]) || is.na(plant1.ind[i,1])){
    m11.e[i]=NA;m10.e[i]=NA;m01.e[i]=NA;m00.e[i]=NA;
  }
  else{
    for(j in 1:9){
      for(k in 1:9){
        if(j==k){
          break;
        }
        else if(sum(animal1.ind[i,j],animal1.ind[i,k],na.rm=T)==2 &&
                sum(plant1.ind[i,j],plant1.ind[i,k],na.rm=T)==2) {
          m11.e[i]=m11.e[i]+1
        }
        else if(sum(animal1.ind[i,j],animal1.ind[i,k],na.rm=T)==2 &&
                sum(plant1.ind[i,j],plant1.ind[i,k],na.rm=T)!=2) {
          m10.e[i]=m10.e[i]+1
        }
        else if(sum(animal1.ind[i,j],animal1.ind[i,k],na.rm=T)!=2 &&
                sum(plant1.ind[i,j],plant1.ind[i,k],na.rm=T)==2) {
          m01.e[i]=m01.e[i]+1
        }
        else if(sum(animal1.ind[i,j],animal1.ind[i,k],na.rm=T)!=2 &&
                sum(plant1.ind[i,j],plant1.ind[i,k],na.rm=T)!=2) {
          m00.e[i]=m00.e[i]+1
        }
      }
    }
  }
}
Jacc.ind.n.post=m11.n/(m10.n+m01.n+m11.n)
Jacc.ind.e.post=m11.e/(m10.e+m01.e+m11.e)
Jacc.ind.b.post=(m11.n+m11.e)/(m10.n+m10.e+m01.n+m01.e+m11.n+m11.e)
Simp.ind.post=(m11.n+m11.e+m00.n+m00.e)/(m10.n+m10.e+m01.n+m01.e+m11.n+m11.e+m00.n+m00.e)


#Delayed-Post
m00.n=rep(0,length(animal2.ind[,1])); m01.n=m00.n; m10.n=m00.n; m11.n=m00.n;
m00.e=m00.n; m01.e=m00.n; m10.e=m00.n; m11.e=m00.n; m00.b=m00.n; m01.b=m00.n; m10.b=m00.n; m11.b=m00.n;
for(i in 1:length(animal2.ind[,1])){
  if(is.na(animal2.ind[i,1]) || is.na(plant2.ind[i,1])){
    m11.n[i]=NA;m10.n[i]=NA;m01.n[i]=NA;m00.n[i]=NA;
  }
  else{
    for(j in 1:9){
      if(animal2.ind[i,j]==1 && plant2.ind[i,j]==1){
        m11.n[i]=m11.n[i]+1
      }
      else if(animal2.ind[i,j]==1 && plant2.ind[i,j]==0){
        m10.n[i]=m10.n[i]+1
      }
      else if(animal2.ind[i,j]==0 && plant2.ind[i,j]==1){
        m01.n[i]=m01.n[i]+1
      }
      else if(animal2.ind[i,j]==0 && plant2.ind[i,j]==0){
        m00.n[i]=m00.n[i]+1
      }
    }
  }
}
for(i in 1:length(animal2.ind[,1])){
  if(is.na(animal2.ind[i,1]) || is.na(plant2.ind[i,1])){
    m11.e[i]=NA;m10.e[i]=NA;m01.e[i]=NA;m00.e[i]=NA;
  }
  else{
    for(j in 1:9){
      for(k in 1:9){
        if(j==k){
          break;
        }
        else if(sum(animal2.ind[i,j],animal2.ind[i,k],na.rm=T)==2 &&
                sum(plant2.ind[i,j],plant2.ind[i,k],na.rm=T)==2) {
          m11.e[i]=m11.e[i]+1
        }
        else if(sum(animal2.ind[i,j],animal2.ind[i,k],na.rm=T)==2 &&
                sum(plant2.ind[i,j],plant2.ind[i,k],na.rm=T)!=2) {
          m10.e[i]=m10.e[i]+1
        }
        else if(sum(animal2.ind[i,j],animal2.ind[i,k],na.rm=T)!=2 &&
                sum(plant2.ind[i,j],plant2.ind[i,k],na.rm=T)==2) {
          m01.e[i]=m01.e[i]+1
        }
        else if(sum(animal2.ind[i,j],animal2.ind[i,k],na.rm=T)!=2 &&
                sum(plant2.ind[i,j],plant2.ind[i,k],na.rm=T)!=2) {
          m00.e[i]=m00.e[i]+1
        }
      }
    }
  }
}
Jacc.ind.n.dpost=m11.n/(m10.n+m01.n+m11.n)
Jacc.ind.e.dpost=m11.e/(m10.e+m01.e+m11.e)
Jacc.ind.b.dpost=(m11.n+m11.e)/(m10.n+m10.e+m01.n+m01.e+m11.n+m11.e)
Simp.ind.dpost=(m11.n+m11.e+m00.n+m00.e)/(m10.n+m10.e+m01.n+m01.e+m11.n+m11.e+m00.n+m00.e)


tapply(Jacc.ind.n.pre,sem,mean,na.rm=T)
tapply(Jacc.ind.n.pre,sem,sd,na.rm=T)




#####Mixed Models Regression#####
Jacc.all.n=c(Jacc.ind.n.pre,Jacc.ind.n.post,Jacc.ind.n.dpost)
Jacc.all.e=c(Jacc.ind.e.pre,Jacc.ind.e.post,Jacc.ind.e.dpost)
Jacc.all.b=c(Jacc.ind.b.pre,Jacc.ind.b.post,Jacc.ind.b.dpost)
Simp.all=c(Simp.ind.pre,Simp.ind.post,Simp.ind.dpost)
semester.all=rep(semester,3)
esl.all=rep(esl,3)
reading.all=rep(reading,3)
writing.all=rep(writing,3)
bioclass.all=rep(bioclass,3)
gender.all=rep(gender,3)
age.all=rep(age,3)
race.all=rep(race,3)
level.all=as.numeric(rep(level,3))
plan.all=as.numeric(rep(plan,3))
cins.pre.all=rep(cins.pre,3)
isea.pre.all=rep(isea.pre,3)
time=c(rep(0,length(cins.pre)),rep(1,length(cins.pre)),rep(2,length(cins.pre)))
time=factor(time,labels = c("pre","post","delpost"))
id=factor(rep(1:length(cins.pre),3))

mixed.jacc.n=lmer(Jacc.all.n ~ semester.all*time + esl.all + reading.all + writing.all +
                  bioclass.all + gender.all + age.all + race.all + level.all + plan.all +
                  cins.pre.all + isea.pre.all +  (1|id))  #Random Intercept model
summary(mixed.jacc.n)

mixed.jacc.e=lmer(Jacc.all.e ~ semester.all*time + esl.all + reading.all + writing.all +
                    bioclass.all + gender.all + age.all + race.all + level.all + plan.all +
                    cins.pre.all + isea.pre.all +  (1|id))  #Random Intercept model
summary(mixed.jacc.e)

mixed.jacc.b=lmer(Jacc.all.b ~ semester.all*time + esl.all + reading.all + writing.all +
                    bioclass.all + gender.all + age.all + race.all + level.all + plan.all +
                    cins.pre.all + isea.pre.all +  (1|id))  #Random Intercept model
summary(mixed.jacc.b)

mixed.simp=lmer(Simp.all ~ semester.all*time + esl.all + reading.all + writing.all +
                  bioclass.all + gender.all + age.all + race.all + level.all + plan.all +
                  cins.pre.all + isea.pre.all +  (1|id))  #Random Intercept model
summary(mixed.simp)


sem.all=semester.all
semester.all=relevel(factor(semester.all),ref="b")




###Graphs for jaccard index over time###
library(ggplot2)

barplot(tapply(Jacc.ind.b.pre,sem,mean,na.rm=T),tapply(Jacc.ind.b.post,sem,mean,na.rm=T),
        tapply(Jacc.ind.b.dpost,sem,mean,na.rm=T))

n0=c(length(na.omit(Jacc.ind.b.pre[semester=="a"])),length(na.omit(Jacc.ind.b.pre[semester=="b"])),
      length(na.omit(Jacc.ind.b.pre[semester=="c"])),length(na.omit(Jacc.ind.b.pre[semester=="d"])))
n1=c(length(na.omit(Jacc.ind.b.post[semester=="a"])),length(na.omit(Jacc.ind.b.post[semester=="b"])),
     length(na.omit(Jacc.ind.b.post[semester=="c"])),length(na.omit(Jacc.ind.b.post[semester=="d"])))
n2=c(length(na.omit(Jacc.ind.b.dpost[semester=="a"])),length(na.omit(Jacc.ind.b.dpost[semester=="b"])),
     length(na.omit(Jacc.ind.b.dpost[semester=="c"])),length(na.omit(Jacc.ind.b.dpost[semester=="d"])))
n=c(n0,n1,n2)

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(tapply(Jacc.ind.b.pre, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(Jacc.ind.b.post, sem, mean, na.rm=TRUE)),
                       as.numeric(tapply(Jacc.ind.b.dpost, sem, mean, na.rm=TRUE))),
                 se=c(as.numeric(tapply(Jacc.ind.b.pre, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(Jacc.ind.b.post, sem, sd, na.rm=TRUE)),
                      as.numeric(tapply(Jacc.ind.b.dpost, sem, sd, na.rm=TRUE))),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  geom_errorbar(aes(ymin=val-1.96*se/sqrt(n),ymax=val+1.96*se/sqrt(n),colour=Treatment),
                size=1,width=.1) + 
  xlab("") + ylab("Jaccard Index")  + ggtitle("") + ylim(0,1) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))
