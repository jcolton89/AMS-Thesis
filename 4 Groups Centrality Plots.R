##Plots of Centrality for each measure##
#Run code from "4 Groups Individual KC NI Networks first#
library(ggplot2)
library(gridExtra)


#######Part 1, just KC Centralities#######

#1) Animal KC
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,1]),as.numeric(H.post.a[,1]),as.numeric(H.dpost.a[,1])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.1=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("Animal - Variation") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,2]),as.numeric(H.post.a[,2]),as.numeric(H.dpost.a[,2])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.2=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("Animal - Heritability") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,3]),as.numeric(H.post.a[,3]),as.numeric(H.dpost.a[,3])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.3=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("Animal - Competition") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,4]),as.numeric(H.post.a[,4]),as.numeric(H.dpost.a[,4])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.4=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("Animal - Limited Resources") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,5]),as.numeric(H.post.a[,5]),as.numeric(H.dpost.a[,5])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.5=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("Animal - Differential Survival") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,6]),as.numeric(H.post.a[,6]),as.numeric(H.dpost.a[,6])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.6=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("Animal - Non-Adaptive Idea") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

grid.arrange(p1.1,p1.2,p1.3,p1.4,p1.5,p1.6)


#2) Plant KC
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre[,1]),as.numeric(H.post[,1]),as.numeric(H.dpost[,1])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.1=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("Plant - Variation") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre[,2]),as.numeric(H.post[,2]),as.numeric(H.dpost[,2])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.2=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("Plant - Heritability") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre[,3]),as.numeric(H.post[,3]),as.numeric(H.dpost[,3])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.3=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("Plant - Competition") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre[,4]),as.numeric(H.post[,4]),as.numeric(H.dpost[,4])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.4=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("Plant - Limited Resources") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre[,5]),as.numeric(H.post[,5]),as.numeric(H.dpost[,5])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.5=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("Plant - Differential Survival") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre[,6]),as.numeric(H.post[,6]),as.numeric(H.dpost[,6])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.6=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("Plant - Non-Adaptive Idea") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

grid.arrange(p1.1,p1.2,p1.3,p1.4,p1.5,p1.6)


#Combine Animal/Plant
#3) KC
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,1]),as.numeric(H.post.a[,1]),as.numeric(H.dpost.a[,1]),
                       as.numeric(H.pre[,1]),as.numeric(H.post[,1]),as.numeric(H.dpost[,1])),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",12),rep("Plant",12)))
p5.1=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")   + #ylim(0,1) +
  ggtitle("Variation") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,2]),as.numeric(H.post.a[,2]),as.numeric(H.dpost.a[,2]),
                       as.numeric(H.pre[,2]),as.numeric(H.post[,2]),as.numeric(H.dpost[,2])),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",12),rep("Plant",12)))
p5.2=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")   + #ylim(0,1) +
  ggtitle("Heritability") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,3]),as.numeric(H.post.a[,3]),as.numeric(H.dpost.a[,3]),
                       as.numeric(H.pre[,3]),as.numeric(H.post[,3]),as.numeric(H.dpost[,3])),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",12),rep("Plant",12)))
p5.3=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")   + #ylim(0,1) +
  ggtitle("Competition") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,4]),as.numeric(H.post.a[,4]),as.numeric(H.dpost.a[,4]),
                       as.numeric(H.pre[,4]),as.numeric(H.post[,4]),as.numeric(H.dpost[,4])),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",12),rep("Plant",12)))
p5.4=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")   + #ylim(0,1) +
  ggtitle("Limited Resources") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,5]),as.numeric(H.post.a[,5]),as.numeric(H.dpost.a[,5]),
                       as.numeric(H.pre[,5]),as.numeric(H.post[,5]),as.numeric(H.dpost[,5])),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",12),rep("Plant",12)))
p5.5=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")   + #ylim(0,1) +
  ggtitle("Differential Survival") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,6]),as.numeric(H.post.a[,6]),as.numeric(H.dpost.a[,6]),
                       as.numeric(H.pre[,6]),as.numeric(H.post[,6]),as.numeric(H.dpost[,6])),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",12),rep("Plant",12)))
p5.6=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")   + #ylim(0,1) +
  ggtitle("Non-Adaptive Idea") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))


grid.arrange(p5.1,p5.2,p5.3,p5.4,p5.5,p5.6)



##Mean Centrality##
#Animal#
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(rowMeans(H.pre.a),rowMeans(H.post.a),rowMeans(H.dpost.a)),
                 Treatment=c("CG","EG1","EG2","EG3"))
p6.0=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + ylim(0,0.2) +
  ggtitle("Animal - Mean") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

#Plant#
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(rowMeans(H.pre),rowMeans(H.post),rowMeans(H.dpost)),
                 Treatment=c("CG","EG1","EG2","EG3"))
p6.1=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + ylim(0,0.2) +
  ggtitle("Plant - Mean") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

#Combined Animal/Plant#
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(rowMeans(H.pre.a),rowMeans(H.post.a),rowMeans(H.dpost.a),
                       rowMeans(H.pre),rowMeans(H.post),rowMeans(H.dpost)),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",12),rep("Plant",12)))
p6.2=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")   + #ylim(0,1) +
  ggtitle("Mean Network Centrality") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))







########Part 2, KC and NI Centralities########


#1) Animal
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,1]),as.numeric(H.post.a[,1]),as.numeric(H.dpost.a[,1])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.1=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("A - Variation") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,2]),as.numeric(H.post.a[,2]),as.numeric(H.dpost.a[,2])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.2=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("A - Heritability") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,3]),as.numeric(H.post.a[,3]),as.numeric(H.dpost.a[,3])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.3=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("A - Competition") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,4]),as.numeric(H.post.a[,4]),as.numeric(H.dpost.a[,4])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.4=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("A - Limited Resources") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,5]),as.numeric(H.post.a[,5]),as.numeric(H.dpost.a[,5])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.5=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("A - Differential Survival") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,6]),as.numeric(H.post.a[,6]),as.numeric(H.dpost.a[,6])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.6=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("A - Non-Adaptive Idea") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))


df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,7]),as.numeric(H.post.a[,7]),as.numeric(H.dpost.a[,7])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.7=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("A - Adapt/Acclimate") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))


df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,8]),as.numeric(H.post.a[,8]),as.numeric(H.dpost.a[,8])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.8=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("A - Need/Goal") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))


df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,9]),as.numeric(H.post.a[,9]),as.numeric(H.dpost.a[,9])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.9=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("A - Non-Adaptive Idea") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))


grid.arrange(p1.1,p1.2,p1.3,p1.4,p1.5,p1.6,p1.7,p1.9,p1.9)




#2) Plant KC
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre[,1]),as.numeric(H.post[,1]),as.numeric(H.dpost[,1])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.1=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("P - Variation") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre[,2]),as.numeric(H.post[,2]),as.numeric(H.dpost[,2])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.2=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("P - Heritability") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre[,3]),as.numeric(H.post[,3]),as.numeric(H.dpost[,3])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.3=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("P - Competition") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre[,4]),as.numeric(H.post[,4]),as.numeric(H.dpost[,4])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.4=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("P - Limited Resources") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre[,5]),as.numeric(H.post[,5]),as.numeric(H.dpost[,5])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.5=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("P - Differential Survival") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre[,6]),as.numeric(H.post[,6]),as.numeric(H.dpost[,6])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.6=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("P - Non-Adaptive Idea") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))


df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre[,7]),as.numeric(H.post[,7]),as.numeric(H.dpost[,7])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.7=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("P - Adapt/Acclimate") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))


df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre[,8]),as.numeric(H.post[,8]),as.numeric(H.dpost[,8])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.8=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("P - Need/Goal") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))


df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre[,9]),as.numeric(H.post[,9]),as.numeric(H.dpost[,9])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p1.9=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + #ylim(0,1) +
  ggtitle("P - Non-Adaptive Idea") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))


grid.arrange(p1.1,p1.2,p1.3,p1.4,p1.5,p1.6,p1.7,p1.8,p1.9)




#Combine Animal/Plant
#3) KC
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,1]),as.numeric(H.post.a[,1]),as.numeric(H.dpost.a[,1]),
                       as.numeric(H.pre[,1]),as.numeric(H.post[,1]),as.numeric(H.dpost[,1])),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",12),rep("Plant",12)))
p5.1=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")   + #ylim(0,1) +
  ggtitle("Variation") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,2]),as.numeric(H.post.a[,2]),as.numeric(H.dpost.a[,2]),
                       as.numeric(H.pre[,2]),as.numeric(H.post[,2]),as.numeric(H.dpost[,2])),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",12),rep("Plant",12)))
p5.2=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")   + #ylim(0,1) +
  ggtitle("Heritability") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,3]),as.numeric(H.post.a[,3]),as.numeric(H.dpost.a[,3]),
                       as.numeric(H.pre[,3]),as.numeric(H.post[,3]),as.numeric(H.dpost[,3])),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",12),rep("Plant",12)))
p5.3=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")   + #ylim(0,1) +
  ggtitle("Competition") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,4]),as.numeric(H.post.a[,4]),as.numeric(H.dpost.a[,4]),
                       as.numeric(H.pre[,4]),as.numeric(H.post[,4]),as.numeric(H.dpost[,4])),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",12),rep("Plant",12)))
p5.4=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")   + #ylim(0,1) +
  ggtitle("Limited Resources") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,5]),as.numeric(H.post.a[,5]),as.numeric(H.dpost.a[,5]),
                       as.numeric(H.pre[,5]),as.numeric(H.post[,5]),as.numeric(H.dpost[,5])),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",12),rep("Plant",12)))
p5.5=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")   + #ylim(0,1) +
  ggtitle("Differential Survival") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,6]),as.numeric(H.post.a[,6]),as.numeric(H.dpost.a[,6]),
                       as.numeric(H.pre[,6]),as.numeric(H.post[,6]),as.numeric(H.dpost[,6])),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",12),rep("Plant",12)))
p5.6=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")   + #ylim(0,1) +
  ggtitle("Non-Adaptive Idea") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,7]),as.numeric(H.post.a[,7]),as.numeric(H.dpost.a[,7]),
                       as.numeric(H.pre[,7]),as.numeric(H.post[,7]),as.numeric(H.dpost[,7])),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",12),rep("Plant",12)))
p5.7=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")   + #ylim(0,1) +
  ggtitle("Adapt/Acclimate") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,8]),as.numeric(H.post.a[,8]),as.numeric(H.dpost.a[,8]),
                       as.numeric(H.pre[,8]),as.numeric(H.post[,8]),as.numeric(H.dpost[,8])),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",12),rep("Plant",12)))
p5.8=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")   + #ylim(0,1) +
  ggtitle("Need/Goal") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(as.numeric(H.pre.a[,9]),as.numeric(H.post.a[,9]),as.numeric(H.dpost.a[,9]),
                       as.numeric(H.pre[,9]),as.numeric(H.post[,9]),as.numeric(H.dpost[,9])),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",12),rep("Plant",12)))
p5.9=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")   + #ylim(0,1) +
  ggtitle("Use/Disuse") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))


grid.arrange(p5.1,p5.2,p5.3,p5.4,p5.5,p5.6,p5.7,p5.8,p5.9)



##Mean Centrality##
#Animal KC#
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(rowMeans(H.pre.a[,1:6]),rowMeans(H.post.a[,1:6]),rowMeans(H.dpost.a[,1:6])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p6.0=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + ylim(0,0.15) +
  ggtitle("Animal KC - Mean") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

#Animal NI
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(rowMeans(H.pre.a[,7:9]),rowMeans(H.post.a[,7:9]),rowMeans(H.dpost.a[,7:9])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p6.1=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + ylim(0,0.05) +
  ggtitle("Animal NI - Mean") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

#Plant KC#
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(rowMeans(H.pre[,1:6]),rowMeans(H.post[,1:6]),rowMeans(H.dpost[,1:6])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p6.2=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + ylim(0,0.15) +
  ggtitle("Plant KC - Mean") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

#Plant NI#
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(rowMeans(H.pre[,7:9]),rowMeans(H.post[,7:9]),rowMeans(H.dpost[,7:9])),
                 Treatment=c("CG","EG1","EG2","EG3"))
p6.3=ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=Treatment),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")  + ylim(0,0.05) +
  ggtitle("Plant NI - Mean") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))


grid.arrange(p6.0,p6.2,p6.1,p6.3)


#Combined Animal/Plant#
df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(rowMeans(H.pre.a[,1:6]),rowMeans(H.post.a[,1:6]),rowMeans(H.dpost.a[,1:6]),
                       rowMeans(H.pre[,1:6]),rowMeans(H.post[,1:6]),rowMeans(H.dpost[,1:6])),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",12),rep("Plant",12)))
p7.0=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")   + #ylim(0,1) +
  ggtitle("Key Concepts") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))

df <- data.frame(x=c(rep(0,4),rep(1,4),rep(3,4)),
                 val=c(rowMeans(H.pre.a[,7:9]),rowMeans(H.post.a[,7:9]),rowMeans(H.dpost.a[,7:9]),
                       rowMeans(H.pre[,7:9]),rowMeans(H.post[,7:9]),rowMeans(H.dpost[,7:9])),
                 Treatment=c("CG","EG1","EG2","EG3"),
                 Type=c(rep("Animal",12),rep("Plant",12)))
p7.1=ggplot(data = df, aes(x=x, y=val)) + 
  geom_line(aes(colour=Treatment,linetype=Type),size=1.2) +
  xlab("") + ylab("Harmonic Centrality")   + #ylim(0,1) +
  ggtitle("Naive Ideas") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),
        legend.text=element_text(size=14),plot.title = element_text(size=19,hjust = 0.5),
        legend.title=element_text(size=14),panel.grid.minor.x=element_blank())+
  scale_x_continuous(breaks=c(0,1,3),
                     labels=c("Pre", "Post", "Delayed-Post"))


grid.arrange(p7.0,p7.1,ncol=2)
