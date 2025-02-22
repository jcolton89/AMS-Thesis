####Graphs for Thesis####

library(igraph)

#Adjacency Matrix #1  Connected Graph
m=matrix(c(1,1,1,0,1,0,
           1,1,1,0,0,0,
           1,1,1,1,0,0,
           0,0,1,1,0,0,
           1,0,0,0,1,1,
           0,0,0,0,1,1),nrow=6,byrow=T)
rownames(m)=c("A","B","C","D","E","F")
colnames(m)=rownames(m)

#Graph from above #1
p1=graph_from_adjacency_matrix(m,weighted=T,mode='lower',diag=F)
plot(p1,vertex.color='lightblue',vertex.label.color="black", layout=layout_in_circle(p1),
     vertex.size=30, edge.width=2)


#par(mar=c(5.1, 4.1, 4.1, 2.1))  #Default
#par(mar=c(1,1,1,1))
#par(mar=c(0.5,0.5,0.5,0.5))


#Adjacency Matrix #2  Disconnected Graph
m=matrix(c(1,1,1,0,0,0,
           1,1,1,0,0,0,
           1,1,1,1,0,0,
           0,0,1,1,0,0,
           0,0,0,0,1,1,
           0,0,0,0,1,1),nrow=6,byrow=T)
rownames(m)=c("A","B","C","D","E","F")
colnames(m)=rownames(m)

#Graph from above #2
p1=graph_from_adjacency_matrix(m,weighted=T,mode='lower',diag=F)
plot(p1,vertex.color='lightblue',vertex.label.color="black", layout=layout_in_circle(p1),
     vertex.size=30, edge.width=2)


#par(mar=c(5.1, 4.1, 4.1, 2.1))  #Default
#par(mar=c(1,1,1,1))
#par(mar=c(0.5,0.5,0.5,0.5))


#Co-occurrence matrix #3, like in adjacency matrix #1 above but with frequencies
m.0=matrix(c(1,10,3,0,0,0,
             10,1,8,0,0,0,
             3,8,1,2,0,0,
             0,0,2,1,0,0,
             0,0,0,0,1,6,
             0,0,0,0,6,1),nrow=6,byrow=T)
rownames(m.0)=c("A","B","C","D","E","F")
colnames(m.0)=rownames(m.0)

#Graph from above #3
p1.0=graph_from_adjacency_matrix(m.0,weighted=T,mode='lower',diag=F)
plot(p1.0,vertex.color='lightblue',vertex.label.color="black", layout=layout_in_circle(p1.0),
     vertex.size=30, edge.width=2)#,edge.label=E(p1.0)$weight, edge.label.color="black")
edge_attr_names(p1.0)




#Example Code#
n1.1=graph_from_adjacency_matrix(M.pre.a[[1]],weighted = T, mode='lower',diag=F)
plot(n1.1,vertex.label.color="black", layout=layout_in_circle(n1.1), edge.width=E(n1.1)$weight/10,
     vertex.size=offdiagsums(M.pre.a[[1]])/10)
title("Pre")
plot(n1.1)



m2=matrix(c(218,58,3,87,145,2,
            58,80,2,39,57,0,
            3,2,5,1,1,0,
            87,39,1,208,103,1,
            145,57,1,103,201,0,
            2,0,0,1,0,2),nrow=6,byrow=T)

p1=graph_from_adjacency_matrix(m,mode='lower',diag=F)
p2=graph_from_adjacency_matrix(m,weighted=T,mode='lower',diag=F)
p3=graph_from_adjacency_matrix(1/m2,mode='lower',diag=F)
p4=graph_from_adjacency_matrix(1/m2,weighted=T,mode='lower',diag=F)

betweenness(p1)


plot(p1)
plot(p4)





############## NEW 2023 ##############


