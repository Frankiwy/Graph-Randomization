library(igraph)
library(mc2d)
library(sdpt3r)
library(glue)
library(ggplot2)
library(gridExtra)
library(ggraph)
library(poweRlaw)


set.seed(1765820)

dir.create('networks_final1')
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
} # the function is used to pre-allocate the matrix of all the edges


graph_generator <- function(Num_nodes, gamma=.5){
  
  E_one=1:Num_nodes
  E_two=rep(0,Num_nodes) 
  # we found out that using 2 vectors instead of a matrix with 2 columns gives a minor 
  # boost in the computation speed.
  E_two[1] <-2 
  E_two[2] <-3
  E_two[3] <-4
  E_two[4] <-1
  
  
  for (new_vertex in 5:Num_nodes){
    if (rbern(n = 1, prob=gamma) ==1) {
      get_vertex = sample(E_one[1:(new_vertex-1)], 1) 
      # choosing existing node uniformly    
      E_two[new_vertex] <-get_vertex
    }
    else {
      get_vertex = sample(E_two[1:(new_vertex-1)], 1) 
      # choosing existing node with a weighted distribution (based on the in-degree)
      E_two[new_vertex] <-get_vertex
    }
  }
  results = list()
  results$one = E_one
  results$two = E_two
  return(results)
} 


cumulative_empirical = function(E){
  
  E_one = E$one # staring node vector 
  E_two = E$two # ending node vector
  Num_nodes = length(E$one)
  
  # we iterate through all the numbers in the E_two (which is the vector containing the end-nodes of
  # the edges) and add one to the corresponding element in vector in_nodes
  # (which is the vector containing the in-degree of the nodes).  
  # For example: 
  # if E_two = [2 3 4 1 2 2 5 2 3 5 5 5], we first add 1 to the second element of 
  # the in_nodes vector, then to the third, then to the fourth, then first, second,
  # second, fifth, etc...
  # At the end, the in_nodes vector will be: [1 4 2 1 4 0 0 0 0 0 0 0] meaning that
  # the in-degree of node 1 is 1, the in-degree of node 2 is 4, etc...

  in_nodes=rep(0,Num_nodes) # pre-allocate a vector with 0
  for (i in 1:Num_nodes){
    in_nodes[E_two[i]]=in_nodes[E_two[i]]+1
  }
  
  
  sorted_in_nodes=sort(in_nodes,decreasing=TRUE)
  indexes=unique(sorted_in_nodes) # it's the number of nodes where the cumulative changes
  
  A=rep.row(c(0,0,0),length(indexes)) 
  # pre-allocate the matrix. In the first column there are the values where there is
  # at least one node with that in-degree 
  
  A[,1]=indexes
  
  
  counter_cumulative = 0
  counter_empirical = 0
  j=1
  for (i in 1:length(indexes)){
    in_degree_value=A[i,1]
    counter_empirical = 0 # here we reset the counter every time we change in-degree node
    # because it is used to compute the empirical distribution.
    while(j<=Num_nodes && sorted_in_nodes[j]==in_degree_value){
      
      # we calculate the empirical distribution with a single
      # loop trough the vector: we use the fact that vector is already sorted,
      # so it is not necessary to look all the elements once we find the 
      # first one which is different from the current in-degree value.
      counter_empirical = counter_empirical +1
      j=j+1
    } 
    
    counter_cumulative = counter_cumulative + counter_empirical
    A[i,3]=counter_cumulative
    A[i,2]=counter_empirical
  }
  
  empirical = rev(A[,2])[-1]
  cumulative = rev(A[,3])[-1]
  in_degree_value = rev(A[,1])[-1]
  return(A)

}


for (i in 1:5){
  A = cumulative_empirical(graph_generator(1000))
  write.csv(A, glue("networks_final1/network_{i}.csv"), row.names = F)
  print(glue('graph {i} stored'))
}



# plots -------------------------------------------------------------------

D1 = as.data.frame(read.csv("distributions_matrices/cum_dev_matrix_1.csv"))
D2 = as.data.frame(read.csv("distributions_matrices/cum_dev_matrix_2.csv"))
D3 = as.data.frame(read.csv("distributions_matrices/cum_dev_matrix_3.csv"))
D4 = as.data.frame(read.csv("distributions_matrices/cum_dev_matrix_4.csv"))
D5 = as.data.frame(read.csv("distributions_matrices/cum_dev_matrix_5.csv"))

par(mfrow=c(1,2))
# Create a first line
plot(x = log(D1$V1), y=log(D1$V3), type = "l", col = "red", lty=1, lwd=2,
     xlab = "K in log scale", ylab = "cumulative in log scale",
     main="Cumulative in log-log scale")
# Add a second line
lines(x = log(D2$V1), y=log(D2$V3),  col = "blue", type = "l", lwd=2, lty = 2)#, log = 'xy')
# Add a third line
lines(x = log(D3$V1), y=log(D3$V3),  col = "green", type = "l", lwd=2, lty = 3)#, log = 'xy')
# Add a fourth line
lines(x = log(D4$V1), y=log(D4$V3),  col = "orange", type = "l", lwd=2, lty = 4)#, log = 'xy')
# Add a fifth line
lines(x = log(D5$V1), y=log(D5$V3),  col = "purple", type = "l", lwd=2, lty = 5)#, log = 'xy')
# Add a legend to the plot
legend("topright", legend=c("Graph 1", "Graph 2", "Graph 3", "Graph 4", "Graph 5"),
      col=c("red", "blue", "green", "orange", "purple"), lty = 1:5, cex=0.5, bty='o',
      bg='lightgrey', title = "Networks:")


# Create a first line
plot(x = log(D1$V1), y=log(D1$V2), type = "l", col = "red", lty=1, lwd=2,
     xlab = "K in log scale", ylab = "empirical in log scale",
     main="Empirical in log-log scale")#, log = 'xy')
# Add a second line
lines(x = log(D2$V1), y=log(D2$V2),  col = "blue", type = "l", lwd=2, lty = 2)#, log = 'xy')
# Add a third line
lines(x = log(D3$V1), y=log(D3$V2),  col = "green", type = "l", lwd=2, lty = 3)#, log = 'xy')
# Add a fourth line
lines(x = log(D4$V1), y=log(D4$V2),  col = "orange", type = "l", lwd=2, lty = 4)#, log = 'xy')
# Add a fifth line
lines(x = log(D5$V1), y=log(D5$V2),  col = "purple", type = "l", lwd=2, lty = 5)#, log = 'xy')
# Add a legend to the plot
legend("topright", legend=c("Graph 1", "Graph 2", "Graph 3", "Graph 4", "Graph 5"),
       col=c("red", "blue", "green", "orange", "purple"), lty = 1:5, cex=0.5, bty='o',
       bg='lightgrey', title = "Networks:")

####################### PLOT WITH REGRESSION ########################
#DE8CF0 #BED905
#525B56 #BE9063
#00743F #F2A104
#36688D #F49F05
#0294A5 #C1403D


p1 <- ggplot(D1, aes(x=log(V1), y=log(V3))) +
  geom_point(size=1, color="#DE8CF0")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="#BED905")+ 
  ggtitle("Cumulative & fitting line for Graph 1") +
  xlab("K in log scale") + ylab("Cumulative in log scale") +
  theme(
    plot.title = element_text(color="#DE8CF0", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=10, face="bold"),
    axis.title.y = element_text(color="black", size=10, face="bold")
  )

p2 <- ggplot(D2, aes(x=log(V1), y=log(V3))) +
  geom_point(size=1, color="#525B56")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="#BE9063")+ 
  ggtitle("Cumulative & fitting line for Graph 2") +
  xlab("K in log scale") + ylab("Cumulative in log scale") +
  theme(
    plot.title = element_text(color="#525B56", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=10, face="bold"),
    axis.title.y = element_text(color="black", size=10, face="bold")
  )

p3 <- ggplot(D3, aes(x=log(V1), y=log(V3))) +
  geom_point(size=1, color="#00743F")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="#F2A104")+ 
  ggtitle("Cumulative & fitting line for Graph 3") +
  xlab("K in log scale") + ylab("Cumulative in log scale") +
  theme(
    plot.title = element_text(color="#00743F", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=10, face="bold"),
    axis.title.y = element_text(color="black", size=10, face="bold")
  )

p4 <- ggplot(D4, aes(x=log(V1), y=log(V3))) +
  geom_point(size=1, color="#36688D")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="#F49F05")+ 
  ggtitle("Cumulative & fitting line for Graph 4") +
  xlab("K in log scale") + ylab("Cumulative in log scale") +
  theme(
    plot.title = element_text(color="#36688D", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=10, face="bold"),
    axis.title.y = element_text(color="black", size=10, face="bold")
  )

p5 <- ggplot(D5, aes(x=log(V1), y=log(V3))) +
  geom_point(size=1, color="#0294A5")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="#C1403D")+ 
  ggtitle("Cumulative & fitting line for Graph 5") +
  xlab("K in log scale") + ylab("Cumulative in log scale") +
  theme(
    plot.title = element_text(color="#0294A5", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=10, face="bold"),
    axis.title.y = element_text(color="black", size=10, face="bold")
  )

graph_names <- c("graph 1","graph 2","graph 3","graph 4","graph 5")
lm_resuls <- rep(0,5)
df_list = list(D1, D2, D3, D4, D5)
counter = 1

for (df in df_list){
  c_lm <- lm(V3~V1, data = df)
  strg <- paste("y = ",toString(round(c_lm$coefficients[[1]],2)),
                toString(round(c_lm$coefficients[[2]],2)), 'x', sep="")
  lm_resuls[counter] = strg
  counter = counter +1
}

# Table
lm_table <- tableGrob(data.frame(cbind(graph_names, lm_resuls)), rows=NULL)
grid.arrange(p1, p2, p3, p4, p5, lm_table, nrow=2, ncol=3)


############## create table ##########
library(gt)
lm_df <- data.frame(cbind(graph_names, lm_resuls))
tb <- lm_df %>% gt()
tb
grid.arrange(p1, p2, p3, p4, p5, tb, nrow=2, ncol=3)


###################################################

A1 = as.data.frame(read.csv("graphs_matrices/graph_1.csv"))
A2 = as.data.frame(read.csv("graphs_matrices/graph_2.csv"))
A3 = as.data.frame(read.csv("graphs_matrices/graph_3.csv"))
A4 = as.data.frame(read.csv("graphs_matrices/graph_4.csv"))
A5 = as.data.frame(read.csv("graphs_matrices/graph_5.csv"))

G1 = graph_from_data_frame(A1, directed = TRUE, vertices = NULL)

# plot with ggraph
ggraph(G1)
layout_with_lgl(G1)

with_lgl(...)
############################# KS TEST ############################################

data <- c(100, 100, 10, 10, 10)
data_pl <- displ$new(data)
est <- estimate_xmin(D1$V3)
D1$V3
fit_power_law(D1$V3)$KS.stat

ks_stat <- rep(0, 5)
p_value <- rep(0, 5)
m = 1
for (df in df_list){
  
  results <- fit_power_law(df$V2)
  ks_stat[m] <- round(results$KS.stat,2)
  p_value[m] <- round(results$KS.p,2)
  m = m+1
}


##################

for (n in 0:10){
  
  graph_gamma = graph_generator(50000,.64)
  cm_em_gamma = cumulative_empirical(graph_gamma)
  
  print(glue("gamma={n/10}"))
  print(fit_power_law(cm_em_gamma[,2], xmin=1)$KS.p)
  plot(x=cm_em_gamma[,1], y=cm_em_gamma[,3], log="xy")
  
}

graph_gamma = graph_generator(50000, .63)
cm_em_gamma = cumulative_empirical(graph_gamma) 


fit_power_law(cm_em_gamma[,2], xmin=1)

plot(x=cm_em_gamma[,1], y=cm_em_gamma[,3], log="xy")


###################################################


g1 = graph_from_edgelist(as.matrix(A1), directed = TRUE)

Num_nodes = length(A1$E_two)
in_nodes=rep(0,Num_nodes) # pre-allocate a vector with 0
for (i in 1:Num_nodes){
  in_nodes[A1$E_two[i]]=in_nodes[A1$E_two[i]]+1
}

g_1000 <- graph_generator(100000)

g_graph_1000 <- graph_from_edgelist(cbind(g_1000$one,g_1000$two), directed = FALSE)
g_graph_1000

plot(g_graph_1000, vertex.size = 0, edge.size = 0.001, vertex.label=NA, vertex.frame.color=NA, vertex.color =NA, edge.color='black')#node.label=NA)#, arrow.size = 0, arrow.width = 0, arrow.mode=)


g_1000_random = graph_generator(100000,gamma=1)
g_1000_random = graph_from_edgelist(cbind(g_1000_random$one,g_1000_random$two), directed = FALSE)

plot(g_1000_random, vertex.size = 0, edge.size = 0.001, vertex.label=NA, vertex.frame.color=NA, vertex.color =NA, edge.color='black')#node.label=NA)#, arrow.size = 0, arrow.width = 0, arrow.mode=)


list_nodes <- c(3,4,5,10)
plot(ER_graph, vertex.color=c("purple",'green')[1+(V(VS_graph)%in%list_nodes)],
     vertex.size=30, vertex.frame.color="black", vertex.label.color="black",
     vertex.label.cex=1, vertex.label.dist=0,
     edge.curved=0, edge.color='black',
     xlim=c(-1,1), ylim=c(-1,1))
title("Erdős–Rényi", adj = 0, line = -5)












###################


graph_gamma = graph_generator(200000, 1)
cm_em_gamma = cumulative_empirical(graph_gamma)

write.csv(graph_gamma, "graphs_matrices/graph_gamma.csv", row.names = F)
write.csv(cm_em_gamma, glue("distributions_matrices/cm_em_gamma.csv"), row.names = F)

######################




# (1) graph with gamma = 1
g_100000_random <- graph_generator(100,gamma=1)
g_graph_100000_random <- graph_from_edgelist(cbind(g_100000_random$one,g_100000_random$two), directed = FALSE)# we have inserted directed=FALSE just because we don't want to plot the edges arrows in the visualization

# (2) graph graph with gamma = .5
g_100000 <- graph_generator(100)
g_graph_100000 <- graph_from_edgelist(cbind(g_100000$one,g_100000$two), directed = FALSE) # we have inserted directed=FALSE just because we don't want to plot the edges arrows in the visualization


plt_1 <- plot(g_graph_100000_random, vertex.size = 0, edge.size = 0.001,
             vertex.label=NA, vertex.frame.color=NA, vertex.color =NA,
             edge.color='black', main='Graph A')


# (2) graph graph with gamma = .5
plt_2 <- plot(g_graph_100000, vertex.size = 0, edge.size = 0.001, vertex.label=NA,
             vertex.frame.color=NA, vertex.color =NA, edge.color='black',
             main='Graph B')



Num_nodes=1000
graph_10 = graph_generator(Num_nodes,gamma=1)
g_graph_10 = graph_from_edgelist(cbind(graph_10$one,graph_10$two), directed = FALSE)
in_nodes=rep(0,Num_nodes) # pre-allocate a vector with 0
for (i in 1:Num_nodes){
  in_nodes[graph_10$two[i]]=in_nodes[graph_10$two[i]]+1
}


vectors <- order(in_nodes, decreasing = TRUE)[1:5]
V(g_graph_10)$color <- ifelse( V(g_graph_10) %in% vectors, "red", NA)
V(g_graph_10)$size <- ifelse( V(g_graph_10) %in% vectors, 4, 0)
plot(g_graph_10, edge.size = 0.001, vertex.label=NA,
     vertex.frame.color=NA, edge.color='black',
     main='Graph B')

###################


read.csv("performances.csv")





