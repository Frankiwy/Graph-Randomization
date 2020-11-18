library(igraph)
#library(Rlab)
library(mc2d)
library(sdpt3r)
library(glue)

set.seed(1765820)


rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
} # the function is used to pre-allocate the matrix of all the edges


graph_generator <- function(Num_nodes){
  
  E_one=1:Num_nodes
  E_two=rep(0,Num_nodes) 
  # we found out that using 2 vectors instead of a matrix with 2 columns gives a minor 
  # boost in the computation speed.
  E_two[1] <-2 
  E_two[2] <-3
  E_two[3] <-4
  E_two[4] <-1
  
  
  for (new_vertex in 5:Num_nodes){
    if (rbern(n = 1, prob=0.5) ==1) {
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
  # pre-allocate the matrix. In the first column there are the values representing
  # the node where there is at least awhere there is 
  # 
  
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
  A = cumulative_empirical(graph_generator(1000000))
  write.csv(A, glue("networks_final/network_{i}.csv"), row.names = F)
  print(glue('graph {i} stored'))
}


#A1 = read.csv("networks_final/network_1.csv")
#A2 = read.csv("networks_final/network_2.csv")
#
#in_degree_value1 = rev(A1[,1])[-1]
#empirical1 = rev(A1[,2])[-1]
#cumulative1 = rev(A1[,3])[-1]
#
#in_degree_value2 = rev(A2[,1])[-1]
#empirical2 = rev(A2[,2])[-1]
#cumulative2 = rev(A2[,3])[-1]
#
#plot(x = in_degree_value1, y = empirical1, log='xy')
#plot(x = in_degree_value1, y = cumulative1, log='xy')
#lines(x=in_degree_value2, y=cumulative2, log='xy')
#
