library(igraph)
#library(Rlab)
library(mc2d)
library(sdpt3r)

proc.time() - starting



starting = proc.time()

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

Num_nodes=100000

E_one=1:Num_nodes
E_two=rep( 0L,Num_nodes)

E_two[1] <-2
E_two[2] <-3
E_two[3] <-4
E_two[4] <-1

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

for (new_vertex in 5:Num_nodes){
  if (rbern(n = 1, prob=0.5) ==1) {
    get_vertex = sample(E_one[1:(new_vertex-1)], 1) # existing page
    E_two[new_vertex] <-get_vertex
  }
  else {
    get_vertex = sample(E_two[1:(new_vertex-1)], 1) # existing page
    E_two[new_vertex] <-get_vertex
  }

}

in_nodes=rep(0L,Num_nodes)
for (i in 1:Num_nodes){
  in_nodes[E_two[i]]=in_nodes[E_two[i]]+1L
}

to_plot=sort(in_nodes,decreasing=TRUE)
indexes=unique(to_plot)

cumulative=rep(0L,Num_nodes)


A=rep.row(c(0,0),length(indexes))

A[,1]=indexes

counter=0
j=1
for (i in 1:length(indexes)){
  element=A[i,1]
  while(j<=Num_nodes && to_plot[j]==element){
    counter=counter+1
    j=j+1
  }
  A[i,2]=counter
  
}


proc.time() - starting


