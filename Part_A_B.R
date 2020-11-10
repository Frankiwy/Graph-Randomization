library(igraph)
#library(Rlab)
library(mc2d)
library(sdpt3r)

graph = erdos.renyi.game(100,p=1/3, type = 'gnp', directed=FALSE)
#hgraph = igraph.sbm.game(10, p=1/10)

g_matrix = as.matrix(as_adj(graph))
g_dimension = dim(g_matrix)[1]

sum = 0
for (n in 1:10000){
  mask = rbern(n = g_dimension, prob=.5)
  vector = c(1:g_dimension)
  indeces_U = vector[!!mask]
  indeces_not_U = vector[!mask]
  num_edges = sum(g_matrix[indeces_U, indeces_not_U])
  sum = sum+num_edges
}
exp = sum/10000
exp
maxcut(g_matrix)$pobj
exp


# PART B ------------------------------------------------------------------
starting = proc.time()

#g <- make_empty_graph() %>%
#  add_vertices(4, color='purple') %>%
#  add_edges(c(1,2, 2,3, 3,4, 4,1))
#plot(g)
#V(g)


g <- make_empty_graph()
g <- g + vertex(1, color='green')
for (m in 2:4){
  old_vertex = tail(V(g), n=1)
  g <- g + 
    vertex(m, color='green') +
    edge(old_vertex, m)
}
g <- g + edge(4,1)
#plot(g)

for (m in 1:10000){
  verteces = V(g)
  new_vertex = tail(V(g), n=1) # get last vertex
  if (rbern(n = 1, prob=.5) ==1) {
    get_vertex = sample(V(g), 1) # existing page
    new_vertex = new_vertex+1 # new page
    g <- g + 
      vertex(new_vertex, color='green') +
      edge(new_vertex, get_vertex)
  }
  else {
    get_vertex = sample(as_edgelist(g)[,2],1)
    new_vertex = new_vertex+1 # new page
    g <- g + 
      vertex(new_vertex, color='green') +
      edge(new_vertex, get_vertex)
    
  }
  #print(new_vertex)
}
#plot(g)

#plot(degree_distribution(g, cumulative = TRUE))


proc.time() - starting



starting = proc.time()

E = matrix(nrow = 0, ncol = 2)
E <- rbind(E, c(1,2))
E <- rbind(E, c(2,3))
E <- rbind(E, c(3,4))
E <- rbind(E, c(4,1))
E

#francescovv
#FrancescoVV

for (new_vertex in 5:1000000){
  if (rbern(n = 1, prob=.5) ==1) {
    get_vertex = sample(E[,1], 1) # existing page
    E <- rbind(E, c(new_vertex, get_vertex))
  }
  else {
    get_vertex = sample(E[,2], 1) # existing page
    E <- rbind(E, c(new_vertex, get_vertex))
  }
  
  if (new_vertex%%10000 == 0) print(new_vertex)
  
  #print(new_vertex)
}

#g <- graph_from_edgelist(E, directed = TRUE)
#plot(g, width = .3, arrow.size= .05)

proc.time() - starting
















