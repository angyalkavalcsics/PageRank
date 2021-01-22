# Need to create an adjacency matrix from web data
# just for now, let
A <- matrix(c(0, 1, 0, 0, 0,
              0, 0, 0, 0, 1,
              1, 1, 0, 1, 1,
              0, 0, 1, 0, 1,
              0, 0, 0, 1, 0), nrow = 5, ncol = 5, byrow = TRUE)
library(igraph)
g <- graph.adjacency(A,mode="directed")
plot(g)

# Notice that the rows of the adjacency matrix represent the outgoing
# edges of a vertex and the columns represent the incoming edges

# A:= the adjacency matrix
# d:= the damping factor
# iter:= how many iterations of the algorithm will run
PageRank1 <- function(A, d = 0.85, iter){
   # find number of vertices p
   p <- nrow(A)
   # create list of initial ranks
   # initial ranks are equal to 1/(p)
   prev.ranks <- c(rep(1/p, p))
   # empty list to hold updated rank 
   new.ranks <- c(rep(0,p))
   # Iterate until convergence
   t = 0
   while (t < iter) {
      # iterate over all vertices U in the graph
      for (U in 1:p) {
         # get vertices V which U has incoming edges from
         incoming <- which(A[,U] == 1)
         # iterate over incoming vertices
         for (V in incoming) {
            # get number of outgoing edges from each vertex V
            # in incoming and update ranks
            outgoing <- length(which(A[V,] == 1))
            update <- prev.ranks[V]/outgoing
            # update
            new.ranks[U] = new.ranks[U] + update
         }
         # include damping factor
         new.ranks[U] <- ((1-d)/p) + d*new.ranks[U]
      }
      prev.ranks <- new.ranks
      new.ranks <- c(rep(0,p))
      t = t+1
   }
   return(prev.ranks)
}

r <- PageRank(A, d = 0.85, iter = 5)
sum(r)

# A:= the adjacency matrix
# d:= the damping factor
# iter:= how many iterations of the algorithm will run
PageRank2 <- function(A, d = 0.85, iter){
   # Need to transpose A so that the columns correspond to
   # the outgoing links and the rows correspond to the 
   # incoming links. 
   # Then, the probabilities in M correspond to the 
   # probability of which link you will follow. 
   A <- t(A)
   N <- nrow(A)
   deg <- colSums(A)
   M <- sweep(A, 2, deg, FUN = '/')
   M[is.na(M)] <- 0 # This is when we should correct for sinks
   E <- matrix(1, N, N)
   # create list of initial ranks
   # initial ranks are equal to 1/(p)
   prev.ranks <- c(rep(1/p, p))
   # empty list to hold updated rank 
   new.ranks <- c(rep(0,p))
   # Create transition matrix
   S <- d*M + ((1-d)/N)*E
   # Iterate until convergence
   t = 0
   while (t < iter) {
      new.ranks <- S%*%prev.ranks
      prev.ranks <- new.ranks
      new.ranks <- c(rep(0,p))
      t = t+1
      }
   return(prev.ranks)
}

r2 <- PageRank(A, d = 0.85, iter = 5)
sum(r2)

# A:= the adjacency matrix
# d:= the damping factor
# iter:= how many iterations of the algorithm will run

# A note about A: the columns of A need to correspond to
# the outgoing links and the rows must correspond to the 
# incoming links.
PageRank <- function(A, d = 0.85, iter){
   p <- nrow(A)
   # sink has no outgoing links,
   # replace with 
   sinks <- which(colSums(A)==0)
   fix.sinks <- rep(1, p)
   for (i in sinks) {
      A[,i] <- fix.sinks
   }
   # no self loops
   diag(A) <- 0
   deg <- colSums(A)
   M <- sweep(A, 2, deg, FUN = '/')
   M[is.na(M)] <- 0 
   e <- c(rep(1,p))
   # create list of initial ranks
   # initial ranks are equal to 1/(p)
   prev.ranks <- c(rep(1/p, p))
   # empty list to hold updated rank 
   new.ranks <- c(rep(0,p))
   # b is defined in the document
   b <- ((1-d)/p)*e
   # Iterate until convergence
   j = 0
   while (j < iter) {
      # w_j is defined in the document
      w_j <- M%*%prev.ranks
      new.ranks <- (d*w_j)+b
      prev.ranks <- new.ranks
      new.ranks <- c(rep(0,p))
      j = j+1
   }
   return(prev.ranks)
}

# Ex with no sink
A <- matrix(c(0, 0, 1, 0,
              1, 0, 0, 0,
              0, 1, 0, 1,
              0, 0, 1, 0,
              0, 1, 0, 1), nrow = 4, ncol = 4, byrow = TRUE)
# library(igraph)
# dev.off()
cmap <- c("#db3236", "#4885ed", "#f4c20d", "#3cba54")
rank <- PageRank(t(A), d = 0.85, iter = 10)
vertex_attr(g, "label") = round(rank, 3)
plot(g, vertex.color = cmap, edge.arrow.size = 0.4, edge.color = "black", vertex.size = 30)




f <- graph.adjacency(t(C),mode="directed")
plot(f)

PageRank(A, d = 0.85, iter = 5)
PageRank(A, d = 0, iter = 2)
PageRank(A, d = 1, iter = 5)
PageRank(A, d = 0.5, iter = 5)




