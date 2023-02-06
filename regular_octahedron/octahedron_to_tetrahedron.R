#03/02/2022 00:12
#By: Igor Salerno Filgueiras
#Script to find how many tetrahedrons can be formed from a regular octahedron (considering the faces centers as possible vertexes as well)

#Step 1: Define vertexes' and face centers' coordinates----

#vertexes coordinates are basically {+-1,0,0}, {0,+-1,0} and {0,0,+-1}
#faces centers are the vertexes corresponding coordinates summed and divided by 3
coord <- structure(list(vertice = c("v1", "v2", "v3", "v4", "v5", "v6","c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"), 
                        x = c(1, 0, 0, 0, 0, -1, 1/3, 1/3, 1/3, 1/3, -1/3, -1/3, -1/3, -1/3), 
                        y = c(0, 0, 1, -1, 0, 0, 1/3, -1/3, 1/3, -1/3, 1/3,-1/3, 1/3, -1/3), 
                        z = c(0, -1, 0, 0, 1, 0, -1/3, -1/3, 1/3, 1/3, -1/3, -1/3, 1/3, 1/3)), 
                   class = c("tbl_df", "tbl", "data.frame"), 
                   row.names = c(NA, -14L))

#Step 2: List all possible tetrahedrons vertexes combinations----
poss_comb <- gtools::combinations(n = 14, r = 4, repeats.allowed = F)

#Step 3: Define a function to determine wether the 4 vertexes are coplanar----
coplanar <- function(p1,p2,p3,p4){
  #extracting coordinates
  x1 <- p1[[2]]
  y1 <- p1[[3]]
  z1 <- p1[[4]]
  x2 <- p2[[2]]
  y2 <- p2[[3]]
  z2 <- p2[[4]]
  x3 <- p3[[2]]
  y3 <- p3[[3]]
  z3 <- p3[[4]]
  x4 <- p4[[2]]
  y4 <- p4[[3]]
  z4 <- p4[[4]]
  
  #defining components
  a1 = x2 - x1
  b1 = y2 - y1
  c1 = z2 - z1
  a2 = x3 - x1
  b2 = y3 - y1
  c2 = z3 - z1
  a = b1 * c2 - b2 * c1
  b = a2 * c1 - a1 * c2
  c = a1 * b2 - b1 * a2
  d = (- a * x1 - b * y1 - c * z1)
  
  # plane equation: a*x + b*y + c*z = 0 #
  # checking if the 4th point satisfies the above equation
  if(a * x4 + b * y4 + c * z4 + d == 0){
    return(1) #coplanar
  }
  else{
    return(0) #not coplanar
  }
}

#Step 4: Check which of the combinations are coplanar (and therefore cannot form a tetrahedron)----
is_coplanar <- rep(0,1001)
for (vertexes in 1:1001) {
  is_coplanar[vertexes] <- coplanar(coord[poss_comb[vertexes,1],], coord[poss_comb[vertexes,2],], coord[poss_comb[vertexes,3],], coord[poss_comb[vertexes,4],])
}

#Step 5: Number of 0s is the number of possible tetrahedrons----
table(is_coplanar)
