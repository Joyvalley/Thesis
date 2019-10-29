M <-  matrix(c(1,-1,0,0,0,1,1,0,1,-1,0,-1),nrow=3)
MtM  <- M %*% t(M)
tMM  <- t(M) %*% (M)
mafs = c(0.3,0.2,0.1,0.15)
P = matrix(rep(2*(mafs - 0.5),3),nrow=3,byrow=T)
Z = M - P
G = (Z %*% t(Z)) /  (2 * sum(mafs*(1-mafs) ))
G
