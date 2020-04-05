y1 <- 0
y2 <- 1
a1 <- 0.8
a2 <- 0.1
F <- matrix(c(a1,a2,1,0), byrow = T, ncol = 2)
#forecast
Y <- matrix(c(y1,y2), ncol = 1)
forec3 <- F%*%F%*%F%*%Y
forec3

#
