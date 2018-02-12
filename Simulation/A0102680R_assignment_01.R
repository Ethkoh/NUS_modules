### R code for 'A0102680R_assignment_01.Rnw'

###################################################
### code chunk number 1: q_1a
###################################################
set.seed(12)
U.1 <- runif(1000)
U.2 <- runif(1000)
X <- 2*mapply(min, U.1, U.2)
hist(X, col='tomato2', main='Histogram of 2min(U.1,U.2)', xlab='x', ylab='', freq=FALSE)
curve(1-(x/2),0,2, col='slateblue3', lwd=1.8,add=TRUE)


###################################################
### code chunk number 2: q_1b
###################################################
set.seed(15)
U.1 <- runif(1000)
U.2 <- runif(1000)
X <- 2*abs(U.1+U.2-1)
hist(X, col='peachpuff', main='Histogram of 2abs(U.1+U.2-1)', xlab='x', ylab='', freq=FALSE)
curve(1-(x/2),0,2, col='slateblue3', lwd=1.8,add=TRUE)


###################################################
### code chunk number 3: q_1c
###################################################
set.seed(16)
U.1 <- runif(1000)
X <- 2*(1-sqrt(U.1))
hist(X, col='slateblue2', main='Histogram of 2(1-sqrt(U.1))', xlab='x', ylab='', freq=FALSE)
curve(1-(x/2),0,2, col='tomato2', lwd=1.8,add=TRUE)


###################################################
### code chunk number 4: q_2
###################################################
set.seed(18)
L <-function(theta){
if(theta<5)
return(0) else
return(1/theta^2)
}
Lvect=Vectorize(L,'theta')
X=seq(5,100,by=0.1)
plot(X,Lvect(X),col='slateblue3')


