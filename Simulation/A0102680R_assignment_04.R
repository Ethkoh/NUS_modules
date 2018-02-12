### R code from vignette source 'assignment_04_soln.Rnw'

###################################################
### code chunk number 1: question 2 compute_c
###################################################
x <- 10000
x1<-0
c.val <- dgamma(x, shape=100, scale=100)/dexp(x, rate=1/10000)
c.val.1 <- dgamma(x1, shape=100, scale=100)/dexp(x1, rate=1/10000)

###################################################
### code chunk number 2: question 3 rejection_no_control
###################################################
set.seed(444)
nsim <- 10000

count <- 1
out <- rep(0, nsim)

while(count <= nsim) {
  U <- runif(1) # for checking acceptance
  V <- runif(1) # for generating candidate
  W <- (-10000)*log(V)  # generate candidate

  # check acceptance criterion
  if(U * c.val * dexp(W, rate=1/10000) <= dgamma(W, shape=100, scale=100)) {
    out[count] <- W   #X=Y
    count <- count + 1
  }
}

# compute h function and confidence interval here 
h.X <- ifelse(out >= 11000, 11000, -2750+ 1.25*floor(out))
xbar<-mean(h.X)

#z.alpha.2=1.96
upper<-xbar+1.96*sqrt(var(h.X)/(nsim))
lower<-xbar-1.96*sqrt(var(h.X)/(nsim))
c(lower,upper) #95% Confidence interval

###################################################
### code chunk number 3: question 5 cov and var and c*
###################################################
Y<-out
cov<-cov(h.X,out)
var<-var(out)
cstar<- -cov/var

###################################################
### code chunk number 4: question 6 CI
###################################################
estimator<-h.X+cstar*(Y-10000)
estimator.final<-mean(estimator)
#z.alpha.2=1.96
upper<-estimator.final+1.96*sqrt(var(estimator)/(nsim))
lower<-estimator.final-1.96*sqrt(var(estimator)/(nsim))
c(lower,upper) #95% Confidence interval