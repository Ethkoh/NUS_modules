### R code for 'A0102680R_assignment_03.Rnw'

###################################################
### code chunk number 1: qn_1
###################################################
# Code for generating from Bin(n,p)
rbinom2 <- function(n, p) {
  U <- runif(n)
  sum(U <= p)
}

# A function to simulate the total amount of claims for one month.
oneMonthRun <- function() {
  #SIMULATE THE NUMBER OF CLAIMS HERE.
  num.claims <- rbinom2(1000,0.05)
  if(num.claims == 0)
    return(0) 
  else {
  # SIMULATE THE AMOUNT OF EACH OF THE CLAIMS HEER.
    U <- runif(num.claims)
    E <-(-800)*log(U)
  }
    sum(E)
}

set.seed(12)
#RUN oneMonthRun() 100000 times
X<-sapply(1:100000,function(x) oneMonthRun())
# ESTIMATE THE FRACTION OF TIMES THE TOTAL CLAIM AMOUNT IS MORE THAN
# 50000.
mean(X>50000)


###################################################
### code chunk number 2: qn_2
###################################################
# Code for generating a homogeneous Poisson process, and return the arrival
# times S
homogPP1 <- function(lambda, T) {
  t <- 0
  i <- 0
  S <- NULL
  
  repeat{
    U <- runif(1)
    t <- t - (1/lambda)*log(U)
    if(t > T)
      break
    i <- i + 1
    S[i] <- t
  }

  S
}

# A function to sample the number of fans on a single bus.
sampleNumFansOnOneBus <- function() {
  # GENERATE FROM DISCRETE UNIFORM ON 20, 21, ..., 40
    U1<-floor(21*runif(1))+20
}

# A function to simulate one run of the process up to 7pm and find the total
# number of fans that have arrived.
oneHourRun <- function() {
  # Simulate one run of the process up to 7pm and find the length.
  # This will correspond to the number of buses that arrived.
  num.bus.arrivals <- length(homogPP1(5, 1))

  # Sample the number of fans on each bus that has arrived
  num.fan.arrivals.each.bus <- sapply(1:num.bus.arrivals, function(x)
    sampleNumFansOnOneBus())

  # SUM THE FANS ON EACH BUS TO GET THE TOTAL NUMBER OF FANS BY 7PM
    sum(num.fan.arrivals.each.bus)
}

set.seed(45)
# RUN oneHourRun() 100000 times
out <- sapply(1:100000, function(x) oneHourRun())

# ESTIMATE THE FRACTION OF TIMES THE NUMBER OF FANS IS AT LEAST 200.
mean(out>=200)


###################################################
### code chunk number 3: qn_3
###################################################
# A function to run one simulation and return the number of customers in the
# still in the bank at T=11
oneRunOutput <- function() {
  # RUN THE POISSON PROCESS UP TO T=11
  P <- homogPP1(0.66,11)

  # GENERATE THE SERVICE TIMES
  U <- runif(length(P))
  service.times <- ifelse(U <=0.35, 2.3,9.9)

  # Return the number of customers whose arrival + service time is 11 or more.
  sum(P + service.times >= 11)
}

set.seed(78)
# YOUR CODE: RUN oneRunOutput() 100000 TIMES.
out <- sapply(1:100000, function(x) oneRunOutput())

# Return the mean of your output as the answer.
mean(out)


