### R code for 'A0102680R_assignment_02.Rnw'

###################################################
### code chunk number 1: q_4a
###################################################
n<-5
i<-c(0,4,1,3)
j<-c(2,2,2,2)
alias.table<-c(0.2,0.2,0.2,0.4)
set.seed(11384)
U<-runif(1)
V<-runif(1)
U<-Vectorize(U)
V<-Vectorize(V)
AliasMethod<-function(U,V){
row.num<-floor((n-1)*U)+1
ifelse( V<alias.table[row.num], i[row.num], j[row.num])}
Draw<-AliasMethod(U,V)
Draw


###################################################
### code chunk number 2: q_4b
###################################################
#Generate Y from qi
set.seed(120)
V<-runif(1)
y<-floor(V*4)+1

#rejection algorithm
RejMethod<-function(y){
p<-c(0.05,0.1,0.7,0.1,0.05)
q<-c(0.2,0.2,0.2,0.2,0.2)
count<-0
repeat{
U<-runif(1)
count<-count+1
if( U*3.5*q[y]<=p[y])
{X<-y
break}}
return<-c(X,count)
return}
RejMethod(y)


###################################################
### code chunk number 3: q_4c
###################################################
draw<-5000

set.seed(23)
aliasfunction<-function(U,V){
U<-runif(draw)
V<-runif(draw)
X<-mapply(AliasMethod,U,V)}
system.time(aliasfunction())
table(x)/draw
#no. of RV for alias
draw*2 

set.seed(33)
V<-runif(draw)
y<-floor(V*4)+1
y<-Vectorize(y)
set.seed(33)
U<-runif(draw)
U<-Vectorize(U)
RejMethod<-Vectorize(RejMethod)
rejfunction<-function(x){
y<-sapply(y,RejMethod)}
system.time(rejfunction())
table(y)/draw
#no. of RV for rej
sum(y)*2
