#####Rogers model infinite states

library(Cairo)
library(rethinking)

#geometric mean fcn
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

####Rogers infinite states, frequencies
w0 <- 0.1
b <- 2
c <- 1.2
U <- 0.3
s <- 0.8
timesteps <- 200
#wI <- w0 + b - c

q <- p <- u <-  rep(0,timesteps + 1 )
wS <- wI  <-  rep(0,timesteps )

q[1] <- 1
p[1] <- 0.5
u[1] <- 0

for (t in 1:timesteps){
	wI[t] <- w0 + s*b - c #just doing it in loop for vectorization and graphing ease
	wS[t] <- w0 + b*q[t] #fitness of social learners
	q[t+1] <- (1-u[t])*( (1-p[t])*s + p[t]*q[t]) + u[t]*0
	p[t+1] <- p[t]*wS[t]/(p[t]*wS[t] + (1-p[t])*wI)
	u[t+1] <- rbinom(n=1,prob=U,size=1) #sample environment changing in next gen, if 0 all SL behav maladaptive
}

par(mfrow = c(4, 1))
par(cex = 0.4)
par(oma = c(4,2,1,1))
par(mar = c(2,4,1,1))

plot( p ~ c(1:(timesteps + 1) ) , ylim=c(0,1) , pch=19 , col="white" , ylab="frequency of social learners (p)" ,  xlab="time (# generations)")
lines( (1:(timesteps +1)) , p , col="red", lty=1, lwd=1)
abline(h=mean(p) , lty=2 , lw=1)
title(paste("u=",U,"; s=",s,"; b=",b,"; c=",c, sep=" ") , outer=TRUE , cex.main=2 , line=-0.5)

plot( q ~ c(1:(timesteps + 1) ) , ylim=c(0,1) , pch=19 , col="white" , ylab="frequency of adapted social learners (q)" ,  xlab="time (# generations)")
lines( (1:(timesteps +1)) , q , col="blue", lty=1, lwd=1)
abline(h=mean(q) , lty=2 , lw=1)

plot( wS ~ c(1:(timesteps) ) , ylim=c(0,2) , pch=19 , col="white" , ylab="fitness of social learners (wS)" ,  xlab="time (# generations)")
lines( c(1:timesteps) , wS , col="orange", lty=1, lwd=1)
abline(h=gm_mean(wS) , lty=2 , lw=1)

plot( wI ~ c(1:timesteps ) , ylim=c(0,2) , pch=19 , col="white" , ylab="fitness of individual learners (wI)" ,  xlab="time (# generations)")
lines( c(1:timesteps ) , wI , col="green", lty=1, lwd=1)
abline(h=gm_mean(wI) , lty=2 , lw=1)


############discreet pop size with carryiing capacity########
K <- 500  #carying capacity
w0 <- 1 #baseline fitness
b <- 2  #benfit of adaptive behavior
c <- 1.4  #cost of IL
U <- 0.2 	#rate of enviro change
s <- 0.9    #prob of IL acquiring adaptive behavior
timesteps <- 100

q <- p <- u <- nI <- nS <- N <-  rep(0,timesteps + 1 )
wS <- wI  <-  rep(0,timesteps )

###initial states
q[1] <- 1    #all social learners have adaptive behavior
p[1] <- 0.5   #50.50 IL/SL mix
u[1] <- 0     #stable env
nI[1] <- 100
nS[1] <- 100
N[1] <- nI[1] + nS[1]

for (t in 1:timesteps){
	#s <- rbinom(n=1,prob=S,size=nI)
	wI[t] <- w0 + s*b - c #just doing it in loop for vectorization and graphing ease
	wS[t] <- w0 + b*q[t] #fitness of social learners
	q[t+1] <- (1-u[t])*( (nI[t]/N[t])*s + (nS[t]/N[t])*q[t]) + u[t]*0
	#nS[t+1] <- round(nS[t]*wS[t]*(1-m))
	#nI[t+1] <- round(nI[t]*wI[t]*(1-m))
	nS[t+1] <- round(nS[t]*wS[t]*(1- (nS[t] + nI[t])/K)) #rounding seems wrong, bur update numer odSL in next gen
	nI[t+1] <- round(nI[t]*wI[t]*(1- (nS[t] + nI[t])/K)) #number of IL in next gen
	N[t+1] <- nS[t+1] + nI[t+1]   #num up for N (total #individual)
	p[t+1] <- nS[t+1]/N[t+1]  #freuency of SL in next gen
	u[t+1] <- rbinom(n=1,prob=U,size=1) #sample environment changing in next gen, if 0 all SL behav maladaptive
}


par(mfrow = c(6, 1))
par(cex = 0.4)
par(oma = c(4,2,1,1))
par(mar = c(2,4,1,1))

plot( p ~ c(1:(timesteps + 1) ) , ylim=c(0,1) , pch=19 , col="white" , ylab="frequency of social learners (p)" ,  xlab="time (# generations)")
lines( (1:(timesteps +1)) , p , col="gray", lty=1, lwd=1)
abline(h=mean(p) , lty=2 , lw=1)

plot( q ~ c(1:(timesteps + 1) ) , ylim=c(0,1) , pch=19 , col="white" , ylab="frequency of adapted social learners (q)" ,  xlab="time (# generations)")
lines( (1:(timesteps +1)) , q , col="blue", lty=1, lwd=1)
abline(h=mean(q) , lty=2 , lw=1)

plot( nI ~ c(1:(timesteps + 1) ) , ylim=c(0, K + 1) , pch=19 , col="white" , ylab="number of individual learners" ,  xlab="time (# generations)")
lines( (1:(timesteps +1)) , nI , col="red", lty=1, lwd=1)
abline(h=mean(nI) , lty=2 , lw=1)

plot( nS ~ c(1:(timesteps + 1) ) , ylim=c(0, K + 1) , pch=19 , col="white" , ylab="number of social learners" ,  xlab="time (# generations)")
lines( (1:(timesteps +1)) , nS , col="blue", lty=1, lwd=1)
abline(h=mean(nS) , lty=2 , lw=1)

plot( wS ~ c(1:(timesteps) ) , ylim=c(0,3) , pch=19 , col="white" , ylab="fitness of social learners (wS)" ,  xlab="time (# generations)")
lines( c(1:timesteps) , wS , col="orange", lty=1, lwd=1)
abline(h=mean(wS) , lty=2 , lw=1)

plot( wI ~ c(1:timesteps ) , ylim=c(0,3) , pch=19 , col="white" , ylab="fitness of individual learners (wI)" ,  xlab="time (# generations)")
lines( c(1:timesteps ) , wI , col="green", lty=1, lwd=1)
abline(h=mean(wI) , lty=2 , lw=1)



############discreet pop size with carryiing capacity and discreet numbers########

w0 <- 1 #baseline fitness
b <- 2 #fitness benefit of right behavior
c <- .2 # cost of individual learning
U <- 0.3 #rate of environmental change
S <- 1 #probability of acquiring succesful behavior via individual learning
timesteps <- 100 #length of simulation
#wI <- w0 + b - c

q <- p <- u <- nI <- nS <- N <-  rep(0,timesteps + 1 )  #vectors for simulation estimates
wS <- wI  <-  rep(0,timesteps ) #vectors for fitness values

q[1] <- 1   #initial frequency of adaptive behavior in social learners
p[1] <- 0.5 #ifrequency of social learners
u[1] <- 0   #initial environmental state (Stable=0)
nI[1] <- 100 # number of individual learners
nS[1] <- 100 # number of sociallearners
N[1] <- nI[1] + nS[1]	#initial pop size
K <- 1000	#carrying capacity
for (t in 1:timesteps){
	#s <- (rbinom(n=1,prob=S,size=nI[t]))/nI[t] #proportion of individuals with adaptive behavior based on S
	s <- 0.9
	wI[t] <- w0 + s*b - c #just doing it in loop for vectorization and graphing ease
	wS[t] <- w0 + b*q[t] #fitness of social learners
	q[t+1] <- (1-u[t])*( (nI[t]/N[t])*s + (nS[t]/N[t])*q[t]) + u[t]*0

	nS[t+1] <- nS[t]*wS[t]*(1- (nS[t] + nI[t])/K)
	nI[t+1] <- nI[t]*wI[t]*(1- (nI[t] + nS[t])/K)
	N[t+1] <- nS[t+1] + nI[t+1]
	p[t+1] <- nS[t+1]/N[t+1]
	u[t+1] <- rbinom(n=1,prob=U,size=1) #sample environment changing in next gen, if 0 all SL behav maladaptive
}


par(mfrow = c(6, 1))
par(cex = 0.4)
par(oma = c(4,2,1,1))
par(mar = c(2,4,1,1))

plot( p ~ c(1:(timesteps + 1) ) , ylim=c(0,1) , pch=19 , col="white" , ylab="frequency of social learners (p)" ,  xlab="time (# generations)")
lines( (1:(timesteps +1)) , p , col="gray", lty=1, lwd=1)
abline(h=mean(p) , lty=2 , lw=1)

plot( q ~ c(1:(timesteps + 1) ) , ylim=c(0,1) , pch=19 , col="white" , ylab="frequency of adapted social learners (q)" ,  xlab="time (# generations)")
lines( (1:(timesteps +1)) , q , col="blue", lty=1, lwd=1)
abline(h=mean(q) , lty=2 , lw=1)

plot( nI ~ c(1:(timesteps + 1) ) , ylim=c(0, K + 1) , pch=19 , col="white" , ylab="number of individual learners" ,  xlab="time (# generations)")
lines( (1:(timesteps +1)) , nI , col="red", lty=1, lwd=1)
abline(h=mean(nI) , lty=2 , lw=1)

plot( nS ~ c(1:(timesteps + 1) ) , ylim=c(0, K + 1) , pch=19 , col="white" , ylab="number of social learners" ,  xlab="time (# generations)")
lines( (1:(timesteps +1)) , nS , col="blue", lty=1, lwd=1)
abline(h=mean(nS) , lty=2 , lw=1)

plot( wS ~ c(1:(timesteps) ) , ylim=c(0,3) , pch=19 , col="white" , ylab="fitness of social learners (wS)" ,  xlab="time (# generations)")
lines( c(1:timesteps) , wS , col="orange", lty=1, lwd=1)
abline(h=mean(wS) , lty=2 , lw=1)

plot( wI ~ c(1:timesteps ) , ylim=c(0,3) , pch=19 , col="white" , ylab="fitness of individual learners (wI)" ,  xlab="time (# generations)")
lines( c(1:timesteps ) , wI , col="green", lty=1, lwd=1)
abline(h=mean(wI) , lty=2 , lw=1)
