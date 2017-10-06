#####Rogers model infinite states

library(Cairo)
library(rethinking)

#geometric mean fcn
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

####Rogers infinite states, frequencies
w0 <- 0.5
b <- 2
c <- 1.2
U <- 0.1
s <- 0.8
timesteps <- 1000
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


############Rogers infinite states with carryiing capacity########
K <- 1000  #carying capacity
w0 <- 1 #baseline fitness
b <- 2  #benfit of adaptive behavior
c <- 1  #cost of IL
U <- 0.25 	#rate of enviro change
s <- 1    #prob of IL acquiring adaptive behavior
timesteps <- 500

q <- p <- u <- nI <- nS <- N <-  rep(0,timesteps + 1 )
wS <- wI  <-  rep(0,timesteps )

###initial states
q[1] <- 1    #all social learners have adaptive behavior
p[1] <- 0.5   #50.50 IL/SL mix
u[1] <- 0     #stable env
nI[1] <- 0.1*K
nS[1] <- 0.1*K
N[1] <- nI[1] + nS[1]

for (t in 1:timesteps){
	#s <- rbinom(n=1,prob=S,size=nI)
	wI[t] <- w0 + s*b - c #just doing it in loop for vectorization and graphing ease
	wS[t] <- w0 + b*q[t] #fitness of social learners
	q[t+1] <- (1-u[t])*( (nI[t]/N[t])*s + (nS[t]/N[t])*q[t]) + u[t]*0
	#nS[t+1] <- round(nS[t]*wS[t]*(1-m))
	#nI[t+1] <- round(nI[t]*wI[t]*(1-m))
	nS[t+1] <- nS[t]*wS[t]*(1 - (N[t]/K)) #rounding seems wrong, bur update numer odSL in next gen
	nI[t+1] <- nI[t]*wI[t]*(1 - (N[t]/K)) #number of IL in next gen
	N[t+1] <- nS[t+1] + nI[t+1]   #num up for N (total #individual)
	p[t+1] <- nS[t+1]/N[t+1]  #freuency of SL in next gen
	u[t+1] <- rbinom(n=1,prob=U,size=1) #sample environment changing in next gen, if 0 all SL behav maladaptive
}


par(mfrow = c(7, 1))
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

plot( N ~ c(1:(timesteps + 1) ) , ylim=c(0, K + 1) , pch=19 , col="white" , ylab="population size" ,  xlab="time (# generations)")
lines( (1:(timesteps +1)) , N , col="black", lty=1, lwd=1)
abline(h=mean(N) , lty=2 , lw=1)

plot( wS ~ c(1:(timesteps) ) , ylim=c(0,3) , pch=19 , col="white" , ylab="fitness of social learners (wS)" ,  xlab="time (# generations)")
lines( c(1:timesteps) , wS , col="orange", lty=1, lwd=1)
abline(h=mean(wS) , lty=2 , lw=1)

plot( wI ~ c(1:timesteps ) , ylim=c(0,3) , pch=19 , col="white" , ylab="fitness of individual learners (wI)" ,  xlab="time (# generations)")
lines( c(1:timesteps ) , wI , col="green", lty=1, lwd=1)
abline(h=mean(wI) , lty=2 , lw=1)


#########loop over values of w0

#######Rogers infinite states with carryiing capacity########
K <- 1000  #carying capacity
w0 <- seq(from=0 , to=1 , by=0.1) #baseline fitness
b <- 2  #benfit of adaptive behavior
c <- 1  #cost of IL
U <- 0.25 	#rate of enviro change
s <- 1    #prob of IL acquiring adaptive behavior
timesteps <- 100
nsims <- 12 #number or simulations for each condition

q <- p <- u <- nI <- nS <- N <-  array(0, dim = c(timesteps + 1,nsims,length(w0)))

wS <- wI  <- array(0, dim = c(timesteps,nsims,length(w0)))

q[1,,] <- 1    #all social learners have adaptive behavior
p[1,,] <- 0.5   #50.50 IL/SL mix
u[1,,] <- 0     #stable env


#rep(0,timesteps )
for (k in 1:length(w0)){ #function of whatever parameter one is varying
	nI[1,,k] <- 0.1*K
	nS[1,,k] <- 0.1*K
	N[1,,k] <- nI[1,,k] + nS[1,,k]
###initial states
	for ( j in 1:nsims){
		for (t in 1:timesteps){
			wI[t,j,k] <- w0[k] + s*b - c #just doing it in loop for vectorization and graphing ease
			wS[t,j,k] <- w0[k] + b*q[t,j,k] #fitness of social learners
			q[t+1,j,k] <- (1-u[t,j,k])*( (nI[t,j,k]/N[t,j,k])*s + (nS[t,j,k]/N[t,j,k])*q[t,j,k]) + u[t,j,k]*0
			nS[t+1,j,k] <- nS[t,j,k]*wS[t,j,k]*(1 - (N[t,j,k]/K)) #rounding seems wrong, bur update numer odSL in next gen
			nI[t+1,j,k] <- nI[t,j,k]*wI[t,j,k]*(1 - (N[t,j,k]/K)) #number of IL in next gen
			N[t+1,j,k] <- nS[t+1,j,k] + nI[t+1,j,k]   #num up for N (total #individual)
			p[t+1,j,k] <- nS[t+1,j,k]/N[t+1,j,k]  #freuency of SL in next gen
			u[t+1,j,k] <- rbinom(n=1,prob=U,size=1) #sample environment changing in next gen, if 0 all SL behav maladaptive
		}

	}
}
