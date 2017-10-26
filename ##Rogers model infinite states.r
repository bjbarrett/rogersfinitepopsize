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
c <- 1.9
U <- 0.05
s <- 1
timesteps <- 100
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
title(paste("u=",U,"; s=",s,"; b=",b,"; c=",c,"; w0=",w0, sep=" ") , outer=TRUE , cex.main=2 , line=-0.5)
points(  seq(0:timesteps) , (u-1) , pch=17)
 
plot( q ~ c(1:(timesteps + 1) ) , ylim=c(0,1) , pch=19 , col="white" , ylab="frequency of adapted social learners (q)" ,  xlab="time (# generations)")
lines( (1:(timesteps +1)) , q , col="blue", lty=1, lwd=1)
abline(h=mean(q) , lty=2 , lw=1)
points(  seq(0:timesteps) , (u-1) , pch=17)

plot( wS ~ c(1:(timesteps) ) , ylim=c(0,b+w0) , pch=19 , col="white" , ylab="fitness of social learners (wS)" ,  xlab="time (# generations)")
lines( c(1:timesteps) , wS , col="orange", lty=1, lwd=1)
abline(h=gm_mean(wS) , lty=2 , lw=1)
points(  seq(0:timesteps) , (u-1) , pch=17)

plot( wI ~ c(1:timesteps ) , ylim=c(0,b+w0) , pch=19 , col="white" , ylab="fitness of individual learners (wI)" ,  xlab="time (# generations)")
lines( c(1:timesteps ) , wI , col="green", lty=1, lwd=1)
abline(h=gm_mean(wI) , lty=2 , lw=1)
points(  seq(0:timesteps) , (u-1) , pch=17)


############Rogers infinite states with carryiing capacity########
K <- 1000  #carying capacity
w0 <- .5 #baseline fitness
b <- 3  #benfit of adaptive behavior
c <- 1  #cost of IL
U <- 0.2 	#rate of enviro change
s <- 1    #prob of IL acquiring adaptive behavior
timesteps <- 100

q <- p <- u <- nI <- nS <- N <-  rep(0,timesteps + 1 )
wS <- wI  <-  rep(0,timesteps )

###initial states
q[1] <- 1    #all social learners have adaptive behavior
p[1] <- 0.5   #50.50 IL/SL mix
u[1] <- 0     #stable env
nI[1] <- 0.2*K
nS[1] <- 0.2*K
N[1] <- nI[1] + nS[1]

for (t in 1:timesteps){
	#s <- rbinom(n=1,prob=S,size=nI)
	wI[t] <- w0 + s*b - c #just doing it in loop for vectorization and graphing ease
	wS[t] <- w0 + b*q[t] #fitness of social learners
	q[t+1] <- (1-u[t])*( (nI[t]/N[t])*s + (nS[t]/N[t])*q[t]) + u[t]*0
	nS[t+1] <- nS[t]*wS[t]*(1 - (N[t]/K)) #rounding seems wrong, bur update numer odSL in next gen
	nI[t+1] <- nI[t]*wI[t]*(1 - (N[t]/K)) #number of IL in next gen
	N[t+1] <- nS[t+1] + nI[t+1]   #num up for N (total #individual)
	p[t+1] <- nS[t+1]/N[t+1]  #freuency of SL in next gen
	u[t+1] <- rbinom(n=1,prob=U,size=1) #sample environment changing in next gen, if 0 all SL behav maladaptive
}


par(mfrow = c(7, 1))
par(cex = 0.4)
par(oma = c(4,2,3,1))
par(mar = c(2,4,1,1))

plot( p ~ c(1:(timesteps + 1) ) , ylim=c(0,1) , pch=19 , col="white" , ylab="frequency of social learners (p)" ,  xlab="time (# generations)")
lines( (1:(timesteps +1)) , p , col="gray", lty=1, lwd=1)
abline(h=mean(p) , lty=2 , lw=1)
points(  seq(0:timesteps) , (u-1) , pch=17)

plot( q ~ c(1:(timesteps + 1) ) , ylim=c(0,1) , pch=19 , col="white" , ylab="frequency of adapted social learners (q)" ,  xlab="time (# generations)")
lines( (1:(timesteps +1)) , q , col="blue", lty=1, lwd=1)
abline(h=mean(q) , lty=2 , lw=1)
points(  seq(0:timesteps) , (u-1) , pch=17)

plot( nI ~ c(1:(timesteps + 1) ) , ylim=c(0, K + 1) , pch=19 , col="white" , ylab="number of individual learners" ,  xlab="time (# generations)")
lines( (1:(timesteps +1)) , nI , col="red", lty=1, lwd=1)
abline(h=mean(nI) , lty=2 , lw=1)
#points(  seq(0:timesteps) , (u-1) , pch=17)

plot( nS ~ c(1:(timesteps + 1) ) , ylim=c(0, K + 1) , pch=19 , col="white" , ylab="number of social learners" ,  xlab="time (# generations)")
lines( (1:(timesteps +1)) , nS , col="blue", lty=1, lwd=1)
abline(h=mean(nS) , lty=2 , lw=1)
#points(  seq(0:timesteps) , (u-1) , pch=17)

plot( N ~ c(1:(timesteps + 1) ) , ylim=c(0, K + 1) , pch=19 , col="white" , ylab="population size" ,  xlab="time (# generations)")
lines( (1:(timesteps +1)) , N , col="black", lty=1, lwd=1)
abline(h=mean(N) , lty=2 , lw=1)
#points(  seq(0:timesteps) , (u-1) , pch=17)

plot( wS ~ c(1:(timesteps) ) , ylim=c(0,3) , pch=19 , col="white" , ylab="fitness of social learners (wS)" ,  xlab="time (# generations)")
lines( c(1:timesteps) , wS , col="orange", lty=1, lwd=1)
abline(h=mean(wS) , lty=2 , lw=1)
points(  seq(0:timesteps) , (u-1) , pch=17)

plot( wI ~ c(1:timesteps ) , ylim=c(0,3) , pch=19 , col="white" , ylab="fitness of individual learners (wI)" ,  xlab="time (# generations)")
lines( c(1:timesteps ) , wI , col="green", lty=1, lwd=1)
abline(h=mean(wI) , lty=2 , lw=1)
points(  seq(0:timesteps) , (u-1) , pch=17)

title(paste("u=",U,";b=",b,";c=",c,";s=",s,";K=",K, sep=" ") , outer=TRUE , cex.main=2 , line=1)
mtext("# generations / time", 1 , line=3.5)


#########loop over values of w0

#######Rogers infinite states with carryiing capacity########
K <- 1000  #carying capacity
w0 <- seq(from=.1 , to=1 , by=0.1) #baseline fitness
b <- 1.1  #benfit of adaptive behavior
c <- 1  #cost of IL
U <- 0.2 	#rate of enviro change
s <- 1    #prob of IL acquiring adaptive behavior
timesteps <- 100
nsims <- 2000 #number or simulations for each condition

q <- p <- u <- nI <- nS <- N <-  array(0, dim = c(timesteps + 1,nsims,length(w0)))

wS <- wI  <- array(0, dim = c(timesteps,nsims,length(w0)))

q[1,,] <- 1    #all social learners have adaptive behavior
p[1,,] <- 0.5   #50.50 IL/SL mix
u[1,,] <- 0     #stable env


#rep(0,timesteps )
for (k in 1:length(w0)){ #function of whatever parameter one is varying
	nI[1,,k] <- 0.2*K
	nS[1,,k] <- 0.2*K
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


###what proportion go extinct 
nI[,j,k]
min(nI[,j,1])
min.nI <- min.N <- min.nS <- mint.nI <- mint.N <- mint.nS <- array(NA, dim = c(nsims,length(w0)))

for(k in 1:length(w0)){
	for (j in 1:nsims){
		min.nI[j,k] <- min(nI[,j,k]) #min number individual learners in each simulation across each condition
		min.nS[j,k] <- min(nS[,j,k]) #min number social learners in each simulation across each condition
		min.N[j,k] <- min(N[,j,k])
		#mint.nI[j,k] <- min(nI[,j,k])
		#mint.nS[j,k] <- min(nS[,j,k])
		mint.N[j,k] <- ifelse( is.finite(min(which( N[,j,k] <1))) , min(which( N[,j,k] <1)) , mint.N[j,k] ) #gives a vector of timesteps where extinction occurs, NAs include persistance
		mint.nS[j,k] <- ifelse( is.finite(min(which( nS[,j,k] <1))) , min(which( nS[,j,k] <1)) , mint.nS[j,k] ) 
		mint.nI[j,k] <- ifelse( is.finite(min(which( nI[,j,k] <1))) , min(which( nI[,j,k] <1)) , mint.nI[j,k] ) 

	}
}




#####plot of minimum population size
par(mfrow = c(length(w0), 1))
par(cex = 0.3)
par(oma = c(4,2,3,1))
par(mar = c(2,3,0,0))

for(k in 1:length(w0)){
	dens(min.N[,k], xlim=c(0,max(min.N)) , show.HPDI=0.8)
	abline(v=median(min.N[,k]))
	points(min.N[,k],rep(0,nsims) , pch="|", col=col.alpha("black", 0.25))
	title(paste("w0=",w0[k], sep=" ") , outer=FALSE , cex.main=2 , line=-1.5)

	#dens(min.nS[,k], xlim=c(0,max(N)) , col="orange" , add=TRUE)
	#dens(min.nI[,k], xlim=c(0,max(N))  , col="blue" , add=TRUE)
	#abline(v=median(min.nI[,k]) , col="blue")
}

mtext("Minimum Population Size", 1 , line=3.5)
title(paste("u=",U,";b=",b,";c=",c,";s=",s,";K=",K, sep=" ") , outer=TRUE , cex.main=2 , line=1)

####minimum number of social learners
par(mfrow = c(length(w0), 1))
par(cex = 0.3)
par(oma = c(4,2,3,1))
par(mar = c(2,3,0,0))

for(k in 1:length(w0)){
	dens(min.nS[,k], xlim=c(0,max(min.N)) , col="orange", show.HPDI=0.8 )
	abline(v=median(min.nS[,k]) , col="orange")
	points(min.nS[,k],rep(0,nsims) , pch="|", col=col.alpha("black", 0.25))
	title(paste("w0=",w0[k], sep=" ") , outer=FALSE , cex.main=2 , line=-1.5)

}

mtext("Minimum Number of Social Learners", 1 , line=3.5)
title(paste("u=",U,";b=",b,";c=",c,";s=",s,";K=",K, sep=" ") , outer=TRUE , cex.main=2 , line=1)

###min num individual learners
par(mfrow = c(length(w0), 1))
par(cex = 0.3)
par(oma = c(4,2,3,1))
par(mar = c(2,3,0,0))

for(k in 1:length(w0)){
	dens(min.nI[,k], xlim=c(0,max(min.N)) , col="blue", show.HPDI=0.8 )
	abline(v=median(min.nI[,k]) , col="blue")
	points(min.nI[,k],rep(0,nsims) , pch="|", col=col.alpha("black", 0.25))

}

mtext("Minimum Number of Individual Learners", 1 , line=3.5)
title(paste("u=",U,";b=",b,";c=",c,";s=",s,";K=",K, sep=" ") , outer=TRUE , cex.main=2 , line=1)


##What proportion of individuals go extinct
prop.extinct.N <- prop.extinct.nI <- prop.extinct.nS <- med.t.N <- med.t.nS <- med.t.nI <- rep(timesteps,length(w0))

for (k in 1:length(w0) ){
	prop.extinct.N[k] <- (nsims-sum(is.na(mint.N[,k])))/nsims
	prop.extinct.nI[k] <- (nsims-sum(is.na(mint.nI[,k])))/nsims
	prop.extinct.nS[k] <- (nsims-sum(is.na(mint.nS[,k])))/nsims

	med.t.N[k] <- mean(mint.N[,k] , na.rm=TRUE)
	med.t.nI[k] <- mean(mint.nI[,k] , na.rm=TRUE)
	med.t.nS[k] <- mean(mint.nS[,k] , na.rm=TRUE)
}

#plot of prob of extripation for all 
plot(w0 , prop.extinct.N , pch=19 , ylim=c(0,1) , ylab="probability of extripation" , xlab="baseline fitness (w0)")
lines(w0 , prop.extinct.N)
points(w0 , prop.extinct.nI ,  pch=18 , col="blue")
points(w0 , prop.extinct.nS ,  pch=17 , col="orange")
lines(w0 , prop.extinct.nI , col="blue")
lines(w0 , prop.extinct.nS , col="orange")
#add legend
legend("topright", c("all individuals","social learners","individual learners"), col=c("black","orange","blue"), horiz=FALSE, pch=c(19,17,18) , lty=1)


plot(w0 , med.t.N , pch=19 , ylim=c(0,timesteps) , ylab="mean time to extripation | extripation" , xlab="baseline fitness (w0)")
lines(w0 , med.t.N )
points(w0 , med.t.nI ,  pch=18 , col="blue")
points(w0 , med.t.nS ,  pch=17 , col="orange")
lines(w0 , med.t.nI  , col="blue")
lines(w0 , med.t.nS  , col="orange")
#add legend
legend("topright", c("all individuals","social learners","individual learners"), col=c("black","orange","blue"), horiz=FALSE, pch=c(19,17,18) , lty=1)


###What are the distrbutions od time to extripation

par(mfrow = c(length(w0), 1))
par(cex = 0.3)
par(oma = c(4,2,3,1))
par(mar = c(2,3,0,0))

for(k in 1:length(w0)){
	dens(mint.N[,k] , rm.na=TRUE , ylim=c(0,0.08) , xlim=c(0,100))
	dens(mint.nI[,k] , rm.na=TRUE , col="blue" , add=TRUE )
	dens(mint.nS[,k] , rm.na=TRUE , col="orange" , add=TRUE )
	abline(v=median(mint.N[,k] , na.rm=TRUE)  , col="black" )
	abline(v=median(mint.nI[,k] , na.rm=TRUE)  , col="blue" )
	abline(v=median(mint.nS[,k] , na.rm=TRUE)  , col="orange" )
}

mtext("time until extripation", 1 , line=3.5)
title(paste("u=",U,";b=",b,";c=",c,";s=",s,";K=",K, sep=" ") , outer=TRUE , cex.main=2 , line=1)

#####what is the effect of varying K?

#########loop over values of K

#######Rogers infinite states with carryiing capacity########
K <- c(10,50,100,250,500,1000,5000,10000)  #carying capacity
#w0 <- seq(from=.1 , to=1 , by=0.1) #baseline fitness
w0 <- .2 #baseline fitness
b <- 2  #benfit of adaptive behavior
c <- 1  #cost of IL
U <- 0.2 	#rate of enviro change
s <- 1    #prob of IL acquiring adaptive behavior
timesteps <- 100
nsims <- 2000 #number or simulations for each condition

q <- p <- u <- nI <- nS <- N <-  array(0, dim = c(timesteps + 1,nsims,length(K)))

wS <- wI  <- array(0, dim = c(timesteps,nsims,length(K)))

q[1,,] <- 1    #all social learners have adaptive behavior
p[1,,] <- 0.5   #50.50 IL/SL mix
u[1,,] <- 0     #stable env


#rep(0,timesteps )
for (k in 1:length(K)){ #function of whatever parameter one is varying
	nI[1,,k] <- 0.2*K[k]
	nS[1,,k] <- 0.2*K[k]
	N[1,,k] <- nI[1,,k] + nS[1,,k]
###initial states
	for ( j in 1:nsims){
		for (t in 1:timesteps){
			wI[t,j,k] <- w0 + s*b - c #just doing it in loop for vectorization and graphing ease
			wS[t,j,k] <- w0 + b*q[t,j,k] #fitness of social learners
			q[t+1,j,k] <- (1-u[t,j,k])*( (nI[t,j,k]/N[t,j,k])*s + (nS[t,j,k]/N[t,j,k])*q[t,j,k]) + u[t,j,k]*0
			nS[t+1,j,k] <- nS[t,j,k]*wS[t,j,k]*(1 - (N[t,j,k]/K[k])) #rounding seems wrong, bur update numer odSL in next gen
			nI[t+1,j,k] <- nI[t,j,k]*wI[t,j,k]*(1 - (N[t,j,k]/K[k])) #number of IL in next gen
			N[t+1,j,k] <- nS[t+1,j,k] + nI[t+1,j,k]   #num up for N (total #individual)
			p[t+1,j,k] <- nS[t+1,j,k]/N[t+1,j,k]  #freuency of SL in next gen
			u[t+1,j,k] <- rbinom(n=1,prob=U,size=1) #sample environment changing in next gen, if 0 all SL behav maladaptive
		}

	}
}


###what proportion go extinct 
nI[,j,k]
min(nI[,j,1])
min.nI <- min.N <- min.nS <- mint.nI <- mint.N <- mint.nS <- array(NA, dim = c(nsims,length(K)))

for(k in 1:length(K)){
	for (j in 1:nsims){
		min.nI[j,k] <- min(nI[,j,k]) #min number individual learners in each simulation across each condition
		min.nS[j,k] <- min(nS[,j,k]) #min number social learners in each simulation across each condition
		min.N[j,k] <- min(N[,j,k])
		#mint.nI[j,k] <- min(nI[,j,k])
		#mint.nS[j,k] <- min(nS[,j,k])
		mint.N[j,k] <- ifelse( is.finite(min(which( N[,j,k] <1))) , min(which( N[,j,k] <1)) , mint.N[j,k] ) #gives a vector of timesteps where extinction occurs, NAs include persistance
		mint.nS[j,k] <- ifelse( is.finite(min(which( nS[,j,k] <1))) , min(which( nS[,j,k] <1)) , mint.nS[j,k] ) 
		mint.nI[j,k] <- ifelse( is.finite(min(which( nI[,j,k] <1))) , min(which( nI[,j,k] <1)) , mint.nI[j,k] ) 

	}
}




#####plot of minimum population size
par(mfrow = c(length(K), 1))
par(cex = 0.3)
par(oma = c(4,2,3,1))
par(mar = c(2,3,0,0))

for(k in 1:length(K)){
	dens(min.N[,k], xlim=c(0,max(min.N)) , show.HPDI=0.8)
	abline(v=median(min.N[,k]))
	points(min.N[,k],rep(0,nsims) , pch="|", col=col.alpha("black", 0.25))
	title(paste("K=",K[k], sep=" ") , outer=FALSE , cex.main=2 , line=-1.5)

	#dens(min.nS[,k], xlim=c(0,max(N)) , col="orange" , add=TRUE)
	#dens(min.nI[,k], xlim=c(0,max(N))  , col="blue" , add=TRUE)
	#abline(v=median(min.nI[,k]) , col="blue")
}

mtext("Minimum Population Size", 1 , line=3.5)
title(paste("u=",U,";b=",b,";c=",c,";s=",s,";w0=",w0, sep=" ") , outer=TRUE , cex.main=2 , line=1)

####minimum number of social learners
par(mfrow = c(length(K), 1))
par(cex = 0.3)
par(oma = c(4,2,3,1))
par(mar = c(2,3,0,0))

for(k in 1:length(K)){
	dens(min.nS[,k], xlim=c(0,max(min.N)) , col="orange", show.HPDI=0.8 )
	abline(v=median(min.nS[,k]) , col="orange")
	points(min.nS[,k],rep(0,nsims) , pch="|", col=col.alpha("black", 0.25))
	title(paste("K=",K[k], sep=" ") , outer=FALSE , cex.main=2 , line=-1.5)

}

mtext("Minimum Population Size", 1 , line=3.5)
title(paste("u=",U,";b=",b,";c=",c,";s=",s,";w0=",w0, sep=" ") , outer=TRUE , cex.main=2 , line=1)

###min num individual learners
par(mfrow = c(length(K), 1))
par(cex = 0.3)
par(oma = c(4,2,3,1))
par(mar = c(2,3,0,0))

for(k in 1:length(K)){
	dens(min.nI[,k], xlim=c(0,max(min.N)) , col="blue", show.HPDI=0.8 )
	abline(v=median(min.nI[,k]) , col="blue")
	points(min.nI[,k],rep(0,nsims) , pch="|", col=col.alpha("black", 0.25))
	title(paste("K=",K[k], sep=" ") , outer=FALSE , cex.main=2 , line=-1.5)

}

mtext("Minimum Population Size", 1 , line=3.5)
title(paste("u=",U,";b=",b,";c=",c,";s=",s,";w0=",w0, sep=" ") , outer=TRUE , cex.main=2 , line=1)


##What proportion of individuals go extinct
prop.extinct.N <- prop.extinct.nI <- prop.extinct.nS <- med.t.N <- med.t.nS <- med.t.nI <- rep(timesteps,length(w0))

for (k in 1:length(K) ){
	prop.extinct.N[k] <- (nsims-sum(is.na(mint.N[,k])))/nsims
	prop.extinct.nI[k] <- (nsims-sum(is.na(mint.nI[,k])))/nsims
	prop.extinct.nS[k] <- (nsims-sum(is.na(mint.nS[,k])))/nsims

	med.t.N[k] <- mean(mint.N[,k] , na.rm=TRUE)
	med.t.nI[k] <- mean(mint.nI[,k] , na.rm=TRUE)
	med.t.nS[k] <- mean(mint.nS[,k] , na.rm=TRUE)
}

#plot of prob of extripation for all 
plot(log(K) , prop.extinct.N , pch=19 , ylim=c(0,1) , ylab="probability of extripation" , xlab="Log Carrying Capacity (log(K))")
lines(log(K) , prop.extinct.N)
points(log(K) , prop.extinct.nI ,  pch=18 , col="blue")
points(log(K) , prop.extinct.nS ,  pch=17 , col="orange")
lines(log(K) , prop.extinct.nI , col="blue")
lines(log(K) , prop.extinct.nS , col="orange")
#add legend
legend("topright", c("all individuals","social learners","individual learners"), col=c("black","orange","blue"), horiz=FALSE, pch=c(19,17,18) , lty=1)


plot(log(K) , med.t.N , pch=19 , ylim=c(0,timesteps) , ylab="mean time to extripation | extripation" , xlab="Log Carrying Capacity (log(K))")
lines(log(K), med.t.N )
points(log(K) , med.t.nI ,  pch=18 , col="blue")
points(log(K) , med.t.nS ,  pch=17 , col="orange")
lines(log(K) , med.t.nI  , col="blue")
lines(log(K) , med.t.nS  , col="orange")
#add legend
legend("topright", c("all individuals","social learners","individual learners"), col=c("black","orange","blue"), horiz=FALSE, pch=c(19,17,18) , lty=1)


###What are the distrbutions od time to extripation

par(mfrow = c(length(K), 1))
par(cex = 0.3)
par(oma = c(4,2,3,1))
par(mar = c(2,3,0,0))

for(k in 1:length(K)){
	dens(mint.N[,k] , rm.na=TRUE , ylim=c(0,0.08) , xlim=c(0,100))
	dens(mint.nI[,k] , rm.na=TRUE , col="blue" , add=TRUE )
	dens(mint.nS[,k] , rm.na=TRUE , col="orange" , add=TRUE )
	abline(v=median(mint.N[,k] , na.rm=TRUE)  , col="black" )
	abline(v=median(mint.nI[,k] , na.rm=TRUE)  , col="blue" )
	abline(v=median(mint.nS[,k] , na.rm=TRUE)  , col="orange" )
}

mtext("time until extripation", 1 , line=3.5)
title(paste("u=",U,";b=",b,";c=",c,";s=",s,";w0=",w0, sep=" ") , outer=TRUE , cex.main=2 , line=1)

###vary u

#####what is the effect of varying u?

#########loop over values of u

#######Rogers infinite states with carryiing capacity########
K <- 1000  #carying capacity
#w0 <- seq(from=.1 , to=1 , by=0.1) #baseline fitness
w0 <- 1 #baseline fitness
b <- 2  #benfit of adaptive behavior
c <- 1  #cost of IL
U <- seq(from=0 , to=1 , by=0.2) 	#rate of enviro change
s <- 1    #prob of IL acquiring adaptive behavior
timesteps <- 100
nsims <- 2000 #number or simulations for each condition

q <- p <- u <- nI <- nS <- N <-  array(0, dim = c(timesteps + 1,nsims,length(U)))

wS <- wI  <- array(0, dim = c(timesteps,nsims,length(U)))

q[1,,] <- 1    #all social learners have adaptive behavior
p[1,,] <- 0.5   #50.50 IL/SL mix
u[1,,] <- 0     #stable env


#rep(0,timesteps )
for (k in 1:length(U)){ #function of whatever parameter one is varying
	nI[1,,k] <- 0.2*K
	nS[1,,k] <- 0.2*K
	N[1,,k] <- nI[1,,k] + nS[1,,k]
###initial states
	for ( j in 1:nsims){
		for (t in 1:timesteps){
			wI[t,j,k] <- w0 + s*b - c #just doing it in loop for vectorization and graphing ease
			wS[t,j,k] <- w0 + b*q[t,j,k] #fitness of social learners
			q[t+1,j,k] <- (1-u[t,j,k])*( (nI[t,j,k]/N[t,j,k])*s + (nS[t,j,k]/N[t,j,k])*q[t,j,k]) + u[t,j,k]*0
			nS[t+1,j,k] <- nS[t,j,k]*wS[t,j,k]*(1 - (N[t,j,k]/K)) #rounding seems wrong, bur update numer odSL in next gen
			nI[t+1,j,k] <- nI[t,j,k]*wI[t,j,k]*(1 - (N[t,j,k]/K)) #number of IL in next gen
			N[t+1,j,k] <- nS[t+1,j,k] + nI[t+1,j,k]   #num up for N (total #individual)
			p[t+1,j,k] <- nS[t+1,j,k]/N[t+1,j,k]  #freuency of SL in next gen
			u[t+1,j,k] <- rbinom(n=1,prob=U[k],size=1) #sample environment changing in next gen, if 0 all SL behav maladaptive
		}

	}
}


###what proportion go extinct 
nI[,j,k]
min(nI[,j,1])
min.nI <- min.N <- min.nS <- mint.nI <- mint.N <- mint.nS <- array(NA, dim = c(nsims,length(U)))

for(k in 1:length(U)){
	for (j in 1:nsims){
		min.nI[j,k] <- min(nI[,j,k]) #min number individual learners in each simulation across each condition
		min.nS[j,k] <- min(nS[,j,k]) #min number social learners in each simulation across each condition
		min.N[j,k] <- min(N[,j,k])
		#mint.nI[j,k] <- min(nI[,j,k])
		#mint.nS[j,k] <- min(nS[,j,k])
		mint.N[j,k] <- ifelse( is.finite(min(which( N[,j,k] <1))) , min(which( N[,j,k] <1)) , mint.N[j,k] ) #gives a vector of timesteps where extinction occurs, NAs include persistance
		mint.nS[j,k] <- ifelse( is.finite(min(which( nS[,j,k] <1))) , min(which( nS[,j,k] <1)) , mint.nS[j,k] ) 
		mint.nI[j,k] <- ifelse( is.finite(min(which( nI[,j,k] <1))) , min(which( nI[,j,k] <1)) , mint.nI[j,k] ) 

	}
}




#####plot of minimum population size
par(mfrow = c(length(U), 1))
par(cex = 0.3)
par(oma = c(4,2,3,1))
par(mar = c(2,3,0,0))

for(k in 1:length(U)){
	dens(min.N[,k], xlim=c(0,max(min.N)) , show.HPDI=0.8)
	abline(v=median(min.N[,k]))
	points(min.N[,k],rep(0,nsims) , pch="|", col=col.alpha("black", 0.25))
	title(paste("u=",U[k], sep=" ") , outer=FALSE , cex.main=2 , line=-1.5)
}

mtext("Minimum Population Size", 1 , line=3.5)
title(paste("K=",K,";b=",b,";c=",c,";s=",s,";w0=",w0, sep=" ") , outer=TRUE , cex.main=2 , line=1)

####minimum number of social learners
par(mfrow = c(length(U), 1))
par(cex = 0.3)
par(oma = c(4,2,3,1))
par(mar = c(2,3,0,0))

for(k in 1:length(U)){
	dens(min.nS[,k], xlim=c(0,max(min.N)) , col="orange", show.HPDI=0.8 )
	abline(v=median(min.nS[,k]) , col="orange")
	points(min.nS[,k],rep(0,nsims) , pch="|", col=col.alpha("black", 0.25))
	title(paste("u=",U[k], sep=" ") , outer=FALSE , cex.main=2 , line=-1.5)
}

mtext("Minimum Population Size", 1 , line=3.5)
title(paste("K=",K,";b=",b,";c=",c,";s=",s,";w0=",w0, sep=" ") , outer=TRUE , cex.main=2 , line=1)

###min num individual learners
par(mfrow = c(length(U), 1))
par(cex = 0.3)
par(oma = c(4,2,3,1))
par(mar = c(2,3,0,0))

for(k in 1:length(U)){
	dens(min.nI[,k], xlim=c(0,max(min.N)) , col="blue", show.HPDI=0.8 )
	abline(v=median(min.nI[,k]) , col="blue")
	points(min.nI[,k],rep(0,nsims) , pch="|", col=col.alpha("black", 0.25))
	title(paste("u=",U[k], sep=" ") , outer=FALSE , cex.main=2 , line=-1.5)
}

mtext("Minimum Population Size", 1 , line=3.5)
title(paste("K=",K,";b=",b,";c=",c,";s=",s,";w0=",w0, sep=" ") , outer=TRUE , cex.main=2 , line=1)


##What proportion of individuals go extinct
prop.extinct.N <- prop.extinct.nI <- prop.extinct.nS <- med.t.N <- med.t.nS <- med.t.nI <- rep(timesteps,length(U))

for (k in 1:length(U) ){
	prop.extinct.N[k] <- (nsims-sum(is.na(mint.N[,k])))/nsims
	prop.extinct.nI[k] <- (nsims-sum(is.na(mint.nI[,k])))/nsims
	prop.extinct.nS[k] <- (nsims-sum(is.na(mint.nS[,k])))/nsims

	med.t.N[k] <- mean(mint.N[,k] , na.rm=TRUE)
	med.t.nI[k] <- mean(mint.nI[,k] , na.rm=TRUE)
	med.t.nS[k] <- mean(mint.nS[,k] , na.rm=TRUE)
}

#plot of prob of extripation for all 
plot(U , prop.extinct.N , pch=19 , ylim=c(0,1) , ylab="probability of extripation" , xlab="rate of enviro change (u)")
lines(U , prop.extinct.N)
points(U , prop.extinct.nI ,  pch=18 , col="blue")
points(U , prop.extinct.nS ,  pch=17 , col="orange")
lines(U , prop.extinct.nI , col="blue")
lines(U , prop.extinct.nS , col="orange")
#add legend
legend("topright", c("all individuals","social learners","individual learners"), col=c("black","orange","blue"), horiz=FALSE, pch=c(19,17,18) , lty=1)


plot(U , med.t.N , pch=19 , ylim=c(0,timesteps) , ylab="mean time to extripation | extripation" , xlab="rate of enviro change (u)")
lines(U, med.t.N )
points(U , med.t.nI ,  pch=18 , col="blue")
points(U , med.t.nS ,  pch=17 , col="orange")
lines(U , med.t.nI  , col="blue")
lines(U , med.t.nS  , col="orange")
#add legend
legend("topright", c("all individuals","social learners","individual learners"), col=c("black","orange","blue"), horiz=FALSE, pch=c(19,17,18) , lty=1)


###What are the distrbutions od time to extripation

par(mfrow = c(length(K), 1))
par(cex = 0.3)
par(oma = c(4,2,3,1))
par(mar = c(2,3,0,0))

for(k in 1:length(K)){
	dens(mint.N[,k] , rm.na=TRUE , ylim=c(0,0.08) , xlim=c(0,100))
	dens(mint.nI[,k] , rm.na=TRUE , col="blue" , add=TRUE )
	dens(mint.nS[,k] , rm.na=TRUE , col="orange" , add=TRUE )
	abline(v=median(mint.N[,k] , na.rm=TRUE)  , col="black" )
	abline(v=median(mint.nI[,k] , na.rm=TRUE)  , col="blue" )
	abline(v=median(mint.nS[,k] , na.rm=TRUE)  , col="orange" )
}

mtext("time until extripation", 1 , line=3.5)
t
