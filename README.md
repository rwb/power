# normal simulation with no selection effect

```R
set.seed(487)

nt <- 500
nc <- 500

trt <- c(rep(0,nc),rep(1,nt))

itt <- vector()
itt.se <- vector()

for(i in 1:1e4){

  y <- rep(NA,nt+nc)
  y[trt==1] <- rnorm(n=nt,mean=10,sd=1)
  y[trt==0] <- rnorm(n=nc,mean=11,sd=1)

  itt[i] <- mean(y[trt==1])-mean(y[trt==0])
  v1 <- (sd(y[trt==1])/sqrt(nt))^2
  v0 <- (sd(y[trt==0])/sqrt(nc))^2
  itt.se[i] <- sqrt(v1+v0)
  }
 
mean(itt)
sd(itt)
mean(itt.se)
```

# normal simulation with selection effect

```R
set.seed(487)

nt <- 500
nc <- 500

trt <- c(rep(0,nc),rep(1,nt))
elg <- c(rep(0,50),rep(1,450),rep(0,50),rep(1,450))

itt <- vector()
itt.se <- vector()

for(i in 1:1e4){

  y <- rep(NA,nt+nc)

  y[trt==1 & elg==1] <- rnorm(n=nt-50,mean=10,sd=1)
  y[trt==0 & elg==1] <- rnorm(n=nc-50,mean=11,sd=1)
  y[trt==1 & elg==0] <- rnorm(n=nt-450,mean=8,sd=1)
  y[trt==0 & elg==0] <- rnorm(n=nc-450,mean=8,sd=1)

  itt[i] <- mean(y[trt==1])-mean(y[trt==0])
  v1 <- (sd(y[trt==1])/sqrt(nt))^2
  v0 <- (sd(y[trt==0])/sqrt(nc))^2
  itt.se[i] <- sqrt(v1+v0)
  }
 
mean(itt)
sd(itt)
mean(itt.se)
```

# binomial simulation with selection effect

```R
set.seed(481)

nt <- 500
nc <- nt
p.elig <- 0.9
n.elig <- nt*p.elig
n.elig
n.inelig <- nt-n.elig
n.inelig

trt <- c(rep(0,nt),rep(1,nc))
elg <- c(rep(0,n.inelig),
         rep(1,n.elig),
         rep(0,n.inelig),
         rep(1,n.elig))

itt <- vector()
itt.se <- vector()
late <- vector()
late.se <- vector()

for(i in 1:1e5){
  y <- rep(NA,nt+nc)
  y[trt==1 & elg==1] <- rbinom(n=n.elig,size=1,p=0.3)
  y[trt==1 & elg==0] <- rbinom(n=n.inelig,size=1,p=0.5)
  y[trt==0 & elg==1] <- rbinom(n=n.elig,size=1,p=0.4)
  y[trt==0 & elg==0] <- rbinom(n=n.inelig,size=1,p=0.5)
  p.hat1 <- mean(y[trt==1])
  p.hat0 <- mean(y[trt==0])
  itt[i] <- p.hat1-p.hat0
  var1 <- p.hat1*(1-p.hat1)/nt
  var0 <- p.hat0*(1-p.hat0)/nc
  itt.se[i] <- sqrt(var1+var0)
  ivd <- mean(elg[trt==1])
  late[i] <- itt[i]/ivd
  late.se[i] <- (itt.se[i]/itt[i])*late[i]
  }

mean(itt)
sd(itt)
mean(itt.se)

mean(late)
sd(late)
mean(late.se,na.rm=T)
```
