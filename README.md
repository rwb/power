# binomial simulation

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
