set.seed(123)
library(ineq)
source("C:/Users/VictorF/Dropbox/Tesis Maestría/Codigos/gcuan.R")

wm <- runif(10, 0, 10000)
decm0 <- decil(wm, rep(1, 10))
wf <- runif(10, 0, 10000)
decf0 <- decil(wf, rep(1, 10))

dfm <- as.data.frame(cbind(wm, decm0))
dff <- as.data.frame(cbind(wf, decf0))
wt0 <- 0.5*(wm+wf)
df0 <- round(cbind(wm, wf, wt0), digits=2)
Gini(wt0)

dfm1 <- dfm[order(dfm$decm0, decreasing=TRUE),]
dff1 <- dff[order(dff$decf0, decreasing=TRUE),]
wt1 <- 0.5*(dfm1$wm+dff1$wf)
Gini(wt1)

df1 <- round(cbind(dfm1$wm, dff1$wf, wt1), digits=2)

dfm2 <- dfm[order(dfm$decm0, decreasing=TRUE),]
dff2 <- dff[order(dff$decf0, decreasing=FALSE),]
wt2 <- 0.5*(dfm2$wm+dff2$wf)
Gini(wt2)

df2 <- round(cbind(dfm2$wm, dff2$wf, wt2), digits=2)

#Mismo ejercicio pero con 100 réplicas para 1000 individuos
#min=0, max=20000

wm <- runif(1000, 0, 20000)
wf <- runif(1000, 0, 20000)
wt0 <- 0.5*(wm+wf)
decwt0 <- gcuan(wt0, ncuan=10)
df0 <- data.frame(wt0, decwt0)
Gini(wt0)

G <- data.frame(matrix(0, ncol=3, nrow=1000))
names(G)<-c("Random", "Positive", "Negative")
H <- data.frame(matrix(0, ncol=10, nrow=1000))
M <- data.frame(matrix(0, ncol=10, nrow=1000))
A <- data.frame(matrix(0, ncol=10, nrow=1000))
P <- data.frame(matrix(0, ncol=10, nrow=1000))
N <- data.frame(matrix(0, ncol=10, nrow=1000))

for(i in 1:1000){
  wm <- runif(1000, 0, 20000)
  decwm <- gcuan(wm, ncuan=10)
  wf <- runif(1000, 0, 20000)
  decwf <- gcuan(wf, ncuan=10)
  wt0<-0.5*(wm+wf)
  decwt0<-gcuan(wt0, ncuan=10)
  A[i,]<-as.numeric(by(wt0, decwt0, mean))
  G[i,1]<-Gini(wt0)
  dfm<-as.data.frame(cbind(wm, decwm))
  dff<-as.data.frame(cbind(wf, decwf))
  H[i,]<-as.numeric(by(dfm$wm, dfm$decwm, mean))
  M[i,]<-as.numeric(by(dff$wf, dff$decwf, mean))
  dfm1<-dfm[order(dfm$decwm, decreasing=TRUE),]
  dff1<-dff[order(dff$decwf, decreasing=TRUE),]
  wt1<-0.5*(dfm1$wm+dff1$wf)
  decwt1<-gcuan(wt1, ncuan=10)
  G[i,2]<-Gini(wt1)
  P[i,]<-as.numeric(by(wt1, decwt1, mean))
  dfm2<-dfm[order(dfm$decwm, decreasing=TRUE),]
  dff2<-dff[order(dff$decwf, decreasing=FALSE),]
  wt2<-0.5*(dfm2$wm+dff2$wf)
  decwt2<-gcuan(wt2, ncuan=10)
  df2<-data.frame(wt2, decwt2)
  G[i,3]<-Gini(wt2) 
  N[i,]<-as.numeric(by(wt2, decwt2, mean))
}

ginis_m <- apply(G, 2, mean)
ginis_sd <- apply(G, 2, sd)
mean_h <- apply(H, 2, mean)
mean_m <- apply(M, 2, mean)
mean_a <- apply(A, 2, mean)
mean_p <- apply(P, 2, mean)
mean_n <- apply(N, 2, mean)

decil <- seq(1,10,1)
cuadro <- data.frame(Decile=decil, Men=mean_h, Women=mean_m, 
                   Random = mean_a, Positive = mean_p, 
                   Negative = mean_n)