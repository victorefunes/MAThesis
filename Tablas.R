library(foreign)
library(doBy)
library(plyr)
library(dplyr)
library(pcaPP)
library(Kendall)
library(ggplot2)
library(MASS)

load("F:/cedlas/Base monografia/Base Educ/Base hogares/BaseT.Rdata")

bTc<-filter(bT, hombre_casado=="Casado")

func <- function(nivel)
{
  return(data.frame(kendall = cor.fk(nivel$nivel_m,nivel$nivel_f)))
}


nivel1<-na.omit(data.frame(homb_n=b1c$nivel_m, mujer_n=b1c$nivel_f, anio=b1c$año))
nivel2<-na.omit(data.frame(homb_n=b2c$nivel_m, mujer_n=b2c$nivel_f, anio=b2c$año))
nivel3<-na.omit(data.frame(homb_n=b3c$nivel_m, mujer_n=b3c$nivel_f, anio=b3c$año))
nivel4<-na.omit(data.frame(homb_n=b4c$nivel_m, mujer_n=b4c$nivel_f, anio=b4c$año))

ktau1<-cor.fk(nivel1$homb_n,nivel1$mujer_n)
ktau2<-cor.fk(nivel2$homb_n,nivel2$mujer_n)
ktau3<-cor.fk(nivel3$homb_n,nivel3$mujer_n)
ktau4<-cor.fk(nivel4$homb_n,nivel4$mujer_n)
ktau<-as.numeric(cbind(ktau1, ktau2, ktau3, ktau4))
write.csv(ktau, file="C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/kendall_tau.txt",
col.names = F, row.names = F)

cor1<-ddply(nivel1, .(anio), function(nivel1){return(data.frame(kendall = cor.fk(nivel1$homb_n,nivel1$mujer_n)))})
cor2<-ddply(nivel2, .(anio), function(nivel2){return(data.frame(kendall = cor.fk(nivel2$homb_n,nivel2$mujer_n)))})
cor3<-ddply(nivel3, .(anio), function(nivel3){return(data.frame(kendall = cor.fk(nivel3$homb_n,nivel3$mujer_n)))})
cor4<-ddply(nivel4, .(anio), function(nivel4){return(data.frame(kendall = cor.fk(nivel4$homb_n,nivel4$mujer_n)))})
cors<-rbind(cor1,cor2,cor3,cor4)
cor.ts<-ts(data=cors$kendall, start=1992, frequency=1)
plot(cor.ts, ylim=c(0.37,0.45), type="b", col="darkgreen")

b1c$nivel_m<- ordered(b1c$nivel_m, labels = c("pinc", "pcomp", "secinc", "secomp", "supinc", "supcomp"))
b1c$nivel_f<- ordered(b1c$nivel_f, labels = c("pinc", "pcomp", "secinc", "secomp", "supinc", "supcomp"))
con1<-xtabs(~nivel_m+nivel_f, data=b1c)
con1<-prop.table(con1)
con1<-format(con1, digits=15)
write.matrix(con1, file="C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/con_1.txt", sep="\t", blocksize=TRUE)

b2c$nivel_m<- ordered(b2c$nivel_m, labels = c("pinc", "pcomp", "secinc", "secomp", "supinc", "supcomp"))
b2c$nivel_f<- ordered(b2c$nivel_f, labels = c("pinc", "pcomp", "secinc", "secomp", "supinc", "supcomp"))
con2<-xtabs(~nivel_m+nivel_f, data=b2c)
con2<-prop.table(con2)
con2<-format(con2, digits=15)
write.matrix(con2, file="C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/con_2.txt", sep="\t", blocksize=TRUE)

b3c$nivel_m<-ordered(b3c$nivel_m, labels = c("pinc", "pcomp", "secinc", "secomp", "supinc", "supcomp"))
b3c$nivel_f<-ordered(b3c$nivel_f, labels = c("pinc", "pcomp", "secinc", "secomp", "supinc", "supcomp"))
con3<-xtabs(~nivel_m+nivel_f, data=b3c)
con3<-prop.table(con3)
con3<-format(con3, digits=15)
write.matrix(con3, file="C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/con_3.txt", sep="\t", blocksize=TRUE)

b4c$nivel_m<-ordered(b4c$nivel_m, labels = c("pinc", "pcomp", "secinc", "secomp", "supinc", "supcomp"))
b4c$nivel_f<-ordered(b4c$nivel_f, labels = c("pinc", "pcomp", "secinc", "secomp", "supinc", "supcomp"))
con4<-xtabs(~nivel_m+nivel_f, data=b4c)
con4<-prop.table(con4)
con4<-format(con4, digits=15)
write.matrix(con4, file="C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/con_4.txt", sep="\t", blocksize=TRUE)

reg1<-lm(as.numeric(nivel_f)~as.numeric(nivel_m)+as.numeric(nivel_m)*factor(año)+
           factor(año), data=b1c)
summary(reg1)
reg2<-lm(as.numeric(nivel_f)~as.numeric(nivel_m)+as.numeric(nivel_m)*factor(año)+
           factor(año), data=b2c)
summary(reg2)
reg3<-lm(as.numeric(nivel_f)~as.numeric(nivel_m)+as.numeric(nivel_m)*factor(año)+
           factor(año), data=b3c)
summary(reg3)
reg4<-lm(as.numeric(nivel_f)~as.numeric(nivel_m)+as.numeric(nivel_m)*factor(año)+
           factor(año), data=b4c)
summary(reg4)

s1c<-subset(b1c, select=c(nivel_m, nivel_f, año))
s2c<-subset(b2c, select=c(nivel_m, nivel_f, año))
s3c<-subset(b3c, select=c(nivel_m, nivel_f, año))
s4c<-subset(b4c, select=c(nivel_m, nivel_f, año))

base<-rbind(s1c,s2c,s3c,s4c)
per<-rep(NA, length(base$año))
per<-replace(per, base$año<=1996,1)
per<-replace(per, base$año>1996 & base$año<=2002,2)
per<-replace(per, base$año>2003 & base$año<=2007,3)
per<-replace(per, base$año>2007,4)
base<-cbind(base, per)

reg5<-lm(as.numeric(nivel_f)~as.numeric(nivel_m)+as.numeric(nivel_m)*factor(per)+
           factor(per), data=base, x=TRUE)
summary(reg5)
reg5.ci<-confint(reg5, level=0.9)

coefs<-as.numeric(summary(reg5)$coefficients[6:8,1])
write.csv(coefs, file="C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/gamma_coeff.txt",
          col.names = F, row.names = F)

coef.df<-data.frame(año=seq(1993,2012,1), coefs=summary(reg5)$coefficients[23:42,1],
ci5=reg5.ci[23:42,1], ci95=reg5.ci[23:42,2])