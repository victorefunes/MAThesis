library(foreign)
library(tidyverse)
library(pcaPP)
library(Kendall)
library(MASS)
library(ineq)
library(psych)
library(vcd)
library(IC2)
library(boot)
library(laeken)
library(animation)
library(broom)
library(tikzDevice)
library(ggthemes)

setwd("C:/Users/VictorF/Dropbox/Tesis Maestría/Paper/Codes/Introduction")

load("F:/cedlas/Base monografia/Base Educ/Base hogares/BaseT.Rdata")

bTc <- bT %>% filter(hombre_casado == "Casado")

#Cálculo de la tau de kendall entre los niveles educativos de los cónyuges por año
nivel <- na.omit(data.frame(homb_n=bTc$nivel_m, mujer_n=bTc$nivel_f, anio=bTc$año))

ktau <- cor.fk(nivel$homb_n, nivel$mujer_n)

#write.csv(ktau, file="C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/kendall_tau.txt",col.names = F, row.names = F)

tikz("Kendall_tau.tex", width = 6, height = 4)
plot1 <- nivel %>% 
  group_by(anio) %>% 
  summarise(kendall=cor.fk(homb_n,mujer_n),size=n()) %>%
  mutate(se=1/sqrt(size-3),
         fisher=fisherz(kendall),
         zlow=fisherz(kendall)+qnorm(0.95)*se,
         zhigh=fisherz(kendall)-qnorm(0.95)*se) %>%
  ggplot(aes(anio, fisher)) + 
  geom_line(aes(color="Kendall's Tau"), size=1)+
  geom_point(aes(color="Kendall's Tau"), size=2)+
  geom_ribbon(aes(ymin=zhigh,ymax=zlow,
                  color="Confidence Interval (95\\%)"), alpha=.2)+
  xlab("Año")+ylab("$\\tau_{t}$")+
  scale_y_continuous(limits = c(0.35, 0.5))+
  geom_vline(xintercept=2003, linetype="dotted") + 
  theme_hc() + xlab("Year") +
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold"))+
  theme(legend.position="bottom") +
  scale_x_continuous(breaks = seq(1992, 2012, 3))
plot1
dev.off()

#### Gini index and bootstrapped SE's ####
bT <- bT %>% mutate(ipcfr=bT$ipcf/bT$ipc)
bT$ipcfr[which(is.na(bT$ipcfr))]<-0
bT <- bT %>% mutate(itfr=bT$itf/bT$ipc)
bT$itfr[which(is.na(bT$itfr))]<-0

coefs <- laeken::gini(bT$ipcfc, years=bT$año, na.rm=TRUE)
coefsm <- laeken::gini(bT$ii_m, years=bT$año, na.rm=TRUE)
coefsf <- laeken::gini(bT$ii_f, years=bT$año, na.rm=TRUE)

bs <- bootVar(bT$ipcfc, years=bT$año, indicator=coefs, R=500, 
             bootType="naive", alpha=0.05, seed=123, na.rm=TRUE)
bsm <- bootVar(bT$ii_m, years=bT$año, indicator=coefsm, R=500, 
            bootType="naive", alpha=0.05, seed=123, na.rm=TRUE)
bsf <- bootVar(bT$ii_f, years=bT$año, indicator=coefsf, R=500, 
            bootType="naive", alpha=0.05, seed=123, na.rm=TRUE)


gini.df <- data.frame(año=bs$year, ic.inferior=bs$ci[,1], ic.superior=bs$ci[,2],
                    coef=bs$value)
ginim.df <- data.frame(año=bsm$year, ic.inferior=bsm$ci[,1], ic.superior=bsm$ci[,2],
                     coef=bsm$value)
ginif.df <- data.frame(año=bsf$year, ic.inferior=bsf$ci[,1], ic.superior=bsf$ci[,2],
                     coef=bsf$value)

tikz("gini_t.tex", width = 6, height = 4)
plot2 <- gini.df %>% 
  ggplot(aes(año, coef)) +
  geom_line(aes(color = "Gini Coefficient")) +
  geom_point(aes(color = "Gini Coefficient")) +
  geom_ribbon(aes(ymin=ic.inferior, ymax=ic.superior, 
                  color = "95\\% Confidence Interval"), alpha=.2)+
  xlab("Year") + ylab("Gini Coefficient") + theme_hc() + 
  geom_vline(xintercept=2003, linetype="dotted") +
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold")) + 
  ylim(35, 55) + 
  scale_x_continuous(breaks = seq(1992, 2012, 3))
plot2
dev.off()

tikz("gini_m.tex", width = 6, height = 4)
plot3 <- ginim.df %>% 
  ggplot(aes(año, coef))+
  geom_line(aes(color="Gini Coefficient"))+
  geom_point(aes(color = "Gini Coefficient")) +
  geom_ribbon(aes(ymin=ic.inferior, ymax=ic.superior, 
                  color="95\\% Confidence Interval"), alpha=.2)+
  xlab("Year")+ylab("Gini Coefficient (Males)")+
  geom_vline(xintercept=2003, linetype="dotted") + theme_hc() +
  scale_x_continuous(breaks = seq(1992, 2012, 3)) +
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold")) + 
  ylim(35,  55) 
plot3
dev.off()

tikz("gini_f.tex", width = 6, height = 4)
plot4 <- ginif.df %>% 
  ggplot(aes(año, coef))+
  geom_line(aes(color="Gini Coefficient"))+
  geom_point(aes(color = "Gini Coefficient")) +
  geom_ribbon(aes(ymin=ic.inferior, ymax=ic.superior, 
                  color="95\\% Confidence Interval"), alpha=.2)+
  xlab("Year")+ylab("Gini Coefficient (Females)")+
  geom_vline(xintercept=2003, linetype="dotted") + theme_hc() +
  scale_x_continuous(breaks = seq(1992, 2012, 3)) + ylim(55, 75) +
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold")) 
plot4
dev.off()

tikz("gini_2.tex", width = 6, height = 4)
plot5 <- gini.df %>% 
  ggplot(aes(año, coef)) + 
  geom_line(color="grey50", size=1) +
  geom_point(color="grey50", size=4) +
  xlab("Year")+ylab("Gini Coefficient")+ theme_hc() + 
  ylim(35, 55)+
  geom_vline(xintercept=2003, linetype="dotted") + 
  scale_x_continuous(breaks = seq(1992, 2012, 3)) + 
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold")) 
plot5
dev.off()

reg1 <- lm(as.numeric(nivel_f)~as.numeric(nivel_m)+
           as.numeric(nivel_m)*factor(año)+factor(año), data=bT, x=TRUE)
reg1 %>% summary()
reg1.ci <- confint(reg1, level=0.95)

df.ci <- data.frame(anio = seq(1993,2012,1), 
                  ci.05 = reg1.ci[23:42,1], 
                  coef = tidy(reg1)[23:42, 2], 
                  ci.95 = reg1.ci[23:42,2])

tikz("coefficients.tex", width = 6, height = 4)
plot6 <- df.ci %>%
  ggplot(aes(anio, coef))+
  geom_line(aes(color="Coefficients"))+
  geom_point(aes(color="Coefficients"))+
  geom_ribbon(aes(ymin=ci.05,ymax=ci.95, 
                  color="Confidence Interval"), alpha=.2)+ 
  theme_hc() + xlab("Year")+ylab("$\\gamma_{t}$")+
  geom_hline(yintercept=0)+geom_vline(xintercept=2003, linetype="dotted")+
  scale_y_continuous(breaks = seq(-0.07, 0.07, 0.02)) +
  scale_x_continuous(breaks = seq(1992, 2012, 3)) + 
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold")) 
plot6
dev.off()

bT$nivel_m <- ordered(bT$nivel_m, labels = c("pinc", "pcomp", "secinc", "secomp", "supinc", "supcomp"))
bT$nivel_f <- ordered(bT$nivel_f, labels = c("pinc", "pcomp", "secinc", "secomp", "supinc", "supcomp"))

con<-list()
ran<-list()
delta<-rep(0,21)
years<-c(1992:2012)
for(i in 1:21){
  con[[i]]<-xtabs(~nivel_m+nivel_f, data=subset(bT, año==years[i]))
  con[[i]]<-prop.table(con[[i]])
  ran[[i]]<-rowSums(con[[i]])%*%t(rowSums(con[[i]]))
  delta[i]<-tr(con[[i]])/tr(ran[[i]])
}

delta.df <- data.frame(year = seq(1992,2012,1), delta=delta)

tikz("delta.tex", width = 6, height = 4)
plot7 <- delta.df %>% 
  ggplot(aes(year, delta)) + 
  geom_line(color="grey50", size=1)+
  geom_point(color="grey50", size=4) + xlab("Year")+ 
  ylab("$\\delta_{t}$")+ theme_hc() + 
  scale_y_continuous(limits = c(2, 2.4)) + 
  scale_x_continuous(breaks = seq(1992, 2012, 3)) +
  geom_vline(xintercept=2003, linetype="dotted")
plot7
dev.off()
  
w_inc <- read.csv("C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/income_share_wife10.txt", sep="\t", header=FALSE)
inc_share <- data.frame(decil=seq(1,10,1), Per1=w_inc[,1], Per4=w_inc[,2])

bT <- bT %>%  
  mutate(ila_mr=ila_m/ipc, 
         ila_fr=ila_f/ipc, 
         ii_mr=ii_m/ipc, 
         ii_fr=ii_f/ipc, 
         inla_mr=inla_m/ipc, 
         inla_fr=inla_f/ipc, 
         ipcfcr=ipcfc/ipc)

dens.df <- data.frame(bT[, c("id","año","ii_mr","ii_fr", "ipcfcr")])
anio <- rep(NA, length(dens.df$año))
anio <- replace(anio, dens.df$año==1992, "1992")
anio <- replace(anio, dens.df$año==1993, "1993")
anio <- replace(anio, dens.df$año==1994, "1994")
anio <- replace(anio, dens.df$año==1995, "1995")
anio <- replace(anio, dens.df$año==1996, "1996")
anio <- replace(anio, dens.df$año==1997, "1997")
anio <- replace(anio, dens.df$año==1998, "1998")
anio <- replace(anio, dens.df$año==1999, "1999")
anio <- replace(anio, dens.df$año==2000, "2000")
anio <- replace(anio, dens.df$año==2001, "2001")
anio <- replace(anio, dens.df$año==2002, "2002")
anio <- replace(anio, dens.df$año==2003, "2003")
anio <- replace(anio, dens.df$año==2004, "2004")
anio <- replace(anio, dens.df$año==2005, "2005")
anio <- replace(anio, dens.df$año==2006, "2006")
anio <- replace(anio, dens.df$año==2007, "2007")
anio <- replace(anio, dens.df$año==2008, "2008")
anio <- replace(anio, dens.df$año==2009, "2009")
anio <- replace(anio, dens.df$año==2010, "2010")
anio <- replace(anio, dens.df$año==2011, "2011")
anio <- replace(anio, dens.df$año==2012, "2012")
dens.df <- cbind(dens.df, anio)
dens.df <- dens.df[,-2]

dens.df <- dens.df %>% 
  mutate(lii_mr=log(ii_mr),
         lii_fr=log(ii_fr), 
         lipcfcr=log(ipcfcr))

tikz("densities_t.tex", width = 6, height = 4)
plot8 <- dens.df %>% 
  filter(anio %in% c("1992", "2002", "2012"), is.finite(lipcfcr)==TRUE) %>% 
  ggplot(aes(x=lipcfcr))+ 
  geom_density(alpha=.3, kernel="epanechnikov", bw="nrd", 
               aes(fill=factor(anio)))+ theme_hc() +
  scale_x_continuous(limits=c(4,12))+
  scale_y_continuous(limits=c(0,0.6))+
  labs(x="Log Per Capita Household Income", y="Density")+
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold")) 
plot8
dev.off()

tikz("densities_m.tex", width = 6, height = 4)
plot9 <- dens.df %>% 
  filter(anio %in% c("1992", "2002", "2012"), is.na(lii_mr)==FALSE) %>% 
  ggplot(aes(x=lii_mr))+ 
  geom_density(alpha=.3, kernel="epanechnikov", bw="nrd", 
               aes(fill=factor(anio)))+ theme_hc() +
  scale_x_continuous(limits=c(4,12))+
  labs(x="Log Per Capita Male Income", y="Density")+
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold")) 
plot9
dev.off()

tikz("densities_f.tex", width = 6, height = 4)
plot10 <- dens.df %>% filter(anio %in% c("1992", "2002", "2012"),
                             is.na(lii_fr)==FALSE) %>%
  ggplot(aes(x=lii_fr))+ 
  geom_density(alpha=.3, kernel="epanechnikov", bw="nrd", 
               aes(fill=factor(anio)))+ theme_hc() + 
  scale_x_continuous(limits=c(4,12))+
  labs(x="Log Per Capita Female Income", y="Density")+
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold")) 
plot10
dev.off()

w_mean <- bT %>% 
  group_by(año) %>% 
  filter(ii_mr>0 & ii_fr>0) %>%
  summarise(lii_mr=mean(log(ii_mr), na.rm=TRUE),
            lii_fr=mean(log(ii_fr), na.rm=TRUE))

## Animaciones
setwd("C:/Users/VictorF/Dropbox/Tesis Maestría/Presentacion/Familiar")
plots<-list()
dens.list<-list()
years<-c("1992","1993","1994","1995","1996","1997","1998","1999","2000",
         "2001","2002","2003","2004","2005", "2006","2007","2008","2009",
         "2010","2011","2012")
saveLatex(for (i in 1:21){
  dens.list[[i]]<-filter(dens.df, anio %in% years[i])
  plots[[i]]<-ggplot(dens.list[[i]], aes(x=lipcfcr))+ 
    geom_density(color="black", kernel="epanechnikov", bw="nrd", fill="grey")+
    scale_x_continuous(limits=c(4,12))+
    scale_y_continuous(limits=c(0,0.6))+
    labs(x="Logaritmo del ingreso per cápita familiar", y="Densidad")+
    annotate("text", x=11, y=0.5,
             label=paste("Año", years[i], sep=" "))+
    annotate("text", x=11, y=0.47,
             label=paste("Gini:", round(gini.df$coef[i], digits=3), sep=" "))
  print(plots[[i]])
}, interval = 1, 
ani.width = 800, 
ani.height = 600,
img.name="itf_img",
latex.filename = "itf.tex",
outdir = getwd())

dens4.df<-dens.df[,c(1,5,6,7)]
dens4.df<-melt(dens4.df, id.vars=c("id", "anio"))

dists4<-ggplot(dens4.df, aes(x=value))+ 
  geom_density(alpha=.3, kernel="epanechnikov", bw="nrd", 
               aes(fill=factor(variable)))+
  scale_x_continuous(limits=c(4,12))+
  labs(x="Logaritmo del ingreso total", y="Densidad")+
  ggtitle(expression(atop(paste("Años:", "1992-2012", sep=" "), 
                          atop(italic("Años"), ""))))
  scale_fill_manual(name="", values=c("black", "grey70"),
                    labels=c("ii_m", "ii_f"))
dists4
             

setwd("C:/Users/VictorF/Dropbox/Tesis Maestría/Presentacion/Total")
plots2<-list()
dens.list2<-list()
dens4.df<-subset(dens4.df, is.finite(value)==TRUE)
years<-c("1992","1993","1994","1995","1996","1997","1998","1999","2000",
         "2001","2002","2003","2004","2005", "2006","2007","2008","2009",
         "2010","2011","2012")
saveLatex(for (i in 1:21){
  dens.list2[[i]]<-filter(dens4.df, anio %in% years[i])
  plots2[[i]]<-ggplot(dens.list2[[i]], aes(x=value))+ 
    geom_density(alpha=.3, kernel="epanechnikov", bw="nrd", 
                 aes(fill=factor(variable)))+
    scale_x_continuous(limits=c(4,12))+
    scale_y_continuous(limits=c(0,0.75))+
    geom_vline(xintercept=as.numeric(w_mean[i,2]), colour="black")+
    geom_vline(xintercept=as.numeric(w_mean[i,3]), colour="grey70")+
    labs(x="Logaritmo del ingreso total", y="Densidad")+
    scale_fill_manual(name="", values=c("black", "grey70"), 
                      labels=c("Varones", "Mujeres"))+
    ggtitle(paste("Año:", years[i],"\n Gini(Varones)=", round(coefsm$value[i], digits=3),
                  "\n Gini(Mujeres)=", round(coefsf$value[i], digits=3), sep=" "))+
    theme(title=element_text(size=16), legend.text=element_text(size=12))
  print(plots2[[i]])
}, interval = .8, 
ani.width = 800, 
ani.height = 600,
img.name="ii_img",
latex.filename = "iit.tex",
outdir = getwd())

fill<- rbind(c("dark cyan", "gray", "gray", "gray", "gray", "gray"),
             c("gray", "dark cyan", "gray", "gray", "gray", "gray"),
             c("gray", "gray", "dark cyan", "gray", "gray", "gray"),
             c("gray", "gray", "gray", "dark cyan", "gray", "gray"),
             c("gray", "gray", "gray", "gray", "dark cyan", "gray"),
             c("gray", "gray", "gray", "gray", "gray", "dark cyan"))

setwd("C:/Users/VictorF/Dropbox/Tesis Maestría/Presentacion/MosaicPlots")
plots3<-list()
dens.list3<-list()
table.list<-list()
years<-c("1992","1993","1994","1995","1996","1997","1998","1999","2000",
         "2001","2002","2003","2004","2005", "2006","2007","2008","2009",
         "2010","2011","2012")
saveLatex(for (i in 1:21){
  dens.list3[[i]]<-bTc %>% filter(año %in% years[i])
  dens.list3[[i]]$nivel_m<-ordered(dens.list3[[i]]$nivel_m, labels = c("P-","P","S-","S","U-","U"))
  dens.list3[[i]]$nivel_f<-ordered(dens.list3[[i]]$nivel_f, labels = c("P-","P","S-","S","U-","U"))
  table.list[[i]]<-structable(~nivel_m+nivel_f, data=dens.list3[[i]])
  table.list[[i]]<-round((table.list[[i]]/sum(colSums(table.list[[i]])))*100, digits=1)
  plots3[[i]]<-mosaic(~nivel_m+nivel_f, data=dens.list3[[i]],
                      gp = gpar(fill = fill, col = 0, fontsize = 20),
                      labeling_args=list(set_varnames=
                                           c(nivel_m="Nivel educativo-hombres",
                                             nivel_f="Nivel educativo-mujeres"), 
                                         fontsize = 16),
                      main=paste("Año:", years[i], sep=" "))
  print(plots3[[i]])
}, interval = .8, 
ani.width = 800, 
ani.height = 600,
img.name="mosaic_img",
latex.filename = "mosaic.tex",
outdir = getwd())

setwd("C:/Users/VictorF/Dropbox/Tesis Maestría/Presentacion/MosaicPlots")
dens.list4<-list()
table.list<-list()
props.list<-list()
years<-c("1992","1993","1994","1995","1996","1997","1998","1999","2000",
         "2001","2002","2003","2004","2005", "2006","2007","2008","2009",
         "2010","2011","2012")
values.list<-list()
tabs.list<-list()
rowvar<- c("P-","P","S-","S","U-","U")
columnvar<- c("P-","P","S-","S","U-","U")
names<-c("Nivel educativo-hombres", "Nivel educativo-mujeres")
dims <- c(6,6)
for (i in 1:21){
  dens.list4[[i]]<-bTc %>% filter(año %in% years[i])
  table.list[[i]]<-structable(~nivel_m+nivel_f, data=dens.list4[[i]])
  table.list[[i]]<-round((table.list[[i]]/sum(colSums(table.list[[i]])))*100, digits=1)
  values.list[[i]]<-c(table.list[[i]])
  tabs.list[[i]]<-structure(c(values.list[[i]]), .Dim = as.integer(dims), 
                   .Dimnames = structure(list(rowvar,columnvar),
                                          .Names = c(names)) , class = "table") 
  png(file = paste("mosaicp_img", i, ".png", sep=""), width = 800, height = 600)
  mosaic(table.list[[i]],pop=FALSE, gp = gpar(fill = fill, col = 0, fontsize = 20), 
         main=paste("Año", years[i], sep=" "))
  labeling_cells(text=tabs.list[[i]] , clip_cells=FALSE)(tabs.list[[i]])
  dev.off()
}

source("C:/Users/VictorF/Dropbox/Tesis Maestría/Codigos/Introducción/MosaicPlots.R")


rm(list=ls())
