library(tidyverse)
library(survey)
library(ineq)
library(lattice)
library(tikzDevice)

setwd("C:/Users/VictorF/Dropbox/Tesis Maestría/Paper/Codes/Introduction")

#Leer los 4 archivos csv
b1 <- read.csv("F:/cedlas/Base monografia/eph199296.csv")
b2 <- read.csv("F:/cedlas/Base monografia/eph199702.csv")
b3 <- read.csv("F:/cedlas/Base monografia/eph200307.csv")
b4 <- read.csv("F:/cedlas/Base monografia/eph200812.csv")

#indicadores de períodos
Per <- rep(1, dim(b1)[1])
b1 <- cbind(b1, Per)
Per <- rep(2, dim(b2)[1])
b2 <- cbind(b2, Per)
Per <- rep(3, dim(b3)[1])
b3 <- cbind(b3, Per)
Per <- rep(4, dim(b4)[1])
b4 <- cbind(b4, Per)

#Unir las cuatro bases descartando las variables innecesarias
Base <- rbind(b1[,-c(1,2)], b2[,-c(1,2)], b3[, -c(1,2,20)], b4[, -c(1,2,20)])
rm(b1,b2,b3,b4, Per)

Base <- Base %>% filter(edad_m>0 & edad_f>0)

#Ingresos laborales constantes
Base <- Base %>%
  mutate(ila_mr = ila_m/ipc,
         ila_fr = ila_f/ipc)

#Ingresos totales y per cápita familiares
Base <- Base %>%
  mutate(itf = ila_mr+ila_fr,
         ipcf = itf/2)

#Subconjunto de mujeres con ingresos laborales positivos
Base1 <- Base %>% filter(ila_fr>0)

#Salario real promedio por año 
ila <- Base1 %>%
  group_by(año) %>%
  summarise(ilamb = mean(ila_mr),
            ilafb = mean(ila_fr))

tikz("Income_mf.tex", width = 10, height = 5)
plot1 <- ila %>%
  rename(Year = año, Men = ilamb, Women = ilafb) %>%
  gather(Gender, Income, Men:Women) %>%
  ggplot(aes(Year, Income, color = Gender)) + 
  geom_line(size = 1) + geom_point(size = 2) + theme_bw() + 
  geom_vline(xintercept = 2003, colour="black", linetype = "longdash") + 
  ylab("Income (2012 Argentine Pesos)") +
  theme(legend.position="bottom", legend.direction="horizontal") + 
  scale_color_grey(start = 0.2, end = 0.8) + 
  scale_x_continuous(breaks = seq(1992, 2012, 3))
plot1
dev.off()

# Gráfico de coeficientes de correlación entre ingresos laborales de 
# cónyuges
tikz("Correlation_mf.tex", width = 10, height = 5)
plot2 <- Base %>%
  group_by(año) %>%
  summarise(correl = cor(ila_mr, ila_fr)) %>%
  rename(Year = año) %>%
  ggplot(aes(Year, correl)) + 
  geom_line(color="grey30") + geom_point(color="grey30") +
  geom_vline(xintercept = 2003, colour="black", linetype = "longdash") + 
  ylim(0.1, 0.3) + ylab("Correlation coefficient") + theme_bw() + 
  scale_x_continuous(breaks = seq(1992, 2012, 3))
plot2
dev.off()
  
#Gráfico del Coeficiente de Gini del itf por año
tikz("Gini_itf.tex", width = 10, height = 5)
plot3 <- Base %>%
  group_by(año) %>%
  summarise(Gini = Gini(itf)) %>%
  rename(Year = año) %>%
  ggplot(aes(Year, Gini)) + theme_bw() +
  geom_line(color="grey30") + geom_point(color="grey30") +
  geom_vline(xintercept = 2003, colour="black", linetype = "longdash")+
  ylim(0.35, 0.55) + 
  scale_x_continuous(breaks = seq(1992, 2012, 3))
plot3
dev.off()


#Gráfico de tasas de participación por género y año
tikz("part_rate.tex", width = 10, height = 5)
plot4 <- Base %>%
  mutate(Part_m = I(ila_m>0),
         Part_f = I(ila_f>0)) %>%
  group_by(año) %>%
  summarise(TasaP_m=(sum(Part_m)/length(Part_m))*100, 
            TasaP_f=(sum(Part_f)/length(Part_f))*100) %>%
  rename(Year = año, Men = TasaP_m, Women = TasaP_f) %>%
  gather(Gender, PRate, Men:Women) %>%
  ggplot(aes(Year, PRate, color = Gender)) +
  geom_line(size = 1) + geom_point(size = 2) + theme_bw() +
  ylim(25, 100)+theme(legend.position="bottom", legend.direction="horizontal") +
  ylab("Participation Rate") + 
  geom_vline(xintercept = 2003, colour="black", linetype = "longdash") + 
  scale_x_continuous(breaks = seq(1992, 2012, 3)) + 
  scale_colour_grey()
plot4
dev.off()

source("C:/Users/VictorF/Dropbox/Tesis Maestría/Codigos/cdf2.R")

Basep1 <- Base %>% filter(Per == 1)
t1 <- xtabs(~Basep1$decilo_m+Basep1$decilo_f)
t1<-round(prop.table(t1), digits=3) 
plot.cdf2(seq(1,10,1), seq(1,10,1), t1, "Decil Marido", "Decil Mujer")

Basep4<-subset(Base, Per==4)
t4<-xtabs(~Basep4$decilo_m+Basep4$decilo_f)
t4<-round(prop.table(t4), digits=3) 
plot.cdf2(seq(1,10,1), seq(1,10,1), t4, "Decil Marido", "Decil Mujer", col="lightblue")



