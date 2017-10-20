library(tidyverse)
library(tikzDevice)
library(ggthemes)

setwd("C:/Users/VictorF/Dropbox/Tesis Maestría/Paper/Codes")

out1<-read.csv("C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Matlab/Out1.txt",
               sep=",", header=FALSE)

out2<-read.csv("C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Matlab/Out2.txt",
               sep=",", header=FALSE)

out3<-read.csv("C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Matlab/Out3.txt",
               sep=",", header=FALSE)

out4<-read.csv("C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Matlab/Out4.txt",
               sep=",", header=FALSE)

out5<-read.csv("C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Matlab/Out5.txt",
               sep=",", header=FALSE)

#Out1: Datos
out1.df1<-data.frame(F_1=out1[,1], C_1=out1[,3])
out1.df2<-data.frame(F_4=out1[,2], C_4=out1[,4])

#Out 2: Emparejamiento aleatorio en 1992 (2012)
out2.df1<-data.frame(F_1=out2[,1], C_1=out2[,3])
out2.df2<-data.frame(F_4=out2[,2], C_4=out2[,4])

#Out 3: Emparejamiento aleatorio en 1992 (2012)+PLF 2012 (1992)
out3.df1<-data.frame(F_1=out3[,1], C_1=out3[,3])
out3.df2<-data.frame(F_4=out3[,2], C_4=out3[,4])

#Out 4: Tablas estandarizadas de 1992 (2012) con marginales de 2012 (1992)
out4.df1<-data.frame(F_1=out4[,1], C_1=out4[,3])
out4.df2<-data.frame(F_4=out4[,2], C_4=out4[,4])

#Out 5: Tablas estandarizadas de 1992 (2012) con PLF de 2012 (1992)
out5.df1<-data.frame(F_1=out5[,1], C_1=out5[,3])
out5.df2<-data.frame(F_4=out5[,2], C_4=out5[,4])


tikz("Lorenz_EA1.tex", width = 6, height = 4)
plot1 <- ggplot() + 
  geom_path(data = out1.df1, aes(x = F_1, y = C_1, color="Data"), size=1)+
  geom_path(data = out2.df1, aes(x = F_1, y = C_1, color="Random mating"), size=1)+
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = 2) + 
  theme_hc() + 
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1))+
  xlab("Cumulative percent of households $(p)$")+
  ylab("Cumulative percent of income $(l_{p})$")+
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold")) 
cap1 <- "Gini(Data)=0.398"
cap2 <- "Gini(Random mating)=0.335"
plot1 <- plot1 + 
  annotate("text", x=0.2, y=c(0.75, 0.85), label=c(cap2, cap1), size=4)  
plot1
dev.off()

tikz("Lorenz_EA2.tex", width = 6, height = 4)
plot2 <- ggplot() + 
  geom_path(data = out1.df2, aes(x = F_4, y = C_4, color="Data"), size=1)+
  geom_path(data = out2.df2, aes(x = F_4, y = C_4, color="Random mating"), size=1)+
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = 2) + 
  theme_hc() + 
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1))+
  xlab("Cumulative percent of households $(p)$")+
  ylab("Cumulative percent of income $(l_{p})$")+
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold")) 
cap1 <- "Gini(Data)=0.382"
cap2 <- "Gini(Random mating)=0.349"
plot2 <- plot2 + 
  annotate("text", x=0.2, y=c(0.75, 0.85), label=c(cap2,cap1), size=4)  
plot2
dev.off()

tikz("LorenzPLF_1.tex", width = 6, height = 4)
plot3 <- ggplot() + 
  geom_path(data = out1.df1, aes(x = F_1, y = C_1, color="Data"), size=1)+
  geom_path(data = out3.df1, aes(x = F_1, y = C_1, color="RM+FLP"), size=1)+
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = 2) + 
  theme_hc() + 
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1))+
  xlab("Cumulative percent of households $(p)$")+
  ylab("Cumulative percent of income $(l_{p})$")+
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold")) 
cap1 <- "Gini(Data)=0.398"
cap2 <- "Gini(RM+FLP)=0.334"
plot3 <- plot3 + 
  annotate("text", x=0.2, y=c(0.75, 0.85), label=c(cap2,cap1), size=4)
plot3
dev.off()

tikz("LorenzPLF_2.tex", width = 6, height = 4)
plot4 <- ggplot() + 
  geom_path(data = out1.df2, aes(x = F_4, y = C_4, color="Data"), size=1)+
  geom_path(data = out3.df2, aes(x = F_4, y = C_4, color="RM+FLP"), size=1)+
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = 2) + 
  theme_hc() + 
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1))+
  xlab("Cumulative percent of households $(p)$")+
  ylab("Cumulative percent of income $(l_{p})$")+
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold")) 
cap1 <- "Gini(Data)=0.382"
cap2 <- "Gini(RM+FLP)=0.341"
plot4 <- plot4 + 
  annotate("text", x=0.2, y=c(0.75, 0.85), label=c(cap2,cap1), size=4) 
plot4
dev.off()

tikz("LorenzMarg_1.tex", width = 6, height = 4)
plot5 <- ggplot() + 
  geom_path(data = out1.df1, aes(x = F_1, y = C_1, color="Data"), size=1)+
  geom_path(data = out4.df1, aes(x = F_1, y = C_1, color="Std+Marg"), size=1)+
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = 2) + 
  theme_hc() + 
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1))+
  xlab("Cumulative percent of households $(p)$")+
  ylab("Cumulative percent of income $(l_{p})$")+
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold")) 
cap1 <- "Gini(Data)=0.398"
cap2 <- "Gini(Std+Marg)=0.346"
plot5 <- plot5 + 
  annotate("text", x=0.2, y=c(0.75, 0.85), label=c(cap2,cap1), size=4)  
plot5
dev.off()

tikz("LorenzMarg_2.tex", width = 6, height = 4)
plot6 <- ggplot() + 
  geom_path(data = out1.df2, aes(x = F_4, y = C_4, color="Data"), size=1)+
  geom_path(data = out4.df2, aes(x = F_4, y = C_4, color="Std+Marg"), size=1)+
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = 2) + 
  theme_hc() + 
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1))+
  xlab("Cumulative percent of households $(p)$")+
  ylab("Cumulative percent of income $(l_{p})$")+
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold")) 
cap1 <- "Gini(Data)=0.382"
cap2 <- "Gini(Std+Marg)=0.351"
plot6 <- plot6 + 
  annotate("text", x=0.2, y=c(0.75, 0.85), label=c(cap2,cap1), size=4)
plot6
dev.off()

tikz("LorenzMargPLF_1.tex", width = 6, height = 4)
plot7 <- ggplot() + 
  geom_path(data = out1.df1, aes(x = F_1, y = C_1, color="Data"), size=1)+
  geom_path(data = out5.df1, aes(x = F_1, y = C_1, color="Marg+FLP"), size=1)+
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = 2) + 
  theme_hc() + 
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1))+
  xlab("Cumulative percent of households $(p)$")+
  ylab("Cumulative percent of income $(l_{p})$")+
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold")) 
cap1 <- "Gini(Data)=0.398"
cap2 <- "Gini(Marg+FLP)=0.345"
plot7 <- plot7 + 
  annotate("text", x=0.2, y=c(0.75, 0.85), label=c(cap2,cap1), size=4)
plot7
dev.off()

tikz("LorenzMargPLF_2.tex", width = 6, height = 4)
plot8 <- ggplot() + 
  geom_path(data = out1.df2, aes(x = F_4, y = C_4, color="Data"), size=1)+
  geom_path(data = out5.df2, aes(x = F_4, y = C_4, color="Marg+FLP"), size=1)+
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = 2) + 
  theme_hc() + 
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1))+
  xlab("Cumulative percent of households $(p)$")+
  ylab("Cumulative percent of income $(l_{p})$")+
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold")) 
cap1 <- "Gini(Data)=0.382"
cap2 <- "Gini(Marg+FLP)=0.342"
plot8 <- plot8 + 
  annotate("text", x=0.2, y=c(0.75, 0.85), label=c(cap2,cap1), size=4) 
plot8
dev.off()

lfp_wife <- read.csv("C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/lfp_wife.txt",
                   sep="\t", header=FALSE)
ish_wife <- read.csv("C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/income_share_wife.txt",
                   sep="\t", header=FALSE)

lfp_wife <- data.frame(quintil=1:5, lfp_wife)
ish_wife <- data.frame(quintil=1:5, ish_wife)

names(lfp_wife) <- c("Quintil", "1992-1996", "2008-2012")
names(ish_wife) <- c("Quintil", "1992-1996", "2008-2012")

tikz("lfp.tex", width = 6, height = 4)
plot9 <- lfp_wife %>%
  gather(period, value, c("1992-1996", "2008-2012")) %>%
  ggplot(aes(x=Quintil, y=value, group=period))+
  geom_line(aes(color=period), size=1) + 
  geom_point(aes(color=period), size=2) + theme_hc() + 
  scale_linetype_manual(name="", values=c(1,2))+
  xlab("Quintile")+ylab("Female labor participation rate") + 
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold")) 
plot9
dev.off()

tikz("ish.tex", width = 6, height = 4)
plot10 <- ish_wife %>%
  gather(period, value, c("1992-1996", "2008-2012")) %>%
  ggplot(aes(x = Quintil, y = value, group = period))+
  geom_line(aes(color=period), size=1) + 
  geom_point(aes(color=period), size=2) + theme_hc() +
  scale_linetype_manual(name="", values=c(1,2))+
  xlab("Quintile")+ylab("Female share of household income") + 
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text(size = 9, face = "bold")) 
plot10
dev.off()

rm(list=ls())