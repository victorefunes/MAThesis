library(ggplot2)
library(reshape2)

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


theme_set(theme_bw())
gr11<-ggplot() + 
  geom_line(data = out1.df1, aes(x = F_1, y = C_1, color="Datos"), size=1)+
  geom_line(data = out2.df1, aes(x = F_1, y = C_1, color="Emparejamiento aleatorio"), size=1)+
  geom_abline(aes(intercept=0, slope=1), size=1)+
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1))+
  xlab(expression(paste("Porcentaje acum. de hogares,",p)))+
  ylab(expression(paste("Porcentaje acum. de ingreso de los hogares,",l[p])))+
  scale_color_manual(name="", values=c("grey70", "grey30"))+
  theme(legend.position="bottom")
  cap1<-"Gini(Datos)=0.398"
  cap2<-"Gini(Emp. Aleatorio)=0.335"
gr11<-gr11+annotate("text", x=0.2, y=c(0.75, 0.85), label=c(cap2,cap1), size=6)  
ggsave("C:/Users/VictorF/Dropbox/Tesis Maestría/Graficos/Lorenz_EA1.eps", plot=gr11)

gr12<-ggplot() + 
  geom_line(data = out1.df2, aes(x = F_4, y = C_4, color="Datos"), size=1)+
  geom_line(data = out2.df2, aes(x = F_4, y = C_4, color="Emparejamiento aleatorio"), size=1)+
  geom_abline(aes(intercept=0, slope=1), size=1)+
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1))+
  xlab(expression(paste("Porcentaje acum. de hogares,",p)))+
  ylab(expression(paste("Porcentaje acum. de ingreso de los hogares,",l[p])))+
  scale_color_manual(name="", values=c("grey70", "grey30"))+
  theme(legend.position="bottom")
cap1<-"Gini(Datos)=0.382"
cap2<-"Gini(Emp. Aleatorio)=0.349"
gr12<-gr12+annotate("text", x=0.2, y=c(0.75, 0.85), label=c(cap2,cap1), size=6)  
ggsave("C:/Users/VictorF/Dropbox/Tesis Maestría/Graficos/Lorenz_EA2.eps", plot=gr12)

gr21<-ggplot() + 
  geom_line(data = out1.df1, aes(x = F_1, y = C_1, color="Datos"), size=1)+
  geom_line(data = out3.df1, aes(x = F_1, y = C_1, color="EA+PLF"), size=1)+
  geom_abline(aes(intercept=0, slope=1), size=1)+
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1))+
  xlab(expression(paste("Porcentaje acum. de hogares,",p)))+
  ylab(expression(paste("Porcentaje acum. de ingreso de los hogares,",l[p])))+
  scale_color_manual(name="", values=c("grey70", "grey30"))+
  theme(legend.position="bottom")
cap1<-"Gini(Datos)=0.398"
cap2<-"Gini(EA+PLF)=0.334"
gr21<-gr21+annotate("text", x=0.2, y=c(0.75, 0.85), label=c(cap2,cap1), size=6)  
ggsave("C:/Users/VictorF/Dropbox/Tesis Maestría/Graficos/LorenzPLF_1.eps", plot=gr21)

gr22<-ggplot() + 
  geom_line(data = out1.df2, aes(x = F_4, y = C_4, color="Datos"), size=1)+
  geom_line(data = out3.df2, aes(x = F_4, y = C_4, color="EA+PLF"), size=1)+
  geom_abline(aes(intercept=0, slope=1), size=1)+
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1))+
  xlab(expression(paste("Porcentaje acum. de hogares,",p)))+
  ylab(expression(paste("Porcentaje acum. de ingreso de los hogares,",l[p])))+
  scale_color_manual(name="", values=c("grey70", "grey30"))+
  theme(legend.position="bottom")
cap1<-"Gini(Datos)=0.382"
cap2<-"Gini(EA+PLF)=0.341"
gr22<-gr22+annotate("text", x=0.2, y=c(0.75, 0.85), label=c(cap2,cap1), size=6)  
ggsave("C:/Users/VictorF/Dropbox/Tesis Maestría/Graficos/LorenzPLF_2.eps", plot=gr22)


gr31<-ggplot() + 
  geom_line(data = out1.df1, aes(x = F_1, y = C_1, color="Datos"), size=1)+
  geom_line(data = out4.df1, aes(x = F_1, y = C_1, color="Est+Marg"), size=1)+
  geom_abline(aes(intercept=0, slope=1), size=1)+
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1))+
  xlab(expression(paste("Porcentaje acum. de hogares,",p)))+
  ylab(expression(paste("Porcentaje acum. de ingreso de los hogares,",l[p])))+
  scale_color_manual(name="", values=c("grey70", "grey30"))+
  theme(legend.position="bottom")
cap1<-"Gini(Datos)=0.398"
cap2<-"Gini(Est+Marg)=0.346"
gr31<-gr31+annotate("text", x=0.2, y=c(0.75, 0.85), label=c(cap2,cap1), size=6)  
ggsave("C:/Users/VictorF/Dropbox/Tesis Maestría/Graficos/LorenzMarg_1.eps", plot=gr31)

gr32<-ggplot() + 
  geom_line(data = out1.df2, aes(x = F_4, y = C_4, color="Datos"), size=1)+
  geom_line(data = out4.df2, aes(x = F_4, y = C_4, color="Est+Marg"), size=1)+
  geom_abline(aes(intercept=0, slope=1), size=1)+
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1))+
  xlab(expression(paste("Porcentaje acum. de hogares,",p)))+
  ylab(expression(paste("Porcentaje acum. de ingreso de los hogares,",l[p])))+
  scale_color_manual(name="", values=c("grey70", "grey30"))+
  theme(legend.position="bottom")
cap1<-"Gini(Datos)=0.382"
cap2<-"Gini(Est+Marg)=0.351"
gr32<-gr32+annotate("text", x=0.2, y=c(0.75, 0.85), label=c(cap2,cap1), size=6)  
ggsave("C:/Users/VictorF/Dropbox/Tesis Maestría/Graficos/LorenzMarg_2.eps", plot=gr32)


gr41<-ggplot() + 
  geom_line(data = out1.df1, aes(x = F_1, y = C_1, color="Datos"), size=1)+
  geom_line(data = out5.df1, aes(x = F_1, y = C_1, color="Marg+PLF"), size=1)+
  geom_abline(aes(intercept=0, slope=1), size=1)+
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1))+
  xlab(expression(paste("Porcentaje acum. de hogares,",p)))+
  ylab(expression(paste("Porcentaje acum. de ingreso de los hogares,",l[p])))+
  scale_color_manual(name="", values=c("grey70", "grey30"))+
  theme(legend.position="bottom")
cap1<-"Gini(Datos)=0.398"
cap2<-"Gini(Marg+PLF)=0.345"
gr41<-gr41+annotate("text", x=0.2, y=c(0.75, 0.85), label=c(cap2,cap1), size=6)  
ggsave("C:/Users/VictorF/Dropbox/Tesis Maestría/Graficos/LorenzMargPLF_1.eps", plot=gr41)

gr42<-ggplot() + 
  geom_line(data = out1.df2, aes(x = F_4, y = C_4, color="Datos"), size=1)+
  geom_line(data = out5.df2, aes(x = F_4, y = C_4, color="Marg+PLF"), size=1)+
  geom_abline(aes(intercept=0, slope=1), size=1)+
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1))+
  xlab(expression(paste("Porcentaje acum. de hogares,",p)))+
  ylab(expression(paste("Porcentaje acum. de ingreso de los hogares,",l[p])))+
  scale_color_manual(name="", values=c("grey70", "grey30"))+
  theme(legend.position="bottom")
cap1<-"Gini(Datos)=0.382"
cap2<-"Gini(Marg+PLF)=0.342"
gr42<-gr42+annotate("text", x=0.2, y=c(0.75, 0.85), label=c(cap2,cap1), size=6)  
ggsave("C:/Users/VictorF/Dropbox/Tesis Maestría/Graficos/LorenzMargPLF_2.eps", plot=gr42)



lfp_wife<-read.csv("C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/lfp_wife.txt",
                   sep="\t", header=FALSE)
ish_wife<-read.csv("C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/income_share_wife.txt",
                   sep="\t", header=FALSE)

lfp_wife<-data.frame(quintil=1:5, lfp_wife)
ish_wife<-data.frame(quintil=1:5, ish_wife)

names(lfp_wife)<-c("Quintil", "1992-1996", "2008-2012")
names(ish_wife)<-c("Quintil", "1992-1996", "2008-2012")

lfp1<-melt(lfp_wife, id="Quintil")
ish1<-melt(ish_wife, id="Quintil")

gr6<-ggplot(lfp1, aes(x=Quintil, y=value, group=variable))+
  geom_line(aes(linetype=variable), size=1)+
  scale_linetype_manual(name="", values=c(1,2))+
  xlab("Quintil")+ylab("Participación laboral femenina")
ggsave("C:/Users/VictorF/Dropbox/Tesis Maestría/Graficos/lfp.eps", plot=gr6)

gr7<-ggplot(ish1, aes(x=Quintil, y=value, group=variable))+
  geom_line(aes(linetype=variable), size=1)+
  scale_linetype_manual(name="", values=c(1,2))+
  xlab("Quintil")+ylab("Participación de la mujer en el ingreso del hogar")
ggsave("C:/Users/VictorF/Dropbox/Tesis Maestría/Graficos/ish.eps", plot=gr7)

rm(list=ls())
