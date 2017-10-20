library(foreign)
library(doBy)
library(plyr)
library(dplyr)
library(pcaPP)
library(Kendall)
library(ggplot2)
library(MASS)
library(reshape2)

load("F:/cedlas/Base monografia/Base Educ/Base hogares/BaseT.Rdata")

# Genero Ingreso per cápita corregido real y elimino datos faltantes
bT<-mutate(bT,ipcfr=ipcfc/ipc )
bT$ipcfr[which(is.na(bT$ipcfr))]<-0

# Genero Ingreso total familiar corregido y elimino datos faltantes
bT<-mutate(bT, itfr=itf/ipc)
bT$itfr[which(is.na(bT$itfr))]<-0

# Genero Ingresos laborales reales para ambos cónyuges y elimino datos faltantes
bT<-mutate(bT, ila_mr=ila_m/ipc)
bT$ila_mr[which(is.na(bT$ila_mr))]<-0

bT<-mutate(bT, ila_fr=ila_f/ipc)
bT$ila_fr[which(is.na(bT$ila_fr))]<-0

# Me quedo con las familias con Ingreso per cápita positivo
bT<-subset(bT, bT$ipcfr>0)

# Genero variable de estado civil
bT<-mutate(bT, estciv=rep(0, length(itf)))
bT$estciv<-replace(bT$estciv, bT$hombre_soltero=="Soltero", 1)
bT$estciv<-replace(bT$estciv, bT$mujer_soltera=="Soltero", 2)
bT$estciv<-replace(bT$estciv, bT$mujer_casada=="Casado", 3)
bT$estciv<-replace(bT$estciv, bT$hombre_casado=="Casado", 3)
bT$estciv<-replace(bT$estciv, bT$hombre_divorciado=="Divorciado", 4)
bT$estciv<-replace(bT$estciv, bT$mujer_divorciada=="Divorciado", 5)
bT$estciv<-factor(bT$estciv, labels=c("0", "solt_m", "solt_f", 
                                      "cas","div_m","div_f"))

#Elimino observaciones que no entran en ninguna categoría
bT<-filter(bT, estciv!=0)

#Creo variable factor para la participación laboral
bT$part_m<-factor(bT$part_m, labels=c("mnpart", "mpart"))
bT$part_f<-factor(bT$part_f, labels=c("fnpart", "fpart"))

#Elimino observaciones faltantes par ambos cónyuges
bT<-filter(bT, !(is.na(part_m)==TRUE & is.na(part_f)==TRUE))

#Variable número de hijos
bT<-mutate(bT, kd=rep(NA, length(id)))
bT$kd<-replace(bT$kd, bT$nhijos==0,1)
bT$kd<-replace(bT$kd, bT$nhijos==1,2)
bT$kd<-replace(bT$kd, bT$nhijos==2,3)
bT$kd<-replace(bT$kd, bT$nhijos>2, 4)
bT$kd<-factor(bT$kd, labels=c("kd0", "kd1", "kd2", "kdm2"))

bT<-subset(bT, select=c(X, ila_m, ila_f, ila_mr, ila_fr, itf, ipcfc, nivel_m,
                        nivel_f,part_m, part_f, ipcfr,itfr, estciv, kd, año))
names(bT)<-c("num", "ila_m", "ila_f", "ila_mr", "ila_fr", "itf", "ipcfc", 
             "nivel_m", "nivel_f","part_m", "part_f", "ipcfr","itfr",
             "estciv", "kd", "Año")

bT<-bT[!(is.na(bT$nivel_m)==TRUE & is.na(bT$nivel_f)==TRUE),]
bT<-bT[!(is.na(bT$kd)==TRUE),]

#Genero grupos, primero elimino sobrantes
bT<-droplevels(bT)

# Solteros y divorciados
bT_sd<-filter(bT, estciv!="cas")
# casados
bT_cs<-filter(bT, estciv=="cas")

# Genero variable nivel educativo para solteros y divorciados
# (Para ellos hogar=individuo)
bT_sd<-mutate(bT_sd, nivel=ifelse(is.na(nivel_m)==TRUE, 
                                  nivel_f, nivel_m))

# Genero variable participación mercado laboral
bT_sd<-mutate(bT_sd, part=rep(0, length(part_m)))
bT_sd$part<-replace(bT_sd$part, bT_sd$part_m=="mpart" | 
                      bT_sd$part_f=="fpart", 1)
bT_sd$part<-replace(bT_sd$part, bT_sd$part_m=="mnpart" | 
                      bT_sd$part_f=="fnpart", 0)

# Genero variable ingreso laboral
bT_sd<-mutate(bT_sd, ila=ifelse(is.na(ila_m)==TRUE,ila_f, ila_m))

bT_sd$nivel<-ordered(bT_sd$nivel, labels = c("pinc", "pcomp", "secinc", 
                                             "secomp", "supinc", "supcomp"))
bT_sd$part<-ordered(bT_sd$part, labels = c("part", "npart"))

bT_cs$nivel_m<-ordered(bT_cs$nivel_m, labels = c("pinc", "pcomp", "secinc", 
                                             "secomp", "supinc", "supcomp"))
bT_cs$nivel_f<-ordered(bT_cs$nivel_f, labels = c("pinc", "pcomp", "secinc", 
                                                 "secomp", "supinc", "supcomp"))

#Elimino variables innecesarias
bT_sd<-bT_sd[,!(names(bT_sd) %in% 
                  c("nivel_m","nivel_f","part_m","part_f", "ila_m", "ila_f"))]


#Elimino niveles no utilizados de variables factor
bT_cs<-droplevels(bT_cs)
bT_sd<-droplevels(bT_sd)

#Genero grupos como interacciones
bT_cs<-group_by(bT_cs, nivel_m, nivel_f, part_m, part_f, kd)
bT_sd<-group_by(bT_sd, nivel, part, kd)

#Grupos para parejas casadas
bT_cs<-mutate(bT_cs, grupos_cs=interaction(estciv,nivel_m,nivel_f,part_m,
                                            part_f,kd,sep="_", lex.order=TRUE))

# Hay parejas para las que faltan datos de nivel educativo/ participación 
# de alguno de los cónyuges, las cuales se eliminan
bT_cs<-filter(bT_cs, grupos_cs != "<NA>")

#Grupos para personas solteras/divorciadas
bT_sd<-mutate(bT_sd, grupos_sd=interaction(estciv,nivel,part,kd, 
                                            sep="_", lex.order=TRUE))

# Calculo ITF e IPCF promedio por grupos
mean_cs<-bT_cs %>% group_by(grupos_cs) %>% 
  summarize(mean_itf=mean(itfr),mean_ipcfr=mean(ipcfr))

mean_sd<-bT_sd %>% group_by(grupos_sd) %>% 
  summarize(mean_itf=mean(itfr),mean_ipcfr=mean(ipcfr))

#Cálculo de cuantiles (Usamos mi versión del gcuan.do y la "built-in" de dplyr)
source("C:/Users/VictorF/Dropbox/Tesis Maestría/Codigos/gcuan.R")

#Dividimos por período y calculamos cuantiles
Per1_cs<-filter(bT_cs, Año %in% c(1992,1993,1994,1995,1996))
#Per1_cs<-mutate(Per1_cs, cuan1_cs=gcuan(ipcfr, ncuan=5))
Per1_cs<-mutate(Per1_cs, cuan1d_cs=ntile(ipcfr,5))

Per1_sd<-filter(bT_sd, Año %in% c(1992,1993,1994,1995,1996))
#Per1_sd<-mutate(Per1_sd, cuan1_sd=gcuan(ipcfr, ncuan=5))
Per1_sd<-mutate(Per1_sd, cuan1d_sd=ntile(ipcfr,5))

Per4_cs<-filter(bT_cs, Año %in% c(2008,2009,2010,2011,2012))
#Per4_cs<-mutate(Per4_cs, cuan4_cs=gcuan(ipcfr, ncuan=5))
Per4_cs<-mutate(Per4_cs, cuan4d_cs=ntile(ipcfr,5))

Per4_sd<-filter(bT_sd, Año %in% c(2008,2009,2010,2011,2012))
#Per4_sd<-mutate(Per4_sd, cuan4_sd=gcuan(ipcfr, ncuan=5))
Per4_sd<-mutate(Per4_sd, cuan4d_sd=ntile(ipcfr,5))

#Tablas de distribución de número de personas y de participación en el ingreso
#en los períodos 1 y 4
tab1_cs<- Per1_cs %>% group_by(grupos_cs, cuan1d_cs) %>%
  summarise(mean_ipcfr=mean(ipcfr, na.rm=TRUE))

tab1_sd<- Per1_sd %>% group_by(grupos_sd, cuan1d_sd) %>%
  summarise(mean_ipcfr=mean(ipcfr, na.rm=TRUE))

tab4_cs<- Per4_cs %>% group_by(grupos_cs, cuan4d_cs) %>%
  summarise(mean_ipcfr=mean(ipcfr, na.rm=TRUE))

tab4_sd<- Per4_sd %>% group_by(grupos_sd, cuan4d_sd) %>%
  summarise(mean_ipcfr=mean(ipcfr, na.rm=TRUE))

# Dar formato de matriz
tab1_cs<-dcast(tab1_cs, grupos_cs~cuan1d_cs)
tab1_sd<-dcast(tab1_sd, grupos_sd~cuan1d_sd)
tab4_cs<-dcast(tab4_cs, grupos_cs~cuan4d_cs)
tab4_sd<-dcast(tab4_sd, grupos_sd~cuan4d_sd)

# Nombrar las filas
rownames(tab1_cs)<-tab1_cs[,1]
rownames(tab1_sd)<-tab1_sd[,1]
rownames(tab4_cs)<-tab4_cs[,1]
rownames(tab4_sd)<-tab4_sd[,1]

# Uno ambas tablas y reemplazo datos faltantes por ceros 
# Primero renombro las columnas de ambas
names(tab1_cs)<-c("Grupo", "Q1", "Q2", "Q3", "Q4", "Q5")
names(tab1_sd)<-c("Grupo", "Q1", "Q2", "Q3", "Q4", "Q5")
names(tab4_cs)<-c("Grupo", "Q1", "Q2", "Q3", "Q4", "Q5")
names(tab4_sd)<-c("Grupo", "Q1", "Q2", "Q3", "Q4", "Q5")

tab1<-rbind(tab1_cs,tab1_sd)
tab4<-rbind(tab4_cs,tab4_sd)

tab1[is.na(tab1)==TRUE]<-0
tab4[is.na(tab4)==TRUE]<-0

rm(tab1_cs,tab1_sd,tab4_cs,tab4_sd)

# Calculo ingreso promedio para cada período para calcular participación
mean_ipcfr1<-mean(Per1_cs$ipcfr, na.rm=TRUE)
mean_ipcfr4<-mean(Per4_cs$ipcfr, na.rm=TRUE)

tab1<-tab1[,-1]/mean_ipcfr1
tab4<-tab4[,-1]/mean_ipcfr4

# Cálculo de las fracciones de hogares en cada quintil
ftab1_cs<- Per1_cs %>% group_by(grupos_cs, cuan1d_cs) %>%
  summarise(N=n())

ftab1_sd<- Per1_sd %>% group_by(grupos_sd, cuan1d_sd) %>%
  summarise(N=n())

ftab4_cs<- Per4_cs %>% group_by(grupos_cs, cuan4d_cs) %>%
  summarise(N=n())

ftab4_sd<- Per4_sd %>% group_by(grupos_sd, cuan4d_sd) %>%
  summarise(N=n())

# Doy formato de tabla
ftab1_cs<-dcast(ftab1_cs, grupos_cs~cuan1d_cs)
ftab1_sd<-dcast(ftab1_sd, grupos_sd~cuan1d_sd)
ftab4_cs<-dcast(ftab4_cs, grupos_cs~cuan4d_cs)
ftab4_sd<-dcast(ftab4_sd, grupos_sd~cuan4d_sd)

# Nombrar las filas
rownames(ftab1_cs)<-ftab1_cs[,1]
rownames(ftab1_sd)<-ftab1_sd[,1]
rownames(ftab4_cs)<-ftab4_cs[,1]
rownames(ftab4_sd)<-ftab4_sd[,1]

# Uno ambas tablas y reemplazo datos faltantes por ceros 
# Primero renombro las columnas de ambas
names(ftab1_cs)<-c("Grupo", "Q1", "Q2", "Q3", "Q4", "Q5")
names(ftab1_sd)<-c("Grupo", "Q1", "Q2", "Q3", "Q4", "Q5")
names(ftab4_cs)<-c("Grupo", "Q1", "Q2", "Q3", "Q4", "Q5")
names(ftab4_sd)<-c("Grupo", "Q1", "Q2", "Q3", "Q4", "Q5")

ftab1<-rbind(ftab1_cs,ftab1_sd)
ftab4<-rbind(ftab4_cs,ftab4_sd)

ftab1[is.na(ftab1)==TRUE]<-0
ftab4[is.na(ftab4)==TRUE]<-0

ftab1<-ftab1[,-1]
ftab4<-ftab4[,-1]

#Cálculo de totales por fila 
rtot_ftab1<-rowSums(ftab1)
rtot_ftab4<-rowSums(ftab4)

tot_ftab1<-sum(rtot_ftab1)
tot_ftab4<-sum(rtot_ftab4)

ftab1<-ftab1/tot_ftab1
ftab4<-ftab4/tot_ftab4

rm(ftab1_cs,ftab1_sd,ftab4_cs,ftab4_sd)

grupos<-read.table("C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/types.txt", sep="\t")
grupos<-as.character(grupos[,1])


# Matcheo los nombres de los grupos con los nombres de las columnas de las tablas
tab1<-tab1[match(grupos, rownames(tab1)),]
tab4<-tab4[match(grupos, rownames(tab4)),]
ftab1<-ftab1[match(grupos, rownames(ftab1)),]
ftab4<-ftab4[match(grupos, rownames(ftab4)),]

#Renombro todas las filas
rownames(tab1)<-grupos
rownames(tab4)<-grupos
rownames(ftab1)<-grupos
rownames(ftab4)<-grupos

#Convierto los missing en ceros
for(i in 1:5){
  tab1[which(is.na(tab1[,i])),i]<-0.0000
  tab4[which(is.na(tab4[,i])),i]<-0.0000
  ftab1[which(is.na(ftab1[,i])),i]<-0.0000
  ftab4[which(is.na(ftab4[,i])),i]<-0.0000
}

# Exporto las tablas
write.table(format(tab1, scientific = FALSE),
            file="C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/r_ftotinc_adj_1.txt",
            sep="\t", quote=FALSE, col.names=FALSE, row.names=FALSE)
write.table(format(tab4, scientific = FALSE),
            file="C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/r_ftotinc_adj_4.txt",
            sep="\t", quote=FALSE, col.names=FALSE, row.names=FALSE)
write.table(format(ftab1, scientific = FALSE),
            file="C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/f_ftotinc_adj_1.txt",
            sep="\t", quote=FALSE, col.names=FALSE, row.names=FALSE)
write.table(format(ftab4, scientific = FALSE),file="C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/f_ftotinc_adj_4.txt",
            sep="\t", quote=FALSE, col.names=FALSE, row.names=FALSE)

# Genero el ingreso laboral relativo de la mujer con respecto al total
Per1_cs<-mutate(Per1_cs, ing_esp=ila_fr/(ila_mr+ila_fr))
Per4_cs<-mutate(Per4_cs, ing_esp=ila_fr/(ila_mr+ila_fr))

iesp1 <- Per1_cs %>% group_by(nivel_m, nivel_f) %>%
  summarise(mean_iesp=mean(ing_esp, na.rm=TRUE))

iesp4 <- Per4_cs %>% group_by(nivel_m, nivel_f) %>%
  summarise(mean_iesp=mean(ing_esp, na.rm=TRUE))

# Formato de matriz
iesp1<-dcast(iesp1, nivel_m~nivel_f)
iesp4<-dcast(iesp4, nivel_m~nivel_f)

write.table(iesp1,file="C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/wife_share_1.txt",sep="\t", col.names = F, row.names = F)
write.table(iesp4,file="C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/wife_share_4.txt",sep="\t", col.names = F, row.names = F)

# Genero ingresos laborales totales y calculo quintiles
Per1_cs<-mutate(Per1_cs, ila_t=ila_mr+ila_fr)
Per4_cs<-mutate(Per4_cs, ila_t=ila_mr+ila_fr)

Per1_cs<-tbl_df(Per1_cs)
Per4_cs<-tbl_df(Per4_cs)

#Diferentes opciones de cálculo de cuantiles
Per1_cs<-mutate(Per1_cs, cuant_ilat1=ntile(ila_t,5))
Per4_cs<-mutate(Per4_cs, cuant_ilat4=ntile(ila_t,5))
Per1_cs<-mutate(Per1_cs, cuant_ilad1=gcuan(ila_t,ncuan=5))
Per4_cs<-mutate(Per4_cs, cuant_ilad4=gcuan(ila_t,ncuan=5))


# participación del ingreso y participación laboral por quintiles del
# ingreso laboral total
part_esp_1<- Per1_cs %>% group_by(cuant_ilat1) %>%
  summarise(part = mean(ing_esp, na.rm = TRUE))

part_wrk_1<- Per1_cs %>% group_by(cuant_ilat1) %>%
  summarise(part = mean(as.numeric(part_f=="fpart"), 
                        na.rm = TRUE))
part_esp_4<- Per4_cs %>% group_by(cuant_ilat4) %>%
  summarise(part = mean(ing_esp, na.rm = TRUE))

part_wrk_4<- Per4_cs %>% group_by(cuant_ilat4) %>%
  summarise(part = mean(as.numeric(part_f=="fpart"), 
                        na.rm = TRUE))

part_esp<-cbind(part_esp_1[,2], part_esp_4[,2])
part_wrk<-cbind(part_wrk_1[,2], part_wrk_4[,2])

write.table(part_esp,file="C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/income_share_wife.txt",sep="\t", col.names = F, row.names = F)
write.table(part_wrk,file="C:/Users/VictorF/Dropbox/Tesis Maestría/codes_ggksPandP/Output/lfp_wife.txt",sep="\t", col.names = F, row.names = F)
