## Escuela Politécnica Nacional - Departamento de Ingeniería Civil y Ambiental, Quito, Ecuador
## Author: Sebastián Páez-Bimos, Natalia Horna, Bastian Manz
## Date: 13/07/2016
## Date: 13/07/2016

##########################################################################
##### Subregion plots for Comparative Evaluation TPMAv7 and IMERG v3 #####
##########################################################################
library(xts)
library(sp)
library(rgdal)
library(hydroGOF)
library(gstat)
library(abind)

library(ggplot2)
library(cowplot)

rm(list=ls())
gc()

load('results_26082016.RData')
load('est_subregions.RData')
load('gauges.RData')

res<-list()

for (k in 1:length(results)){
  temp<-1:99
  temp1<-data.frame()
  temp2<-data.frame()
  for (i in 1:length(est_subregions)){
    for (j in 1:length(est_subregions[[i]])){
      a<-which(colnames(gauges$hr)==est_subregions[[i]][j])
      
      #QTS
      temp<-cbind(temp,results[[k]]$Qts_ratio[,a])
            
      #QPE
      temp1<-rbind(temp1,results[[k]]$QPE[a,])
      
      #OF
      temp2<-rbind(temp2,results[[k]]$occ_frq[a,])
    }
  }
  temp<-temp[,-1]
  res[[k]]<-list()
  res[[k]]$QTS<-temp
  res[[k]]$QPE<-temp1
  res[[k]]$OF<-temp2
}

###QTS###

##1hr
qts_0.1<-cbind(rowMeans(res[[1]]$QTS[,1:34],na.rm=T),
               rowMeans(res[[1]]$QTS[,35:74],na.rm=T),
               rowMeans(res[[1]]$QTS[,75:197],na.rm=T),
               rowMeans(res[[1]]$QTS[,198:274],na.rm=T),
               rowMeans(res[[1]]$QTS[,275:287],na.rm=T),
               rowMeans(res[[1]]$QTS[,288:302],na.rm=T)
)

QTS_gg0.1 <- data.frame(rep(seq(0.01,0.99,0.01),6),
                        unlist(as.data.frame(qts_0.1)),
                        c(rep("I",99),
                          rep("II",99),
                          rep("III",99),
                          rep("IV",99),
                          rep("V",99),
                          rep("VI",99))
)
colnames(QTS_gg0.1)<-c('f_x','QTS','Zone')


qts_0.1_1hr<-ggplot(QTS_gg0.1,aes(x=f_x , y=QTS,color=Zone))+
  geom_line(size=0.75)+ 
  scale_color_manual(values=c("lightsalmon","khaki1","turquoise1","violet","steelblue","green"))+
  ylab("Estimated Qts. / Gauge Qts. (Ratio)")+
  xlab("F(x)")+ 
  theme_bw() + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black", size = 0.7),
        axis.line.y = element_line(color="black", size = 0.7))+
  theme(legend.position = "none")+
  theme(legend.position = "top", legend.title=element_blank(),legend.key = element_blank())+geom_hline(yintercept = 1)

##3 hr
qts_0.25_3hr<-cbind(rowMeans(res[[2]]$QTS[,1:34],na.rm=T),
                    rowMeans(res[[2]]$QTS[,35:74],na.rm=T),
                    rowMeans(res[[2]]$QTS[,75:197],na.rm=T),
                    rowMeans(res[[2]]$QTS[,198:274],na.rm=T),
                    rowMeans(res[[2]]$QTS[,275:287],na.rm=T),
                    rowMeans(res[[2]]$QTS[,288:302],na.rm=T),
                    rowMeans(res[[3]]$QTS[,1:34],na.rm=T),
                    rowMeans(res[[3]]$QTS[,35:74],na.rm=T),
                    rowMeans(res[[3]]$QTS[,75:197],na.rm=T),
                    rowMeans(res[[3]]$QTS[,198:274],na.rm=T),
                    rowMeans(res[[3]]$QTS[,275:287],na.rm=T),
                    rowMeans(res[[3]]$QTS[,288:302],na.rm=T)
)

QTS_gg0.25_3hr <- data.frame(rep(seq(0.01,0.99,0.01),12),
                             unlist(as.data.frame(qts_0.25_3hr)),
                             c(rep('IMERG',99*6),
                               rep('TMPA',99*6)),
                             rep(c(rep("I",99),
                                   rep("II",99),
                                   rep("III",99),
                                   rep("IV",99),
                                   rep("V",99),
                                   rep("VI",99)),2)
)
colnames(QTS_gg0.25_3hr)<-c('f_x','QTS','Product','Zone')


qts_0.25_3hr<-ggplot(QTS_gg0.25_3hr,aes(x=f_x , y=QTS,color=Zone, linetype=Product,shape=Product))+
  geom_line(size= 0.7)+ geom_point(size=4)+
  scale_linetype_manual(values=c("solid",'solid'))+
  scale_color_manual(values=c("lightsalmon","khaki1","turquoise1","violet","steelblue","green"))+
  scale_size_manual(values=c(3,0.5))+
  scale_shape_manual(values=c(NA,1))+
  guides(color=guide_legend(override.aes=list(shape=NA)))+
  ylab("Estimated Qts. / Gauge Qts. (Ratio)")+
  xlab("F(x)")+ 
  theme_bw() + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black", size = 0.7),
        axis.line.y = element_line(color="black", size = 0.7))+
  theme(legend.position = "none")+
  theme(legend.position = "top", legend.title=element_blank(),legend.key = element_blank())+geom_hline(yintercept = 1)

##day
qts_0.25_d<-cbind(rowMeans(res[[4]]$QTS[,1:34],na.rm=T),
                  rowMeans(res[[4]]$QTS[,35:74],na.rm=T),
                  rowMeans(res[[4]]$QTS[,75:197],na.rm=T),
                  rowMeans(res[[4]]$QTS[,198:274],na.rm=T),
                  rowMeans(res[[4]]$QTS[,275:287],na.rm=T),
                  rowMeans(res[[4]]$QTS[,288:302],na.rm=T),
                  rowMeans(res[[5]]$QTS[,1:34],na.rm=T),
                  rowMeans(res[[5]]$QTS[,35:74],na.rm=T),
                  rowMeans(res[[5]]$QTS[,75:197],na.rm=T),
                  rowMeans(res[[5]]$QTS[,198:274],na.rm=T),
                  rowMeans(res[[5]]$QTS[,275:287],na.rm=T),
                  rowMeans(res[[5]]$QTS[,288:302],na.rm=T)
)

QTS_gg0.25_d <- data.frame(rep(seq(0.01,0.99,0.01),12),
                           unlist(as.data.frame(qts_0.25_d)),
                           c(rep('IMERG',99*6),
                             rep('TMPA',99*6)),
                           rep(c(rep("I",99),
                                 rep("II",99),
                                 rep("III",99),
                                 rep("IV",99),
                                 rep("V",99),
                                 rep("VI",99)),2)
)
colnames(QTS_gg0.25_d)<-c('f_x','QTS','Product','Zone')

qts_0.25_d<-ggplot(QTS_gg0.25_d,aes(x=f_x , y=QTS,color=Zone, linetype=Product,shape=Product))+
  geom_line(size= 0.7)+ geom_point(size=3)+
  scale_linetype_manual(values=c("solid",'solid'))+
  scale_color_manual(values=c("lightsalmon","khaki1","turquoise1","violet","steelblue","green"))+
  scale_size_manual(values=c(3,0.5))+
  scale_shape_manual(values=c(NA,1))+
  guides(color=guide_legend(override.aes=list(shape=NA)))+
  ylab("Estimated Qts. / Gauge Qts. (Ratio)")+
  xlab("F(x)")+ 
  theme_bw() + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black", size = 0.7),
        axis.line.y = element_line(color="black", size = 0.7))+
  theme(legend.position = "none")+
  theme(legend.position = "top", legend.title=element_blank(),legend.key = element_blank())+geom_hline(yintercept = 1)

###OF###
OFs_0.1 <- res[[1]]$OF
colnames(OFs_0.1) <- c("Gauge-hr","IMERG-0.1-hr")

OFs_0.1_gg <- data.frame(unlist(OFs_0.1),
                     c(rep("1-Gauge",34),
                       rep("2-Gauge",40),
                       rep("3-Gauge",123),
                       rep("4-Gauge",77),
                       rep("5-Gauge",13),
                       rep("6-Gauge",15),
                       rep("1-IMERG",34),
                       rep("2-IMERG",40),
                       rep("3-IMERG",123),
                       rep("4-IMERG",77),
                       rep("5-IMERG",13),
                       rep("6-IMERG",15)),
                      rep("0.1-hr",302*2),
                     c(rep("I",34),
                       rep("II",40),
                       rep("III",123),
                       rep("IV",77),
                       rep("V",13),
                       rep("VI",15),
                       rep("I",34),
                       rep("II",40),
                       rep("III",123),
                       rep("IV",77),
                       rep("V",13),
                       rep("VI",15))
)
colnames(OFs_0.1_gg) <- c("Val","Zone_product","Space.Time",'Zone')

b<-qplot(Zone_product,Val,data=OFs_0.1_gg,fill=Zone,geom='boxplot',xlab=("Spatial scale: 0.1°/ hr "),ylab=("Occurrence Frequency (%)"))+
  facet_grid(.~Zone,scale='free') +
  scale_x_discrete(labels = c('G','I','G','I','G','I','G','I','G','I','G','I')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightsalmon",'khaki1',"turquoise1","plum1","steelblue2","greenyellow"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10)) 
  
g <- ggplotGrob(b)

#cambiar los paneles del fondo por color blanco
g$grobs[[9]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[11]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[13]]$children[[1]]$children[[1]]$gp$fill <- "white"

file='OF_0.1_1hr_31082016.pdf'
pdf(file=file, width=10, height=10)
plot(b)
dev.off()



OFs_0.25 <- cbind(res[[2]]$OF,
                  res[[3]]$OF[,2])
colnames(OFs_0.25) <- c("Gauge-3hr","IMERG-0.25-3hr","TMPA-0.25-3hr")

OFs_0.25_gg <- data.frame(unlist(OFs_0.25),
                         c(rep("1-Gauge",34),
                           rep("2-Gauge",40),
                           rep("3-Gauge",123),
                           rep("4-Gauge",77),
                           rep("5-Gauge",13),
                           rep("6-Gauge",15),
                           rep("1-IMERG",34),
                           rep("2-IMERG",40),
                           rep("3-IMERG",123),
                           rep("4-IMERG",77),
                           rep("5-IMERG",13),
                           rep("6-IMERG",15),
                           rep("1-TMPA",34),
                           rep("2-TMPA",40),
                           rep("3-TMPA",123),
                           rep("4-TMPA",77),
                           rep("5-TMPA",13),
                           rep("6-TMPA",15)),
                           rep("0.25-3hr",302*3),
                         c(rep("I",34),
                           rep("II",40),
                           rep("III",123),
                           rep("IV",77),
                           rep("V",13),
                           rep("VI",15),
                           rep("I",34),
                           rep("II",40),
                           rep("III",123),
                           rep("IV",77),
                           rep("V",13),
                           rep("VI",15),
                           rep("I",34),
                           rep("II",40),
                           rep("III",123),
                           rep("IV",77),
                           rep("V",13),
                           rep("VI",15))
)
colnames(OFs_0.25_gg) <- c("Val","Zone_product","Space.Time",'Zone')

b<-qplot(Zone_product,Val,data=OFs_0.25_gg,fill=Zone,geom='boxplot',xlab=("Spatial scale: 0.25°/ 3hr "),ylab=("Occurrence Frequency (%)"))+
  facet_grid(.~Zone,scale='free') +
  scale_x_discrete(labels = c('G','I','T','G','I','T','G','I','T','G','I','T','G','I','T','G','I','T')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightsalmon",'khaki1',"turquoise1","plum1","steelblue2","greenyellow"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10)) 

g <- ggplotGrob(b)

#cambiar los paneles del fondo por color blanco
g$grobs[[9]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[11]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[13]]$children[[1]]$children[[1]]$gp$fill <- "white"

file='OF_0.25_3hr_31082016.pdf'
pdf(file=file, width=10, height=6)
plot(g)
dev.off()


OFs_0.25d <- cbind(res[[4]]$OF,
                  res[[5]]$OF[,2])
colnames(OFs_0.25d) <- c("Gauge-day","IMERG-0.25-day","TMPA-0.25-day")

OFs_0.25d_gg <- data.frame(unlist(OFs_0.25d),
                          c(rep("1-Gauge",34),
                            rep("2-Gauge",40),
                            rep("3-Gauge",123),
                            rep("4-Gauge",77),
                            rep("5-Gauge",13),
                            rep("6-Gauge",15),
                            rep("1-IMERG",34),
                            rep("2-IMERG",40),
                            rep("3-IMERG",123),
                            rep("4-IMERG",77),
                            rep("5-IMERG",13),
                            rep("6-IMERG",15),
                            rep("1-TMPA",34),
                            rep("2-TMPA",40),
                            rep("3-TMPA",123),
                            rep("4-TMPA",77),
                            rep("5-TMPA",13),
                            rep("6-TMPA",15)),
                          rep("0.25-day",302*3),
                          c(rep("I",34),
                            rep("II",40),
                            rep("III",123),
                            rep("IV",77),
                            rep("V",13),
                            rep("VI",15),
                            rep("I",34),
                            rep("II",40),
                            rep("III",123),
                            rep("IV",77),
                            rep("V",13),
                            rep("VI",15),
                            rep("I",34),
                            rep("II",40),
                            rep("III",123),
                            rep("IV",77),
                            rep("V",13),
                            rep("VI",15))
)
colnames(OFs_0.25d_gg) <- c("Val","Zone_product","Space.Time",'Zone')

b<-qplot(Zone_product,Val,data=OFs_0.25d_gg,fill=Zone,geom='boxplot',xlab=("Spatial scale: 0.25°/ day "),ylab=("Occurrence Frequency (%)"))+
  facet_grid(.~Zone,scale='free') +
  scale_x_discrete(labels = c('G','I','T','G','I','T','G','I','T','G','I','T','G','I','T','G','I','T')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightsalmon",'khaki1',"turquoise1","plum1","steelblue2","greenyellow"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10)) 

g <- ggplotGrob(b)

#cambiar los paneles del fondo por color blanco
g$grobs[[9]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[11]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[13]]$children[[1]]$children[[1]]$gp$fill <- "white"

file='OF_0.25_day_31082016.pdf'
pdf(file=file, width=10, height=6)
plot(g)
dev.off()


###QPE####
CORs <- cbind(res[[1]]$QPE[,1],
              res[[2]]$QPE[,1],
              res[[3]]$QPE[,1],
              res[[4]]$QPE[,1],
              res[[5]]$QPE[,1]
)
colnames(CORs)  <- c("Gauge-IMERG-0.1-hr","Gauge-IMERG-0.25-3hr","Gauge-TMPA-0.25-3hr","Gauge-IMERG-0.25-day","Gauge-TMPA-0.25-day")

RMSEs <- cbind(res[[1]]$QPE[,2],
               res[[2]]$QPE[,2],
               res[[3]]$QPE[,2],
               res[[4]]$QPE[,2],
               res[[5]]$QPE[,2]
)
colnames(RMSEs)  <- c("Gauge-IMERG-0.1-hr","Gauge-IMERG-0.25-3hr","Gauge-TMPA-0.25-3hr","Gauge-IMERG-0.25-day","Gauge-TMPA-0.25-day")

Bias <- cbind(res[[1]]$QPE[,5],
              res[[2]]$QPE[,5],
              res[[3]]$QPE[,5],
              res[[4]]$QPE[,5],
              res[[5]]$QPE[,5]
)
colnames(Bias)  <- c("Gauge-IMERG-0.1-hr","Gauge-IMERG-0.25-3hr","Gauge-TMPA-0.25-3hr","Gauge-IMERG-0.25-day","Gauge-TMPA-0.25-day")

#plot QPE 0.1/hr

CORs_gg0.1 <- data.frame(unlist(as.data.frame(CORs[,1])),
                      c(rep("1-Gauge/IMERG",34),
                        rep("2-Gauge/IMERG",40),
                        rep("3-Gauge/IMERG",123),
                        rep("4-Gauge/IMERG",77),
                        rep("5-Gauge/IMERG",13),
                        rep("6-Gauge/IMERG",15)),
                        rep("0.1-hr",302),
                      c(rep("I",34),
                        rep("II",40),
                        rep("III",123),
                        rep("IV",77),
                        rep("V",13),
                        rep("VI",15))
)
colnames(CORs_gg0.1) <- c("Val","Zone_product","Space.Time",'Zone')

RMSEs_gg0.1 <- data.frame(unlist(as.data.frame(RMSEs[,1])),
                         c(rep("1-Gauge/IMERG",34),
                           rep("2-Gauge/IMERG",40),
                           rep("3-Gauge/IMERG",123),
                           rep("4-Gauge/IMERG",77),
                           rep("5-Gauge/IMERG",13),
                           rep("6-Gauge/IMERG",15)),
                         rep("0.1-hr",302),
                         c(rep("I",34),
                           rep("II",40),
                           rep("III",123),
                           rep("IV",77),
                           rep("V",13),
                           rep("VI",15))
)
colnames(RMSEs_gg0.1) <- c("Val","Zone_product","Space.Time",'Zone')

Bias_gg0.1 <- data.frame(unlist(as.data.frame(Bias[,1])),
                          c(rep("1-Gauge/IMERG",34),
                            rep("2-Gauge/IMERG",40),
                            rep("3-Gauge/IMERG",123),
                            rep("4-Gauge/IMERG",77),
                            rep("5-Gauge/IMERG",13),
                            rep("6-Gauge/IMERG",15)),
                          rep("0.1-hr",302),
                          c(rep("I",34),
                            rep("II",40),
                            rep("III",123),
                            rep("IV",77),
                            rep("V",13),
                            rep("VI",15))
)
colnames(Bias_gg0.1) <- c("Val","Zone_product","Space.Time",'Zone')

# COR plot
a<-qplot(Zone_product,Val,data=CORs_gg0.1,fill=Zone,geom='boxplot',xlab=("Spatial scale: 0.1°/hr "),ylab=("Correlation coeficient (-)"))+
  facet_grid(.~Zone,scale='free') +
  scale_x_discrete(labels = c('GI','GI','GI','GI','GI','GI')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightsalmon",'khaki1',"turquoise1","plum1","steelblue2","greenyellow"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.position="none") 

corr0.1 <- ggplotGrob(b)

#cambiar los paneles del fondo por color blanco
corr0.1$grobs[[9]]$children[[1]]$children[[1]]$gp$fill <- "white"
corr0.1$grobs[[11]]$children[[1]]$children[[1]]$gp$fill <- "white"
corr0.1$grobs[[13]]$children[[1]]$children[[1]]$gp$fill <- "white"

b<-qplot(Zone_product,Val,data=RMSEs_gg0.1,fill=Zone,geom='boxplot',xlab=("Spatial scale:0.1/hr"),ylab=("Root Mean Square Error (mm/h)"))+
  facet_grid(.~Zone,scale='free') +
  scale_x_discrete(labels = c('GI','GI','GI','GI','GI','GI'))+
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightsalmon",'khaki1',"turquoise1","plum1","steelblue2","greenyellow"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.position="none") 

rmse0.1 <- ggplotGrob(b)

#cambiar los paneles del fondo por color blanco
rmse0.1$grobs[[9]]$children[[1]]$children[[1]]$gp$fill <- "white"
rmse0.1$grobs[[11]]$children[[1]]$children[[1]]$gp$fill <- "white"
rmse0.1$grobs[[13]]$children[[1]]$children[[1]]$gp$fill <- "white"

c<-qplot(Zone_product,Val,data=Bias_gg0.1,fill=Zone,geom='boxplot',xlab=("Spatial scale:0.1/hr"),ylab=("Percentage Bias (%)"))+
  facet_grid(.~Zone,scale='free') +
  scale_x_discrete(labels = c('GI','GI','GI','GI','GI','GI'))+
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightsalmon",'khaki1',"turquoise1","plum1","steelblue2","greenyellow"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))+
        scale_y_continuous(limits = c(0,300)) 

bias0.1 <- ggplotGrob(b)

#cambiar los paneles del fondo por color blanco
bias0.1$grobs[[9]]$children[[1]]$children[[1]]$gp$fill <- "white"
bias0.1$grobs[[11]]$children[[1]]$children[[1]]$gp$fill <- "white"
bias0.1$grobs[[13]]$children[[1]]$children[[1]]$gp$fill <- "white"

file='QPE0.1hr_31082016.pdf'
pdf(file=file, width=12, height=8)
plot_grid(a,b,c, ncol = 3, nrow = 1)
dev.off()


#plot QPE 0.25/3hr
CORs_gg0.25 <- data.frame(unlist(as.data.frame(CORs[,2:3])),
                         c(rep("1-Gauge/IMERG",34),
                           rep("2-Gauge/IMERG",40),
                           rep("3-Gauge/IMERG",123),
                           rep("4-Gauge/IMERG",77),
                           rep("5-Gauge/IMERG",13),
                           rep("6-Gauge/IMERG",15),
                           rep("1-Gauge/TMPA",34),
                           rep("2-Gauge/TMPA",40),
                           rep("3-Gauge/TMPA",123),
                           rep("4-Gauge/TMPA",77),
                           rep("5-Gauge/TMPA",13),
                           rep("6-Gauge/TMPA",15)),
                           rep("0.25-3hr",302*2),
                           c(rep("I",34),
                           rep("II",40),
                           rep("III",123),
                           rep("IV",77),
                           rep("V",13),
                           rep("VI",15),
                           rep("I",34),
                           rep("II",40),
                           rep("III",123),
                           rep("IV",77),
                           rep("V",13),
                           rep("VI",15))
)
colnames(CORs_gg0.25) <- c("Val","Zone_product","Space.Time",'Zone')

RMSEs_gg0.25 <- data.frame(unlist(as.data.frame(RMSEs[,2:3])),
                          c(rep("1-Gauge/IMERG",34),
                            rep("2-Gauge/IMERG",40),
                            rep("3-Gauge/IMERG",123),
                            rep("4-Gauge/IMERG",77),
                            rep("5-Gauge/IMERG",13),
                            rep("6-Gauge/IMERG",15),
                            rep("1-Gauge/TMPA",34),
                            rep("2-Gauge/TMPA",40),
                            rep("3-Gauge/TMPA",123),
                            rep("4-Gauge/TMPA",77),
                            rep("5-Gauge/TMPA",13),
                            rep("6-Gauge/TMPA",15)),
                          rep("0.25-3hr",302*2),
                          c(rep("I",34),
                            rep("II",40),
                            rep("III",123),
                            rep("IV",77),
                            rep("V",13),
                            rep("VI",15),
                            rep("I",34),
                            rep("II",40),
                            rep("III",123),
                            rep("IV",77),
                            rep("V",13),
                            rep("VI",15))
)
colnames(RMSEs_gg0.25) <- c("Val","Zone_product","Space.Time",'Zone')

Bias_gg0.25 <- data.frame(unlist(as.data.frame(Bias[,2:3])),
                          c(rep("1-Gauge/IMERG",34),
                            rep("2-Gauge/IMERG",40),
                            rep("3-Gauge/IMERG",123),
                            rep("4-Gauge/IMERG",77),
                            rep("5-Gauge/IMERG",13),
                            rep("6-Gauge/IMERG",15),
                            rep("1-Gauge/TMPA",34),
                            rep("2-Gauge/TMPA",40),
                            rep("3-Gauge/TMPA",123),
                            rep("4-Gauge/TMPA",77),
                            rep("5-Gauge/TMPA",13),
                            rep("6-Gauge/TMPA",15)),
                          rep("0.25-3hr",302*2),
                          c(rep("I",34),
                            rep("II",40),
                            rep("III",123),
                            rep("IV",77),
                            rep("V",13),
                            rep("VI",15),
                            rep("I",34),
                            rep("II",40),
                            rep("III",123),
                            rep("IV",77),
                            rep("V",13),
                            rep("VI",15))
)
colnames(Bias_gg0.25) <- c("Val","Zone_product","Space.Time",'Zone')


# COR plot
a<-qplot(Zone_product,Val,data=CORs_gg0.25,fill=Zone,geom='boxplot',xlab=("Spatial scale:0.25/3hr"),ylab=("Correlation coeficient (-)"))+
  facet_grid(.~Zone,scale='free') +
  scale_x_discrete(labels = c('GI','GT','GI','GT','GI','GT','GI','GT','GI','GT','GI','GT'))+
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightsalmon",'khaki1',"turquoise1","plum1","steelblue2","greenyellow"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.position="none") 

corr0.25 <- ggplotGrob(b)

#cambiar los paneles del fondo por color blanco
corr0.25$grobs[[9]]$children[[1]]$children[[1]]$gp$fill <- "white"
corr0.25$grobs[[11]]$children[[1]]$children[[1]]$gp$fill <- "white"
corr0.25$grobs[[13]]$children[[1]]$children[[1]]$gp$fill <- "white"

b<-qplot(Zone_product,Val,data=RMSEs_gg0.25,fill=Zone,geom='boxplot',xlab=("Spatial scale:0.25/3hr"),ylab=("Root Mean Square Error (mm/h)"))+
  facet_grid(.~Zone,scale='free') +
  scale_x_discrete(labels = c('GI','GT','GI','GT','GI','GT','GI','GT','GI','GT','GI','GT'))+
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightsalmon",'khaki1',"turquoise1","plum1","steelblue2","greenyellow"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.position="none") 

rmse0.25 <- ggplotGrob(b)

#cambiar los paneles del fondo por color blanco
rmse0.25$grobs[[9]]$children[[1]]$children[[1]]$gp$fill <- "white"
rmse0.25$grobs[[11]]$children[[1]]$children[[1]]$gp$fill <- "white"
rmse0.25$grobs[[13]]$children[[1]]$children[[1]]$gp$fill <- "white"

c<-qplot(Zone_product,Val,data=Bias_gg0.25,fill=Zone,geom='boxplot',xlab=("Spatial scale:0.25/3hr"),ylab=("Percentage Bias (%)"))+
  facet_grid(.~Zone,scale='free') +
  scale_x_discrete(labels = c('GI','GT','GI','GT','GI','GT','GI','GT','GI','GT','GI','GT'))+
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightsalmon",'khaki1',"turquoise1","plum1","steelblue2","greenyellow"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))+
        scale_y_continuous(limits = c(0,300)) 

bias0.25 <- ggplotGrob(b)

#cambiar los paneles del fondo por color blanco
bias0.25$grobs[[9]]$children[[1]]$children[[1]]$gp$fill <- "white"
bias0.25$grobs[[11]]$children[[1]]$children[[1]]$gp$fill <- "white"
bias0.25$grobs[[13]]$children[[1]]$children[[1]]$gp$fill <- "white"

file='QPE0.253hr_31082016.pdf'
pdf(file=file, width=12, height=8)
plot_grid(a,b,c, ncol = 3, nrow = 1)
dev.off()


#plot QPE 0.25/day
CORs_gg0.25d <- data.frame(unlist(as.data.frame(CORs[,4:5])),
                          c(rep("1-Gauge/IMERG",34),
                            rep("2-Gauge/IMERG",40),
                            rep("3-Gauge/IMERG",123),
                            rep("4-Gauge/IMERG",77),
                            rep("5-Gauge/IMERG",13),
                            rep("6-Gauge/IMERG",15),
                            rep("1-Gauge/TMPA",34),
                            rep("2-Gauge/TMPA",40),
                            rep("3-Gauge/TMPA",123),
                            rep("4-Gauge/TMPA",77),
                            rep("5-Gauge/TMPA",13),
                            rep("6-Gauge/TMPA",15)),
                          rep("0.25-day",302*2),
                          c(rep("I",34),
                            rep("II",40),
                            rep("III",123),
                            rep("IV",77),
                            rep("V",13),
                            rep("VI",15),
                            rep("I",34),
                            rep("II",40),
                            rep("III",123),
                            rep("IV",77),
                            rep("V",13),
                            rep("VI",15))
)
colnames(CORs_gg0.25d) <- c("Val","Zone_product","Space.Time",'Zone')

RMSEs_gg0.25d <- data.frame(unlist(as.data.frame(RMSEs[,4:5])),
                           c(rep("1-Gauge/IMERG",34),
                             rep("2-Gauge/IMERG",40),
                             rep("3-Gauge/IMERG",123),
                             rep("4-Gauge/IMERG",77),
                             rep("5-Gauge/IMERG",13),
                             rep("6-Gauge/IMERG",15),
                             rep("1-Gauge/TMPA",34),
                             rep("2-Gauge/TMPA",40),
                             rep("3-Gauge/TMPA",123),
                             rep("4-Gauge/TMPA",77),
                             rep("5-Gauge/TMPA",13),
                             rep("6-Gauge/TMPA",15)),
                           rep("0.25-3hr",302*2),
                           c(rep("I",34),
                             rep("II",40),
                             rep("III",123),
                             rep("IV",77),
                             rep("V",13),
                             rep("VI",15),
                             rep("I",34),
                             rep("II",40),
                             rep("III",123),
                             rep("IV",77),
                             rep("V",13),
                             rep("VI",15))
)
colnames(RMSEs_gg0.25d) <- c("Val","Zone_product","Space.Time",'Zone')

Bias_gg0.25d <- data.frame(unlist(as.data.frame(Bias[,4:5])),
                          c(rep("1-Gauge/IMERG",34),
                            rep("2-Gauge/IMERG",40),
                            rep("3-Gauge/IMERG",123),
                            rep("4-Gauge/IMERG",77),
                            rep("5-Gauge/IMERG",13),
                            rep("6-Gauge/IMERG",15),
                            rep("1-Gauge/TMPA",34),
                            rep("2-Gauge/TMPA",40),
                            rep("3-Gauge/TMPA",123),
                            rep("4-Gauge/TMPA",77),
                            rep("5-Gauge/TMPA",13),
                            rep("6-Gauge/TMPA",15)),
                          rep("0.25-3hr",302*2),
                          c(rep("I",34),
                            rep("II",40),
                            rep("III",123),
                            rep("IV",77),
                            rep("V",13),
                            rep("VI",15),
                            rep("I",34),
                            rep("II",40),
                            rep("III",123),
                            rep("IV",77),
                            rep("V",13),
                            rep("VI",15))
)
colnames(Bias_gg0.25d) <- c("Val","Zone_product","Space.Time",'Zone')

# COR plot
a<-qplot(Zone_product,Val,data=CORs_gg0.25d,fill=Zone,geom='boxplot',xlab=("Spatial scale:0.25/day"),ylab=("Correlation coeficient (-)"))+
  facet_grid(.~Zone,scale='free') +
  scale_x_discrete(labels = c('GI','GT','GI','GT','GI','GT','GI','GT','GI','GT','GI','GT'))+
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightsalmon",'khaki1',"turquoise1","plum1","steelblue2","greenyellow"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.position="none") 

corr0.25d <- ggplotGrob(b)

#cambiar los paneles del fondo por color blanco
corr0.25d$grobs[[9]]$children[[1]]$children[[1]]$gp$fill <- "white"
corr0.25d$grobs[[11]]$children[[1]]$children[[1]]$gp$fill <- "white"
corr0.25d$grobs[[13]]$children[[1]]$children[[1]]$gp$fill <- "white"

b<-qplot(Zone_product,Val,data=RMSEs_gg0.25d,fill=Zone,geom='boxplot',xlab=("Spatial scale:0.25/day"),ylab=("Root Mean Square Error (mm/h)"))+
  facet_grid(.~Zone,scale='free') +
  scale_x_discrete(labels = c('GI','GT','GI','GT','GI','GT','GI','GT','GI','GT','GI','GT'))+
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightsalmon",'khaki1',"turquoise1","plum1","steelblue2","greenyellow"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.position="none") 

rmse0.25d <- ggplotGrob(b)

#cambiar los paneles del fondo por color blanco
rmse0.25d$grobs[[9]]$children[[1]]$children[[1]]$gp$fill <- "white"
rmse0.25d$grobs[[11]]$children[[1]]$children[[1]]$gp$fill <- "white"
rmse0.25d$grobs[[13]]$children[[1]]$children[[1]]$gp$fill <- "white"

c<-qplot(Zone_product,Val,data=Bias_gg0.25d,fill=Zone,geom='boxplot',xlab=("Spatial scale:0.25/day"),ylab=("Percentage Bias (%)"))+
  facet_grid(.~Zone,scale='free') +
  scale_x_discrete(labels = c('GI','GT','GI','GT','GI','GT','GI','GT','GI','GT','GI','GT'))+
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightsalmon",'khaki1',"turquoise1","plum1","steelblue2","greenyellow"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))+
        scale_y_continuous(limits = c(0,300))

bias0.25d <- ggplotGrob(b)

#cambiar los paneles del fondo por color blanco
bias0.25d$grobs[[9]]$children[[1]]$children[[1]]$gp$fill <- "white"
bias0.25d$grobs[[11]]$children[[1]]$children[[1]]$gp$fill <- "white"
bias0.25d$grobs[[13]]$children[[1]]$children[[1]]$gp$fill <- "white"

file='QPE0.25day_31082016.pdf'
pdf(file=file, width=12, height=8)
plot_grid(a,b,c, ncol = 3, nrow = 1)
dev.off()