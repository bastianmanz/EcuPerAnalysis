###############################################
# Analisis de Deteccion, QPE y Distribuciones #
###############################################
##########for average of GPM and TRMM##########
#################(pixel 0.25)##################
###############################################

library(xts)
library(sp)
library(rgdal)
library(hydroGOF)
library(gstat)

rm(list=ls())
gc()

#TRMM y GPM pixel=0.25
load('gauge025_avg.RData')

load("tmpa_stnlocation.RData")
names(tmpa) # all at scale 0.25 degrees
load("gpm025_stnlocation.RData",verbose = T)
names(gpm025) # all at scale 0.25 degrees

#eliminar columnas con valores repetidos GPM025
gpm025_pixel<-list()
gpm025_pixel$hr<-gpm025$hr[,-which(duplicated(gpm025$sp))] 
colnames(gpm025_pixel$hr)<-gpm025$sp[-which(duplicated(gpm025$sp))]

gpm025_pixel$hr3<-gpm025$hr3[,-which(duplicated(gpm025$sp))] 
colnames(gpm025_pixel$hr3)<-gpm025$sp[-which(duplicated(gpm025$sp))]

gpm025_pixel$d<-gpm025$d[,-which(duplicated(gpm025$sp))] 
colnames(gpm025_pixel$d)<-gpm025$sp[-which(duplicated(gpm025$sp))]

rm(gpm025)

#eliminar columnas con valores repetidos TRMM
trmm_pixel<-list()
trmm_pixel$hr<-tmpa$hr[,-which(duplicated(tmpa$sp@data$ID))] 
colnames(trmm_pixel$hr)<-colnames(gpm025_pixel$hr)

trmm_pixel$hr3<-tmpa$hr3[,-which(duplicated(tmpa$sp@data$ID))] 
colnames(trmm_pixel$hr3)<-colnames(gpm025_pixel$hr3)

trmm_pixel$d<-tmpa$d[,-which(duplicated(tmpa$sp@data$ID))]
colnames(trmm_pixel$d)<-colnames(gpm025_pixel$d)

rm(tmpa)

#ordenar columnas en funcion del orden que tiene en gauge025_avg
gpm025_pixel$hr<-gpm025_pixel$hr[,order(match(colnames(gpm025_pixel$hr),colnames(gauge025_avg$hr)))]
gpm025_pixel$hr3<-gpm025_pixel$hr3[,order(match(colnames(gpm025_pixel$hr3),colnames(gauge025_avg$hr3)))]
gpm025_pixel$d<-gpm025_pixel$d[,order(match(colnames(gpm025_pixel$d),colnames(gauge025_avg$d)))]

trmm_pixel$hr<-trmm_pixel$hr[,order(match(colnames(trmm_pixel$hr),colnames(gauge025_avg$hr)))]
trmm_pixel$hr3<-trmm_pixel$hr3[,order(match(colnames(trmm_pixel$hr3),colnames(gauge025_avg$hr3)))]
trmm_pixel$d<-trmm_pixel$d[,order(match(colnames(trmm_pixel$d),colnames(gauge025_avg$d)))]


#evaluar entre gauge025_avg y gpm025_pixel y trmm_pixel
g_products0.25 <- list(gauge025_avg$hr,gauge025_avg$hr,gauge025_avg$hr3,gauge025_avg$hr3,gauge025_avg$d,gauge025_avg$d)
sat_products0.25 <- list(gpm025_pixel$hr,trmm_pixel$hr,gpm025_pixel$hr3,trmm_pixel$hr3,gpm025_pixel$d,trmm_pixel$d)

# Define range of thresholds for detection stats
thresholds_detect <- data.frame(c("min1h","min3h","min_d","max1h","max3h"),c(0.1,0.3,2.4,10,30))
colnames(thresholds_detect)<-c("timestep","threshold")

# Define min. threshold for QPE & Distributions
min_threshold <- 0.1 

results0.25 <- list()

for(k in 1:length(g_products0.25)){
  
  print(Sys.time())
  
  # Deteccion 
  detection <- list()
  
  if (k==1|k==2|k==3|k==4){
    detection$Hits <- matrix(ncol=ncol(g_products0.25[[1]]),nrow=2)
  }else{
    detection$Hits <- matrix(ncol=ncol(g_products0.25[[1]]),nrow=1)
  }
  
  detection$Miss <- detection$Hits
  detection$FAs <- detection$Hits
  detection$CZs <- detection$Hits
  detection$ACC <- detection$Hits
  detection$POD <- detection$Hits
  detection$FAR <- detection$Hits
  detection$FBI <- detection$Hits
  detection$ETS <- detection$Hits
  detection$HSS <- detection$Hits
  
  for(i in 1:ncol(g_products0.25[[k]])){
    print(paste(k,'detection',i,sep='_'))
    if (k==1|k==2){
      a=c(1,4)
    }
    if (k==3| k==4){
      a=c(2,5)
    }
    if( k==5| k==6){
      a=3
    }
    
    gs <- merge(g_products0.25[[k]][,i],sat_products0.25[[k]][,i])
    gs <- gs[complete.cases(gs),]
    if (nrow(gs)==0){
      next
    } else{
      gauge <- gs[,1]
      sat <- gs[,2]      
      
      for(j in 1:length(a)){
        b<-a[j]
        
        hits <- sum(gauge>thresholds_detect$threshold[b] & sat>thresholds_detect$threshold[b], na.rm=T)
        miss <- sum(gauge>thresholds_detect$threshold[b] & !(sat>thresholds_detect$threshold[b]), na.rm=T)
        fas <- sum(!(gauge>thresholds_detect$threshold[b]) & sat>thresholds_detect$threshold[b], na.rm=T)
        czeroes <- sum(gauge<=thresholds_detect$threshold[b] & sat<=thresholds_detect$threshold[b],na.rm=T)
        
        detection$Hits[j,i] <- hits/ sum(!is.na(sat)) # perc_hits
        detection$Miss[j,i] <- miss/ sum(!is.na(sat)) # perc_miss
        detection$FAs[j,i] <- fas/ sum(!is.na(sat)) # perc_fas
        detection$CZs[j,i] <- czeroes/ sum(!is.na(sat)) # perc_czs
        
        detection$ACC[j,i] <- (hits + czeroes)/ sum(!is.na(sat)) # ACC
        detection$POD[j,i] <- hits/(hits + miss) # POD
        detection$FAR[j,i] <- fas/(hits + fas) # FAR
        detection$FBI[j,i] <- (hits + fas)/(hits + miss) # FBI
        He = ((hits + miss)*(hits + fas))/sum(!is.na(sat))
        detection$ETS[j,i] <- (hits-He)/(hits + fas + miss - He) # ETS
        detection$HSS[j,i] <- 2*((hits*czeroes)-(fas*miss))/(((hits+miss)*(miss+czeroes)) + ((hits+fas)*(fas+czeroes))) # HSS
      }
    }
  }
  
  # QPE
  QPE <- as.data.frame(matrix(ncol=6,nrow=ncol(g_products0.25[[k]])))
  colnames(QPE) <- c("COR","RMSE","MAE","ME","Bias","NSE")
  
  for(i in 1:ncol(g_products0.25[[k]])){
    print(paste(k,'QPE',i,sep='_'))
    if (sum(g_products0.25[[k]][,i]>0,na.rm=T)<10){
      QPE$COR[i] <- NA
      QPE$RMSE[i] <- NA
      QPE$MAE[i] <- NA
      QPE$ME[i] <- NA
      QPE$Bias[i] <- NA
      QPE$NSE[i] <- NA
    }else{
    
    gs <- merge(g_products0.25[[k]][,i],sat_products0.25[[k]][,i])
    gs <- gs[complete.cases(gs),]
    if (nrow(gs)==0){
      next
    } else{
      gauge <- gs[,1]
      sat <- gs[,2]
      
      gauge[gauge < min_threshold]=0
      sat[sat < min_threshold]=0
      
      QPE$COR[i] <- cor(gauge,sat,use="na.or.complete")
      QPE$RMSE[i] <- rmse(sat,gauge, na.rm=TRUE)
      QPE$MAE[i] <- mae(sat,gauge, na.rm=TRUE)
      QPE$ME[i] <- me(sat,gauge, na.rm=TRUE)
      QPE$Bias[i] <- pbias(sat,gauge, na.rm=TRUE)
      QPE$NSE[i] <- NSE(sat,gauge, na.rm=TRUE)
      
    }
    }
  }
  # Occurrence Frequency and Distributions
  
  occ_frq <- as.data.frame(matrix(data=NA,nrow=ncol(g_products0.25[[k]]),ncol=2))
  colnames(occ_frq) <- c("GAUGE","SAT")
  
  qts <- seq(0.01,0.99,0.01)
  
  gauge_Qts <- as.data.frame(matrix(data=NA,ncol=ncol(g_products0.25[[k]]),nrow=length(qts)))
  sat_Qts <- gauge_Qts
  Qts_ratio <- gauge_Qts
  
  for(i in 1:ncol(g_products0.25[[k]])){
    print(paste(k,'OF_QTS',i,sep='_'))
    
    gs <- merge(g_products0.25[[k]][,i],sat_products0.25[[k]][,i])
    gs <- gs[complete.cases(gs),]
    if (nrow(gs)==0){
      next
    } else{
      gauge <- gs[,1]
      sat <- gs[,2]
      
      gauge[gauge < min_threshold]=0
      sat[sat < min_threshold]=0
      
      occ_frq$SAT[i] <- sum(sat>0,na.rm=T)/sum(!is.na(sat))
      occ_frq$GAUGE[i] <- sum(gauge>0,na.rm=T)/sum(!is.na(gauge))
      
      sat_Qts[,i] <- quantile(sat[sat>0],probs=qts,na.rm=T)
      gauge_Qts[,i] <- quantile(gauge[gauge>0],probs=qts,na.rm=T)
      Qts_ratio[,i] <- sat_Qts[,i]/gauge_Qts[,i]
    }
  }
  
  results0.25[[k]] <- list()
  results0.25[[k]]$detection <- detection
  results0.25[[k]]$QPE <- QPE
  results0.25[[k]]$gauges_Qts <- gauge_Qts
  results0.25[[k]]$sat_Qts <- sat_Qts
  results0.25[[k]]$Qts_ratio <- Qts_ratio
  results0.25[[k]]$occ_frq <- occ_frq
}

#save(results0.25,file='results025.RData')


##PIXEL ANALYSIS##

load('gauges.RData')
load('gauge025_avg.RData')
load('results_22072016.RData')
load('results025.RData')
load('pixel_0.25.RData')
load("tmpa_stnlocation.RData")
load("gpm025_stnlocation.RData")

table0.25<-data.frame()
for (k in 1:length(pixel_0.25)){
  b<-character()
  for (i in 1:length(pixel_0.25[[k]])){
    a<-c(pixel_0.25[[k]][i],names(pixel_0.25)[k])
    b<-rbind(b,a)
  }
  table0.25<-rbind(table0.25,b)
  
}
colnames(table0.25)<-c('gauge','pixel')
table0.25$gauge<-as.character(table0.25$gauge)
table0.25$pixel<-as.character(table0.25$pixel)

### diferencia correlacion average y point2pixel de imerg a 3hr ###
cor_hr3_imerg_avg<-cbind(colnames(gauge025_avg$hr3),results0.25[[3]]$QPE$COR)
colnames(cor_hr3_imerg_avg)<-c('pixel','cor_hr3_imerg_avg')
cor_hr3_imerg_avg<-as.data.frame(cor_hr3_imerg_avg)
 
#completar cada gauge con la informacion del pixel al que pertenece
cor_hr3_imerg_avg<-cor_hr3_imerg_avg[match(table0.25$pixel,cor_hr3_imerg_avg$pixel),2]

table0.25$cor_hr3_imerg_avg<-as.numeric(as.character(cor_hr3_imerg_avg))

cor_g_i_hr3<-cbind(1:ncol(gauges$hr3),results[[2]]$QPE[,1])
colnames(cor_g_i_hr3)<-c('gauge','cor_g_i_hr3')
cor_g_i_hr3<-as.data.frame(cor_g_i_hr3)

cor_g_i_hr3<-cor_g_i_hr3[match(table0.25$gauge,cor_g_i_hr3$gauge),2]
table0.25$cor_g_i_hr3<-as.numeric(as.character(cor_g_i_hr3))

table0.25$dif_g_i_hr3<-round(abs(table0.25$cor_g_i_hr3-table0.25$cor_hr3_imerg_avg),2)

### diferencia correlacion average y point2pixel de tmpa a 3hr ###
cor_hr3_tmpa_avg<-cbind(colnames(gauge025_avg$hr3),results0.25[[4]]$QPE$COR)
colnames(cor_hr3_tmpa_avg)<-c('pixel','cor_hr3_tmpa_avg')
cor_hr3_tmpa_avg<-as.data.frame(cor_hr3_tmpa_avg)

#completar cada gauge con la informacion del pixel al que pertenece
cor_hr3_tmpa_avg<-cor_hr3_tmpa_avg[match(table0.25$pixel,cor_hr3_tmpa_avg$pixel),2]

table0.25$cor_hr3_tmpa_avg<-as.numeric(as.character(cor_hr3_tmpa_avg))

cor_g_t_hr3<-cbind(1:ncol(gauges$hr3),results[[3]]$QPE[,1])
colnames(cor_g_t_hr3)<-c('gauge','cor_g_t_hr3')
cor_g_t_hr3<-as.data.frame(cor_g_t_hr3)

cor_g_t_hr3<-cor_g_t_hr3[match(table0.25$gauge,cor_g_t_hr3$gauge),2]
table0.25$cor_g_t_hr3<-as.numeric(as.character(cor_g_t_hr3))

table0.25$dif_g_t_hr3<-round(abs(table0.25$cor_g_t_hr3-table0.25$cor_hr3_tmpa_avg),2)

table0.25$pixel[which(table0.25$dif_hr==max(table0.25$dif,na.rm=T))]

### diferencia correlacion average y point2pixel de imerg a day ###
cor_day_imerg_avg<-cbind(colnames(gauge025_avg$d),results0.25[[5]]$QPE$COR)
colnames(cor_day_imerg_avg)<-c('pixel','cor_day_imerg_avg')
cor_day_imerg_avg<-as.data.frame(cor_day_imerg_avg)

#completar cada gauge con la informacion del pixel al que pertenece
cor_day_imerg_avg<-cor_day_imerg_avg[match(table0.25$pixel,cor_day_imerg_avg$pixel),2]

table0.25$cor_day_imerg_avg<-as.numeric(as.character(cor_day_imerg_avg))

cor_g_i_day<-cbind(1:ncol(gauges$d),results[[4]]$QPE[,1])
colnames(cor_g_i_day)<-c('gauge','cor_g_i_day')
cor_g_i_day<-as.data.frame(cor_g_i_day)

cor_g_i_day<-cor_g_i_day[match(table0.25$gauge,cor_g_i_day$gauge),2]
table0.25$cor_g_i_day<-as.numeric(as.character(cor_g_i_day))

table0.25$dif_g_i_day<-round(abs(table0.25$cor_g_i_day-table0.25$cor_day_imerg_avg),2)

### diferencia correlacion average y point2pixel de tmpa a day ###
cor_day_tmpa_avg<-cbind(colnames(gauge025_avg$d),results0.25[[6]]$QPE$COR)
colnames(cor_day_tmpa_avg)<-c('pixel','cor_day_tmpa_avg')
cor_day_tmpa_avg<-as.data.frame(cor_day_tmpa_avg)

#completar cada gauge con la informacion del pixel al que pertenece
cor_day_tmpa_avg<-cor_day_tmpa_avg[match(table0.25$pixel,cor_day_tmpa_avg$pixel),2]

table0.25$cor_day_tmpa_avg<-as.numeric(as.character(cor_day_tmpa_avg))

cor_g_t_day<-cbind(1:ncol(gauges$d),results[[5]]$QPE[,1])
colnames(cor_g_t_day)<-c('gauge','cor_g_t_day')
cor_g_t_day<-as.data.frame(cor_g_t_day)

cor_g_t_day<-cor_g_t_day[match(table0.25$gauge,cor_g_t_day$gauge),2]
table0.25$cor_g_t_day<-as.numeric(as.character(cor_g_t_day))

table0.25$dif_g_t_day<-round(abs(table0.25$cor_g_t_day-table0.25$cor_day_tmpa_avg),2)

#save(table0.25,file='table0.25.RData')

#para encontar el pixel con la mayor diferencia
pixel<-c(table0.25$pixel[which(table0.25$dif_g_i_hr3==max(table0.25$dif_g_i_hr3,na.rm=T))],table0.25$pixel[which(table0.25$dif_g_t_hr3==max(table0.25$dif_g_t_hr3,na.rm=T))],table0.25$pixel[which(table0.25$dif_g_i_day==max(table0.25$dif_g_i_day,na.rm=T))],table0.25$pixel[which(table0.25$dif_g_t_day==max(table0.25$dif_g_t_day,na.rm=T))])
gauge<-c(table0.25$gauge[which(table0.25$dif_g_i_hr3==max(table0.25$dif_g_i_hr3,na.rm=T))],table0.25$gauge[which(table0.25$dif_g_t_hr3==max(table0.25$dif_g_t_hr3,na.rm=T))],table0.25$gauge[which(table0.25$dif_g_i_day==max(table0.25$dif_g_i_day,na.rm=T))],table0.25$gauge[which(table0.25$dif_g_t_day==max(table0.25$dif_g_t_day,na.rm=T))])
n_row<-c(which(table0.25$dif_g_i_hr3==max(table0.25$dif_g_i_hr3,na.rm=T)),which(table0.25$dif_g_t_hr3==max(table0.25$dif_g_t_hr3,na.rm=T)),which(table0.25$dif_g_i_day==max(table0.25$dif_g_i_day,na.rm=T)),which(table0.25$dif_g_t_day==max(table0.25$dif_g_t_day,na.rm=T)))

max_dif<- apply(table0.25[,grepl('^dif',colnames(table0.25))],2,function(x) max(x,na.rm=T))

which(table0.25$dif_g_i_day==0.61)

##plots

#3hr TMPA pixel 2037
hr3_174<-gauges$hr3[,174]
hr3_177<-gauges$hr3[,177]
hr3_185<-gauges$hr3[,185]

which(colnames(gauge025_avg$d)==2037)
hr3_133t<-tmpa$hr3[which(index(tmpa$hr3)==index(gauges$hr3)[1]):which(index(tmpa$hr3)==index(gauges$hr3)[nrow(gauges$hr3)]),133]

file='pixel_3hr_2037t_10082016.pdf'
pdf(file=file, width=12, height=6)
plot(hr3_133t[complete.cases(merge(hr3_174,hr3_133t)),],hr3_174[complete.cases(merge(hr3_174,hr3_133t)),],xlab='',ylab='',ylim=c(0,25),xlim=c(0,10),axes='',col="deeppink",pch=16)
abline(lm(hr3_174[complete.cases(merge(hr3_174,hr3_133t)),]~hr3_133t[complete.cases(merge(hr3_174,hr3_133t)),]),col="deeppink")
par(new=T)
plot(hr3_133t[complete.cases(merge(hr3_185,hr3_133t)),],hr3_185[complete.cases(merge(hr3_185,hr3_133t)),],xlab='',ylab='',ylim=c(0,25),xlim=c(0,10),main='pixel 2037',col='darkgoldenrod2',pch=4,cex=1.2)
abline(lm(hr3_185[complete.cases(merge(hr3_185,hr3_133t)),]~hr3_133t[complete.cases(merge(hr3_185,hr3_133t)),]),col='darkgoldenrod2')
par(new=T)
plot(hr3_133t[complete.cases(merge(hr3_177,hr3_133t)),],hr3_177[complete.cases(merge(hr3_177,hr3_133t)),],xlab='TMPA 0.25º/ 3 hr',ylab=expression("gauge 3 hr "*" (mm hr"^{-1}*")"),ylim=c(0,25),xlim=c(0,10),col='green4',pch=8)
abline(lm(hr3_177[complete.cases(merge(hr3_177,hr3_133t)),]~hr3_133t[complete.cases(merge(hr3_177,hr3_133t)),]),col='green4')

legend("topright",pch =c(16,4,8,45),
       col = c("deeppink",'darkgoldenrod2','green4','black'),title="gauge",
       legend = c(colnames(gauges$hr)[c(174,185,177)],'trend line'),cex=0.9,ncol=1,xjust=0.05,x.intersp=0.5)
dev.off()

#3hr IMERG pixel 2037
hr3_133i<-gpm025$hr3[which(index(gpm025$hr3)==index(gauges$hr3)[1]):which(index(gpm025$hr3)==index(gauges$hr3)[nrow(gauges$hr3)]),133]

file='pixel_3hr_2037i_10082016.pdf'
pdf(file=file, width=12, height=6)
plot(hr3_133i[complete.cases(merge(hr3_174,hr3_133i)),],hr3_174[complete.cases(merge(hr3_174,hr3_133i)),],xlab=' ',ylab=' ',xlim=c(0,10),ylim=c(0,25),axes='',col="deeppink",pch=16)
abline(lm(hr3_174[complete.cases(merge(hr3_174,hr3_133i)),]~hr3_133i[complete.cases(merge(hr3_174,hr3_133i)),]),col="deeppink")
par(new=T)
plot(hr3_133i[complete.cases(merge(hr3_185,hr3_133i)),],hr3_185[complete.cases(merge(hr3_185,hr3_133i)),],xlab=' ',ylab=' ',xlim=c(0,10),ylim=c(0,25),main='pixel 2037',col='darkgoldenrod2',pch=4,cex=1.2)
abline(lm(hr3_185[complete.cases(merge(hr3_185,hr3_133i)),]~hr3_133i[complete.cases(merge(hr3_185,hr3_133i)),]),col='darkgoldenrod2')
par(new=T)
plot(hr3_133i[complete.cases(merge(hr3_177,hr3_133i)),],hr3_177[complete.cases(merge(hr3_177,hr3_133i)),],xlab='IMERG 0.25º/ 3 hr',ylab=expression("gauge 3 hr "*" (mm hr"^{-1}*")"),xlim=c(0,10),ylim=c(0,25),col='green4',pch=8)

legend("topright",pch =c(16,4,8,45),
       col = c("deeppink",'darkgoldenrod2','green4','black'),title="gauge",
       legend = c(colnames(gauges$hr)[c(174,185,177)],'trend line'),cex=0.9,ncol=1,xjust=0.05,x.intersp=0.5)
dev.off()

#day TMPA pixel 2037
day_174<-gauges$d[,174]
day_177<-gauges$d[,177]
day_185<-gauges$d[,185]

which(colnames(gauge025_avg$d)==2037)
day_133t<-tmpa$d[which(index(tmpa$d)==index(gauges$d)[1]):which(index(tmpa$d)==index(gauges$d)[nrow(gauges$d)]),133]

file='pixel_d_2037t_10082016.pdf'
pdf(file=file, width=12, height=6)
plot(day_133t[complete.cases(merge(day_174,day_133t)),],day_174[complete.cases(merge(day_174,day_133t)),],xlab='',ylab='',xlim=c(0,2),ylim=c(0,25),axes='',col="deeppink",pch=16)
abline(lm(day_174[complete.cases(merge(day_174,day_133t)),]~day_133t[complete.cases(merge(day_174,day_133t)),]),col="deeppink")
par(new=T)
plot(day_133t[complete.cases(merge(day_185,day_133t)),],day_185[complete.cases(merge(day_185,day_133t)),],xlab='',ylab='',xlim=c(0,2),ylim=c(0,25),main='pixel 2037',col='darkgoldenrod2',pch=4,cex=1.2)
abline(lm(day_185[complete.cases(merge(day_185,day_133t)),]~day_133t[complete.cases(merge(day_185,day_133t)),]),col='darkgoldenrod2')
par(new=T)
plot(day_133t[complete.cases(merge(day_177,day_133t)),],day_177[complete.cases(merge(day_177,day_133t)),],xlab='TMPA 0.25º/ day',ylab=expression("gauge day "*" (mm hr"^{-1}*")"),xlim=c(0,2),ylim=c(0,25),col='green4',pch=8)
legend("topright",pch =c(16,4,8,45),
       col = c("deeppink",'darkgoldenrod2','green4','black'),title="gauge",
       legend = c(colnames(gauges$hr)[c(174,185,177)],'trend line'),cex=0.9,ncol=1,xjust=0.05,x.intersp=0.5)
dev.off()

#day IMERG pixel 2037
day_133i<-gpm025$d[which(index(gpm025$d)==index(gauges$d)[1]):which(index(gpm025$d)==index(gauges$d)[nrow(gauges$d)]),133]

file='pixel_d_2037i_10082016.pdf'
pdf(file=file, width=12, height=6)
plot(day_133i[complete.cases(merge(day_174,day_133i)),],day_174[complete.cases(merge(day_174,day_133i)),],xlab=' ',ylab=' ',xlim=c(0,2),ylim=c(0,25),axes='',col="deeppink",pch=16)
abline(lm(day_174[complete.cases(merge(day_174,day_133i)),]~day_133i[complete.cases(merge(day_174,day_133i)),]),col="deeppink")
par(new=T)
plot(day_133i[complete.cases(merge(day_185,day_133i)),],day_185[complete.cases(merge(day_185,day_133i)),],xlab=' ',ylab=' ',xlim=c(0,2),ylim=c(0,25),main='pixel 2037',col='darkgoldenrod2',pch=4,cex=1.2)
abline(lm(day_185[complete.cases(merge(day_185,day_133i)),]~day_133i[complete.cases(merge(day_185,day_133i)),]),col='darkgoldenrod2')
par(new=T)
plot(day_133i[complete.cases(merge(day_177,day_133i)),],day_177[complete.cases(merge(day_177,day_133i)),],xlab='IMERG 0.25º/ day',ylab=expression("gauge day "*" (mm hr"^{-1}*")"),xlim=c(0,2),ylim=c(0,25),col='green4',pch=8)
legend("topright",pch =c(16,4,8,45),
       col = c("deeppink",'darkgoldenrod2','green4','black'),title="gauge",
       legend = c(colnames(gauges$hr)[c(174,185,177)],'trend line'),cex=0.9,ncol=1,xjust=0.05,x.intersp=0.5)
dev.off()

# 3hr TMPA pixel 559
hr3_250<-gauges$hr3[,250]
hr3_251<-gauges$hr3[,251]

which(colnames(gauge025_avg$hr3)==559)
hr3_23t<-tmpa$hr3[which(index(tmpa$hr3)==index(gauges$hr3)[1]):which(index(tmpa$hr3)==index(gauges$hr3)[nrow(gauges$hr3)]),23]

file='pixel_3hr_559t_10082016.pdf'
pdf(file=file, width=12, height=6)
plot(hr3_23t[complete.cases(merge(hr3_250,hr3_23t)),],hr3_250[complete.cases(merge(hr3_250,hr3_23t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',pch =16,col='deeppink')
abline(lm(hr3_250[complete.cases(merge(hr3_250,hr3_23t)),]~hr3_23t[complete.cases(merge(hr3_250,hr3_23t)),]),col='deeppink')
par(new=T)
plot(hr3_23t[complete.cases(merge(hr3_251,hr3_23t)),],hr3_251[complete.cases(merge(hr3_251,hr3_23t)),],xlab='TMPA 0.25º/ 3 hr',ylab=expression("gauge 3hr "*" (mm hr"^{-1}*")"),col='green4',pch=8, main='pixel 559')
abline(lm(hr3_251[complete.cases(merge(hr3_251,hr3_23t)),]~hr3_23t[complete.cases(merge(hr3_251,hr3_23t)),]),col='green4')
legend("topright",pch =c(16,8,45),
       col = c("deeppink",'green4','black'),title="gauge",
       legend = c(colnames(gauges$hr)[c(250,251)],'trend line'),cex=0.9,ncol=1,xjust=0.05,x.intersp=0.5)
dev.off()

# 3hr IMERG pixel 559
hr3_23i<-gpm025$hr3[which(index(gpm025$hr3)==index(gauges$hr3)[1]):which(index(gpm025$hr3)==index(gauges$hr3)[nrow(gauges$hr3)]),23]

file='pixel_3hr_559i_10082016.pdf'
pdf(file=file, width=12, height=6)
plot(hr3_23i[complete.cases(merge(hr3_250,hr3_23i)),],hr3_250[complete.cases(merge(hr3_250,hr3_23i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',pch =16,col='deeppink')
abline(lm(hr3_250[complete.cases(merge(hr3_250,hr3_23i)),]~hr3_23i[complete.cases(merge(hr3_250,hr3_23i)),]),col='deeppink')
par(new=T)
plot(hr3_23i[complete.cases(merge(hr3_251,hr3_23i)),],hr3_251[complete.cases(merge(hr3_251,hr3_23i)),],xlab='IMERG 0.25º/ 3 hr',ylab=expression("gauge 3 hr "*" (mm hr"^{-1}*")"),col='green4',pch=8, main='pixel 559')
abline(lm(hr3_251[complete.cases(merge(hr3_251,hr3_23i)),]~hr3_23i[complete.cases(merge(hr3_251,hr3_23i)),]),col='green4')
legend("topright",pch =c(16,8,45),
       col = c("deeppink",'green4','black'),title="gauge",
       legend = c(colnames(gauges$hr)[c(250,251)],'trend line'),cex=0.9,ncol=1,xjust=0.05,x.intersp=0.5)
dev.off()


# day TMPA pixel 559
day_250<-gauges$d[,250]
day_251<-gauges$d[,251]

which(colnames(gauge025_avg$d)==559)
day_23t<-tmpa$d[which(index(tmpa$d)==index(gauges$d)[1]):which(index(tmpa$d)==index(gauges$d)[nrow(gauges$d)]),23]

file='pixel_d_559t_10082016.pdf'
pdf(file=file, width=12, height=6)
plot(day_23t[complete.cases(merge(day_250,day_23t)),],day_250[complete.cases(merge(day_250,day_23t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',pch =16,col='deeppink')
abline(lm(day_250[complete.cases(merge(day_250,day_23t)),]~day_23t[complete.cases(merge(day_250,day_23t)),]),col='deeppink')
par(new=T)
plot(day_23t[complete.cases(merge(day_251,day_23t)),],day_251[complete.cases(merge(day_251,day_23t)),],xlab='TMPA 0.25º/ day',ylab=expression("gauge 3 hr "*" (mm hr"^{-1}*")"),col='green4',pch=8, main='pixel 559')
abline(lm(day_251[complete.cases(merge(day_251,day_23t)),]~day_23t[complete.cases(merge(day_251,day_23t)),]),col='green4')
legend("topright",pch =c(16,8,45),
       col = c("deeppink",'green4','black'),title="gauge",
       legend = c(colnames(gauges$hr)[c(250,251)],'trend line'),cex=0.9,ncol=1,xjust=0.05,x.intersp=0.5)
dev.off()

# day IMERG pixel 559
day_23i<-gpm025$d[which(index(gpm025$d)==index(gauges$d)[1]):which(index(gpm025$d)==index(gauges$d)[nrow(gauges$d)]),23]

file='pixel_d_559i_10082016.pdf'
pdf(file=file, width=12, height=6)
plot(day_23i[complete.cases(merge(day_250,day_23i)),],day_250[complete.cases(merge(day_250,day_23i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',pch =16,col='deeppink')
abline(lm(day_250[complete.cases(merge(day_250,day_23i)),]~day_23i[complete.cases(merge(day_250,day_23i)),]),col='deeppink')
par(new=T)
plot(day_23i[complete.cases(merge(day_251,day_23i)),],day_251[complete.cases(merge(day_251,day_23i)),],xlab='IMERG 0.25º/ day',ylab=expression("gauge 3 hr "*" (mm hr"^{-1}*")"),col='green4',pch=8, main='pixel 559')
abline(lm(day_251[complete.cases(merge(day_251,day_23i)),]~day_23i[complete.cases(merge(day_251,day_23i)),]),col='green4')
legend("topright",pch =c(16,8,45),
       col = c("deeppink",'green4','black'),title="gauge",
       legend = c(colnames(gauges$hr)[c(250,251)],'trend line'),cex=0.9,ncol=1,xjust=0.05,x.intersp=0.5)
dev.off()

#3hr TMPA pixel con mas gauges (Papallacta)
hr3_33<-gauges$hr3[,33]
hr3_71<-gauges$hr3[,71]
hr3_76<-gauges$hr3[,76]
hr3_82<-gauges$hr3[,82]
hr3_83<-gauges$hr3[,83]
hr3_84<-gauges$hr3[,84]
hr3_85<-gauges$hr3[,85]
hr3_86<-gauges$hr3[,86]
hr3_87<-gauges$hr3[,87]
hr3_89<-gauges$hr3[,89]

which(colnames(gauge025_avg$hr3)==1167)
hr3_98t<-tmpa$hr3[which(index(tmpa$hr3)==index(gauges$hr3)[1]):which(index(tmpa$hr3)==index(gauges$hr3)[nrow(gauges$hr3)]),98]

file='pixel_3hr_1167t_10082016.pdf'
pdf(file=file, width=12, height=6)
plot(hr3_98t[complete.cases(merge(hr3_33,hr3_98t)),],hr3_33[complete.cases(merge(hr3_33,hr3_98t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',pch =16)
abline(lm(hr3_33[complete.cases(merge(hr3_33,hr3_98t)),]~hr3_98t[complete.cases(merge(hr3_33,hr3_98t)),]))
par(new=T)
plot(hr3_98t[complete.cases(merge(hr3_71,hr3_98t)),],hr3_71[complete.cases(merge(hr3_71,hr3_98t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='red',pch =16)
abline(lm(hr3_71[complete.cases(merge(hr3_71,hr3_98t)),]~hr3_98t[complete.cases(merge(hr3_71,hr3_98t)),]),col='red')
par(new=T)
plot(hr3_98t[complete.cases(merge(hr3_76,hr3_98t)),],hr3_76[complete.cases(merge(hr3_76,hr3_98t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='blue',pch=8,cex=0.7)
abline(lm(hr3_76[complete.cases(merge(hr3_76,hr3_98t)),]~hr3_98t[complete.cases(merge(hr3_76,hr3_98t)),]),col='blue')
par(new=T)
plot(hr3_98t[complete.cases(merge(hr3_82,hr3_98t)),],hr3_82[complete.cases(merge(hr3_82,hr3_98t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='green')
abline(lm(hr3_82[complete.cases(merge(hr3_82,hr3_98t)),]~hr3_98t[complete.cases(merge(hr3_82,hr3_98t)),]),col='green')
par(new=T)
plot(hr3_98t[complete.cases(merge(hr3_83,hr3_98t)),],hr3_83[complete.cases(merge(hr3_83,hr3_98t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='gray',pch=9,cex=1.2)
abline(lm(hr3_83[complete.cases(merge(hr3_83,hr3_98t)),]~hr3_98t[complete.cases(merge(hr3_83,hr3_98t)),]),col='gray')
par(new=T)
plot(hr3_98t[complete.cases(merge(hr3_84,hr3_98t)),],hr3_84[complete.cases(merge(hr3_84,hr3_98t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='cyan',pch=25)
abline(lm(hr3_84[complete.cases(merge(hr3_84,hr3_98t)),]~hr3_98t[complete.cases(merge(hr3_84,hr3_98t)),]),col='cyan')
par(new=T)
plot(hr3_98t[complete.cases(merge(hr3_85,hr3_98t)),],hr3_85[complete.cases(merge(hr3_85,hr3_98t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='darkgoldenrod2',pch=15)
abline(lm(hr3_85[complete.cases(merge(hr3_85,hr3_98t)),]~hr3_98t[complete.cases(merge(hr3_85,hr3_98t)),]),col='darkgoldenrod2')
par(new=T)
plot(hr3_98t[complete.cases(merge(hr3_86,hr3_98t)),],hr3_86[complete.cases(merge(hr3_86,hr3_98t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='deeppink',pch =17)
abline(lm(hr3_86[complete.cases(merge(hr3_86,hr3_98t)),]~hr3_98t[complete.cases(merge(hr3_86,hr3_98t)),]),col='deeppink')
par(new=T)
plot(hr3_98t[complete.cases(merge(hr3_87,hr3_98t)),],hr3_87[complete.cases(merge(hr3_87,hr3_98t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='green4',pch=8)
abline(lm(hr3_87[complete.cases(merge(hr3_87,hr3_98t)),]~hr3_98t[complete.cases(merge(hr3_87,hr3_98t)),]),col='green4')
par(new=T)
plot(hr3_98t[complete.cases(merge(hr3_89,hr3_98t)),],hr3_89[complete.cases(merge(hr3_89,hr3_98t)),],main='pixel 1167',xlab='TMPA 0.25º/ 3 hr',ylab=expression("gauge 3 hr "*" (mm hr"^{-1}*")"),ylim=c(0,2),col='mediumorchid1',pch=4,cex=1.6)
abline(lm(hr3_89[complete.cases(merge(hr3_89,hr3_98t)),]~hr3_98t[complete.cases(merge(hr3_89,hr3_98t)),]),col='mediumorchid1')

legend("topright",pch =c(16,16,8,1,9,25,15,17,8,4,45),
       col = c("black","red","blue","green","gray","cyan","darkgoldenrod2","deeppink",'green4','mediumorchid1','black'),title="gauge",
       legend = c(colnames(gauges$hr)[c(33,71,76,82,83,84,85,86,87,89)],'trend line'),cex=0.9,ncol=1,xjust=0.05,x.intersp=0.5)
dev.off()

#3hr IMERG pixel con mas gauges (Papallacta)
hr3_98i<-gpm025$hr3[which(index(gpm025$hr3)==index(gauges$hr3)[1]):which(index(gpm025$hr3)==index(gauges$hr3)[nrow(gauges$hr3)]),98]

file='pixel_3hr_1167i_10082016.pdf'
pdf(file=file, width=12, height=6)
plot(hr3_98i[complete.cases(merge(hr3_33,hr3_98i)),],hr3_33[complete.cases(merge(hr3_33,hr3_98i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',pch =16)
abline(lm(hr3_33[complete.cases(merge(hr3_33,hr3_98i)),]~hr3_98i[complete.cases(merge(hr3_33,hr3_98i)),]))
par(new=T)
plot(hr3_98i[complete.cases(merge(hr3_71,hr3_98i)),],hr3_71[complete.cases(merge(hr3_71,hr3_98i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='red',pch =16)
abline(lm(hr3_71[complete.cases(merge(hr3_71,hr3_98i)),]~hr3_98i[complete.cases(merge(hr3_71,hr3_98i)),]),col='red')
par(new=T)
plot(hr3_98i[complete.cases(merge(hr3_76,hr3_98i)),],hr3_76[complete.cases(merge(hr3_76,hr3_98i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='blue',pch=8,cex=0.7)
abline(lm(hr3_76[complete.cases(merge(hr3_76,hr3_98i)),]~hr3_98i[complete.cases(merge(hr3_76,hr3_98i)),]),col='blue')
par(new=T)
plot(hr3_98i[complete.cases(merge(hr3_82,hr3_98i)),],hr3_82[complete.cases(merge(hr3_82,hr3_98i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='green')
abline(lm(hr3_82[complete.cases(merge(hr3_82,hr3_98i)),]~hr3_98i[complete.cases(merge(hr3_82,hr3_98i)),]),col='green')
par(new=T)
plot(hr3_98i[complete.cases(merge(hr3_83,hr3_98i)),],hr3_83[complete.cases(merge(hr3_83,hr3_98i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='gray',pch=9,cex=1.2)
abline(lm(hr3_83[complete.cases(merge(hr3_83,hr3_98i)),]~hr3_98i[complete.cases(merge(hr3_83,hr3_98i)),]),col='gray')
par(new=T)
plot(hr3_98i[complete.cases(merge(hr3_84,hr3_98i)),],hr3_84[complete.cases(merge(hr3_84,hr3_98i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='cyan',pch=25)
abline(lm(hr3_84[complete.cases(merge(hr3_84,hr3_98i)),]~hr3_98i[complete.cases(merge(hr3_84,hr3_98i)),]),col='cyan')
par(new=T)
plot(hr3_98i[complete.cases(merge(hr3_85,hr3_98i)),],hr3_85[complete.cases(merge(hr3_85,hr3_98i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='darkgoldenrod2',pch=15)
abline(lm(hr3_85[complete.cases(merge(hr3_85,hr3_98i)),]~hr3_98i[complete.cases(merge(hr3_85,hr3_98i)),]),col='darkgoldenrod2')
par(new=T)
plot(hr3_98i[complete.cases(merge(hr3_86,hr3_98i)),],hr3_86[complete.cases(merge(hr3_86,hr3_98i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='deeppink',pch =17)
abline(lm(hr3_86[complete.cases(merge(hr3_86,hr3_98i)),]~hr3_98i[complete.cases(merge(hr3_86,hr3_98i)),]),col='deeppink')
par(new=T)
plot(hr3_98i[complete.cases(merge(hr3_87,hr3_98i)),],hr3_87[complete.cases(merge(hr3_87,hr3_98i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='green4',pch=8)
abline(lm(hr3_87[complete.cases(merge(hr3_87,hr3_98i)),]~hr3_98i[complete.cases(merge(hr3_87,hr3_98i)),]),col='green4')
par(new=T)
plot(hr3_98i[complete.cases(merge(hr3_89,hr3_98i)),],hr3_89[complete.cases(merge(hr3_89,hr3_98i)),],main='pixel 1167',xlab='IMERG 0.25º/ 3 hr',ylab=expression("gauge 3 hr "*" (mm hr"^{-1}*")"),ylim=c(0,2),col='mediumorchid1',pch=4,cex=1.6)
abline(lm(hr3_89[complete.cases(merge(hr3_89,hr3_98i)),]~hr3_98i[complete.cases(merge(hr3_89,hr3_98i)),]),col='mediumorchid1')

legend("topright",pch =c(16,16,8,1,9,25,15,17,8,4,45),
       col = c("black","red","blue","green","gray","cyan","darkgoldenrod2","deeppink",'green4','mediumorchid1','black'),title="gauge",
       legend = c(colnames(gauges$hr)[c(33,71,76,82,83,84,85,86,87,89)],'trend line'),cex=0.9,ncol=1,xjust=0.05,x.intersp=0.5)
dev.off()

#day TMPA pixel con mas gauges (Papallacta)
day_33<-gauges$d[,33]
day_71<-gauges$d[,71]
day_76<-gauges$d[,76]
day_82<-gauges$d[,82]
day_83<-gauges$d[,83]
day_84<-gauges$d[,84]
day_85<-gauges$d[,85]
day_86<-gauges$d[,86]
day_87<-gauges$d[,87]
day_89<-gauges$d[,89]

which(colnames(gauge025_avg$d)==1167)
day_98t<-tmpa$d[which(index(tmpa$d)==index(gauges$d)[1]):which(index(tmpa$d)==index(gauges$d)[nrow(gauges$d)]),98]

file='pixel_d_1167t_10082016.pdf'
pdf(file=file, width=12, height=6)
plot(day_98t[complete.cases(merge(day_33,day_98t)),],day_33[complete.cases(merge(day_33,day_98t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',pch =16)
abline(lm(day_33[complete.cases(merge(day_33,day_98t)),]~day_98t[complete.cases(merge(day_33,day_98t)),]))
par(new=T)
plot(day_98t[complete.cases(merge(day_71,day_98t)),],day_71[complete.cases(merge(day_71,day_98t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='red',pch =16)
abline(lm(day_71[complete.cases(merge(day_71,day_98t)),]~day_98t[complete.cases(merge(day_71,day_98t)),]),col='red')
par(new=T)
plot(day_98t[complete.cases(merge(day_76,day_98t)),],day_76[complete.cases(merge(day_76,day_98t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='blue',pch=8,cex=0.7)
abline(lm(day_76[complete.cases(merge(day_76,day_98t)),]~day_98t[complete.cases(merge(day_76,day_98t)),]),col='blue')
par(new=T)
plot(day_98t[complete.cases(merge(day_82,day_98t)),],day_82[complete.cases(merge(day_82,day_98t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='green')
abline(lm(day_82[complete.cases(merge(day_82,day_98t)),]~day_98t[complete.cases(merge(day_82,day_98t)),]),col='green')
par(new=T)
plot(day_98t[complete.cases(merge(day_83,day_98t)),],day_83[complete.cases(merge(day_83,day_98t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='gray',pch=9,cex=1.2)
abline(lm(day_83[complete.cases(merge(day_83,day_98t)),]~day_98t[complete.cases(merge(day_83,day_98t)),]),col='gray')
par(new=T)
plot(day_98t[complete.cases(merge(day_84,day_98t)),],day_84[complete.cases(merge(day_84,day_98t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='cyan',pch=25)
abline(lm(day_84[complete.cases(merge(day_84,day_98t)),]~day_98t[complete.cases(merge(day_84,day_98t)),]),col='cyan')
par(new=T)
plot(day_98t[complete.cases(merge(day_85,day_98t)),],day_85[complete.cases(merge(day_85,day_98t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='darkgoldenrod2',pch=15)
abline(lm(day_85[complete.cases(merge(day_85,day_98t)),]~day_98t[complete.cases(merge(day_85,day_98t)),]),col='darkgoldenrod2')
par(new=T)
plot(day_98t[complete.cases(merge(day_86,day_98t)),],day_86[complete.cases(merge(day_86,day_98t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='deeppink',pch =17)
abline(lm(day_86[complete.cases(merge(day_86,day_98t)),]~day_98t[complete.cases(merge(day_86,day_98t)),]),col='deeppink')
par(new=T)
plot(day_98t[complete.cases(merge(day_87,day_98t)),],day_87[complete.cases(merge(day_87,day_98t)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='green4',pch=8)
abline(lm(day_87[complete.cases(merge(day_87,day_98t)),]~day_98t[complete.cases(merge(day_87,day_98t)),]),col='green4')
par(new=T)
plot(day_98t[complete.cases(merge(day_89,day_98t)),],day_89[complete.cases(merge(day_89,day_98t)),],main='pixel 1167',xlab='TMPA 0.25º/ day',ylab=expression("gauge day "*" (mm hr"^{-1}*")"),ylim=c(0,2),col='mediumorchid1',pch=4,cex=1.6)
abline(lm(day_89[complete.cases(merge(day_89,day_98t)),]~day_98t[complete.cases(merge(day_89,day_98t)),]),col='mediumorchid1')

legend("topright",pch =c(16,16,8,1,9,25,15,17,8,4,45),
       col = c("black","red","blue","green","gray","cyan","darkgoldenrod2","deeppink",'green4','mediumorchid1','black'),title="gauge",
       legend = c(colnames(gauges$hr)[c(33,71,76,82,83,84,85,86,87,89)],'trend line'),cex=0.9,ncol=1,xjust=0.05,x.intersp=0.5)
dev.off()

#day IMERG pixel con mas gauges (Papallacta)
day_98i<-gpm025$d[which(index(gpm025$d)==index(gauges$d)[1]):which(index(gpm025$d)==index(gauges$d)[nrow(gauges$d)]),98]

file='pixel_d_1167i_10082016.pdf'
pdf(file=file, width=12, height=6)
plot(day_98i[complete.cases(merge(day_33,day_98i)),],day_33[complete.cases(merge(day_33,day_98i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',pch =16)
abline(lm(day_33[complete.cases(merge(day_33,day_98i)),]~day_98i[complete.cases(merge(day_33,day_98i)),]))
par(new=T)
plot(day_98i[complete.cases(merge(day_71,day_98i)),],day_71[complete.cases(merge(day_71,day_98i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='red',pch =16)
abline(lm(day_71[complete.cases(merge(day_71,day_98i)),]~day_98i[complete.cases(merge(day_71,day_98i)),]),col='red')
par(new=T)
plot(day_98i[complete.cases(merge(day_76,day_98i)),],day_76[complete.cases(merge(day_76,day_98i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='blue',pch=8,cex=0.7)
abline(lm(day_76[complete.cases(merge(day_76,day_98i)),]~day_98i[complete.cases(merge(day_76,day_98i)),]),col='blue')
par(new=T)
plot(day_98i[complete.cases(merge(day_82,day_98i)),],day_82[complete.cases(merge(day_82,day_98i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='green')
abline(lm(day_82[complete.cases(merge(day_82,day_98i)),]~day_98i[complete.cases(merge(day_82,day_98i)),]),col='green')
par(new=T)
plot(day_98i[complete.cases(merge(day_83,day_98i)),],day_83[complete.cases(merge(day_83,day_98i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='gray',pch=9,cex=1.2)
abline(lm(day_83[complete.cases(merge(day_83,day_98i)),]~day_98i[complete.cases(merge(day_83,day_98i)),]),col='gray')
par(new=T)
plot(day_98i[complete.cases(merge(day_84,day_98i)),],day_84[complete.cases(merge(day_84,day_98i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='cyan',pch=25)
abline(lm(day_84[complete.cases(merge(day_84,day_98i)),]~day_98i[complete.cases(merge(day_84,day_98i)),]),col='cyan')
par(new=T)
plot(day_98i[complete.cases(merge(day_85,day_98i)),],day_85[complete.cases(merge(day_85,day_98i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='darkgoldenrod2',pch=15)
abline(lm(day_85[complete.cases(merge(day_85,day_98i)),]~day_98i[complete.cases(merge(day_85,day_98i)),]),col='darkgoldenrod2')
par(new=T)
plot(day_98i[complete.cases(merge(day_86,day_98i)),],day_86[complete.cases(merge(day_86,day_98i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='deeppink',pch =17)
abline(lm(day_86[complete.cases(merge(day_86,day_98i)),]~day_98i[complete.cases(merge(day_86,day_98i)),]),col='deeppink')
par(new=T)
plot(day_98i[complete.cases(merge(day_87,day_98i)),],day_87[complete.cases(merge(day_87,day_98i)),],xlab=' ',ylab=' ',ylim=c(0,2),axes='',col='green4',pch=8)
abline(lm(day_87[complete.cases(merge(day_87,day_98i)),]~day_98i[complete.cases(merge(day_87,day_98i)),]),col='green4')
par(new=T)
plot(day_98i[complete.cases(merge(day_89,day_98i)),],day_89[complete.cases(merge(day_89,day_98i)),],main='pixel 1167',xlab='IMERG 0.25º/ day',ylab=expression("gauge day "*" (mm hr"^{-1}*")"),ylim=c(0,2),col='mediumorchid1',pch=4,cex=1.6)
abline(lm(day_89[complete.cases(merge(day_89,day_98i)),]~day_98i[complete.cases(merge(day_89,day_98i)),]),col='mediumorchid1')

legend("topright",pch =c(16,16,8,1,9,25,15,17,8,4,45),
       col = c("black","red","blue","green","gray","cyan","darkgoldenrod2","deeppink",'green4','mediumorchid1','black'),title="gauge",
       legend = c(colnames(gauges$hr)[c(33,71,76,82,83,84,85,86,87,89)],'trend line'),cex=0.9,ncol=1,xjust=0.05,x.intersp=0.5)
dev.off()