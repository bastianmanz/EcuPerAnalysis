# Scripts (online) GPM evaluation Ecuador-Peru

setwd("C:/Bastian/GPM/publication_ecu_peru")

rm(list=ls())
gc()
Sys.setenv(TZ='UTC')

library(xts)
library(sp)
library(rgdal)
library(hydroGOF)
library(gstat)

##################################################
## Combine (Merge) all Ecu. stns from .csv files #
##################################################
setwd("C:/Bastian/GPM/publication_ecu_peru/procesados/")
gauges <- list()

# crear un "dummy" para anadir los series de tiempo
index <- seq(as.POSIXct("2014-04-01 00:00:00",tz="America/Bogota"),as.POSIXct("2015-09-01 00:00:00",tz="America/Bogota"),by="hours")
ecu_z <- zoo(rep(NA,length(index)),index) # objeto zoo vacio

# crear lista de archivos,
# unzip only .csv files from procesados.zip
stns_ecu <-list.files(pattern = "^F")

#leer archivos de csv, convertir en zoo y eliminar fechas invalidas
irregular_timestamps_num <- rep(NA,length(stns_ecu))
irregular_timestamps_id <- list()
duplicated_dates <- list()

for(i in 1:length(stns_ecu)){
  
  print(i)
  
  gauge_i <- read.csv(stns_ecu[i],header=T)
  gauge_i$Fecha<-as.POSIXct(gauge_i$Fecha, format="%Y-%m-%d %H:%M:%S", tz="America/Bogota")
  gauge_i <- zoo(gauge_i$mm,gauge_i$Fecha)
    
  # identificar fechas que no son horas completas
  irregular_timestamps_num[i] <- sum((as.numeric(index(gauge_i))%%3600)>0)
  irregular_timestamps_id[[i]] <- which((as.numeric(index(gauge_i))%%3600)>0)
  
  if(irregular_timestamps_num[i]>0){
    gauge_i <- gauge_i[-irregular_timestamps_id[[i]]]
  }
  end <- length(gauge_i)
  
  # buscar y eliminar fechas duplicadas
  time_difs <- as.numeric(difftime(index(gauge_i)[2:end],index(gauge_i)[1:(end-1)],units="hours"))
  
  if(sum(time_difs<1)>0){
    duplicated_dates[[i]] <- c(which(time_difs==0),which(time_difs==0)+1)
    gauge_i <- gauge_i[-duplicated_dates[[i]]]
  }
  
  # check que no hay fechas duplicadas
#   print(length(gauge_i) - length(unique(index(gauge_i))))
  
  ecu_z <- merge(ecu_z, gauge_i)
  rm(gauge_i)
  
}

# elegir solo el periodo 2014-04-01 hasta 2015-08-31
#  y eliminar el dummy
start <- which(index(ecu_z)==as.POSIXct("2014-04-01 00:00:00",tz="America/Bogota"))
end <- which(index(ecu_z)==as.POSIXct("2015-08-31 23:00:00",tz="America/Bogota"))
ecu_z <- ecu_z[start:end,2:ncol(ecu_z)]

ecu_names <- substr(stns_ecu,2,nchar(stns_ecu)-4)
colnames(ecu_z) <- ecu_names

# para centrar la fecha en el periodo de observacion
index(ecu_z) <- index(ecu_z)-(30*60)
gauges$hr <- ecu_z

# P23, i.e. gauge 64, sp data missing, hence remove
gauges$hr <- gauges$hr[,-64]

# generate spatial information
sp_data <- read.csv("total_estaciones.csv",header=T)
active_stns <- rep(NA,length(colnames(gauges$hr)))
for(i in 1:length(colnames(gauges$hr))){
  active_stns[i] <- which(as.character(sp_data$Estacion)==colnames(gauges$hr)[i])
}
gauges$sp <- sp_data[active_stns,]
coordinates(gauges$sp) <- ~Longitud + Latitud
proj4string(gauges$sp) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

###################################################
# Load IMHEA records, convert to hourly and merge #
###################################################

imhea <- read.csv("iMHEA_30min_20160129.csv",header=F)

# extraer informacion espacial
imhea_sp <- as.data.frame(t(imhea[1:4,2:ncol(imhea)]))
colnames(imhea_sp) <- c("latitud","longitud","elevacion","estacion")
imhea_sp$latitud <- as.numeric(as.character(imhea_sp$latitud))
imhea_sp$longitud <- as.numeric(as.character(imhea_sp$longitud))
imhea_sp$elevacion <- as.numeric(as.character(imhea_sp$elevacion))
imhea_sp$estacion <- as.character(imhea_sp$estacion)
coordinates(imhea_sp) <- c("longitud", "latitud")
proj4string(imhea_sp) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

## extraer informacion temporal y convertir en un objeto de series de tiempo ("zoo" y "xts")
imhea_data <- imhea[5:nrow(imhea),2:ncol(imhea)]
imhea_data <- apply(imhea_data,2,function(x) as.numeric(x))
imhea_z <- zoo(imhea_data,as.POSIXct(as.character(imhea[5:nrow(imhea),1]),tz="America/Bogota"))

# elegir solo el periodo 2014-04-01 hasta 2015-08-31
start <- which(index(imhea_z)==as.POSIXct("2014-04-01 00:00:00",tz="America/Bogota"))
end <- which(index(imhea_z)==as.POSIXct("2015-08-31 23:00:00",tz="America/Bogota"))
imhea_z <- imhea_z[start:end,]

imhea_index_hrly <- strptime("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="America/Bogota") + ceiling(as.numeric(index(imhea_z))/(60*60))*(60*60)
imhea_hrly <- aggregate(imhea_z,imhea_index_hrly,sum)

colnames(imhea_hrly) <- imhea_sp@data$estacion

# para centrar la fecha en el periodo de observacion
index(imhea_hrly) <- index(imhea_hrly)-(30*60) # -30 min

# anadir estaciones "ecu" y "imhea"
gauges$hr <- merge(gauges$hr, imhea_hrly)

# add imhea spatial information
imhea_sp <- sp_data[1:nrow(imhea_sp),]
coordinates(imhea_sp) <- ~Longitud + Latitud
proj4string(imhea_sp) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
gauges$sp <- rbind(gauges$sp,imhea_sp)

##################################
## aggregate to 3-hrly and daily #
##################################

# 3hrs
ind_3hrs <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(gauges$hr))/(3*60*60))*(3*60*60)
gauges$hr3 <- aggregate(gauges$hr,ind_3hrs,sum)
index(gauges$hr3) <- index(gauges$hr3)-(90*60) # centrar la fecha (-90 min)

# daily
ind_days <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(gauges$hr))/(24*60*60))*(24*60*60)
gauges$d <- aggregate(gauges$hr,ind_days,sum)
index(gauges$d) <- index(gauges$d)-(12*60*60) # centrar la fecha (-12 hrs)

###############################################
# ICL server: extract GPM-IMERG Production v3 #
###############################################
# 
# rm(list=ls())
# gc()
# 
# library(rgdal)
# library(xts)
# 
# load("IMERG_EcuPeru.RData")
# load("ID_GPM.RData")
# 
# cal_df <- matrix(data=imerg_cal_ts,ncol=nrow(imerg_grd),nrow=length(time_series),byrow=TRUE)
# cal_z <- as.zoo(cal_df)
# index(cal_z) <- time_series
# 
# cal_z <- cal_z[,ID_GPM]
# 
# ind_hr <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(cal_z))/(60*60))*(60*60)
# ind_3hrs <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(cal_z))/(3*60*60))*(3*60*60)
# ind_day <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(cal_z))/(24*60*60))*(24*60*60)
# 
# cal_hr <- aggregate(cal_z,ind_hr,function(x) sum(x,na.rm=T)/2) # correct rate (mm/hr) to accum (mm/30min) to accum hr (mm)
# index(cal_hr) <- index(cal_hr)-(30*60) # centrar la fecha (-30 min)
# cal_3hrs <- aggregate(cal_z,ind_3hrs,function(x) sum(x,na.rm=T)/2)
# index(cal_3hrs) <- index(cal_3hrs)-(90*60) # centrar la fecha (-90 min)
# cal_day <- aggregate(cal_z,ind_day,function(x) sum(x,na.rm=T)/2)
# index(cal_day) <- index(cal_day)-(12*60*60) # centrar la fecha (-12 hrs)
# 
# gpm <- list()
# gpm$sp <- imerg_grd[ID_GPM,]
# gpm$hr <- cal_hr
# gpm$hr3 <- cal_3hrs
# gpm$d <- cal_day
# 
# # errors
# err_df <- matrix(data=imerg_error_ts,ncol=nrow(imerg_grd),nrow=length(time_series),byrow=TRUE)
# err_z <- as.zoo(err_df)
# index(err_z) <- time_series
# 
# err_z <- err_z[,ID_GPM]
# 
# err_hr <- aggregate(err_z,ind_hr,function(x) sum(x,na.rm=T)/2)
# index(err_hr) <- index(err_hr)-(30*60) # centrar la fecha (-30 min)
# err_3hrs <- aggregate(err_z,ind_3hrs,function(x) sum(x,na.rm=T)/2)
# index(err_3hrs) <- index(err_3hrs)-(90*60) # centrar la fecha (-90 min)
# err_day <- aggregate(err_z,ind_day,function(x) sum(x,na.rm=T)/2)
# index(err_day) <- index(err_day)-(12*60*60) # centrar la fecha (-12 hrs)
# 
# gpm$e.hr <- err_hr
# gpm$e.hr3 <- err_3hrs
# gpm$e.d <- err_day
# 
# save(gpm,file="gpm_stnlocation.RData")

###################################
# prepare GPM-IMERG Production v3 #
###################################

bbox <- c(2,-19,-81.5,-69) # limites de la extension espacial en lat/lon (norte,sur,oeste,este)
north <- bbox[1]
south <- bbox[2]
west <- bbox[3]
east <- bbox[4]

# define global IMERG grid
lat <- seq(-89.95,89.95,by=0.1)
imerg_lat <- lat[which(lat>=south & lat<=north)]
lon <- seq(-179.95,179.95,by=0.1)
imerg_lon <- lon[which(lon>=west & lon<=east)]
imerg_grd <- expand.grid(x = imerg_lon, y = imerg_lat)
colnames(imerg_grd)<-c("lon","lat")
imerg_grd <- SpatialPoints(imerg_grd)
imerg_grd = SpatialPointsDataFrame(imerg_grd, data.frame(ID=c(1:length(imerg_grd))))
proj4string(imerg_grd) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

gpm.sp <- imerg_grd

# colocate at 0.1 deg
ID_GPM <- numeric()
loc_dists <- numeric()

for (i in 1:nrow(gauges$sp)) {
  ID_GPM[i] <- which.min(spDistsN1(gpm.sp,gauges$sp[i,],longlat=TRUE))
  loc_dists[i] <- min(spDistsN1(gpm.sp,gauges$sp[i,] ,longlat=TRUE))
}
ID_GPM[which(loc_dists > 7.77)]=NA # 7.77km; max diagonal of GPM cell
gauges$sp@data <- cbind(gauges$sp@data,ID_GPM)

# save(ID_GPM,file="ID_GPM.RData")

# load
load("gpm_stnlocation.RData")
# names(gpm)

###################################
# prepare TMPA Research Version 7 #
###################################

# load
load("TMPAV7_EcuPeru.RData")
trmm.sp <- trmm$points
coordinates(trmm.sp) <- ~coords.x1 + coords.x2
proj4string(trmm.sp) <- proj4string(gauges$sp)
trmm.3hrs <- trmm$ts

# colocate gauges & TRMM at 0.25 deg
ID_TRMM <- numeric()
loc_dists <- numeric()

for (i in 1:nrow(gauges$sp)) {
  ID_TRMM[i] <- which.min(spDistsN1(trmm.sp,gauges$sp[i,],longlat=TRUE))
  loc_dists[i] <- min(spDistsN1(trmm.sp,gauges$sp[i,] ,longlat=TRUE))
}
ID_TRMM[which(loc_dists > 19.8)]=NA # 19.8km: max diagonal of TMPA cell
# save(ID_TRMM,file="ID_TRMM.RData")
gauges$sp@data <- cbind(gauges$sp@data,ID_TRMM)

tmpa <- list()
tmpa$sp <- trmm.sp[ID_TRMM,]
tmpa$hr3 <- trmm.3hrs[,ID_TRMM]*3 # dates in 3hrly trmm already centred, but need to convert mm/hr to accum. mm/3hrs

# 30min TRMM, time-stamps centred on 3hr window, so resolve to 30min windows to find appropriate daily sum
trmm.3hrs.dm <- trmm.3hrs[,ID_TRMM]
index(trmm.3hrs.dm) <- index(trmm.3hrs.dm) - (60*60)
ind_trmm_30min <- seq(min(index(trmm.3hrs.dm)),max(index(trmm.3hrs.dm)),by="30 min")
dummy_30min <- zoo(rep(NA,length(ind_trmm_30min)),ind_trmm_30min)
trmm.30min <- merge(dummy_30min,trmm.3hrs.dm)
index(trmm.30min) <- index(trmm.30min) - (15*60)
trmm.30min <- na.locf(trmm.30min,maxgap=6)
trmm.30min <- trmm.30min[,-1] # remove dummy

# new 3-hrly, shifted time-stamps to fit gauges,gpm
ind_hr3rev <- strptime("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC") + ceiling(as.numeric(index(trmm.30min))/(3*60*60))*(3*60*60)
tmpa$hr3rev <- aggregate(trmm.30min,ind_hr3rev,function(x) mean(x,na.m=T)) # divide by two, as units in mm/hr
index(tmpa$hr3rev) <- index(tmpa$hr3rev)-(1.5*60*60) # centrar la fecha (-12 hrs)

# daily
ind_days <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(trmm.30min))/(24*60*60))*(24*60*60)
tmpa$d <- aggregate(trmm.30min,ind_days,function(x) mean(x,na.m=T)*24) # divide by two, as units in mm/hr
index(tmpa$d) <- index(tmpa$d)-(12*60*60) # centrar la fecha (-12 hrs)

# save(tmpa,file="tmpa_stnlocation.RData")

load("tmpa_stnlocation.RData")

######################################
# Aggregate GPM 3hrly, daily to 0.25 #
######################################

tmpa025_id <- trmm.sp
tmpa025_id@data[,1] <- 1:nrow(tmpa025_id)

gpm025_id <- idw(ID ~ 1,locations=tmpa025_id,newdata=imerg_grd,idp=0,nmax=1,maxdist=27.57,debug.level=0)[,1]
# save(gpm025_id,file="gpm025_id.RData")

#################
# ICL Server... #
#################
# 
# rm(list=ls())
# gc()
# 
# library(rgdal)
# library(xts)
# 
# load("IMERG_EcuPeru.RData")
# load("gpm025_id.RData")
# 
# cal_df <- matrix(data=imerg_cal_ts,ncol=nrow(imerg_grd),nrow=length(time_series),byrow=TRUE)
# err_df <- matrix(data=imerg_error_ts,ncol=nrow(imerg_grd),nrow=length(time_series),byrow=TRUE)
# 
# cal_df025 <- matrix(data=NA,ncol=nrow(imerg_grd),nrow=length(time_series))
# err_df025 <- matrix(data=NA,ncol=nrow(imerg_grd),nrow=length(time_series))
# for(i in 1:length(unique(gpm025_id@data[,1]))){
#   print(i/length(unique(gpm025_id@data[,1])))
#   cal_df025[,i] <- rowMeans(cal_df[,which(gpm025_id@data[,1]==i)],na.rm=T)
#   err_df025[,i] <- rowMeans(err_df[,which(gpm025_id@data[,1]==i)],na.rm=T)
# }
# 
# cal_z <- as.zoo(cal_df025)
# index(cal_z) <- time_series
# err_z <- as.zoo(err_df025)
# index(err_z) <- time_series
# 
# load("ID_TRMM.RData")
# 
# cal_z <- cal_z[,ID_TRMM]
# err_z <- err_z[,ID_TRMM]
# 
# ind_hr <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(cal_z))/(60*60))*(60*60)
# ind_3hrs <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(cal_z))/(3*60*60))*(3*60*60)
# ind_day <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(cal_z))/(24*60*60))*(24*60*60)
# 
# cal_hr <- aggregate(cal_z,ind_hr,function(x) sum(x,na.rm=T)/2) # correct rate (mm/hr) to accum (mm/30min) to accum hr (mm)
# index(cal_hr) <- index(cal_hr)-(30*60) # centrar la fecha (-30 min)
# cal_3hrs <- aggregate(cal_z,ind_3hrs,function(x) sum(x,na.rm=T)/2)
# index(cal_3hrs) <- index(cal_3hrs)-(90*60) # centrar la fecha (-90 min)
# cal_day <- aggregate(cal_z,ind_day,function(x) sum(x,na.rm=T)/2)
# index(cal_day) <- index(cal_day)-(12*60*60) # centrar la fecha (-12 hrs)
# 
# gpm <- list()
# gpm$sp <- ID_TRMM # ids for trmm cells
# gpm$hr <- cal_hr
# gpm$hr3 <- cal_3hrs
# gpm$d <- cal_day
# 
# # errors
# err_hr <- aggregate(err_z,ind_hr,function(x) sum(x,na.rm=T)/2)
# index(err_hr) <- index(err_hr)-(30*60) # centrar la fecha (-30 min)
# err_3hrs <- aggregate(err_z,ind_3hrs,function(x) sum(x,na.rm=T)/2)
# index(err_3hrs) <- index(err_3hrs)-(90*60) # centrar la fecha (-90 min)
# err_day <- aggregate(err_z,ind_day,function(x) sum(x,na.rm=T)/2)
# index(err_day) <- index(err_day)-(12*60*60) # centrar la fecha (-12 hrs)
# 
# gpm$e.hr <- err_hr
# gpm$e.hr3 <- err_3hrs
# gpm$e.d <- err_day
# gpm025 <- gpm
# 
# save(gpm025,file="gpm025_stnlocation.RData")



###############################################
# Analisis de Deteccion, QPE y Distribuciones #
###############################################

# All available products
# records for each product/ temporal/ spatial resolution are by gauge location
#"e." implies random error as per GPM-IMERG, currently not used in analysis
names(gauges)
names(gpm) # all at scale 0.1 degrees
names(tmpa) # all at scale 0.25 degrees
load("gpm025_stnlocation.RData")
gpm025$sp <- trmm.sp[gpm025$sp,] # gpm is aggregated around TMPA pixels here
names(gpm025) # all at scale 0.25 degrees

# tmpa$hr3 does not have the same time windows as gauges$hr3, gpm025$hr3
g_products <- list(gauges$hr,gauges$hr3,gauges$hr3,gauges$d,gauges$d)
sat_products <- list(gpm$hr,gpm025$hr3,tmpa$hr3rev,gpm025$d,tmpa$d)

# Define range of thresholds for detection stats
thresholds <- seq(0.1,30,0.1)
# Define min. threshold for QPE & Distributions
min_threshold <- 0.1 

# list to store results
results <- list()

for(k in c(1,2,3,4,5)){ # 1:length(g_products)
  
#   # Deteccion 
#   detection <- list()
#   detection$Hits <- matrix(ncol=ncol(g_products[[1]]),nrow=length(thresholds))
#   detection$Miss <- detection$Hits
#   detection$FAs <- detection$Hits
#   detection$CZs <- detection$Hits
#   detection$ACC <- detection$Hits
#   detection$POD <- detection$Hits
#   detection$FAR <- detection$Hits
#   detection$FBI <- detection$Hits
#   detection$ETS <- detection$Hits
#   detection$HSS <- detection$Hits
#   
#   for(i in c(1:ncol(g_products[[k]]))[-54]){
#   
#     gs <- merge(g_products[[k]][,i],sat_products[[k]][,i])
#     gs <- gs[complete.cases(gs),]
#     gauge <- gs[,1]
#     sat <- gs[,2]
# 
#     for(j in 1:length(thresholds)){
#     
#       print(i + j/length(thresholds))
#   
#       hits <- sum(gauge>thresholds[j] & sat>thresholds[j], na.rm=T)
#       miss <- sum(gauge>thresholds[j] & !(sat>thresholds[j]), na.rm=T)
#       fas <- sum(!(gauge>thresholds[j]) & sat>thresholds[j], na.rm=T)
#       czeroes <- sum(gauge<=thresholds[j] & sat<=thresholds[j],na.rm=T)
#   
#       detection$Hits[j,i] <- hits/ sum(!is.na(sat)) # perc_hits
#       detection$Miss[j,i] <- miss/ sum(!is.na(sat)) # perc_miss
#       detection$FAs[j,i] <- fas/ sum(!is.na(sat)) # perc_fas
#       detection$CZs[j,i] <- czeroes/ sum(!is.na(sat)) # perc_czs
#   
#       detection$ACC[j,i] <- (hits + czeroes)/ sum(!is.na(sat)) # ACC
#       detection$POD[j,i] <- hits/(hits + miss) # POD
#       detection$FAR[j,i] <- fas/(hits + fas) # FAR
#       detection$FBI[j,i] <- (hits + fas)/(hits + miss) # FBI
#       He = ((hits + miss)*(hits + fas))/sum(!is.na(sat))
#       detection$ETS[j,i] <- (hits-He)/(hits + fas + miss - He) # ETS
#       detection$HSS[j,i] <- 2*((hits*czeroes)-(fas*miss))/(((hits+miss)*(miss+czeroes)) + ((hits+fas)*(fas+czeroes))) # HSS
#     }
#   }
# 
#   # QPE
#   QPE <- as.data.frame(matrix(ncol=6,nrow=ncol(g_products[[k]])))
#   colnames(QPE) <- c("COR","RMSE","MAE","ME","Bias","NSE")
# 
#   for(i in c(1:ncol(g_products[[k]]))[-54]){
#     
#     print(i/ncol(g_products[[k]]))
#   
#     gs <- merge(g_products[[k]][,i],sat_products[[k]][,i])
#     gs <- gs[complete.cases(gs),]
#     gauge <- gs[,1]
#     sat <- gs[,2]
#   
#     gauge[gauge < min_threshold]=0
#     sat[sat < min_threshold]=0
#       
#     QPE$COR[i] <- cor(gauge,sat,use="na.or.complete")
#     QPE$RMSE[i] <- rmse(sat,gauge, na.rm=TRUE)
#     QPE$MAE[i] <- mae(sat,gauge, na.rm=TRUE)
#     QPE$ME[i] <- me(sat,gauge, na.rm=TRUE)
#     QPE$Bias[i] <- pbias(sat,gauge, na.rm=TRUE)
#     QPE$NSE[i] <- NSE(sat,gauge, na.rm=TRUE)
#   
#   }

  # Occurrence Frequency and Distributions

  occ_frq <- as.data.frame(matrix(data=NA,nrow=ncol(g_products[[k]]),ncol=2))
  colnames(occ_frq) <- c("GAUGE","SAT")

  qts <- seq(0.01,0.99,0.01)

  gauge_Qts <- as.data.frame(matrix(data=NA,ncol=ncol(g_products[[k]]),nrow=length(qts)))
  colnames(gauge_Qts) <- colnames(gauges$z)
  sat_Qts <- gauge_Qts
  Qts_ratio <- gauge_Qts

  for(i in c(1:ncol(g_products[[k]]))[-54]){
    
    print(k + i/ncol(g_products[[k]]))
  
    gs <- merge(g_products[[k]][,i],(sat_products[[k]][,i])/3)
    gs <- gs[complete.cases(gs),]
    if(nrow(gs)>0){
      
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
  
  results[[k]] <- list()
#   results[[k]]$detection <- detection
#   results[[k]]$QPE <- QPE
  results[[k]]$occ_frq <- occ_frq
  results[[k]]$gauges_Qts <- gauge_Qts
  results[[k]]$sat_Qts <- sat_Qts
  results[[k]]$Qts_ratio <- Qts_ratio
}


## TO DO update TMPA to 2015-08 incl.
## check why TMPA daily qts larger than gauges/gpm

### plotting: qts ratio

plot(seq(0.01,0.99,0.01),rep(1,99),type="l",col="white",ylim=c(0,3),
     ylab="Estimated Qts. / Gauge Qts. (Ratio)",
     xlab= "F(x)")
abline(h=1)
lines(seq(0.01,0.99,0.01),rowMeans(results[[1]]$Qts_ratio,na.rm=T),col="red",lwd=2)
lines(seq(0.01,0.99,0.01),rowMeans(results[[2]]$Qts_ratio,na.rm=T),col="red",lty=2)
lines(seq(0.01,0.99,0.01),rowMeans(results[[3]]$Qts_ratio,na.rm=T),col="blue",lty=2)
lines(seq(0.01,0.99,0.01),rowMeans(results[[4]]$Qts_ratio,na.rm=T),col="red",type="o",lty=2)
lines(seq(0.01,0.99,0.01),rowMeans(results[[5]]$Qts_ratio,na.rm=T),type="o",col="blue",lty=2)
legend(x=0,y=0.75, pch = c(15,45,45,1,1),
       col = c("red","red","blue","red","blue"),
       legend = c("GPM-IMERG 0.1/ hr","GPM-IMERG 0.25/ 3-hr","TMPA 0.25/ 3-hr","GPM-IMERG 0.25/ day","TMPA 0.25/ day"),
       bty="n",cex=0.9,ncol=1,xjust=0.05,x.intersp=0.5,lwd="")

### plotting: OF
library(ggplot2)

OFs <- cbind(results[[1]]$occ_frq,
             results[[2]]$occ_frq[,2],
             results[[3]]$occ_frq,
             results[[4]]$occ_frq[,2],
             results[[5]]$occ_frq)
colnames(OFs) <- c("Gauge-hr","IMERG-0.1-hr","IMERG-0.25-3hr","Gauge-3hr",
                   "TMPA-0.25-3hr","IMERG-0.25-day","Gauge-day","IMERG-0.25-day")
OFs <- OFs[,c(1,2,4,3,5,7,6,8)]

OFs_gg <- data.frame(unlist(OFs),
                     c(rep("Gauge",98),
                       rep("IMERG",98),
                       rep("Gauge",98),
                       rep("IMERG",98),
                       rep("TMPA",98),
                       rep("Gauge",98),
                       rep("IMERG",98),
                       rep("TMPA",98)),
                     c(rep("0.1-hr",98),
                       rep("0.1-hr",98),
                       rep("0.25-3hr",98*3),
                       rep("0.25-day",98*3))
                     )
colnames(OFs_gg) <- c("Val","Product","Space.Time")

regional_boxplot <- ggplot(data=OFs_gg, aes(factor(Space.Time), Val, fill=factor(OFs_gg$Product))) + # ,levels=levels(OFs_gg$Product)[c(1,2,3)],ordered=T
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) + 
  scale_x_discrete(breaks=c("0.1-hr", "0.25-3hr", "0.25-day"),labels=c("0.1°/ hr","0.25°/ 3-hr","0.25°/ day")) +
  xlab("Spatial Scale") +
  ylab("Occurrence Frequency (%)") +
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot() +
#   geom_vline(xintercept=c(1.5)) +                        
#   geom_vline(xintercept=c(2.5),linetype="dashed") +
#   geom_vline(xintercept=c(3.5),linetype="dashed") +
#   geom_vline(xintercept=c(4.5),linetype="dashed") +
#   geom_vline(xintercept=c(5.5),linetype="dashed") +
#   geom_vline(xintercept=c(6.5),linetype="dashed") +
#   geom_text(data=N_reg,aes(label=N,y=11), col='black', size=6) +
  #ylim(0,6) +
  #geom_hline(yintercept = 0) +
  # scale_colour_manual(values = c("gold2","indianred2","cornflowerblue")) +
  scale_fill_manual(" ",values = c("gold2","indianred2","cornflowerblue"))

regional_boxplot
