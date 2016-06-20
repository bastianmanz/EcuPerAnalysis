
##############################
#### Gauge Pre-processing ####
##############################

###PROGRAMA PARA REALIZAR CONTROL DE DATOS OBSERVADOS###
###CON EL METODO DE EXTREME VALUES CHECK (SHEN 2010)###
rm(list=ls())
gc()

library(xts)

###Estaciones Ecuador###
#guardar los archivos .csv de INAMHI con M, como respaldo de los originales
for (i in 1:length(list.files(pattern='^FM'))){
  temp<-read.csv(list.files(pattern='^FM')[i])
  write.csv(temp,paste(substring(list.files(pattern='^FM')[i],2,6),'.csv',sep=''))
}

#abrir la informacion a nivel diario de las estaciones de INAMHI
inamhi_dia<-read.csv('Pdia_INAMHI.csv', skip=2, header=T)

#obtener el valor maximo diario de precipitacion para cada mes de informacion disponible
pdm<-cbind(as.character(inamhi_dia$codigo),inamhi_dia$mes,apply(inamhi_dia[,4:length(inamhi_dia)],1,function(x) max(x,na.rm=T)))
colnames(pdm)<-c('codigo','mes','pdia')
pdm<-as.data.frame(pdm)

#para resumir en un data frame el valor máximo diario de cada mes de cada estacion
valores<-as.data.frame(matrix(ncol=13))
colnames(valores)<-c('codigo',1:12)

est_depurar<-substring(list.files(pattern='^M'),1,5)

for (i in 1:length(est_depurar)){
  prueba<-subset(pdm,pdm$codigo==est_depurar[i])
  prue<-as.data.frame(tapply(as.numeric(as.character(prueba$pdia)),prueba$mes, function(x) max(x,na.rm=T)))
  prue<-prue[c(2,7,8,9,10,11,12,13,14,3,4,5),]
  prue<-c(est_depurar[i],prue)
  valores<-rbind(valores,prue)
}
valores<-valores[-1,]
Pmax_dia<-cbind(valores$codigo,as.data.frame(matrix(as.numeric(as.matrix(valores[,2:13])),ncol=12)))
#Pmax_dia<-cbind(Pmax_dia,rowSums(Pmax_dia[,2:13])) #revision visual con isoyetas

#incuir manualmente para estaciones con NA los valores de una estacion cercana que se conozca su comportamiento
Pmax_dia[44,2:13]<-Pmax_dia[43,2:13]
Pmax_dia[51,2:13]<-Pmax_dia[11,2:13]
Pmax_dia[52,2:13]<-Pmax_dia[11,2:13]
Pmax_dia[53,2:13]<-Pmax_dia[11,2:13]
Pmax_dia[48,2:13]<-Pmax_dia[28,2:13]
Pmax_dia[50,2:13]<-Pmax_dia[28,2:13]

##comparar los valores horarios con los máximos diarios, si el horario supera al maximo diario reemplazarlo por NA
#elaborar una tabla donde se resume el porcentaje de datos rechazados
tabla_resumen<-data.frame(est_depurar,NA)
colnames(tabla_resumen)<-c('estacion', '%rejected')
for (j in 1:length(est_depurar)){
  b<-data.frame()
  a<-read.csv(list.files(pattern='^M')[j])
  a$Fecha<-as.POSIXct(a$Fecha, tz='America/Bogota')
  for (k in 1:12){
    temporal<-subset(a,as.numeric(format(a$Fecha,"%m"))==k)
    temporal$mm[temporal$mm>Pmax_dia[j,k+1]]<-NA
    b<-rbind(b,temporal)
  }
  c<-zoo(b$mm,b$Fecha)
  c<-cbind(index(c),as.data.frame(c))
  colnames(c)<-c('Fecha','mm')
  #reemplazar el archivo .csv si se realizo algun cambio
  if (sum(is.na(a$mm))!=sum(is.na(b$mm))){
    write.csv(c,paste('F',est_depurar[j],'.csv',sep=''),row.names=F)
  }
  #aumentar un resumen de porcentaje de cambios
  tabla_resumen[j,2]<-round((sum(is.na(b$mm))-sum(is.na(a$mm)))/nrow(a)*100,2)
}

#write.csv(tabla_resumen,'tabla_resumen.csv',row.names=F)

###Estaciones Peru###
rm(list=ls())
gc()

#abrir la informacion a divel diario de las estaciones de SENAMHI
senamhi_dia<-read.csv('Pdia_SENAMHI.csv', header=T)

#arreglar los nombres de estaciones, reemplazar _ por espacio
colnames(senamhi_dia)<-gsub('_',' ',colnames(senamhi_dia))

#abrir .csv con los codigos de las estaciones con informacion horaria (DatosHorarios-Peru_original.csv)
senamhi_est<-read.csv("est_senamhi.csv",header=F,stringsAsFactors=FALSE)

#abrir .csv donde constan el codigo y nombre de todas las estaciones
senamhi_nom<-read.csv('listEstaciones_Peru.csv',header=T,stringsAsFactors=FALSE)

#recortar solo estaciones con informacion diaria disponible
senamhi_nom<-senamhi_nom[na.omit(match(senamhi_est[,1],senamhi_nom$Estacion)),]

senamhi_d<-senamhi_dia[,c(na.omit(match(senamhi_nom$NOMBRE,colnames(senamhi_dia))))]

#cambiar los nombres de las estaciones por los codigos
colnames(senamhi_d)<-senamhi_nom[which(!is.na(match(senamhi_nom$NOMBRE,colnames(senamhi_dia)))),1]
#poner columna fecha en data frame senamhi_d
Fecha<-as.Date(as.character(senamhi_dia$Fecha))
senamhi_d<-cbind(Fecha,senamhi_d)

#para resumir en un data frame el valor máximo diario de cada mes de cada estacion disponible
valores<-as.data.frame(matrix(ncol=13))
for (i in 2:ncol(senamhi_d)){
  temp<-tapply(as.numeric(as.character(senamhi_d[,i])),format(senamhi_d$Fecha,'%m'),function(x) max(x,na.rm=T))
  temp<-c(colnames(senamhi_d)[i],temp)
  valores<-rbind(valores,temp)
}
valores<-valores[-1,]

#eliminar estaciones que no tienen informacion
for (i in 1:nrow(valores)){
  if(all(is.infinite(as.numeric(valores[i,2:13])))==T){
    valores<-valores[-i,]
  }
}

##comparar los valores horarios con los máximos diarios, si el horario supera al maximo diario reemplazarlo por NA
#elaborar una tabla donde se resume el porcentaje de datos rechazados

tabla_resumen_senamhi<-data.frame(valores[,1],NA)
colnames(tabla_resumen_senamhi)<-c('estacion', '%rejected')

#abrir valores horarios
horarios_senamhi<-read.csv('DatosHorarios-Peru_original.csv',header=F, skip=1,stringsAsFactors=FALSE)
colnames(horarios_senamhi)<-c('Fecha',senamhi_est[,1])
horarios_senamhi$Fecha<-as.POSIXct(horarios_senamhi$Fecha, format='%d/%m/%Y %H:%M', tz='America/Bogota')

f<-1:nrow(horarios_senamhi)
for (j in 1:nrow(valores)){
  #columna de data frame horarios_senamhi donde se ecuentra la estacion de valores
  a<-which(valores[j,1]==colnames(horarios_senamhi))
  b<-data.frame()
  fecha<-data.frame()
  
  for (k in 1:12){
    print(paste(j,k,sep='-'))
    #separar valores por meses
    temporal<-subset(horarios_senamhi[,a],as.numeric(format(horarios_senamhi$Fecha,"%m"))==k)
    
    temporal[temporal>valores[j,k+1]]<-NA
    temporal2<-subset(horarios_senamhi[,1],as.numeric(format(horarios_senamhi$Fecha,"%m"))==k)
    fecha<-rbind(fecha,as.data.frame(temporal2))
    c<-as.data.frame(temporal)
    colnames(c)<-valores[j,1]
    b<-rbind(b,c)
  }
  f<-cbind(f,b)
  
  #aumentar un resumen de porcentaje de cambios
  tabla_resumen_senamhi[j,2]<-round((sum(is.na(b))-sum(is.na(horarios_senamhi[,a])))/nrow(horarios_senamhi)*100,2)
}

#write.csv(tabla_resumen_senamhi,'tabla_resumen_senamhi.csv',row.names=F)

#data frame Fechas y valores diarios de precipitacion corregidos
d<-horarios_senamhi[,1]
g<-cbind(d,f)
g<-g[,-2]
colnames(g)[1]<-'Fecha'

#para ordernar las fechas se transforma a zoo
e<-zoo(g[,2:ncol(g)],as.POSIXct(g[,1]))

#unir las estaciones con reemplazos NA con las estaciones donde no se hizo extreme values check
horarios_senamhi_final<-cbind(horarios_senamhi[,-match(valores[,1],colnames(horarios_senamhi))],as.data.frame(e))

#ordenar las columnas segun el data frame inicial
horarios_senamhi_final1<-horarios_senamhi_final[,order(match(colnames(horarios_senamhi_final),colnames(horarios_senamhi)))]

#reemplazar el archivo .csv si se realizo algun cambio
#write.csv(horarios_senamhi_final1,"DatosHorarios-Peru.csv",row.names=F)

###PROGRAMA PARA REALIZAR gauges.RData###
# Scripts (online) GPM evaluation Ecuador-Peru

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
start <- which(index(ecu_z)==as.POSIXct("2014-04-01 00:00:00", tz='America/Bogota'))
end <- which(index(ecu_z)==as.POSIXct("2015-08-31 23:00:00", tz='America/Bogota'))
ecu_z <- ecu_z[start:end,2:ncol(ecu_z)]

ecu_names <- substr(stns_ecu,2,nchar(stns_ecu)-4)
colnames(ecu_z) <- ecu_names

# para centrar la fecha en el periodo de observacion
index(ecu_z) <- index(ecu_z)-(30*60)
gauges$hr <- ecu_z

# generate spatial information
sp_data <- read.csv("estaciones_epmaps_inamhi.csv",header=T)
active_stns <- rep(NA,length(colnames(gauges$hr)))
for(i in 1:length(colnames(gauges$hr))){
  active_stns[i] <- which(as.character(sp_data$Estacion)==colnames(gauges$hr)[i])
}

gauges$sp <- sp_data[active_stns,]
coordinates(gauges$sp) <- ~longitud + latitud
proj4string(gauges$sp) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

###################################################
# Load IMHEA records, convert to hourly and merge #
###################################################

imhea <- read.csv("iMHEA_30min_TB_withgaps.csv",header=F)

# extraer informacion espacial
imhea_sp <- as.data.frame(t(imhea[1:4,2:ncol(imhea)]))
colnames(imhea_sp) <- c("latitud","longitud","Altura","Estacion")
imhea_sp$latitud <- as.numeric(as.character(imhea_sp$latitud))
imhea_sp$longitud <- as.numeric(as.character(imhea_sp$longitud))
imhea_sp$Altura <- as.numeric(as.character(imhea_sp$Altura))
imhea_sp$Estacion <- as.character(imhea_sp$Estacion)
coordinates(imhea_sp) <- c("longitud", "latitud")
proj4string(imhea_sp) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

## extraer informacion temporal y convertir en un objeto de series de tiempo ("zoo" y "xts")
imhea_data <- imhea[5:nrow(imhea),2:ncol(imhea)]
imhea_data <- apply(imhea_data,2,function(x) as.numeric(x))
imhea_z <- zoo(imhea_data,as.POSIXct(as.character(imhea[5:nrow(imhea),1]),format= "%d/%m/%Y %H:%M",tz="America/Bogota"))

# elegir solo el periodo 2014-04-01 hasta 2015-08-31
start <- which(index(imhea_z)==as.POSIXct("2014-04-01 00:00:00",tz="America/Bogota"))
end <- which(index(imhea_z)==as.POSIXct("2015-08-31 23:00:00",tz="America/Bogota"))
imhea_z <- imhea_z[start:end,]

imhea_index_hrly <- strptime("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="America/Bogota") + ceiling(as.numeric(index(imhea_z))/(60*60))*(60*60)
imhea_hrly <- aggregate(imhea_z,imhea_index_hrly,sum)
index(imhea_hrly)<-as.POSIXct(format(time(imhea_hrly)),tz='UTC')

colnames(imhea_hrly) <- imhea_sp@data$Estacion

# para centrar la fecha en el periodo de observacion
index(imhea_hrly) <- index(imhea_hrly)-(30*60) # -30 min

# anadir estaciones "ecu" y "imhea"
gauges$hr <- merge(gauges$hr, imhea_hrly)

# add imhea spatial information
gauges$sp <- rbind(gauges$sp,imhea_sp)

###################################################
# Load SENAMHI records, convert to hourly and merge #
###################################################

senamhi <- read.csv("DatosHorarios-Peru.csv",header=F, skip=1,stringsAsFactors=FALSE)
estaciones_senamhi<- read.csv("est_senamhi.csv",header=F,stringsAsFactors=FALSE)

colnames(senamhi)<-c('Fecha',estaciones_senamhi[,1])

## extraer informacion temporal y convertir en un objeto de series de tiempo ("zoo" y "xts")

senamhi_data <- senamhi[,2:ncol(senamhi)]
senamhi_data <- apply(senamhi_data,2,function(x) as.numeric(x))
senamhi_z <- zoo(senamhi_data,as.POSIXct(as.character(senamhi[,1]),format= "%Y-%m-%d %H:%M",tz="America/Bogota"))

# elegir solo el periodo 2014-04-01 hasta 2015-08-31
start <- which(index(senamhi_z)==as.POSIXct("2014-04-01 00:00:00",tz="America/Bogota"))
end <- which(index(senamhi_z)==as.POSIXct("2015-08-31 23:00:00",tz="America/Bogota"))
senamhi_z <- senamhi_z[start:end,]


peru_z <- zoo(rep(NA,length(index)),index) # objeto zoo vacio
# buscar y eliminar fechas duplicadas
time_difs <- na.omit(as.numeric(difftime(index(senamhi_z)[2:end],index(senamhi_z)[1:(end-1)],units="hours")))

if(sum(time_difs<1)>0){
  duplicated_dates<- c(which(time_difs==0),which(time_difs==0)+1)
  senamhi_z<- senamhi_z[-duplicated_dates,]
}
peru_z<-merge(peru_z,senamhi_z)
peru_z<-peru_z[,-1]

senamhi_hrly<-peru_z

# para centrar la fecha en el periodo de observacion
index(senamhi_hrly) <- index(senamhi_hrly)-(30*60) # -30 min

# anadir estaciones "ecu" y "imhea"
gauges$hr <- merge(gauges$hr, senamhi_hrly)

# extraer informacion espacial
senamhi_sp<-read.csv('listEstaciones_Peru.csv',header=T,stringsAsFactors=FALSE)

senamhi_sp <- cbind(senamhi_sp$latitud,senamhi_sp$longitud,senamhi_sp$Altura,senamhi_sp$Estacion)
senamhi_sp<-as.data.frame(senamhi_sp)
colnames(senamhi_sp) <- c("latitud","longitud","Altura","Estacion")
senamhi_sp$latitud <- as.numeric(as.character(senamhi_sp$latitud))
senamhi_sp$longitud <- as.numeric(as.character(senamhi_sp$longitud))
senamhi_sp$Altura <- as.numeric(as.character(senamhi_sp$Altura))

#ordenar las estaciones de senamhi_sp de acuerdo al orden en el que aparecen en las columnas de senamhi_hrly
senamhi_sp<-senamhi_sp[order(match(as.character(senamhi_sp[,4]),colnames(senamhi_hrly))),]
senamhi_sp<-senamhi_sp[which(!is.na(match(senamhi_sp$Estacion,colnames(senamhi_hrly)))),]

coordinates(senamhi_sp) <- ~longitud + latitud
proj4string(senamhi_sp) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

#anadir estaciones senamhi
gauges$sp <- rbind(gauges$sp,senamhi_sp)

prueba<-gauges$sp[order(match(gauges$sp$Estacion,colnames(gauges$hr))),]

#eliminar estaciones que no tienen informacion
delete<-numeric()
for (i in 1:ncol(gauges$hr)){
  del<-numeric()
  if(all(is.na(as.numeric(gauges$hr[,i])))==T){
    del<-i
  }
  delete<-c(delete,del)
}

gauges$sp<-gauges$sp[-delete,]
gauges$hr<-gauges$hr[,-delete]

##################################
## aggregate to 3-hrly and daily #
##################################

# 3hrs
ind_3hrs <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(gauges$hr))/(3*60*60))*(3*60*60)
gauges$hr3 <- aggregate(gauges$hr,ind_3hrs,function(x) round(sum(x)/3,2))
index(gauges$hr3) <- index(gauges$hr3)-(90*60) # centrar la fecha (-90 min)

# daily
ind_days <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(gauges$hr))/(24*60*60))*(24*60*60)
gauges$d <- aggregate(gauges$hr,ind_days,function(x) round(sum(x)/24,2))
index(gauges$d) <- index(gauges$d)-(12*60*60) # centrar la fecha (-12 hrs)

#save(gauges,file="gauges.RData")

###################################
# prepare GPM-IMERG Production v3 #
###################################

# load("gauges.RData)

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

save(ID_GPM,file="ID_GPM.RData")

###############################################
# ICL server: extract GPM-IMERG Production v3 #
###############################################

rm(list=ls())
gc()

library(rgdal)
library(xts)

load("IMERG_EcuPeru.RData")
load("ID_GPM.RData")

cal_df <- matrix(data=imerg_cal_ts,ncol=nrow(imerg_grd),nrow=length(time_series),byrow=TRUE)
cal_z <- as.zoo(cal_df)
index(cal_z) <- time_series

cal_z <- cal_z[,ID_GPM]

ind_hr <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(cal_z))/(60*60))*(60*60)
ind_3hrs <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(cal_z))/(3*60*60))*(3*60*60)
ind_day <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(cal_z))/(24*60*60))*(24*60*60)

cal_hr <- aggregate(cal_z,ind_hr,function(x) mean(x,na.rm=T)) # mean rain rate over 1hr period (in units of mm/hr)
index(cal_hr) <- index(cal_hr)-(30*60) # centrar la fecha (-30 min)
cal_3hrs <- aggregate(cal_z,ind_3hrs,function(x) mean(x,na.rm=T)) # mean rain rate over 3hrs period (in units of mm/hr)
index(cal_3hrs) <- index(cal_3hrs)-(90*60) # centrar la fecha (-90 min)
cal_day <- aggregate(cal_z,ind_day,function(x) mean(x,na.rm=T)) # mean rain rate over 1 day period (in units of mm/hr)
index(cal_day) <- index(cal_day)-(12*60*60) # centrar la fecha (-12 hrs)

gpm <- list()
gpm$sp <- imerg_grd[ID_GPM,]
gpm$hr <- cal_hr
gpm$hr3 <- cal_3hrs
gpm$d <- cal_day

# # errors - we're not really considering these at this stage...
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

save(gpm,file="gpm_stnlocation.RData")


###################################
# prepare TMPA Research Version 7 #
###################################

# load
load("TMPAV7_EcuPeru_1998-2015.RData")
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
save(ID_TRMM,file="ID_TRMM.RData")
gauges$sp@data <- cbind(gauges$sp@data,ID_TRMM)

tmpa <- list()
tmpa$sp <- trmm.sp[ID_TRMM,]

trmm.3hrs.dm <- trmm.3hrs[,ID_TRMM] # TRMM provides rainfall rates estimates (in mm/hr)
index(trmm.3hrs.dm) <- index(trmm.3hrs.dm) - (75*60) # move start time from 00:00 to 22:45, i.e. centre of first 30min window (22;30 - 23:00)

# create 30-min index and zoo object
ind.trmm.30min.start <- min(index(trmm.3hrs.dm)) - (75*60) # move start time from 00:00 to 22:45, i.e. centre of first 30min window (22;30 - 23:00)
ind.trmm.30min.end <- max(index(trmm.3hrs.dm)) + (75*60) # move start time from 21:00 to 22:15, i.e. centre of last 30min window (22:00 - 22:30)
ind.trmm.30min <- seq(ind.trmm.30min.start,ind.trmm.30min.end,by="30 min")
trmm.30min <- apply(trmm.3hrs.dm,2,function(x) rep(x,each=6)) # rep each value 6x times (6* 30min = 3hrs)
trmm.30min <- as.xts(zoo(trmm.30min,ind.trmm.30min))

# hrly with same time windows as gauges, GPM-IMERG
ind_hrs <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(trmm.30min))/(1*60*60))*(1*60*60)
tmpa$hr <- aggregate(trmm.30min,ind_hrs,function(x) mean(x,na.rm=T)) # mean rain rate every hour in units of mm/hr
index(tmpa$hr) <- index(tmpa$hr)-(0.5*60*60) # centrar la fecha (-30 min)

# 3-hrly with same time windows as gauges, GPM-IMERG
ind_3hrs <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(trmm.30min))/(3*60*60))*(3*60*60)
tmpa$hr3 <- aggregate(trmm.30min,ind_3hrs,function(x) mean(x,na.rm=T)) # mean rain rate every 3 hours in units of mm/hr
index(tmpa$hr3) <- index(tmpa$hr3)-(1.5*60*60) # centrar la fecha (-1.5 hrs)

# daily with same time windows as gauges, GPM-IMERG
ind_days <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(trmm.30min))/(24*60*60))*(24*60*60)
tmpa$d <- aggregate(trmm.30min,ind_days,function(x) mean(x,na.rm=T)) # mean rain rate every day in units of mm/hr
index(tmpa$d) <- index(tmpa$d)-(12*60*60) # centrar la fecha (-12 hrs)

save(tmpa,file="tmpa_stnlocation.RData")

######################################
# Aggregate GPM 3hrly, daily to 0.25 #
######################################

tmpa025_id <- trmm.sp
tmpa025_id@data[,1] <- 1:nrow(tmpa025_id)

gpm025_id <- idw(ID ~ 1,locations=tmpa025_id,newdata=imerg_grd,idp=0,nmax=1,maxdist=27.57,debug.level=0)[,1]
save(gpm025_id,file="gpm025_id.RData")

#################
# ICL Server... #
#################

rm(list=ls())
gc()

library(rgdal)
library(xts)

load("IMERG_EcuPeru.RData")
load("gpm025_id.RData")

cal_df <- matrix(data=imerg_cal_ts,ncol=nrow(imerg_grd),nrow=length(time_series),byrow=TRUE)
# err_df <- matrix(data=imerg_error_ts,ncol=nrow(imerg_grd),nrow=length(time_series),byrow=TRUE)

cal_df025 <- matrix(data=NA,ncol=nrow(imerg_grd),nrow=length(time_series))
# err_df025 <- matrix(data=NA,ncol=nrow(imerg_grd),nrow=length(time_series))
for(i in 1:length(unique(gpm025_id@data[,1]))){
  print(i/length(unique(gpm025_id@data[,1])))
  cal_df025[,i] <- rowMeans(cal_df[,which(gpm025_id@data[,1]==i)],na.rm=T)
#   err_df025[,i] <- rowMeans(err_df[,which(gpm025_id@data[,1]==i)],na.rm=T)
}

cal_z <- as.zoo(cal_df025)
index(cal_z) <- time_series
# err_z <- as.zoo(err_df025)
# index(err_z) <- time_series

load("ID_TRMM.RData")

cal_z <- cal_z[,ID_TRMM]
# err_z <- err_z[,ID_TRMM]

ind_hr <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(cal_z))/(60*60))*(60*60)
ind_3hrs <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(cal_z))/(3*60*60))*(3*60*60)
ind_day <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(cal_z))/(24*60*60))*(24*60*60)

cal_hr <- aggregate(cal_z,ind_hr,function(x) mean(x,na.rm=T)) # average rain rate in units of mm/hr
index(cal_hr) <- index(cal_hr)-(30*60) # centrar la fecha (-30 min)
cal_3hrs <- aggregate(cal_z,ind_3hrs,function(x) mean(x,na.rm=T)) # average rain rate in units of mm/hr
index(cal_3hrs) <- index(cal_3hrs)-(90*60) # centrar la fecha (-90 min)
cal_day <- aggregate(cal_z,ind_day,function(x) mean(x,na.rm=T)) # average rain rate in units of mm/hr
index(cal_day) <- index(cal_day)-(12*60*60) # centrar la fecha (-12 hrs)

gpm025 <- list()
gpm025$sp <- ID_TRMM # ids for trmm cells
gpm025$hr <- cal_hr
gpm025$hr3 <- cal_3hrs
gpm025$d <- cal_day

# # errors
# err_hr <- aggregate(err_z,ind_hr,function(x) sum(x,na.rm=T)/2)
# index(err_hr) <- index(err_hr)-(30*60) # centrar la fecha (-30 min)
# err_3hrs <- aggregate(err_z,ind_3hrs,function(x) sum(x,na.rm=T)/2)
# index(err_3hrs) <- index(err_3hrs)-(90*60) # centrar la fecha (-90 min)
# err_day <- aggregate(err_z,ind_day,function(x) sum(x,na.rm=T)/2)
# index(err_day) <- index(err_day)-(12*60*60) # centrar la fecha (-12 hrs)
# 
# gpm025$e.hr <- err_hr
# gpm025$e.hr3 <- err_3hrs
# gpm025$e.d <- err_day

save(gpm025,file="gpm025_stnlocation.RData")


###############################################
# Analisis de Deteccion, QPE y Distribuciones #
###############################################

# All available products
# records for each product/ temporal/ spatial resolution are by gauge location
#"e." implies random error as per GPM-IMERG, currently not used in analysis
load("gauges.RData")
names(gauges)
load("gpm_stnlocation.RData")
names(gpm) # all at scale 0.1 degrees
load("tmpa_stnlocation.RData")
names(tmpa) # all at scale 0.25 degrees
load("gpm025_stnlocation.RData",verbose = T)
#gpm025$sp <- trmm.sp[gpm025$sp,] # gpm is aggregated around TMPA pixels here
names(gpm025) # all at scale 0.25 degrees

g_products <- list(gauges$hr,gauges$hr3,gauges$hr3,gauges$d,gauges$d)
sat_products <- list(gpm$hr,gpm025$hr3,tmpa$hr3,gpm025$d,tmpa$d)

#Identificar precipitacion maxima
for (n in 1:ncol(gauges$hr)){
  print(paste(n,colnames(gauges$hr)[n],max(gauges$hr[,n], na.rm=T),sep='-'))
}

# Define range of thresholds for detection stats
thresholds <- seq(0.1,30,0.1)
# Define min. threshold for QPE & Distributions
min_threshold <- 0.1 

# list to store results
results <- list()

for(k in 1:length(g_products)){
  
  print(Sys.time())
  
  # Deteccion 
  detection <- list()
  detection$Hits <- matrix(ncol=ncol(g_products[[1]]),nrow=length(thresholds))
  detection$Miss <- detection$Hits
  detection$FAs <- detection$Hits
  detection$CZs <- detection$Hits
  detection$ACC <- detection$Hits
  detection$POD <- detection$Hits
  detection$FAR <- detection$Hits
  detection$FBI <- detection$Hits
  detection$ETS <- detection$Hits
  detection$HSS <- detection$Hits
  
  for(i in c(1:ncol(g_products[[k]]))){
    
    gs <- merge(g_products[[k]][,i],sat_products[[k]][,i])
    gs <- gs[complete.cases(gs),]
    if (nrow(gs)==0){
      next
    } else{
      gauge <- gs[,1]
      sat <- gs[,2]      
        
    for(j in 1:length(thresholds)){
      
      #print(i + j/length(thresholds))
      
      hits <- sum(gauge>thresholds[j] & sat>thresholds[j], na.rm=T)
      miss <- sum(gauge>thresholds[j] & !(sat>thresholds[j]), na.rm=T)
      fas <- sum(!(gauge>thresholds[j]) & sat>thresholds[j], na.rm=T)
      czeroes <- sum(gauge<=thresholds[j] & sat<=thresholds[j],na.rm=T)
      
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
  QPE <- as.data.frame(matrix(ncol=6,nrow=ncol(g_products[[k]])))
  colnames(QPE) <- c("COR","RMSE","MAE","ME","Bias","NSE")
  
  for(i in 1:ncol(g_products[[k]])){
    
    gs <- merge(g_products[[k]][,i],sat_products[[k]][,i])
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
  # Occurrence Frequency and Distributions
  
  occ_frq <- as.data.frame(matrix(data=NA,nrow=ncol(g_products[[k]]),ncol=2))
  colnames(occ_frq) <- c("GAUGE","SAT")
  
  qts <- seq(0.01,0.99,0.01)
  
  gauge_Qts <- as.data.frame(matrix(data=NA,ncol=ncol(g_products[[k]]),nrow=length(qts)))
  colnames(gauge_Qts) <- colnames(gauges$z)
  sat_Qts <- gauge_Qts
  Qts_ratio <- gauge_Qts
  
  for(i in 1:ncol(g_products[[k]])){
    
    gs <- merge(g_products[[k]][,i],sat_products[[k]][,i])
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
  
  results[[k]] <- list()
  results[[k]]$detection <- detection
  results[[k]]$QPE <- QPE
  results[[k]]$gauges_Qts <- gauge_Qts
  results[[k]]$sat_Qts <- sat_Qts
  results[[k]]$Qts_ratio <- Qts_ratio
  results[[k]]$occ_frq <- occ_frq
}




###PROGRAMA PARA OBTENER EL PROMEDIO DE LAS ESTACIONES###
###QUE ESTAN DENTRO DE UN MISMO PIXEL DE GPM###
rm(list=ls())
gc()

load('gauges.RData')
load ('ID_GPM.RData')

#unir los codigos de las estaciones con su ID_GPM
df_gpm<-data.frame(as.character(gauges$sp@data$Estacion),ID_GPM)

#determinar las estaciones que tienen el mismo ID_GPM
pixel_est<-split(as.character(df_gpm[,1]),df_gpm[,2])

#promediar las estaciones que se enceuntran en un mismo pixel
gauges_avg<-data.frame(1:nrow(gauges$hr))
for (i in 1:length(pixel_est)){
  a<-data.frame(1:nrow(gauges$hr))
  for (j in 1:length(pixel_est[[i]])){
    b<-as.data.frame(gauges$hr[,which(colnames(gauges$hr)==pixel_est[[i]][j])])
    a<-cbind(a,b)
                 
  }
  c<-data.frame(a[,-1])
  if (ncol(c)==1){
    d<-c
  }else{
    d<-data.frame(round(rowMeans(c,na.rm = TRUE),2))
  }
  
  colnames(d)<-names(pixel_est)[i]
  gauges_avg<-cbind(gauges_avg,d)
}
gauges_avg<-gauges_avg[,-1]

#para reemplazar NaN por NA,
for (i in 1:ncol(gauges_avg)){
  gauges_avg[which(is.nan(gauges_avg[,i])),i]<-NA
}
