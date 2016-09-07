
###PROGRAMA PARA OBTENER EL PROMEDIO DE LAS ESTACIONES###
###QUE ESTAN DENTRO DE UN MISMO PIXEL DE GPM###
rm(list=ls())
gc()

Sys.setenv(TZ='UTC')

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

g_gpm_avg_z<-zoo(gauges_avg[,1:ncol(gauges_avg)],index(gauges$hr))

g_gpm_avg<-list()

g_gpm_avg$hr<-g_gpm_avg_z

# 3hrs
ind_3hrs <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(g_gpm_avg$hr))/(3*60*60))*(3*60*60)
g_gpm_avg$hr3 <- aggregate(g_gpm_avg$hr,ind_3hrs,function(x) round(sum(x)/3,2))
index(g_gpm_avg$hr3) <- index(g_gpm_avg$hr3)-(90*60) # centrar la fecha (-90 min)

# daily
ind_days <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(g_gpm_avg$hr))/(24*60*60))*(24*60*60)
g_gpm_avg$d <- aggregate(g_gpm_avg$hr,ind_days,function(x) round(sum(x)/24,2))
index(g_gpm_avg$d) <- index(g_gpm_avg$d)-(12*60*60) # centrar la fecha (-12 hrs)

gauge01_avg<-g_gpm_avg

#save(gauge01_avg,file="gauge01_avg.RData")


###PROGRAMA PARA OBTENER EL PROMEDIO DE LAS ESTACIONES###
###QUE ESTAN DENTRO DE UN MISMO PIXEL DE TRMM###
rm(list=ls())
gc()

load('gauges.RData')
load ('ID_TRMM.RData')

#unir los codigos de las estaciones con su ID_TRMM
df_trmm<-data.frame(as.character(gauges$sp@data$Estacion),ID_TRMM)

#determinar las estaciones que tienen el mismo ID_TRMM
pixel_est2<-split(as.character(df_trmm[,1]),df_trmm[,2])

#promediar las estaciones que se enceuntran en un mismo pixel
gauges_avg2<-data.frame(1:nrow(gauges$hr))
for (i in 1:length(pixel_est2)){
  a<-data.frame(1:nrow(gauges$hr))
  for (j in 1:length(pixel_est2[[i]])){
    b<-as.data.frame(gauges$hr[,which(colnames(gauges$hr)==pixel_est2[[i]][j])])
    a<-cbind(a,b)
  }
  c<-data.frame(a[,-1])
  if (ncol(c)==1){
    d<-c
  }else{
    d<-data.frame(round(rowMeans(c,na.rm = TRUE),2))
  }
  
  colnames(d)<-names(pixel_est2)[i]
  gauges_avg2<-cbind(gauges_avg2,d)
}
gauges_avg2<-gauges_avg2[,-1]


#para reemplazar NaN por NA,
for (i in 1:ncol(gauges_avg2)){
  gauges_avg2[which(is.nan(gauges_avg2[,i])),i]<-NA
}

g_trmm_avg_z<-zoo(gauges_avg2[,1:ncol(gauges_avg2)],index(gauges$hr))

g_trmm_avg<-list()

g_trmm_avg$hr<-g_trmm_avg_z

# 3hrs
ind_3hrs <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(g_trmm_avg$hr))/(3*60*60))*(3*60*60)
g_trmm_avg$hr3 <- aggregate(g_trmm_avg$hr,ind_3hrs,function(x) round(sum(x)/3,2))
index(g_trmm_avg$hr3) <- index(g_trmm_avg$hr3)-(90*60) # centrar la fecha (-90 min)

# daily
ind_days <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(index(g_trmm_avg$hr))/(24*60*60))*(24*60*60)
g_trmm_avg$d <- aggregate(g_trmm_avg$hr,ind_days,function(x) round(sum(x)/24,2))
index(g_trmm_avg$d) <- index(g_trmm_avg$d)-(12*60*60) # centrar la fecha (-12 hrs)

gauge025_avg<-g_trmm_avg

#save(gauge025_avg,file="gauge025_avg.RData")
