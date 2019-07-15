library(raster)
library(rNOMADS)
library(rgdal)
library(data.table)
library(maptools)
library(ncdf)
library(ncdf4)
library(lattice)
library(chron)
library(reshape2)
library(plyr)
library(ggplot2)
library(svMisc)

setwd("~/Plocha/Data_CHMU/GIS")
vp <- readOGR('basins.shp', 'basins')
vp@proj4string
vp = spTrans(vp, from = 'wgs_pol', to = 'wgs2')
plot(vp)
plot(vp, add=TRUE)

setwd("~/Plocha/erko")
cr=readOGR('hrcr.shp')
cr=spTrans(cr, from= 'krov', to= 'wgs2')




setwd("~/Plocha/Data_CHMU/PRECIP/laef/2015")
nc=nc_open('laef_pf_2015061000.nc')
leadtime=names(nc$var)


####### LAEF data ze slozky Aladin16 #################################################################################
setwd("~/Plocha/Data_CHMU/Aladin16")
temp <- dir()
fun=function(a,b){b-a}

for(i in 2:seq_along(temp)){    #nasmeruje do slozky s daty
  setwd(file.path("~/Plocha/Data_CHMU/Aladin16/", temp[[i]])) #nastavit directory i   
  dirdat=list.files(pattern=".nc") #nacte jmena souboru .nc ve vybrane slozce
  
  for (j in 1:length(dirdat)){    #vybere konkretni file  seq_along(dirdat)
    nc=nc_open(dirdat[j])
    leadtime=names(nc$var) #nacte promene pro volbu casu predpovedi
    nc_close(nc)
    for (k in 2:(length(leadtime)-2)) {   #nacte leadtimes od druhe hodnoty (bez teploty ale s  nultou predikci, prvni akumulace 3h odecita tento nulovy raster pro usnadneni cyklu)
      
      cfpf=substr(dirdat[j], start = 12, stop = 13)  #vycte z nazvu souboru cf nebo pf pro podminku if
      
      if ( cfpf=="pf"){
        r1=brick(dirdat[j], varname=leadtime[k])
        r2=brick(dirdat[j], varname=leadtime[k+1])    #nacte vrstvy k odecteni sumarizovanych srazek
        rdiff=fun(r1,r2)
      } else {
        r1=raster(dirdat[j], varname=leadtime[k])
        r2=raster(dirdat[j], varname=leadtime[k+1])    #nacte vrstvy k odecteni sumarizovanych srazek
        rdiff=fun(r1,r2)
      }
      
      soubor_bez_konc <- substr(dirdat[j], start = 1,stop = nchar(dirdat[j])-3)
      
      if (nchar(leadtime[k+1])<18){ 
        hodiny <- substr(x = leadtime[k+1], start = nchar(leadtime[k+1])-1, stop = nchar(leadtime[k+1])-1)
      } else { 
        hodiny <- substr(x = leadtime[k+1], start = nchar(leadtime[k+1])-2, stop = nchar(leadtime[k+1])-1)
      }
      
      if (nchar(hodiny) == 1){
        nove_jmeno <- paste0(soubor_bez_konc, "_0", hodiny, "h.nc")
      } else {
        nove_jmeno <- paste0(soubor_bez_konc, "_", hodiny, "h.nc")
      }
      
      
      writeRaster(rdiff, filename = paste0('~/Plocha/Data_CHMU/Aladin16_unsum/',temp[[i]],'/',nove_jmeno), overwrite=TRUE) #ulozit odecteny raster do jine slozky a pridat do nazvu leadtime
      
      
    }
  }  
}


###prevod souradnic polygonu pro vsechny laef



#### LAEF data 2015-2018 do slozky laef_unsum  ###########################################

setwd("~/Plocha/Data_CHMU/PRECIP/laef")
temp <- dir()
fun=function(a,b){b-a}
v1 <- c("acc6h", "acc12h", "acc18h", "acc24h", "acc30h", "acc36h", "acc42h", "acc48h")    
v2 <- c("acc12h", "acc18h", "acc24h", "acc30h", "acc36h", "acc42h", "acc48h", "acc54h")

for(i in seq_along(temp)){    #nasmeruje do slozky s daty
  setwd(file.path("~/Plocha/Data_CHMU/PRECIP/laef/", temp[[i]])) #nastavit directory i   
  dirdat=list.files(pattern=".nc") #nacte jmena souboru .nc ve vybrane slozce
  
  for (j in 1:length(dirdat)){    #vybere konkretni file  seq_along(dirdat)
    nc=nc_open(dirdat[j])
    leadtime=names(nc$var) #nacte promene pro volbu casu predpovedi
    nc_close(nc)
    
    for (k in seq_along(v1)) {   #nacte leadtimes od druhe hodnoty (bez teploty ale s  nultou predikci, prvni akumulace 3h odecita tento nulovy raster pro usnadneni cyklu)
      
      cfpf=substr(dirdat[j], start = 6, stop = 7)  #vycte z nazvu souboru cf nebo pf pro podminku if
      
      if ( cfpf=="pf"){
        r1=brick(dirdat[j], varname = leadtime[grepl(pattern = v1[k], x = leadtime)])  
        r2=brick(dirdat[j], varname = leadtime[grepl(pattern = v2[k], x = leadtime, fixed = T)])  #nacte vrstvy k odecteni sumarizovanych srazek
        rdiff=fun(r1,r2)
      } else {
        r1=raster(dirdat[j], varname = leadtime[grepl(pattern = v1[k], x = leadtime)])  
        r2=raster(dirdat[j], varname = leadtime[grepl(pattern = v2[k], x = leadtime, fixed = T)])    #nacte vrstvy k odecteni sumarizovanych srazek
        rdiff=fun(r1,r2)
      }
      
      soubor_bez_konc <- substr(dirdat[j], start = 1,stop = nchar(dirdat[j])-3)
      
      if (nchar(leadtime[grepl(pattern = v2[k], x = leadtime)])<18){ 
        hodiny <- substr(x = leadtime[grepl(pattern = v2[k], x = leadtime)], start = nchar(leadtime[grepl(pattern = v2[k], x = leadtime)])-1, stop = nchar(leadtime[grepl(pattern = v2[k], x = leadtime)])-1)
      } else { 
        hodiny <- substr(x = leadtime[grepl(pattern = v2[k], x = leadtime)], start = nchar(leadtime[grepl(pattern = v2[k], x = leadtime)])-2, stop = nchar(leadtime[grepl(pattern = v2[k], x = leadtime)])-1)
      }
      
      if (nchar(hodiny) == 1){
        nove_jmeno <- paste0(soubor_bez_konc, "_0", hodiny, "h.nc")
      } else {
        nove_jmeno <- paste0(soubor_bez_konc, "_", hodiny, "h.nc")
      }
      
      
      writeRaster(rdiff, filename = paste0('~/Plocha/Data_CHMU/PRECIP/laef_unsum/',temp[[i]],'/',nove_jmeno), overwrite=TRUE) #ulozit odecteny raster do jine slozky a pridat do nazvu leadtime
      
}  
}
}
  
  

setwd("~/Plocha/Data_CHMU/PRECIP/laef")
temp <- dir()

for(i in seq_along(temp)){    #nasmeruje do slozky s daty
  setwd(file.path("~/Plocha/Data_CHMU/PRECIP/laef/", temp[[i]])) #nastavit directory i   
  dirdat=list.files(pattern=".nc") #nacte jmena souboru .nc ve vybrane slozce
  
  for (j in 1:length(dirdat)){    #vybere konkretni file  seq_along(dirdat)
    nc=nc_open(dirdat[j])
    leadtime=names(nc$var) #nacte promene pro volbu casu predpovedi
    nc_close(nc)
    
         cfpf=substr(dirdat[j], start = 6, stop = 7)  #vycte z nazvu souboru cf nebo pf pro podminku if
      
      if ( cfpf=="pf"){
        r1=brick(dirdat[j], varname = leadtime[grepl(pattern = "acc6h", x = leadtime)])  
        
      } else {
        r1=raster(dirdat[j], varname = leadtime[grepl(pattern = "acc6h", x = leadtime)])  
        
      }
      
      soubor_bez_konc <- substr(dirdat[j], start = 1,stop = nchar(dirdat[j])-3)
      
      if (nchar(leadtime[grepl(pattern = "acc6h", x = leadtime)])<18){ 
        hodiny <- substr(x = leadtime[grepl(pattern = "acc6h", x = leadtime)], start = nchar(leadtime[grepl(pattern = "acc6h", x = leadtime)])-1, stop = nchar(leadtime[grepl(pattern = "acc6h", x = leadtime)])-1)
      } else { 
        hodiny <- substr(x = leadtime[grepl(pattern = "acc6h", x = leadtime)], start = nchar(leadtime[grepl(pattern = "acc6h", x = leadtime)])-2, stop = nchar(leadtime[grepl(pattern = "acc6h", x = leadtime)])-1)
      }
      
      if (nchar(hodiny) == 1){
        nove_jmeno <- paste0(soubor_bez_konc, "_0", hodiny, "h.nc")
      } else {
        nove_jmeno <- paste0(soubor_bez_konc, "_", hodiny, "h.nc")
      }
      
      
      writeRaster(rdiff, filename = paste0('~/Plocha/Data_CHMU/PRECIP/laef_unsum/',temp[[i]],'/',nove_jmeno), overwrite=TRUE) #ulozit odecteny raster do jine slozky a pridat do nazvu leadtime
      
    }  
  }


######### LAEF exktrakce #################################
setwd("~/Plocha/Data_CHMU/Aladin16_unsum/2015")
filez <- list.files()
file.rename(from=filez, to=sub(pattern="LAEF4hydro", replacement="laef", filez))

#________________________________________________________________

setwd("~/Plocha/Data_CHMU/Aladin16_unsum") #pro Vojtu musim nechat, je to dobrej kluk
 
TABLE= data.table()
tablist= list()

temp <- dir()
setwd("~/Plocha/Data_CHMU/GIS")
vp <- readOGR('basins.shp', 'basins')
source("/home/vokounm/Plocha/DATA/GITHUB/Ensemble-data-manipulation/spTrans_extrakce.R")
vp = spTrans(vp, from = 'wgs_pol', to = 'wgs2')



for(i in 7:length(temp)){
  setwd(file.path("~/Plocha/Data_CHMU/Aladin16_unsum/", temp[[i]])) 
  dirdat=list.files(pattern=".nc")  
  TAB_list <- list()
  TAB16_list <- list()

  
  for (p in seq_along(vp$OBJECTID_1)){ 
    vps=vp[vp$OBJECTID_1== p,]  
    
    
        for(j in 1: length (dirdat)){ 
            TAB = data.table(ORIGIN=1) 
            time <- substr(dirdat[j], start = 9,stop = 18)
            TAB[, ORIGIN:=as.POSIXct(time, format = '%Y%m%d%H')]
            ah = as.double(substring(dirdat[j],20,21))
            TAB[,AHEAD:= ah]
            TAB[,TIME:=ORIGIN+(AHEAD*3600)]
            TAB[,ID:= vps$PROFIL]
            
            TABB<-apply(TAB, 1, as.list)
            
            cfpf=substr(dirdat[j], start = 6, stop = 7)
            
            b = brick(dirdat[j])
            
            if (cfpf=="cf"){
              
              ex <- data.frame(value=t((extract(b,vps,fun=mean,df=TRUE)*1000)[1 : nlayers(b)+1])) #extrakce plus prevod jednotek na mm
                                                     #df=TRUE vytvari datatable
              ex$MODEL = "LAEF_CF" 
              TAB=cbind(TAB, ex)
              TAB_list[[j]]<-TAB
             
              } else {
                 
                
                  ex <- data.frame(value=t((extract(b,vps,fun=mean,df=TRUE)*1000)[1 : nlayers(b)+1])) #extrakce plus prevod jednotek na mm 
                  
                  ex$MODEL <- paste0("LAEF_", c(1:nrow(ex)))                                        #df=TRUE vytvari datatable
                  TAB16 <- cbind(TAB, ex)
                 TAB16_list[[j]] <- TAB16
                
                }
            print(paste0("VYSTUP ", j, " POVODI ", p, " ROK ", i))  
        }
    
   
    
   
  
  }
 
  TABLE16 = do.call(rbind,TAB16_list) 
  TABLE = do.call(rbind,TAB_list) 
  
  FIN = rbind(TABLE, TABLE16)
  saveRDS(object = FIN, file = paste0("/home/vokounm/Plocha/Data_CHMU/DATA_EXTRAKCE/LAEF_", i, ".rds"))
  rm(FIN, TABLE16, TABLE)
  gc()
  
  }




##### ALADIN 2011-2015 ##########




setwd("~/Plocha/Data_CHMU/Aladin_4ext")
fun=function(a,b){b-a}
dirdat=list.files(pattern=".nc")
v1=c("_06","_12", "_18", "_24", "30", "_36", "_42", "_48")
v2=c( "_12", "_18", "_24", "_30", "_36", "_42", "_48", "_54")


     for (k in seq_along(v2)) {
    
        
         #r1=stack(dirdat[grepl(pattern = v1[k], x = dirdat)], varname )  
         #r2=stack(dirdat[grepl(pattern = v2[k], x = dirdat)])  #nacte vrstvy k odecteni sumarizovanych srazek
         #rdiff=fun(r1,r2)
       
         d1=dirdat[grepl(pattern = v1[k], x = dirdat)]
         d2=dirdat[grepl(pattern = v2[k], x = dirdat)]
         
         for (m in seq_along(d2)) { 
           
           r1=raster(d1[m], varname="A_PCP_GDS3_HTGL")  
           r2=raster(d2[m], varname="A_PCP_GDS3_HTGL")    #nacte vrstvy k odecteni sumarizovanych srazek
           rdiff=fun(r1,r2)
         
            datum <- substr(d2[m], start = 19,stop = nchar(d2[m])-3)
            nove_jmeno <- paste0("SURFPREC_TOTAL_", datum, v2[k], "h.nc")
            
            writeRaster(rdiff, filename = paste0('~/Plocha/Data_CHMU/Aladin_4ext_unsum/',nove_jmeno), overwrite=TRUE)
        }   
     }

### 6h

dirdat=list.files(pattern="_06")
for (m in seq_along(dirdat)) { 
  
  r=raster(dirdat[m], varname="A_PCP_GDS3_HTGL")
  datum <- substr(dirdat[m], start = 19,stop = nchar(dirdat[m])-3)
  nove_jmeno <- paste0("SURFPREC_TOTAL_", datum, "_06h.nc")
  writeRaster(r, filename = paste0('~/Plocha/Data_CHMU/Aladin_4ext_unsum/',nove_jmeno), overwrite=TRUE)
  
}


### sjednoceni projekce


extent(r)<-c(11.682, 19.508, 47.989, 51.508)

setwd("~/Plocha/Data_CHMU/GIS")
vp <- readOGR('basins.shp', 'basins')

vp = spTrans(vp, from = 'wgs_pol', to = 'wgs2')

###### ALADIN 2015=2018  ###########################
setwd("~/Plocha/Data_CHMU/PRECIP/aladin")
temp <- dir()
fun=function(a,b){b-a}

v1=c("+06.nc","+12.nc", "+18.nc", "+24.nc", "+30.nc", "+36.nc", "+42.nc", "+48.nc")
v2=c( "+12.nc", "+18.nc", "+24.nc", "+30.nc", "+36.nc", "+42.nc", "+48.nc", "+54.nc")

for (i in seq_along(temp)){
     setwd(file.path("~/Plocha/Data_CHMU/PRECIP/aladin/", temp[[i]]))
     dirdat=list.files(pattern=".nc")

      for (k in seq_along(v2)) {
  
     d1=dirdat[grepl(pattern = v1[k], x = dirdat)]
     d2=dirdat[grepl(pattern = v2[k], x = dirdat)]
  
           for (m in seq_along(d2)) { 
    
    r1=raster(d1[m], varname="A_PCP_GDS3_HTGL")  
    r2=raster(d2[m], varname="A_PCP_GDS3_HTGL")    #nacte vrstvy k odecteni sumarizovanych srazek
    rdiff=fun(r1,r2)
    
    datum <- substr(d2[m], start = 16,stop = nchar(d2[m])-6)
    nove_jmeno <- paste0("SURFPREC_TOTAL_", datum,v2[k])
    nove_jmeno <- gsub("\\+", "_",nove_jmeno)
    
    writeRaster(rdiff, filename = paste0('~/Plocha/Data_CHMU/PRECIP/aladin_unsum/',temp[[i]],'/',nove_jmeno), overwrite=TRUE)
  }   
}
}
### 6h
for (i in seq_along(temp)){
  setwd(file.path("~/Plocha/Data_CHMU/PRECIP/aladin/", temp[[i]]))
  dirdat=list.files(pattern="06.nc")
  for (m in seq_along(dirdat)) { 
  
  r=raster(dirdat[m], varname="A_PCP_GDS3_HTGL")
  datum <- substr(dirdat[m], start = 16,stop = nchar(dirdat[m])-6)
  nove_jmeno <- paste0("SURFPREC_TOTAL_", datum, "_06h.nc")

  
  writeRaster(r, filename = paste0('~/Plocha/Data_CHMU/PRECIP/aladin_unsum/',temp[[i]],'/',nove_jmeno), overwrite=TRUE)
} 
}



##### ALADIN extrakce ######
setwd("~/Plocha/Data_CHMU/PRECIP/aladin_unsum")
temp=dir()


for (i in seq_along(temp)){
  setwd(file.path("~/Plocha/Data_CHMU/PRECIP/aladin_unsum/", temp[[i]])) 
   files=grep(list.files(path=file.path("~/Plocha/Data_CHMU/PRECIP/aladin_unsum/", temp[[i]])), pattern='06h.nc', inv=T, value=T)
   file.rename(from=files, to=sub(pattern=".nc", replacement="h.nc", files))
  
  }



#//////////////////////////////////////////////////////////////////////////////////////////////////

 

TABLE= data.table()
tablist= list()

dirdat <- dir()
setwd("~/Plocha/Data_CHMU/GIS")
vp <- readOGR('basins.shp', 'basins')
source("/home/vokounm/Plocha/DATA/GITHUB/Ensemble-data-manipulation/spTrans_extrakce.R")
vp = spTrans(vp, from = 'wgs_pol', to = 'wgs2')


setwd("~/Plocha/Data_CHMU/Aladin_4ext_unsum")

TAB_list <- list()
TAB16_list <- list()


for (p in seq_along(vp$OBJECTID_1)){ 
  vps=vp[vp$OBJECTID_1== p,]  
  
  
  for(j in 1: length (dirdat)){ 
    TAB = data.table(ORIGIN=1) 
    time <- substr(dirdat[j], start = 16,stop = 25)
    TAB[, ORIGIN:=as.POSIXct(time, format = '%Y%m%d%H')]
    ah = as.double(substring(dirdat[j],27,28))
    TAB[,AHEAD:= ah]
    TAB[,TIME:=ORIGIN+(AHEAD*3600)]
    TAB[,ID:= vps$PROFIL]
    
    TABB<-apply(TAB, 1, as.list)
    
    
    b = brick(dirdat[j])
    bb <- extent(11.682, 19.508, 47.989, 51.508)
    extent(b) <- bb
    b <- setExtent(b, bb, keepres=TRUE)
    
    ex <- data.frame(value=t(extract(b,vps,fun=mean,df=TRUE)[1 : nlayers(b)+1])) #extrakce plus prevod jednotek na mm
    #df=TRUE vytvari datatable
    ex$MODEL = "ALADIN-CZ" 
    TAB=cbind(TAB, ex)
    TAB_list[[j]]<-TAB
    
    print(paste0("VYSTUP ", j, " POVODI ", p))  
  }
  
  
  
  
  
}


TABLE = do.call(rbind,TAB_list) 

saveRDS(object = TABLE, file = paste0("/home/vokounm/Plocha/Data_CHMU/DATA_EXTRAKCE/ALADIN_CZ.rds"))


##### Merge CZ RAD ######

setwd("~/Plocha/Data_CHMU/merge_ascii/2016")
temp=dir()
  for (i in 223:length(temp)){ 
    setwd(file.path("~/Plocha/Data_CHMU/merge_ascii/2016",temp[[i]]))
    dirdat=list.files()
    
    for (j in seq_along(dirdat)){ 
      r=raster(dirdat[[j]])
      name=names(r)
      newname=paste0(name, ".nc")
      writeRaster(r, filename = paste0("~/Plocha/Data_CHMU/merge/2016/",newname),overwrite=TRUE)
    
    }
  }



setwd("~/Plocha/Data_CHMU/merge")
temp=dir()
sumtime=c("_6h.nc", "_12h.nc","_18h.nc", "_24h.nc")
for (i in seq_along(temp)){ 
  setwd(file.path("~/Plocha/Data_CHMU/merge/",temp[[i]]))
  dirdat=list.files()
  datum <- substr(dirdat, start = 16,stop = nchar(dirdat[1])-11)      
  datum <- unique.POSIXlt(datum)
   
         
          for (m in seq_along(datum) ){ 
            hours=list.files(pattern=(datum[[m]]))
            dayprc= stack(hours)
            indices<-rep(1:4, each=6)
            sum6<-stackApply(dayprc, indices, fun = sum)
            
          #  x=sum(dayprc[[13:18]])
            
                for (s in seq_along(sumtime)){
                for (t in 1:4){ 
            
            writeRaster(sum6[[t]], filename = paste0("~/Plocha/Data_CHMU/merge_6h/", temp[[i]], '/', datum[[m]], sumtime[[s]]), overwrite=TRUE)
            
                }
          }  
}
}


### sjednoceni projekce

setwd("~/Plocha/Data_CHMU/GIS")
vp <- readOGR('basins.shp', 'basins')
vp = spTrans(vp, from = 'wgs_pol', to = 'wgs2')

setwd("~/Plocha/erko")
cr=readOGR('hrcr.shp')
cr=spTrans(cr, from= 'krov', to= 'wgs2')
cr2=spTransform(cr, CRS('+init=epsg:32633'))

setwd("~/Plocha/Data_CHMU/merge_6h/2018")
r=raster('20180921_24h.nc')
r@crs = CRS('+init=epsg:32633')

vp2 = spTransform(vp, CRS('+init=epsg:32633'))

vps=vp2[vp2$OBJECTID_1=='1',]
plot(vps)
plot(r,add=TRUE)
plot(vps,add=TRUE)

ex=extract(r,vps,fun=mean,df=TRUE)

#extrakce v cyklu pro merge od roku 2015   ######### DOLADIT NA MERGE 

setwd("~/Plocha/Data_CHMU/merge_6h")
temp <- dir()
TAB_list <- list()
TAB_list2 <- list()

for(i in 1:length(temp)){
  setwd(file.path("~/Plocha/Data_CHMU/merge_6h/", temp[[i]])) 
  dirdat=list.files(pattern=".nc")  



for (p in seq_along(vp$OBJECTID_1)){ 
  vps=vp2[vp2$OBJECTID_1== p,]  
  
  
  for(j in 1: length (dirdat)){ 
    TAB = data.table(TIME=1) 
    time <- substr(dirdat[j], start = 1,stop = 11)
    time=gsub("\\_","",time)
    TAB[, TIME:=as.POSIXct(time, format = '%Y%m%d%H')]
    TAB[,ID:= vps$PROFIL]
    
    TABB<-apply(TAB, 1, as.list)
    
    
    b = brick(dirdat[j])
  
    ex <- data.frame(value=t(extract(b,vps,fun=mean,df=TRUE)[1 : nlayers(b)+1])) #extrakce plus prevod jednotek na mm
 
    ex$MODEL = "RAINFALL" 
    TAB=cbind(TAB, ex)
    #TABsum=rbind(TABsum, TAB)
    TAB_list[[j]]<-TAB
    
    print(paste0("VYSTUP ", j, " POVODI ", p, " ROK ", i))  
  }
  TABLE = do.call(rbind,TAB_list) 
  TAB_list2[[p]]<-TABLE



  
} 
  TABLE2 = do.call(rbind,TAB_list2) 
  saveRDS(object = TABLE2, file = paste0("/home/vokounm/Plocha/Data_CHMU/DATA_EXTRAKCE/merge", temp[[i]],".rds"))
}





#### cyklus pro merge data v letech 2011 - 2014

setwd("~/Plocha/Data_CHMU/CZRAD_date")
temp=dir()
TAB=data.frame()
TABLE=data.frame()

for ( i in 1:(length(temp)-2)){
  
  b=brick(temp[i])
  b@crs = CRS('+init=epsg:32633')
  
  
  for (p in seq_along(vp$OBJECTID_1)){ 
    vps=vp2[vp2$OBJECTID_1== p,]         #vp2 nacist ve sjednoceni projekce
  
      ex=as.data.table(extract(b,vps,fun=mean,df=TRUE) )
      ex=melt(ex)
      ex= ex[2:length(ex$variable),]
      time <- substr(names(b), start = 2, stop=12)
      time=paste0(time,"06")
      time=gsub("\\.2506","12",time)
      time=gsub("\\.506","18",time)
      time=gsub("\\.7506","24",time)
      time=as.POSIXct(time, format = '%Y%m%d%H', tz="UTC")
      startime=substr(b@title, start=20, stop=27)
      startime=paste0(startime,"00")
      startime=as.POSIXct(startime, format = '%Y%m%d%H', tz="UTC")
      t=as.numeric(difftime(startime,time[1], units=("hours")))
      time=time+((t[1]+6)*3600)                               #+6h jelikoz nacteny startime je od 00h a ne 06h
      ex[, TIME:=time]
      
      
      ex$variable=NULL
      ex$MODEL="RAINFALL"
     
      ex[,ID:= vps$PROFIL]
      
      TAB=rbind(TAB, ex)
      
  }    
  
  TABLE=rbind(TABLE,TAB)
  
  }

saveRDS(object = TABLE, file = paste0("/home/vokounm/Plocha/Data_CHMU/DATA_EXTRAKCE/merge2011-2014.rds"))


 ### COSMO extrakce #####


setwd("~/Plocha/Data_CHMU/GIS")
vp <- readOGR('basins.shp', 'basins')
source("/home/vokounm/Plocha/DATA/GITHUB/Ensemble-data-manipulation/spTrans_extrakce.R")
vp = spTrans(vp, from = 'wgs_pol', to = 'wgs2')  #spatna prjekce


setwd("~/Plocha/Data_CHMU/COSMO_LEPS")
TABLE= data.table()
tablist= list()

temp <- dir()

for (i in length(temp)){ 
  setwd(file.path("~/Plocha/Data_CHMU/COSMO_LEPS/", temp[[i]]))
  dirdat=dir()
  
  for (k in seqalong(dirdat)){  
    setwd(file.path("~/Plocha/Data_CHMU/COSMO_LEPS/", temp[[i]], dirdat[[k]]))
    dirdat2=dir()

for (p in seq_along(vp$OBJECTID_1)){ 
  vps=vp[vp$OBJECTID_1== p,]  
  
  
  for(j in 1: length (dirdat2)){ 
    setwd(file.path("~/Plocha/Data_CHMU/COSMO_LEPS/", temp[[i]], dirdat[[k]], dirdat2[[j]]))
    agr=dir(pattern="PREC6h")  
    
    
    for(l in 1:length(agr)){ 
    
    
    TAB = data.table(ORIGIN=1) 
    time <- substr(dirdat2[j], start = 1,stop = 10)
    TAB[, ORIGIN:=as.POSIXct(time, format = '%Y%m%d%H')]
    ah = 6*l
    TAB[,AHEAD:= ah]
    TAB[,TIME:=ORIGIN+(AHEAD*3600)]
   # TAB[,ID:= vps$PROFIL]
    
    TABB<-apply(TAB, 1, as.list)
    
    
    b = brick(agr[l])
    #bb <- extent(11.682, 19.508, 47.989, 51.508)
   # bb<-extent(-1.625, 4.0625 , 0.1875, 4)
    bb<-extent(10.8401, 19.9304, 47.9968, 51.3391)
  #  bb<-extent(0.5625, 6.1875 , -2.0000 ,1.7500 )
    extent(b) <- bb
    b <- setExtent(b, bb, keepres=TRUE)
    profiles=vp$PROFIL
    ex <- data.frame(value=t(extract(b,vp,fun=mean,df=TRUE)[1 : nlayers(b)+1])) #extrakce plus prevod jednotek na mm
    #df=TRUE vytvari datatable
    names(ex) <- vp$PROFIL
    ex$MODEL <- paste0("COSMO_", substr(dirdat2[j], start = 14,stop = 15))    
    TAB=cbind(TAB, ex)
    TABmelt=melt(TAB, id=c(1:3,12))
    TAB_list[[j]]<-TAB
    
    print(paste0("VYSTUP ", j, " POVODI ", p))  
  }
  
  }
} 
  
  }  
}


TABLE = do.call(rbind,TAB_list) 

saveRDS(object = TABLE, file = paste0("/home/vokounm/Plocha/Data_CHMU/DATA_EXTRAKCE/ALADIN_CZ.rds"))



