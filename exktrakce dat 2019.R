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

setwd("~/Plocha/Data_CHMU/GIS")
vp <- readOGR('basins.shp', 'basins')
vp@proj4string
vp = spTrans(vp, from = 'krov', to = 'wgs')
plot(vp)



list.files("~/Plocha/Data_CHMU/PRECIP/laef/2015", full.names = T)
#overeni souradnicovych systemu
r=raster("/home/vokounm/Plocha/Data_CHMU/PRECIP/laef/2015/laef_pf_2015102512.nc")
projection(r) <- CRS('+proj=ob_tran +o_proj=longlat +o_lon_p=-170 +o_lat_p=40 +lon_0=180-m 57.2957795130823')
plot(r)
newproj<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
pokus <- projectRaster(from = r,
                       crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
plot(pokus)
rw=raster('LAEF4hydro_pf_2012040100.nc')
pr3 <- projectExtent(rw, newproj)
#res(pr3) <- 200000
pokus<-projectRaster(r,pr3)

file<-list.files("/media/martin/DELL Drive/COSMO/2012070100_pf01_cleps_ml", pattern="H12070106TOT")
rwgs = spTrans(r, from = 'rot', to = 'wgs')
ras=brick(file)
names(r$dim)
plot(r)
plot(vp2, add=TRUE,frame.plot=FALSE)
crs(r)
rr=spTrans(r,from=NA, to = "wgs")


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



