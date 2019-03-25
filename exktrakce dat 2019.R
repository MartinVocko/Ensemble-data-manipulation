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

for(i in seq_along(temp)){    #nasmeruje do slozky s daty
  setwd(file.path("~/Plocha/Data_CHMU/Aladin16/", temp[[i]])) #nastavit directory i   "~/Plocha/COSMO/"
  dirdat=list.files(pattern=".nc") #nacte jmena souboru .nc ve vybrane slozce
  leadtime=names((nc_open(dirdat[1])$var)) #nacte promene pro volbu casu predpovedi
  
    for (j in seq_along(dirdat)){    #vybere konkretni file
      
      for (k in 2:(length(leadtime)-1))    #nacte leadtimes od druhe hodnoty (bez teploty ale s  nultou predikci, prvni akumulace 3h odecita tento nulovy raster pro usnadneni cyklu)
      r1=brick(dirdat[j], varname=leadtime[k])
      r2=brick(dirdat[j], varname=leadtime[k+1])    #nacte vrstvy k odecteni sumarizovanych srazek
      rdiff=fun(r1,r2)
      
      soubor_bez_konc <- substr(dirdat[j], start = 1,stop = nchar(dirdat[j])-3)
      hodiny <- substr(x = leadtime[k+1], start = nchar(leadtime[k+1])-1, stop = nchar(leadtime[k+1])-1)
      nove_jmeno <- paste0(soubor_bez_konc, "_0", hodiny, "h.nc")
      
      writeRaster(rdiff, filename = paste0('~/Plocha/Data_CHMU/Aladin16/Aladin16_unsum/',nove_jmeno))
      
      #ulozit odecteny raster do jine slozky a pridat do nazvu leadtime
      
      
      
#pf=as.data.frame(sapply(temp[[i]], strsplit,'_')) ######################################### prcat filu
#  time = as.POSIXct(sapply(strsplit(temp[[i]], '_'), function(x)x[1]), format='%Y%m%d%H')
  # progress(i,progress.bar = TRUE, max.value = length(temp))
  cat(i, '\n')
  }  
}
