spTrans=function(spobj, from=NA, to='wgs'){
  
  require(rgdal)
  #  require(raster)
  
  if ((is.na(spobj@proj4string@projargs)) & (is.na(from))) stop('No proj4string and no "from" argument defined for spobject.')
  
  switch (from, 
          'rot' = {spobj@proj4string = CRS( '+proj=ob_tran +o_proj=longlat +o_lon_p=-170 +o_lat_p=40 +lon_0=180-m 57.2957795130823')},
          'wgs' = {spobj@proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') },
          'krov' = {spobj@proj4string = CRS('+proj=krovak +lat_0=49.5 +lon_0=24.83333333333333 +alpha=0 +k=0.9999 +x_0=0 +y_0=0 +ellps=bessel +units=m +no_defs') },
          's42' = {spobj@proj4string = CRS('+proj=tmerc +lat_0=0 +lon_0=15 +k=1 +x_0=3500000 +y_0=0 +ellps=krass +units=m +no_defs')}  # +init = epsg:28403
  )
  
  switch (to, 
         
          'rot' = {crs = CRS( '+proj=ob_tran +o_proj=longlat +to_meter=0.0174533 +lon_0=-170 +o_lat_p=40 +o_lon_p=180-m 57.2957795130823+ellps=WGS84 +datum=WGS84 +wktext +no_defs"')},
          'wgs' = {crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') },
          'krov' = {crs = CRS('+proj=krovak +lat_0=49.5 +lon_0=24.83333333333333 +alpha=0 +k=0.9999 +x_0=0 +y_0=0 +ellps=bessel +units=m +no_defs') },
          's42' = {crs = CRS('+proj=tmerc +lat_0=0 +lon_0=15 +k=1 +x_0=3500000 +y_0=0 +ellps=krass +units=m +no_defs')}  
  )
  
  #	res = if (class(spobj) == 'SpatialGridDataFrame') (
  #		projectRaster(raster(spobj), crs=crs, res=attr(spobj@grid, 'cells.dim'))
  #	) else (
  #		spTransform(spobj,crs)
  #		)  
  
  return(spTransform(spobj,crs))
  
}
setwd("~/Plocha/erko")
vp= readShapePoly('vstupol_all')
vp <- readOGR('vstupol_all.shp', 'vstupol_all')
vp@proj4string
vp = spTrans(vp, from = 'krov', to = 'wgs')
plot(vp)

#spplot(r)+layer(sp.polygons(vp, fill=NA, col = 'red'))

setwd("/media/martin/DELL Drive/COSMO/2012070100_pf01_cleps_ml")

TAB = data.table()  
temp = list.files(pattern="*.grd")

####################################################################################################
#overeni souradnicovych systemu
r=raster("H12070106TOT_PREC6h___0D00060000.grd")
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

######################################################################
#setwd("~/Plocha/COSMO/")


r=raster("H12070106TOT_PREC6h___0D00060000.grd")
exmat = extract(r[[1]],vp, na.rm=TRUE, df = TRUE, cellnumbers = TRUE) #vp je polygonova vrstva pro extrakci


setwd("/media/martin/DELL Drive/COSMO")
TAB = list()#data.table()  
TABLE= list()#data.table()


temp <- dir()

for(i in seq_along(temp)){
  setwd(file.path("/media/martin/DELL Drive/COSMO/", temp[[i]])) #nastavit directory i   "~/Plocha/COSMO/"
  dirdat=list.files(pattern=".grd")   #nacte jmena souboru .grd ve vybrane slozce
  pf=as.data.frame(sapply(temp[[i]], strsplit,'_')) ######################################### prcat filu
   time = as.POSIXct(sapply(strsplit(temp[[i]], '_'), function(x)x[1]), format='%Y%m%d%H')
  # progress(i,progress.bar = TRUE, max.value = length(temp))
  cat(i, '\n')
 

  
   for(j in 1: length (dirdat)){ 
   
  ex = extract(raster(dirdat[j]), exmat$cell)  #provede extrakci a nasleduje nastaveni hodnot do data.table
  ex = melt(data.table(OBL = exmat$ID, ex), id.vars = c('OBL'))
  tab = ex[, .(PR = mean(value)), by = .(OBL, variable)]
  ah= as.double(substring(dirdat[j],27,28)) #separace AHEAD hodnoty z nazvu predpovedi
  tab[,AHEAD:= ah]
  tab[, ORIGIN:=time]#[AHEAD]]   #potreba pricist 6h
  tab[,ORIGIN:=ORIGIN+21600]
  
  tab[,TIME:=ORIGIN+(AHEAD*3600)]
  
  tab[, EID:= pf[2,]] #prirazeni cisla clenu ensemblu pf
  
  TAB[[length(TAB)+1]] = tab[, .(OBL, EID, SID = 'PF', ORIGIN,AHEAD,TIME, PR)]
  #TABLE=rbind(TABLE,TAB)
  }

}


costab = rbindlist(TAB)
x=factor(c(costab$OBL))
costab[,OBL:=revalue(x, c("1"="r","2"="s","3"="m","4"="n","5"="h","6"="j","7"="l","8"="o","9"="t","10"="q","11"="u","12"="g","13"="a","14"="b","15"="f","16"="p","17"="k","18"="i","19"="e","20"="d","21"="c","22"="N","23"="G","24"="O","25"="I","26"="P","27"="J","28"="K","29"="H","30"="F","31"="E","32"="D","33"="M","34"="L","35"="A","36"="B","37"="C"))]
saveRDS(costab, 'COSMOtab')

dc = dcast.data.table(costab, OBL + SID + TIME + AHEAD + ORIGIN ~ EID,value.var = "PR") #preklopeni ensemblu do vlastnich sloupcu
dc$SID=NULL

setnames(dc,"AHEAD","AHEADcosmo")
setnames(dc,"ORIGIN","ORIGINcosmo")

mv=melt(dc,id.vars = 1:4)
stat = mv[, .(MEANcosmo = mean(value), MEDIANcosmo = median(value)), keyby = .(OBL, TIME, AHEADcosmo, ORIGINcosmo)]   #vzpocet prumeru a medianu
setkey(stat, OBL, TIME,AHEADcosmo,ORIGINcosmo)
setkey(dc, OBL, TIME, AHEADcosmo, ORIGINcosmo)
dcs=dc[stat]


setkey(dcs, OBL, TIME)
setkey(v,ID,TIME)
vertabcos=v[dcs,nomatch=0]
saveRDS(vertabcos,"vertabcos")

vtcos<-vertabcos[! AHEAD %in% c(24,30,36,42,48,54)] #odstraneni radku s ahead vyssimi nez 18
saveRDS(vtcos,"vertabcos18")

vtc=na.omit(vtcos)     #odstraneni radku s NaN
