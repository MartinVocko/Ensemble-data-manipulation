setwd("~/Plocha/DATA/Aladin_4ext")
fls = dir(pattern = '*grb')
template = 'ncl_convert2nc XXX'

#konverze do nc
for (i in 1:length(fls)){
  exe = paste('ncl_convert2nc', fls[i])
  system(exe,wait=FALSE)
}

####################################################################################################
#extrakce hodnot
setwd("~/Plocha/DATA/Aladin_4ext")

temp = list.files(pattern="*.nc")
TAB = data.table() 

sez = data.table(dir(pattern="nc"))
#sez = sez[1:10000, ]
sez[, V2:= gsub('SURFPREC\\_TOTAL\\_|\\.nc', '', V1)]
sez[, AHEAD:=sapply(strsplit(V2, '_'), function(x)x[1])]
sez[, TIME:=sapply(strsplit(V2, '_'), function(x)x[2])]
  
sez[, DTM:=as.POSIXct(TIME, format = '%Y%m%d%H')]
  
y = raster(temp[i], varname = "g3_lat_0")
x = raster(temp[i], varname = "g3_lon_1")

r = raster('SURFPREC_TOTAL_06_2011040100.nc')
y = raster('SURFPREC_TOTAL_06_2011040100.nc', varname = "g3_lat_0")
x = raster('SURFPREC_TOTAL_06_2011040100.nc', varname = "g3_lon_1")

plat = rasterToPoints(y)
plon = rasterToPoints(x)
lonlat = cbind(plon[, 3], plat[, 3])

ll = SpatialPoints(lonlat)
ex = gIntersects(ll, vp, byid = TRUE)              #intersect a extract hodnot pro polygon
exx = apply(ex, 1, which)

dtms = sez[, unique(DTM)]
dt = dtms[1]
TAB = list()
  
for (j in 1:length(dtms)){
    sez1 = sez[DTM==dtms[j]]
    s = stack(sez[DTM==dtms[j], V1], varname = 'A_PCP_GDS3_HTGL' )
    
    for (i in 1:nrow(sez1)){
    
      r= s[[i]] #raster(temp[i], varname='A_PCP_GDS3_HTGL')
      names(r) = 'Total.precipitation'
#   y = raster(temp[i], varname = "g3_lat_0")
#   x = raster(temp[i], varname = "g3_lon_1")

  #r = raster('SURFPREC_TOTAL_06_2011040100.nc')
  #y = raster('SURFPREC_TOTAL_06_2011040100.nc', varname = "g3_lat_0")
  #x = raster('SURFPREC_TOTAL_06_2011040100.nc', varname = "g3_lon_1")

#   plat = rasterToPoints(y)
#   plon = rasterToPoints(x)
#   lonlat = cbind(plon[, 3], plat[, 3])
# 
#   ll = SpatialPoints(lonlat)
  v = rasterToPoints(r)

  dta = SpatialPointsDataFrame(ll, data.frame(v))
#   ex = gIntersects(dta, vp, byid = TRUE)              #intersect a extract hodnot pro polygon
#   exx = apply(ex, 1, which)
#   
  tab=data.table(DTM = dtms[j], AHEAD = sez1[i,AHEAD], OBL = vp$Id, PR = sapply(exx, function(x) mean(dta[x, ]$Total.precipitation)))

 
 # TAB[[as.character(dtms[j])]] = tab
  TAB[[length(TAB)+1]] = tab
  res = do.call(rbind, TAB)
  #tab[, EID:= 1]
  #TAB = rbind(TAB, tab[, .(OBL, PR,EID, SID = 'cf', TIME, AHEAD)])
}}

#res = do.call(rbind, TAB)
saveRDS(TAB, 'aladin4.RDS')
tab
