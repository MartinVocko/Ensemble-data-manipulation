#Prevedeni souřadnicového szstemu
spTrans=function(spobj, from=NA, to='wgs'){
  
  require(rgdal)
  #  require(raster)
  
  if ((is.na(spobj@proj4string@projargs)) & (is.na(from))) stop('No proj4string and no "from" argument defined for spobject.')
  
  switch (from, 
          'wgs' = {spobj@proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') },
          'krov' = {spobj@proj4string = CRS('+proj=krovak +lat_0=49.5 +lon_0=24.83333333333333 +alpha=0 +k=0.9999 +x_0=0 +y_0=0 +ellps=bessel +units=m +no_defs') },
          's42' = {spobj@proj4string = CRS('+proj=tmerc +lat_0=0 +lon_0=15 +k=1 +x_0=3500000 +y_0=0 +ellps=krass +units=m +no_defs')}  # +init = epsg:28403)
          
          switch (to,  'wgs' = {crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') },
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

vp= readShapePoly('vstupol_all')
vp = spTrans(vp, from = 'krov', to = 's42')