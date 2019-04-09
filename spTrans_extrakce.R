spTrans=function(spobj, from=NA, to='wgs'){
  
  require(rgdal)

  
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
          's42' = {crs = CRS('+proj=tmerc +lat_0=0 +lon_0=15 +k=1 +x_0=3500000 +y_0=0 +ellps=krass +units=m +no_defs')},  
          'wgs2' = {crs= CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 ')}
  )
  
 
  return(spTransform(spobj,crs))
  
}
