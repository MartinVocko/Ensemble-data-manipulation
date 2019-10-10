sez = data.table(dir())
#sez = sez[1:10000, ]
sez[, V2:= gsub('SURFPREC\\_TOTAL\\_|\\.nc', '', V1)]
sez[, AHEAD:=sapply(strsplit(V2, '_'), function(x)x[1])]
sez[, TIM:=sapply(strsplit(V2, '_'), function(x)x[2])]

sez[, DTM:=as.POSIXct(TIM, format = '%Y%m%d%H')]


dtms = sez[, unique(DTM)]
dt = dtms[1]
for (dt in dtms){
  sez[DTM==dt]
  s = stack(sez[DTM==dt, V1])
  extract
} 