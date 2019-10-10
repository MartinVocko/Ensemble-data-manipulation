#r=brick('2013_12h.nc')
#plot(r[[which(r@z[[1]]==20130714)]])


#nacteni polygonu v ruznych souradnicovych systemech (potreba nacist fci spTrans)
setwd("~/Plocha/erko")
pol= readShapePoly('vstupol_all')
vp = spTrans(pol, from = 'krov', to = 's42')
vpw= spTrans(pol, from = 'krov', to = 'wgs')

#nacteni mereni a predpovedi pro vybranou udalost  
#CZRAD
#stack()
#OBS=brick('all_obs') #obs data z diplomky
setwd("~/Plocha/DATA/OBS")
OBS=brick('2013_6_6h.nc')
#b = OBS[[1:54]]
b = OBS[[3:10]] #merena data od 1.6. 12:00 (prvni data=suma 12-18h)

#b1=sum(b[[1:6]])
#b2=sum(b[[7:12]])
#b3=sum(b[[13:18]])
#b4=sum(b[[19:24]])
#b5=sum(b[[25:30]])
#b6=sum(b[[31:36]])
#b7=sum(b[[37:42]])
#b8=sum(b[[43:48]])
#b9=sum(b[[49:54]])

#b6h=brick(c(b1,b2,b3,b4,b5,b6,b7,b8,b9))  # 6h sumy neelegantne

obsum=sum(b)
#sum2=sum(b6h)
(ext=extract(obsum,vp2[7,],na.rm=TRUE, df = TRUE)

OBS@crs = CRS('+init=epsg:32633')
vp2 = spTransform(vp, CRS('+init=epsg:32633'))
#nacteni d
ID=names(b)
dID = data.table(ID)
d=(readRDS('key.rds')[12:60])
#d=readRDS('key.rds')
dID[, LEAD:=as.integer(substr(gsub('\\.','-',gsub('X', '', ID)), 12, 13))]
setkey(d, BASE, LEAD) 
setkey(dID, BASE, LEAD)
#d1 = d[dID]

#vykresleni casoveho prubehu udalosti
m = extract(b, vp2[7,], fun=mean,na.rm=TRUE, df = TRUE)


dta = data.table(LEAD = d[, LEAD],BASE = d[, (BASE)], t(m[, 2:ncol(m)]))
dta = data.table(melt(dta, id.vars=c('LEAD', 'BASE')))
dta = dta[1:8]
dta[,LEAD:=LEAD-11]
dta[, cLEAD:= (LEAD) * 6]
dta[, dtmLEAD:= BASE + (12+cLEAD) * 60 * 60]
ggplot(dta) + geom_line(aes(x = LEAD, y = value)) + facet_grid(variable~BASE)
pobs=ggplot(dta, aes(x = (dtmLEAD), y = cumsum(value), col = 'OBS'))+geom_line()

#ALADIN
setwd("~/Plocha/DATA/ALADIN/2013")
AL=brick("LAEF4hydro_cf_2013060112.nc", varname="tp")
alsum=AL[[9]]*1000
ext=extract(alsum,vpw[7,],na.rm=TRUE, df = TRUE)

r=readRDS("aladin2013.RDS")
r[, BASE:= TIME - AHEAD ]
ir = r[OBL==7 & BASE==as.POSIXlt('2013-06-01 12:00:00')]
setnames(ir, 'TIME', 'dtmLEAD')
ir[, dtmLEAD:= dtmLEAD-3600]
ir$EID[ir$EID==1]<-"ALADIN"
ir=ir[2:10]
ir=ir[-9,]
#ir[,PR:=PR*3600]
pal=ggplot(ir,aes(x=(dtmLEAD), y = PR * 2000 , colour = 'ALADIN'))+geom_line(size=2)

#LAEF
setwd("~/Plocha/DATA/LAEF/2013")
ENS=brick("LAEF4hydro_pf_2013060112.nc", varname="tp",level=10,nlayers=1)
ensum=ensmean[[9]]*1000
e=readRDS('laef2013.RDS')
e[, BASE:= TIME - AHEAD ]
ie = e[OBL==7 & BASE==as.POSIXlt('2013-06-01 12:00:00')]
setnames(ie, 'TIME', 'dtmLEAD')
ie[, dtmLEAD:= dtmLEAD-3600]
ie=ie[-c(1,10,11,20,21,30,31,40,41,50,51,60,61,70,71,80,81,90,91,100,101,110,111,120,121,130,131,140,141,150,151,160),]
#names(ie)[2]="ENS MEMBERS"

pens=ggplot(ie,aes(x=dtmLEAD,y=PR*2000, group=EID, colour=EID))+geom_line(size=0.3)+stat_summary(fun.y=mean,colour="red", geom="line",aes(group=1),size=2)+ylab("PRECIPITATION [mm]")+xlab("TIME")

dta[, dtm:=as.POSIXct(as.POSIXlt(dtmLEAD, tz='CEST'))]
ggplot(ie,aes(x=dtmLEAD,y=PR*1000))+geom_line(aes(group=EID,color='LAEF'), size=0.3,alpha = .6)+ylab("PRECIPITATION [mm]")+xlab("TIME")+stat_summary(fun.y=mean,aes(colour="ENSMean"), geom="line",size=2)+ geom_line(aes(x=(dtmLEAD), y = PR*1000 , col = 'ALADIN' ), data = ir)+geom_line(aes(x = (dtmLEAD)-2*3600, y = cumsum(value), col = 'OBS'), data = dta)+scale_colour_brewer(name = 'SOURCE', palette = 'Set1')+theme(text = element_text(size = 20))

ie[, dPR:=c(0, diff(PR)), by = EID]
ir[, dPR:=c(0, diff(PR)), by = EID]
Sys.setlocale(locale= "en_US.UTF-8")
ggplot(ie,aes(x=dtmLEAD,y=dPR*1417.32 ))+geom_line(aes(group=EID, color='LAEF'), size=0.3, alpha = .6)+ylab("PRECIPITATION [mm]")+xlab("TIME")+stat_summary(fun.y=mean,aes(colour="ENSMean"), geom="line",size=2)+ geom_line(aes(x=(dtmLEAD), y = dPR*1417.32 , col = 'ALADIN' ) , data = ir)+geom_line(aes(x = (dtmLEAD), y = value, col = 'OBS'), data = dta)+scale_colour_brewer(name = 'SOURCE', palette = 'Set1')+theme(text = element_text(size = 20))

require(RColorBrewer)
display.brewer.all()


########casove charakteristiky
tal = stackApply(AL, fun = function(x, ...)if(all(is.na(x)))(0)else(as.double(which.max(diff(x))[1])), indices= rep(1, 10), na.rm=TRUE)

tens=stackApply(ensmean, fun = function(x, ...)if(all(is.na(x)))(0)else(as.double(which.max(diff(x))[1])), indices= rep(1, 10), na.rm=TRUE)

bt=(brick('all_obs')[[0:54]])
bt=OBS[[3:11]]
tb = stackApply(bt, fun = function(x, ...)if(all(is.na(x)))(0)else(as.double(which.max(diff(x))[1])), indices= rep(1, 10), na.rm=TRUE)

plot(tal,title(main="ALADIN-CZ"),zlim=c(0,10),axes=FALSE,col=brewer.pal(n=9,name='YlOrRd'),colNA = 'white' ,bty = 'n',addfun = function()plot(vpw,add=TRUE,lwd=1.5))
require(rasterVis)

plot(tens,zlim=c(0,10),axes=FALSE,title(main="ALADIN/LAEF"), col=brewer.pal(n=9,name='YlOrRd'), colNA = 'white', bty = 'n',addfun = function()plot(vpw,add=TRUE,lwd=1.5))


plot(tb,zlim=c(0,10),axes=FALSE,title(main="CZRAD"), col=brewer.pal(n=9,name='YlOrRd'), colNA = 'white', bty = 'n',addfun = function()plot(vp2,add=TRUE,lwd=1.5))

fvp = fortify(vpw)

gplot(tal)+geom_tile(aes(fill = factor(value))) + scale_fill_brewer(palette=3)+geom_polygon(aes(x=long, y=lat, group = group), fill = NA, col = 'black',lwd=2, data = fvp)
ggsave('pokus.png', widht= , height= , dpi = , scale = 1.5)

gplot(tens)+geom_tile(aes(fill = factor(value))) + scale_fill_brewer(palette=3)+geom_polygon(aes(x=long, y=lat, group = group), fill = NA, col = 'black',lwd=2, data = fvp)
ggsave('pokus.png', widht= , height= , dpi = , scale = 1.5)


gplot(tb)+geom_tile(aes(fill = factor(value))) + scale_fill_brewer(palette=3)+geom_polygon(aes(x=long, y=lat, group = group), fill = NA, col = 'black',lwd=2, data = fvp)
ggsave('pokus.png', widht= , height= , dpi = , scale = 1.5)


#plot vybraneho polygonu
#vpr='vstupni_polygony'
k=plot(obsum)
l=plot(vp[7,], add=TRUE)
kl=mask(k,l,maskvalue=TRUE,) #error????

#nastaveni barev
brks <- seq(0, 150, by=0.001)
nb <- length(brks)-1
cols=topo.colors(nb)
plot(obsum,col=cols,colNA = 'darkolivegreen4',frame.plot=F)

xr = range(values(obsum), na.rm=TRUE)
rx = rescale(xr, to = c(0,1), from = c(0, 280))

#show_col(gradient_n_pal(c('white', 'red', 'orange'))(seq(rx[1], rx[2], length = 100)))
col = gradient_n_pal(c('blue','green','yellow','orange','red','darkred','white'))(seq(rx[1], rx[2], length = 150))
acol = alpha(col, alpha = seq(rx[1], rx[2], length = 150))

#sumy srazek
plot(obsum,col=col,axes=FALSE, title(main="CZRAD"))
plot(vp2,add=TRUE,ldw=2)
plot(vp2[7,],add=TRUE, lwd=4)
legend(title="48 hours summary")

plot(alsum,col=col,axes=FALSE, title(main="ALADIN-CZ"))
plot(vpw,add=TRUE,ldw=3)
plot(vpw[7,],add=TRUE, lwd=5)

plot(ensum,col=col,axes=FALSE, title(main="ALADIN/LAEF"))
plot(vpw,add=TRUE,ldw=3)
plot(vpw[7,],add=TRUE, lwd=5)

###########rozdil mereni-pozorovani

cl=gradient_n_pal(c('darkblue','blue','blue','blue','white','white','red','red','darkred'))(seq(rx[1], rx[2], length = 100))
#sjednoceni COR a RESS
newpr="+init=epsg:32633"
alsumw=projectRaster(alsum,crs=newpr)
ensumw=projectRaster(ensum,crs=newpr)
obsumx=aggregate(obsum,fact=c(10.8,16.7),fun=mean,na.rm=TRUE)
obsumx=resample(obsumx,alsumw,method="ngb")

roz1=alsumw-obsumx
plot(roz1,col=cl,  title(main="ALADIN-CZ - CZRAD"),axes=FALSE, zlim=c(-100,80),addfun = function()plot(vp2,add=TRUE,lwd=2))
plot(vp2[7,],add=TRUE,lwd=4)

roz2=ensumw-obsumx
plot(roz2,col=cl,  title(main="LAEF - CZRAD"),axes=FALSE, zlim=c(-100,80),addfun = function()plot(vp2,add=TRUE,lwd=2))
plot(vp2[7,],add=TRUE,lwd=4)

roz3=alsumw-ensumw
plot(roz3,col=cl,  title(main="ALADIN-CZ - LAEF"),axes=FALSE, zlim=c(-130,100),addfun = function()plot(vp2,add=TRUE,lwd=2))
plot(vp2[7,],add=TRUE,lwd=4)

####ETS
o=obsumx
p=alsumw

#p = mask(p,vp2[7,])
#o = mask(o,vp2[7,])

p = mask(p,vp2)
o = mask(o,vp2)

rh=c(-1,5,0,  5,150,1)
rclmat=matrix(rh,ncol=3, byrow=TRUE)
hp=reclassify(p,rclmat)
plot(hp)
hp


brk <- seq(0, 1, by=1)
nb <- length(brk)
clr=(c('lightgrey', 'darkgreen'),nb)
plot(hito, title(main="OBS >60mm"),axes=FALSE, col=c('lightgrey', 'darkblue'),axis.args=list(at=seq(0,1,by=1)))
     
plot(mf, title(main="LAEF-OBS"),axes=FALSE, col=c('darkolivegreen', 'orange','darkviolet'),axis.args=list(at=seq(-1,1,by=1)))  

obsum=brick('all_obs.gri')
ob=obsum[[1:48]]
xr = range(values(obsum), na.rm=TRUE)
rx = rescale(xr, to = c(0,1), from = c(0, 30))
col = gradient_n_pal(c( 'white','darkblue','green','green','green', 'yellow','yellow','yellow','orange','orange','orange','red','red','red', 'darkred', 'darkred','white', 'white'))(seq(rx[1], rx[2], length = 20))
plt=plot(ob,col=col,addfun = function()plot(vp,add=TRUE,lwd=1))
an=animate(ob,col=col,addfun = function()plot(vp,add=TRUE,lwd=1))

saveGIF(plt)   
     

x=c(10,20,30,40,50,60,70,80)
y=c(0.165,	0.272,	0.325,	0.539,	0.556,	0.382,	0.111,	0.044)
z=c(0.189,	0.301,	0.279,	0.391,	0.575,	0.291,	0.082,	0.034)

plot(x,y,type="l",col="red",lwd="4",title(main="ETS"), xlab="mm/48h", ylab="ETS")
lines(x,z,type="l",col="blue",lwd="4")
legend('topright',legend=c('ALADIN-CZ','ALADIN/LAEF'), col=c('','blue'),lty=1,bg='white')
par(bg='grey90') 

