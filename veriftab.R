&!#Uprava RDS tabulek s predpovedmi, odsumarizace a vymzani 0 kroku predpovedi

df=readRDS("aladin2015.RDS")
df=df[order(OBL)]
df=df[, dPR:=c(0, diff(PR))] #, by = EID
#setDT(df)[ , ddPR:= ifelse(AHEAD=="06",PR, dPR)]

df=df[!(df$AHEAD==1),]
df=df[,AHEAD:=(AHEAD-1)]
df=df[,TIME:=TIME-3600]
#rename(df,c("DTM"="TIME"))
df=df[,ORIGIN:=(TIME-AHEAD)]
df=df[,OBLAST:=OBL]
x=factor(c(df$OBL))
df[,OBL:=revalue(x, c("1"="r","2"="s","3"="m","4"="n","5"="h","6"="j","7"="l","8"="o","9"="t","10"="q","11"="u","12"="g","13"="a","14"="b","15"="f","16"="p","17"="k","18"="i","19"="e","20"="d","21"="c","22"="N","23"="G","24"="O","25"="I","26"="P","27"="J","28"="K","29"="H","30"="F","31"="E","32"="D","33"="M","34"="L","35"="A","36"="B","37"="C"))]
saveRDS(df, 'laef2015cf.RDS')

#spojeni RDS tabulek ensemblu dohromady

df=rbind(a,b,c,d,e)
saveRDS(df, "laef_cf.RDS")
#######################################################################################
#vysledna verif tabulka

setwd("~/Plocha/DATA/OBS1h")
dta=readRDS("dtaival")
dta = dta[, .(VYB = any(VYB), PR = sum(PR), TIME = DTM[1] - 3600, EVE=EVE[!is.na(EVE)][1], tps = tps[1], tps_all = paste(unique(tps), collapse = '-')), by = .(IVAL, ID)]
dta[!is.na(EVE), EID:=paste(ID, EVE, sep = '_')]
dta=dta[VYB=='TRUE']
dta=dta[,HOUR:=6]
#dta=dta[ID=="q"]
obs=dta[, !c("IVAL","VYB","EID","HOUR"),with=FALSE]
#dt=dta[EVE=="104"]




setwd("~/Plocha/DATA/tab_RDS")
dat=readRDS("laef.RDS")
#dat=dat[OBL==10 ]
#dat[,origin:= TIME-AHEAD]
dat$dPR<-dat$dPR*1000
dc = dcast.data.table(dat, OBL + SID + TIME + AHEAD + ORIGIN ~ EID,value.var = "dPR")
#dc=dc[,TIME:=TIME-3600]

#obs[dc, on = 'TIME'] #doladit, obs jen priklad
#obs[, kTIME:=as.character(TIME)]
#dc[, kTIME:=as.character(TIME)]
#pok =obs[dc, on = 'kTIME']
#obs[kTIME=='2011-05-03 12:00:00']
#dc[kTIME=='2011-05-03 12:00:00']

setkey(dc, TIME, OBL)
setkey(obs, TIME, ID)
vertab=obs[dc, nomatch = 0,] #allow.cartesian = TRUE
vertab$SID=NULL

ala=readRDS("aladin.RDS")
#ala[,ORIGIN:=DTM]
#ala[,TIME:=ORIGIN+(AHEAD*3600)]
#ala$DTM=NULL
ala$PR=NULL
ala$OBLAST=NULL
ala$ORIGIN=NULL

setnames(ala,"dPR","CF")
#setnames(ala,"dPR","CF")
setkey(vertabulka, TIME, ID, AHEAD)
setkey(ala, TIME, OBL, AHEAD)
vertabulka=vertabulka[ala,nomatch=0]
saveRDS(vertabulka,"vertab.RDS")
#vertabulka$EID=NULL
#vertabulka$SID=NULL
vertabulka$CF<-vertabulka$CF*1000

#pridani prumeru a medianu
vertab$MEAN <- rowMeans(subset(vertab, select = c(9:24)), na.rm = TRUE)
#vertab$MEDIAN <- rowMedians(subset(vertab, select = c(9:24)), na.rm = TRUE)

mv = melt(vertab, id.vars = 1:8)  
stat = mv[, .(MEAN = mean(value), MEDIAN = median(value)), keyby = .(ID, PR, TIME, EVE, tps, tps_all, AHEAD, ORIGIN)]
setkey(vertab,ID, PR, TIME, EVE, tps, tps_all, AHEAD, ORIGIN)
stat[vertab, on = c('ID', 'PR', 'TIME', 'OBL', 'EVE', 'AHEAD','tps','tps_all', 'ORIGIN')]
vertabulka=stat[vertab]

#prubehy udaalosti
#dat=dat[,forid:=rep(1:428,each=160)]
dat=dat[forid==221]  #potreba nejak zjistit forid
#dat=dat[TIME=="2011-07-20 01:00:00"]
verif=ensembleData(forecasts=members,dates=dat$TIME,)

#rank histogram
forecast=vertabulka[,!c( "ID","PR","EVE", "tps", "tps_all", "AHEAD","ORIGIN", "CF", "ALADIN"),with=FALSE]
observation=vertabulka[,!c("TIME", "ID","EVE","tps","tps_all", "AHEAD","ORIGIN","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","CF","ALADIN"),with=FALSE]
verifRankHist(forecast,observation)
