def.events = function(sra, mit, tstep = 10,thr=0.1, na.fill=TRUE){
  
  sra[sra<=thr]=0
  if (na.fill) sra[is.na(sra)] = 0
  if (all(mit=='R')) ({mit=360; R=TRUE}) else (R=FALSE)
  if (length(mit)==1) mit = rep(mit, length(sra))
  mit = mit/tstep
  nul = rle(sra<=thr)
  cs = cumsum(nul$len)
  mit = mit[cs]
  
  nul$val[ (nul$val==TRUE) & (nul$len<mit) ] = FALSE
  breaks = c(0,cs)#if (cs[1]==1) (cs) else (c(1, cs))
  fct = cut(1:length(sra), breaks = breaks, inc = TRUE)
  
  
  kde = rle(nul$val[as.integer(fct)])
  cs = cumsum(kde$len)
  breaks = c(0,cs)#if (cs[1]==1) (cs) else (c(1, cs))
  fct = as.integer(cut(1:length(sra), breaks = breaks, inc = TRUE))#, labels = 1:length(breaks)) )
  kkde = which(kde$val)
  fct[as.integer(fct)%in%kkde] = NA
  
  if (R == TRUE) ({
    
    DT = data.table(sra = sra, events = fct)
    #  setkey(DT, events)
    sta = DT[, list(max = max(sra), sum = sum(sra)), by = events]
    fct[fct %in% sta[!( (sum>=12.7) | (max>=6)),events]] = NA
    
  })
  
  # as.integer(factor(fct))
  fct
}  
