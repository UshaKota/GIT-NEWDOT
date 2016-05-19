id.cols.use  <-colnames(dot.tech.gender.dt)[-grep("dev.use.freq.",names(dot.tech.gender.dt))]
measure.cols.use <- colnames(dot.tech.gender.dt)[grep("dev.use.freq.",names(dot.tech.gender.dt))]


dot.tech.gender.use.m<-data.table(melt(dot.tech.gender.dt, id.vars = id.cols.use, measure.vars = measure.cols.use,
                                       na.rm=T ,variable.name = "dev.use", value.name = "dev.use.freq"))


one.use.case.m<-dot.tech.gender.use.m[ID ==43912947 , list(device,dev.avail.freq,dev.use)]


dot.tech.gender.use.m$use.dev.freq =  apply(dot.tech.gender.use.m, 1, function(u){
  
  bool = sapply(freq.levels[,1], function(x) grepl(x, u[['dev.use']]))
  if(any(bool)) freq.levels[bool] else NA
})

dot.tech.gender.use.m$device.use =  apply(unique(dot.tech.gender.use.m), 1, function(u){
  
  bool1 = sapply(device[,1], function(x) grepl(x, u[['dev.use']]))
  
  if(any(bool1) ) device[bool1] else NA
})



#some people use devices that they do not have access to.. so add that using device as another device --
#insert a row for the user when the using device does not match the access device


#http://stackoverflow.com/questions/15347282/split-string-column-and-insert-as-multiple-new-rows

#get the subset for use frequency
id.cols.use.m <- colnames(dot.tech.gender.use.m)[-grep("dev.use|dev.use.freq",names(dot.tech.gender.dt.ac.m))]

f <- as.formula(paste(paste(id.cols.use.m, collapse = " + "), "~ dev.use"))

dot.tech.gender.dt<-data.table(dcast(data =dot.tech.gender.use.m, f, value.var = "dev.use.freq",function(x) length(unique(x))))

#drop the dev.use.freq. cols

dot.tech.gender.dt<-dot.tech.gender.dt[, which(grepl("dev.use|dev.use.freq", colnames(dot.tech.gender.dt))):=NULL]
#small.dt.use1<-dot.tech.gender.dt[InternalID ==43912947,list(InternalID, device,dev.avail.freq,freq.use.dev)]

one.use.case.c<-dot.tech.gender.use.m[ID ==43912947,list( device,dev.avail.freq,device.use,use.dev.freq)]

use.case.m<-unique(dot.tech.gender.dt[,list( device.use,use.dev.freq), by = ID])

cols<-c("dev.use", "use.dev.freq")
acc.case.mm<-unique(dot.tech.gender.dt[, !cols, with=FALSE,by = ID])


acc.case.m<-unique(dot.tech.gender.dt[,list( device,dev.avail.freq), by = ID])

#rename to merge
rename(use.case.m, replace = c("device.use" = "device"))

dot.tech.gender.dt.dev<-merge(acc.case.m,use.case.m, by = c("ID", "device"),all.y = T)
#
