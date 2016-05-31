                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    library(data.table)
library(ggplot2)
library(plyr)
library(stringr)
library(stringi)
library(tidyr)
library(reshape2)
library(vcd)
library(lattice)
library(MASS)
library(FactoMineR)
#http://stats.stackexchange.com/questions/90380/interpreting-negative-coordinates-and-origin-in-2d-correspondence-analysis-plot?lq=1
#http://www.sthda.com/english/wiki/correspondence-analysis-in-r-the-ultimate-guide-for-the-analysis-the-visualization-and-the-interpretation-r-software-and-data-mining#visual-inspection

filename = "dotnewData.RDS"


if(file.exists(filename)){
  print("fileexists")
  #create dot survey response data set from the RDS
  dotsr<-readRDS(filename)
  
} else{
  
  
  dotsr <- data.table(read.csv("DOTCleanedData1.csv",skip = 1,stringsAsFactors = F))
  saveRDS(dotsr, "dotnewData.RDS")
  
}
# #convert dotData to data table
# dotsr<-data.table(dotsr,keep.rownames=TRUE)

#to have an idea check how many vairables and type of variables the DOT data has
dot.var.dt<-data.table(var.name = colnames(dotsr),var.type = sapply(dotsr,class))

#*****************************************************
#How does access to technology vary by gender?
#What devices do you have access to?
#How often do you actually use these devices?
#Where do you most commonly use the devices you have access to?
#How much access to do you have to the Internet?
#How often do you actually use the Internet?
#Where do you most commonly use the Internet?
#What digital services/tools do you have access to?
#Where do you most commonly use digital services?

#****************************************************

#rename the X column to ID
rename(dotsr,replace = c("X" = "ID"))


#Get only relevant columns from the dot survey response data set
dot.tech.gender.dt<-subset(dotsr, select = grepl("How.often.do.you.actually.use.the.Internet|ID|Gender|Age|Location|Geo|What.devices|use.these.devices|most.commonly.use.the.devices|access.to.do.you.have.to.the.Internet|commonly.use.the.Internet|What.digital.services.tools.do.you.have.access.to|commonly.use.digital", names(dotsr)))

#can I drop the rows with Gender..Male and Gender..Female. == NA

#create factor variables for devices
devices<-matrix(c("Desktopcomputer", "Laptop", "Tablet", "Smartphone", "Featurerichmobilephone","Basicmobilephone"),ncol = 1, byrow=T)
device<-as.table(devices)


freq.levels<- matrix(c("Anytime","Multipletimes", "Onceaday", "Everyfewdays", "Onceaweek","Onceeveryfewweeks",
                       "Onceamonth","Lessthanonceamonth","Never","never", "Idontknow"),ncol = 1, byrow = T)

freq<-as.table(freq.levels)
#truncate lengthy col names using .. or ...and lengthyphrases

#What devices do you have access to?
#How often do you actually use these devices?
#Where do you most commonly use the devices you have access to?
#How much access to do you have to the Internet?
#How often do you actually use the Internet?
#Where do you most commonly use the Internet?
#What digital services/tools do you have access to?
#Where do you most commonly use digital services?

###########################################################################
names(dot.tech.gender.dt) <- gsub("\\.", "", names(dot.tech.gender.dt))

names(dot.tech.gender.dt) <- gsub("\\Agerange", "", names(dot.tech.gender.dt))

names(dot.tech.gender.dt) <- gsub("Whatdevicesdoyouhaveaccessto", "acc.dev", names(dot.tech.gender.dt))

names(dot.tech.gender.dt) <- gsub("\\Howoftendoyouactuallyusethesedevices", "dev.use.freq.", names(dot.tech.gender.dt))

names(dot.tech.gender.dt) <- gsub("Wheredoyoumostcommonlyusethedevicesyouhaveaccessto", "use.loc.device.", names(dot.tech.gender.dt))

#How much access to do you have to the Internet
names(dot.tech.gender.dt) <- gsub("HowmuchaccesstodoyouhavetotheInternet", "acc.to.Internet.", names(dot.tech.gender.dt))

#HowoftendoyouactuallyusetheInternet
# HowoftendoyouactuallyusetheInternet
names(dot.tech.gender.dt) <- gsub("HowoftendoyouactuallyusetheInternet", "use.Internet", names(dot.tech.gender.dt))

#Where do you most commonly use the Internet
names(dot.tech.gender.dt) <- gsub("WheredoyoumostcommonlyusetheInternet", "use.loc.Internet.", names(dot.tech.gender.dt))

##What digital services
names(dot.tech.gender.dt) <- gsub("Whatdigitalservices", "acc.digi.tools.", names(dot.tech.gender.dt))

#Where do you most commonly use digital services
names(dot.tech.gender.dt) <- gsub("Wheredoyoumostcommonlyusedigitalservices", "loc.use.digi.tools.", names(dot.tech.gender.dt))

########################
#check what variables have been chosen for the research Q
#to have an idea check how many vairables and type of variables the DOT data has
dot.tech.gender.var.dt<-data.table(var.name = colnames(dot.tech.gender.dt),var.type = sapply(dot.tech.gender.dt,class))

#remove NA values in all mandatory columns.Age, Gender, Location,columns 1:9
dot.tech.gender.dt<-dot.tech.gender.dt[rowSums(is.na(dot.tech.gender.dt[,1:9, with = F]))==0,,drop =F]

dot.tech.gender.dt.man<-dot.tech.gender.dt[,1:9, with=F,by = ID]

#remove all NA rows in all other acc/use technology columns 10 :534

dot.tech.gender.dt<-dot.tech.gender.dt[rowSums(is.na(dot.tech.gender.dt[,10:584, with = F])) !=484,,drop =F]

############################
#frame the columns for subsets
#now the clean dot.tech.gender.dt dimensions are 548 x 534. so there were 19 such cases

cols.1 <-colnames(dot.tech.gender.dt)[grep("acc.dev",names(dot.tech.gender.dt))]
cols.2<-colnames(dot.tech.gender.dt)[grep("dev.use.freq.",names(dot.tech.gender.dt))]
cols.3<-colnames(dot.tech.gender.dt)[grep("use.loc.device.",names(dot.tech.gender.dt))]
cols.4<-colnames(dot.tech.gender.dt)[grep("acc.to.Internet.",names(dot.tech.gender.dt))]
cols.5<-colnames(dot.tech.gender.dt)[grep("use.Internet",names(dot.tech.gender.dt))]
cols.6<-colnames(dot.tech.gender.dt)[grep("use.loc.Internet.",names(dot.tech.gender.dt))]
cols.7<-colnames(dot.tech.gender.dt)[grep("acc.digi.tools.",names(dot.tech.gender.dt))]
cols.8<-colnames(dot.tech.gender.dt)[grep("loc.use.digi.tools.",names(dot.tech.gender.dt))]

############################################
## now get 8 subsets of data and merge them as needed for the associations

#Get only relevant columns from the dot survey response data set for technology access variables
dot.gen.acc.dev.dt<-subset(dot.tech.gender.dt, select = grepl("ID|Gender|Age|Location|Geo|acc.dev", names(dot.tech.gender.dt)))
dot.gen.use.dev.dt<-subset(dot.tech.gender.dt, select = grepl("ID|Gender|Age|Location|Geo|dev.use.freq.", names(dot.tech.gender.dt)))
dot.gen.dev.loc.dt<-subset(dot.tech.gender.dt, select = grepl("ID|Gender|Age|Location|Geo|use.loc.device.", names(dot.tech.gender.dt)))
dot.gen.acc.intrnt.dt<-subset(dot.tech.gender.dt, select = grepl("ID|Gender|Age|Location|Geo|acc.to.Internet.", names(dot.tech.gender.dt)))
dot.gen.use.intrnt.dt<-subset(dot.tech.gender.dt, select = grepl("ID|Gender|Age|Location|Geo|use.Internet.", names(dot.tech.gender.dt)))
dot.gen.use.intrnt.loc.dt<-subset(dot.tech.gender.dt, select = grepl("ID|Gender|Age|Location|Geo|use.loc.Internet.", names(dot.tech.gender.dt)))
dot.gen.acc.digi.dt<-subset(dot.tech.gender.dt, select = grepl("ID|Gender|Age|Location|Geo|acc.digi.tools.", names(dot.tech.gender.dt)))
dot.gen.loc.digi.dt<-subset(dot.tech.gender.dt, select = grepl("ID|Gender|Age|Location|Geo|loc.use.digi.tools.", names(dot.tech.gender.dt)))

################################################
#melt for access to devices
#melt each of them

dot.gen.acc.dev.dt.m<-data.table(melt(dot.gen.acc.dev.dt,measure.vars = cols.1 ,
                                         variable.name = "dev.acc", value.name = "acc.freq"))
#one.case<-dot.tech.gender.dt.ac.m[ID ==43912947 , list(dev.acc,acc.freq)]

#http://stackoverflow.com/questions/28230360/using-grepl-in-r-to-match-string
dot.gen.acc.dev.dt.m$device = apply(dot.gen.acc.dev.dt.m, 1, function(u){
  bool = sapply(device[,1], function(x) grepl(x, u[['dev.acc']]))
  if(any(bool)&(!is.na(u[['acc.freq']]))) device[bool] else NA
})

# 
# #add access.freq column
# 
dot.gen.acc.dev.dt.m$dev.avail.freq = apply(dot.gen.acc.dev.dt.m, 1, function(u){
  bool = sapply(freq[,1], function(x) grepl(x, u[['dev.acc']]))
  if(any(bool) & (!is.na(u[['acc.freq']]))) freq[bool] else NA
})
one.acc.case.m<-dot.gen.acc.dev.dt.m[ID ==43912947 , list(dev.acc,acc.freq,device,dev.avail.freq)]

# #remove the measure cols and value cols from dcast formula
id.cols.acc.m <- colnames(dot.gen.acc.dev.dt.m)[-grep("dev.acc|acc.freq",names(dot.gen.acc.dev.dt.m))]

f <- as.formula(paste(paste(id.cols.acc.m, collapse = " + "), "~ dev.acc"))

dot.gen.acc.dev.dt<-data.table(dcast(data =dot.gen.acc.dev.dt.m[!is.na(dev.avail.freq),], f, value.var = "acc.freq",function(x) length(unique(x))))

dot.gen.acc.dev.dt.m<-NULL
#drop acc: columns..
#http://stackoverflow.com/questions/24590341/removing-multiple-columns-from-r-data-table-with-parameter-for-columns-to-remove
#dt[, (colsToDelete) := NULL]

dot.gen.acc.dev.dt<-dot.gen.acc.dev.dt[, which(grepl("acc.dev", colnames(dot.gen.acc.dev.dt))):=NULL]
one.acc.case<-dot.gen.acc.dev.dt[ID ==43912947 , list(device,dev.avail.freq)]
#
id.cols.acc.m<-NULL
f<-NULL


#enter image description here
#http://stats.stackexchange.com/questions/90380/interpreting-negative-coordinates-and-origin-in-2d-correspondence-analysis-plot?lq=1

##############################use device##########################

dot.gen.use.dev.dt.m<-data.table(melt(dot.gen.use.dev.dt,measure.vars = cols.2 ,
                                      variable.name = "dev.use", value.name = "dev.use.freq"))

dot.gen.use.dev.dt.m$device.use = apply(dot.gen.use.dev.dt.m, 1, function(u){
  bool = sapply(device[,1], function(x) grepl(x, u[['dev.use']]))
  if(any(bool)&(!is.na(u[['dev.use.freq']]))) device[bool] else NA
})

# 
# add use.freq column
# 
dot.gen.use.dev.dt.m$use.dev.freq = apply(dot.gen.use.dev.dt.m, 1, function(u){
  bool = sapply(freq[,1], function(x) grepl(x, u[['dev.use']]))
  if(any(bool) & (!is.na(u[['dev.use.freq']]))) freq[bool] else NA
})

#
# #remove the measure cols and value cols from dcast formula
id.cols.use.m <- colnames(dot.gen.use.dev.dt.m)[-grep("dev.use|dev.use.freq",names(dot.gen.use.dev.dt.m))]
f <- as.formula(paste(paste(id.cols.use.m, collapse = " + "), "~ dev.use"))
dot.gen.use.dev.dt<-data.table(dcast(data = dot.gen.use.dev.dt.m[!is.na(use.dev.freq),], f, value.var = "dev.use.freq",function(x) length(unique(x))))

dot.gen.use.dev.dt<-dot.gen.use.dev.dt[, which(grepl("dev.use|dev.use.freq", colnames(dot.gen.use.dev.dt))):=NULL]
one.use.case<-dot.gen.use.dev.dt[ID ==43912947 , list(device.use,use.dev.freq)]
dot.gen.use.dev.dt.m<-NULL
id.cols.use.m<-NULL
f<-NULL


##############################use location of device##########################
dev.use.loc <-matrix(c("EverywhereIalwayshaveaninternetenableddevice", "Evrywhr.IE.device",
                       "Atwork", "work",
                       "Athome", "home",
                       "Atafamilymembershouse", "FM house",
                       "Atschool", "school",
                       "Atalibrarycommunitycentreorchurch", "library.CC.Church",
                       "Ataninternetcafe", "ICafe"),ncol = 2, byrow = T)



dot.gen.dev.loc.dt.m<-data.table(melt(dot.gen.dev.loc.dt,measure.vars = cols.3 ,
                                      variable.name = "dev.loc", value.name = "loc.freq"))

dot.gen.dev.loc.dt.m$use.at = apply(dot.gen.dev.loc.dt.m, 1, function(u){
  bool = sapply(dev.use.loc[,1], function(x) grepl(x, u[['dev.loc']]))
  if(any(bool)&(!is.na(u[['loc.freq']])))dev.use.loc[bool, 2] else NA
})

# 
# add use.location column
# 
dot.gen.dev.loc.dt.m$location.use.freq = apply(dot.gen.dev.loc.dt.m, 1, function(u){
  bool = sapply(freq[,1], function(x) grepl(x, u[['dev.loc']]))
  if(any(bool) & (!is.na(u[['loc.freq']]))) freq[bool] else NA
})

#
# #remove the measure cols and value cols from dcast formula
id.cols.use.loc.m <- colnames(dot.gen.dev.loc.dt.m)[-grep("dev.loc|loc.freq",names(dot.gen.dev.loc.dt.m))]
f <- as.formula(paste(paste(id.cols.use.loc.m, collapse = " + "), "~ dev.loc"))
dot.gen.dev.loc.dt<-data.table(dcast(data = dot.gen.dev.loc.dt.m[!is.na(location.use.freq),], f, value.var = "loc.freq",function(x) length(unique(x))))

dot.gen.dev.loc.dt<-dot.gen.dev.loc.dt[, which(grepl("use.loc.dev", colnames(dot.gen.dev.loc.dt))):=NULL]
one.loc.use.case<-dot.gen.dev.loc.dt[ID ==43912947 , list(use.at,location.use.freq)]
dot.gen.dev.loc.dt.m<-NULL
id.cols.use.loc.m<-NULL
f<-NULL
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#stacked bar plots for device - access/use frequencies across Locations
# dev.use.loc<-dot.gen.dev.loc.dt[, list(Frequency = length(ID)),by = c( "LocationRespondent","use.at","location.use.freq")]
# png("device-use-locationfrequency-by-Location.png", width = 1200, height = 600)
# 
# #http://stackoverflow.com/questions/6644997/showing-data-values-on-stacked-bar-chart-in-ggplot2
# acc.case.m.c<-acc.case.m[, list(Frequency = length(ID)),by = c( "LocationRespondent","device","dev.avail.freq")]
# 
# Data0 <- ddply(acc.case.m.c, .(LocationRespondent,dev.avail.freq), 
#               transform, pos = cumsum(Frequency) - (0.5 * Frequency)
# )
# 
# Data <- ddply(use.case.m.c, .(LocationRespondent,use.dev.freq), 
#               transform, pos = cumsum(Frequency) - (0.5 * Frequency)
# )
# 
# # 

# 
# gg<-ggplot(Data, aes(x = use.dev.freq,y=Frequency  )) +
#   geom_bar(aes(fill = device), stat="identity") +
#   geom_text(aes(label = ifelse(Frequency >= 10, Frequency,""), y = pos), size = 4) +
#   facet_wrap(~LocationRespondent,scales = "free_y" ) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#   ggtitle("Device use frequencies varying by Location") + scale_fill_manual(values=cbPalette)
# 
# 
# 
# print(gg)
# dev.off()
# 
# dev.loc.gen.tab<-xtabs( ~GenderC + use.at , data= dot.gen.dev.loc.dt)
# 
# pt<-prop.table(dev.loc.gen.tab,margin = 1)
# kmtab<-addmargins(pt)
# p1<-mosaic(pt,color = T,shade=T,cex.axis = 0.5,
#            labeling = labeling_values,
#            labeling_args = list(abbreviate_labs = c(0,6,8,8),
#                                 rot_labels = c(0,0,90,0),
#                                 just_labels = c("right","left","left","right"),
#                                 gp_text = gpar(fontsize=10)),
#            gp_labels = gpar(fontsize = 10)
# )


#print(p1)

#########################clean up for internet access##########################


dot.gen.acc.intrnt.dt.m<-data.table(melt(dot.gen.acc.intrnt.dt, measure.vars = cols.4 ,
                                             variable.name = "int.dev.acc", value.name = "int.acc.freq"))


one.inrnt.acc.case.m<-dot.gen.acc.intrnt.dt.m[ID ==43912947 , list(int.dev.acc,int.acc.freq)]

dot.gen.acc.intrnt.dt.m$inrnt.on.device = apply(dot.gen.acc.intrnt.dt.m, 1, function(u){
  bool = sapply(device[,1], function(x) grepl(x, u[['int.dev.acc']],ignore.case=TRUE))
  if(any(bool)&(!is.na(u[['int.acc.freq']]))) device[bool] else NA
})

dot.gen.acc.intrnt.dt.m$inrnt.dev.freq = apply(dot.gen.acc.intrnt.dt.m, 1, function(u){
  bool = sapply(freq[,1], function(x) grepl(x, u[['int.dev.acc']]))
  if(any(bool) & (!is.na(u[['int.dev.acc']]))) freq[bool] else NA
})

# #remove the measure cols and value cols from dcast formula
id.cols.inrnt.acc.m <- colnames(dot.gen.acc.intrnt.dt.m)[-grep("int.dev.acc|int.acc.freq",names(dot.gen.acc.intrnt.dt.m))]
f <- as.formula(paste(paste(id.cols.inrnt.acc.m, collapse = " + "), "~ int.dev.acc"))
dot.gen.acc.intrnt.dt<-data.table(dcast(data = dot.gen.acc.intrnt.dt.m[!is.na(int.acc.freq),], f, value.var = "int.acc.freq",function(x) length(unique(x))))

dot.gen.acc.intrnt.dt<-dot.gen.acc.intrnt.dt[, which(grepl("acc.to.Internet", colnames(dot.gen.acc.intrnt.dt))):=NULL]
one.inrnt.acc.case<-dot.gen.acc.intrnt.dt[ID ==43912947 , list(inrnt.on.device,inrnt.dev.freq)]
dot.gen.acc.intrnt.dt.m<-NULL
id.cols.inrnt.acc.m<-NULL
f<-NULL


########################internet access location ######################
###############clean up for internet use freq.on device###########

dot.gen.use.intrnt.dt.m<-data.table(melt(dot.gen.use.intrnt.dt,measure.vars = cols.5 ,
                                      variable.name = "use.intrnt", value.name = "use.intrnt.freq"))

dot.gen.use.intrnt.dt.m$intrnt.use.on.dev = apply(dot.gen.use.intrnt.dt.m, 1, function(u){
  bool = sapply(device[,1], function(x) grepl(x, u[['use.intrnt']],ignore.case = T))
  if(any(bool)&(!is.na(u[['use.intrnt.freq']]))) device[bool] else NA
})

# 
# add use.internt.freq column
# 
dot.gen.use.intrnt.dt.m$intrnt.on.dev.use.freq = apply(dot.gen.use.intrnt.dt.m, 1, function(u){
  bool = sapply(freq[,1], function(x) grepl(x, u[['use.intrnt']]))
  if(any(bool) & (!is.na(u[['use.intrnt.freq']]))) freq[bool] else NA
})
one.use.intrnt.case<-dot.gen.use.intrnt.dt.m[ID ==43912947 , list(intrnt.use.on.dev,intrnt.on.dev.use.freq)]

#
# #remove the measure cols and value cols from dcast formula
id.cols.use.intrnt.on.dev.m <- colnames(dot.gen.use.intrnt.dt.m)[-grep("use.intrnt|use.intrnt.freq",names(dot.gen.use.intrnt.dt.m))]
f <- as.formula(paste(paste(id.cols.use.intrnt.on.dev.m, collapse = " + "), "~ use.intrnt"))
dot.gen.use.intrnt.dt<-data.table(dcast(data = dot.gen.use.intrnt.dt.m[!is.na(use.intrnt.freq),], f, value.var = "use.intrnt.freq",function(x) length(unique(x))))

dot.gen.use.intrnt.dt<-dot.gen.use.intrnt.dt[, which(grepl("use.Internet",colnames(dot.gen.use.intrnt.dt))):=NULL]
one.use.intrnt.case<-dot.gen.use.intrnt.dt[ID ==43912947 , list(intrnt.use.on.dev,intrnt.on.dev.use.freq)]
dot.gen.use.intrnt.dt.m<-NULL
id.cols.use.intrnt.on.dev.m<-NULL
f<-NULL


dev.gen.intrnt.tab<-xtabs( ~intrnt.on.dev.use.freq + intrnt.use.on.dev +GenderC, data= dot.gen.use.intrnt.dt)

# pt<-prop.table(dev.gen.intrnt.tab,margin = 1)
# kmtab<-addmargins(pt)
# p1<-mosaic(dev.gen.intrnt.tab,main = "How does internet usage vary by Gender", color = T,shade=T,cex.axis = 0.5, xlab="",ylab="",
#            labeling = labeling_values,
#            labeling_args = list(abbreviate_labs = c(0,6,8,0),
#                                 rot_labels = c(0,0,90,0),
#                                 just_labels = c("right","left","left","right"),
#                                 gp_text = gpar(fontsize=10)),
#            gp_labels = gpar(fontsize = 10)
# )
#http://stats.stackexchange.com/questions/153446/r-how-to-interpret-mosaic-and-association-plots?rq=1
# png("Frequency-Device-used-for-internet-association-by-gender.png", width = 1200, height = 600)
# 
# p1<-cotabplot(~intrnt.on.dev.use.freq + intrnt.use.on.dev | GenderC, 
#               data = dev.gen.intrnt.tab, 
#               panel = cotab_coindep, n= 5000,
#               cex.axis = 0.5,
#               labeling_args = list(abbreviate_labs = c(0,6,8,0),
#                                     rot_labels = c(0,0,90,0),
#                                     just_labels = c("right","left","left","right"),
#                                      gp_text = gpar(fontsize=10)),
#                                      gp_labels = gpar(fontsize = 10))
# print(p1)
# dev.off()

#dev.gen.intrnt.tab<-xtabs(~GenderC + intrnt.on.dev.use.freq + AgeC + intrnt.use.on.dev, data = dot.gen.use.intrnt.dt)
# png("Freq-Device-used-for-intrnt-assoc-by-gender-Age.png", width = 1200, height = 600)
# set.seed(42)
# p2<-cotabplot(~intrnt.on.dev.use.freq + intrnt.use.on.dev + AgeC, data = dot.gen.use.intrnt.dt, panel = cotab_mosaic, direction = "v")
# 
# print(p2)
# dev.off()

# png("Freq-Device-used-for-intrnt-assoc-by-Age.png", width = 1200, height = 600)
# set.seed(432)
# dev.gen.intrnt.tab3<-xtabs( ~intrnt.on.dev.use.freq + intrnt.use.on.dev + AgeC, data= dot.gen.use.intrnt.dt)
# 
# p3<-cotabplot(~intrnt.on.dev.use.freq + intrnt.use.on.dev | AgeC, data = dev.gen.intrnt.tab3, panel = cotab_coindep, n=5000,
#               panel_args = list(type = "assoc", margins = c(2,1,1,2), varnames = FALSE),
#               labeling_args = list(abbreviate_labs = c(0,6,8,0),rot_labels = c(0,0,90,0),
#               just_labels = c("right","left","left","right"),
#               gp_text = gpar(fontsize=10)),
#               gp_labels = gpar(fontsize = 10))
# 
# print(p3)
# dev.off()
# 

#################################### #internet use location#####################
###############clean up for internet use location and location use freq.on device

dot.gen.use.intrnt.loc.dt.m<-data.table(melt(dot.gen.use.intrnt.loc.dt,measure.vars = cols.6 ,
                                         variable.name = "use.loc.intrnt", value.name = "use.loc.intrnt.freq"))

dot.gen.use.intrnt.loc.dt.m$intrnt.use.loc = apply(dot.gen.use.intrnt.loc.dt.m, 1, function(u){
  bool = sapply(dev.use.loc[,1], function(x) grepl(x, u[['use.loc.intrnt']],ignore.case = T))
  if(any(bool)&(!is.na(u[['use.loc.intrnt.freq']]))) dev.use.loc[bool, 2] else NA
})

# add use.internt.freq column
# 
dot.gen.use.intrnt.loc.dt.m$intrnt.use.loc.freq = apply(dot.gen.use.intrnt.loc.dt.m, 1, function(u){
  bool = sapply(freq[,1], function(x) grepl(x, u[['use.loc.intrnt']]))
  if(any(bool) & (!is.na(u[['use.loc.intrnt.freq']]))) freq[bool] else NA
})

#
# #remove the measure cols and value cols from dcast formula
id.cols.use.intrnt.loc.m <- colnames(dot.gen.use.intrnt.loc.dt.m)[-grep("use.loc.intrnt|use.loc.intrnt.freq",names(dot.gen.use.intrnt.loc.dt.m))]
f <- as.formula(paste(paste(id.cols.use.intrnt.loc.m, collapse = " + "), "~ use.loc.intrnt"))
dot.gen.use.intrnt.loc.dt<-data.table(dcast(data = dot.gen.use.intrnt.loc.dt.m[!is.na(use.loc.intrnt.freq),], f, value.var = "use.loc.intrnt.freq",function(x) length(unique(x))))

dot.gen.use.intrnt.loc.dt<-dot.gen.use.intrnt.loc.dt[, which(grepl("use.loc.Internet", colnames(dot.gen.use.intrnt.loc.dt))):=NULL]
one.use.loc.intrnt.case<-dot.gen.use.intrnt.loc.dt[ID ==43912947 , list(intrnt.use.loc,intrnt.use.loc.freq)]
dot.gen.use.intrnt.loc.dt.m<-NULL
id.cols.use.intrnt.loc.m<-NULL
f<-NULL


######################### access to digital services#########################

digi.tools<-matrix( c("useSMSTextmessages","takedigitalphotos","takedigitalvideos", "makedigitalpayments",
                      "receivedigitalpayments","accesspoliticalnews","accesssocialnetworksFacebookTwitterothers",
                      "getinformationaboutmyhealth","lookforapplyforjobs","offerprofessionalservicesforafee",
                      "playgames","researchtopicsthatinterestme","engageindiscussionswithmypeers",
                      "useawordprocessor","usebudgetingtools","createappsforsmartphonesorcomputers",
                      "buildawebsite","sellproductsorservicesonline","accessbankingandorfinancialservicesaccountsloans")
                    , ncol=1,byrow =T)

digi.ser.t<-as.table(digi.tools)

dot.gen.acc.digi.dt.m<-data.table(melt(dot.gen.acc.digi.dt,measure.vars = cols.7 ,
                                       variable.name = "digi.acc", value.name = "digi.tools"))
dot.gen.acc.digi.dt.m$tool.ser.dig = apply(dot.gen.acc.digi.dt.m, 1, function(u){
    bool = sapply(digi.ser.t[,1], function(x) grepl(x, u[['digi.acc']],ignore.case = T))
    if(any(bool)&(!is.na(u[['digi.tools']]))) digi.ser.t[bool] else NA
  })
  


dot.gen.acc.digi.dt.m$frq.ac.dig = apply(dot.gen.acc.digi.dt.m, 1, function(u){
  bool = sapply(freq[,1], function(x) grepl(x, u[['digi.acc']]))
  if(any(bool) & (!is.na(u[['digi.tools']]))) freq[bool] else NA
})


id.cols.digi.tools.m <- colnames(dot.gen.acc.digi.dt.m)[-grep("digi.acc|digi.tools",names(dot.gen.acc.digi.dt.m))]
f <- as.formula(paste(paste(id.cols.digi.tools.m, collapse = " + "), "~ digi.acc"))

dot.gen.acc.digi.dt<-data.table(dcast(data = dot.gen.acc.digi.dt.m[!is.na(digi.tools),], f, value.var = "digi.tools",function(x) length(unique(x))))
one.acc.digi.loc.case<-dot.gen.acc.digi.dt[ID ==43912947 , list(tool.ser.dig,frq.ac.dig)]

dot.gen.acc.digi.dt<-dot.gen.acc.digi.dt[, which(grepl("acc.digi.tools", colnames(dot.gen.acc.digi.dt))):=NULL]
id.cols.digi.tools.m<-NULL
dot.gen.acc.digi.dt.m<-NULL
f<-NULL



###################location use of digitial services###############

dot.gen.loc.digi.dt.m<-data.table(melt(dot.gen.loc.digi.dt,measure.vars = cols.8 ,
                                             variable.name = "loc.use.digi", value.name = "loc.digi.freq"))

dot.gen.loc.digi.dt.m$digi.loc.of.use = apply(dot.gen.loc.digi.dt.m, 1, function(u){
  bool = sapply(dev.use.loc[,1], function(x) grepl(x, u[['loc.use.digi']],ignore.case = T))
  if(any(bool)&(!is.na(u[['loc.digi.freq']]))) dev.use.loc[bool, 2] else NA
})

# add use.internt.freq column
# 
dot.gen.loc.digi.dt.m$at.loc.digi.use.freq = apply(dot.gen.loc.digi.dt.m, 1, function(u){
  bool = sapply(freq[,1], function(x) grepl(x, u[['loc.use.digi']]))
  if(any(bool) & (!is.na(u[['loc.digi.freq']]))) freq[bool] else NA
})


#
# #remove the measure cols and value cols from dcast formula
id.cols.use.digi.loc.m <- colnames(dot.gen.loc.digi.dt.m)[-grep("loc.use.digi|loc.digi.freq",names(dot.gen.loc.digi.dt.m))]
f <- as.formula(paste(paste(id.cols.use.digi.loc.m, collapse = " + "), "~ loc.use.digi"))
dot.gen.loc.digi.dt<-data.table(dcast(data = dot.gen.loc.digi.dt.m[!is.na(loc.digi.freq),], f, value.var = "loc.digi.freq",function(x) length(unique(x))))

dot.gen.loc.digi.dt<-dot.gen.loc.digi.dt[, which(grepl("loc.use.digi.tools", colnames(dot.gen.loc.digi.dt))):=NULL]
one.use.digi.loc.case<-dot.gen.loc.digi.dt[ID ==43912947 , list(digi.loc.of.use,at.loc.digi.use.freq)]
dot.gen.loc.digi.dt.m<-NULL
id.cols.use.digi.loc.m<-NULL
f<-NULL
