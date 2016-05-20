
library(data.table)
library(plyr)
library(stringr)
library(stringi)
library(tidyr)
library(reshape2)
library(vcd)
library(lattice)
library(MASS)

library(corrplot)
library(ggplot2)
library (stm) #for open ended-text survey responses
library(RColorBrewer)


filename = "dotnewData.RDS"


if(file.exists(filename)){
  print("fileexists")
  #create dot survey response data set from the RDS
  dotsr<-readRDS(filename)
  
} else{
  
  
  dotsr <- data.table(read.csv("DOTCleanedData1.csv",skip = 1,stringsAsFactors = F))
  saveRDS(dotsr, "dotnewData.RDS")
  
}

#Variables for 5th Research Q 
# In what ways are youth in these countries using technology and social media for learning
# In what ways have you ever used devices for learning?
# In what ways do you use the Internet for learning?
# How often do you use the following digital services/tools for
# learning?
# What other digital services/tools do you use for learning?
# If you have participated an online course, what did you think of it?

#rename the X column to ID
rename(dotsr,replace = c("X" = "ID"))

#Get mandatory relevant columns from the dot survey response data set
dot.tech.sm.learn.man.dt<-subset(dotsr, select = grepl("ID|Gender|Age|Location|Geo", names(dotsr)))

dot.tech.sm.learn.dt<-dotsr[,1146:1219,with=F]

#check all the variables chosen for this Q
dot.tech.sm.learn.var.dt<-data.table(var.name = colnames(dot.tech.sm.learn.dt),var.type = sapply(dot.tech.sm.learn.dt,class))


names(dot.tech.sm.learn.dt) <- gsub("In.what.ways.do.you.use.devices.for.learning", "dev.for.learn", names(dot.tech.sm.learn.dt))
dot.tech.sm.learn.full.dt<-cbind(dot.tech.sm.learn.man.dt,dot.tech.sm.learn.dt)

#remove NAs in mandatory cols
dot.tech.sm.learn.full.dt<-dot.tech.sm.learn.full.dt[rowSums(is.na(dot.tech.sm.learn.full.dt[,1:9, with = F]))==0,,drop =F]

#remove NA when count exceeds other variables
dot.tech.sm.learn.full.dt<-dot.tech.sm.learn.full.dt[rowSums(is.na(dot.tech.sm.learn.full.dt[,10:73, with = F])) != 63,,drop =F]

#write.table(dot.tech.sm.learn.var.dt, "Q5vars.txt",sep = " ")

##############Open ended response Questions again######################
# "1" "In.what.ways.have.you.ever.used.devices.for.learning....Mobile.phone" "character"
# "2" "In.what.ways.have.you.ever.used.devices.for.learning....Desktop.computer" "character"
# "3" "In.what.ways.have.you.ever.used.devices.for.learning....Laptop.computer" "character"
# "4" "In.what.ways.have.you.ever.used.devices.for.learning....Tablet" "character"
# "5" "In.what.ways.have.you.ever.used.devices.for.learning....Other.electronic.devices" "character"
# "6" "In.what.ways.do.you.use.the.Internet.for.learning." "character"

# "What.other.digital.services.tools.do.you.use.for.learning." "character"
# "71" 
# "If.you.have.participated.an.online.course..what.did.you.think.of.it.....What.was.the.most.challenging.part.of.participati
# ng.in.an.online.course." "character"
# "72" 
# "If.you.have.participated.an.online.course..what.did.you.think.of.it.....What.was.the.best.part.about.participating.in.an.
# online.course." "character"
# "73" 
# "If.you.have.participated.an.online.course..what.did.you.think.of.it.....Would.you.seek.out.an.online.course.again..Why.or
# .why.not." "character"

names(dot.tech.sm.learn.full.dt) <- gsub("In.what.ways.have.you.ever.used.devices.for.learning....Mobile.phone", "Mobil.phone.learn", names(dot.tech.sm.learn.full.dt))
names(dot.tech.sm.learn.full.dt) <- gsub("In.what.ways.have.you.ever.used.devices.for.learning....Desktop.computer", "Desktop.learn", names(dot.tech.sm.learn.full.dt))

names(dot.tech.sm.learn.full.dt) <- gsub("In.what.ways.have.you.ever.used.devices.for.learning....Laptop.computer", "Laptop.learn", names(dot.tech.sm.learn.full.dt))

names(dot.tech.sm.learn.full.dt) <- gsub("In.what.ways.have.you.ever.used.devices.for.learning....Tablet", "Tab.learn", names(dot.tech.sm.learn.full.dt))

names(dot.tech.sm.learn.full.dt) <- gsub("In.what.ways.have.you.ever.used.devices.for.learning....Other.electronic.devices", "Other.edev.learn", names(dot.tech.sm.learn.full.dt))

names(dot.tech.sm.learn.full.dt) <- gsub("In.what.ways.do.you.use.the.Internet.for.learning.", "Intrnt.for.learn", names(dot.tech.sm.learn.full.dt))

names(dot.tech.sm.learn.full.dt) <- gsub("What.other.digital.services.tools.do.you.use.for.learning.", "digi.tools.for.learn", names(dot.tech.sm.learn.full.dt))

names(dot.tech.sm.learn.full.dt) <- gsub("If.you.have.participated.an.online.course..what.did.you.think.of.it.....What.was.the.most.challenging.part.of.participating.in.an.online.course.", "chlnge.in.online.course", names(dot.tech.sm.learn.full.dt))

names(dot.tech.sm.learn.full.dt) <- gsub("If.you.have.participated.an.online.course..what.did.you.think.of.it.....What.was.the.best.part.about.participating.in.an.online.course.", "best.in.online.course", names(dot.tech.sm.learn.full.dt))

names(dot.tech.sm.learn.full.dt) <- gsub("If.you.have.participated.an.online.course..what.did.you.think.of.it.....Would.you.seek.out.an.online.course.again..Why.or.why.not.", "seek.again.online.course", names(dot.tech.sm.learn.full.dt))

# raw.vect2<-dot.tech.sm.learn.full.dt[(seek.again.online.course != "") & (Age.Range > 2), list(ID, GenderC, LocationRespondent,seek.again.online.course), by = AgeC]
# temp2<-textProcessor(documents=raw.vect2$seek.again.online.course,metadata=raw.vect2)
# raw2.vect2<-raw.vect2[seek.again.online.course != " "]
# 
# meta2<-temp2$meta
# vocab2<-temp2$vocab
# docs2<-temp2$documents
# out2 <- prepDocuments(docs2, vocab2, meta2)
# docs2<-out2$documents
# vocab2<-out2$vocab
# meta2 <-out2$meta
# temp2<-NULL
# 
# 
# seek.again.online.course.reason<- stm(out2$documents, out2$vocab, K = 5,prevalence =~ AgeC , content =~ AgeC,
#                                     data = out2$meta, init.type = "Spectral")
# 
# png("WC-seek-again-online-course.png", width = 1200, height = 1000,res=150)
# 
# cloud(seek.again.online.course.reason, scale = c(3,1),colors=brewer.pal(6,"Dark2"))
# 
# dev.off()
# 
# png("Labels-seek-again-online-course.png", width = 1200, height = 1000,res=120)
# #  
# plot.STM(seek.again.online.course.reason, type = "labels", main= "reasons seek again or not an online course ",cex = 0.5)
# 
# dev.off()
# 
# 
# png("Perspectives-prev-seek-again-online-course.png", width = 1200, height = 1200,res=200)
# #  
# plot.STM(seek.again.online.course.reason, type = "perspectives", topics = 5,main = "reasons to seek or not an online course",cex = 5)
# 
# dev.off()
# 
# 
# 
# prep <- estimateEffect(1:5 ~ AgeC , seek.again.online.course.reason,
#                        meta = out2$meta, uncertainty = "Global")
# png("Contrast-seek-online-course.png", width = 1500, height = 800,res=100)
# 
# plot.estimateEffect(prep, covariate = "AgeC", topics = c(1,3,5),
#                     model = seek.again.online.course.reason, method = "difference",
#                     cov.value1 = "25-30", cov.value2 = "31+",
#                     xlab = "More 25-30 ... More 31+",
#                     main = "Effect of Age on Vocab used",
#                     xlim = c(-0.8, .5))
# 
# dev.off()
# 
###############end of open-ended-responses for Q5######################

#####################How often you use Internet for learning################
#"How.often.do.you.use.the.Internet.for.learning...Very.often." 

use.freq.for.lrn = matrix(c("Very.often","Often","Somewhat.often",
                           "Not.very.often","Never" ,"I.don.t.know"),ncol = 1, byrow =T)

intrnt.freq.of.use<-as.table(use.freq.for.lrn)

names(dot.tech.sm.learn.full.dt) <- gsub("How.often.do.you.use.the.Internet.for.learning", "intrnt.use.for.lrn.freq", names(dot.tech.sm.learn.full.dt))

dot.intrnt.use.for.lrn.dt<-subset(dot.tech.sm.learn.full.dt, select = grepl("ID|Gender|Age|Location|Geo|intrnt.use.for.lrn.freq", names(dot.tech.sm.learn.full.dt)))


cols.1 <-colnames(dot.intrnt.use.for.lrn.dt)[grep("intrnt.use.for.lrn.freq",names(dot.intrnt.use.for.lrn.dt))]

dot.intrnt.use.for.lrn.dt.m<-data.table(melt(dot.intrnt.use.for.lrn.dt,measure.vars = cols.1 ,
                                          variable.name = "inrnt.use.freq", value.name = "use.intrnt.freq"))
one.youth.intnt.use.case.m0<-dot.intrnt.use.for.lrn.dt.m[ID ==43675088 , list(inrnt.use.freq,use.intrnt.freq)]

#http://stackoverflow.com/questions/28230360/using-grepl-in-r-to-match-string
# 
# #add access.freq column
# 
dot.intrnt.use.for.lrn.dt.m$frq.use.for.learn.int = apply(dot.intrnt.use.for.lrn.dt.m, 1, function(u){
  bool = sapply(intrnt.freq.of.use[,1], function(x) grepl(x, u[['inrnt.use.freq']]))
  if(any(bool) & (!is.na(u[['use.intrnt.freq']]))) intrnt.freq.of.use[bool] else NA
})
#one.youth.act.case.m<-dot.youth.work.edev.dt.m[ID ==43675088 , list(work,work.freq)]

# #remove the measure cols and value cols from dcast formula
id.cols.use.for.lrn.int.m <- colnames(dot.intrnt.use.for.lrn.dt.m)[-grep("inrnt.use.freq|use.intrnt.freq",names(dot.intrnt.use.for.lrn.dt.m))]

f <- as.formula(paste(paste(id.cols.use.for.lrn.int.m, collapse = " + "), "~ inrnt.use.freq"))

dot.intrnt.use.for.lrn.dt<-data.table(dcast(data =dot.intrnt.use.for.lrn.dt.m[!is.na(use.intrnt.freq),], f, value.var = "use.intrnt.freq",function(x) length(unique(x))))


dot.intrnt.use.for.lrn.dt<-dot.intrnt.use.for.lrn.dt[, which(grepl("intrnt.use.for.lrn.freq...", colnames(dot.intrnt.use.for.lrn.dt))):=NULL]
one.youth.use.intnt.for.lrn<-dot.intrnt.use.for.lrn.dt[ID ==43675088 , list(frq.use.for.learn.int)]
#
dot.intrnt.use.for.lrn.dt.m<-NULL
id.cols.use.for.lrn.int.m<-NULL
f<-NULL


#plot for inrnt use for learning frequencies

youth.use.intrnt.for.lrn.A<-dot.intrnt.use.for.lrn.dt[, list(Frequency = length(ID)),by = c( "AgeC","frq.use.for.learn.int")]


DataA <- ddply(youth.use.intrnt.for.lrn.A, .(AgeC,frq.use.for.learn.int), 
               transform, pos = cumsum(Frequency) - (0.5 * Frequency)
)
png("youth-intrnt-use-for-lrng-by-Age.png", width = 1200, height = 800,res=110)

gg<-ggplot(DataA, aes(x = frq.use.for.learn.int,y=Frequency  )) +
  geom_bar(aes(fill = frq.use.for.learn.int), stat="identity") +
  geom_text(aes(label = ifelse(Frequency >= 5, Frequency,""), y = pos), size = 3) +
  facet_wrap(~AgeC,scales = "free_y" ) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("How does using Internet for learning vary by Age") +scale_fill_brewer(palette="Set2")

print(gg)
dev.off()

youth.use.intrnt.for.lrn.G<-dot.intrnt.use.for.lrn.dt[, list(Frequency = length(ID)),by = c( "GenderC","frq.use.for.learn.int")]


DataG <- ddply(youth.use.intrnt.for.lrn.G, .(GenderC,frq.use.for.learn.int), 
               transform, pos = cumsum(Frequency) - (0.5 * Frequency)
)
png("youth-intrnt-use-for-lrng-by-Gender.png", width = 1200, height = 800,res=110)

gg<-ggplot(DataG, aes(x = frq.use.for.learn.int,y=Frequency  )) +
  geom_bar(aes(fill = frq.use.for.learn.int), stat="identity") +
  geom_text(aes(label = ifelse(Frequency >= 5, Frequency,""), y = pos), size = 3) +
  facet_wrap(~GenderC,scales = "free_y" ) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("How does using Internet for learning vary by Gender") +scale_fill_brewer(palette="Set2")


print(gg)
dev.off()


youth.use.intrnt.for.lrn.L<-dot.intrnt.use.for.lrn.dt[, list(Frequency = length(ID)),by = c( "LocationRespondent","frq.use.for.learn.int")]


DataL <- ddply(youth.use.intrnt.for.lrn.L, .(LocationRespondent,frq.use.for.learn.int), 
               transform, pos = cumsum(Frequency) - (0.5 * Frequency)
)
png("youth-intrnt-use-for-lrng-by-Location.png", width = 1200, height = 800,res=110)

gg<-ggplot(DataL, aes(x = frq.use.for.learn.int,y=Frequency  )) +
  geom_bar(aes(fill = frq.use.for.learn.int), stat="identity") +
  geom_text(aes(label = ifelse(Frequency >= 5, Frequency,""), y = pos), size = 3) +
  facet_wrap(~LocationRespondent,scales = "free_y" ) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("How does using frequencies of Internet for learning vary by Location") +scale_fill_brewer(palette="Set2")


print(gg)
dev.off()


##############frequency of digital tools used for learning###############
#####How.often.do.you.use.the.following.digital.services.tools.for.learning....#############
digi.tools= matrix(c("Facebook","YouTube","LinkedIn","Podcasts",
                               "Free.online.courses", "Paid.online.courses" ,
                               "Wikipedia","Google" ), ncol = 1, byrow= T)
digi.tools.for.lrn<-as.table(digi.tools)

digi.tool.freq.of.use<-as.table(use.freq.for.lrn)

names(dot.tech.sm.learn.full.dt) <- gsub("How.often.do.you.use.the.following.digital.services.tools.for.learning....", "dig.tools.use.for.lrn.freq", names(dot.tech.sm.learn.full.dt))

dot.digi.tools.use.for.lrn.dt<-subset(dot.tech.sm.learn.full.dt, select = grepl("ID|Gender|Age|Location|Geo|dig.tools.use.for.lrn.freq", names(dot.tech.sm.learn.full.dt)))


cols.2 <-colnames(dot.digi.tools.use.for.lrn.dt)[grep("dig.tools.use.for.lrn.freq",names(dot.digi.tools.use.for.lrn.dt))]

dot.digi.tools.use.for.lrn.dt.m<-data.table(melt(dot.digi.tools.use.for.lrn.dt,measure.vars = cols.2 ,
                                             variable.name = "dig.tools.use.for.lrn.freq", value.name = "use.dig.tools.freq"))
one.youth.dig.use.case.m0<-dot.digi.tools.use.for.lrn.dt.m[ID ==43675088 , list(dig.tools.use.for.lrn.freq,use.dig.tools.freq)]

#http://stackoverflow.com/questions/28230360/using-grepl-in-r-to-match-string
# 
dot.digi.tools.use.for.lrn.dt.m$digi.tools = apply(dot.digi.tools.use.for.lrn.dt.m, 1, function(u){
  bool = sapply(digi.tools.for.lrn[,1], function(x) grepl(x, u[['dig.tools.use.for.lrn.freq']]))
  if(any(bool)&(!is.na(u[['use.dig.tools.freq']]))) digi.tools.for.lrn[bool] else NA
})

# #add access.freq column
# 
dot.digi.tools.use.for.lrn.dt.m$digi.tool.lrn.use.freq = apply(dot.digi.tools.use.for.lrn.dt.m, 1, function(u){
  bool = sapply(digi.tool.freq.of.use[,1], function(x) grepl(x, u[['dig.tools.use.for.lrn.freq']]))
  if(any(bool) & (!is.na(u[['use.dig.tools.freq']]))) digi.tool.freq.of.use[bool] else NA
})
one.youth.digi.case.m<-dot.digi.tools.use.for.lrn.dt.m[ID ==43675088 , list(digi.tools,digi.tool.lrn.use.freq)]
# 
# #remove the measure cols and value cols from dcast formula
id.cols.use.for.lrn.dig.m <- colnames(dot.digi.tools.use.for.lrn.dt.m)[-grep("dig.tools.use.for.lrn.freq|use.dig.tools.freq",names(dot.digi.tools.use.for.lrn.dt.m))]

f <- as.formula(paste(paste(id.cols.use.for.lrn.dig.m, collapse = " + "), "~ dig.tools.use.for.lrn.freq"))

dot.digi.tools.use.for.lrn.dt<-data.table(dcast(data =dot.digi.tools.use.for.lrn.dt.m[!is.na(use.dig.tools.freq),], f, value.var = "use.dig.tools.freq",function(x) length(unique(x))))

dot.digi.tools.use.for.lrn.dt.m<-NULL

dot.digi.tools.use.for.lrn.dt<-dot.digi.tools.use.for.lrn.dt[, which(grepl("dig.tools.use.for.lrn.freq|use.dig.tools.freq", colnames(dot.digi.tools.use.for.lrn.dt))):=NULL]
one.youth.use.digi.for.lrn<-dot.digi.tools.use.for.lrn.dt[ID ==43675088 , list(digi.tools,digi.tool.lrn.use.freq)]
#
id.cols.use.for.lrn.dig.m<-NULL
f<-NULL


youth.use.digi.tools.for.lrn.L<-dot.digi.tools.use.for.lrn.dt[, list(Frequency = length(ID)),by = c( "LocationRespondent","digi.tools","digi.tool.lrn.use.freq")]


DataL.dig <- ddply(youth.use.digi.tools.for.lrn.L, .(digi.tools,digi.tool.lrn.use.freq), 
               transform, pos = cumsum(Frequency) - (0.5 * Frequency)
)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

png("youth-dig-tools-use-for-lrng-by-Location.png", width = 1200, height = 800,res=110)

gg<-ggplot(DataL.dig, aes(x = digi.tool.lrn.use.freq,y=Frequency  )) +
  geom_bar(aes(fill = LocationRespondent), stat="identity") +
  geom_text(aes(label = ifelse(Frequency >= 10, Frequency,""), y = pos), size = 3) +
  facet_wrap(~digi.tools,scales = "free_y" ) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("How does using frequencies of digital tools for learning vary by Location") + scale_fill_manual(values=cbPalette)


print(gg)
dev.off()


youth.use.digi.tools.for.lrn.G<-dot.digi.tools.use.for.lrn.dt[, list(Frequency = length(ID)),by = c( "GenderC","digi.tools","digi.tool.lrn.use.freq")]


DataG.dig <- ddply(youth.use.digi.tools.for.lrn.G, .(digi.tools,digi.tool.lrn.use.freq), 
                   transform, pos = cumsum(Frequency) - (0.5 * Frequency)
)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

png("youth-dig-tools-use-for-lrng-by-Gender.png", width = 1200, height = 800,res=110)

gg<-ggplot(DataG.dig, aes(x = digi.tool.lrn.use.freq,y=Frequency  )) +
  geom_bar(aes(fill = GenderC), stat="identity") +
  geom_text(aes(label = ifelse(Frequency >= 10, Frequency,""), y = pos), size = 3) +
  facet_wrap(~digi.tools,scales = "free_y" ) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("How does using frequencies of digital tools for learning vary by Gender") + scale_fill_brewer(palette="Pastel1")


print(gg)
dev.off()

youth.use.digi.tools.for.lrn.A<-dot.digi.tools.use.for.lrn.dt[, list(Frequency = length(ID)),by = c( "AgeC","digi.tools","digi.tool.lrn.use.freq")]


DataA.dig <- ddply(youth.use.digi.tools.for.lrn.A, .(digi.tools,digi.tool.lrn.use.freq), 
                   transform, pos = cumsum(Frequency) - (0.5 * Frequency)
)
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

png("youth-dig-tools-use-for-lrng-by-Age.png", width = 1200, height = 800,res=110)

gg<-ggplot(DataA.dig, aes(x = digi.tool.lrn.use.freq,y=Frequency  )) +
  geom_bar(aes(fill = AgeC), stat="identity") +
  geom_text(aes(label = ifelse(Frequency >= 10, Frequency,""), y = pos), size = 3) +
  facet_wrap(~digi.tools,scales = "free_y" ) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("How do use frequencies of digital tools for learning vary by Age") + scale_fill_manual(values=cbPalette)


print(gg)
dev.off()

