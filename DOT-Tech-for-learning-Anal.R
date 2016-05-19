
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

raw.vect2<-dot.tech.sm.learn.full.dt[(Mobil.phone.learn != "") & (Age.Range > 2), list(ID, GenderC, LocationRespondent,Mobil.phone.learn), by = AgeC]
temp2<-textProcessor(documents=raw.vect2$Mobil.phone.learn,metadata=raw.vect2)
raw2.vect2<-raw.vect2[Mobil.phone.learn != " "]

meta2<-temp2$meta
vocab2<-temp2$vocab
docs2<-temp2$documents
out2 <- prepDocuments(docs2, vocab2, meta2)
docs2<-out2$documents
vocab2<-out2$vocab
meta2 <-out2$meta
temp2<-NULL


dev.for.learn.Mobil.phone<- stm(out2$documents, out2$vocab, K = 5,prevalence =~ AgeC , content =~ AgeC,
                                    data = out2$meta, init.type = "Spectral")

png("WC-mobile-phone-for-lrng.png", width = 1200, height = 1000,res=200)

cloud(dev.for.learn.Mobil.phone, scale = c(3,1),colors=brewer.pal(6,"Dark2"))

dev.off()

png("Labels-mobile-phone-for-lrng.png", width = 1200, height = 1000,res=120)
#  
plot.STM(dev.for.learn.Mobil.phone, type = "labels", main= "Ways of using Mobile Phone for learning ",cex = 0.5)

dev.off()


png("Perspectives-prev-Mobile-phone-for-lrng.png", width = 1200, height = 1200,res=200)
#  
plot.STM(dev.for.learn.Mobil.phone, type = "perspectives", topics = 5,main = "Ways of using Mobile Phone for learning",cex = 5)

dev.off()



prep <- estimateEffect(1:5 ~ AgeC , dev.for.learn.Mobil.phone,
                       meta = out2$meta, uncertainty = "Global")
png("Contrast-covariate-Mobile-Phone-lrng.png", width = 1500, height = 800,res=100)

plot.estimateEffect(prep, covariate = "AgeC", topics = c(1,3,5),
                    model = dev.for.learn.Mobil.phone, method = "difference",
                    cov.value1 = "25-30", cov.value2 = "31+",
                    xlab = "More 25-30 ... More 31+",
                    main = "Effect of Age on Vocab used",
                    xlim = c(-0.8, .5))

dev.off()

