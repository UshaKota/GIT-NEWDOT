library(data.table)
library(plyr)
library(stringr)
library(stringi)
library(tidyr)
library(reshape2)
library(vcd)
library(lattice)
library(MASS)

#for text mining
library(igraph)
library(fpc)


library(corrplot)
library(ggplot2)
library (stm) #for open ended-text survey responses
library(RColorBrewer)



# "tm", "SnowballC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
# "cluster", "igraph", "fpc"

filename = "dotnewData.RDS"


if(file.exists(filename)){
  print("fileexists")
  #create dot survey response data set from the RDS
  dotsr<-readRDS(filename)
  
} else{
  
  
  dotsr <- data.table(read.csv("DOTCleanedData1.csv",skip = 1,stringsAsFactors = F))
  saveRDS(dotsr, "dotnewData.RDS")
  
}

#Variables for 3rd Research Q 
#In what ways are youth in these countries using technology and social media to support or engage in formal work? Informal work?
# for formal work
#In what ways do you use devices for work?
#How often do you use any electronic devices for the following work related activities
#How often do you use the Internet for the following activities?
#What other ways you use the Internet for work?
#What digital services or tools do you use for work?
#What is your favourite digital service or tool for:

#for informal work
#In what ways do you use devices to generate an income?
#In what ways do you use the Internet to generate an income?
#If you use the Internet to generate an income, how did you first learn
#about the opportunity?
# In what ways do you see your friends and/or peers using the
# Internet to generate an income?
# In what ways do you use digital services/tools to generate an
# in
# In what ways do you see your friends and/or peers using the digital
# services/tools to generate an income?come?

# What is the best way you know of to generate an income using
# technology, the Internet, or digital services/tools?

# If you or someone you know does online work to generate an
# income, how did you/they connect to that opportunity?


#rename the X column to ID
rename(dotsr,replace = c("X" = "ID"))

#Get only relevant columns from the dot survey response data set
dot.tech.sm.work.man.dt<-subset(dotsr, select = grepl("ID|Gender|Age|Location|Geo", names(dotsr)))

dot.tech.sm.work.dt<-dotsr[,960:1144,with=F]

#check all the variables chosen for this Q
dot.tech.sm.work.var.dt<-data.table(var.name = colnames(dot.tech.sm.work.dt),var.type = sapply(dot.tech.sm.work.dt,class))


names(dot.tech.sm.work.dt) <- gsub("In.what.ways.do.you.use.devices.for.work", "dev.for.work", names(dot.tech.sm.work.dt))
dot.tech.sm.work.full.dt<-cbind(dot.tech.sm.work.man.dt,dot.tech.sm.work.dt)

#remove NAs in mandatory cols
dot.tech.sm.work.full.dt<-dot.tech.sm.work.full.dt[rowSums(is.na(dot.tech.sm.work.full.dt[,1:9, with = F]))==0,,drop =F]

#remove NA when count exceeds other variables
dot.tech.sm.work.full.dt<-dot.tech.sm.work.full.dt[rowSums(is.na(dot.tech.sm.work.full.dt[,10:194, with = F])) != 184,,drop =F]





#******************proceed to process rest of the variables for Q2###########

#create the frequency table
 work.freq = matrix(c("Very.often","Often","Somewhat.often",
                      "Not.very.often","Never" ,"I.don.t.know"),ncol = 1, byrow =T)


freq.of.work<-as.table(work.freq)

 work = matrix(c( "Word.processing",
                       "To.manage.budgets",
                       "Business.communication",
                       "Task.management", 
                       "Project.planning",
                       "Scheduling", 
                       "Keeping.track.of.how.many.hours.I.work",
                       "Communicating.with.coworkers",
                       "To.keep.track.of.my.income", 
                       "SMS..text.messaging.",
                       "Phone.calls.for.work"),ncol=1,byrow=T)
 work.type<-as.table(work)
 
 
 #basic clean up of variable names
 
 dot.youth.work.edev.dt<-subset(dot.tech.sm.work.full.dt, select = grepl("ID|Gender|Age|Location|Geo|How.often.do.you.use.any.electronic.devices.for.the.following.work.related.activities....", names(dot.tech.sm.work.full.dt)))
 
 names(dot.youth.work.edev.dt) <- gsub("How.often.do.you.use.any.electronic.devices.for.the.following.work.related.activities....", "acti.type.time.", names(dot.youth.work.edev.dt))
 #one.youth.act.case.m00<-dot.youth.work.edev.dt[ID ==43675088 ,]
 
 
 #reshape the subset
 cols.1 <-colnames(dot.youth.work.edev.dt)[grep("acti.type.time.",names(dot.youth.work.edev.dt))]

 dot.youth.work.edev.dt.m<-data.table(melt(dot.youth.work.edev.dt,measure.vars = cols.1 ,
                                       variable.name = "act.type.freq", value.name = "act.freq"))
 one.youth.act.case.m0<-dot.youth.work.edev.dt.m[ID ==43675088 , list(act.type.freq,act.freq)]
 
 #http://stackoverflow.com/questions/28230360/using-grepl-in-r-to-match-string
 dot.youth.work.edev.dt.m$work = apply(dot.youth.work.edev.dt.m, 1, function(u){
   bool = sapply(work.type[,1], function(x) grepl(x, u[['act.type.freq']]))
   if(any(bool)&(!is.na(u[['act.freq']]))) work.type[bool] else NA
 })
 
 # 
 # #add access.freq column
 # 
 dot.youth.work.edev.dt.m$work.freq = apply(dot.youth.work.edev.dt.m, 1, function(u){
   bool = sapply(freq.of.work[,1], function(x) grepl(x, u[['act.type.freq']]))
   if(any(bool) & (!is.na(u[['act.freq']]))) freq.of.work[bool] else NA
 })
 #one.youth.act.case.m<-dot.youth.work.edev.dt.m[ID ==43675088 , list(work,work.freq)]
 
 # #remove the measure cols and value cols from dcast formula
 id.cols.work.m <- colnames(dot.youth.work.edev.dt.m)[-grep("act.type.freq|act.freq",names(dot.youth.work.edev.dt.m))]
 
 f <- as.formula(paste(paste(id.cols.work.m, collapse = " + "), "~ act.type.freq"))
 
 dot.youth.work.edev.dt<-data.table(dcast(data =dot.youth.work.edev.dt.m[!is.na(act.freq),], f, value.var = "act.freq",function(x) length(unique(x))))
 
 dot.youth.work.edev.dt.m<-NULL
 
 dot.youth.work.edev.dt<-dot.youth.work.edev.dt[, which(grepl("acti.type.time.", colnames(dot.youth.work.edev.dt))):=NULL]
 one.youth.case<-dot.youth.work.edev.dt[ID ==43675088 , list(work,work.freq)]
 #
 id.cols.work.m<-NULL
 f<-NULL
 
 
###############Process for How Often you use Internet for following activities#####################
 
 #Internet.for.acti.
 internet.for.actvts<-matrix(c("To.communicate.with.my.coworkers",
                               "Research.related.to.my.job",
                               "Research.to.improve.my.job.performance", 
                               "Connecting.with.others.in.my.field",
                               "Connecting.with.mentors",
                               "Training.for.my.job",
                               "To.track.my.performance.and.or.hours.worked",
                               "To.communicate.with.customers.or.clients",
                               "To.plan.events.for.work", 
                               "Accessing.professional.development.resources",
                               "Learning.for.professional.development",
                               "To.apply.for.jobs"  ),ncol=1,byrow=T)
 
 intrnt.for.actvts<-as.table(internet.for.actvts)
 
 dot.youth.intrnt.act.dt<-subset(dot.tech.sm.work.full.dt, select = grepl("ID|Gender|Age|Location|Geo|How.often.do.you.use.the.Internet.for.the.following.activities....", names(dot.tech.sm.work.full.dt)))
 
 names(dot.youth.intrnt.act.dt) <- gsub("How.often.do.you.use.the.Internet.for.the.following.activities....", "Internet.for.acti.", names(dot.youth.intrnt.act.dt))
 one.youth.intrnt.case.m00<-dot.youth.intrnt.act.dt[ID ==43675088 ,]
 
 
 #reshape the subset
 cols.2 <-colnames(dot.youth.intrnt.act.dt)[grep("Internet.for.acti.",names(dot.youth.intrnt.act.dt))]
 
 dot.youth.intrnt.act.dt.m<-data.table(melt(dot.youth.intrnt.act.dt,measure.vars = cols.2 ,
                                           variable.name = "intrnt.fol.act", value.name = "intrnt.act.freq"))
 one.youth.intnt.act.case.m0<-dot.youth.intrnt.act.dt.m[ID ==43675088 , list(intrnt.fol.act,intrnt.act.freq)]
 
#  #http://stackoverflow.com/questions/28230360/using-grepl-in-r-to-match-string
 dot.youth.intrnt.act.dt.m$use.intrnt.act = apply(dot.youth.intrnt.act.dt.m, 1, function(u){
   bool = sapply(intrnt.for.actvts[,1], function(x) grepl(x, u[['intrnt.fol.act']]))
   if(any(bool)&(!is.na(u[['intrnt.act.freq']]))) intrnt.for.actvts[bool] else NA
 })
 
 # 
 # #add access.freq column
 # 
 dot.youth.intrnt.act.dt.m$act.freq.with.intrnt = apply(dot.youth.intrnt.act.dt.m, 1, function(u){
   bool = sapply(freq.of.work[,1], function(x) grepl(x, u[['intrnt.fol.act']]))
   if(any(bool) & (!is.na(u[['intrnt.act.freq']]))) freq.of.work[bool] else NA
 })
 one.youth.intnt.case.m<-dot.youth.intrnt.act.dt.m[ID ==43675088 , list(use.intrnt.act,act.freq.with.intrnt)]
 
#  # #remove the measure cols and value cols from dcast formula
 id.cols.intrnt.m <- colnames(dot.youth.intrnt.act.dt.m)[-grep("intrnt.act.freq|intrnt.fol.act",names(dot.youth.intrnt.act.dt.m))]
 
 f <- as.formula(paste(paste(id.cols.intrnt.m, collapse = " + "), "~ intrnt.fol.act"))
 
 dot.youth.intrnt.act.dt<-data.table(dcast(data =dot.youth.intrnt.act.dt.m[!is.na(intrnt.act.freq),], f, value.var = "intrnt.act.freq",function(x) length(unique(x))))
 
 dot.youth.intrnt.act.dt.m<-NULL
 
 dot.youth.intrnt.act.dt<-dot.youth.intrnt.act.dt[, which(grepl("Internet.for.acti.", colnames(dot.youth.intrnt.act.dt))):=NULL]
 one.youth.inrnt.case<-dot.youth.intrnt.act.dt[ID ==43675088 , list(use.intrnt.act,act.freq.with.intrnt)]
 #
 id.cols.intrnt.m<-NULL
 f<-NULL
 
 
 
##################process open-ended text input responses again######################
##########What other waus do you use devices for work##################
 
#  raw.vect1<-dot.tech.sm.work.full.dt[(dev.for.work. != "") & (Age.Range > 2), list(ID, GenderC, LocationRespondent,dev.for.work.), by = AgeC]
#  temp1<-textProcessor(documents=raw.vect1$dev.for.work.,metadata=raw.vect1)
#  raw2.vect1<-raw.vect1[dev.for.work. != " "]
#  
#  meta1<-temp1$meta
#  vocab1<-temp1$vocab
#  docs1<-temp1$documents
#  out1 <-prepDocuments(docs1, vocab1, meta1)
#  docs1<-out1$documents
#  vocab1<-out1$vocab
#  meta1 <-out1$meta
#  temp1<-NULL
#  
#  
#  
#  #set.seed(01234)
#  devworkcontent <- stm(out1$documents, out1$vocab, K = 5,prevalence =~ AgeC , content =~ AgeC,
#                        data = out1$meta, init.type = "Spectral")
#  
# #  devworkcontent.select <- selectModel(out1$documents, out1$vocab, K = 5,
# #                                prevalence =~ as.factor(AgeC) , data = out1$meta, seed = 8458159)
# #                                
#  png("Labels-dev-use-for-work.png", width = 1200, height = 1000,res=160)
#  #  
#  plot.STM(devworkcontent, type = "labels", main= "Other Ways of youth using devices for work",cex = 0.5)
#  
#  dev.off()
#  
#   
#  png("Perspectives-prev-dev-use-for-work.png", width = 1200, height = 1000,res=160)
#  #  
#  plot.STM(devworkcontent, type = "perspectives", topics = 5,main = "Other Ways of youth using devices for work")
# 
#  dev.off()
#  
#  
#  
#  prep <- estimateEffect(1:5 ~ AgeC , devworkcontent,
#                         meta = out1$meta, uncertainty = "Global")
#  png("Contrast-covariate-dev-use-for-work.png", width = 1200, height = 800,res=100)
#  
#  plot.estimateEffect(prep, covariate = "AgeC", topics = c(1,3,5),
#                      model = devworkcontent, method = "difference",
#                      cov.value1 = "25-30", cov.value2 = "31+",
#                      xlab = "More 25-30 ... More 31+",
#                      main = "Effect of Age on Vocab used",
#                      xlim = c(-.1, .1))
#  
#  dev.off()
# #  p<-cloud(devworkcontent, topic = 5, scale = c(2,.25),colors=brewer.pal(6,"Dark2"))
# #  
#  
###########"What.other.ways.you.use.the.Internet.for.work."###########################
 
#  names(dot.tech.sm.work.full.dt) <- gsub("What.other.ways.you.use.the.Internet.for.work.", "intrnt.other.ways.for.work", names(dot.tech.sm.work.full.dt))
#  
#  raw.vect2<-dot.tech.sm.work.full.dt[(intrnt.other.ways.for.work != "") & (Age.Range > 2), list(ID, GenderC, AgeC,LocationRespondent,intrnt.other.ways.for.work), by = AgeC]
#  temp2<-textProcessor(documents=raw.vect2$intrnt.other.ways.for.work,metadata=raw.vect2)
#  raw2.vect2<-raw.vect2[intrnt.other.ways.for.work != " "]
#  
#  meta2<-temp2$meta
#  vocab2<-temp2$vocab
#  docs2<-temp2$documents
#  out2 <- prepDocuments(docs2, vocab2, meta2)
#  docs2<-out2$documents
#  vocab2<-out2$vocab
#  meta2 <-out2$meta
#  temp2<-NULL
#  
#  
#  intrnt.work.content <- stm(out2$documents, out2$vocab, K = 5,prevalence =~ AgeC , content =~ AgeC,
#                        data = out2$meta, init.type = "Spectral")
#  
#                                
#  png("Labels-inrnt-use-for-work.png", width = 1200, height = 1000,res=160)
#  #  
#  plot.STM(intrnt.work.content, type = "labels", main= "Other Ways of youth using Internet for work",cex = 0.5)
#  
#  dev.off()
#  
#  
#  png("Perspectives-prev-intrnt-use-for-work.png", width = 1200, height = 1000,res=160)
#  #  
#  plot.STM(intrnt.work.content, type = "perspectives", topics = 5,main = "Other Ways of youth using Internet for work")
#  
#  dev.off()
#  
#  
#  
#  prep <- estimateEffect(1:5 ~ AgeC , intrnt.work.content,
#                         meta = out2$meta, uncertainty = "Global")
#  png("Contrast-covariate-intrnt-use-for-work.png", width = 1500, height = 800,res=100)
#  
#  plot.estimateEffect(prep, covariate = "AgeC", topics = c(1,3,5),
#                      model = intrnt.work.content, method = "difference",
#                      cov.value1 = "25-30", cov.value2 = "31+",
#                      xlab = "More 25-30 ... More 31+",
#                      main = "Effect of Age on Vocab used",
#                      xlim = c(-0.5, .1))
#  
#  dev.off()
# #  #
# #What.digital.services.or.tools.do.you.use.for.work." "character"##############
 
#  names(dot.tech.sm.work.full.dt) <- gsub("What.digital.services.or.tools.do.you.use.for.work.", "dig.ser.use.for.work", names(dot.tech.sm.work.full.dt))
#  
#  raw.vect3<-dot.tech.sm.work.full.dt[(dig.ser.use.for.work != "") & (Age.Range > 2), list(ID, GenderC,LocationRespondent,dig.ser.use.for.work), by = AgeC]
#  temp3<-textProcessor(documents=raw.vect3$dig.ser.use.for.work,metadata=raw.vect3)
#  
#  meta3<-temp3$meta
#  vocab3<-temp3$vocab
#  docs3<-temp3$documents
#  out3 <- prepDocuments(docs3, vocab3, meta3)
#  docs3<-out3$documents
#  vocab3<-out3$vocab
#  meta3 <-out3$meta
#  temp3<-NULL
#  
#  
#  
#  digi.tools.work.content <- stm(out3$documents, out3$vocab, K = 5,prevalence =~ AgeC , content =~ AgeC,
#                             data = out3$meta, init.type = "Spectral")
#  
#  
#  png("Labels-digi-tools-use-for-work.png", width = 1200, height = 1000,res=160)
#  #  
#  plot.STM(digi.tools.work.content, type = "labels", main= "Other Ways of youth using digital tools for work",cex = 0.5)
#  
#  dev.off()
#  
#  
#  png("Perspectives-prev-digitools-use-for-work.png", width = 1200, height = 1000,res=160)
#  #  
#  plot.STM(digi.tools.work.content, type = "perspectives", topics = 5,main = "Other Ways of youth using Internet for work")
#  
#  dev.off()
#  
#  
#  prep <- estimateEffect(1:5 ~ AgeC , digi.tools.work.content,
#                         meta = out3$meta, uncertainty = "Global")
#  png("Contrast-covariate-digi-tools-use-for-work.png", width = 1500, height = 800,res=100)
#  
#  plot.estimateEffect(prep, covariate = "AgeC", topics = c(1,3,5),
#                      model = digi.tools.work.content, method = "difference",
#                      cov.value1 = "25-30", cov.value2 = "31+",
#                      xlab = "More 25-30 ... More 31+",
#                      main = "Effect of Age on Vocab used",
#                      xlim = c(-0.5, .1))
#  
#  dev.off()
 
 
 #################### more##################################
#  "165" "What.is.your.favourite.digital.service.or.tool.for....Connecting.with.managers..supervisors..or.bosses" "character"
#  "166" "What.is.your.favourite.digital.service.or.tool.for....Connecting.with.coworkers" "character"
#  "167" "What.is.your.favourite.digital.service.or.tool.for....Connecting.with.clients.or.customers" "character"
#  "168" "What.is.your.favourite.digital.service.or.tool.for....Professional.development" "character"
#  "169" "What.is.your.favourite.digital.service.or.tool.for....Work.related.learning" "character"
 names(dot.tech.sm.work.full.dt) <- gsub("What.is.your.favourite.digital.service.or.tool.for....Connecting.with.managers..supervisors..or.bosses", "fav.tool.to.connect.ppl.top", names(dot.tech.sm.work.full.dt))
 names(dot.tech.sm.work.full.dt) <- gsub("What.is.your.favourite.digital.service.or.tool.for....Connecting.with.coworkers", "fav.tool.to.connect.coworkers", names(dot.tech.sm.work.full.dt))
 
 names(dot.tech.sm.work.full.dt) <- gsub("What.is.your.favourite.digital.service.or.tool.for....Connecting.with.clients.or.customers", "fav.tool.to.connect.clients", names(dot.tech.sm.work.full.dt))
 
 names(dot.tech.sm.work.full.dt) <- gsub("What.is.your.favourite.digital.service.or.tool.for....Professional.development", "fav.tool.for.prof.dev", names(dot.tech.sm.work.full.dt))
 
 names(dot.tech.sm.work.full.dt) <- gsub("What.is.your.favourite.digital.service.or.tool.for....Work.related.learning", "fav.tool.for.work.rel.lrng", names(dot.tech.sm.work.full.dt))
 
 raw.vect2<-dot.tech.sm.work.full.dt[(fav.tool.for.work.rel.lrng != "") & (Age.Range > 2), list(ID, GenderC, LocationRespondent,fav.tool.for.work.rel.lrng), by = AgeC]
 temp2<-textProcessor(documents=raw.vect2$fav.tool.for.work.rel.lrng,metadata=raw.vect2)
 raw2.vect2<-raw.vect2[fav.tool.for.work.rel.lrng != " "]
 
 meta2<-temp2$meta
 vocab2<-temp2$vocab
 docs2<-temp2$documents
 out2 <- prepDocuments(docs2, vocab2, meta2)
 docs2<-out2$documents
 vocab2<-out2$vocab
 meta2 <-out2$meta
 temp2<-NULL
 
 
 fav.digi.tools.work.rel.lrng <- stm(out2$documents, out2$vocab, K = 5,prevalence =~ AgeC , content =~ AgeC,
                            data = out2$meta, init.type = "Spectral")
 
 
 png("Labels-fav-dig-tools-work-rel-lrng.png", width = 1200, height = 1000,res=120)
 #  
 plot.STM(fav.digi.tools.work.rel.lrng, type = "labels", main= "favorite digitools work related learning ",cex = 0.5)
 
 dev.off()
 
 
 png("Perspectives-prev-fav-digi-tools-work-rel-lrng.png", width = 1200, height = 1000,res=100)
 #  
 plot.STM(fav.digi.tools.work.rel.lrng, type = "perspectives", topics = 5,main = "favorite tools for work related learning")
 
 dev.off()
 
 
 
 prep <- estimateEffect(1:5 ~ AgeC , fav.digi.tools.work.rel.lrng,
                        meta = out2$meta, uncertainty = "Global")
 png("Contrast-covariate-digi-tools-work-rel-lrng.png", width = 1500, height = 800,res=100)
 
 plot.estimateEffect(prep, covariate = "AgeC", topics = c(1,3,5),
                     model = fav.digi.tools.work.rel.lrng, method = "difference",
                     cov.value1 = "25-30", cov.value2 = "31+",
                     xlab = "More 25-30 ... More 31+",
                     main = "Effect of Age on Vocab used",
                     xlim = c(-0.8, .5))
 
 dev.off()
 #