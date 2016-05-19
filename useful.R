#now the clean dot.tech.gender.dt dimensions are 548 x 534. so there were 19 such cases
id.cols.dev.loc <-colnames(dot.tech.gender.dt)[-grep("use.loc.device.",names(dot.tech.gender.dt))]


dot.tech.gender.dt.dev.loc.m<-data.table(melt(dot.tech.gender.dt, id.vars = id.cols.dev.loc, measure.vars = grep("use.loc.device.",names(dot.tech.gender.dt),value=T ),
                                              na.rm=T ,variable.name = "dev.loc", value.name = "loc.freq"))

#http://stackoverflow.com/questions/28230360/using-grepl-in-r-to-match-string
dot.tech.gender.dt.dev.loc.m$use.at = apply(dot.tech.gender.dt.dev.loc.m, 1, function(u){
  bool = sapply(dev.use.loc[,1], function(x) grepl(x, u[['dev.loc']]))
  if(any(bool)&(u[['loc.freq']] == 1))dev.use.loc[bool,] else NA
})

# 
# #add loc.use.freq column
# 
dot.tech.gender.dt.dev.loc.m$location.use.freq = apply(dot.tech.gender.dt.dev.loc.m, 1, function(u){
  bool = sapply(freq[,1], function(x) grepl(x, u[['loc.freq']]))
  if(any(bool) & (u[['loc.freq']] == 1)) freq[bool] else NA
})

id.cols.loc.m <- colnames(dot.tech.gender.dt.dev.loc.m)[-grep("loc.freq|dev.loc",names(dot.tech.gender.dt.dev.loc.m))]

f <- as.formula(paste(paste(id.cols.loc.m, collapse = " + "), "~ dev.loc"))

dot.tech.gender.dt<-data.table(dcast(data =dot.tech.gender.dt.dev.loc.m, f, value.var = "loc.freq",function(x) length(unique(x))))

#http://stackoverflow.com/questions/28230360/using-grepl-in-r-to-match-string
dot.tech.gender.dt.ac.int.m$inrnt.device = apply(dot.tech.gender.dt.ac.int.m, 1, function(u){
  bool = sapply(device[,1], function(x) grepl(x, u[['int.dev.acc']]))
  if(any(bool)&(u[['int.acc.freq']] == 1)) device[bool] else NA
})

# # 
# # #add intrnt.access.freq column
# # 
# dot.tech.gender.dt.ac.int.m$intrnt.freq = apply(dot.tech.gender.dt.ac.int.m, 1, function(u){
#   bool = sapply(freq[,1], function(x) grepl(x, u[['dev.acc']]))
#   if(any(bool) & (u[['int.acc.freq']] == 1)) freq[bool] else NA
# })
# 
# # #remove the measure cols and value cols from dcast formula
# id.cols.inrnt.acc.m <- colnames(dot.tech.gender.dt.ac.int.m)[-grep("int.dev.acc|int.acc.freq",names(dot.tech.gender.dt.ac.int.m))]
# 
# f <- as.formula(paste(paste(id.cols.inrnt.acc.m, collapse = " + "), "~ int.dev.acc"))
# 
# dot.tech.gender.dt<-data.table(dcast(data =dot.tech.gender.dt.ac.int.m, f, value.var = "int.acc.freq",function(x) length(unique(x))))
# 



#*******8
dot.tech.gender.dt.ac.m<-data.table(melt(dot.tech.gender.dt, id.vars = id.cols.acc, measure.vars = grep("acc.dev",names(dot.tech.gender.dt),value=T ),
                                         variable.name = "dev.acc", value.name = "acc.freq"))
one.case<-dot.tech.gender.dt.ac.m[ID ==43912947 , list(dev.acc,acc.freq)]

#http://stackoverflow.com/questions/28230360/using-grepl-in-r-to-match-string
dot.tech.gender.dt.ac.m$device = apply(dot.tech.gender.dt.ac.m, 1, function(u){
  bool = sapply(device[,1], function(x) grepl(x, u[['dev.acc']]))
  if(any(bool)&(!is.na(u[['acc.freq']]))) device[bool] else NA
})

# 
# #add access.freq column
# 
dot.tech.gender.dt.ac.m$dev.avail.freq = apply(dot.tech.gender.dt.ac.m, 1, function(u){
  bool = sapply(freq[,1], function(x) grepl(x, u[['dev.acc']]))
  if(any(bool) & (!is.na(u[['acc.freq']]))) freq[bool] else NA
})
one.acc.case.m<-dot.tech.gender.dt.ac.m[ID ==43912947 , list(dev.acc,acc.freq,device,dev.avail.freq)]

# #remove the measure cols and value cols from dcast formula
id.cols.acc.m <- colnames(dot.tech.gender.dt.ac.m)[-grep("dev.acc|acc.freq",names(dot.tech.gender.dt.ac.m))]

f <- as.formula(paste(paste(id.cols.acc.m, collapse = " + "), "~ dev.acc"))

dot.tech.gender.dt<-data.table(dcast(data =dot.tech.gender.dt.ac.m[!is.na(dev.avail.freq),], f, value.var = "acc.freq",function(x) length(unique(x))))


#drop acc: columns..
#http://stackoverflow.com/questions/24590341/removing-multiple-columns-from-r-data-table-with-parameter-for-columns-to-remove
#dt[, (colsToDelete) := NULL]

dot.tech.gender.dt<-dot.tech.gender.dt[, which(grepl("acc.dev", colnames(dot.tech.gender.dt))):=NULL]
one.acc.case<-dot.tech.gender.dt[ID ==43912947 , list(device,dev.avail.freq)]
#
get.remain.cols<-colnames(dot.tech.gender.dt)[-grep("device|dev.avail.freq",names(dot.tech.gender.dt))]
one.acc.case.u<-unique(dot.tech.gender.dt[ID ==43912947 ,], by = c("device","dev.avail.freq"))


#melt for use device and use freq. columns
id.cols.use  <-colnames(dot.tech.gender.dt)[-grep("dev.use.freq.",names(dot.tech.gender.dt))]

measure.cols.use <- colnames(dot.tech.gender.dt)[grep("dev.use.freq.",names(dot.tech.gender.dt))]


dot.tech.gender.use.m<-data.table(melt(unique(dot.tech.gender.dt, by = c("ID","device", "dev.avail.freq")), id.vars = id.cols.use, measure.vars = measure.cols.use,
                                       variable.name = "dev.use", value.name = "dev.use.freq"))


one.use.case.m<-dot.tech.gender.use.m[ID ==43912947 , list(device,dev.avail.freq,dev.use)]
# 
# 
dot.tech.gender.use.m$use.dev.freq =  apply(dot.tech.gender.use.m, 1, function(u){
  
  bool = sapply(freq[,1], function(x) grepl(x, u[['dev.use']]))
  if(any(bool) & !is.na(u[['dev.use.freq']])) freq[bool] else NA
})

dot.tech.gender.use.m$device.use =  apply(dot.tech.gender.use.m, 1, function(u){
  
  bool = sapply(device[,1], function(x) grepl(x, u[['dev.use']]))
  if(any(bool)&!is.na(u[['dev.use.freq']])) device[bool] else NA
})
one.use.case<-dot.tech.gender.use.m[ID ==43912947 , list(device,dev.avail.freq,dev.use,device.use,use.dev.freq)]
#
# dot.tech.gender.use.b<-dot.tech.gender.use.m
# 
# #some people use devices that they do not have access to.. so add that using device as another device --
# #insert a row for the user when the using device does not match the access device
# 
# 
# #http://stackoverflow.com/questions/15347282/split-string-column-and-insert-as-multiple-new-rows
# 
#get the subset for use frequency
id.cols.use.m <- colnames(dot.tech.gender.use.m)[-grep("dev.use|dev.use.freq",names(dot.tech.gender.use.m))]

f <- as.formula(paste(paste(id.cols.use.m, collapse = " + "), "~ dev.use"))

dot.tech.gender.dt.k<-data.table(dcast(data =dot.tech.gender.use.m[!is.na(use.dev.freq),], f, value.var = "dev.use.freq",function(x) length(unique(x))))

#drop the dev.use.freq. cols

dot.tech.gender.dt<-dot.tech.gender.dt.k[, which(grepl("dev.use|dev.use.freq", colnames(dot.tech.gender.dt.k))):=NULL]
one.use.case.k<-unique(dot.tech.gender.dt[ID ==43912947 ,] , by =  c("device.use","use.dev.freq"))
#
# # one.use.case.c<-dot.tech.gender.use.m[ID ==43912947,list( device,dev.avail.freq,device.use,use.dev.freq)]
# # 
# use.case.m<-unique(dot.tech.gender.dt[,list(Gender, GenderC, AgeRange, AgeC,LocationRespondent, device.use,use.dev.freq), by = ID])
# 
# cols<-c("device.use", "use.dev.freq")
# #acc.case.mm<-unique(dot.tech.gender.dt[, !cols, with=FALSE,by = ID])
# 
# 
# acc.case.m<-unique(dot.tech.gender.dt[,list(Gender,GenderC, AgeRange, AgeC,LocationRespondent, device,dev.avail.freq), by = ID])
# 
# # #rename to merge
# rename(use.case.m, replace = c("device.use" = "device"))
# # 
# acc.use.na<-merge(acc.case.m,use.case.m, by = c("ID", "device"), all.y=T)#-->this one creates NA values for mandatory columns 3:7
#  
# #copy the mandatory column values from .y columnes to merged
# na.cols<-c("Gender.x", "GenderC.x","AgeRange.x" ,"AgeC.x","LocationRespondent.x")
# 
# no.na.cols<-c("Gender.y", "GenderC.y","AgeRange.y" ,"AgeC.y","LocationRespondent.y")
# #http://stackoverflow.com/questions/16943939/elegantly-assigning-multiple-columns-in-data-table-with-lapply
# 
# acc.use.no.na<-acc.use.na[, (na.cols) := lapply(no.na.cols, function(x) {get(x)}),by = ID]
# 
# setnames(acc.use.no.na,old = na.cols, new  = c("Gender", "GenderC", "AgeRange" ,"AgeC","LocationRespondent"))
# 
# #http://stackoverflow.com/questions/24590341/removing-multiple-columns-from-r-data-table-with-parameter-for-columns-to-remove
# acc.use.c<-acc.use.no.na[, (no.na.cols) := NULL]
# 
# #[15] "use.loc.device.AtaninternetcafeOnceamonth"                                                                                     
# 
# 
# #create aux table for location of device use
# 

get.remain.cols<-colnames(dot.tech.gender.dt)[-grep("device|dev.avail.freq|device.use|use.dev.freq",names(dot.tech.gender.dt))]


dev.use.loc <-matrix(c("EverywhereIalwayshaveaninternetenableddevice", "Evrywhr.IE.device",
                       "Atwork", "work",
                       "Athome", "home",
                       "Atafamilymembershouse", "FM house",
                       "Atschool", "school",
                       "Atalibrarycommunitycentreorchurch", "library.CC.Church",
                       "Ataninternetcafe", "ICafe"),ncol = 2, byrow = T)


#now the clean dot.tech.gender.dt dimensions are 548 x 534. so there were 19 such cases
id.cols.dev.loc <-colnames(dot.tech.gender.dt)[-grep("use.loc.device.",names(dot.tech.gender.dt))]


dot.tech.gender.dt.dev.loc.m<-data.table(melt(unique(dot.tech.gender.dt,by =get.remain.cols) , id.vars = id.cols.dev.loc, measure.vars = grep("use.loc.device.",names(dot.tech.gender.dt),value=T ),
                                              variable.name = "dev.loc", value.name = "loc.freq"))

#http://stackoverflow.com/questions/28230360/using-grepl-in-r-to-match-string
dot.tech.gender.dt.dev.loc.m$use.at = apply(dot.tech.gender.dt.dev.loc.m, 1, function(u){
  bool = sapply(dev.use.loc[,1], function(x) grepl(x, u[['dev.loc']]))
  if(any(bool)& !is.na(u[['loc.freq']]))dev.use.loc[bool, 2] else NA
})

# # 
# # #add loc.use.freq column
# # 
dot.tech.gender.dt.dev.loc.m$location.use.freq = apply(dot.tech.gender.dt.dev.loc.m, 1, function(u){
  bool = sapply(freq[,1], function(x) grepl(x, u[['loc.freq']]))
  if(any(bool) & u[['loc.freq']] == 1) freq[bool] else NA
})


#one.use.case.c<-dot.tech.gender.dt.dev.loc.m[ID ==43912947,list(GenderC,AgeC,device,dev.avail.freq,device.use,use.dev.freq,dev.loc,loc.freq,use.at,location.use.freq)]

id.cols.loc.m <- colnames(dot.tech.gender.dt.dev.loc.m)[-grep("loc.freq|dev.loc",names(dot.tech.gender.dt.dev.loc.m))]

f <- as.formula(paste(paste(id.cols.loc.m, collapse = " + "), "~ dev.loc"))

dot.tech.gender.dt<-data.table(dcast(data =dot.tech.gender.dt.dev.loc.m[!is.na(use.at)|!is.na(device)| !is.na(device.use),], f, value.var = "loc.freq",function(x) length(unique(x))))


#clean up for device for internet and internet access freq
# id.cols.acc.int <-colnames(dot.tech.gender.dt)[-grep("acc.to.Internet.",names(dot.tech.gender.dt))]
# 
# 
# dot.tech.gender.dt.ac.int.m<-data.table(melt(unique(dot.tech.gender.dt,by = id.cols.acc.int), id.vars = id.cols.acc.int, measure.vars = grep("acc.to.Internet.",names(dot.tech.gender.dt),value=T ),
#                                              variable.name = "int.dev.acc", value.name = "int.acc.freq"))
# 
#*#



digi.ser<-matrix( c("useSMSTextmessages",                            
                    "takedigitalphotos" ,                              
                    "takedigitalvideos" ,                              
                    "makedigitalpayments",                            
                    "receivedigitalpayments",                         
                    "accesspoliticalnews",                            
                    "accesssocialnetworksFacebookTwitterothers",      
                    "getinformationaboutmyhealth",                     
                    "lookforapplyforjobs",                           
                    "offerprofessionalservicesforafee",              
                    "playgames",                                    
                    "researchtopicsthatinterestme",                    
                    "engageindiscussionswithmypeers",                 
                    "useawordprocessor",                              
                    "usebudgetingtools",                               
                    "createappsforsmartphonesorcomputers",            
                    "buildawebsite",                                   
                    "sellproductsorservicesonline",                  
                    "accessbankingandorfinancialservicesaccountsloans")
                  , ncol=1,byrow =T)



###############clean up access of digital services
#
# dot.gen.acc.digi.dt<-subset(dot.tech.gender.dt, select = grepl("ID|Gender|Age|Location|Geo|acc.digi.tools.", names(dot.tech.gender.dt)))
# dot.gen.loc.digi.dt<-subset(dot.tech.gender.dt, select = grepl("ID|Gender|Age|Location|Geo|loc.use.digi.tools.", names(dot.tech.gender.dt)))

dot.gen.acc.digi.dt.m<-data.table(melt(dot.gen.acc.digi.dt,measure.vars = cols.7 ,
                                       variable.name = "digi.acc", value.name = "digi.tools"))

# dot.gen.acc.digi.dt.m$opts.dig = apply(dot.gen.acc.digi.dt.m, 1, function(u){
#   bool = sapply(digi.ser[,1], function(x) grepl(x, u[['digi.acc']],ignore.case = T))
#   if(any(bool)&(!is.na(u[['digi.tools']]))) digi.ser[bool] else NA
# })

# add digital services. access freq column
# 
dot.gen.acc.digi.dt.m$freq.ser.ak = apply(dot.gen.acc.digi.dt.m, 1, function(u){
  bool = sapply(freq[,1], function(x) grepl(x, u[['digi.acc']],ignore.case = T))
  if(any(bool) & (!is.na(u[['digi.tools']]))) freq[bool] else NA
})
one.digi.acc.case.m<-dot.gen.acc.digi.dt.m[ID ==43912947 , list(digi.acc, digi.tools,freq.ser.ak)]

#
# #remove the measure cols and value cols from dcast formula
id.cols.digi.tools.m <- colnames(dot.gen.acc.digi.dt.m)[-grep("digi.acc|digi.tools",names(dot.gen.acc.digi.dt.m))]
f <- as.formula(paste(paste(id.cols.digi.tools.m, collapse = " + "), "~ digi.acc"))

dot.gen.acc.digi.dt<-data.table(dcast(data = dot.gen.acc.digi.dt.m[!is.na(digi.tools),], f, value.var = "digi.tools",function(x) length(unique(x))))

dot.gen.acc.digi.dt.m<-dot.gen.acc.digi.dt[, which(grepl("acc.digi.tools", colnames(dot.gen.acc.digi.dt))):=NULL]
aa<-dot.gen.acc.digi.dt[ID ==43912947 , list(freq.ser.ak)]
dot.gen.acc.digi.dt.m<-NULL
id.cols.digi.tools.m<-NULL
f<-NULL



####################topic model-plots-code###########################3


#https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html

dot.text.ip.dig.tools.dt<-subset(dotsr, select = grepl("Researcher.notes.for.the..Access|What.other.ways|ID|Gender|Age|Location|Geo", names(dotsr)))

names(dot.text.ip.dig.tools.dt) <- gsub("Researcher.notes.for.the..Access.to.technology..the.Internet..and.digital.services.tools..section", "notes", names(dot.text.ip.dig.tools.dt))

names(dot.text.ip.dig.tools.dt) <- gsub("What.other.ways.you.use.the.Internet.for.work.", "use.ways", names(dot.text.ip.dig.tools.dt))


#https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html
#create the corpus of words from text input columns
# tdm <- Corpus(DataframeSource(dot.text.ip.dig.tools.dt[, list(use.ways)]))
# tdm <- tm_map(tdm, removePunctuation)   # *Removing punctuation:*    
# tdm <- tm_map(tdm, removeNumbers)      # *Removing numbers:*    
# tdm <- tm_map(tdm, tolower)   # *Converting to lowercase:*    
# tdm <- tm_map(tdm, removeWords, stopwords("english"))   # *Removing "stopwords" 
# tdm <- tm_map(tdm, stemDocument)   # *Removing common word endings* (e.g., "ing", "es")   
# tdm <- tm_map(tdm, stripWhitespace)   # *Stripping whitespace   
# tdm <- tm_map(tdm, PlainTextDocument)   
# dtm <- DocumentTermMatrix(tdm)   
# # tdm.f <- TermDocumentMatrix(tdm)   
# frqt <- colSums(as.matrix(dtm))   
# ord <- order(freq,decreasing = T)   
# m <- as.matrix(dtm)   
#dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   

# lords <- tm_map(lords, removeWords, "noble")
# 
# lords <- tm_map(lords, removeWords, "lord")
# 
# Or you can make a list of words, c("noble", "lord", etc.), to remove them in one go:
#   
#   lords <- tm_map(lords, removeWords, c("noble", "lord"))


#https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/
# set.seed(42)
# png("Internet use ways.png", width = 1200, height = 800,res=100)
# 
# #limit words by specifying min frequency
# wc<-wordcloud(names(frqt),frqt, min.freq=1,colors=brewer.pal(6,"Dark2"))
# print(wc)
# dev.off()

#another wordcloud to check how are youth using devices for work


# tdm <- Corpus(DataframeSource(dot.tech.sm.work.dt[(dev.for.work. != ""), list(dev.for.work.)]))
# tdm <- tm_map(tdm, removePunctuation)   # *Removing punctuation:*    
# tdm <- tm_map(tdm, removeNumbers)      # *Removing numbers:*    
# tdm <- tm_map(tdm, tolower)   # *Converting to lowercase:*    
# tdm <- tm_map(tdm, removeWords, stopwords("english"))   # *Removing "stopwords" 
# tdm <- tm_map(tdm, stemDocument)   # *Removing common word endings* (e.g., "ing", "es")   
# tdm <- tm_map(tdm, stripWhitespace)   # *Stripping whitespace   
# tdm <- tm_map(tdm, PlainTextDocument)   
# #dtm <- DocumentTermMatrix(tdm)   
# # tdm.f <- TermDocumentMatrix(tdm)   
# 
# #head(wf)  
# 
# remove.words <-c("act",
# "acti","acting",
# "activities","activity",
# "advance","advancing",
# "air","almost",
# "also",
# "another","ans",
# "answer","applied",
# "authorized",
# "aware","basic",
# "basically",
# "boss",
# "bosses",
# "cal","calcul",
# "called","calling",
# "cameras","can","carry",
# "cars","case","cent",
# "challeng","children",
# "class","clean",
# "cli","colleagu",
# "commun",
# "communic","compani",
# "compnays","comput",
# "concerning",
# "condit",
# "conferenc",
# "cont","cowok",
# "deepen",
# "didnt","different",
# "docu",
# "documet",
# "doesnt","done",
# "dont","easier",
# "especially",
# "etc","even",
# "face",
# "fil","filesto",
# "handl","hom",
# "machin",
# "mainly","manic",
# "many","offic",
# "onlin",
# "opportun",
# "ord","organ",
# "oth","patn",
# "peopl",
# "phon",
# "plac","prepar",
# "purpos",
# "rel",
# "said",
# "shes","shrare",
# "since","skyp",
# "stud","therefore",
# "togeth",
# "wal","want",
# "youve")
# 
# 
# tdm <- tm_map(tdm, removeWords, remove.words)
# dtm <- DocumentTermMatrix(tdm)   
# 
# rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
# dtm.new   <- dtm[rowTotals> 0, ]           #remove all docs without words
# 
# 
# 
# wf <- data.frame(word=names(frqt), freq=frqt)   
# frqt <- colSums(as.matrix(dtm.new))   
# ord <- order(frqt,decreasing = T)   
# 
# set.seed(22)
# png("Devices-use-ways-for-work.png", width = 1200, height = 800,res=150)
# 
# #limit words by specifying min frequency
# wc<-wordcloud(names(frqt),frqt, min.freq=5,colors=brewer.pal(6,"Dark2"))
# print(wc)
# dev.off()
# 
# #convert to term document matrix and remove all those words that occur in only 1% of the data
# myTdm <- TermDocumentMatrix(tdm, control = list(wordLengths=c(1,Inf), weighting=weightTfIdf))
# #http://stats.stackexchange.com/questions/160539/is-this-interpretation-of-sparsity-accurate
# myTdm2 <- removeSparseTerms(myTdm, sparse=0.993)
# 
# #convert to matrix
# m2 <- as.matrix(myTdm2)
# #compute euclidean distance
# distMatrix <- dist(scale(m2))
# 
# 
# 
# #fit the clusters
# fit <- hclust(distMatrix, method="ward.D2")
# 
# #plot the clusters
# png("Hierarchical5-cluster-word-freq-devices-for-work.png", width = 1200, height = 800,res=150)
# 
# 
# #dendrogram visualization - Unsupervised Machine Learning
# p1<-plot(fit,main ="In what ways devices are used for work",cex=0.4)  
# groups <- cutree(fit, k=2)   # "k=" defines the number of clusters you are using   
# rect.hclust(fit, k=2, border="red") # draw dendogram with red borders around the 5 clusters   
# 
# print(p1)
# 
# dev.off()
# 
# 
# # 
# # 
# # dtmss <- removeSparseTerms(dtm, 0.05) # This makes a matrix that is only 15% empty space, maximum.   
# png("Histogram4-word-freq-devices-for-work.png", width = 1200, height = 800,res=150)
# 
# p <- ggplot(subset(wf, frqt>10), aes(word, freq))    
# p <- p + geom_bar(stat="identity")   
# p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
# print(p)  
# dev.off()
# 
# 
# png("K-means4-cluster-word-freq-devices-for-work.png", width = 1200, height = 800,res=70)
# #my.dtm2 <- DocumentTermMatrix(myTdm2)   
# 
# d <- dist(t(dtm.new), method="euclidian")   
# kfit <- kmeans(scale(d), centers=6)   
# clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   
# 
# dev.off()
# 
# res.km <- eclust(dtm.new, "kmeans", nstart = 25)
# 
# # #plot the elbow to determine the clusters
# # png("find-clusters-for-dev-work-corpus.png", width = 1200, height = 800,res=150)
# # 
# # wss <- 2:100
# # for (i in 2:100) wss[i] <- sum(kmeans(d,centers=i,nstart=25)$withinss)
# # plot(2:100, wss[2:100], type= "b", xlab= "Number of Clusters",ylab= "Within groups sum of squares")
# # 
# # dev.off()
# # 
# # 
# #plot of chunk unnamed-chunk-28
# #https://www.quora.com/What-is-a-good-explanation-of-Latent-Dirichlet-Allocation
# #https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/
# burnin <- 4000
# iter <- 2000
# thin <- 500
# seed <-list(2003,5,63,100001,765)
# nstart <- 5
# best <- TRUE
# 
# k<-5
# ldaOut <-LDA(as.matrix(dtm.new),k, method= "Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
# ldaOut.topics <- as.matrix(topics(ldaOut))
# write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))
# 
# ldaOut.terms <- as.matrix(terms(ldaOut,6))
# write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))
# 
# topicProbabilities <- as.data.frame(ldaOut@gamma)
# write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))
# 

names(dot.tech.sm.work.full.dt) <- gsub("What.other.ways.you.use.the.Internet.for.work.", "intrnt.other.ways.for.work", names(dot.tech.sm.work.full.dt))

raw.vect<-dot.tech.sm.work.full.dt[(intrnt.other.ways.for.work != "")& !is.na(GenderC) & !is.na(AgeC), list(ID, GenderC, AgeC,LocationRespondent,intrnt.other.ways.for.work), by = AgeC]
temp<-textProcessor(documents=raw.vect$intrnt.other.ways.for.work,metadata=raw.vect)

meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents
out <- prepDocuments(docs, vocab, meta)
docs<-out$documents
vocab<-out$vocab
meta <-out$meta

set.seed(01234)
intrnt.other.use.ways.work.wo.prev <- stm(docs,vocab,K=20,init.type = "Spectral", data= out$meta)


png("WO-Prev-other-ways-internet-usd-for-work.png", width = 1200, height = 800,res=100)

plot.STM(intrnt.other.use.ways.work.wo.prev, type = "perspectives", xlim = c(0, .3),topics = c(3,10))
dev.off()

intrnt.other.use.ways.work.wth.prev <- stm(docs,vocab,K=20,prevalence =~GenderC , init.type = "Spectral", data= out$meta)
png("Wth-Prev-other-ways-internet-usd-for-work.png", width = 1200, height = 800,res=100)

plot.STM(intrnt.other.use.ways.work.wth.prev, type = "perspectives", xlim = c(0, .3),topics = c(3,10))
dev.off()

intrnt.other.use.ways.work.wth.prev.n.cont <- stm(docs,vocab,K=20,prevalence =~GenderC , content=~ AgeC,init.type = "Spectral", data= out$meta)
png("Prev--Cont-other-ways-internet-usd-for-work.png", width = 1200, height = 800,res=100)

plot.STM(intrnt.other.use.ways.work.wth.prev.n.cont, type = "perspectives", xlim = c(0, .3),topics = c(3,10))
dev.off()


#What.digital.services.or.tools.do.you.use.for.work." "character"

names(dot.tech.sm.work.full.dt) <- gsub("What.digital.services.or.tools.do.you.use.for.work.", "dig.ser.use.for.work", names(dot.tech.sm.work.full.dt))

raw.vect<-dot.tech.sm.work.full.dt[(dot.tech.sm.work.full.dt != ""), list(ID, GenderC, AgeC,LocationRespondent,dot.tech.sm.work.full.dt), by = AgeC]
temp<-textProcessor(documents=raw.vect$dot.tech.sm.work.full.dt,metadata=raw.vect)

meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents
out <- prepDocuments(docs, vocab, meta)
docs<-out$documents
vocab<-out$vocab
meta <-out$meta


# 
# 
# 
# 
# 
# # 
# # 
# # 
# # 
