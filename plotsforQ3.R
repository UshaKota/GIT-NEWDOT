cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#stacked bar plots for device - access/use frequencies across Locations
# dev.use.loc<-dot.gen.dev.loc.dt[, list(Frequency = length(ID)),by = c( "LocationRespondent","use.at","location.use.freq")]
png("youth-etronic-dev-use-for-work-by-Location.png", width = 1200, height = 800,res=110)

#http://stackoverflow.com/questions/6644997/showing-data-values-on-stacked-bar-chart-in-ggplot2
youth.use.edev.work.c<-dot.youth.work.edev.dt[, list(Frequency = length(ID)),by = c( "LocationRespondent","work","work.freq")]

Data0 <- ddply(youth.use.edev.work.c, .(LocationRespondent,work.freq), 
               transform, pos = cumsum(Frequency) - (0.5 * Frequency)
)

#  Data <- ddply(use.case.m.c, .(LocationRespondent,use.dev.freq), 
#                transform, pos = cumsum(Frequency) - (0.5 * Frequency)
#  )

# 


gg<-ggplot(Data0, aes(x = work.freq,y=Frequency  )) +
  geom_bar(aes(fill = work), stat="identity") +
  geom_text(aes(label = ifelse(Frequency >= 10, Frequency,""), y = pos), size = 3) +
  facet_wrap(~LocationRespondent,scales = "free_y" ) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Activities related to work on Electronic devices by Location") +scale_fill_brewer(palette="Set2")



print(gg)
dev.off()

youth.use.edev.work.A<-dot.youth.work.edev.dt[, list(Frequency = length(ID)),by = c( "AgeC","work","work.freq")]


DataA <- ddply(youth.use.edev.work.A, .(AgeC,work.freq), 
               transform, pos = cumsum(Frequency) - (0.5 * Frequency)
)
png("youth-etronic-dev-use-for-work-by-Age.png", width = 1200, height = 800,res=110)

gg<-ggplot(DataA, aes(x = work.freq,y=Frequency  )) +
  geom_bar(aes(fill = work), stat="identity") +
  geom_text(aes(label = ifelse(Frequency >= 10, Frequency,""), y = pos), size = 3) +
  facet_wrap(~AgeC,scales = "free_y" ) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Activities related to work on Electronic devices by Age") +scale_fill_brewer(palette="Set2")



print(gg)
dev.off()

youth.use.edev.work.G<-dot.youth.work.edev.dt[, list(Frequency = length(ID)),by = c( "GenderC","work","work.freq")]


DataG <- ddply(youth.use.edev.work.G, .(GenderC,work.freq), 
               transform, pos = cumsum(Frequency) - (0.5 * Frequency)
)
png("youth-etronic-dev-use-for-work-by-Gender.png", width = 1200, height = 800,res=110)

gg<-ggplot(DataG, aes(x = work.freq,y=Frequency  )) +
  geom_bar(aes(fill = work), stat="identity") +
  geom_text(aes(label = ifelse(Frequency >= 10, Frequency,""), y = pos), size = 3) +
  facet_wrap(~GenderC,scales = "free_y" ) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Activities related to work on Electronic devices by Gender") +scale_fill_brewer(palette="Set2")



print(gg)
dev.off()


youth.use.edev.work.W<-dot.youth.work.edev.dt[, list(Frequency = length(ID)),by = c( "GenderC","work","work.freq")]


DataW <- ddply(youth.use.edev.work.W, .(work,work.freq), 
               transform, pos = cumsum(Frequency) - (0.5 * Frequency)
)
png("Work related-activies-by-Gender-by-.png", width = 1200, height = 800,res=110)

gg<-ggplot(DataW, aes(x = work.freq,y=Frequency  )) +
  geom_bar(aes(fill = GenderC), stat="identity") +
  geom_text(aes(label = ifelse(Frequency >= 10, Frequency,""), y = pos), size = 3) +
  facet_wrap(~work,scales = "free_y" ) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Work on Electronic devices by Gender") +scale_fill_brewer(palette="Pastel1")



print(gg)
dev.off()



youth.use.edev.work.W.L<-dot.youth.work.edev.dt[, list(Frequency = length(ID)),by = c( "LocationRespondent","work","work.freq")]

DataW.L <- ddply(youth.use.edev.work.W.L, .(work,work.freq), 
                 transform, pos = cumsum(Frequency) - (0.5 * Frequency)
)
png("Work related-activies-by-Location.png", width = 1200, height = 800,res=110)

gg<-ggplot(DataW.L, aes(x = work.freq,y=Frequency  )) +
  geom_bar(aes(fill = LocationRespondent), stat="identity") +
  geom_text(aes(label = ifelse(Frequency >= 10, Frequency,""), y = pos), size = 3) +
  facet_wrap(~work,scales = "free_y" ) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Work on Electronic devices by Location") +scale_fill_brewer(palette="Pastel1")



print(gg)
dev.off()


youth.use.intrnt.work.A<-dot.youth.intrnt.act.dt[, list(Frequency = length(ID)),by = c( "AgeC", "use.intrnt.act","act.freq.with.intrnt")]

DataI.A <- ddply(youth.use.intrnt.work.A, .(use.intrnt.act,act.freq.with.intrnt), 
                 transform, pos = cumsum(Frequency) - (0.5 * Frequency)
)
png("Internent-use-for-Work related-activies-by-Age.png", width = 1200, height = 800,res=110)

gg<-ggplot(DataI.A, aes(x = act.freq.with.intrnt,y=Frequency  )) +
  geom_bar(aes(fill = AgeC), stat="identity") +
  geom_text(aes(label = ifelse(Frequency >= 10, Frequency,""), y = pos), size = 3) +
  facet_wrap(~use.intrnt.act,scales = "free_y" ) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("How does Youth using Internet for Work vary by Age") +scale_fill_brewer(palette="Pastel1")



print(gg)
dev.off()



###############process for open-ended text input responses###################

raw.vect<-dot.tech.sm.work.full.dt[(dev.for.work. != "") & !is.na(AgeC) & !is.na(GenderC), list(ID, GenderC, AgeC,LocationRespondent,dev.for.work.), by = AgeC]
temp<-textProcessor(documents=raw.vect$dev.for.work.,metadata=raw.vect)

meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents
out <- prepDocuments(docs, vocab, meta)
docs<-out$documents
vocab<-out$vocab
meta <-out$meta

# dev.use.ways.work <- stm(docs,vocab,K=10, prevalence =~  GenderC, init.type = "Spectral", data= out$meta)
# devworkcontent <- stm(out$documents, out$vocab, K = 20,prevalence =~ AgeC , content =~ GenderC,
#                       data = out$meta, init.type = "Spectral")
# 
# prep <- estimateEffect(1:10 ~GenderC , dev.use.ways.work,
#                        meta = out$meta, uncertainty = "Global")
# plot.estimateEffect(prep, covariate = "GenderC", topics = c(3, 7, 10),
#                     model = dev.use.ways.work, method = "difference",
#                     cov.value1 = "female", cov.value2 = "male",
#                     xlab = "More female ... More male",
#                     main = "Effect of female vs. male",
#                     xlim = c(-.1, .1))

# cloud(dev.use.ways.work, topic = 7, scale = c(2,.25))
# mod.out.corr <- topicCorr(dev.use.ways.work)
#plot.topicCorr(mod.out.corr)
#dev.use.way.Select <- selectModel(docs,vocab,K=10, data=raw.vect,runs=20)

# device.for.work.model <- selectModel(out$documents, out$vocab, K = 20,
#                               prevalence =~ as.factor(AgeC), max.em.its = 20,
#                               data = out$meta, runs = 20, seed = 8458159)

