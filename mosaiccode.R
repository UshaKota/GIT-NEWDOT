#p<-mosaic(t, gp = shading_max,keep = F)

#http://stackoverflow.com/questions/35399772/r-vcdmosaic-overlapping-labels?rq=1

p<-mosaic(~ dev.avail.freq +device , data=acc.case.m,subset = GenderC == "male", legend=FALSE, gp=shading_max,labeling= labeling_border(rot_labels = c(90,0,0,0), 
                                                                                                                                        just_labels = c("left", 
                                                                                                                                                        "center", 
                                                                                                                                                        "center", 
                                                                                                                                                        "center")))
print(p)

#https://learnr.wordpress.com/2009/03/29/ggplot2_marimekko_mosaic_chart/
#http://stackoverflow.com/questions/22238278/fine-tuning-ggplot2s-geom-boxplot


# re-level for mosaic display
png("dev-acc-by-gender.png", width = 1200, height = 800)
p1<-mosaic(~ dev.avail.freq + device + GenderC, data=acc.case.m, expected=~dev.avail.freq:GenderC + device, legend=FALSE, gp=shading_Friendly ,labeling= labeling_border(rot_labels = c(90,0,0,0), 
                                                                                                                                                                         just_labels = c("left", 
                                                                                                                                                                                         "center", 
                                                                                                                                                                                         "center", 
                                                                                                                                                                                         "center")))






print(p1)

#dev.copy(png,"dev-acc-by-gender.png") 
dev.off()

k<-structable(t)

crosstab <- table(use.case.m$LocationRespondent , use.case.m$device)
crosstab <- crosstab[,] #Reorder or and subset rows

#If Row Proportions are wanted later
proportions <- round(prop.table(crosstab , 2 )*100)

#Hand role the structure for the mosaic
values <- c(crosstab)
rowvarcat <- c("A","B")
columnvarcat <- c("1","2")
names=c("Variable Name 1", "Variable Name 2")
dims <- c(2,2) #columns then rows

TABS <- structure( c(values), .Dim = as.integer(dims), .Dimnames = structure( list(rowvarcat,columnvarcat ),
                                                                              .Names = c(names) ) , class = "table") 

PROPORTIONS <- structure( c(proportions), .Dim = as.integer(dims), .Dimnames = structure( list(rowvarcat,columnvarcat ),
                                                                                          .Names = c(names) ) , class = "table") 

TABSPROPORTIONS <- structure( c(paste(proportions,"%","\n", "(",values,")",sep="")), .Dim = as.integer(dims), .Dimnames = structure( list(rowvarcat,columnvarcat ),
                                                                                                                                     .Names = c(names) ) , class = "table") 

mosaic(TABS,pop=FALSE, main="Plot Title", sub="Plot Subtitle)")
labeling_cells(text=TABSPROPORTIONS , clip_cells=FALSE)(TABS )




#set.seed(1071)
fill_colors <- matrix(c("dark cyan", "gray", "gray","dark magenta"), ncol = 2)
png("dev-acc-by-gender.png", width = 800, height = 600)

p1<-mosaic(~ dev.avail.freq + device + GenderC, data=acc.case.m, pop = F,
           expected= ~device : GenderC,legend=FALSE, 
           gp = gpar(fill = fill_colors, col = 0),
           labeling = labeling_values,
           labeling_args = list(abbreviate_labs = c(8,6,1,1),
                                rot_labels = c(0,0,90,0),
                                just_labels = c("left","left","left","left"),
                                gp_text = gpar(col = "blue",fontsize=14)),
           gp_labels = gpar(fontsize = 14)
)

print(p1)
dev.off()

png("dev-use-by-gender.png", width = 800, height = 600)

p2<-mosaic(~ use.dev.freq + device + GenderC, data=use.case.m, pop = F,
           expected= ~device : GenderC, legend = F,
           gp = gpar(fill = fill_colors, col = 0),
           labeling = labeling_values,
           labeling_args = list(abbreviate_labs = c(8,6,1,1),
                                rot_labels = c(0,0,90,0),
                                just_labels = c("left","left","left","left"),
                                gp_text = gpar(col = "blue",fontsize=14)),
           gp_labels = gpar(fontsize = 14)
)




print(p2)
dev.off()

png("dev-use-by-location.png", width = 1600, height = 600)

p3<-mosaic(~ device + LocationRespondent, data=use.case.m, pop = F,
           expected= ~device : LocationRespondent, legend = F,
           gp = gpar(fill = fill_colors, col = 0),
           labeling = labeling_values,
           labeling_args = list(abbreviate_labs = c(8,6,1,1),
                                rot_labels = c(0,0,90,0),
                                just_labels = c("left","left","left","right"),
                                gp_text = gpar(col = "blue",fontsize=14)),
           gp_labels = gpar(fontsize = 14)
)




print(p3)
dev.off()

t<-table(use.case.m$AgeC,use.case.m$device,use.case.m$use.dev.freq)

png("use-by-age-loc.png", width = 1600, height = 600)

pn<-mosaicplot(t,pop=F,col = hcl(c(120, 10)),labeling_args = list(abbreviate_labs = c(4,6,4,4),
                                                                  rot_labels = c(90,90,90,90)))

print(pn)
dev.off()


#http://stackoverflow.com/questions/23479512/stratified-random-sampling-from-data-frame-in-r

#compute samples
acc.key.cols <-names(acc.case.m)
setkeyv(acc.case.m, acc.key.cols)

use.key.cols <-names(use.case.m)
setkeyv(use.case.m, use.key.cols)

# Population size by strata
acc.case.m[, .N, keyby = list(ID, LocationRespondent)]

# Select sample
set.seed(2)
s <- data.table(strata(acc.case.m, c("ID", "LocationRespondent"), size = (), "srswr"))

# # Sample size by strata
# s[, .N, keyby = list(ID, LocationRespondent,device)]


#mosaic plot for acc/use frequencies

#acc.use.c.tab<-table(acc.use.c$dev.avail.freq, acc.use.c$use.dev.freq)

png("device-use-association-by-gender.png", width = 1200, height = 600)

p<-mosaic(~GenderC + device + use.dev.freq, data = acc.use.c,color = T,shade=T,cex.axis = 0.5,
          labeling = labeling_values,
          labeling_args = list(abbreviate_labs = c(0,6,8,8),
                               rot_labels = c(0,0,90,0),
                               just_labels = c("right","left","left","right"),
                               gp_text = gpar(fontsize=10)),
          gp_labels = gpar(fontsize = 10)
)

print(p)
dev.off()


dev.loc.gen.tab<-xtabs( ~GenderC + device + use.dev.freq, data= unique(acc.use.c,by = "device"))

pt<-prop.table(dev.loc.gen.tab)
kmtab<-addmargins(pt)
p1<-mosaic(pt,color = T,shade=T,cex.axis = 0.5,
           labeling = labeling_values,
           labeling_args = list(abbreviate_labs = c(0,6,8,8),
                                rot_labels = c(0,0,90,0),
                                just_labels = c("right","left","left","right"),
                                gp_text = gpar(fontsize=10)),
           gp_labels = gpar(fontsize = 10)
)


print(p1)
#stacked bar plots for device - access/use frequencies across Locations
# use.case.m.c<-use.case.m[, list(Frequency = length(ID)),by = c( "LocationRespondent","device","use.dev.freq")]
# png("device-use-frequency-by-Location.png", width = 1200, height = 600)
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
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
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
# 
# png("device-access-frequency-by-Location.png", width = 1200, height = 600)
# 
# 
# gg<-ggplot(Data0, aes(x = dev.avail.freq,y=Frequency  )) +
#   geom_bar(aes(fill = device), stat="identity") +
#   geom_text(aes(label = ifelse(Frequency >= 10, Frequency,""), y = pos), size = 4) +
#   facet_wrap(~LocationRespondent,scales = "free_y" ) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#   ggtitle("Device access frequencies varying by Location") + scale_fill_manual(values=cbPalette)
# 
# 
# 
# print(gg)
# dev.off()
# 
# 
# 
# #mosaic plot

# setDT(acc.case.m)[,pop.size:= length(unique(ID)) ,by = LocationRespondent]
# 
# acc.strat<-svydesign(id = ~1, strata = ~LocationRespondent, fpc =~pop.size, data = unique(acc.case.m,by = "ID"))



#check for chi-square test of dependence for Gender, device, device access and device use.

# tb1.gen.dev<-table(acc.use.c$GenderC,acc.use.c$device)
# tb1.gen.dev.use<-table(acc.use.c$GenderC,acc.use.c$use.dev.freq)
# 
# gen.dev.ind<-chisq.test(tb1.gen.dev)

#margin.table(tb1.gen.dev.acc.use)

#http://stats.stackexchange.com/questions/8342/appropriate-way-to-deal-with-a-3-level-contingency-table
setDT(acc.use.c)[(dev.avail.freq==use.dev.freq),count.match := length(ID),by = c("device","dev.avail.freq","use.dev.freq")]
gen.acc.use.tab <-xtabs(~ GenderC + dev.avail.freq + use.dev.freq , acc.use.c)








