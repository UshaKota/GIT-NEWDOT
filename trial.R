
library(data.table)
library(ggplot2)
library(plyr)
library(stringr)
library(stringi)
library(tidyr)
library(reshape2)
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
# #convert dotData to data table
# dotsr<-data.table(dotsr,keep.rownames=TRUE)

#to have an idea check how many vairables and type of variables the DOT data has
dot.var.dt<-data.table(var.name = colnames(dotsr),var.type = sapply(dotsr,class))


#rename the X column to ID
rename(dotsr,replace = c("X" = "ID"))
dotsr<-dotsr[rowSums(is.na(dotsr[,1:21, with = F]))==0,,drop =F]
dot.tech.gender.dt0<-subset(dotsr, select = grepl("How.often.do.you.actually.use.the.Internet|ID|Gender|Age|Location|Geo|What.devices|use.these.devices|most.commonly.use.the.devices|access.to.do.you.have.to.the.Internet|commonly.use.the.Internet|What.digital.services.tools.do.you.have.access.to|commonly.use.digital", names(dotsr)))

dot.tech.gender.dt<-subset(dotsr, 
                           select =
                             grepl("How.often.do.you.actually.use.the.Internet|ID|
                                   Gender|Age|Location|Geo|
                                   What.devices|use.these.devices|
                                   most.commonly.use.the.devices|
                                   access.to.do.you.have.to.the.Internet|
                                   commonly.use.the.Internet|
                                   What.digital.services.tools.do.you.have.access.to |
                                   commonly.use.digital", 
                                   names(dotsr)))



make.plot.dt<-function(main.dt, plot.var,freq.var2){
  plotting.dt<-main.dt[, list(Frequency = length(ID)),
                       by = c( plot.var,freq.var2, "GenderC")]
  
  #geom_bar(aes(y = (..count..)/sum(..count..)))
  return (plotting.dt)
  
}

make.plot.freq.dt<-function(plotting.dt, freq.var1,freq.var2) {
  Frequency<-plotting.dt$Frequency
  freq.var1<-freq.var1
  freq.var2<-freq.var2
  
  plot.freq.dt <- ddply(plotting.dt,.(plotting.dt[[freq.var1]],
                                      plotting.dt[[freq.var2]]), 
                        transform, pos = cumsum(Frequency) - (0.5*Frequency))
  return (plot.freq.dt)
  
  
}

plot.data<-function(main.dt,facet.var,freq.var2, titl.str) {
  plotting.dt<-make.plot.dt(main.dt,facet.var,freq.var2)
  plot.freq.dt<-make.plot.freq.dt(plotting.dt, facet.var,freq.var2)
  xval<-plot.freq.dt[[freq.var2]]
  gg<-ggplot(plot.freq.dt, aes(x=xval,
                               y= plot.freq.dt$Frequency )) +
    
    geom_bar(aes(fill = plot.freq.dt$GenderC), stat="identity") +
    
    geom_text(aes(label = ifelse(plot.freq.dt$Frequency >= 5,
                                 plot.freq.dt$Frequency,""), 
                  y = pos), size = 5) +
    facet_wrap(reformulate(facet.var),scales = "free_y" ) + 
    theme(strip.text.x = element_text(size=16)) +
    theme(axis.text = element_text(colour = "blue",size = 16)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    ggtitle(titl.str) + 
    
    theme(plot.title = element_text(size = rel(3), colour = "blue"),
          axis.title.x = element_text(color="blue", size=20, face="bold"),
          axis.title.y = element_text(color="blue", size=20, face="bold")) +
    scale_fill_brewer(palette="Accent") + 
    theme(legend.text = element_text(size = 16, colour = "blue")) +
    labs(x = "Age Category", y = "Respondents",size = 20, color = "blue")
  
  print(gg)
  
}

plot.data(dotsr, "GeographyC", "EducationC", 
          "Survey Education by Geography")


# geo.edu.dt<-dotsr[, list(Frequency = length(ID)),by = c( "GeographyC","EducationC","GenderC")]
# 
# 
# DataGE<- ddply(geo.edu.dt, .(GeographyC,EducationC), 
#                transform, pos = cumsum(Frequency) - (0.5 * Frequency)
# )
# 
# gg<-ggplot(DataGE, aes(x=EducationC,
#                              y= Frequency )) +
#   
#   geom_bar(aes(fill = GenderC), stat="identity") +
#   
#   geom_text(aes(label = ifelse(Frequency >= 5,
#                                Frequency,""), 
#                 y = pos), size = 5) +
#   facet_wrap(~GeographyC,scales = "free_y" ) + 
#   theme(strip.text.x = element_text(size=16)) +
#   theme(axis.text = element_text(colour = "blue",size = 16)) + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#   ggtitle("test") + 
#   
#   theme(plot.title = element_text(size = rel(3), colour = "blue"),
#         axis.title.x = element_text(color="blue", size=20, face="bold"),
#         axis.title.y = element_text(color="blue", size=20, face="bold")) +
#   scale_fill_brewer(palette="Accent") + 
#   theme(legend.text = element_text(size = 16, colour = "blue")) +
#   labs(x = "Age Category", y = "Respondents",size = 20, color = "blue")
# 
# print(gg)
# 
