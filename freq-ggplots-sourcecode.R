

make.plot.tech.dt<-function(sub.dt, facet.var,tech.use.acc.freq.var){
  
  plotting.dt<-sub.dt[, list(Frequency = length(ID)),
                      by = c(facet.var, tech.use.acc.freq.var,"GenderC")]
  return (plotting.dt)
  
}


make.plot.tech.freq.dt<-function(plotting.dt, freq.var1,freq.var2) {
  
  Frequency<-plotting.dt$Frequency
  
  freq.var1<-freq.var1
  freq.var2<-freq.var2
  
  plot.freq.dt <- ddply(plotting.dt,.(plotting.dt[[freq.var1]],
                                      plotting.dt[[freq.var2]]), 
                        transform, pos = cumsum(Frequency) -(0.5*Frequency))
  return (plot.freq.dt)
  
  
}

plot.tech.data<-function(sub.dt,facet.var,tech.use.acc.freq.var, titl.str) {
  
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  plotting.dt<-make.plot.tech.dt(sub.dt,facet.var,
                                 tech.use.acc.freq.var)
  plot.freq.dt<-make.plot.tech.freq.dt(plotting.dt,facet.var,
                                       tech.use.acc.freq.var)
  gg<-ggplot(plot.freq.dt, aes(x=plot.freq.dt[[tech.use.acc.freq.var]],
                               y= plot.freq.dt$Frequency )) + 
    geom_bar(aes(fill = GenderC), stat="identity") + 
    geom_text(aes(label = ifelse(plot.freq.dt$Frequency > 20,
                                 plot.freq.dt$Frequency,""), y = pos),
              size = 6) +
    
    facet_wrap(reformulate(facet.var),scales = "free_y" ) +
    theme(strip.text.x = element_text(size=20)) + 
    theme(axis.text = element_text(colour = "blue",size = 16)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    ggtitle(titl.str) +
    theme(plot.title = element_text(size = rel(3), colour = "blue"),
          axis.title.x = element_text(color="blue", size=30, face="bold"),
          axis.title.y = element_text(color="blue", 
                                      size=30, face="bold")) +
    theme(legend.text = element_text(size = 16, colour = "blue")) +
    labs(x = "Frequency Category", y = "Frequency",size = 20, 
         color = "blue") +
    scale_fill_brewer(palette= "Set3")  
  
  
  print(gg)
  
}