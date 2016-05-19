
library(data.table)
library(reshape2)
library(plyr)

repro.dt <- data.table(read.csv("repro-exmpl4.csv", header=T,stringsAsFactors = F))

#create an aux. fruits table
fruits<-matrix(c("apples", "pears"), ncol = 1, byrow=T)
fruit<-as.table(fruits)

#create an aux. frequency table
freq.levels<- matrix(c("everyday", "onceinaweek", "onceinamonth"),ncol = 1, byrow = T)

freq<-as.table(freq.levels)

#melt for buying  fruit frequencies
id.cols.buy.m <-names(repro.dt)[-grep("buy",names(repro.dt))]


repro.buy.m<-data.table(melt(repro.dt, id.vars = id.cols.buy.m, measure.vars = grep("buy",names(repro.dt),value=T ),
                                         variable.name = "buy.fruit", value.name = "buy.freq"))

repro.buy.m[, c("fruit", "freq.pur") := list(NA, NA)]

repro.buy.m$fruit = apply(repro.buy.m, 1, function(u){
  bool = sapply(fruit[,1], function(x) grepl(x, u[['buy.fruit']]))
  if(any(bool) & !(is.na(u[['buy.freq']]))) fruit[bool] else NA
})

repro.buy.m$freq.pur= apply(repro.buy.m, 1, function(u){
  bool = sapply(freq.levels[,1], function(x) grepl(x, u[['buy.fruit']]))
  if(!is.na(u[['fruit']])) freq.levels[bool] else NA
})


#add a fruit value only when he has purchased one


#check for the spl case that a person who does not purchase a fruit has the new columns = NA.
one.no.buy.case.m<-unique(repro.buy.m[ID == 1236,])

#remove the redunant "buy" columns -- they are taken care
id.cols.buy.m <- colnames(repro.buy.m)[-grep("buy",names(repro.buy.m))]

f <- as.formula(paste(paste(id.cols.buy.m, collapse = " + "), "~ buy.fruit"))

repro.buy.c<-data.table(dcast(data = repro.buy.m, f, value.var = "buy.freq",function(x) length(x)))
repro.buy.c<-repro.buy.c[, which(grepl("buy", colnames(repro.buy.c))):=NULL]

#get a unique case -- unqiue by those columns that are yet to be reshaped.
#the fact is that all those columns that are yet to be reshaped would have been added as duplicate rows -- 
#since these columns are of different lengths for each id

all.buy.case<-unique(repro.buy.c, by =grep("eat",names(repro.buy.c)) )

#melt for eating fruits and frequencies

id.cols.eat <-colnames(repro.buy.c)[-grep("eat",names(repro.buy.c))]

measure.cols.eat <-grep("eat",names(repro.buy.c),value=T )

#send only those rows of data that are unique by measure variables-->this drops the duplicates
repro.eat.m<-data.table(melt(unique(repro.buy.c,by = measure.cols.eat),  id.vars = id.cols.eat, measure.vars = grep("eat",names(repro.buy.c),value=T ),
                             variable.name = "eat.fruit", value.name = "eat.freq"))

#make freq.pur  =NA for all those fruits in eat list but not in buy list

#add fruit eat column--> this adds a duplicate value for freq.pur -- is this a bug?
repro.eat.m$fruit.et = apply(repro.eat.m, 1, function(u){
  bool = sapply(fruit[,1], function(x) grepl(x, u[['eat.fruit']]))

  if(any(bool) & !is.na(u[['eat.freq']])) fruit[bool] else NA
})
#all.eat.case.a<-unique(repro.eat.m, by =grep("freq.pur",names(repro.eat.m)) )

# 
# #add fruit eating freq column
# 
repro.eat.m$freq.et= apply(repro.eat.m, 1, function(u){
  bool = sapply(freq.levels[,1], function(x) grepl(x, u[['eat.fruit']]))
  if(!is.na(u[['eat.freq']]) & !is.na(u[['fruit.et']])) freq.levels[bool] else NA
})



#how many cases ?
# 1. did not buy fruits , but ate fruits-  fruit! = fruit.et, freq.pur = NA , freq.et = 1 
# 2. did not eat fruits, but bought fruits - fruit != frui.et freq.et = NA, freq.pur = 1
# 3. buy and eat same fruits same freq -  fruit = fruit.et & freq.pur = freq.et, freq.pur =1, freq.eat =1
# 4. buy and eat same fruits different freq, freq.pur = 1, freq.et = 1
# 5. buy and eat diff fruits same freq 2 rows - freq.pur = 1, freq.et =NA , 
#                                               freq.pur = NA. freq.et =1
# 6. buy and eat diff fruits diff freq. 2 rows  same as above

# 


#http://stackoverflow.com/questions/24536771/r-datatable-conditionally-replacing-column-values
#http://stackoverflow.com/questions/12399592/na-in-data-table
#set appropriate frequencies


id.cols.eat.m <- colnames(repro.eat.m)[-grep("eat.fruit",names(repro.eat.m))]

f <- as.formula(paste(paste(id.cols.eat.m, collapse = " + "), "~ eat.fruit"))

repro.buy.eat.c<-data.table(dcast(data = repro.eat.m[!is.na(fruit) | !is.na(fruit.et),], f, value.var = "eat.freq",function(x) length(x)))
repro.buy.eat.c<-repro.buy.eat.c[, which(grepl("eat", colnames(repro.buy.eat.c))):=NULL]


#melt again now to merge fruit columns (buy or eat - they are still fruits)-- so have just one column

id.cols.buy.eat.m<-colnames(repro.buy.eat.c)[-grep("fruit|fruit.et",names(repro.buy.eat.c))]

repro.buy.eat.m<-data.table(melt(repro.buy.eat.c, id.vars = id.cols.buy.eat.m, measure.vars = grep("fruit|fruit.et",names(repro.buy.eat.c),value=T ),
                                variable.name = "fruit.final", value.name = "buy.eat.f"))

# 
repro.buy.eat.m$freq.et.x = apply(repro.buy.eat.m, 1, function(u){
    if(u[['fruit.final']] == 'fruit' ) NA else u[['freq.et']]
})

repro.buy.eat.m$freq.pur.x = apply(repro.buy.eat.m, 1, function(u){
  if(u[['fruit.final']] == 'fruit.et') NA else u[['freq.pur']]
})

#add the merged column fruit with a condition to set either freq.pur or freq.et

id.cols.buy.eat.m <- colnames(repro.buy.eat.m)[-grep("final",names(repro.buy.eat.m))]

f <- as.formula(paste(paste(id.cols.buy.eat.m, collapse = " + "), "~ fruit.final"))

#you just need to filter out rows that have NAs for "value.name" -- they are redundant
repro.buy.eat.final<-data.table(dcast(data = repro.buy.eat.m[!is.na(buy.eat.f),], f, value.var = "buy.eat.f",function(x) length(unique(x))))

#drop the redunant fruit columns -- because you have merged them now into buy.eat.f
repro.buy.eat.final<-repro.buy.eat.final[, which(grepl("fruit", colnames(repro.buy.eat.final))):=NULL]

#now rename the final-- merged column to "fruit"
rename(repro.buy.eat.final,replace = c("buy.eat.f" = "fruit"))

#drop old columns
repro.buy.eat.final<-repro.buy.eat.final[, c("freq.pur", "freq.et") := NULL]

#rename new columns
#http://stackoverflow.com/questions/20987295/rename-multiple-columns-by-names
setnames(repro.buy.eat.final,old =c("freq.pur.x","freq.et.x"), new  = c("freq.pur", "freq.et"))

#get uique rows
repro.buy.eat.final<-unique(repro.buy.eat.final, by=c("ID", "freq.pur", "freq.et"))

#merge rows of similar frequencies and same fruit
#http://stackoverflow.com/questions/27955491/r-data-table-conditional-aggregation

repro.buy.eat.final[, freq.pur:=(freq.pur[ fruit %in% repro.buy.eat.final$fruit &  !is.na(freq.pur)]),by= c("ID", "fruit")]

#http://stackoverflow.com/questions/12353820/sort-rows-in-data-table-r
result<-unique(repro.buy.eat.final[order(ID, fruit, freq.pur,freq.et)], by = c("ID", "fruit"))
                                 

