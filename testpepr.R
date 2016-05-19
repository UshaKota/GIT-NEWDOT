

dotpepr <- data.table(read.delim2("DOTpeper - Cleaned Data.txt", header=T, skip = 1,stringsAsFactors = F))
saveRDS(dotpepr, "dotpeperData.RDS")
