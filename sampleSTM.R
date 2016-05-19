library(stm)

data <- read.csv("poliblogs2008.csv")
processed <- textProcessor(data$documents, metadata = data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta  <-out$meta
poliblogPrevFit <- stm(out$documents, out$vocab, K = 20,
                       prevalence =~ rating + s(day), max.em.its = 75,
                       data = out$meta, init.type = "Spectral")
