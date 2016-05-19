library(foreign)

mydata <- 
  read.dta( 
    "http://www.ats.ucla.edu/stat/books/sop/momsag.dta" , 
    convert.factors = FALSE 
  )


