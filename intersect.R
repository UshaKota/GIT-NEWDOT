
library(UpSetR)

movies <- read.csv(system.file("extdata", "movies.csv", package = "UpSetR"), 
                   header = T, sep = ";")


upset(movies, nsets = 6, number.angles = 30, point.size = 5, name.size = 12, 
      line.size = 2, mainbar.y.label = "Genre Intersections", sets.x.label = "Movies Per Genre")
