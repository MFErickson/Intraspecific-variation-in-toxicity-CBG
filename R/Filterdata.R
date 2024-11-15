###Import data set

dt <- read.csv("data/raw/clean_toxdat.csv")
CGB <- dt[dt$Binomial == "Zizina otis", ]

CGB <- dt[dt$Binomial == "Zizina otis", ]

write.csv(CGB, "data/processed/CGB.csv", row.names = FALSE)
