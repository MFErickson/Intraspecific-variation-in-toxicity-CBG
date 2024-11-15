library(readxl)



# This is a function to convert a data frame from original format to that
# compatible with Kaplan-Meier analysis
convertToKM <- function(tox) {
  timeCols <- c("X30m", "X60m", "X120m", "X180m", "X240m")
  times <- c(30, 60, 120, 180, 240)
  
  # Turn spreadsheet into a form suitable for Kaplan–Meier analysis
  l <- lapply(seq_len(nrow(tox)), function(i) {
    row <- tox[i, ]
    # Need a row for each individual daphnia 
    ids <- seq_len(row$Ndaphnia)
    # Arbitrarily assign deaths to daphnia
    deaths <- unlist(row[, timeCols])
    diedAt <- sapply(ids, function(id) which(id <= deaths)[1])
    
    # Build a data frame with a row per Daphnia
    df <- data.frame(Daphnia = ids, time = times[diedAt], death = 1)
    # Properly indicate those that didn't die
    df$death[is.na(df$time)] <- 0
    df$time[is.na(df$time)] <- tail(times, 1)
    
    # Keep all of the original columns
    cbind(row, df, row.names = NULL)
  })
  do.call(rbind, l)
}



# Read original toxicity data
tox <- read.csv("data/Processed/CGB.csv")


# Convert to Kaplan-Meier compatible format
kmdata <- convertToKM(tox)

#######
write.csv(kmdata, "data/processed/kmdata.csv", row.names = FALSE)
