corr <- function(directory, threshold = 0) {
    filenames <- list.files(directory, pattern = '.csv', full.names = TRUE)
    count <- 0
    correlations <- 0
    for (i in 1:length(filenames)) {
        data <- read.csv(filenames[i])
        complete_measurements <- !is.na(data[['sulfate']]) & !is.na(data[['nitrate']])
        if (sum(complete_measurements) > threshold) {
            correlations[i] <- cor(data[['sulfate']][complete_measurements], data[['nitrate']][complete_measurements])
        } else {
            correlations[i] <- NA
        }
       
    }
    correlations[!is.na(correlations)]
}