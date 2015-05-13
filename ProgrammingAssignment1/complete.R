complete <- function(directory, id = 1:332) {
    data_frame <- data.frame(id = integer(0), nobs = integer(0))
    colnames(data_frame) <- c('id', 'nobs')
    filenames <- paste(formatC(id, width = 3, format = 'd', flag = '0'), 'csv', sep = '.')
    filenames <- paste(directory, filenames, sep = '/')
    for (i in 1:length(id)) {
        data <- read.csv(filenames[i])
        indices <- !is.na(data[["sulfate"]]) & !is.na(data[["nitrate"]])
        new_column <- c(id[i], sum(indices))
        data_frame <- rbind(data_frame, new_column)
    }
    data_frame
}