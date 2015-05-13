complete <- function(directory, id = 1:332) {
    files <- list.files(path = directory, pattern = "*.csv", full.names = TRUE)
    count = 0
    for (i in 1:length(id)) {
        data <- read.csv(files[i])
        indices <- !is.na(data[["sulfate"]]) & !is.na(data[["nitrate"]])
        count = count + sum(indices)
    }
    count
}