pollutantmean <- function(directory, pollutant, id = 1:332) {
    filename <- paste(formatC(id, width = 3, format = 'd', flag = '0'), 'csv', sep = '.')
    pollutant_vector = 0
    for (i in 1:length(id)) {
        data <- read.csv(paste(directory, filename[i], sep = '/'))
        indices <- is.na(data[[pollutant]])
        if (sum(!indices) > 0) {
            pollutant_vector = c(pollutant_vector, data[[pollutant]][!indices])
        }
        
        #print(c(counts[i], i))  
    }
    mean(pollutant_vector)
}