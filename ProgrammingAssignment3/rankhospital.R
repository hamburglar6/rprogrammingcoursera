rankhospital <- function(state, outcome, num = 'best') {
    
    possible_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
    if (outcome == possible_outcomes[1]) {
        index <- 11
    } else if (outcome == possible_outcomes[2]) {
        index <- 17
    } else if (outcome == possible_outcomes[3]) {
        index <- 23
    } else {
        stop('invalid outcome')
    }
    
    data <- read.csv('rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv')
    loc <- data[, 7] == state & data[, index] != 'Not Available' 
    if (sum(loc) == 0) {
        stop('invalid state')
    }
    
    # this code was fucking up before because everything was a "factor variable" and R was ordering
    # 10.0 ahead of e.g. 9.2
    
    subdata <- data.frame(data[loc, c(1, 2, 7, index)])
    subdata <- subdata[order(as.numeric(levels(subdata[,4])[subdata[,4]]), subdata[[2]]),]
    
    if (length(num) > 1) {
        row <- subdata[num, ]
    } else if (num == 'best') {
        num <- 1
        row <- subdata[num, ]
    } else if (num == 'worst') {
        row <- tail(subdata, n=1)
    } else if (is.integer(as.integer(num))) {
        row <- subdata[num, ]
    } else {
        return('NA')
    }
#    row
    as.character(row$Hospital.Name)
    
}