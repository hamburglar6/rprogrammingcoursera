rankall <- function(outcome, num = 'best'){
    
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
    states <- levels(data[,7])
    
    df <- data.frame(hospital = character(), state = character(), stringsAsFactors=FALSE)
    
    for (st in states){
    
        loc <- data[,7] == st & data[,index] != 'Not Available'
        if (is.integer(num) & sum(loc) < num) {
            hosp <- NA
        } else {
            subdata <- data[loc, c(2, index) ]  #col 1 is hospital name, col 2 is outcome data 
            subdata <- subdata[order(as.numeric(levels(subdata[,2])[subdata[,2]]), subdata[[1]]),]
            
            if (num == 'worst') {
                hosp <- as.character(subdata[sum(loc), 1])
                #row <- tail(subdata, n = 1)
                #hosp <- as.character(row[[1]])
            } else if (num == 'best') {
                hosp <- as.character(subdata[1, 1])
            } else {
                hosp <- as.character(subdata[num, 1])
            }
            
            #hosp <- as.character(subdata[num, 1])
            
        }
        df <- rbind(df, data.frame(as.list(c(hospital = hosp, state = st)), stringsAsFactors = FALSE))
        
    }
    df
}