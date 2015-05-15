## input a 2 letter abbreviation string for the state and one of the three
## possible conditions.  The output is the best hospital for that condition 
## in that state.

best <- function(state, outcome) {
    data <- read.csv('rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv')
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
    loc <- data[, 7] == state & data[, index] != 'Not Available'
    if (sum(loc) == 0) stop("invalid state")
    
    subdata <- data[loc, c(1, 2, 7, index)]
    subdata <- subdata[order(as.numeric(levels(subdata[,4])[subdata[,4]]), subdata[[2]]),]
    
    print(subdata[1,])
    as.character(subdata[1,2])
    
#     best_result <- min(as.numeric(outcome_data[loc, index]), na.rm = TRUE)
#     best_loc <- as.numeric(outcome_data[, index]) == best_result & outcome_data[, 7] == state 
#     
#     if (sum(best_loc) == 1) {
#         as.character(outcome_data[best_loc, 2])
#     } else {
#          #as.character(outcome_data[best_loc,2][order(outcome_data[best_loc, 2])][1])
#          min(as.character(outcome_data[best_loc,2]))
#          
#     }

    
    #best_loc <- as.numeric(outcome_data[loc, index]) == best_result
}