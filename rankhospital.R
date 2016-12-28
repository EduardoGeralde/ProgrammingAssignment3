## Johns Hopkins University - Data Science Specialization
## Course 2 - R Programming

## by Eduardo Geralde Neto

##Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a state,
##an outcome (outcome), and the ranking of a hospital in that state for that outcome (num). 
##The function reads the outcome-of-care-measures.csv file and returns a character vector with the name of the
##hospital that has the ranking specified by the num argument. 
##For example, the call rankhospital("MD", "heart failure", 5) would return a character vector containing the
##name of the hospital with the 5th lowest 30-day death rate for heart failure in MD state. 
##The num argument can take values “best”, “worst”, or an integer indicating the ranking (smaller numbers are 
##better). If the number given by num is larger than the number of hospitals in that state, then the function
##should return NA. Hospitals that do not have data on a particular outcome should be excluded from the set of 
##hospitals when deciding the rankings.
##It may occur that multiple hospitals have the same 30-day mortality rate for a given cause of death. In those
##cases ties should be broken by using the alphabetical order in hospital`s name.

##Part 3: Ranking hospitals by outcome in a state

rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv")
        
        ## Check that state and outcome are valid
        states <- levels(data[, 7])[data[, 7]]
        state_flag <- FALSE
        for (i in 1:length(states)) {
                if (state == states[i]) {
                        state_flag <- TRUE
                }
        }
        if (!state_flag) {
                stop ("invalid state")
        } 
        if (!((outcome == "heart attack") | (outcome == "heart failure")
              | (outcome == "pneumonia"))) {
                stop ("invalid outcome")
        }
        
        ## Return hospital name in that state with the given rank 30-day death rate
        col <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        
        data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
        data[, 2] <- as.character(data[, 2])
        statedata <- data[grep(state, data$State), ]
        orderdata <- statedata[order(statedata[, col], statedata[, 2], na.last = NA), ]
        if(num == "best") {
                orderdata[1, 2]
        } else if(num == "worst") {
                orderdata[nrow(orderdata), 2]
        } else{
                orderdata[num, 2]
        }
}