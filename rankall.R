## Johns Hopkins University - Data Science Specialization
## Course 2 - R Programming

## by Eduardo Geralde Neto

##Write a function called rankall that takes two arguments: an outcome name and a hospital ranking. The function
##reads the outcome-of-care-measures.csv file and returns a 2-column data frame containing the hospital in each
##state that has the ranking specified in num. For example the function call rankall("heart attack", "best") 
##would return a data frame containing the names of the hospitals that are the best in their respective states
##for 30-day heart attack death rates. The function should return a value for every state (some may be NA). 
##The first column in the data frame is named hospital, which contains the hospital name, and the second column
##is named state, which contains the 2-character abbreviation for the state name. Hospitals that do not have 
##data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.
##The rankall function should handle ties in the 30-day mortality rates in the same way that the rankhospital
##function handles ties.
##NOTE: For the purpose of this part of the assignment (and for efficiency), your function should NOT call the
##rankhospital function from the previous section.
##The num variable can take values “best”, “worst”, or an integer indicating the ranking (smaller numbers are 
##better). If the number given by num is larger than the number of hospitals in that state, then the function 
##should return NA.

##Part 4: Ranking hospitals in all states

rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv")
        
        ## Check that outcome is valid
        if (!((outcome == "heart attack") | (outcome == "heart failure")
              | (outcome == "pneumonia"))) {
                stop ("invalid outcome")
        }
        
        ## For each state, find the hospital of the given rank
        col <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        
        data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
        data[, 2] <- as.character(data[, 2])
        
        # Generate an empty vector that will be filled later, row by row, to generate the final output.
        output <- vector()
        
        states <- levels(data[, 7])
        
        for(i in 1:length(states)) {
                statedata <- data[grep(states[i], data$State), ]
                orderdata <- statedata[order(statedata[, col], statedata[, 2], 
                                             na.last = NA), ]
                hospital <- if(num == "best") {
                        orderdata[1, 2]
                } else if(num == "worst") {
                        orderdata[nrow(orderdata), 2]
                } else{
                        orderdata[num, 2]
                }
                output <- append(output, c(hospital, states[i]))
        }
        
        ## Return a data frame with the hospital names and the (abbreviated) state name
        output <- as.data.frame(matrix(output, length(states), 2, byrow = TRUE))
        colnames(output) <- c("hospital", "state")
        rownames(output) <- states
        
        output
}