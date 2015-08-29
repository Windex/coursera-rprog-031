rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <-
        read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome is valid
    valid_outcomes <-
        c(
            "heart attack" = 11, "heart failure" = 17, "pneumonia" = 23
        )
    outcol <- names(data)[valid_outcomes[outcome]]
    if (is.na(outcol))
        stop("invalid outcome")
    
    ## For each state, find the hospital of the given rank
    data[,outcol] <- suppressWarnings(as.numeric(data[,outcol]))
    
    ranks <- NULL
    states <- sort(unique(data$State))
    for (state in states) {
        s <- data[data$State == state, c("Hospital.Name", "State", outcol)]
        
        r <-
            s$Hospital.Name[order(s[,outcol], s$Hospital.Name, na.last = NA)]
        result <- if ("best" == num)
            r[1]
        else if ("worst" == num)
            r[length(r)]
        else
            r[num]
        ranks <-
            rbind(ranks, data.frame(hospital = result, state = state))
    }
    
    ## Return a data frame with the hospital names and the (abbreviated) state name
    ranks
}