rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <-
        read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    s <- data[data$State == state,]
    if (0 == nrow(s))
        stop("invalid state")
    valid_outcomes <-
        c(
            "heart attack" = 11, "heart failure" = 17, "pneumonia" = 23
        )
    outcol <- names(s)[valid_outcomes[outcome]]
    if (is.na(outcol))
        stop("invalid outcome")
    
    ## Return hospital name in that state with the given rank 30-day death rate
    s[,outcol] <- suppressWarnings(as.numeric(s[,outcol]))
    ranking <-
        s$Hospital.Name[order(s[,outcol], s$Hospital.Name, na.last = NA)]
    if ("best" == num)
        ranking[1]
    else if ("worst" == num)
        ranking[length(ranking)]
    else
        ranking[num]
}