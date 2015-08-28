best <- function(state, outcome) {
    ## Read outcome data
    data <-
        read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    s <- data[data$State == state,]
    if (0 == nrow(s)) {
        stop("invalid state")
    }
    valid_outcomes <-
        c(
            "heart attack" = 11, "heart failure" = 17, "pneumonia" = 23
        )
    outcol <- names(s)[valid_outcomes[outcome]]
    if (is.na(outcol)) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
    s[,outcol] <- suppressWarnings(as.numeric(s[,outcol]))
    sort(s$Hospital.Name[s[,outcol] == min(s[,outcol], na.rm = TRUE)])
}