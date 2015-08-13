pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    
    files_list <- list.files(directory, full.names = TRUE)
    
    mns <- c()
    wts <- c()
    
    for (i in id) {
        dat <- read.csv(files_list[i])
        p <- dat[!is.na(dat[, pollutant]), pollutant]
        mns <- c(mns, mean(p))
        wts <- c(wts, length(p))
    }
    weighted.mean(mns, wts)
}