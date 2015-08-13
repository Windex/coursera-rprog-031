corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    files_list <- list.files(directory, full.names = TRUE)
    
    thresh_dat <- complete(directory)
    ids <- with(thresh_dat, thresh_dat[nobs > threshold, ])$id
    
    out <- numeric(length(ids))
    
    if (length(ids) > 0) {
        for (i in 1:length(ids)) {
            dat <- read.csv(files_list[ids[i]])
            out[i] <- cor(dat$sulfate, dat$nitrate, use="complete.obs")
        }
    }
    out
}