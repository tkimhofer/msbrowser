#' @title  Transformation functions
#' @param df data.frame with scantime, m/z value and intensity
#' @param trans char indicating transformation methods
#' @return data frame with transformed counts
transf <- function(df, trans) {
    
    switch(trans, log10 = {
        df$Int <- log10(df$Int)
    }, sqrt = {
        df$Int <- df$Int^2
    }, exp = {
        df$Int <- sqrt(df$Int)
    }, reciprocal = {
        df$Int <- 1/(df$Int)
    }, )
    
    return(df)
}
