#  Transformation functions
# return data frame with transformed counts
# df is data.frame with scantime, m/z value and intensity
# trans is char indicating transformation methods
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
