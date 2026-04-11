#  Transformation functions
# return data frame with transformed counts
# df is data.frame with scantime, m/z value and intensity
# trans is char indicating transformation methods
transf <- function(df, trans = "log10") {
  stopifnot("Int" %in% colnames(df))

  df2 <- df

  df2$Int <- switch(
    trans,
    none  = df2$Int,
    sqrt  = sqrt(df2$Int),
    log10 = log10(df2$Int + 1),
    stop("Unknown transformation: ", trans)
  )

  df2
}
