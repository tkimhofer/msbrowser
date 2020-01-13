#' @title  Transformation functions
#' @param df data.frame with scantime, m/z value and intensity

transf=function(df, trans){

  switch(trans,
        # 'none'={},
         'log10'={df$Int=log10(df$Int)},
         'sqrt'={df$Int=df$Int^2},
         'exp'={df$Int=sqrt(df$Int)},
         'reciprocal'={df$Int=1/(df$Int)},
  )

  return(df)
}
