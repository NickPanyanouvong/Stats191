## Functions

# Takes in a dataframe, two variables, and an upper-lower bound,
# and fits it with 0 before the lower bound, 1 after the upper,
# and a linear regression in between. This may be discontinuous,
# but the hope is that at the optimized point it will be continuous.

piecewise_residual_square <- function(df, dependent, independent, low, high) {
  piece_one <- sum(dependent[independent <= low]^2)
  if(low != high) {
    yvals <- dependent[independent < high]
    middle <- independent[independent < high]
    yvals <- yvals[middle > low]
    middle <- middle[middle > low]
    yvals <- yvals[!is.na(yvals)]
    middle <- middle[!is.na(middle)]
    piece_two <- sum(((middle - low)/(high-low) - yvals)^2)
  } else {
    piece_two <- 0
  }
  piece_three <- sum((1-dependent[independent >= high])^2)
  return(piece_one + piece_two + piece_three)
}

piecewise_predict <- function(input, model, cutoff) {
  if(input <= -1*cutoff) {
    return(0)
  }
  if(input >= cutoff) {
    return(1)
  }
  return(0.5 + input/(2*cutoff))
}

multiple_piecewise_residual <- function(df, dependent, independent, steps) {
  return(targeted_piecewise_residual(df, dependent, independent, steps, 0, max(max(independent),max(-1*independent)) * 1.05))
}

targeted_piecewise_residual <- function(df, dependent, independent, steps, lower, upper) {
  step <- (upper-lower)/steps
  cutoffs <- c()
  residuals <- c()
  for(i in 0:steps) {
    cutoffs <- append(cutoffs,lower + step * i)
    residuals <- append(residuals,piecewise_residual_square(df,dependent, independent, -1 * (lower + step * i), lower + step * i))
  }
  return(data.frame(cutoffs,residuals))
}

# Just returns a list of z-scores from a list
normalize <- function(statistic){
  return ((statistic - mean(statistic))/var(statistic))
}

# Returns the mode because that doesn't exist by default for some reason
Mode <- function(statistic) {
  uniques <- unique(statistic)
  uniques[which.max(tabulate(match(statistic, uniques)))]
}