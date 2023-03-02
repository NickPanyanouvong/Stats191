## Functions

# Takes in a dataframe, two variables, and an upper-lower bound,
# and fits it with 0 before the lower bound, 1 after the upper,
# and a linear regression in between. This may be discontinuous,
# but the hope is that at the optimized point it will be continuous.

piecewise_residual_square <- function(df, dependent, independent, low, high) {
  piece_one <- sum(dependent[independent <= low]^2)
  piece_two <- deviance(lm(dependent[independent > low][independent < high] ~ independent[independent > low][independent < high], data = filter(filter(df,score_dif < high),score_dif > low)))
  piece_three <- sum((1-dependent[independent >= high])^2)
  return(piece_one + piece_two + piece_three)
}


multiple_piecewise_residual <- function(df, dependent, independent, steps) {
  maxcutoff <- max(max(independent),max(-1*independent)) * 1.05
  step <- maxcutoff/steps
  cutoffs <- c()
  residuals <- c()
  for(i in 0:steps) {
    cutoffs <- append(cutoffs,step * i)
    residuals <- append(residuals,piecewise_residual_square(df,dependent, independent, -1 * step * i, step * i))
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