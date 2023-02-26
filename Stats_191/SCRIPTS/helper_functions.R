## Functions

# This one isn't in use and doesn't even work 
# But I'm leaving it in case my progress is helpful
# It connects to the task tagged (SPECIAL)

piecewise_residual_square <- function(df, dependent, independent, low, high) {
  piece_one <- deviance(lm(independent = 0, data = filter(df, dependent <= low)))
  piece_two <- deviance(lm(independent = 1/(high - low) * dependent, data = filter(df, dependent > low && dependent < high)))
  piece_three <- deviance(lm(independent = 1, data = filter(df, dependent >= high)))
  return(piece_one + piece_two + piece_three)
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