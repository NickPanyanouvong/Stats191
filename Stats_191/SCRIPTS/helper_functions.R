#### Functions

## Utility stuff

# Just returns a list of z-scores from a list
normalize <- function(statistic){
  return ((statistic - mean(statistic))/var(statistic))
}

# Returns the mode because that doesn't exist by default for some reason
Mode <- function(statistic) {
  uniques <- unique(statistic)
  uniques[which.max(tabulate(match(statistic, uniques)))]
}

### Piecewise regression stuff

## General utility
piecewise_predict <- function(input, cutoff) {
  if(input <= -1*cutoff) {
    return(0)
  }
  if(input >= cutoff) {
    return(1)
  }
  return(0.5 + input/(2*cutoff))
}

piecewise_multipredict <- function(input_vec, cutoff) {
  output = vector(mode(input_vec),length(input_vec))
  output[input_vec <= -1*cutoff] <- 0
  output[input_vec >= cutoff] <- 1
  parsed <- input_vec[input_vec < cutoff]
  output[input_vec < cutoff][parsed
      >-1 * cutoff] <- 0.5 + input_vec[input_vec < cutoff][parsed
      >-1 * cutoff]/(2*cutoff)
  return(output)
}

lower_limit <- function(dependent, independent) {
  return(min(abs(last_loss(dependent,independent)),abs(first_win(dependent,independent))))
}

last_loss <- function(dependent, independent) {
  return(min(independent[dependent == 1]))
}

first_win <- function(dependent, independent) {
  return(max(independent[dependent == 0]))
}

## Objective functions

# Notice how these have the same arguments, and both want to be
# minimized, so we can treat them the same

# Takes in a dataframe, two variables, and an upper-lower bound,
# and fits it with 0 before the lower bound, 1 after the upper,
# and a linear regression in between. This may be discontinuous,
# but the hope is that at the optimized point it will be continuous.

piecewise_residual_square <- function(dependent, independent, cutoff) {
  return(sum((dependent - piecewise_multipredict(independent,cutoff))^2))
}

piecewise_probability <- function(dependent, independent, cutoff) {
  return(-1 * abs(prod(1 - dependent - piecewise_multipredict(independent,cutoff))))
}

# Finding residauls for each cutoff

multiple_piecewise_residual <- function(dependent, independent, steps, objective_func) {
  return(targeted_piecewise_residual(dependent, independent, steps, 0, max(max(independent),max(-1*independent)) * 1.05, objective_func))
}

targeted_piecewise_residual <- function(dependent, independent, steps, lower, upper, objective_func) {
  step <- (upper-lower)/steps
  cutoffs <- c()
  residuals <- c()
  for(i in 0:steps) {
    cutoffs <- append(cutoffs,lower + step * i)
    residuals <- append(residuals,objective_func(dependent, independent, lower + step * i))
  }
  return(data.frame(cutoffs,residuals))
}

# Using these to find the best cutoff

optimize_cutoff <- function(dependent, independent, objective_func) {
  cutoff <- find_cutoff(dependent, independent, 1000, 
                        lower_limit(dependent,independent) * 0.95,
                        max(abs(independent)) * 1.05, objective_func)
  cutoff <- find_cutoff(dependent, independent, 1000, 
                        cutoff + 5, cutoff - 5, objective_func)
  cutoff <- find_cutoff(dependent, independent, 400, 
                        cutoff + 0.1, cutoff - 0.1, objective_func)
  return(round(cutoff,3))
}

find_cutoff <- function(dependent, independent, steps, lower, upper, objective_func) {
  residual_by_cutoff <- targeted_piecewise_residual(dependent, independent, steps, lower, upper, objective_func)
  return(mean(residual_by_cutoff$cutoffs[which(residual_by_cutoff$residuals == min(residual_by_cutoff$residuals))]))
}

# Simulating samples to test for bias

simulate_piecewise_sample <- function(input, cutoff) {
  return(sample(c(0,1), size = 1,prob = c(1-piecewise_predict(input, cutoff),piecewise_predict(input, cutoff))))
}

simulate_piecewise_distribution <- function(cutoff, n, score_sd) {
  score_difs = rnorm(n, sd=score_sd)
  winners = c()
  for(item in score_difs) {
    winners <- append(winners, simulate_piecewise_sample(item, cutoff))
  }
  return(data.frame(score_difs, winners))
}

# Can run ~1000 sims of n=200 per minute on residual sqaured,
# and the same in 30 seconds on probability
cutoff_distribution <- function(true_cutoff, numsims, numsamples, score_sd, objective_func) {
  measured_cutoffs <- c()
  for(i in 1:numsims) {
    score_by_winners <- simulate_piecewise_distribution(true_cutoff, 
                                                        numsamples, score_sd)
    measured_cutoffs <- append(measured_cutoffs,optimize_cutoff(score_by_winners$winners,
                                                                score_by_winners$score_difs,
                                                                objective_func))
    if(i %% (numsims/100) == 0) {
      print(paste(i/numsims*100,"%"))
    }
  }
  return(measured_cutoffs)
}

### Finding the best team for normal game

# Takes in lists of win chances and score differences, an input, and
# the number of digits the entries in the dataframe are rounded to,
# and outputs the associated win chance

confidence_predict <- function(odds, score_difs, input, rounding_places) {
  if(round(input,rounding_places) %in% score_difs) {
    return(odds[score_difs
              == round(input, digits = rounding_places)])
  }
  if(round(input,rounding_places) < min(score_difs)) {
    return(odds[1])
  }
  if(round(input,rounding_places) > max(score_difs)) {
    return(odds[length(odds)])
  }
  print("confidence_predict error! Score_difs have gaps")
}

# Takes in a normalized probability density function for score_dif
# and outputs the chances of winning. Also needs auxilary inputs
# like rounding places and the chances of winning at score_difs,
# along with the bounds to search over

# score_dif_density_func should output probability density as a function
# of score_dif

distribution_win_chance <- function(odds, score_difs, score_dif_density_func,
                                    rounding_places, low, high) {
  cur_score_dif <- low
  step_size <- 10^{-1*rounding_places}
  result <- 0
  for(i in 0:ceiling((high - low)/step_size)) {
    result <- result + step_size * score_dif_density_func(cur_score_dif) *
      confidence_predict(odds,score_difs,cur_score_dif,rounding_places)
    cur_score_dif <- round(cur_score_dif + step_size,rounding_places)
  }
  return(result)
}

# Uses above with a list of scores of teams we're playing against
# combined with a score_func for our team to calculate the odds
# of never losing a game

undefeated_odds <- function(odds, score_difs, score_func, team_scores,
                            rounding_places, low, high) {
  result <- 1
  for(score in team_scores) {
    result <- result * distribution_win_chance(
      odds, score_difs, function(x){score_func(x + score)},
      rounding_places, low - score, high - score
    )
  }
  return(result)
}

# Same as above, but calculates average won games

expected_wins <- function(odds, score_difs, score_func, team_scores,
                            rounding_places, low, high) {
  result <- 0
  for(score in team_scores) {
    result <- result + distribution_win_chance(
      odds, score_difs, function(x){score_func(x + score)},
      rounding_places, low - score, high - score
    )
  }
  return(result)
}
