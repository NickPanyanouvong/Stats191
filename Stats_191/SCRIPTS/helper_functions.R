#### Functions

### Graphing

# Generates and saves multiple histograms for the specified COLS of the dataframe
multi_histogram <- function(data, cols){
  
  for(column in cols){
    
    png(paste("../OUTPUTS/", column, ".png"))

    bin_width <- (max(data[[column]]) - min(data[[column]])) / 50 # Adjust bin width
    histo <- ggplot(data = data, aes(x = !!sym(column))) +
      geom_histogram(binwidth = bin_width, color = 'black', fill = 'lightblue') +
      ggtitle(paste(column, " distribution")) +
      theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))
    
    print(histo)
    
    dev.off()
  }
}

### Utility stuff

# Just returns a list of z-scores from a list
normalize <- function(statistic){
  return ((statistic - mean(statistic))/var(statistic))
}

# Returns the mode because that doesn't exist by default for some reason
Mode <- function(statistic) {
  uniques <- unique(statistic)
  uniques[which.max(tabulate(match(statistic, uniques)))]
}

# Determine if an element matches the specified sport
match_sport <- function(strings, sport){
  results <- c()
  for(string in strings){
    if (string == sport){
      results <- c(results, 1)
    } else {
      results <- c(results, 0)
    }
  }
  
  return(results)
}

### Bucketing to build teams efficiently

# Partitions a dataframe into buckets by one variable, while
# each bucket is organized in descending order by another.

# Takes column names in strings.

partition <- function(df, bucketvar, sortvar, bucketsize, maxsize) {
  buckets <- list()
  lower <- min(df[[bucketvar]])
  while(lower <= max(df[[bucketvar]])) {
    temp <- filter(filter(df,!!as.symbol(bucketvar) >= lower),
                 !!as.symbol(bucketvar) < lower + bucketsize)
    temp <- temp[order(temp[[sortvar]], decreasing = TRUE),]
    temp <- filter(temp[1:maxsize,],!is.na(!!as.symbol(bucketvar)))
    buckets[[length(buckets) + 1]] <- temp[order(temp[[sortvar]], decreasing = TRUE),]
    lower <- lower + bucketsize
  }
  return(buckets)
}

# Finds the last index of a vector that is nonzero, excluding the
# index at the end of the vector. (Very specific but useful here.)
last_nonfinal_nonzero <- function(vec) {
  i <- length(vec) - 1
  while(i > 0){
    if(vec[i] != 0) {
      return(i)
    }
    i <- i - 1
  }
  return(0)
}

# Grabs given numbers of players from the appropriate buckets
make_team <- function(amounts, buckets) {
  if(length(amounts) != length(buckets)) {
    print(amounts)
    print(length(buckets))
    stop("Amounts not as long as buckets!")
  }
  output <- NULL
  for(i in 1:length(buckets)) {
    if(amounts[i] > 0) {
      temp <- buckets[[i]]
      output <- rbind(output,temp[1:amounts[i],])
    }
  }
  return(output)
}

# Takes in a list of amounts and the buckets they belong to, and
# increments it by one, moving the rightmost entry right by one,
# or if it's at the last spot, moving the next rightmost right by
# one and resetting everything at the last spot to that spot.

# Example: 2 0 0 0 -> 1 1 0 0  -> 1 0 1 0 -> 1 0 0 1 -> 0 2 0 0

increment_amounts <- function(buckets,bucket_amounts) {
  last <- last_nonfinal_nonzero(bucket_amounts)
  # Reset all the ones at the end to the new beginning, after
  # incrementing the appropriate 'turtle' counter
  add <- bucket_amounts[length(bucket_amounts)]
  bucket_amounts[length(bucket_amounts)] <- 0
  # If everything is in the last cell, return null to signal
  # that we're done
  if(last == 0) {
    print("All at the end!")
    return(NULL)
  }
  bucket_amounts[last] <- bucket_amounts[last] - 1
  bucket_amounts[last + 1] <- 1 + add
  # Not all the buckets are the same length, need to make them fit
  cur <- last + 1
  while(bucket_amounts[cur] > nrow(buckets[[cur]])) {
    # If we're at the last one, we need to roll over by incrementing
    # again
    if(cur == length(bucket_amounts)){
      # Normal increment stuff
      add <- bucket_amounts[length(bucket_amounts)]
      bucket_amounts[length(bucket_amounts)] <- 0
      cur <- last_nonfinal_nonzero(bucket_amounts)
      # If everything is in the last cell, return as usual
      if(cur == 0) {
        return(NULL)
      }
      bucket_amounts[cur] <- bucket_amounts[cur] - 1
      bucket_amounts[cur + 1] <- 1 + add
    } else {
      # Just slide the excess over to the next bucket
      bucket_amounts[cur + 1] <- bucket_amounts[cur + 1] + bucket_amounts[cur] - nrow(buckets[[cur]])
      bucket_amounts[cur] <- nrow(buckets[[cur]])
    }
    cur <- cur + 1
    # Repeat to make sure there's enough space in the 
    # bucket we slid the excess into
  }
  return(bucket_amounts)
}

# Tries all combinations of these players, but you never use
# a player unless all higher-mean players from their sd bucket
# have already been used. Takes a list of dataframes, like above,
# along with auxilary inputs.

# num_sd is the number of standard deviation away from the mean to
# calculate over

bucket_combo_teams <- function(bucketlist, num_sd, odds, score_difs,
                               team_scores, rounding_places, totalruns) {
  # This keeps track of how many people we are pulling from each bucket
  bucket_amounts <- numeric(length(bucketlist))
  bucket_amounts[1] <- 10
  player1 <- c()
  player2 <- c()
  player3 <- c()
  player4 <- c()
  player5 <- c()
  player6 <- c()
  player7 <- c()
  player8 <- c()
  player9 <- c()
  player10 <- c()
  avg_wins <- c()
  undefeated_chance <- c()
  i <- 0
  while(!is.null(bucket_amounts)) {
    players <- make_team(bucket_amounts, bucketlist)
    player1[length(player1)+1] <- players$`student label`[1]
    player2[length(player2)+1] <- players$`student label`[2]
    player3[length(player3)+1] <- players$`student label`[3]
    player4[length(player4)+1] <- players$`student label`[4]
    player5[length(player5)+1] <- players$`student label`[5]
    player6[length(player6)+1] <- players$`student label`[6]
    player7[length(player7)+1] <- players$`student label`[7]
    player8[length(player8)+1] <- players$`student label`[8]
    player9[length(player9)+1] <- players$`student label`[9]
    player10[length(player10)+1] <- players$`student label`[10]
    
    # We can model the distribution of their average easily,
    # because each of their score distributions are normal (see graph)
    team_pdf <- score_pdf_from_players(players$mean,players$sd)
    team_cdf <- score_cdf_from_players(players$mean,players$sd)
    results <- success_measures(odds, score_difs, team_pdf, team_cdf,
                                team_scores, rounding_places, 
                                mean(players$mean) - num_sd * sqrt(sum(players$sd^2)),
                                mean(players$mean) + num_sd * sqrt(sum(players$sd^2)))
    avg_wins[length(avg_wins)+1] <- results[1]
    undefeated_chance[length(undefeated_chance)+1] <- results[2]
    bucket_amounts <- increment_amounts(bucketlist,bucket_amounts)
    i <- i + 1
    if(i %% round(totalruns/100,0) == 0) {
      print(paste(round(i/totalruns * 100,1),"%","done"))
    }
  }
  print(paste("Ran",i,"times"))
  return(data.frame(player1,player2,player3,player4,player5,player6,
                    player7,player8,player9,player10,avg_wins,
                    undefeated_chance))
}

### Piecewise regression stuff

## Piecewise utility functions

# Just takes in a cutoff to determine the model and an input
# to evaluate it at and returns appropriates
piecewise_predict <- function(input, cutoff) {
  if(input <= -1*cutoff) {
    return(0)
  }
  if(input >= cutoff) {
    return(1)
  }
  return(0.5 + input/(2*cutoff))
}

# Same as above but optimized for vectors - significant
# time reduction
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

# These three just define the lower bound of where we should
# bother searching for cutoffs
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
# minimized, so we can treat them the same in our functions

# This one caused bad skew but is included for defending why we
# didn't use it - it's vertical distance squared
piecewise_residual_square <- function(dependent, independent, cutoff) {
  return(sum((dependent - piecewise_multipredict(independent,cutoff))^2))
}

# This one is significantly better, it's the chance of the observed
# game results happening in a particular model
piecewise_probability <- function(dependent, independent, cutoff) {
  return(-1 * abs(prod(1 - dependent - piecewise_multipredict(independent,cutoff))))
}

# Finding residuals for each cutoff

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

## Simulating samples to test for bias

# Represents a single game, randomly determines the result
# based on the model
simulate_piecewise_sample <- function(input, cutoff) {
  return(sample(c(0,1), size = 1,prob = c(1-piecewise_predict(input, cutoff),piecewise_predict(input, cutoff))))
}

# Based on a normal distribution of score differences, this
# uses the above to generate many games in a given model
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


### Finding the best team for normal season

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
                                    score_dif_cdf, rounding_places, low, high) {
  if(low > 0) {
    print("Distribution win chance low end must be below 0!")
    low <- -1
  }
  
  cur_score_dif <- 0
  step_size <- 10^{-1*rounding_places}
  result <- 0
  for(i in 0:ceiling((high)/step_size)) {
    if(confidence_predict(odds,score_difs,cur_score_dif,rounding_places)
       > 0.999) {
      result <- result + score_dif_cdf(cur_score_dif)
      break
    }
    result <- result + step_size * score_dif_density_func(cur_score_dif) *
      confidence_predict(odds,score_difs,cur_score_dif,rounding_places)
    cur_score_dif <- round(cur_score_dif + step_size,rounding_places)
  }
  cur_score_dif <- 0
  for(i in 0:ceiling((low)/step_size)) {
    if(confidence_predict(odds,score_difs,cur_score_dif,rounding_places)
       < 0.001) {
      break
    }
    result <- result + step_size * score_dif_density_func(cur_score_dif) *
      confidence_predict(odds,score_difs,cur_score_dif,rounding_places)
    cur_score_dif <- round(cur_score_dif - step_size,rounding_places)
  }
  return(result)
}

# Uses above with a list of scores of teams we're playing against
# combined with a score_func for our team to calculate the odds
# of never losing a game and average won games - first entry
# is average, second is undefeated odds

success_measures <- function(odds, score_difs, score_func, score_cdf,
                            team_scores, rounding_places, low, high) {
  result <- c(0,1)
  win_chances <- c()
  for(score in team_scores) {
    win_chances <- append(win_chances,distribution_win_chance(
      odds, score_difs, function(x){score_func(x + score)},
      function(x){score_cdf(x + score)}, rounding_places,
      low - score, high - score
    ))
  }
  for(chance in win_chances) {
    result[1] <- result[1] + chance
    result[2] <- result[2] * chance
  }
  return(result)
}

# Assuming a normal distribution, and that the overall game performance
# is the average of the players' game performance, we can make a 
# helper function to calculate the score distribution for the team

# The normal assumption is reasonable - see the analysis sheet,
# there's a helpful plot on it

score_pdf_from_players <- function(player_means, player_sds) {
  output_func <- function(x){dnorm(x,mean = mean(player_means),
                                   sd = sqrt(sum(player_sds^2)))}
  return(output_func)
}

score_cdf_from_players <- function(player_means, player_sds) {
  output_func <- function(x){pnorm(x,mean = mean(player_means),
                                   sd = sqrt(sum(player_sds^2)),
                                   lower.tail = FALSE)}
  return(output_func)
}
