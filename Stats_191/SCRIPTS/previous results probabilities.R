# Install packages (uncomment and run when needed)
# install.packages('tidyverse')
# install.packages('haven')

# Libraries and helpers
library(tidyverse)
library(haven)
library(rstudioapi)

# Adjust wd
file_path <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(file_path))

# Import helpers
source("helper_functions.R")

# Import processed data
previous_results <- read_csv("../PROCESSED_DATA/processed_previous_results.csv")
match_ups <- read_csv("../RAW_DATA/season_match_up.csv")

##### PREVIOUS RESULTS ANALYSIS #####


## Understanding the distribution of previous results
length(previous_results$score_dif) # n = 200

summary(previous_results$score_dif)

ggplot(previous_results, aes(x=score_dif)) + 
  geom_histogram(binwidth=5, color="grey50", fill="lightblue", alpha=0.8) +
  theme_minimal()

# Looks approximately normally distributed, with a mean of:
mean(previous_results$score_dif) 
#-0.116, and standard deviation of:
score_sd = sd(previous_results$score_dif) 
# 23.328, for a standard error on the mean of:
sd(previous_results$score_dif)/length(previous_results$score_dif)
# 0.116. So the mean is not significantly different from 0,
# and it's approximately normally distributed with SD = 23.328
# This will be useful later.

previous_results_means <- previous_results %>% 
  mutate(bucket = floor(score_dif/5) + 0.5) %>%
  group_by(bucket) %>%
  summarize(
    n = sum(!is.na(`winning team`)), 
    mean = mean(`winning team`)
  )

# From these, we can see that the bulk of the data is between
# -35 and +35 on the score_dif axis. This will be useful later.

# This one shows the winrate in the buckets as a function of score difference:
# (buckets of 5)
# TODO: (fast) Rename the axis on these
ggplot(previous_results_means, aes(x=bucket*5, y=mean, fill=bucket)) + 
  geom_bar(stat="identity") +
  labs(y="Chance of winning", x="Score difference") +
  geom_text(aes(label=round(mean, digits=3)), color="green", vjust=-2.5) + 
  theme_minimal()
# We can see from this that for score_dif < -35, we lose (almost) every game -
# there would be a confidence interval on that which makes for a small chance,
# but anyway - and for score_dif > 35, we win (almost) any game

# Thus, we can model this function piecewise. Below some cutoff, we always lose,
# and above another, we always win. Between, we can model it as linear.

# Notably, this function should be symmetric about score_dif = 0, mean = 0.5
# due to the win chance at score_dif=x being equal to the lose chance at 
# score_dif = -x (in a perfect world)

# Also notably, this function should be continuous (or at least close) -
# this means that we expect the function's confidence interval to contain
# (-35,0) and (35,1), or whatever our cutoffs are.

# This one limits it to those bounds, and makes a pretty reasonable trendline
# It makes an error message but the issue it says is happening isn't actually
# happening
ggplot(filter(previous_results_means, abs(bucket*5) <= 35), aes(x=bucket*5, y=mean, fill=bucket)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=round(mean, digits=3)), color="green", vjust=-2.5) + 
  stat_smooth(method="lm") +
  theme_minimal()

# But we can do better than that! Lets do some regressions

### Regression stuff with helper functions

# Initially, I did this with vertical distance squared. The code
# below is left as a record of that, but is not actually helpful for
# regression - it turns out vertical distance squared has a strong
# bias towards underestimating the cutoff

## With vertical distance squared
piecewise_residual_square(previous_results$`winning team`,
                          previous_results$score_dif, 35)

residual_by_cutoff <- multiple_piecewise_residual(previous_results$`winning team`,
                                                  previous_results$score_dif, 2000, piecewise_residual_square)

# We can minimize this
ggplot(residual_by_cutoff, aes(x=cutoffs, y=residuals)) + 
  geom_point() +
  theme_minimal()

cutoff <- mean(residual_by_cutoff$cutoffs[which(residual_by_cutoff$residuals == min(residual_by_cutoff$residuals))])

# Now we know the region to search in
targeted_range <- targeted_piecewise_residual(previous_results$`winning team`,
                                              previous_results$score_dif, 50000, cutoff - 5, cutoff + 5,
                                              piecewise_residual_square)
# Accurate to three decimal places - because we searched 
# a range of length 10 in 50,000 intervals
cutoff <- round(mean(targeted_range$cutoffs[which(targeted_range$residuals == min(targeted_range$residuals))]),3)


a <- read.csv("../PROCESSED_DATA/cutoff_distribution_demo.csv")$a
# Proof-of-concept simulation:
# WARNING: TAKES 1 MINUTE TO RUN
# a <- append(a,cutoff_distribution(cutoff, 1000, 200, score_sd, piecewise_residual_square))


# This is the plot that makes the bias clear. The true cutoff is shown in
# red, and as you can see, the data are significantly and systematically
# skewed from it. This means we can't trust the results of this regression
# without significant adjustments, so we might as well regress a different
# way
ggplot(data.frame(a), aes(x=a)) +
  geom_histogram(binwidth=0.5, color="grey50", fill="lightblue", alpha=0.8) +
  theme_minimal() + 
  stat_function(fun = function(x){dnorm(x,mean = mean(a),sd = sd(a))}*length(a)*0.5) +
  geom_vline(xintercept = cutoff, color = "red")



# write.csv(data.frame(a),"../PROCESSED_DATA/cutoff_distribution_demo.csv")

# The new objective function is the probability of the observed data
# occurring in the theorized model. All of these chances will be small,
# but some will be much smaller than others.

## With -1 times probability of occurring
# The -1 is so that we can minimize this, instead of maximizing -
# it lets us use the same functions as vertical distance squared
piecewise_probability(previous_results$`winning team`,
                      previous_results$score_dif, 35)

probs_by_cutoff <- multiple_piecewise_residual(previous_results$`winning team`,
                                               previous_results$score_dif, 2000, piecewise_probability)

probs_by_cutoff$residuals <- probs_by_cutoff$residuals/sum(probs_by_cutoff$cutoffs[2] * probs_by_cutoff$residuals)

probs_by_cutoff <- probs_by_cutoff %>% mutate(
  cumprob = cumsum(residuals)*probs_by_cutoff$cutoffs[2]
)

percentile_2.5 <- probs_by_cutoff$cutoff[probs_by_cutoff$cumsum>0.025][1]

percentile_97.5 <- probs_by_cutoff$cutoff[probs_by_cutoff$cumsum>0.975][1]

# Plot with lines denoting the 95% confidence interval for the cutoff
# From this plot, you can already see the expected cutoff and how
# we expect the possible true cutoffs to be distributed given these
# data.
ggplot(probs_by_cutoff, aes(x=cutoffs, y=residuals)) + 
  geom_point() +
  theme_minimal() +
  labs(y="Probability density (given observed results)", x="Cutoff") +
  geom_vline(xintercept = percentile_2.5, color = "red") +
  geom_vline(xintercept = percentile_97.5, color = "red")



cutoff <- mean(residual_by_cutoff$cutoffs[which(residual_by_cutoff$residuals == min(residual_by_cutoff$residuals))])

# Now we know the region to search in
targeted_range <- targeted_piecewise_residual(previous_results$`winning team`,
                                              previous_results$score_dif, 50000, cutoff - 5, cutoff + 5,
                                              piecewise_probability)
# Accurate to three decimal places - because we searched 
# a range of length 10 in 50,000 intervals
cutoff <- round(mean(targeted_range$cutoffs[which(targeted_range$residuals == min(targeted_range$residuals))]),3)


p_distribution <- read.csv("../PROCESSED_DATA/p_cutoff_distribution_demo.csv")$a
# Proof-of-concept simulation:
# WARNING: TAKES 5 MINUTES TO RUN (if you uncomment it)
# p_distribution <- append(p_distribution,cutoff_distribution(cutoff, 10000, 200, score_sd, piecewise_probability))

ggplot(data.frame(p_distribution), aes(x=p_distribution)) +
  geom_histogram(binwidth=0.5, color="grey50", fill="lightblue", alpha=0.8) +
  theme_minimal() + 
  stat_function(fun = function(x){dnorm(x,mean = mean(p_distribution),
                                        sd = sd(p_distribution))}*length(p_distribution)*0.5) +
  geom_vline(xintercept = cutoff)

# Plot shows a little bias on the mean, but none on the mode - 
# seems acceptable

# write.csv(data.frame(p_distribution),"../PROCESSED_DATA/p_cutoff_distribution_demo.csv")

# Now we want probabilities of winning at different score
# differences, given this distribution of cutoffs - the mean chance
# of winning, given that we observed this distribution.

# We'll round to 2 decimal places and evaluate integrals with
# subintervals of 0.01, because those are the gradations on the
# score data we have
rounding_places <- 2

## Takes ~3 minutes to run (if you uncomment it)

# This code just runs all the computations. We stored it so
# we don't have to recalculate it every time.
# It just goes through and for a given score difference, adds up
# the products of winning under a given model times our confidence
# of that model being true under these observations.

# chance_winning <- c()
# score_difs <- c()
# score_step_size <- 10^{-1*rounding_places}
# current_score_dif <- min(previous_results$score_dif)
# no_zeros <- filter(probs_by_cutoff,residuals>0)
# for(j in 1:ceiling((max(previous_results$score_dif)
#                  - min(previous_results$score_dif))/score_step_size)) {
#   score_difs <- append(score_difs,round(current_score_dif, rounding_places))
#   current_sum <- 0
#   for(i in 1:nrow(no_zeros)) {
#       current_sum <- current_sum + no_zeros[i,2] * piecewise_predict(
#         current_score_dif, no_zeros[i,1])
#   }
#   chance_winning <- append(chance_winning,current_sum * probs_by_cutoff$cutoffs[2])
#   current_score_dif <- current_score_dif + score_step_size
# }
# win_chances <- data.frame(score_difs,chance_winning)
# write_csv(win_chances,"../PROCESSED_DATA/confidence_win_rates.csv")

win_chances <- read_csv("../PROCESSED_DATA/confidence_win_rates.csv")

# This is what our probability curve looks like after accounting
# for our confidence distribution in true cutoffs
ggplot(win_chances, aes(x=score_difs, y=chance_winning)) + 
  geom_point() +
  theme_minimal() +
  labs(y="Chance of winning (given observed dist.)", x="Score difference")


