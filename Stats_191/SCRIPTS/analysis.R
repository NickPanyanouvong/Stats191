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
normalized_games <- read_csv("../PROCESSED_DATA/processed_normalized_all_games.csv")
all_games <- read_csv("../PROCESSED_DATA/processed_all_games.csv")
previous_results <- read_csv("../PROCESSED_DATA/processed_previous_results.csv")
match_ups <- read_csv("../RAW_DATA/season_match_up.csv")

## Preliminary transformations and regressions

# Do regressions on (almost) all relevant covariates and add
# the predicted values and residuals into the dataframes
# TODO: add previous sport experience to both after making indicator functions
overallmodel <- lm(`game score` ~ `percent training sessions attended`
                   + `overall fitness score` 
                   + `# extra strategy sessions attended`
                   + `hours of sleep the night before game`
                   + `# meals on day prior to game`
                   + `university year`
                   + evening + morning + night_owl,
                   data=all_games)
summary(overallmodel) # Run lines like this again to see the model

all_games <- subset(cbind(all_games,predict(overallmodel, se.fit = TRUE)) %>%
                      mutate(residual = `game score` - fit), select = -c(df,residual.scale))

normalizedmodel <- lm(`game score` ~ `percent training sessions attended`
                      + `overall fitness score` 
                      + `# extra strategy sessions attended`
                      + `hours of sleep the night before game`
                      + `# meals on day prior to game`
                      + `university year`
                      + evening + morning + night_owl,
                      data=normalized_games)

summary(normalizedmodel)

normalized_games <- subset(cbind(normalized_games,predict(normalizedmodel, se.fit = TRUE)) %>%
                             mutate(residual = `game score` - fit), select = -c(df,residual.scale))


##### ALL GAMES ANALYSIS #####

# Number of meals is initially positively correlated
model <- lm(`game score` ~ `# meals on day prior to game`,
            data=all_games)
summary(model)
ggplot(all_games, aes(x=`# meals on day prior to game`,y=`game score`)) + 
  stat_smooth(method = "lm") +
  geom_point() +
  theme_minimal()
# But after accounting for training sessions attended, it's negatively correlated (and significantly so)
model <- lm(`game score` ~ `# meals on day prior to game` + `percent training sessions attended`,
            data=all_games)
summary(model)
ggplot(all_games, aes(x=`# meals on day prior to game`,y=`percent training sessions attended`)) + 
  stat_smooth(method = "lm") +
  geom_point() +
  theme_minimal()

# There's two ways to manage this: we can say that there might be an unknown
# causal relationship present and so it's simply impossible to vary the number
# of meals eaten without affecting the other covariates, which is what the 
# beta value represents (think of the midterm). OR we can ignore that and say
# we should have them eat less meals for better coaching - I think the former
# is better.





## Models for morning and evening games

# This is useful for checking whether our model is a good fit - 
# specifically, whether the relationship between game score and
# the other covariates depends on whether it's a morning or evening game.

# Notably, the most significant difference (by far) between the two is 
# for the night_owl indicator variable - which is what we expect to see,
# intuitively. None of the other ones are particularly significant,
# especially with the multiple testing fallacy. There is also a significant
# difference in the intercept - which makes sense too, because the intercept
# represents early birds. So early birds have an 11 point difference (after
# accounting for changes in the other variables) between morning and evening
# games, while night owls only have about a 3 point difference

# TODO: (easy) add code here that compares the average of early birds in
# morning games to night owls in night games, or code that compares the
# difference like I did verbally

# TODO: (difficult) Automatedly do a significance test on to see 

# TODO: add indicator functions (once added to all_games) to the below
# regressions
eveningmodel <- lm(`game score` ~ `percent training sessions attended`
                   + `overall fitness score` 
                   + `# extra strategy sessions attended`
                   + `hours of sleep the night before game`
                   + `# meals on day prior to game`
                   + `university year`
                   + night_owl,
                   data=filter(all_games, evening == 1))
summary(eveningmodel)


morningmodel <- lm(`game score` ~ `percent training sessions attended`
                   + `overall fitness score` 
                   + `# extra strategy sessions attended`
                   + `hours of sleep the night before game`
                   + `# meals on day prior to game`
                   + `university year`
                   + night_owl,
                   data=filter(all_games, morning == 1))
summary(morningmodel)






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

# I couldn't find a better way to make a histogram of this type
previous_results_means <- previous_results %>% 
  mutate(bucket = floor(score_dif/5) + 0.5) %>%
  group_by(bucket) %>%
  summarize(
    n = sum(!is.na(`winning team`)), 
    mean = mean(`winning team`)
  )

# From these, we can see that the bulk of the data is between
# -35 and +35 on the score_dif axis. This will be useful later.

# TODO: (easy) add code here that finds exactly what percentage is between
# -35 and +35 - optimally, we want more than 90 or 95%



## Understanding how score_dif affects win chance

model <- lm(`winning team` ~ score_dif,
            data=previous_results)
summary(model)

# TODO: Do the below graph using ggplot so it matches with the other graphs

plot(`winning team` ~ score_dif, data = previous_results)
abline(model)

# This one shows the winrate in the buckets as a function of score difference:
# (buckets of 5)
# TODO: (fast) Rename the axis on these
ggplot(previous_results_means, aes(x=bucket*5, y=mean, fill=bucket)) + 
  geom_bar(stat="identity") +
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

# The equation for the trendline from that is below. Keep in mind the
# slope should be divided by five to convert to score_dif units
model <- lm(mean ~ bucket,
            data=filter(previous_results_means, abs(bucket*5) <= 35))
summary(model)

# Here's a more accurate, very similar trendline that doesn't use buckets
model <- lm(`winning team` ~ score_dif,
            data=filter(previous_results, abs(score_dif) < 35))
summary(model)

### Regression stuff with helper functions

## With vertical distance squared
piecewise_residual_square(previous_results$`winning team`,
                          previous_results$score_dif, 35)

residual_by_cutoff <- multiple_piecewise_residual(previous_results$`winning team`,
                            previous_results$score_dif, 2000, piecewise_residual_square)

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

ggplot(data.frame(a), aes(x=a)) +
  geom_histogram(binwidth=0.5, color="grey50", fill="lightblue", alpha=0.8) +
  theme_minimal() + 
  stat_function(fun = function(x){dnorm(x,mean = mean(a),sd = sd(a))}*length(a)*0.5)

write.csv(data.frame(a),"../PROCESSED_DATA/cutoff_distribution_demo.csv")

## With -1 times probability of occurring
# The -1 is so that we can minimize this, instead of maximizing
piecewise_probability(previous_results$`winning team`,
                          previous_results$score_dif, 35)

probs_by_cutoff <- multiple_piecewise_residual(previous_results$`winning team`,
                                                  previous_results$score_dif, 2000, piecewise_probability)

probs_by_cutoff$residuals <- probs_by_cutoff$residuals/sum(probs_by_cutoff$cutoffs[2] * probs_by_cutoff$residuals)

probs_by_cutoff <- probs_by_cutoff %>% mutate(
  rownum = row_number()
)

probs_by_cutoff <- probs_by_cutoff %>% mutate(
  cumsum = cumsum(residuals)*probs_by_cutoff$cutoffs[2]
)

percentile_2.5 <- probs_by_cutoff$cutoff[probs_by_cutoff$cumsum>0.025][1]

percentile_97.5 <- probs_by_cutoff$cutoff[probs_by_cutoff$cumsum>0.975][1]

# Plot with lines denoting the 95% confidence interval for the cutoff

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
# WARNING: TAKES 30 SECONDS TO RUN
# p_distribution <- append(p_distribution,cutoff_distribution(cutoff, 10000, 200, score_sd, piecewise_probability))

ggplot(data.frame(p_distribution), aes(x=p_distribution)) +
  geom_histogram(binwidth=0.5, color="grey50", fill="lightblue", alpha=0.8) +
  theme_minimal() + 
  stat_function(fun = function(x){dnorm(x,mean = mean(p_distribution),
          sd = sd(p_distribution))}*length(p_distribution)*0.5) +
  geom_vline(xintercept = cutoff)

# write.csv(data.frame(p_distribution),"../PROCESSED_DATA/p_cutoff_distribution_demo.csv")
# A side note:

# I really want to do a a regression of the form:
# p = 1/pi arctan(beta * score_dif) + 1/2
# but it just won't happen. Thus why I did the piecewise linear above

# On further examination, maybe this is a logistic curve!
# We'll see if we learn a technique for that though.

# (This is an attempted transformation to accomplish it with linear
# regression, but it failed because this should really be using
# horizontal squared distance.)

# TODO: (??? difficulty) try the below regression with horizontal
# squared distance

# Looks like horizontal squared distance doesn't exist in r/is just
# accomplished by switching the axes, which ruins the point here.
# I think this is a dead end
model <- lm(score_dif ~ tan(pi*(`winning team`-1/2)),
            data=previous_results)
summary(model)
plot(score_dif ~ `winning team`, data = previous_results)
abline(model)


