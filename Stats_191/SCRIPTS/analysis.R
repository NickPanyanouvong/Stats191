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

# Do regressions on all relevant covariates and add
# the predicted values and residuals into the dataframes

# Because night_owl interacts with the evening and morning variables,
# we need to add extra terms to account for the cross-variable
# interaction - this makes sense, because being a night owl affects
# your performance differently based on the time of day
overallmodel <- lm(`game score` ~ `percent training sessions attended`
                   + `overall fitness score` 
                   + `# extra strategy sessions attended`
                   + `hours of sleep the night before game`
                   + `# meals on day prior to game`
                   + `university year`
                   + `curling` + `gymnastics` + `baseball` + `martial_arts`
                   + `frisbee` + `table_tennis` + `basketball`
                   + evening + morning + night_owl
                   + night_owl * morning + night_owl * evening,
                   data=all_games)
summary(overallmodel) # Run lines like this again to see the model

all_games <- subset(cbind(all_games,predict(overallmodel, se.fit = TRUE)) %>%
                      mutate(residual = `game score` - `fit`), select = -c(`df`,`residual.scale`))

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

## RESIDUAL PLOTS

# We want to plot the residuals of our model's game score predictions against the
# predictions themselves. Let's extract this from all_games and create a scatterplot.

all_games_residuals <- select(all_games, `fit`, `residual`, `se.fit`)
ggplot(all_games_residuals, aes(x = fit, y = residual)) +
  geom_point(color = 'red')

normalized_residuals <- select(normalized_games, `fit`, `residual`, `se.fit`)
ggplot(normalized_residuals, aes(x = fit, y = residual)) +
  geom_point(color = 'blue')
  

##### ALL GAMES ANALYSIS #####

## Errors are approzimately normal
ggplot(data.frame(overallmodel$residuals), aes(x=overallmodel$residuals)) +
  geom_histogram(binwidth=1, color="grey50", fill="lightblue", alpha=0.8) +
  theme_minimal() + 
  stat_function(fun = function(x){dnorm(x,mean = mean(overallmodel$residuals),
                                        sd = sd(overallmodel$residuals))}*length(overallmodel$residuals)*1)

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

# TODO: (difficult) Automatically run pairwise t-tests probing differences in beta.
# Use scaled-down significance threshold to adjust for multiple testing fallacy.

eveningmodel <- lm(`game score` ~ `percent training sessions attended`
                   + `overall fitness score` 
                   + `# extra strategy sessions attended`
                   + `hours of sleep the night before game`
                   + `# meals on day prior to game`
                   + `university year` + `curling` + `gymnastics` + `baseball` + `martial_arts`
                   + `frisbee` + `table_tennis` + `basketball`
                   + night_owl,
                   data=filter(all_games, evening == 1))
summary(eveningmodel)


morningmodel <- lm(`game score` ~ `percent training sessions attended`
                   + `overall fitness score` 
                   + `# extra strategy sessions attended`
                   + `hours of sleep the night before game`
                   + `# meals on day prior to game`
                   + `university year` + `curling` + `gymnastics` + `baseball` + `martial_arts`
                   + `frisbee` + `table_tennis` + `basketball`
                   + night_owl,
                   data=filter(all_games, morning == 1))
summary(morningmodel)

# -----------------------------------------------------------------------------
## RESIDUAL PLOTS
#------------------------------------------------------------------------------
# Let's explore the residual plots of each of these models. Start w/ morning:
morning_residuals <- filter(all_games, morning == 1) %>%
  select(`game score`, `early bird/night owl`)

morning_residuals <- cbind(morning_residuals, predict(morningmodel, se.fit = TRUE)) %>%
  mutate(residuals = `game score` - fit)

# General residual plot

png("../OUTPUTS/Morning_General_Residuals.png")

ggplot(morning_residuals, aes(x = fit, y = residuals)) +
  geom_point(color = 'green') +
  ggtitle("Morning Model Residuals")

dev.off()

png("../OUTPUTS/Morning_Grouped_Residuals.png")

# Grouping by night owl/early bird
ggplot(morning_residuals, aes(x = fit, y = residuals)) +
  geom_point(aes(color = `early bird/night owl`)) +
  ggtitle("Morning Model Residuals")

dev.off()

# Let's explore the evening model now!
evening_residuals <- filter(all_games, evening == 1) %>%
  select(`game score`, `early bird/night owl`)

evening_residuals <- cbind(evening_residuals, predict(eveningmodel, se.fit = TRUE)) %>%
  mutate(residuals = `game score` - fit)

# General residual plot   
png("../OUTPUTS/Evening_General_Residuals.png")

ggplot(evening_residuals, aes(x = fit, y = residuals)) +
  geom_point(color = 'green') +
  ggtitle("Evening Model Residuals")

dev.off()

png("../OUTPUTS/Evening_Grouped_Residuals.png")
# Grouping by night owl/early bird
ggplot(evening_residuals, aes(x = fit, y = residuals)) +
  geom_point(aes(color = `early bird/night owl`)) +
  ggtitle("Evening Model Residuals")

dev.off()

#--------------------------------------------------------------------------
## HYPOTHESIS TESTING
#--------------------------------------------------------------------------

# extract betas and standard errors from evening and morning models
evening_betas <- eveningmodel$coefficients
morning_betas <- morningmodel$coefficients
evening_errors <- sqrt(diag(vcov(eveningmodel)))
morning_errors <- sqrt(diag(vcov(morningmodel)))

# run t-test between evening and morning betas, using adjusted
# significance threshold
new_alpha = 0.05 / length(morning_betas)
new_conf_level = 1 - new_alpha
names_betas <- names(morning_betas)

for (i in 1:length(evening_betas)) {
  betas <- c(evening_betas[i], morning_betas[i])
  std_dev <- sqrt((evening_errors[i])^2 + (morning_errors[i])^2)
  print(names_betas[i])
  print(t.test(betas, mu=0, sd=std_dev, conf.level=new_conf_level))
}

#--------------------------------------------------------------------------
## Finding the best morning & evening players
#--------------------------------------------------------------------------

# Make a hypothetical version of the data where every game was played
# in the or in the evening
morningized_games <- all_games
morningized_games$morning <- 1
morningized_games$evening <- 0

eveningized_games <- all_games
eveningized_games$morning <- 0
eveningized_games$evening <- 1

# Set the variables we see as mutable to the average to remove their effects.
# We decided the training and strategy sessions could be controlled
# the coach on real teams, so we shouldn't consider their impact
# on an individual's score.
morningized_games$`percent training sessions attended` <- mean(all_games$`percent training sessions attended`)
morningized_games$`# extra strategy sessions attended` <- mean(all_games$`# extra strategy sessions attended`)
eveningized_games$`percent training sessions attended` <- mean(all_games$`percent training sessions attended`)
eveningized_games$`# extra strategy sessions attended` <- mean(all_games$`# extra strategy sessions attended`)

# Then predict how the players would have scored. We can use
# overallmodel for this, because we know that the only night_owl
# has a significant difference between morning and evening games
# and its interaction with that has been accounted for.

# It's better to use overallmodel to avoid bias due to 
# non-statistically significant, but still potentially impactful,
# differences between overallmodel and morning/evening model
morningized_games$`game score` <- predict.lm(overallmodel, morningized_games)
eveningized_games$`game score` <- predict.lm(overallmodel, eveningized_games)

# Adjust by the residual, in case there is some confounding variable
# biasing them. (The residual in these dataframes is still the 
# residual from all_games, which is useful here.)
morningized_games$`game score` <- morningized_games$`game score` + morningized_games$residual
eveningized_games$`game score` <- eveningized_games$`game score` + eveningized_games$residual

# Then, average these by player
morning_players <- morningized_games %>% group_by(`student label`) %>% summarize(
  mean = mean(`game score`),
  sd = sd(`game score`)
)
top_morning <- top_n(morning_players,20,mean)
top_morning <- top_morning[order(top_morning$mean,decreasing = TRUE),]

evening_players <- eveningized_games %>% group_by(`student label`) %>% summarize(
  mean = mean(`game score`),
  sd = sd(`game score`)
)
top_evening <- top_n(evening_players,20,mean)
top_evening <- top_evening[order(top_evening$mean,decreasing = TRUE),]


best_morning <- c()
best_evening <- c()
for(i in 1:20) {
  if(!(top_morning$`student label`[i] %in% best_evening)) {
    if(top_morning$`student label`[i] ==
       top_evening$`student label`[i]) {
      if(top_morning$mean[i] - mean(top_morning$mean) >
         top_evening$mean[i] - mean(top_evening$mean)) {
        best_morning <- append(best_morning, top_morning$`student label`[i])
      } else {
        best_evening <- append(best_evening, top_evening$`student label`[i])
      }
    } else {
      best_morning <- append(best_morning, top_morning$`student label`[i])
      best_evening <- append(best_evening, top_evening$`student label`[i])
    }
  }
}

# Keep only the top 10 from each. Those are our teams!
best_morning <- best_morning[1:10]
best_evening <- best_evening[1:10]

#--------------------------------------------------------------------------
## Find our regular season team
#--------------------------------------------------------------------------

# Remove the players used for east coast/west coast cups
adjusted_games <- filter(all_games,!(`student label` %in% best_morning))
adjusted_games <- filter(adjusted_games,!(`student label` %in% best_evening))

# Again, we want to remove the effect of the training and strategy sessions
adjusted_games$`percent training sessions attended` <- mean(all_games$`percent training sessions attended`)
adjusted_games$`# extra strategy sessions attended` <- mean(all_games$`# extra strategy sessions attended`)

adjusted_games$`game score` <- predict.lm(overallmodel, adjusted_games)
adjusted_games$`game score` <- adjusted_games$`game score` + adjusted_games$residual

# Summarize by player
all_players <- adjusted_games %>% group_by(`student label`) %>% summarize(
  mean = mean(`game score`),
  sd = sd(`game score`)
)

# Naively select the best 10, and compare everyone to the 10th best
best_10 <- top_n(all_players,10,mean)
best_10 <- best_10[order(best_10$mean,decreasing = TRUE),]

all_players <- all_players %>% mutate(
  chance_better_than_10 = pnorm((mean - best_10$mean[10])/sqrt(sd^2 + best_10$sd[10]^2))
)
