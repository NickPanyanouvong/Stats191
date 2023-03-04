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








