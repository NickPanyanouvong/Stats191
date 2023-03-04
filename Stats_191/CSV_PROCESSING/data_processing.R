##### RUN THIS CODE BEFORE DOING ANYTHING ELSE #####

# MAKE SURE YOU HAVE ALL CSVs DOWNLOADED AND IN YOUR WORKING DIRECTORY

# Throughout this, there are varying difficulties of tasks marked TODO.
# They represent places where my work has been left at a place where there
# is a natural continuation of my work. Feel free to continue or start anew
# however you see fit, just the TODO activities give leads for stuck moments.

## Dependencies
library(tidyverse)
library(haven)

# Adjust wd, verify
file_path <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(file_path))

source('../SCRIPTS/helper_functions.R') # Import helpers

## Open data and make their storage variables
games <- list(read_csv("../RAW_DATA/game_1_data.csv"),
              read_csv("../RAW_DATA/game_2_data.csv"),
              read_csv("../RAW_DATA/game_3_data.csv"),
              read_csv("../RAW_DATA/game_4_data.csv"),
              read_csv("../RAW_DATA/game_5_data.csv"),
              read_csv("../RAW_DATA/game_6_data.csv"),
              read_csv("../RAW_DATA/game_7_data.csv"),
              read_csv("../RAW_DATA/game_8_data.csv"),
              read_csv("../RAW_DATA/game_9_data.csv"),
              read_csv("../RAW_DATA/game_10_data.csv"))

# Reads the games list into one big dataframe
# with an extra category indicating the game number
all_games <- games[[1]] %>%
  mutate(game = 1)
for (index in 2:10) {
  all_games = rbind(all_games, games[[index]] %>% mutate(game = index))
}

# Converts some of the categoricals in the just-made dataframe into  
# indicator functions. The way I'm doing grades is clunky but I
# couldn't find a better way


# Identify all unique sports
previous_sports_col <- distinct(select(all_games, 'previous competitive sports'))
unique_sports_list <- as.list(previous_sports_col)[[1]]

# Generate indicator columns
columns <- c()

for(i in seq(length(unique_sports_list))){
  sport <- unique_sports_list[[i]]
  print(sport)
  col <- match_sport(all_games$'previous competitive sports', sport)
  columns[[i]] <- col
  
}

# Add all sports indicator columns
all_games <- all_games %>%
  mutate(curling = columns[[1]], gymnastics = columns[[2]], baseball = columns[[3]],
         martial_arts = columns[[4]], frisbee = columns[[5]], table_tennis = columns[[6]],
         basketball = columns[[7]], football = columns[[8]])


all_games <- all_games %>%
  mutate(evening = as.integer(`game scheduled` == "evening"),
         morning = as.integer(`game scheduled` == "morning"),
         `west coast` = as.integer(coast == "west coast"),
         `university year` = ifelse(`university year`=="frosh",1,
                                    ifelse(`university year`=="sophomore",2,
                                           ifelse(`university year`=="junior",3,
                                                  ifelse(`university year`=="senior",4,NA)))),
         night_owl = as.integer(`early bird/night owl` == "night owl"))

#mutate(`winning team` = as_factor(`winning team`)) %>% # Previously used but breaks averages

# NICK CODE: Write to CSV
write.csv(all_games, "../PROCESSED_DATA/processed_all_games.csv", row.names = FALSE)

# Creates a dataframe that has information in one row for each player
# TODO: Fix evening score and morning score (they are currently broken)
# TODO: Add variances on each covariate? Could be good for confidence intervals
# TODO: Add indicator function for previous sports once added to all_games
player_means <- distinct(all_games %>%
                           group_by(`student label`) %>%
                           summarize(`game score` = mean(`game score`),
                                     `evening score` = sum(`game score` * evening)/sum(evening),
                                     `morning score` = sum(morning * `game score`)/sum(morning),
                                     `percent training sessions attended` = mean(`percent training sessions attended`),
                                     `overall fitness score` = mean(`overall fitness score`),
                                     `# extra strategy sessions attended` = mean(`# extra strategy sessions attended`),
                                     `hours of sleep the night before game` = mean(`hours of sleep the night before game`),
                                     `previous competitive sports` = Mode(`previous competitive sports`),
                                     `# meals on day prior to game` = mean(`# meals on day prior to game`),
                                     `university year` = Mode(`university year`),
                                     evening = mean(evening),
                                     morning = mean(morning),
                                     night_owl = Mode(night_owl)))

# Normalized version of all_games, except for indicator function variables
# Useful for the 'how best to coach a player' portion, we can compare
# beta values directly with this.
normalized_games <- all_games %>%
  mutate(`percent training sessions attended` = normalize(`percent training sessions attended`),
         `overall fitness score` = normalize(`overall fitness score`),
         `# extra strategy sessions attended` = normalize(`# extra strategy sessions attended`),
         `hours of sleep the night before game` = normalize(`hours of sleep the night before game`),
         `# meals on day prior to game` = normalize(`# meals on day prior to game`),
         `university year` = normalize(`university year`))

match_ups <- read_csv("../RAW_DATA/season_match_up.csv")
previous_results <- read_csv("../RAW_DATA/previous_season_results.csv")

write.csv(all_games, "../PROCESSED_DATA/processed_normalized_all_games.csv", row.names = FALSE)

# Add a score difference variable
previous_results <- previous_results %>%
  mutate(score_dif = `team 1 score` - `team 2 score`)

write.csv(previous_results, "../PROCESSED_DATA/processed_previous_results.csv", row.names = FALSE)