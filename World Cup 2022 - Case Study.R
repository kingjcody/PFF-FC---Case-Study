## Libraries ##
library(readxl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(writexl)
library(gt)
library(dplyr)
library(RSQLite)
library(tidyverse)
library(tictoc)
library(skimr)
library(sqldf)
library(gtExtras)
library(ggplot2)
library(ggdist)
library(ggimage)
library(plotly)
library(flexdashboard)
library(shiny)
library(shinydashboard)
library(extrafont)
library(Metrics)
library(glmnet)
library(caret)
library(progress)
library(data.table)
library(purrr)
library(utils)
library(png)
library(grid)
library(rvest)
library(httr)

## Load Files ##
file_paths <- c(
  "C:/Users/cking/OneDrive/Documents/Cody King/PFF/competitions.csv",
  "C:/Users/cking/OneDrive/Documents/Cody King/PFF/players.csv",
  "C:/Users/cking/OneDrive/Documents/Cody King/PFF/metadata.csv",
  "C:/Users/cking/OneDrive/Documents/Cody King/PFF/events.json",
  "C:/Users/cking/OneDrive/Documents/Cody King/PFF/rosters.csv"
)

# Loop to load files
load_file <- function(file_path) {
  if (grepl("\\.csv$", file_path)) {
    return(read.csv(file_path, stringsAsFactors = FALSE))
  } else if (grepl("\\.json$", file_path)) {
    return(jsonlite::fromJSON(file_path, flatten = TRUE))
  } else {
    stop("Unsupported file type")
  }
}


# Load all files into a list
loaded_files <- lapply(file_paths, load_file)


# Assign names based on file paths
names(loaded_files) <- gsub(".*/(.*)\\..*", "\\1", file_paths)

# Load to environment
list2env(loaded_files, envir = .GlobalEnv)


## Clean Competitions ##
competitions <- competitions %>%
  mutate(
    games_fixed = gsub("'", "\"", games),
    games_parsed = lapply(games_fixed, function(x) tryCatch(fromJSON(x), error = function(e) NULL))
  ) %>%
  unnest_longer(games_parsed) %>%
  unnest_wider(games_parsed, names_sep = "_") %>%
  select(id, name, games_parsed_id, games_parsed_season)

write_xlsx(competitions, "competitions_clean.xlsx")
cat("Filtered JSON data saved to 'competitions_clean.xlsx'\n")

## Clean Metadata ##
# Function to clean and parse JSON strings
clean_and_parse_json <- function(text) {
  cleaned_text <- gsub("'", "\"", text) # Replace single quotes with double quotes
  cleaned_text <- gsub('""', '"', cleaned_text) # Remove redundant double quotes
  tryCatch(jsonlite::fromJSON(cleaned_text), error = function(e) NULL) # Safely parse JSON
}

# Parsing and cleaning metadata
metadata <- metadata %>%
  mutate(
    awayTeam_parsed = lapply(awayTeam, clean_and_parse_json),
    awaykit_parsed = lapply(awayTeamKit, clean_and_parse_json),
    homeTeam_parsed = lapply(homeTeam, clean_and_parse_json),
    homekit_parsed = lapply(homeTeamKit, clean_and_parse_json),
    stadium_parsed = lapply(stadium, clean_and_parse_json),
    video_parsed = lapply(videos, clean_and_parse_json)
  ) %>%
  mutate(
    # Extracting team details
    away_id = sapply(awayTeam_parsed, `[[`, "id"),
    away_name = sapply(awayTeam_parsed, `[[`, "name"),
    away_shortName = sapply(awayTeam_parsed, `[[`, "shortName"),
    home_id = sapply(homeTeam_parsed, `[[`, "id"),
    home_name = sapply(homeTeam_parsed, `[[`, "name"),
    home_shortName = sapply(homeTeam_parsed, `[[`, "shortName"),
    
    # Extracting kit colors
    away_primaryColor = sapply(awaykit_parsed, `[[`, "primaryColor"),
    away_primaryTextColor = sapply(awaykit_parsed, `[[`, "primaryTextColor"),
    away_secondaryColor = sapply(awaykit_parsed, `[[`, "secondaryColor"),
    away_secondaryTextColor = sapply(awaykit_parsed, `[[`, "secondaryTextColor"),
    home_primaryColor = sapply(homekit_parsed, `[[`, "primaryColor"),
    home_primaryTextColor = sapply(homekit_parsed, `[[`, "primaryTextColor"),
    home_secondaryColor = sapply(homekit_parsed, `[[`, "secondaryColor"),
    home_secondaryTextColor = sapply(homekit_parsed, `[[`, "secondaryTextColor"),
    
    # Extracting stadium details
    stadium_id = sapply(stadium_parsed, `[[`, "id"),
    stadium_name = sapply(stadium_parsed, `[[`, "name"),
    stadium_pitchLength = sapply(stadium_parsed, `[[`, "pitchLength"),
    stadium_pitchWidth = sapply(stadium_parsed, `[[`, "pitchWidth"),
    
    # Extracting video details
    video_id = sapply(video_parsed, `[[`, "id"),
    videourl = sapply(video_parsed, `[[`, "videoUrl"),
    video_fps = sapply(video_parsed, `[[`, "fps")
  ) %>%
  mutate(
    # Handling date and time
    date = as.POSIXct(date, format = "%Y-%m-%dT%H:%M:%S"),
    date_only = as.Date(date),
    time_only = format(date, "%H:%M:%S")
  ) %>%
  # Selecting relevant columns
  select(-awayTeam, -awayTeamKit, -homeTeam, -homeTeamKit, -stadium, -videos, -competition, -date, 
         -awayTeam_parsed, -awaykit_parsed, -homeTeam_parsed, -homekit_parsed, -stadium_parsed, -video_parsed)

# Save the cleaned dataset
write.csv(metadata, "metadata_cleaned.csv", row.names = FALSE)
cat("Parsed metadata data saved to 'metadata_cleaned.csv'\n")

## Clean Rosters ##
clean_json_for_parsing <- function(text) {
  if (is.na(text) || text == "") return(text)
  
  cleaned_text <- iconv(text, to = "UTF-8", sub = "byte")
  player_name_pattern <- "([A-Za-z]+['’][A-Za-z]+)"
  player_names <- unique(unlist(regmatches(cleaned_text, gregexpr(player_name_pattern, cleaned_text))))
  
  for (name in player_names) {
    cleaned_text <- gsub(name, paste0("<PLAYER_NAME_", gsub("['’]", "_", name), ">"), cleaned_text)
  }
  
  cleaned_text <- gsub('(?<!\\\\)"', '\\"', cleaned_text, perl = TRUE)
  cleaned_text <- gsub("'", "\"", cleaned_text)
  
  for (name in player_names) {
    placeholder <- paste0("<PLAYER_NAME_", gsub("['’]", "_", name), ">")
    cleaned_text <- gsub(placeholder, name, cleaned_text)
  }
  
  return(cleaned_text)
}

safe_parse_json <- function(json_string) {
  tryCatch(fromJSON(json_string), error = function(e) json_string)
}

rosters <- rosters %>%
  mutate(
    player_fixed = sapply(player, clean_json_for_parsing),
    team_fixed = sapply(team, clean_json_for_parsing),
    player_parsed = lapply(player_fixed, safe_parse_json),
    team_parsed = lapply(team_fixed, safe_parse_json)
  ) %>%
  mutate(
    player_id = sapply(player_parsed, function(x) if (is.list(x)) x$id else NA),
    player_nickname = sapply(player_parsed, function(x) if (is.list(x)) x$nickname else x),
    team_id = sapply(team_parsed, function(x) if (is.list(x)) x$id else NA),
    team_name = sapply(team_parsed, function(x) if (is.list(x)) x$name else x)
  )

rosters <- rosters %>%
  select(game_id, player_id, player_nickname, positionGroupType, shirtNumber, team_id, team_name)

write_xlsx(rosters, "rosters_cleaned.xlsx")
cat("Parsed rosters data saved to 'rosters_cleaned.xlsx'\n")

## Clean Events ##
events <- events %>%
  filter(gameId != 10517) # Remove the World Cup Final

# Extract and unnest the possessionEvents into a separate data frame
possession_events <- events$possessionEvents %>%
  map_dfr(~as.data.frame(.x, stringsAsFactors = FALSE)) # Unnest and convert to data frame


## Upload Tracking ##########################################################################################################
#Decompress the file
R.utils::bunzip2("C:/Users/cking/Downloads/10515.jsonl.bz2", remove = FALSE)

# Read and process the JSON file
tracking <- readLines("C:/Users/cking/Downloads/10515.jsonl") %>%
  map(fromJSON) %>% 
  map_dfr(unlist)

## Teams Metadata ##
# Combine metadata to get unique team information
teams <- metadata %>%
  select(away_id, away_name, away_shortName, away_primaryColor, away_primaryTextColor, away_secondaryColor, away_secondaryTextColor) %>%
  rename(team_id = away_id, team_name = away_name, shortName = away_shortName, primaryColor = away_primaryColor, primaryTextColor = away_primaryTextColor, secondaryColor = away_secondaryColor, secondaryTextColor = away_secondaryTextColor) %>%
  bind_rows(
    metadata %>%
      select(home_id, home_name, home_shortName, home_primaryColor, home_primaryTextColor, home_secondaryColor, home_secondaryTextColor) %>%
      rename(team_id = home_id, team_name = home_name, shortName = home_shortName, primaryColor = home_primaryColor, primaryTextColor = home_primaryTextColor, secondaryColor = home_secondaryColor, secondaryTextColor = home_secondaryTextColor)
  ) %>%
  distinct(team_id, .keep_all = TRUE)

#change south korea to korea republic
teams$team_name[teams$team_name == "South Korea"] <- "Korea Republic"

## Download Team Logos ##
# Specify the URL
url <- "https://logos.fandom.com/wiki/Logopedia:Theme/Logos_of_2022_FIFA_World_Cup_competing_teams"

# Read the Page
webpage <- read_html(url)

# Extract Images
logos <- webpage %>%
  html_nodes("img") %>%
  html_attr("src")

# Filter relevant images
logos <- logos[grepl("static.wikia", logos)] 

# Define URL
base_url <- "https://static.wikia.nocookie.net"

# Only base URL to relative URLs
logos <- ifelse(grepl("^http", logos), logos, paste0(base_url, logos))

# Folder where the images will be stored
image_folder <- "C:/Users/cking/OneDrive/Documents/Cody King/PFF/PFF - Logos/"


# Download the images
for (i in seq_along(logos)) {
  # Construct the file path where the image will be saved
  image_path <- paste0(image_folder, "logo_", i, ".png")

  # Download the image
  download.file(logos[i], image_path, mode = "wb")
}

## REMOVE JUNK IMAGES FROM PATH ##

# List all images
logo_files <- list.files(image_folder, full.names = TRUE, pattern = "\\.png$")

# Sort the logo files
logo_files_sorted <- logo_files[order(as.numeric(gsub("logo_(\\d+).png", "\\1", basename(logo_files))))]

# Sort the team names alphabetically
teams <- teams[order(teams$team_name), ]

# Checking to make sure the number of logos matches the number of teams
if (length(logo_files_sorted) != nrow(teams)) {
  stop("The number of logos does not match the number of teams.")
}

# Assign the sorted images to the teams in alphabetical order
teams$image_path <- logo_files_sorted

## Shooting Data ##
# Filter and select shooting events
shooting_events <- possession_events %>%
  filter(possessionEventType == "SH") %>%
  select(id, formattedGameClock, gameClock, grades, contains("shoot") | contains("shot") | contains("goal")) %>%
  select(where(~ !all(is.na(.)))) %>%
  unnest(grades) %>%
  filter(player.id == shootingEvent.shooterPlayer.id)  # Filter for the corresponding player ID

# Remove unnecessary columns
shooting_events <- shooting_events %>%
  select(-c(gradeStyle, gradeType, gradeLabel, insertedAt, updatedAt))

# Update shotOutcomeType
shooting_events <- shooting_events %>%
  mutate(shootingEvent.shotOutcomeType = case_when(
    shootingEvent.shotOutcomeType == "B" ~ "Block on target",
    shootingEvent.shotOutcomeType == "C" ~ "Block off target",
    shootingEvent.shotOutcomeType == "F" ~ "Save off target",
    shootingEvent.shotOutcomeType == "G" ~ "Goal",
    shootingEvent.shotOutcomeType == "L" ~ "Goalline clearance",
    shootingEvent.shotOutcomeType == "O" ~ "Off target",
    shootingEvent.shotOutcomeType == "S" ~ "Save on target",
    TRUE ~ shootingEvent.shotOutcomeType
  ))

# Update shotType
shooting_events <- shooting_events %>%
  mutate(shootingEvent.shotType = case_when(
    shootingEvent.shotType == "B" ~ "Bicycle",
    shootingEvent.shotType == "D" ~ "Diving",
    shootingEvent.shotType == "F" ~ "Side foot",
    shootingEvent.shotType == "I" ~ "Sliding",
    shootingEvent.shotType == "L" ~ "Lob",
    shootingEvent.shotType == "O" ~ "Outside foot",
    shootingEvent.shotType == "S" ~ "Standard",
    shootingEvent.shotType == "T" ~ "Studs",
    shootingEvent.shotType == "V" ~ "Volley",
    TRUE ~ shootingEvent.shotType
  ))

# Update shotNatureType
shooting_events <- shooting_events %>%
  mutate(shootingEvent.shotNatureType = case_when(
    shootingEvent.shotNatureType == "A" ~ "Placement",
    shootingEvent.shotNatureType == "F" ~ "Flick",
    shootingEvent.shotNatureType == "P" ~ "Power",
    shootingEvent.shotNatureType == "T" ~ "Toe punt",
    TRUE ~ shootingEvent.shotNatureType
  ))

# Update shotInitialHeightType
shooting_events <- shooting_events %>%
  mutate(shootingEvent.shotInitialHeightType = case_when(
    shootingEvent.shotInitialHeightType == "1" ~ "Bottom third",
    shootingEvent.shotInitialHeightType == "2" ~ "Middle third",
    shootingEvent.shotInitialHeightType == "3" ~ "Top third",
    shootingEvent.shotInitialHeightType == "C" ~ "Cross bar",
    shootingEvent.shotInitialHeightType == "F" ~ "Fails to reach",
    shootingEvent.shotInitialHeightType == "G" ~ "Ground",
    shootingEvent.shotInitialHeightType == "N" ~ "Narrowly over crossbar",
    shootingEvent.shotInitialHeightType == "O" ~ "Far over the crossbar",
    shootingEvent.shotInitialHeightType == "U" ~ "Narrowly under crossbar",
    TRUE ~ shootingEvent.shotInitialHeightType
  ))

# Update ballHeightType
shooting_events <- shooting_events %>%
  mutate(shootingEvent.ballHeightType = case_when(
    shootingEvent.ballHeightType == "A" ~ "Above head",
    shootingEvent.ballHeightType == "G" ~ "On the ground",
    shootingEvent.ballHeightType == "H" ~ "Between waist and head",
    shootingEvent.ballHeightType == "L" ~ "Off the ground but below waist",
    shootingEvent.ballHeightType == "M" ~ "Video missing",
    shootingEvent.ballHeightType == "V" ~ "Half volley",
    TRUE ~ shootingEvent.ballHeightType
  ))

# Update pressureType
shooting_events <- shooting_events %>%
  mutate(shootingEvent.pressureType = case_when(
    shootingEvent.pressureType == "A" ~ "Attempted pressure",
    shootingEvent.pressureType == "L" ~ "Pressures lane",
    shootingEvent.pressureType == "P" ~ "Pressures player",
    TRUE ~ shootingEvent.pressureType
  ))

# Update bodyMovementType
shooting_events <- shooting_events %>%
  mutate(shootingEvent.bodyMovementType = case_when(
    shootingEvent.bodyMovementType == "AG" ~ "Away from goal",
    shootingEvent.bodyMovementType == "LA" ~ "Lateral",
    shootingEvent.bodyMovementType == "ST" ~ "Static",
    shootingEvent.bodyMovementType == "TG" ~ "Towards goal",
    TRUE ~ shootingEvent.bodyMovementType
  ))

# Update advantageType
shooting_events <- shooting_events %>%
  mutate(shootingEvent.advantageType = case_when(
    shootingEvent.advantageType == "N" ~ "Disallowed",
    TRUE ~ shootingEvent.advantageType
  ))

# Get unique rosters to join with shooting_events
rosters_d <- rosters %>%
  distinct(player_id, player_nickname, shirtNumber, team_name, team_id)

shooting_events <- shooting_events %>%
  left_join(rosters_d %>% select(player_id, shirtNumber, team_name, team_id), by = c("shootingEvent.shooterPlayer.id" = "player_id")) %>%
  left_join(rosters_d %>% select(player_id, shirtNumber, team_name, team_id), by = c("shootingEvent.pressurePlayer.id" = "player_id"), suffix = c("_shooter", "_pressure")) %>%
  left_join(rosters_d %>% select(player_id, shirtNumber, team_name, team_id), by = c("shootingEvent.saverPlayer.id" = "player_id"), suffix = c("_pressure", "_saver")) %>%
  left_join(rosters_d %>% select(player_id, shirtNumber, team_name, team_id), by = c("shootingEvent.blockerPlayer.id" = "player_id"), suffix = c("_saver", "_blocker")) %>%
  rename(
    shooter_id = shootingEvent.shooterPlayer.id,
    shooter_number = shirtNumber_shooter,
    shooter_team = team_name_shooter,
    shooter_tid = team_id_shooter,
    saver_id = shootingEvent.saverPlayer.id,
    saver_number = shirtNumber_saver,
    saver_team = team_name_saver,
    saver_tid = team_id_saver,
    blocker_id = shootingEvent.blockerPlayer.id,
    blocker_number = shirtNumber_blocker,
    blocker_team = team_name_blocker,
    blocker_tid = team_id_blocker,
    pressure_id = shootingEvent.pressurePlayer.id,
    pressure_number = shirtNumber_pressure,
    pressure_team = team_name_pressure,
    pressure_tid = team_id_pressure
  )

# Select final columns
shooting_clean <- shooting_events %>%
  select(
    id,
    formattedGameClock,
    gameClock,
    shooter_id,
    shootingEvent.shooterPlayer.nickname,
    shooter_number,
    shooter_team,
    shooter_tid,
    playerGrade,
    shootingEvent.shotOutcomeType,
    shootingEvent.advantageType,
    shootingEvent.shotType,
    shootingEvent.shotNatureType,
    shootingEvent.ballMoving,
    shootingEvent.shotBodyType,
    shootingEvent.bodyMovementType,
    shootingEvent.shotInitialHeightType,
    shootingEvent.ballHeightType,
    shootingEvent.saveHeightType,
    shootingEvent.pressureType,
    pressure_id,
    shootingEvent.pressurePlayer.nickname,
    saver_id,
    shootingEvent.saverPlayer.nickname,
    blocker_id,
    shootingEvent.blockerPlayer.nickname,
    pressure_number,
    pressure_team,
    pressure_tid,
    saver_number,
    saver_team,
    saver_tid,
    blocker_number,
    blocker_team,
    blocker_tid
  )

# Unnest the possessionEvents and attach gameId
events_possession_unique <- events %>%
  unnest(possessionEvents, names_sep = "_") %>%
  select(gameId, possessionEvents_id) %>%
  distinct(possessionEvents_id, .keep_all = TRUE)

# Left join to attach gameId to shooting_clean and remove duplicates
shooting_clean <- shooting_clean %>%
  left_join(events_possession_unique, by = c("id" = "possessionEvents_id")) %>%
  distinct(id, .keep_all = TRUE)

# Clean the shooting_clean dataset
shooting_clean <- shooting_clean %>%
  rename(
    shooter_name = shootingEvent.shooterPlayer.nickname,
    pressure_name = shootingEvent.pressurePlayer.nickname,
    saver_name = shootingEvent.saverPlayer.nickname,
    blocker_name = shootingEvent.blockerPlayer.nickname,
    shot_outcome = shootingEvent.shotOutcomeType,
    shot_type = shootingEvent.shotType,
    shot_nature = shootingEvent.shotNatureType,
    shot_height = shootingEvent.shotInitialHeightType,
    ball_height = shootingEvent.ballHeightType,
    body_movement = shootingEvent.bodyMovementType,
    pressure_type = shootingEvent.pressureType,
    advantage_type = shootingEvent.advantageType
  )

# Create new columns for shot success, blocked, saved, and off target
shooting_clean <- shooting_clean %>%
  mutate(
    shot_success = ifelse(shot_outcome == "Goal", 1, 0),
    shot_blocked = ifelse(grepl("Block", shot_outcome), 1, 0),
    shot_saved = ifelse(grepl("Save", shot_outcome), 1, 0),
    shot_offtarget = ifelse(shot_outcome == "Off target", 1, 0)
  )

# Handle missing pressure_type
shooting_clean$pressure_type[is.na(shooting_clean$pressure_type)] <- "No Pressure"



## Graph ##

# Create under_pressure column
shooting_clean$under_pressure <- ifelse(is.na(shooting_clean$pressure_tid) | shooting_clean$pressure_tid == 0, 0, 1)

# Aggregate success rate and count for each team, under pressure and not under pressure
success_vs_pressure <- shooting_clean %>%
  group_by(shooter_tid, under_pressure, shooter_team) %>%
  summarize(
    shot_success_mean = mean(shot_success),
    shot_success_count = n(),
    .groups = "drop"
  )

# Separate success rates for shots under pressure and no pressure
success_pressure <- success_vs_pressure %>% filter(under_pressure == 1)
success_no_pressure <- success_vs_pressure %>% filter(under_pressure == 0)

# Merge the success data for both conditions
cross_plot_data <- merge(success_pressure, success_no_pressure, by = "shooter_tid", suffixes = c("_pressure", "_no_pressure"))

# Merge with teams data to get the images and calculate total shots and successful shots
cross_plot_data <- cross_plot_data %>%
  left_join(teams[, c("team_id", "image_path")], by = c("shooter_tid" = "team_id")) %>%
  left_join(shooting_clean %>%
              group_by(shooter_tid) %>%
              summarize(total_shots = n(), .groups = "drop"), by = "shooter_tid") %>%
  left_join(shooting_clean %>%
              filter(under_pressure == 1) %>%
              group_by(shooter_tid) %>%
              summarize(successful_shots_pressure = sum(shot_success), .groups = "drop"), by = "shooter_tid") %>%
  left_join(shooting_clean %>%
              filter(under_pressure == 0) %>%
              group_by(shooter_tid) %>%
              summarize(successful_shots_no_pressure = sum(shot_success), .groups = "drop"), by = "shooter_tid")

# Remove unnecessary columns
cross_plot_data <- cross_plot_data[, !(colnames(cross_plot_data) %in% c("under_pressure_pressure", "under_pressure_no_pressure", "shot_success_count_pressure", "shot_success_count_no_pressure"))]

# Calculate average success rates
avg_success_pressure <- mean(cross_plot_data$shot_success_mean_pressure, na.rm = TRUE)
avg_success_no_pressure <- mean(cross_plot_data$shot_success_mean_no_pressure, na.rm = TRUE)

# Define the new maximum axis value
max_value <- 0.35

# Plotting without jitter
ggplot(cross_plot_data, aes(x = shot_success_mean_pressure, y = shot_success_mean_no_pressure)) +
  geom_image(aes(image = image_path), size = 0.03) +  # Adjust size for the images to fit
  labs(
    title = "Pressure v No Pressure - Goal Success",
    subtitle = "How Teams Perform When Pressured vs Not Pressured While Shooting the Ball",
    x = "Pressure on Shooter",
    y = "No Pressure on Shooter"
  ) +
  theme(
    panel.background = element_rect(fill = "#49BCE3"),
    plot.background = element_rect(fill = "#56042C"),
    axis.text.x = element_text(color = "white", face = "bold", size = 8),
    axis.text.y = element_text(color = "white", face = "bold", size = 8),
    axis.title.x = element_text(color = "white", face = "bold", size = 10),
    axis.title.y = element_text(color = "white", face = "bold", size = 10),
    plot.title = element_text(color = "white", face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(color = "white", size = 12, hjust = 0.5),
    axis.ticks = element_line(color = "white", size = .5),
    axis.ticks.length = unit(0.2, "cm")
  ) +
  scale_x_continuous(
    expand = c(0.1, 0),  # Add padding behind 0 but not in front of the maximum
    breaks = seq(0, max_value, by = 0.05),  # Breaks at 0, 5%, 10%, 15%, ..., 35%
    labels = scales::percent,
    limits = c(0, max_value),  # Set the x-axis to strictly range from 0 to 35%
    minor_breaks = NULL  # Remove minor ticks
  ) +
  scale_y_continuous(
    expand = c(0.1, 0),  # Add padding behind 0 but not in front of the maximum
    breaks = seq(0, max_value, by = 0.05),  # Breaks at 0, 5%, 10%, 15%, ..., 35%
    labels = scales::percent,
    limits = c(0, max_value),  # Set the y-axis to strictly range from 0 to 35%
    minor_breaks = NULL  # Remove minor ticks
  ) +
  geom_hline(yintercept = avg_success_no_pressure, color = "black", linetype = "dashed", size = .5) +
  geom_vline(xintercept = avg_success_pressure, color = "black", linetype = "dashed", size = .5)


## Next Step Idea ##

# Calculate log-weighted averages
cross_plot_data$log_shots <- log1p(cross_plot_data$total_shots) # log(1 + x) avoids log(0)

cross_plot_data <- cross_plot_data %>%
  mutate(
    weighted_success_pressure = shot_success_mean_pressure * log_shots,
    weighted_success_no_pressure = shot_success_mean_no_pressure * log_shots
  )

# Aggregate by team
team_summary <- cross_plot_data %>%
  group_by(shooter_tid) %>%
  summarize(
    weighted_avg_pressure = sum(weighted_success_pressure, na.rm = TRUE) / sum(log_shots, na.rm = TRUE),
    weighted_avg_no_pressure = sum(weighted_success_no_pressure, na.rm = TRUE) / sum(log_shots, na.rm = TRUE),
    total_shots = sum(total_shots, na.rm = TRUE),
    .groups = "drop"
  )

# Merge with team names
team_summary <- team_summary %>%
  left_join(teams[, c("team_id", "team_name", "image_path")], by = c("shooter_tid" = "team_id"))

# View the result
print(team_summary)

