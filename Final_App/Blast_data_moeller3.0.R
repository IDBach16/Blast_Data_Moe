library(webdriver)
library(tidyverse)
library(rvest)
library(jsonlite)
library(DBI)
library(RSQLite)

# Set up the PhantomJS session
blast_url <- "https://blastconnect.com/login"

pjs <- run_phantomjs()

ses <- Session$new(port = pjs$port)

ses$go(blast_url)

# Take screenshot (optional, for debugging)
ses$takeScreenshot()

# Login to Blast Connect
user <- ses$findElement(css = "body > div.page-container > div > div.row.login-wrapper > div > form.login-form > div.form-wrapper > div:nth-child(2) > input")
user$sendKeys("timheld7@gmail.com")

pass <- ses$findElement(css = "body > div.page-container > div > div.row.login-wrapper > div > form.login-form > div.form-wrapper > div.form-group.has-feedback > input")
pass$sendKeys("Moeller1!")

bt <- ses$findElement(css = "body > div.page-container > div > div.row.login-wrapper > div > form.login-form > div.form-wrapper > div.form-actions > button")
bt$click()

# Wait for login to complete
Sys.sleep(10)

# Define player names and IDs
player_names <- tibble(player_name = c("Charlie Valencic","Alex Lott","Noah Goettke","Adam Holstein",
                                       "Will Schrimer","Luke Pappano","Logan Rosenberger","Adam Maybury",
                                       "Cooper Ridley","Griffin Booth","Tyler Willenbrink","Kayde Ridley",
                                       "Carter Christenson","Connor Scoggins","Gunnar Voellmecke","Jake Bell",
                                       "Athan Bridges","Teegan Cumberland","Connor Cuozzo","Matt Ponatoski",
                                       "Jackson Porta","Donovan Glosser","Camdon Broadnax","Brody Foltz"),
                       id = c(437961,438737,438070,408367,437963,437968,437969,438071,437959,437958,438075,438072,
                              437960,360763,356423,442478,438147,296412,437967,437966,437962,322227,309062,287042))

# Define function to fetch player data
player_data <- function(player_id, player_name=NULL){
  ses$go(paste0("https://moeller-high-school.blastconnect.com/api/v3/insights/",player_id,"/metrics?action_type=swing&context_constraint[]=all&context_environment[]=all&context_pitch_location[]=all&context_pitch_type[]=all&context_slap_type[]=all&date[]=2024-01-05&date[]=2025-09-19&impact_type=impact&order=descending&page=1&per_page=100&score=true&sort_by=swing&sport=baseball&swing_type=swings_with_impact&videos_only=false"))
  
  url_adj <- ses$getSource()[[1]]
  
  json_data <- sub(".*<pre.*?>", "", url_adj)
  json_data <- sub("</pre>.*", "", json_data)
  
  df <- fromJSON(json_data, flatten = T)[['data']][['data']]
  
  # Check if df is NULL or empty, return an empty tibble if so
  if (is.null(df) || length(df) == 0) {
    return(tibble())
  }
  
  # Ensure df is a data frame
  df <- as_tibble(df)
  
  # Add player_id and player_name columns
  df %>% mutate(player_id = player_id, player_name = player_name)
}

# Fetch data for all players using map_df
data <- 1:nrow(player_names) %>%
  purrr::map_df(function(x) player_data(player_names$id[x], player_names$player_name[x]))

# Rename columns for easier access
data <- data %>%
  rename_with(~ gsub("^metrics.", "", .), starts_with("metrics"))

# Write the final data to a CSV file
#write.csv(data, "player_metrics_data.csv", row.names = FALSE)
#cat("CSV file 'player_metrics_data.csv' has been successfully written.\n")

################## Data Cleaning ##################

library(dplyr)
library(lubridate)

# Clean the data: Remove columns with all NA values
data_cleaned <- data %>% 
  select_if(~!all(is.na(.)))

# Move player_id and player_name to the front
data_cleaned <- data_cleaned %>%
  select(player_id, player_name, created_at.date, everything())

# Remove unnecessary columns
data_cleaned <- data_cleaned %>%
  select(-sport_id, -peak_speed.name, -context.environment, -has_video, -has_ball_flight, -is_air_swing,
         -early_connection.name, -body_tilt_angle.name, -body_rotation.name, -bat_path_angle.name, -swing_speed.name,
         -connection.name, -vertical_bat_angle.name, -planar_efficiency.name, -peak_hand_speed.name, -power.name,
         -time_to_contact.name, -commit_time.name, -on_plane.name, -blast_factor_2.name, -equipment.name,
         -blast_factor_2.value, -blast_factor_2.score, -created_at.time, -early_connection.display_value,
         -swing_speed.display_value, -connection.name, -vertical_bat_angle.display_value, -planar_efficiency.display_value,
         -peak_hand_speed.display_value, -power.display_value, -rotational_acceleration.display_value, -time_to_contact.display_value,
         -commit_time.display_value, -on_plane.display_value, -peak_speed.display_value, -connection.display_value, 
         -bat_path_angle.display_value, -body_rotation.display_value, -body_tilt_angle.display_value, -rotational_acceleration.name)

# Correct usage of rename() function
data_cleaned <- data_cleaned %>%
  rename(
    early_connection = early_connection.value,
    body_tilt_angle = body_tilt_angle.value,
    bat_path_angle = bat_path_angle.value,
    swing_speed = swing_speed.value,
    connection = connection.value,
    vertical_bat_angle = vertical_bat_angle.value,
    on_plane_efficiency = planar_efficiency.value,
    peak_hand_speed = peak_hand_speed.value,
    power = power.value,
    rotational_acceleration = rotational_acceleration.value,
    time_to_contact = time_to_contact.value,
    body_rotation = body_rotation.value,
    commit_time = commit_time.value,
    on_plane = on_plane.value,
    on_plane_efficiency_score = planar_efficiency.score,
    peak_speed = peak_speed.value 
  )

# Convert columns to appropriate data types
df <- data_cleaned  # Use the correct dataframe reference

# Convert player_id to character
df$player_id <- as.character(df$player_id)

# Specify integer columns
int_columns <- c("handedness", "early_connection", "body_tilt_angle", "bat_path_angle", 
                 "connection", "vertical_bat_angle", "power.score", 'on_plane_efficiency', 
                 "body_rotation", "on_plane", "blast_factor_2.display_value")
df[int_columns] <- lapply(df[int_columns], as.integer)

# Specify numeric columns
num_columns <- c("swing_speed", "peak_hand_speed", "power", "rotational_acceleration", 
                 "time_to_contact", "commit_time", "peak_speed")
df[num_columns] <- lapply(df[num_columns], as.numeric)

db = dbConnect(SQLite(),"Moeller_Blast.sqlite")
dbWriteTable(db,"Moeller_Blast",df,overwrite=T)
dbDisconnect(db)
#file.copy("C:/Users/ibach/OneDrive - Terillium/Pictures/Moller Misc/Moeller_Blast.sqlite")

# # Check the result
# head(df)
# str(df)

# setwd("C:/Users/ibach/OneDrive - Terillium/Pictures/Moller Misc")
# # Write cleaned data to a CSV file
# #write.csv(data_cleaned[1:100, ], "moeller_blast", row.names = FALSE)
# write.csv(df, "moeller_blast_full.csv", row.names = FALSE)
# #########################################################################
# library(dplyr)
# # Group by player_name and calculate the mean for all numeric columns
# means_by_player <- df %>%
#   group_by(player_name) %>%
#   summarise(across(where(is.numeric), mean, na.rm = TRUE))
# 
# # View the result
# print(means_by_player)
# 
# ##########
# 
# # Load necessary libraries
# library(dplyr)
# 
# # Specify the columns to exclude
# exclude_columns <- c("player_id", "player_name", "created_at.date", "id", "blast_id", "action_type", "handedness")
# 
# # Select only numeric columns and exclude specified columns
# numeric_columns <- df %>%
#   select(-one_of(exclude_columns)) %>%
#   select_if(is.numeric)
# 
# # Calculate the mean for each numeric column
# mean_values <- colMeans(numeric_columns, na.rm = TRUE)
# 
# # Print the mean values
# print(mean_values)
# 
# ##########k means clustering of hitter########
# 
# # Step 2: Group by player_name and calculate the mean of relevant features
# player_means <- df %>%
#   group_by(player_name) %>%
#   summarise(
#     mean_peak_hand_speed = mean(peak_hand_speed, na.rm = TRUE),
#     mean_swing_speed = mean(swing_speed, na.rm = TRUE),
#     mean_connection = mean(connection, na.rm = TRUE),
#     mean_rotational_acceleration = mean(rotational_acceleration, na.rm = TRUE),
#     mean_body_rotation = mean(body_rotation, na.rm = TRUE),
#     mean_vertical_bat_angle = mean(vertical_bat_angle, na.rm = TRUE),
#     mean_time_to_contact = mean(time_to_contact, na.rm = TRUE)
#   )

