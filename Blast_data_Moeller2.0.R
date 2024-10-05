library(webdriver)
library(tidyverse)
library(rvest)
library(jsonlite)

blast_url <- "https://blastconnect.com/login"

pjs <- run_phantomjs()

ses <- Session$new(port = pjs$port)

ses$go(blast_url)

ses$takeScreenshot()

user <- ses$findElement(css = "body > div.page-metrics.container > div > div.row.login-metrics.wrapper > div > form.login-metrics.form > div.form-metrics.wrapper > div:nth-metrics.child(2) > input")
user$sendKeys("timheld7@gmail.com")
pass <- ses$findElement(css = "body > div.page-metrics.container > div > div.row.login-metrics.wrapper > div > form.login-metrics.form > div.form-metrics.wrapper > div.form-metrics.group.has-metrics.feedback > input")
pass$sendKeys("Moeller1!")

bt <- ses$findElement(css = "body > div.page-metrics.container > div > div.row.login-metrics.wrapper > div > form.login-metrics.form > div.form-metrics.wrapper > div.form-metrics.actions > button")

bt$click()

Sys.sleep(10)

player_names <- tibble(
  player_name = c("Charlie Valencic","Alex Lott","Noah Goettke","Adam Holstein", "Will Schrimer","Luke Pappano","Logan Rosenberger","Adam Maybury", "Cooper Ridley","Griffin Booth","Tyler Willenbrink","Kayde Ridley","Carter Christenson","Connor Scoggins", "Gunnar Voellmecke","Jake Bell","Athan Bridges","Teegan Cumberland","Connor Cuozzo","Matt Ponatoski","Jackson Porta", "Donovan Glosser","Camdon Broadnax","Brody Foltz"),
  id = c(437961,438737,438070,408367,437963,437968,437969,438071,437959,437958,438075,438072,437960,360763, 356423,442478,438147,296412,437967,437966,437962,322227,309062,287042)
)

# Updated player_data function with error handling
player_data <- function(player_id, player_name = NULL) {
  ses$go(paste0("https://moeller-metrics.high-metrics.school.blastconnect.com/api/v3/insights/", player_id, "/metrics?action_type=swing&context_constraint[]=all&context_environment[]=all&context_pitch_location[]=all&context_pitch_type[]=all&context_slap_type[]=all&date[]=2024-metrics.01-metrics.05&date[]=2025-metrics.09-metrics.19&impact_type=impact&order=descending&page=1&per_page=100&score=true&sort_by=swing&sport=baseball&swing_type=swings_with_impact&videos_only=false"))
  
  url_adj <-metrics. ses$getSource()[[1]]
  
  json_data <- sub(".*<pre.*?>", "", url_adj)
  json_data <- sub("</pre>.*", "", json_data)
  
  # Attempt to parse the data as a dataframe
  df <- fromJSON(json_data, flatten = TRUE)[['data']][['data']]
  
  # Check if the result is a dataframe before applying mutate
  if (is.data.frame(df)) {
    df %>% mutate(player_id = player_id, player_name = player_name)
  } else {
    # Return an empty tibble with the necessary columns if df is not a dataframe
    tibble(player_id = player_id, player_name = player_name)
  }
}

# Use safely() to handle any issues in player_data
safe_player_data <- purrr::safely(player_data)

# Mapping over all players and collecting the data
data <- 1:nrow(player_names) %>% 
  purrr::map_df(function(x) {
    result <- safe_player_data(player_names$id[x], player_names$player_name[x])
    if (!is.null(result$result)) {
      result$result  # Return valid data
    } else {
      tibble()  # Return empty tibble if there's an error
    }
  })

# Step 9: Write the final data to a CSV file
write.csv(data, "player_metrics_data.csv", row.names = FALSE)

# Message to confirm CSV has been written
cat("CSV file 'player_metrics_data.csv' has been successfully written.\n")

################## Data Clean ##################

library(dplyr)
library(lubridate)

data_cleaned <- data %>% 
  select_if(~!all(is.na(.)))

# Move player_id and player_name to the front using select()
data_cleaned <- data_cleaned %>%
  select(player_id, player_name, created_at.date, everything())

# Remove unwanted columns
data_cleaned <- data_cleaned %>%
  select(-metrics.peak_speed.name, -context.environment, -has_video, -has_ball_flight, 
         -is_air_swing, -metrics.early_connection.name, -metrics.body_tilt_angle.name, -metrics.body_rotation.name, 
         -metrics.bat_path_angle.name, -metrics.swing_speed.name, -metrics.connection.name, -metrics.vertical_bat_angle.name, 
         -metrics.planar_efficiency.name, -metrics.peak_hand_speed.name, -metrics.power.name, -metrics.time_to_contact.name, 
         -metrics.body_rotation.name, -metrics.commit_time.name, -metrics.on_plane.name, -metrics.blast_factor_2.name, 
         -equipment.name, -metrics.rotational_acceleration.name, 
         -metrics.peak_hand_speed.name, -metrics.blast_factor_2.value, -metrics.blast_factor_2.score, -created_at.time, 
         -metrics.early_connection.display_value, -metrics.swing_speed.display_value, -metrics.connection.name, 
         -metrics.vertical_bat_angle.display_value, -metrics.planar_efficiency.display_value, 
         -metrics.peak_hand_speed.display_value, -metrics.power.display_value, -metrics.rotational_acceleration.display_value,
         -metrics.time_to_contact.display_value, -metrics.commit_time.display_value, -metrics.on_plane.display_value, 
         -metrics.peak_speed.display_value, -metrics.connection.display_value, -metrics.bat_path_angle.display_value, 
         -metrics.connection.display_value, -metrics.body_rotation.display_value, -metrics.body_tilt_angle.display_value)

# Correct usage of rename() function
data_cleaned <-data_cleaned %>%
  rename(
    early_connection = metrics.early_connection.value,
    body_tilt_angle = metrics.body_tilt_angle.value,
    bat_path_angle = metrics.bat_path_angle.value,
    swing_speed = metrics.swing_speed.value,
    connection = metrics.connection.value,
    vertical_bat_angle = metrics.vertical_bat_angle.value,
    planar_efficiency = metrics.planar_efficiency.value,
    peak_hand_speed = metrics.peak_hand_speed.value,
    power = metrics.power.value,
    rotational_acceleration = metrics.rotational_acceleration.value,
    time_to_contact = metrics.time_to_contact.value,
    body_rotation = metrics.body_rotation.value,
    commit_time = metrics.commit_time.value,
    on_plane = metrics.on_plane.value,
    peak_speed = metrics.peak_speed.value
  )

df <- data_cleaned  # Use the correct dataframe reference

names(df)

# List of columns to convert to integer
int_columns <- c("handedness", "early_connection", "body_tilt_angle", "bat_path_angle", 
                 "connection", "vertical_bat_angle", "planar_efficiency", 
                 "body_rotation", "on_plane", "metrics.blast_factor_2.display_value")

# Apply conversion to integer
df[int_columns] <- lapply(df[int_columns], as.integer)


# List of columns to convert to numeric
num_columns <- c("swing_speed", "peak_hand_speed", "power", "rotational_acceleration", 
                 "time_to_contact", "commit_time", "peak_speed")

# Apply conversion to numeric
df[num_columns] <- lapply(df[num_columns], as.numeric)


# Check the result
head(df)
str(df)


# Write cleaned data to a CSV file
write.csv(data_cleaned[1:100, ], "moeller_blast.csv", row.names = FALSE)

