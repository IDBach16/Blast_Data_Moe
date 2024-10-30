# Load necessary libraries
library(shiny)           # Framework for building web applications
library(ggplot2)         # Data visualization
library(dplyr)           # Data manipulation
library(tidyr)           # Data tidying
library(DT)              # Rendering data tables
library(shinythemes)     # Themes for Shiny applications
library(webdriver)       # Control web browsers from R
library(tidyverse)       # Collection of data science tools (includes ggplot2, dplyr, etc.)
library(rvest)           # Web scraping
library(jsonlite)        # JSON data handling
library(DBI)             # Database interface
library(RSQLite)         # SQLite interface
library(cluster)         # Clustering algorithms
library(plotly)          # Interactive plots

# Example database connection and data retrieval (uncomment and modify if needed)
db = dbConnect(SQLite(), "Moeller_Blast.sqlite")
df = dbGetQuery(db, "SELECT * from Moeller_Blast")

# Ensure the date column is in the correct format (if necessary)
# df$created_at.date <- as.Date(df$created_at.date)

# Define the User Interface (UI)
ui <- fluidPage(
  theme = shinytheme("cyborg"),  # Apply the "cyborg" theme for styling
  # Custom CSS for background image and other styling
  tags$style(HTML("
    body {
      background-image: url('https://github.com/IDBach16/Blast_Data_Moe/raw/main/maxresdefault.jpg');
      background-attachment: fixed;
      background-size: cover;
      background-repeat: no-repeat;
      background-position: center;
    }
    .tab-content .active h3, .tab-content .active p {
      color: #E0E0E0;  # Light text for better readability
    }
    .tab-content .active {
      padding: 15px;
      border-radius: 5px;
    }
    .custom-border {
      border: 2px solid #E0E0E0;
      padding: 15px;
      border-radius: 5px;
      background-color: rgba(0, 0, 0, 0.9);  # Dark background
      color: #FFFFFF;  # Light text color
    }
  ")),
  
  titlePanel("Moeller Blast Dashboard"),
  
  # Define tab panels for various analyses
  tabsetPanel(
    # First Tab: Filter Analysis
    # Filter Analysis Tab
    tabPanel("Introduction and Tabs",
             div(class = "border-box",
                 h3("Overview of Dashboard"),
                 p("**Developed exclusively by Ian D. Bach**"),
                 p("The Moeller Blast Dashboard leverages Blast Motion Data to provide comprehensive insights into hitter performance."),
                 p("This advanced tool is organized into key sections, each crafted to highlight specific metrics and performance aspects with clarity and intuition."),
                 p("Users can delve into detailed swing analytics, track swing efficiency and power, and compare performance across different game situations and pitch types."),
                 p("The dashboard includes dynamic visualizations of metrics such as bat speed, on-plane efficiency, and rotational acceleration, offering a detailed perspective on swing mechanics."),
                 p("Additionally, it highlights player consistency, progression over time, and clustering insights, enabling coaches and players to develop targeted strategies for performance enhancement."),
                 
                 h3("Headers/Tabs:"),
                 
                 div(class = "border-box",
                     h3(tags$u("Percentile Plot and Filter Tab")),
                     p("The 'Percentile Plot and Filter' tab allows users to visualize individual player performance percentiles across various metric groups, offering insights into player rankings within specific skill areas."),
                     p("Users can select a player and a metric group, such as 'Plane/Contact,' 'Connection,' or 'Rotational Power,' to focus on key aspects of performance."),
                     p("After pressing the 'Generate Plot' button, a lollipop-style plot displays the chosen player's rank within their selected group."),
                     p("Additionally, this tab features two tables: one showing overall mean values across all players and another with player-specific averages, providing a comprehensive view of individual and group performance statistics.")
                 ),
                 
                 div(class = "border-box",
                     h3(tags$u("Correlation Analysis Tab")),
                     p("The 'Correlation Analysis' tab is designed to identify and examine strong correlations between different performance metrics."),
                     p("Users can set a correlation threshold using a numeric input, defining the minimum correlation strength to be displayed."),
                     p("By pressing 'Analyze,' the tab generates a table listing metric pairs with correlations above the specified threshold, offering a quick overview of significant relationships within the dataset."),
                     p("For a more visual approach, this tab also includes an interactive correlation heatmap, enabling users to explore these relationships dynamically.")
                 ),
                 
                 div(class = "border-box",
                     h3(tags$u("Performance Analysis Tab")),
                     p("In the 'Performance Analysis' tab, users can assess and categorize players' percentile ranks across selected key metrics, allowing for focused analysis on specific performance attributes."),
                     p("Users select the metrics they wish to analyze from a list of checkboxes, such as 'swing speed' or 'rotational acceleration.'"),
                     p("After updating the analysis, the tab generates a performance table that groups players into percentile categories, such as 'Top 25%' or 'Bottom 25%,' for each selected metric."),
                     p("This categorical breakdown provides an intuitive overview of individual strengths and areas for improvement relative to their peers.")
                 ),
                 
                 div(class = "border-box",
                     h3(tags$u("Performance Score and SQL Statement Tab")),
                     p("The 'Performance Score and SQL Statement' tab combines a custom SQL query interface with a visual performance ranking for players."),
                     p("Users can enter and execute SQL queries to retrieve data directly from the database, providing flexibility to explore specific data subsets or customize their analyses."),
                     p("The results are displayed in a data table for easy viewing."),
                     p("Additionally, this tab features a bar plot that visualizes each player’s composite performance score, scaled from 1 to 100, offering a high-level comparison of player performance across multiple metrics.")
                 ),
                 
                 div(class = "border-box",
                     h3(tags$u("Player Consistency Overview Tab")),
                     p("The 'Player Consistency Overview' tab allows users to analyze player performance stability across a specified date range."),
                     p("Users set a date range to filter data within that timeframe and input a threshold to flag players with high variability."),
                     p("The tab includes a summary table showing each player’s consistency for metrics like swing speed, rotational acceleration, and body rotation, represented by standard deviation values."),
                     p("For further detail, consistency plots display these standard deviations graphically, helping identify players with performance fluctuations."),
                     p("An additional table lists players whose standard deviation exceeds the threshold, highlighting those needing further review for consistency improvements.")
                 ),
                 
                 div(class = "border-box",
                     h3(tags$u("Player Groupings Analysis Tab")),
                     p("The 'Player Groupings Analysis' tab leverages clustering to group players based on averaged metrics, aiding in identifying common performance characteristics across the player pool."),
                     p("A Plotly scatter plot visualizes the clusters, using PCA (Principal Component Analysis) to enhance interpretability."),
                     p("An accompanying data table provides detailed information about each player’s cluster membership."),
                     p("Additionally, a textual summary describes each group and lists the players within each, offering insights into the unique characteristics and training needs of different player segments.")
                 ),
                 
                 div(class = "border-box",
                     h3(tags$u("Linear Model - Metrics Tab")),
                     p("In the 'Linear Model - Metrics' tab, users can explore relationships between specific metrics through linear modeling, providing insight into potential cause-and-effect dynamics in performance."),
                     p("Users select an independent (X-axis) and dependent (Y-axis) variable, which generates a scatter plot with a fitted regression line."),
                     p("The tab includes a model summary, detailing statistical indicators such as the R-squared value and model coefficients."),
                     p("Accompanying descriptions offer context for clusters within the data, helping users understand and interpret clusters like 'Inconsistent Contact Path' or 'Low Power Output,' which highlight players' common performance patterns.")
                 ),
                 
                 div(class = "border-box",
                     h3(tags$u("Player Progression Tracking Tab")),
                     p("The 'Player Progression Tracking' tab offers a timeline-based view of a player’s progression in key metrics, ideal for monitoring improvements or regressions over time."),
                     p("Users select a player from a dropdown list and specify the metrics they wish to visualize, such as peak hand speed or swing speed."),
                     p("The tab generates a time series plot displaying the selected metrics over time, offering a visual progression narrative."),
                     p("An optional summary table provides basic statistics for each metric, such as average, minimum, and maximum values, giving a more quantitative view of the player’s development trajectory.")
                 ))),
    
    
    tabPanel(
      "Percentile Plot and Filter",
      sidebarLayout(
        sidebarPanel(
          selectInput("ath", "Select Player:", choices = unique(df$player_name)),
          selectInput("group", "Select Group:", choices = c("Plane/Contact", "Connection", "Rotational Power")),
          actionButton("plotButton", "Generate Plot")
        ),
        mainPanel(
          plotOutput("percentilePlot"),
          h3("Overall Means"),
          DTOutput("meansTable"),
          h3("Means by Player"),
          DTOutput("meansByPlayerTable")
        )
      )
    ),
    
    # Second Tab: Correlation Analysis
    tabPanel("Correlation Analysis",
             sidebarLayout(
               sidebarPanel(
                 # Input for setting correlation threshold
                 numericInput("threshold", "Correlation Threshold (0 to 1):", 0.60, min = 0, max = 1, step = 0.01),
                 actionButton("analyze", "Analyze")
               ),
               mainPanel(
                 h3("High Positive Correlation Pairs"),
                 dataTableOutput("correlation_table"),
                 h3("Interactive Correlation Heatmap"),
                 plotlyOutput("correlation_heatmap")
               )
             )
    ),
    
    # Third Tab: Performance Analysis
    tabPanel("Performance Analysis",
             sidebarLayout(
               sidebarPanel(
                 h4("Select Key Metrics"),
                 # Checkbox input for selecting which metrics to analyze
                 checkboxGroupInput("metrics", "Choose metrics to analyze:", 
                                    choices = c("swing_speed", "rotational_acceleration", "body_rotation", "connection.score", 
                                                "blast_factor_2.display_value", "bat_path_angle.score", "on_plane_efficiency_score"),
                                    selected = c("peak_speed", "rotational_acceleration")),
                 actionButton("update", "Update Analysis")
               ),
               mainPanel(
                 h3("Player Performance Table"),
                 tableOutput("performanceTable")
               )
             )
    ),
    
    # Fourth Tab: Performance Plot with SQL Query
    tabPanel("Performance Score and SQL Statement",
             sidebarLayout(
               sidebarPanel(
                 h4("Write Your SQL Query (DB = Moeller_Blast)"),
                 # Text area for user to enter SQL queries
                 textAreaInput("sql_query", "Enter SQL Query:", 
                               value = "SELECT * FROM Moeller_Blast", 
                               rows = 5, placeholder = "Write your SQL query here..."),
                 actionButton("run_query", "Run Query")
               ),
               mainPanel(
                 h3("SQL Query Results"),
                 DTOutput("sqlTable"),
                 plotOutput("performancePlot")
               )
             )
    ),
    
    # Fifth Tab: Player Consistency Overview
    tabPanel("Player Consistency Overview",
             sidebarLayout(
               sidebarPanel(
                 # Date range filter for data analysis
                 dateRangeInput(
                   inputId = "unique_date_filter",
                   label = "Select Date Range:",
                   start = Sys.Date() - 30,  # Start date is set to 30 days ago
                   end = Sys.Date()          # End date is today
                 ),
                 # Numeric input for setting inconsistency threshold
                 numericInput("unique_threshold", "Set Inconsistency Threshold:", value = 10, min = 1, max = 100, step = 1)
               ),
               mainPanel(
                 # Display summary tables and plots related to consistency
                 h3("Summary Table"),
                 tableOutput("unique_consistency_table"),
                 br(),
                 h3("Swing Speed Consistency (Standard Deviation)"),
                 plotOutput("unique_swing_speed_plot"),
                 br(),
                 h3("Rotational Acceleration Consistency (Standard Deviation)"),
                 plotOutput("unique_rotational_acc_plot"),
                 br(),
                 h3("Body Rotation Consistency (Standard Deviation)"),
                 plotOutput("unique_body_rotation_plot"),
                 br(),
                 h3("Inconsistent Players"),
                 tableOutput("unique_inconsistent_players_table")
               )
             )
    ),
    
    # Sixth Tab: Clustering Analysis
    tabPanel("Player Groupings Analysis",
             sidebarLayout(
               sidebarPanel(
                 h3("Player Group Summary"),
                 uiOutput("groupSummary")
               ),
               mainPanel(
                 # Display clustering results
                 h3("K-Means Clustering Visualization"),
                 plotlyOutput("clusterPlot"),
                 br(),
                 h3("Cluster Data Table"),
                 DT::dataTableOutput("clusterTable")
               )
             )
    ),
    
    # Analysis Dashboard: For linear model analysis and clustering descriptions
    tabPanel("Linear Model - Metrics",
             sidebarLayout(
               sidebarPanel(
                 hr(),
                 p("This app allows you to build a linear model based on selected swing metrics and visualize clusters."),
                 # Dropdowns to select variables for analysis
                 selectInput("xvar", "Select X-axis Variable (Independent):", 
                             choices = c("Rotation" = "body_rotation",
                                         "Bat Speed" = "swing_speed",
                                         "On Plane Efficiency" = "on_plane_efficiency",
                                         "Attack Angle" = "bat_path_angle",
                                         "Connection" = "connection",
                                         "Early Connection" = "early_connection",
                                         "Connection at Impact" = "body_tilt_angle",
                                         "Rotational Acceleration" = "rotational_acceleration",
                                         "Commit Time" = "commit_time",
                                         "Peak Speed" = "peak_speed",
                                         "Time to Contact" = "time_to_contact",
                                         "Vertical Bat Angle" = "vertical_bat_angle",
                                         "Peak Hand Speed" = "peak_hand_speed",
                                         "On Plane" = "on_plane",
                                         "Power" = "power")),
                 selectInput("yvar", "Select Y-axis Variable (Dependent):", 
                             choices = c("Rotation" = "body_rotation",
                                         "Bat Speed" = "swing_speed",
                                         "On Plane Efficiency" = "on_plane_efficiency",
                                         "Attack Angle" = "bat_path_angle",
                                         "Connection" = "connection",
                                         "Early Connection" = "early_connection",
                                         "Connection at Impact" = "body_tilt_angle",
                                         "Rotational Acceleration" = "rotational_acceleration",
                                         "Commit Time" = "commit_time",
                                         "Peak Speed" = "peak_speed",
                                         "Time to Contact" = "time_to_contact",
                                         "Vertical Bat Angle" = "vertical_bat_angle",
                                         "Peak Hand Speed" = "peak_hand_speed",
                                         "On Plane" = "on_plane",
                                         "Power" = "power"))
               ),
               mainPanel(
                 # Interactive plots and descriptive analysis
                 plotlyOutput("linearPlot"),
                 h4("Understanding the Cluster Types"),
                 div(class = "border-box",
                     p(strong("Inconsistent Contact Path:"), 
                       " This cluster represents players who may struggle with maintaining a consistent swing path, leading to varying contact points with the ball. 
                     This could be due to inconsistencies in metrics like on-plane efficiency, connection, or early connection. 
                     Players in this group may benefit from drills that emphasize maintaining a steady, repeatable swing path to improve their consistency.")
                 ),
                 div(class = "border-box",
                     p(strong("Low Power Output:"), 
                       " Players in this cluster exhibit lower than expected power metrics, such as swing speed, power, and rotational acceleration. 
                     This suggests they may need to work on generating more force and improving their core strength and technique to maximize power transfer during their swing.")
                 ),
                 div(class = "border-box",
                     p(strong("Inefficient Swing Dynamics:"), 
                       " This group reflects players whose swings are not as efficient, possibly due to issues in timing, body rotation, or overall mechanics. 
                     Indicators might include higher commit time or irregularities in time to contact. 
                     Improving swing efficiency can help players in this cluster achieve smoother, more effective swings.")
                 ),
                 h4("Scatter Plot with Regression Line"),
                 hr(),
                 h4("Model Summary"),
                 verbatimTextOutput("modelSummary"),
                 hr()
               )
             )
    ),
    
    # Player Progression Tracking Tab
    tabPanel("Player Progression Tracking",
             sidebarLayout(
               sidebarPanel(
                 selectInput("player_name", "Select Player:", 
                             choices = unique(df$player_name),
                             selected = unique(df$player_name)[1]),
                 
                 checkboxGroupInput("metrics", "Select Metrics to Display:",
                                    choices = c("Peak Hand Speed (mph)" = "peak_hand_speed",
                                                "Swing Speed (mph)" = "swing_speed",
                                                "On Plane Efficiency (%)" = "on_plane_efficiency",
                                                "Rotational Acceleration (g)" = "rotational_acceleration",
                                                "Body Rotation (degrees)" = "body_rotation"),
                                    selected = c("peak_hand_speed", "swing_speed", "on_plane_efficiency"))
               ),
               
               mainPanel(
                 plotlyOutput("progression_plot"),
                 br(),
                 #DTOutput("summary_table")
               )
             )
    ),
    
    
    # Include the CSS within the UI for custom styles
    tags$head(
      tags$style(HTML("
.border-box {
  border: 1px solid #444; /* Darker border */
  padding: 10px;
  margin-bottom: 10px;
  border-radius: 5px;
  background-color: #000; /* Black background */
  color: #fff; /* White text */
}
    "))
    )
  )
)

# Define the Server Logic
server <- function(input, output, session) {
  
  # Define user-friendly names for metrics (move inside server function)
  metric_names <- list(
    peak_hand_speed = "Peak Hand Speed (mph)",
    swing_speed = "Swing Speed (mph)",
    on_plane_efficiency = "On Plane Efficiency (%)",
    rotational_acceleration = "Rotational Acceleration (g)",
    body_rotation = "Body Rotation (degrees)"
  )
  
  # Calculate overall means across all players for each numeric column
  means <- reactive({
    df %>%
      select(where(is.numeric)) %>%
      summarise_all(mean, na.rm = TRUE) %>%
      pivot_longer(cols = everything(), names_to = "variable", values_to = "mean")
  })
  
  # Calculate means by player
  means_by_player <- reactive({
    df %>%
      group_by(player_name) %>%
      summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
      pivot_longer(cols = -player_name, names_to = "metric", values_to = "mean_value")
  })
  
  # Render overall means table
  output$meansTable <- renderDT({
    datatable(means()) %>%
      formatRound(columns = 'mean', digits = 2)
  })
  
  # Filtered data for selected player
  filtered_data_by_player <- reactive({
    means_by_player() %>%
      filter(player_name == input$ath)
  })
  
  # Render Means by Player Table
  output$meansByPlayerTable <- renderDT({
    datatable(filtered_data_by_player()) %>%
      formatRound(columns = 'mean_value', digits = 2)
  })
  
  # Define the percentile lollipop function
  percentile_lollipop <- function(df, stat) {
    df <- df %>% 
      select(y = paste0(tolower(stat), "_rank")) %>%
      mutate(x = stat)
    
    p <- ggplot(df, aes(label = y)) +
      geom_segment(aes(x=x, xend=x, y=0, yend=100), color="gray", size = 3) +
      geom_point(aes(x=stat, y=0), size = 7, fill = "gray", alpha = 1, shape = 21) +
      geom_point(aes(x=stat, y=50), size = 7, fill = "gray", alpha = 1, shape = 21) +
      geom_point(aes(x=stat, y=100), size = 7, fill = "gray", alpha = 1, shape = 21) +
      geom_point(aes(x=x, y=y), fill="red", size=14, alpha = 1, shape = 21) +
      geom_text(aes(x=stat, y=y), size = 5) +
      theme_light() +
      coord_flip() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      ggtitle(paste0(stat, ' Percentile'))
    
    return(p)
  }
  
  # Define the function to create percentile plots for each group
  create_percentile <- function(ath, type = "Plane/Contact") {
    
    # Calculate ranks for each metric
    df <- df %>%
      dplyr::mutate(
        early_connection_rank = 100 * round(percent_rank(early_connection), 3),
        connection_rank = 100 * round(percent_rank(connection), 3),
        on_plane_efficiency_rank = 100 * round(percent_rank(on_plane_efficiency), 3),
        vertical_bat_angle_rank = 100 * round(percent_rank(vertical_bat_angle), 3),
        time_to_contact_rank = 100 * round(percent_rank(time_to_contact), 3),
        body_tilt_angle_rank = 100 * round(percent_rank(body_tilt_angle), 3),
        swing_speed_rank = 100 * round(percent_rank(swing_speed), 3),
        connection.score_rank = 100 * round(percent_rank(connection.score), 3),
        power.score_rank = 100 * round(percent_rank(power.score), 3),
        body_rotation_rank = 100 * round(percent_rank(body_rotation), 3),
        on_plane_rank = 100 * round(percent_rank(on_plane), 3),
        peak_speed_rank = 100 * round(percent_rank(peak_speed), 3),
        bat_path_angle.score_rank = 100 * round(percent_rank(bat_path_angle.score), 3),
        peak_hand_speed_rank = 100 * round(percent_rank(peak_hand_speed), 3),
        power_rank = 100 * round(percent_rank(power), 3),
        rotational_acceleration_rank = 100 * round(percent_rank(rotational_acceleration), 3),
        commit_time_rank = 100 * round(percent_rank(commit_time), 3),
        peak_speed.score_rank = 100 * round(percent_rank(peak_speed.score), 3),
        blast_factor_2.display_value_rank = 100 * round(percent_rank(blast_factor_2.display_value), 3),
        on_plane_efficiency_score_rank = 100 * round(percent_rank(on_plane_efficiency_score), 3)
      )
    
    # Generate plots based on selected group
    if (type == "Plane/Contact") {
      plots <- list(
        percentile_lollipop(df %>% filter(player_name == ath) %>%
                              group_by(player_name) %>%
                              summarise(body_tilt_angle_rank = round(mean(body_tilt_angle_rank, na.rm = T))), "body_tilt_angle"),
        percentile_lollipop(df %>% filter(player_name == ath) %>%
                              group_by(player_name) %>%
                              summarise(bat_path_angle.score_rank = round(mean(bat_path_angle.score_rank, na.rm = T))), "bat_path_angle.score"),
        percentile_lollipop(df %>% filter(player_name == ath) %>%
                              group_by(player_name) %>%
                              summarise(on_plane_efficiency_score_rank = round(mean(on_plane_efficiency_score_rank, na.rm = T))), "on_plane_efficiency_score")
      )
      
    } else if (type == "Connection") {
      plots <- list(
        percentile_lollipop(df %>% filter(player_name == ath) %>%
                              group_by(player_name) %>%
                              summarise(connection_rank = round(mean(connection_rank, na.rm = T))), "connection"),
        percentile_lollipop(df %>% filter(player_name == ath) %>%
                              group_by(player_name) %>%
                              summarise(connection.score_rank = round(mean(connection.score_rank, na.rm = T))), "connection.score")
      )
      
    } else if (type == "Rotational Power") {
      plots <- list(
        percentile_lollipop(df %>% filter(player_name == ath) %>%
                              group_by(player_name) %>%
                              summarise(swing_speed_rank = round(mean(swing_speed_rank, na.rm = T))), "swing_speed"),
        percentile_lollipop(df %>% filter(player_name == ath) %>%
                              group_by(player_name) %>%
                              summarise(power.score_rank = round(mean(power.score_rank, na.rm = T))), "power.score"),
        percentile_lollipop(df %>% filter(player_name == ath) %>%
                              group_by(player_name) %>%
                              summarise(time_to_contact_rank = round(mean(time_to_contact_rank, na.rm = T))), "time_to_contact"),
        percentile_lollipop(df %>% filter(player_name == ath) %>%
                              group_by(player_name) %>%
                              summarise(body_rotation_rank = round(mean(body_rotation_rank, na.rm = T))), "body_rotation"),
        percentile_lollipop(df %>% filter(player_name == ath) %>%
                              group_by(player_name) %>%
                              summarise(peak_hand_speed_rank = round(mean(peak_hand_speed_rank, na.rm = T))), "peak_hand_speed"),
        percentile_lollipop(df %>% filter(player_name == ath) %>%
                              group_by(player_name) %>%
                              summarise(blast_factor_2.display_value_rank = round(mean(blast_factor_2.display_value_rank, na.rm = T))), "blast_factor_2.display_value"),
        percentile_lollipop(df %>% filter(player_name == ath) %>%
                              group_by(player_name) %>%
                              summarise(peak_speed_rank = round(mean(peak_speed_rank, na.rm = T))), "peak_speed")
      )
    } else {
      stop("Invalid group type. Please specify 'Plane/Contact', 'Connection', or 'Rotational Power'.")
    }
    
    return(plots)
  }
  
  # Render the percentile plot based on input
  output$percentilePlot <- renderPlot({
    req(input$plotButton)  # Ensure plot only updates when button is pressed
    
    # Generate percentile plots based on the selected player and group
    percentile_plots <- create_percentile(input$ath, input$group)
    
    # Combine the plots into a single plot grid layout
    cowplot::plot_grid(plotlist = percentile_plots, ncol = 1)
  })
  
  # ---------------------------------------------------
  # End of Server Logic for "Filter Analysis" Tab
  # ---------------------------------------------------
  
  ############Start of correlation
  
  observeEvent(input$analyze, {
    # Assuming the dataset is loaded as `df`
    req(df)  # Ensure df is available
    
    # Step 1: Select only numeric columns and exclude those containing "score" or "peak_speed"
    numeric_data <- df %>%
      select(where(is.numeric)) %>%
      select(-contains("score"), -contains("peak_speed"))
    
    # Step 2: Create the correlation matrix
    correlation_matrix <- cor(numeric_data, use = "complete.obs")
    
    # Step 3: Extract pairs with high positive correlation based on user input threshold
    correlations_data <- as.data.frame(as.table(correlation_matrix)) %>%
      filter(Var1 != Var2) %>%  # Exclude self-correlations
      arrange(desc(Freq)) %>%  # Sort by correlation values
      filter(Freq > input$threshold)  # Keep only positive correlations above the threshold
    
    # Remove duplicate pairs (e.g., A-B and B-A)
    correlations_data <- correlations_data[!duplicated(t(apply(correlations_data[,1:2], 1, sort))), ]
    
    # Round the correlation values to 3 decimals
    correlations_data$Freq <- round(correlations_data$Freq, 3)
    
    # Step 4: Render the results in a DataTable with more descriptive column names
    output$correlation_table <- renderDataTable({
      correlations_data %>%
        rename(`First Variable` = Var1, `Second Variable` = Var2, `Correlation Coefficient` = Freq)
    })
    
    # Step 5: Create an interactive correlation heatmap using Plotly
    output$correlation_heatmap <- renderPlotly({
      # Melt the filtered correlation matrix for plotly
      melted_corr_matrix <- reshape2::melt(correlation_matrix)  # Ensure you use reshape2::melt to avoid conflicts
      
      # Filter out only significant correlations (above the threshold)
      high_corrs <- melted_corr_matrix %>% 
        filter(abs(value) > input$threshold & Var1 != Var2)
      
      # Generate the heatmap
      plot_ly(
        x = colnames(correlation_matrix),
        y = colnames(correlation_matrix),
        z = correlation_matrix,
        type = "heatmap",
        colors = colorRamp(c("blue", "white", "red")),
        colorbar = list(title = "Correlation")
      ) %>%
        layout(
          title = "Interactive Correlation Heatmap (Excluding 'Score' and 'Peak Speed' Columns)",
          xaxis = list(title = "", tickangle = 45),
          yaxis = list(title = "")
        )
    })
  })
  
  
  ################end of correlation
  
  ####### Start of Tab 2: Performance Analysis #######
  
  # Performance calculation
  calculate_performance <- reactive({
    means_by_player <- df %>%
      group_by(player_name) %>%
      summarise(across(where(is.numeric), mean, na.rm = TRUE))
    
    classify_percentile <- function(metric, data) {
      if (!metric %in% names(data)) return(data)
      lower_bound <- quantile(data[[metric]], 0.25, na.rm = TRUE)
      upper_bound <- quantile(data[[metric]], 0.75, na.rm = TRUE)
      data %>%
        mutate(!!paste0(metric, "_performance") := case_when(
          !!sym(metric) <= lower_bound ~ "Bottom 25%",
          !!sym(metric) >= upper_bound ~ "Top 25%",
          TRUE ~ "Middle 50%"
        ))
    }
    
    for (metric in input$metrics) {
      means_by_player <- classify_percentile(metric, means_by_player)
    }
    
    performance_columns <- c("player_name", paste0(input$metrics, "_performance"))
    selected_columns <- performance_columns[performance_columns %in% names(means_by_player)]
    means_by_player %>% select(all_of(selected_columns))
  })
  
  observeEvent(input$update, {
    output$performanceTable <- renderTable({
      calculate_performance()
    })
  })
  
  ####### End of Tab 2: Performance Analysis #######
  
  ####### Start of Tab 3: Performance Score and SQL Statement #######
  
  # SQL Query Logic
  observeEvent(input$run_query, {
    query <- input$sql_query
    result <- tryCatch({
      dbGetQuery(db, query)
    }, error = function(e) {
      data.frame(Error = "Invalid SQL query or database issue")
    })
    output$sqlTable <- renderDT({
      datatable(result)
    })
  })
  
  # Performance score ranking
  performance_data <- reactive({
    means_by_player <- df %>%
      group_by(player_name) %>%
      summarise(across(matches("\\.score|_score"), mean, na.rm = TRUE)) %>%
      rowwise() %>%
      mutate(performance_score = sum(c_across(matches("\\.score|_score")), na.rm = TRUE))
    
    min_score <- min(means_by_player$performance_score, na.rm = TRUE)
    max_score <- max(means_by_player$performance_score, na.rm = TRUE)
    
    means_by_player %>%
      mutate(scaled_score = ((performance_score - min_score) / (max_score - min_score)) * 100)
  })
  
  # Output plot
  output$performancePlot <- renderPlot({
    ggplot(performance_data(), aes(x = reorder(player_name, scaled_score), y = scaled_score)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(
        title = "Scaled Composite Performance Score by Player (1-100)",
        x = "Player Name",
        y = "Scaled Performance Score (1-100)"
      ) +
      theme_minimal()
  })
  
  ####### End of Tab 3: Performance Score and SQL Statement #######
  
  ####### Start of Tab 4: Player Consistency Overview #######
  
  # Reactive filtered data based on date input
  consistency_filtered_data <- reactive({
    df %>%
      filter(as.Date(created_at.date) >= input$unique_date_filter[1] & 
               as.Date(created_at.date) <= input$unique_date_filter[2])
  })
  
  # Calculate consistency data
  consistency_data <- reactive({
    consistency_filtered_data() %>%
      group_by(player_name) %>%
      summarise(
        sd_peak_speed = sd(peak_speed, na.rm = TRUE),
        sd_rotational_acceleration = sd(rotational_acceleration, na.rm = TRUE),
        sd_body_rotation = sd(body_rotation, na.rm = TRUE)
      ) %>%
      mutate(
        inconsistent_peak_speed = sd_peak_speed > input$unique_threshold,
        inconsistent_rotational_acceleration = sd_rotational_acceleration > input$unique_threshold,
        inconsistent_body_rotation = sd_body_rotation > input$unique_threshold
      )
  })
  
  # Render the summary table
  output$unique_consistency_table <- renderTable({
    consistency_data()
  })
  
  # Plots for consistency
  output$unique_swing_speed_plot <- renderPlot({
    ggplot(consistency_data(), aes(x = reorder(player_name, sd_peak_speed), y = sd_peak_speed)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Swing Speed Consistency (Standard Deviation)",
           x = "Player",
           y = "Standard Deviation of Swing Speed") +
      theme_minimal()
  })
  
  output$unique_rotational_acc_plot <- renderPlot({
    ggplot(consistency_data(), aes(x = reorder(player_name, sd_rotational_acceleration), y = sd_rotational_acceleration)) +
      geom_bar(stat = "identity", fill = "darkorange") +
      coord_flip() +
      labs(title = "Rotational Acceleration Consistency (Standard Deviation)",
           x = "Player",
           y = "Standard Deviation of Rotational Acceleration") +
      theme_minimal()
  })
  
  output$unique_body_rotation_plot <- renderPlot({
    ggplot(consistency_data(), aes(x = reorder(player_name, sd_body_rotation), y = sd_body_rotation)) +
      geom_bar(stat = "identity", fill = "purple") +
      coord_flip() +
      labs(title = "Body Rotation Consistency (Standard Deviation)",
           x = "Player",
           y = "Standard Deviation of Body Rotation") +
      theme_minimal()
  })
  
  # Table for inconsistent players
  output$unique_inconsistent_players_table <- renderTable({
    consistency_data() %>%
      filter(inconsistent_peak_speed | inconsistent_rotational_acceleration | inconsistent_body_rotation)
  })
  
  ####### End of Tab 4: Player Consistency Overview #######
  
  ####### Clustering Analysis Logic #######
  
  # Step 2: Group by player_name and calculate the mean of relevant features
  player_means_cluster <- df %>%
    group_by(player_name) %>%
    summarise(
      mean_peak_hand_speed = mean(peak_hand_speed, na.rm = TRUE),
      mean_swing_speed = mean(swing_speed, na.rm = TRUE),
      mean_connection = mean(connection, na.rm = TRUE),
      mean_rotational_acceleration = mean(rotational_acceleration, na.rm = TRUE),
      mean_body_rotation = mean(body_rotation, na.rm = TRUE),
      mean_vertical_bat_angle = mean(vertical_bat_angle, na.rm = TRUE),
      mean_time_to_contact = mean(time_to_contact, na.rm = TRUE)
    )
  
  # Step 3: Standardize the features
  scaled_features <- scale(player_means_cluster %>% select(-player_name))
  
  # Step 4: Perform K-Means clustering with 4 clusters
  set.seed(42)
  kmeans_model <- kmeans(scaled_features, centers = 4)
  
  # Add the cluster assignments to the player_means_cluster data
  player_means_cluster$cluster <- as.factor(kmeans_model$cluster)
  
  # Manually define cluster names
  cluster_names <- c(
    "Cluster 1: Improve Swing Speed",
    "Cluster 2: Enhance Body Rotation",
    "Cluster 3: Better Connection and Timing",
    "Cluster 4: Improve Bat Angle and Acceleration"
  )
  player_means_cluster$cluster_name <- cluster_names[as.numeric(player_means_cluster$cluster)]
  
  # PCA for visual clustering
  pca_result <- prcomp(scaled_features, center = TRUE, scale. = TRUE)
  pca_df <- as.data.frame(pca_result$x[, 1:2])
  pca_df$cluster_name <- player_means_cluster$cluster_name
  pca_df$player_name <- player_means_cluster$player_name
  
  # Render Clustering Plot
  output$clusterPlot <- renderPlotly({
    plot_ly(pca_df, x = ~PC1, y = ~PC2, type = 'scatter', mode = 'markers',
            color = ~cluster_name, text = ~paste("Player: ", player_name, "<br>Cluster: ", cluster_name),
            marker = list(size = 10, opacity = 0.7)) %>%
      layout(title = "K-Means Clustering Visualization (PCA)",
             xaxis = list(title = "Principal Component 1"),
             yaxis = list(title = "Principal Component 2"))
  })
  
  # Render Cluster Data Table
  output$clusterTable <- DT::renderDataTable({
    player_means_cluster %>% select(player_name, cluster_name, everything())
  })
  
  # Output: Summary of player groups
  output$groupSummary <- renderUI({
    summary_text <- player_means_cluster %>%
      group_by(cluster_name) %>%
      summarise(players = paste(player_name, collapse = ", "),
                total_players = n()) %>%
      mutate(summary = paste(cluster_name, ":", total_players, "players -", players)) %>%
      pull(summary)
    
    # Create a list of summary items to display
    HTML(paste(summary_text, collapse = "<br><br>"))
  })
  
  ####### End of Clustering Analysis #######
  
  ############ Start of another #############
  
  # Calculate mean metrics for each player based on the specified weaknesses
  swing_metrics_data <- df %>%
    group_by(player_name) %>%
    summarize(
      early_connection = mean(early_connection, na.rm = TRUE),
      body_tilt_angle = mean(body_tilt_angle, na.rm = TRUE),
      bat_path_angle = mean(bat_path_angle, na.rm = TRUE),
      swing_speed = mean(swing_speed, na.rm = TRUE),
      connection = mean(connection, na.rm = TRUE),
      vertical_bat_angle = mean(vertical_bat_angle, na.rm = TRUE),
      on_plane_efficiency = mean(on_plane_efficiency, na.rm = TRUE),
      peak_hand_speed = mean(peak_hand_speed, na.rm = TRUE),
      power = mean(power, na.rm = TRUE),
      rotational_acceleration = mean(rotational_acceleration, na.rm = TRUE),
      time_to_contact = mean(time_to_contact, na.rm = TRUE),
      body_rotation = mean(body_rotation, na.rm = TRUE),
      commit_time = mean(commit_time, na.rm = TRUE),
      on_plane = mean(on_plane, na.rm = TRUE),
      peak_speed = mean(peak_speed, na.rm = TRUE)
    )
  
  # Scale the data for clustering
  numeric_data <- swing_metrics_data %>% select_if(is.numeric)
  scaled_data <- scale(numeric_data)
  
  # Apply K-Means clustering (3 clusters as an example)
  set.seed(42)
  kmeans_result <- kmeans(scaled_data, centers = 3)
  
  # Add cluster information back to the dataframe
  swing_metrics_data$cluster <- as.factor(kmeans_result$cluster)
  
  # Assign new cluster names based on observed deficiencies
  cluster_names <- c("Inconsistent Contact Path", "Low Power Output", "Inefficient Swing Dynamics")
  swing_metrics_data$cluster_name <- factor(swing_metrics_data$cluster, labels = cluster_names)
  
  # Function to fit a linear model and return the summary
  fit_linear_model <- function(x, y) {
    valid_indices <- complete.cases(x, y)
    model <- lm(y[valid_indices] ~ x[valid_indices])
    return(summary(model))
  }
  
  # Function to add regression line to the Plotly plot
  add_regression_line <- function(x, y) {
    valid_indices <- complete.cases(x, y)
    model <- lm(y[valid_indices] ~ x[valid_indices])
    line_data <- data.frame(
      x = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = 100),
      y = predict(model, newdata = data.frame(x = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = 100)))
    )
    return(line_data)
  }
  
  # Function to calculate R^2 between two variables
  calculate_r_squared <- function(x, y) {
    valid_indices <- complete.cases(x, y)
    model <- lm(y[valid_indices] ~ x[valid_indices])
    summary(model)$r.squared
  }
  
  # Render dynamic scatter plot with regression line using Plotly
  output$linearPlot <- renderPlotly({
    # Extract selected variables
    x_data <- swing_metrics_data[[input$xvar]]
    y_data <- swing_metrics_data[[input$yvar]]
    
    # Calculate R^2 value
    r_squared <- calculate_r_squared(x_data, y_data)
    
    # Get regression line data
    line_data <- add_regression_line(x_data, y_data)
    
    # Create the plotly scatter plot with regression line
    plot_ly() %>%
      add_trace(
        data = swing_metrics_data,
        x = as.formula(paste("~", input$xvar)),
        y = as.formula(paste("~", input$yvar)),
        color = ~cluster_name,
        text = ~paste("Player:", player_name, 
                      "<br>", input$xvar, ":", swing_metrics_data[[input$xvar]],
                      "<br>", input$yvar, ":", swing_metrics_data[[input$yvar]]),
        type = 'scatter',
        mode = 'markers'
      ) %>%
      add_trace(
        data = line_data,
        x = ~x,
        y = ~y,
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'red', dash = 'dash')
      ) %>%
      layout(
        title = paste("Linear Model of", input$yvar, "vs", input$xvar),
        xaxis = list(title = input$xvar),
        yaxis = list(title = input$yvar),
        legend = list(title = list(text = "Cluster Group")),
        annotations = list(
          x = 0.05, y = 1.05, text = paste("R² =", round(r_squared, 3)),
          showarrow = FALSE, xref='paper', yref='paper',
          font = list(size = 12, color = ifelse(r_squared > 0.5, "green", "red"))
        )
      )
  })
  
  # Render model summary
  output$modelSummary <- renderPrint({
    # Extract selected variables
    x_data <- swing_metrics_data[[input$xvar]]
    y_data <- swing_metrics_data[[input$yvar]]
    
    # Fit linear model and show summary
    model_summary <- fit_linear_model(x_data, y_data)
    print(model_summary)
  })
  
  # ---------------------------------------------------
  # Start of Server Logic for "Player Progression Tracking" Tab
  # ---------------------------------------------------
  
  # Reactive data filtered by player, without date filtering
  filtered_data <- reactive({
    app_data <- df
    app_data$created_at.date <- as.Date(app_data$created_at.date, format = "%Y-%m-%d")
    
    app_data %>%
      filter(player_name == input$player_name) %>%
      group_by(created_at.date) %>%
      summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
      arrange(created_at.date)
  })
  
  # Render the plot using Plotly
  output$progression_plot <- renderPlotly({
    player_data <- filtered_data()
    
    p <- ggplot(player_data, aes(x = created_at.date)) +
      labs(title = paste("Player Progression Over Time:", input$player_name),
           x = "Date",
           y = "Metrics",
           color = "Metrics") +
      theme_minimal()
    
    if ("peak_hand_speed" %in% input$metrics) {
      p <- p + geom_line(aes(y = peak_hand_speed, color = metric_names$peak_hand_speed), size = 1.2)
    }
    if ("swing_speed" %in% input$metrics) {
      p <- p + geom_line(aes(y = swing_speed, color = metric_names$swing_speed), size = 1.2)
    }
    if ("on_plane_efficiency" %in% input$metrics) {
      p <- p + geom_line(aes(y = on_plane_efficiency, color = metric_names$on_plane_efficiency), size = 1.2)
    }
    if ("rotational_acceleration" %in% input$metrics) {
      p <- p + geom_line(aes(y = rotational_acceleration, color = metric_names$rotational_acceleration), size = 1.2)
    }
    if ("body_rotation" %in% input$metrics) {
      p <- p + geom_line(aes(y = body_rotation, color = metric_names$body_rotation), size = 1.2)
    }
    
    ggplotly(p)
  })
  
  # Render the summary table with user-friendly names
  output$summary_table <- renderDT({
    player_data <- filtered_data()
    
    summary_stats <- player_data %>%
      summarise(across(all_of(input$metrics), 
                       list(Average = ~mean(.x, na.rm = TRUE),
                            Min = ~min(.x, na.rm = TRUE),
                            Max = ~max(.x, na.rm = TRUE))))
    
    colnames(summary_stats) <- sapply(strsplit(colnames(summary_stats), "_"), function(x) {
      paste(metric_names[[x[1]]], x[2])
    })
    
    summary_table <- t(summary_stats) %>% as.data.frame()
    summary_table$Metric <- rownames(summary_table)
    summary_table <- summary_table %>% 
      select(Metric, everything()) %>%
      rename(Average = V1, Min = V2, Max = V3)
    
    datatable(summary_table, 
              rownames = FALSE,
              options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  # ---------------------------------------------------
  # End of Server Logic for "Player Progression Tracking" Tab
  # ---------------------------------------------------
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
