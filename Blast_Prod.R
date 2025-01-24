#setwd("C:/Users/ibach/OneDrive - Terillium/Pictures/Moeller_Blast")
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
    # Introduction Tab
    tabPanel("Introduction",
             div(class = "custom-border",
                 h3("Overview of Dashboard"),
                 p("**Updated as of 01/22/2025**"),
                 p("**Developed exclusively for Moeller Baseball by Ian D. Bach**"),
                 p("The Moeller Blast Dashboard leverages Blast Motion Data to provide comprehensive insights into hitter performance."),
                 p("This advanced tool is organized into key sections, each crafted to highlight specific metrics and performance aspects with clarity and intuition."),
                 p("Users can delve into detailed swing analytics, track swing efficiency and power, and compare performance across different game situations and pitch types."),
                 p("The dashboard includes dynamic visualizations of metrics such as bat speed, on-plane efficiency, and rotational acceleration, offering a detailed perspective on swing mechanics."),
                 p("Additionally, it highlights player consistency, progression over time, and clustering insights, enabling coaches and players to develop targeted strategies for performance enhancement."),
                 
                 tags$video(src = "https://www.dropbox.com/scl/fi/uludelalhfeg93hzmomr2/Blast-Baseball-Solution-Overview-YouTube.mp4?rlkey=6wf7m2kxf1p1php8xckcjdgz6&st=i1cpcja3&raw=1", 
                            type = "video/mp4", controls = TRUE, width = "80%", height = "auto"),
                 p("This video provides a comprehensive overview of the Blast Baseball solution.")
             )
    ),
    
    
    tabPanel("Tabs Overview", 
             
             h3("Headers/Tabs:"),
             
             div(class = "border-box",
                 h3(tags$u("Percentile Plot and Filter Tab")),
                 p("The 'Percentile Plot and Filter' tab allows users to visualize individual player performance percentiles across various metric groups, offering insights into player rankings within specific skill areas."),
                 p("Users can select a player and a metric group, such as 'Plane/Contact,' 'Connection,' or 'Rotational Power,' to focus on key aspects of performance."),
                 p("After pressing the 'Generate Plot' button, a lollipop-style plot displays the chosen player's rank within their selected group."),
                 p("Additionally, this tab features two tables: one showing overall mean values across all players and another with player-specific averages, providing a comprehensive view of individual and group performance statistics.")
             ),
             div(class = "border-box",
                 h3(tags$u("Blast Reports")),
                 p("Power Plot: Plots bat speed against rotational acceleration, with a target performance zone highlighted in green."),
                 p("Contact Plot: Displays on-plane efficiency versus bat path angle, again marking a preferred range in green for ideal contact metrics."),
                 p("Consistency Plot: Visualizes vertical bat angle versus connection, adding early connection points for comparison. Each plot is enhanced with Plotly interactivity for better user engagement and clarity.")
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
             )),
    
    # Data Dictionary Tab
    tabPanel("Data Dictionary",
             DTOutput("data_dict_table")),
    
    
    tabPanel(
      "Percentile Plot and Filter",
      sidebarLayout(
        sidebarPanel(
          selectInput("ath", "Select Player:", choices = unique(df$player_name)),
          selectInput("group", "Select Group:", choices = c("Plane/Contact", "Connection", "Rotational Power")),
          actionButton("plotButton", "Generate Plot")
        ),
        mainPanel(
          img(src = "https://github.com/IDBach16/Blast_Data_Moe/raw/main/Screenshot%202024-11-07%20112644.png", 
              height = "400px", width = "700px"),
          div(class = "border-box",
              h3(tags$u("Percentile Lollipop Plots")),
              p("Purpose: Shows how a player ranks within the team for specific metrics."),
              p("How to Use: Select a metric group (Plane/Contact, Connection, or Rotational Power) and a player."),
              p("Percentiles represent a player’s rank within the team:"),
              p("- High Percentile (75th–100th): Among the top performers in this metric."),
              p("- Middle Percentile (25th–75th): Indicates average performance within the team."),
              p("- Low Percentile (0–25th): Highlights areas for improvement."),
              p("Example: A 90th percentile for on-plane efficiency means the player’s contact quality is among the best on the team.")
          ),
          div(class = "border-box",
              h3(tags$u("Metric Names and Key Insights")),
              p("Peak Hand Speed (mph): Max speed of hands during the swing; helps gauge quickness."),
              p("Swing Speed (mph): Bat speed in the zone, indicating power potential."),
              p("On Plane Efficiency (%): Time bat stays aligned with pitch path; higher values suggest better contact."),
              p("Rotational Acceleration (g): Body’s rotation speed, impacting bat speed and swing power."),
              p("Body Rotation (degrees): Player’s rotation angle during the swing, essential for control.")
          ),
          div(class = "border-box",
              h3(tags$u("Key Metrics to Watch Out For")),
              p("For strong overall performance, keep an eye on the following:"),
              p("- On Plane Efficiency: High values suggest consistent contact with pitches."),
              p("- Rotational Acceleration: Essential for explosive power."),
              p("- Connection: Should remain close to the team mean for stability."),
              p("- Connection Score: Higher percentiles indicate strong control throughout the swing.")
          ),
          plotOutput("percentilePlot"),
          div(class = "border-box",
              h3(tags$u("Overall Averages")),
              p("What It Shows: Team-wide averages for each metric, providing a baseline for individual comparison."),
              p("How to Use: Compare individual players to the team average to quickly spot strengths and areas for improvement.")
          ),
          #h3("Overall Means"),
          DTOutput("meansTable"),
          div(class = "border-box",
              h3(tags$u("Player-Specific Averages")),
              p("What It Shows: Displays each player’s average for all metrics."),
              p("How to Use: Select a player to view their personal averages and compare them to team benchmarks.")
          ),
          #h3("Means by Player"),
          DTOutput("meansByPlayerTable")
        )
      )
    ),
    
    tabPanel(
      "Blast Reports",
      sidebarLayout(
        sidebarPanel(
          selectInput("player", "Select Player:", choices = unique(df$player_name)),
          hr()
        ),
        mainPanel(
          div(class = "border-box",
              h3(tags$u("Chart Style Overview for Coaches")),
              p("Thes charts use a simple, clear style to focus attention. Blue dots represent data points, with labels on both axes and a centered title.
                A green shaded area highlights a target zone, while soft gridlines provide structure without distraction. This clean design makes the chart easy to interpret at a glance.")),
          img(src = "https://github.com/IDBach16/Blast_Data_Moe/raw/main/Screenshot%202024-11-07%20112644.png", 
              height = "400px", width = "700px"),
          div(class = "border-box",
              h3("Power"),
              p("The Power plot displays the relationship between Bat Speed (mph) and Rotational Acceleration (g). 
           High values in both Bat Speed and Rotational Acceleration indicate powerful swings. The green shaded area 
           highlights the optimal range for peak performance, where high bat speed and rotational acceleration 
           coincide."),
              plotlyOutput("powerPlot")),
          
          div(class = "border-box",
              h3("Contact"),
              p("The Contact plot visualizes On Plane Efficiency (%) versus Attack Angle (deg), focusing on the hitter's 
           swing path and efficiency. High on-plane efficiency and ideal attack angles generally improve contact quality, 
           shown by the green shaded area where ideal values align. This plot helps in assessing the swing's 
           effectiveness in staying on plane with the pitch."),
              plotlyOutput("contactPlot")),
          
          div(class = "border-box",
              h3("Consistency"),
              p("The Consistency plot compares Vertical Bat Angle (deg) with Connection (deg) and Early Connection (deg). 
           Vertical bat angle and connection consistency help determine a player’s swing stability, with the green 
           shaded region highlighting the optimal range. Consistency in connection angles indicates a repeatable 
           swing, which is crucial for achieving consistent results at the plate."),
              p("Blue = Connection at Impact"),
              p("Grey = Early Connection"),
              plotlyOutput("consistencyPlot"))
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
                 div(class = "border-box",
                     h3(tags$u("Enhanced Explanation of Correlation Matrix Analysis for Coaches")),
                     p("The correlation matrix analysis is a straightforward way to identify and leverage the relationships between key performance metrics for players. Here’s how it works with our custom code:"),
                     p(tags$b("Filtering the Data:"), " Only numeric columns are included, with 'score' and 'peak speed' metrics excluded to focus on the most relevant factors."),
                     p(tags$b("Creating the Correlation Matrix:"), " We calculate the relationship between each pair of metrics in the data, producing values that range from -1 to +1."),
                     p(tags$b("Identifying High Correlations:"), " Based on a user-defined threshold, the code highlights pairs of metrics with high positive correlations, showing where improving one metric might likely improve another."),
                     p(tags$b("Visualizing Results:"), " Coaches can view this data in a clear table with the most relevant metric pairs, and in an interactive heatmap that highlights strong relationships with color gradients."),
                     p(tags$b("Using the Correlation Matrix Analysis:"), " This tool provides a data-driven perspective on player performance, helping coaches focus training on key metrics that influence each other and simplifying complex data into actionable insights.")
                 ),
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
                 div(class = "border-box",
                     h3(tags$u("Performance Calculation Module for Coaches")),
                     p("This module evaluates player performance across metrics, using percentiles to highlight strengths and improvement areas."),
                     
                     h3(tags$u("Key Features:")),
                     p(tags$b("Player Averages:"), " Provides each player’s average across selected metrics for a baseline view."),
                     p(tags$b("Percentile Classification:"), " Players are grouped into performance tiers:"),
                     p("- ", tags$b("Top 25%:"), " Above-average performers."),
                     p("- ", tags$b("Middle 50%:"), " Average performers."),
                     p("- ", tags$b("Bottom 25%:"), " Development areas."),
                     
                     p(tags$b("Dynamic Performance Display:"), " The table refreshes with each update, showing current performance tiers."),
                     p(tags$b("Interactive Table:"), " Coaches can view each player’s tier, focusing on strengths and areas to improve."),
                     
                     h3(tags$u("Using This Tool:")),
                     p(tags$b("Identify Standouts:"), " Recognize top players for strengths and lineup planning."),
                     p(tags$b("Targeted Coaching:"), " Focus training on metrics where players are in lower tiers.")
                 ),
                 
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
                 div(class = "border-box",
                     h3(tags$u("SQL Query and Performance Ranking Module for Coaches")),
                     p("This module lets coaches run custom queries on player data and view overall performance rankings to assess team strengths."),
                     h3(tags$u("SQL Query Execution")),
                     p(tags$b("Purpose:"), " Coaches can enter custom SQL queries to retrieve specific data."),
                     p(tags$b("Error Handling:"), " If an error occurs, an error message will help troubleshoot."),
                     p(tags$b("Display:"), " Results appear in an interactive table for easy exploration.")
                 ),
                 h3("SQL Query Results (Show Below After Execution)"),
                 DTOutput("sqlTable"),
                 # Performance Score Calculation
                 div(class = "border-box",
                     h3(tags$u("Performance Score Calculation")),
                     p(tags$b("Average Scores:"), " Calculates average scores for each performance metric for every player."),
                     p(tags$b("Composite Score:"), " Combines these averages into a single 'performance score' for each player."),
                     p(tags$b("Scaled Score (1-100):"), " Normalizes scores to a 1-100 scale for easy comparison."),
                     h3(tags$u("Visual Performance Ranking")),
                     p(tags$b("Player Rankings:"), " Players are ranked by their scaled performance score, with top performers at the top."),
                     p(tags$b("Bar Chart:"), " Displays each player's score in a horizontal bar chart, making it easy to spot leaders and areas for improvement.")
                 ),
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
                 mainPanel(
                   # Introductory Explanation
                   div(class = "border-box",
                       h3(tags$u("Consistency Analysis Overview")),
                       p("This tab helps coaches track player consistency in peak speed, rotational acceleration, and body rotation over a selected date range. Using standard deviation, it highlights any players whose performance varies beyond a set Inconsistency Threshold.
                         Coaches can adjust this threshold to define acceptable consistency levels for each metric, making it easy to spot and address areas needing stability training. This tool offers a clear, customizable way to support consistent player performance.")
                   ),
                   br(),
                   
                   # Summary Table
                   div(class = "border-box",
                       h3(tags$u("Summary Table")),
                       p("This table provides a breakdown of each player's standard deviations for key metrics, helping to identify those with high variability."),
                       tableOutput("unique_consistency_table")
                   ),
                   br(),
                   
                   # Swing Speed Consistency Plot
                   div(class = "border-box",
                       h3(tags$u("Swing Speed Consistency (Standard Deviation)")),
                       p("This plot shows the standard deviation in swing speed for each player, with higher values indicating greater inconsistency."),
                       plotOutput("unique_swing_speed_plot")
                   ),
                   br(),
                   
                   # Rotational Acceleration Consistency Plot
                   div(class = "border-box",
                       h3(tags$u("Rotational Acceleration Consistency (Standard Deviation)")),
                       p("This plot represents the variation in rotational acceleration for each player, highlighting those who struggle with rotational stability."),
                       plotOutput("unique_rotational_acc_plot")
                   ),
                   br(),
                   
                   # Body Rotation Consistency Plot
                   div(class = "border-box",
                       h3(tags$u("Body Rotation Consistency (Standard Deviation)")),
                       p("This plot visualizes each player's body rotation consistency, indicating areas where rotational control may need focus."),
                       plotOutput("unique_body_rotation_plot")
                   ),
                   br(),
                   
                   # Inconsistent Players Table
                   div(class = "border-box",
                       h3(tags$u("Inconsistent Players")),
                       p("This table highlights players who have exceeded the consistency thresholds for one or more metrics, showing areas for targeted stability training."),
                       tableOutput("unique_inconsistent_players_table")
                   )
                 )
                 
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
                 # Clustering Analysis Overview
                 div(class = "border-box",
                     h3(tags$u("Clustering Analysis Overview")),
                     p("K-Means is a method for grouping players with similar performance patterns. It works by dividing data into a set number of clusters (in this case, four) and finding a central point for each cluster.
                       Players are assigned to the cluster they’re closest to, and the centers are adjusted until the groups are stable. Here, I used K-Means on metrics like swing speed, hand speed, and body rotation, creating clusters that show each player's strengths and focus areas. By standardizing the metrics, each cluster reflects balanced performance comparisons among players.")
                 ),
                 br(),
                 
                 # Key Steps in Clustering
                 div(class = "border-box",
                     h4(tags$u("Key Steps")),
                     p(tags$b("1. Data Grouping and Averaging:"), " Averages are calculated for each player's metrics (e.g., hand speed, swing speed, rotation) to summarize performance."),
                     p(tags$b("2. Standardizing Metrics:"), " Metrics are standardized to prevent scale differences from affecting the results."),
                     p(tags$b("3. K-Means Clustering:"), " Players are grouped into four clusters based on performance patterns. Each cluster suggests a focus area:")
                 ),
                 br(),
                 
                 # Clustering Visualization
                 h3("K-Means Clustering Visualization"),
                 plotlyOutput("clusterPlot"),
                 br(),
                 
                 # Cluster Data Table
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
                 div(class = "border-box",
                     h3(tags$u("Understanding the Linear Model")),
                     p("A linear model helps us see how one metric (like swing speed) affects another (like power). It draws a line that best represents the relationship between two variables."),
                     p("For instance, if swing speed impacts power, the model shows how changes in swing speed might increase or decrease power."),
                     h3(tags$u("Interpreting R² (R-squared)")),
                     p("R² measures the strength of the relationship between two variables. If R² is close to 1, it means the line accurately explains the relationship, indicating a strong effect of one metric on the other."),
                     p("If R² is closer to 0, it suggests little to no relationship, meaning changes in one metric may not affect the other significantly."),
                     p("By looking at R², coaches can gauge how much one metric influences another, helping to prioritize training focus.")
                 ),
                 br(),
                 plotlyOutput("linearPlot"),
                 div(class = "border-box",
                     h4("Understanding the Cluster Types"),
                     
                     p(tags$b("Inconsistent Contact Path:"), 
                       " This cluster represents players who may struggle with maintaining a consistent swing path, leading to varying contact points with the ball. ",
                       "This could be due to inconsistencies in metrics like on-plane efficiency, connection, or early connection. ",
                       "Players in this group may benefit from drills that emphasize maintaining a steady, repeatable swing path to improve their consistency."),
                     
                     p(tags$b("Low Power Output:"), 
                       " Players in this cluster exhibit lower than expected power metrics, such as swing speed, power, and rotational acceleration. ",
                       "This suggests they may need to work on generating more force and improving their core strength and technique to maximize power transfer during their swing."),
                     
                     p(tags$b("Inefficient Swing Dynamics:"), 
                       " This group reflects players whose swings are not as efficient, possibly due to issues in timing, body rotation, or overall mechanics. ",
                       "Indicators might include higher commit time or irregularities in time to contact. ",
                       "Improving swing efficiency can help players in this cluster achieve smoother, more effective swings.")
                 ),
                 
                 hr(),
                 div(class = "border-box",
                     h3(tags$u("Interpreting the Linear Model Output for Analytics Team & Coaches")),
                     p("This output helps coaches understand how well the linear model explains the relationship between two variables. Each section provides insights into the model's accuracy and significance."),
                     
                     h4(tags$u("Residuals")),
                     p("Residuals show the difference between actual data points and the model’s predictions. Ideally, residuals are close to zero, indicating the model accurately fits the data."),
                     p("In this case, residuals would be small if the model has a close match with the actual data."),
                     
                     h4(tags$u("Coefficients")),
                     p(tags$b("Intercept:"), " The starting point of the line when the independent variable (X) is zero. It represents the expected value of the outcome variable when the predictor is zero."),
                     p(tags$b("Coefficient for Predictor:"), " This value shows how much the outcome variable is expected to change with a one-unit increase in the predictor. A positive coefficient indicates an increase in the outcome, while a negative one indicates a decrease."),
                     p(tags$b("t value and p-value:"), " These values indicate the statistical significance of the predictor. A low p-value (typically below 0.05) suggests that the predictor's effect is statistically significant and not due to random chance."),
                     
                     h4(tags$u("R-squared")),
                     p("The R-squared value measures the proportion of variability in the outcome variable that the model explains. An R-squared value close to 1 indicates that the model provides an excellent fit to the data, capturing most of the variability."),
                     p("Adjusted R-squared accounts for the number of predictors in the model, making it a more reliable indicator of fit when using multiple predictors."),
                     
                     h4(tags$u("F-statistic")),
                     p("The F-statistic and its associated p-value assess the overall significance of the model. A high F-statistic with a low p-value indicates that the model, as a whole, significantly explains the variation in the outcome variable, giving confidence in its predictive strength.")
                 ),
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
                 # Explanation for Coaches: Player Progression Tracking
                 div(class = "border-box",
                     h4("Explanation for Coaches: Player Progression Tracking"),
                     
                     p("This section of the app lets coaches track a player's performance progression over time, focusing on specific metrics without limiting to a date range. Here’s how it works:"),
                     
                     h5(tags$b("Filtering Data by Player")),
                     p("The app filters data based on the selected player, grouping it by date. For each date, it calculates daily averages for selected metrics, giving a day-by-day breakdown of the player’s performance."),
                     
                     h5(tags$b("Visualizing Performance Progression")),
                     p("A line plot shows how each selected metric changes over time. For example, metrics like peak hand speed, swing speed, on-plane efficiency, rotational acceleration, or body rotation appear as individual lines, helping coaches identify trends such as improvements or declines over time."),
                     
                 ),
                 br(),
                 
                 # Plot and Table Outputs
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
  
  ############ Start of  LM another #############
  
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
  
  
  ##############Data Dictonary Creation:::
  data_dict <- data.frame(
    Variable = c("On Plane Efficiency Score", "On Plane Efficiency", "Rotational Acceleration Score", 
                 "Early Connection", "Connection", "Connection Score", "Swing Speed", "Vertical Bat Angle", 
                 "Time to Contact", "Peak Speed", "Peak Speed Score", "Power", "Power Score", "Body Tilt Angle", 
                 "Bat Path Angle Score", "Bat Path Angle", "Rotational Acceleration (Score)", 
                 "Rotational Acceleration", "Body Rotational (Score)", "Body Rotation", "Commit Time", 
                 "On Plane", "Blast Factor 2", "Peak Hand Speed"),
    Ranges = c("20-80", "37-99", "25 - 80", "49 - 136", "62 - 112", "20-80", "30.4 - 84.0", 
               "-65 - (-7)", "0.12 - 0.27", "30.6 - 84.0", "20-80", "0.46 - 6.42", "20 - 80", "6 - 73", 
               "0-80", "-32 - 32", "20-80", "0.6 - 26.9", "20 - 80", "28 - 69", "0.02 - 0.17", 
               "5 - 99", "17-85", "10.8 - 26.8"),
    Data_Type = c("Integer", "Integer", "Integer", "Integer", "Integer", "Integer", "Numeric", 
                  "Integer", "Numeric", "Numeric", "Integer", "Numeric", "Integer", "Integer", 
                  "Integer", "Integer", "Integer", "Numeric", "Integer", "Integer", "Numeric", 
                  "Integer", "Integer", "Numeric"),
    Correlation = c(
      "Moderate negative with Early Connection (-0.47), Moderate positive with Body Tilt Angle (0.29)", 
      "Moderate negative with Early Connection (-0.48), Moderate positive with Body Tilt Angle (0.29)", 
      "Peak Speed, Body Tilt Angle", 
      "Strong positive with Connection (0.58), Moderate negative with Body Tilt Angle (-0.61), Weak positive with Peak Speed (0.15)", 
      "Strong positive with Early Connection (0.58), Moderate positive with Vertical Bat Angle (0.17)", 
      "Strong negative with Early Connection (-0.42), Moderate negative with Swing Speed (-0.18)", 
      "Very strong positive with Peak Speed (0.99), Strong positive with Blast Factor 2 (0.83)", 
      "Moderate negative with Body Tilt Angle (-0.59), Weak positive with Connection (0.17)", 
      "Strong negative with Swing Speed (-0.57)", 
      "Very strong positive with Swing Speed (0.99)", 
      "", 
      "", 
      "", 
      "Strong negative with Early Connection (-0.61), Moderate negative with Connection (-0.68)", 
      "Weak positive with Early Connection (0.17), Moderate positive with Bat Path Angle (0.23)", 
      "Strong positive with Bat Path Angle Score (1.00), Weak positive with Peak Speed (0.21)", 
      "Weak positive with Swing Speed (0.29), Weak positive with Peak Speed (0.10)", 
      "Weak positive with Swing Speed (0.30), Weak positive with Rotational Acceleration Score (0.29)", 
      "Weak negative with Early Connection (-0.13)", 
      "Weak negative with Swing Speed (-0.18)", 
      "Weak positive with Swing Speed (0.28)", 
      "Weak negative with Swing Speed (-0.24)", 
      "Strong positive with Swing Speed (0.83), Weak positive with Early Connection (0.14)", 
      ""
    ),
    Definition = c(
      "This measures how efficiently the bat stays in the hitting plane throughout the swing. Higher efficiency typically leads to more consistent and solid contact with the ball.",
      "Measures the percentage of your swing where the bat is on the swing plane. Your vertical bat angle at contact establishes the plane for that swing. Plane is a great indicator for making more consistent contact on the barrel of the bat.",
      "Rotation measures how quickly your bat accelerates into the swing plane. Rotation is a good indicator of how you build bat speed by sequencing properly vs. pulling the bat with your hands.",
      "Early Connection measures the relationship between your body tilt and vertical bat angle at the start of the downswing. A higher value indicates better early bat-ball connection.",
      "This variable evaluates the overall connection quality between the bat and the ball.",
      "Connection at impact measures the relationship between your body tilt and vertical bat angle at impact. Maintaining good connection for all pitch locations indicates dynamic adjustability.",
      "Bat Speed is the observed speed of the sweet spot of the bat at impact.",
      "Vertical Bat Angle is the angle of the bat with respect to horizontal at the moment of impact. Provides the location of the barrel of the bat relative to the knob.",
      "This represents how quickly a player can get the bat to the ball from the start of their swing, measured in seconds.",
      "Same as Swing Speed.",
      "Same as Swing Speed on a 20 - 80 scale.",
      "The average Power generated during the swing, measured in Watts.",
      "Same as Power on a 20 - 80 scale.",
      "This measures the angle of the player's body tilt during the swing. A good tilt can help a player generate more power and control in the swing.",
      "The bat path angle reflects the direction of the bat as it moves through the swing relative to the ball.",
      "The bat path angle reflects the direction of the bat as it moves through the swing relative to the ball.",
      "Same as Rotational Acceleration on a 20-80 scale.",
      "Rotational acceleration refers to how quickly a player’s bat speeds up in a circular motion as they swing.",
      "This refers to how well the player's body rotates during the swing.",
      "This refers to how well the player's body rotates during the swing.",
      "The time it takes for the hitter to commit to swinging after deciding to do so.",
      "These variables refer to the top speed of the bat during the swing.",
      "Blast Factor is a holistic swing score that is a weighted average of other scored metrics.",
      "The measure of the maximum speed at the handle of the bat."
    )
  )
  
  output$data_dict_table <- renderDT({
    datatable(data_dict, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  
  #########################################
  #### Start of Blast Internal Plots#####
  # Filter data by player
  player_data <- reactive({
    df %>% filter(player_name == input$player)
  })
  
  # Power Plot
  output$powerPlot <- renderPlotly({
    p <- ggplot(player_data(), aes(x = peak_speed, y = rotational_acceleration)) +
      geom_point(alpha = 0.5, color = "blue") +
      labs(title = "Power",
           x = "Bat Speed (mph)",
           y = "Rotational Acceleration (g)") +
      xlim(45, 80) +
      ylim(0, 35) +
      annotate("rect", xmin = 60, xmax = 80, ymin = 0, ymax = 20, alpha = 0.1, fill = "green") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Contact Plot
  output$contactPlot <- renderPlotly({
    p <- ggplot(player_data(), aes(x = on_plane_efficiency, y = bat_path_angle)) +
      geom_point(alpha = 0.5, color = "blue") +
      labs(title = "Contact",
           x = "On Plane Efficiency (%)",
           y = "Attack Angle (deg)") +
      xlim(20, 100) +
      ylim(-30, 30) +
      annotate("rect", xmin = 60, xmax = 100, ymin = 5, ymax = 20, alpha = 0.1, fill = "green") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Consistency Plot
  output$consistencyPlot <- renderPlotly({
    p <- ggplot(player_data()) +
      geom_point(aes(x = vertical_bat_angle, y = connection), alpha = 0.5, color = "blue") +
      geom_point(aes(x = vertical_bat_angle, y = early_connection), alpha = 0.5, color = "grey") +
      labs(title = "Consistency",
           x = "Vertical Bat Angle (deg)",
           y = "Connection (deg)") +
      xlim(0, -50) +
      ylim(60, 130) +
      annotate("rect", xmin = -10, xmax = -50, ymin = 80, ymax = 100, alpha = 0.1, fill = "green") +
      theme_minimal() +
      scale_color_manual(values = c("blue", "grey")) +
      theme(legend.position = "top") +
      guides(color = guide_legend(title = NULL))
    ggplotly(p)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
