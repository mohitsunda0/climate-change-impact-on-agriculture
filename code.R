library(shiny)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(randomForest)
library(dplyr)
library(shinyjs)

# Load or Initialize User Data
user_file <- "users.csv"
if (!file.exists(user_file)) {
  write.csv(data.frame(username = character(), password = character()), user_file, row.names = FALSE)
}

# Placeholder Dataset
processed_data <- data.frame(
  Average_Temperature_C = runif(100, 10, 30),
  Total_Precipitation_mm = runif(100, 50, 200),
  Crop_Yield_MT_per_HA = runif(100, 1, 5),
  Year = sample(2000:2025, 100, replace = TRUE)
)

# Preprocess the Data
preprocess_data <- function(data) {
  data %>%
    mutate(Climate_Impact_Score = Average_Temperature_C * Total_Precipitation_mm)
}

processed_data <- preprocess_data(processed_data)

# Prepare ML Model
prepare_ml_model <- function(data) {
  features <- c("Average_Temperature_C", "Total_Precipitation_mm", "Year")
  target <- "Crop_Yield_MT_per_HA"
  
  set.seed(123)
  rf_model <- randomForest(
    formula = as.formula(paste(target, "~ ", paste(features, collapse = " + "))),
    data = data,
    ntree = 100
  )
  
  return(rf_model)
}

ml_model <- prepare_ml_model(processed_data)

# UI Layout
ui <- fluidPage(
  # Custom CSS for Landing Page and Popups
  
  tags$head(
    tags$style(HTML("
      * {
        margin: 0;
        padding: 0;
        box-sizing: border-box;
      }

      body {
        font-family: 'Arial', sans-serif;
        line-height: 1.6;
        background-color: #f8f8f8;
      }

      /* Background */
      .landing-page {
        background-image: url('farming3.jpg');
        background-size: cover;
        background-position: center;
        color: white;
        min-height: 100vh;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: space-between;
      }

      /* Header */
      .header {
        width: 100%;
        padding: 20px;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }

      .header .logo {
        font-size: 24px;
        font-weight: bold;
        color: white;
      }

      .header nav {
        display: flex;
        gap: 20px;
      }

      .header nav a {
        text-decoration: none;
        color: white;
        font-size: 16px;
      }

      .header nav a:hover {
        color: #90ee90;
      }

      .header .auth-buttons {
        display: flex;
        gap: 15px;
      }

      .header .auth-buttons a {
        padding: 8px 20px;
        border-radius: 20px;
        text-decoration: none;
        color: white;
        font-size: 14px;
      }

      .header .auth-buttons .login {
        background: transparent;
        border: 1px solid white;
      }

      .header .auth-buttons .signup {
        background: #2ecc71;
        border: none;
      }

      /* Popup Overlay */
      .popup-overlay {
        display: none;
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(0, 0, 0, 0.5);
        z-index: 1000;
        justify-content: center;
        align-items: center;
      }

      .popup-content {
        background: white;
        padding: 30px;
        border-radius: 10px;
        width: 400px;
        max-width: 90%;
        position: relative;
      }

      .popup-close {
        position: absolute;
        top: 10px;
        right: 10px;
        font-size: 24px;
        cursor: pointer;
      }

      /* Main Content */
      .hero {
        text-align: center;
        padding: 50px 20px;
      }

      .hero h1 {
        font-size: 48px;
        font-weight: bold;
        margin-bottom: 20px;
      }

      .hero p {
        font-size: 16px;
        margin-bottom: 30px;
      }

      .hero .btn-container {
        display: flex;
        gap: 20px;
        justify-content: center;
      }

      .hero .btn-container a {
        text-decoration: none;
        padding: 15px 30px;
        border-radius: 30px;
        font-size: 16px;
        font-weight: bold;
        color: white;
      }

      .hero .btn-container .get-started {
        background-color: #2ecc71;
      }

      .hero .btn-container .learn-more {
        background-color: #3498db;
      }

      /* Statistics Section */
      .statistics {
        display: flex;
        justify-content: center;
        gap: 40px;
        padding: 40px;
        background: white;
        color: #333;
        border-radius: 20px;
        margin: 20px auto;
        width: 90%;
      }

      .statistics .stat {
        text-align: center;
      }

      .statistics .stat h3 {
        font-size: 28px;
        font-weight: bold;
        color: #2ecc71;
      }

      .statistics .stat p {
        font-size: 14px;
        margin-top: 10px;
      }

      /* Login Form Styles */
      .login-form input, .signup-form input {
        width: 100%;
        padding: 10px;
        margin: 10px 0;
        border: 1px solid #ddd;
        border-radius: 5px;
      }

      .login-form button, .signup-form button {
        width: 100%;
        padding: 10px;
        background-color: #2ecc71;
        color: white;
        border: none;
        border-radius: 5px;
        cursor: pointer;
      }
    "))
  ),
  useShinyjs(),
  
  # Popup Overlays
  div(
    id = "login-popup",
    class = "popup-overlay",
    div(
      class = "popup-content",
      span(class = "popup-close", HTML("&times;"), onclick = "document.getElementById('login-popup').style.display = 'none'"),
      div(
        class = "login-form",
        h2("Login"),
        textInput("login_username", "Username", placeholder = "Enter your username"),
        passwordInput("login_password", "Password", placeholder = "Enter your password"),
        actionButton("login_submit", "Login", class = "btn-login")
      )
    )
  ),
  
  div(
    id = "signup-popup",
    class = "popup-overlay",
    div(
      class = "popup-content",
      span(class = "popup-close", HTML("&times;"), onclick = "document.getElementById('signup-popup').style.display = 'none'"),
      div(
        class = "signup-form",
        h2("Sign Up"),
        textInput("register_username", "Username", placeholder = "Choose a username"),
        passwordInput("register_password", "Password", placeholder = "Create a password"),
        actionButton("register_submit", "Sign Up", class = "btn-signup")
      )
    )
  ),
  
  # Main page output
  uiOutput("main_page")
)

# Server Logic
server <- function(input, output, session) {
  # User Functions
  logged_in <- reactiveVal(FALSE)
  current_user <- reactiveVal(NULL)
  show_login <- reactiveVal(TRUE)
  
  load_users <- function() {
    read.csv(user_file, stringsAsFactors = FALSE)
  }
  
  save_user <- function(username, password) {
    users <- load_users()
    new_user <- data.frame(username = username, password = password)
    users <- rbind(users, new_user)
    write.csv(users, user_file, row.names = FALSE)
  }
  
  output$main_page <- renderUI({
    if (!logged_in()) {
      # Landing Page
      div(
        class = "landing-page",
        # Header
        div(
          class = "header",
          div(class = "logo", "AgriClimate Insights"),
          div(
            class = "nav",
            tags$a(href = "#", "Home"),
            tags$a(href = "#", "About Us"),
            tags$a(href = "#", "Pages")
          ),
          div(
            class = "auth-buttons",
            tags$a(href = "#", class = "login", onclick = "document.getElementById('login-popup').style.display = 'flex'", "Login"),
            tags$a(href = "#", class = "signup", onclick = "document.getElementById('signup-popup').style.display = 'flex'", "Sign up")
          )
        ),
        # Hero Section
        div(
          class = "hero",
          h1("Climate Change Impact On Agriculture"),
          p("Empowering farmers with data-driven insights for sustainable agriculture"),
          div(
            class = "btn-container",
            tags$a(href = "#", class = "get-started", "Get Started"),
            tags$a(href = "#", class = "learn-more", "Learn More")
          )
        ),
        # Statistics Section
        div(
          class = "statistics",
          div(
            class = "stat",
            h3("10K+"),
            p("Total Records")
          ),
          div(
            class = "stat",
            h3("30+"),
            p("Countries Covered")
          ),
          div(
            class = "stat",
            h3("20+"),
            p("Crop Types")
          ),
          div(
            class = "stat",
            h3("87+"),
            p("Regions Covered")
          )
        )
      )
    } else {
      dashboardPage(
        dashboardHeader(
          title = "AgriClimate Insights",
          # Add username display in header
          tags$li(
            class = "dropdown",
            style = "padding: 15px; color: black; position: absolute; right: 20px;",
            paste("Welcome,", current_user()),
            tags$style(HTML("
        .dropdown { 
          float: right !important; 
          padding-right: 15px !important;
        }
      "))
          )
        ),
      
        dashboardSidebar(
          sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
            menuItem("Analytics", tabName = "analytics", icon = icon("chart-bar")),
            menuItem("Prediction", tabName = "prediction", icon = icon("chart-line")),
            menuItem("3D Visualization", tabName = "3d_visualization", icon = icon("cube")),
            menuItem("About", tabName = "about", icon = icon("info-circle")),
            actionButton("logout", "Logout", icon = icon("sign-out"))
          )
        ),
        dashboardBody(
          tabItems(
            # Overview Section
            tabItem(tabName = "overview",
                    fluidRow(
                      valueBox("10000", "Total Records", icon = icon("database"), color = "green"),
                      valueBox("30 Countries", "Countries Covered", icon = icon("globe"), color = "blue"),
                      valueBox("20 Crops", "Crop Types", icon = icon("seedling"), color = "yellow")
                    ),
                    fluidRow(
                      box(title = "Climate Impact Score Distribution", plotlyOutput("impact_plot"), width = 6),
                      box(title = "Crop Yield Distribution", plotlyOutput("yield_plot"), width = 6)
                    )
            ),
            # Analytics Section
            tabItem(tabName = "analytics",
                    fluidRow(
                      box(title = "Bar Chart: Average Yield by Year", plotlyOutput("bar_chart"), width = 6),
                      box(title = "Line Chart: Climate Impact Over Time", plotlyOutput("line_chart"), width = 6)
                    ),
                    fluidRow(
                      box(title = "Heatmap: Temperature vs Precipitation", plotlyOutput("heatmap"), width = 12)
                    ),
                    fluidRow(
                      box(title = "Scatter Plot: Precipitation vs Yield", plotlyOutput("scatter_plot"), width = 6),
                      box(title = "Box Plot: Yield by Year", plotlyOutput("box_plot"), width = 6)
                    )
            ),
            # Prediction Section
            tabItem(tabName = "prediction",
                    fluidRow(
                      box(
                        title = "Predict Crop Yield",
                        sliderInput("pred_temp", "Average Temperature (°C)", min = min(processed_data$Average_Temperature_C), max = max(processed_data$Average_Temperature_C), value = mean(processed_data$Average_Temperature_C)),
                        sliderInput("pred_precip", "Total Precipitation (mm)", min = min(processed_data$Total_Precipitation_mm), max = max(processed_data$Total_Precipitation_mm), value = mean(processed_data$Total_Precipitation_mm)),
                        numericInput("pred_year", "Year", value = 2025, min = min(processed_data$Year), max = max(processed_data$Year)),
                        actionButton("predict_btn", "Predict"),
                        tags$div(style = "font-size: 18px; color: #123524; margin-top: 20px;", textOutput("prediction_result"))
                      ),
                      box(
                        title = "Prediction Visualization",
                        plotlyOutput("prediction_plot", height = "300px"),
                        width = 6
                      )
                    )
            ),
            # 3D Visualization Section
            tabItem(tabName = "3d_visualization",
                    fluidRow(
                      box(title = "3D Scatter Plot", plotlyOutput("scatter3d_plot", height = "400px"), width = 6)
                    ),
                    fluidRow(
                      box(title = "3D Heatmap", plotlyOutput("heatmap3d_plot", height = "400px"), width = 12)
                    )
            ),
            # About Section
            tabItem(tabName = "about",
                    fluidRow(
                      box(title = "About the Dataset and Climate Impact", width = 12, 
                          tags$p(style = "font-size: 16px; color: #123524;",
                                 "This dataset contains records of agricultural yield influenced by climate factors such as average temperature and total precipitation. 
                                 The climate impact score combines these factors to gauge the potential effect of climate conditions on crop yield. 
                                 Our aim is to provide insights into how changing climatic patterns affect agricultural productivity and to assist in informed decision-making for sustainable farming practices."
                          )
                      )
                    )
            )
          )
        )
      )
    }
  })
  
  
  # Login Logic
  observeEvent(input$login_submit, {
    users <- load_users()
    if (nrow(users[users$username == input$login_username & users$password == input$login_password, ]) > 0) {
      logged_in(TRUE)
      current_user(input$login_username)
      showNotification("Login Successful!", type = "message")
      runjs("document.getElementById('login-popup').style.display = 'none';")
    } else {
      showNotification("Invalid Credentials!", type = "error")
    }
  })
  
  # Registration Logic
  observeEvent(input$register_submit, {
    users <- load_users()
    if (input$register_username %in% users$username) {
      showNotification("Username already exists!", type = "error")
    } else {
      save_user(input$register_username, input$register_password)
      showNotification("Registration successful!", type = "message")
      runjs("document.getElementById('signup-popup').style.display = 'none';")
    }
  })
  
  # Logout Logic
  observeEvent(input$logout, {
    logged_in(FALSE)
    current_user(NULL)
  })
  
  # Color Palette
  color_palette <- c(
    "#1E90FF",   # Dodger Blue
    "#2ECC71",   # Emerald Green
    "#E74C3C",   # Bright Red
    "#F39C12",   # Orange
    "#9B59B6",   # Amethyst Purple
    "#3498DB",   # Peter River Blue
    "#1ABC9C"    # Turquoise
  )
  
  # Add these plotting functions to your server logic
  
  # Impact Plot
  output$impact_plot <- renderPlotly({
    plot_ly(processed_data, 
            x = ~Climate_Impact_Score, 
            type = "histogram", 
            marker = list(color = color_palette[1], line = list(color = color_palette[4], width = 1.5))
    ) %>%
      layout(
        title = "Climate Impact Score Distribution",
        xaxis = list(title = "Climate Impact Score"),
        yaxis = list(title = "Frequency"),
        plot_bgcolor = "#F0F0F0",
        paper_bgcolor = "#FFFFFF"
      )
  })
  
  # Yield Plot
  output$yield_plot <- renderPlotly({
    plot_ly(processed_data, 
            x = ~Crop_Yield_MT_per_HA, 
            type = "histogram", 
            marker = list(color = color_palette[2], line = list(color = color_palette[5], width = 1.5))
    ) %>%
      layout(
        title = "Crop Yield Distribution",
        xaxis = list(title = "Crop Yield (MT/HA)"),
        yaxis = list(title = "Frequency"),
        plot_bgcolor = "#F0F0F0",
        paper_bgcolor = "#FFFFFF"
      )
  })
  
  # Bar Chart: Average Yield by Year
  output$bar_chart <- renderPlotly({
    yearly_avg <- processed_data %>%
      group_by(Year) %>%
      summarise(Avg_Yield = mean(Crop_Yield_MT_per_HA))
    
    plot_ly(yearly_avg, 
            x = ~Year, 
            y = ~Avg_Yield, 
            type = "bar", 
            marker = list(color = color_palette[3], line = list(color = color_palette[6], width = 1.5))
    ) %>%
      layout(
        title = "Average Yield by Year",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Average Crop Yield (MT/HA)"),
        plot_bgcolor = "#F0F0F0",
        paper_bgcolor = "#FFFFFF"
      )
  })
  
  # Line Chart: Climate Impact Over Time
  output$line_chart <- renderPlotly({
    yearly_impact <- processed_data %>%
      group_by(Year) %>%
      summarise(Avg_Climate_Impact = mean(Climate_Impact_Score))
    
    plot_ly(yearly_impact, 
            x = ~Year, 
            y = ~Avg_Climate_Impact, 
            type = "scatter", 
            mode = "lines+markers",
            line = list(color = color_palette[4], width = 3),
            marker = list(color = color_palette[5], size = 10)
    ) %>%
      layout(
        title = "Climate Impact Score Over Time",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Average Climate Impact Score"),
        plot_bgcolor = "#F0F0F0",
        paper_bgcolor = "#FFFFFF"
      )
  })
  
  # Heatmap: Temperature vs Precipitation
  output$heatmap <- renderPlotly({
    plot_ly(processed_data, 
            x = ~Average_Temperature_C, 
            y = ~Total_Precipitation_mm, 
            z = ~Crop_Yield_MT_per_HA, 
            type = "heatmap", 
            colors = colorRamp(c(color_palette[1], color_palette[2], color_palette[3]))
    ) %>%
      layout(
        title = "Heatmap: Temperature vs Precipitation",
        xaxis = list(title = "Average Temperature (°C)"),
        yaxis = list(title = "Total Precipitation (mm)"),
        plot_bgcolor = "#F0F0F0",
        paper_bgcolor = "#FFFFFF"
      )
  })
  
  # Scatter Plot: Precipitation vs Yield
  output$scatter_plot <- renderPlotly({
    plot_ly(processed_data, 
            x = ~Total_Precipitation_mm, 
            y = ~Crop_Yield_MT_per_HA, 
            type = "scatter", 
            mode = "markers",
            marker = list(
              color = ~Total_Precipitation_mm, 
              colorscale = list(
                list(0, color_palette[1]),
                list(0.5, color_palette[4]),
                list(1, color_palette[3])
              ), 
              size = 10,
              opacity = 0.7
            )
    ) %>%
      layout(
        title = "Precipitation vs Crop Yield",
        xaxis = list(title = "Total Precipitation (mm)"),
        yaxis = list(title = "Crop Yield (MT/HA)"),
        plot_bgcolor = "#F0F0F0",
        paper_bgcolor = "#FFFFFF"
      )
  })
  
  # Box Plot: Yield by Year
  output$box_plot <- renderPlotly({
    plot_ly(processed_data, 
            x = ~as.factor(Year), 
            y = ~Crop_Yield_MT_per_HA, 
            type = "box",
            marker = list(color = color_palette[6])
    ) %>%
      layout(
        title = "Crop Yield Distribution by Year",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Crop Yield (MT/HA)"),
        plot_bgcolor = "#F0F0F0",
        paper_bgcolor = "#FFFFFF"
      )
  })
  
  # 3D Scatter Plot
  output$scatter3d_plot <- renderPlotly({
    plot_ly(processed_data, 
            x = ~Average_Temperature_C, 
            y = ~Total_Precipitation_mm, 
            z = ~Crop_Yield_MT_per_HA, 
            type = "scatter3d", 
            mode = "markers",
            marker = list(
              color = ~Crop_Yield_MT_per_HA, 
              colorscale = list(
                list(0, color_palette[1]),
                list(0.5, color_palette[4]),
                list(1, color_palette[3])
              ), 
              size = 5,
              opacity = 0.7
            )
    ) %>%
      layout(
        title = "3D Scatter: Temperature, Precipitation, and Yield",
        scene = list(
          xaxis = list(title = "Avg Temperature (°C)"),
          yaxis = list(title = "Total Precipitation (mm)"),
          zaxis = list(title = "Crop Yield (MT/HA)")
        ),
        plot_bgcolor = "#F0F0F0",
        paper_bgcolor = "#FFFFFF"
      )
  })
  
  # 3D Heatmap
  output$heatmap3d_plot <- renderPlotly({
    plot_ly(processed_data, 
            x = ~Average_Temperature_C, 
            y = ~Total_Precipitation_mm, 
            z = ~Year,
            type = "heatmap", 
            colors = colorRamp(c(color_palette[2], color_palette[5], color_palette[7]))
    ) %>%
      layout(
        title = "3D Heatmap: Temperature, Precipitation, and Year",
        xaxis = list(title = "Avg Temperature (°C)"),
        yaxis = list(title = "Total Precipitation (mm)"),
        plot_bgcolor = "#F0F0F0",
        paper_bgcolor = "#FFFFFF"
      )
  })
  
  # Prediction Plot
  output$prediction_plot <- renderPlotly({
    req(input$predict_btn)
    
    # Prepare prediction input
    new_data <- data.frame(
      Average_Temperature_C = input$pred_temp,
      Total_Precipitation_mm = input$pred_precip,
      Year = input$pred_year
    )
    
    # Make prediction
    predicted_yield <- predict(ml_model, new_data)
    
    # Create comparison plot
    plot_ly() %>%
      add_trace(
        x = c("Actual Range", "Predicted"),
        y = c(mean(processed_data$Crop_Yield_MT_per_HA), predicted_yield),
        type = "bar",
        marker = list(
          color = c(color_palette[4], color_palette[3]),
          line = list(color = color_palette[6], width = 1.5)
        )
      ) %>%
      layout(
        title = "Crop Yield Prediction",
        xaxis = list(title = ""),
        yaxis = list(title = "Crop Yield (MT/HA)"),
        plot_bgcolor = "#F0F0F0",
        paper_bgcolor = "#FFFFFF"
      )
  })
  # Prediction Logic
  observeEvent(input$predict_btn, {
    new_data <- data.frame(
      Average_Temperature_C = input$pred_temp, 
      Total_Precipitation_mm = input$pred_precip, 
      Year = input$pred_year
    )
    pred <- predict(ml_model, new_data)
    
    output$prediction_result <- renderText({ 
      paste("Predicted Crop Yield:", round(pred, 2), "MT/HA") 
    })
    
    output$prediction_plot <- renderPlotly({
      plot_ly(
        x = c("Input Data", "Predicted Crop Yield"), 
        y = c(0, pred), 
        type = "bar", 
        name = "Crop Yield Prediction"
      ) %>%
        layout(title = "Crop Yield Prediction", 
               xaxis = list(title = ""), 
               yaxis = list(title = "Crop Yield (MT/HA)"))
    })
  })
}
# Run App
shinyApp(ui = ui, server = server)