install.packages(c("shiny", "readxl", "plotly", "bslib", "zoo", "dplyr"))

library(shiny)
library(readxl)
library(plotly)
library(bslib)
library(zoo)
library(dplyr)

# Load synthesized dataset
file_path <- "oceanographic_data.xlsx"
data <- read_excel(file_path)

# UI
ui <- fluidPage( title = "Oceanpgraphic dashboard",
                 theme = bs_theme(bootswatch = "minty", base_font = font_google("Roboto")),
                 
                 tags$head(
                   tags$style(HTML("
      body {
        background-color: #e6f7ff;
        font-size: 18px;
        font-family: 'Roboto', sans-serif;
      }
      h2, h4, label, .control-label {
        font-weight: bold;
        color: #004d80;
      }
      .shiny-input-container {
        font-size: 18px;
      }
      .table {
        font-size: 16px;
      }
    "))
                 ),
                 
                 titlePanel(
                   tags$h2("Oceanographic Monitoring Dashboard", style = "text-align: center; color: #003366; padding-top: 10px;")
                 ),
                 
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("selected_page", "Choose a Page:",
                                 choices = c("Ocean Conditions", "Wave Analysis", "Water Quality", "Disaster Monitoring", "Statistical Analysis")),
                     
                     hr(),
                     h4("Status"),
                     htmlOutput("alerts"),
                     
                     hr(),
                     h4("Summary & Insights"),
                     uiOutput("summary_insights"),
                     
                     hr(),
                     h4("Live Alert Log"),
                     tableOutput("log_table"),
                     downloadButton("download_log", "Download Log"),
                     
                     width = 3
                   ),
                   
                   mainPanel(
                     uiOutput("page_content")
                   )
                 )
)

# Server
server <- function(input, output, session) {
  current_row <- reactiveVal(1)
  empty_df <- data[0, ]
  empty_df$Wave_Height_MA10 <- numeric(0)
  empty_df$Wave_Height_Z <- numeric(0)
  empty_df$Temp_MA10 <- numeric(0)
  accumulated_data <- reactiveVal(empty_df)
  log_data <- reactiveVal(data.frame(Time = character(), Message = character(), stringsAsFactors = FALSE))
  
  auto_update <- reactiveTimer(5000)
  
  observeEvent(auto_update(), {
    isolate({
      if (current_row() <= nrow(data)) {
        new_row <- data[current_row(), ]
        updated_data <- bind_rows(accumulated_data(), new_row)
        
        # Keep only the most recent 500 rows to prevent overload
        if (nrow(updated_data) > 500) {
          updated_data <- tail(updated_data, 500)
        }
        
        if (nrow(updated_data) >= 10) {
          updated_data$Wave_Height_MA10 <- rollmean(updated_data$Wave_Height_m, 10, fill = NA, align = "right")
          updated_data$Wave_Height_Z <- scale(updated_data$Wave_Height_m)
          updated_data$Temp_MA10 <- rollmean(updated_data$Sea_Surface_Temperature_C, 10, fill = NA, align = "right")
        }
        
        accumulated_data(updated_data)
        
        alert_msgs <- c()
        if (!is.na(new_row$Wave_Height_m) && new_row$Wave_Height_m > 2.5)
          alert_msgs <- c(alert_msgs, "High Wave Alert")
        if (!is.na(new_row$Dissolved_Oxygen_mgL) && new_row$Dissolved_Oxygen_mgL < 3)
          alert_msgs <- c(alert_msgs, "Low Dissolved Oxygen")
        
        if (length(alert_msgs) > 0) {
          new_log <- data.frame(Time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), Message = paste(alert_msgs, collapse = " | "))
          log_data(bind_rows(log_data(), new_log))
        }
        
        current_row(current_row() + 1)
      } else {
        current_row(1)
        accumulated_data(data[0, ])
        log_data(data.frame(Time = character(), Message = character(), stringsAsFactors = FALSE))
      }
    })
  })
  
  # Remainder of server logic unchanged
  
  output$alerts <- renderUI({
    latest <- tail(accumulated_data(), 1)
    alerts <- c()
    
    if (nrow(latest) > 0) {
      if (!is.na(latest$Wave_Height_m) && latest$Wave_Height_m > 2.5)
        alerts <- c(alerts, "High Wave Alert")
      if (!is.na(latest$Dissolved_Oxygen_mgL) && latest$Dissolved_Oxygen_mgL < 3)
        alerts <- c(alerts, "Low Dissolved Oxygen")
    }
    
    if (length(alerts) == 0) {
      HTML("<b>Status:</b> All parameters normal")
    } else {
      HTML(paste("<b>Status:</b><br>", paste(alerts, collapse = "<br>")))
    }
  })
  
  output$summary_insights <- renderUI({
    recent <- tail(accumulated_data(), 10)
    if (nrow(recent) == 0) return("No data yet.")
    
    avg_wave <- round(mean(recent$Wave_Height_m, na.rm = TRUE), 2)
    avg_temp <- round(mean(recent$Sea_Surface_Temperature_C, na.rm = TRUE), 2)
    
    wave_lm <- lm(Wave_Height_m ~ seq_along(Wave_Height_m), data = recent)
    slope <- coef(wave_lm)[2]
    trend <- ifelse(slope > 0.01, "Rising", ifelse(slope < -0.01, "Falling", "Stable"))
    
    correlation <- tryCatch({
      round(cor(recent$Surface_Current_Speed_mps, recent$Wave_Height_m, use = "complete.obs"), 2)
    }, error = function(e) NA)
    
    HTML(paste0(
      "<ul>",
      "<li><b>Avg. Wave Height:</b> ", avg_wave, " m</li>",
      "<li><b>Wave Height Trend:</b> ", trend, "</li>",
      "<li><b>Avg. Sea Temperature:</b> ", avg_temp, " °C</li>",
      "<li><b>Wave-Current Correlation:</b> ", ifelse(is.na(correlation), "N/A", correlation), "</li>",
      "</ul>"
    ))
  })
  
  output$log_table <- renderTable({
    log_data()
  })
  
  output$download_log <- downloadHandler(
    filename = function() { paste0("ocean_alert_log_", Sys.Date(), ".csv") },
    content = function(file) {
      write.csv(log_data(), file, row.names = FALSE)
    }
  )
  
  output$page_content <- renderUI({
    switch(input$selected_page,
           "Ocean Conditions" = fluidRow(
             column(6, plotlyOutput("tidal_height_line")),
             column(6, plotlyOutput("current_scatter")),
             column(12, plotlyOutput("current_direction_polar"))
           ),
           "Wave Analysis" = fluidRow(
             column(6, plotlyOutput("wave_height_line")),
             column(6, plotlyOutput("wave_energy_bar")),
             column(12, plotlyOutput("wave_period_histogram"))
           ),
           "Water Quality" = fluidRow(
             column(6, plotlyOutput("temperature_heatmap")),
             column(6, plotlyOutput("salinity_gauge")),
             column(6, plotlyOutput("oxygen_gauge")),
             column(6, plotlyOutput("chlorophyll_boxplot"))
           ),
           "Disaster Monitoring" = fluidRow(
             column(6, plotlyOutput("tsunami_line")),
             column(6, textOutput("cyclone_impact"))
           ),
           "Statistical Analysis" = fluidRow(
             column(12, h4("Moving Average of Wave Height")),
             column(12, plotlyOutput("wave_moving_avg")),
             column(12, br(), h4("Trend Analysis")),
             column(12, textOutput("trend_text")),
             column(12, br(), h4("Current-Wave Correlation")),
             column(12, textOutput("correlation_text")),
             column(12, br(), h4("Z-Score Anomalies (Wave Height)")),
             column(12, tableOutput("zscore_table"))
           )
    )
  })
  
  output$tidal_height_line <- renderPlotly({
    plot_ly(accumulated_data(), x = ~Time_s, y = ~Tidal_Height_m, type = 'scatter', mode = 'lines') %>%
      layout(title = "Tidal Height Trend")
  })
  
  output$current_scatter <- renderPlotly({
    plot_ly(accumulated_data(), x = ~Surface_Current_Speed_mps, y = ~Surface_Current_Direction_deg, mode = 'markers') %>%
      layout(title = "Surface Current Speed vs. Direction")
  })
  
  output$current_direction_polar <- renderPlotly({
    plot_ly(accumulated_data(), r = ~Surface_Current_Speed_mps, theta = ~Surface_Current_Direction_deg, type = 'scatterpolar') %>%
      layout(title = "Surface Current Direction")
  })
  
  output$wave_height_line <- renderPlotly({
    plot_ly(accumulated_data(), x = ~Time_s, y = ~Wave_Height_m, type = 'scatter', mode = 'lines') %>%
      layout(title = "Wave Height Over Time")
  })
  
  output$wave_energy_bar <- renderPlotly({
    plot_ly(accumulated_data(), x = ~Time_s, y = ~Wave_Energy_kWpm, type = 'scatter', mode = 'lines') %>%
      layout(title = "Wave Energy Over Time")
  })
  
  output$wave_period_histogram <- renderPlotly({
    plot_ly(accumulated_data(), x = ~Time_s, y = ~Wave_Period_sec, type = 'scatter', mode = 'lines') %>%
      layout(title = "Wave Period Trend")
  })
  
  output$temperature_heatmap <- renderPlotly({
    plot_ly(accumulated_data(), x = ~Time_s, y = ~Sea_Surface_Temperature_C, type = 'scatter', mode = 'lines') %>%
      layout(title = "Sea Surface Temperature")
  })
  
  output$salinity_gauge <- renderPlotly({
    plot_ly(type = "indicator", mode = "gauge+number", value = tail(accumulated_data()$Salinity_PSU, 1), 
            title = list(text = "Salinity (PSU)"), gauge = list(axis = list(range = c(30, 40))))
  })
  
  output$oxygen_gauge <- renderPlotly({
    plot_ly(type = "indicator", mode = "gauge+number", value = tail(accumulated_data()$Dissolved_Oxygen_mgL, 1), 
            title = list(text = "Dissolved Oxygen (mg/L)"), gauge = list(axis = list(range = c(0, 10))))
  })
  
  output$chlorophyll_boxplot <- renderPlotly({
    plot_ly(accumulated_data(), x = ~Time_s, y = ~Chlorophyll_Concentration_mg_m3, type = 'scatter', mode = 'lines') %>%
      layout(title = "Chlorophyll Concentration Trend")
  })
  
  output$tsunami_line <- renderPlotly({
    plot_ly(accumulated_data(), x = ~Time_s, y = ~Tsunami_Wave_Propagation_mps, type = 'scatter', mode = 'lines') %>%
      layout(title = "Tsunami Wave Propagation")
  })
  
  output$cyclone_impact <- renderText({
    paste("Cyclone Impact:", tail(accumulated_data()$Hurricane_Cyclone_Impact, 1))
  })
  
  output$wave_moving_avg <- renderPlotly({
    plot_ly(accumulated_data(), x = ~Time_s) %>%
      add_lines(y = ~Wave_Height_m, name = "Raw", line = list(color = 'lightblue')) %>%
      add_lines(y = ~Wave_Height_MA10, name = "MA(10)", line = list(color = 'darkblue')) %>%
      layout(title = "Wave Height & Moving Average")
  })
  
  output$trend_text <- renderText({
    recent <- tail(accumulated_data(), 10)
    if (nrow(recent) < 3) return("Insufficient data.")
    slope <- coef(lm(Wave_Height_m ~ seq_along(Wave_Height_m), data = recent))[2]
    trend <- ifelse(slope > 0.01, "increasing", ifelse(slope < -0.01, "decreasing", "stable"))
    paste("Based on a linear model of the last 10 readings, wave height is", trend, "with a slope of", round(slope, 4))
  })
  
  output$correlation_text <- renderText({
    df <- tail(accumulated_data(), 20)
    if (nrow(df) < 5) return("Not enough data.")
    r <- tryCatch({
      round(cor(df$Surface_Current_Speed_mps, df$Wave_Height_m, use = "complete.obs"), 2)
    }, error = function(e) NA)
    if (is.na(r)) return("Correlation not computable.")
    paste("Pearson correlation between surface current speed and wave height (last 20 readings):", r)
  })
  
  output$zscore_table <- renderTable({
    df <- accumulated_data()
    if (!"Wave_Height_Z" %in% names(df)) return(NULL)
    outliers <- df[abs(df$Wave_Height_Z) > 2, c("Time_s", "Wave_Height_m", "Wave_Height_Z")]
    names(outliers) <- c("Time", "Wave Height", "Z-Score")
    outliers
  })
}

shinyApp(ui = ui, server = server)
