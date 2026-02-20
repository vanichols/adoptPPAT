library(shiny)
library(rhandsontable)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(ggnewscale)
library(ggrepel)
library(scales)
library(patchwork)
library(bslib)

# global ------------------------------------------------------------------

data_betas <- read_rds("data/processed/data_betas.RDS")
data_example <- read_rds("data/processed/data_example.RDS")

# Source utility functions (ridge plots, overlapping ridge plots)
source("R/utils.R")

ui <- page_fluid(
  titlePanel("ADOPT-IPM online P-PAT tool"),
  
  navset_tab(
    nav_panel(
      "Welcome",
      card(
        card_header("Welcome to the online version of the P-PAT tool"),
        card_body(
          h3("Getting Started"),
          p("This application allows you to enter ratings for different metrics and view visualizations of your ratings data."),
          tags$ul(
            tags$li("Navigate to the 'Ratings' tab to enter your data"),
            tags$li("Fill in both tables with your ratings"),
            tags$li("Click one of the summary buttons to see the visualization"),
            tags$li("You can download your visualization as a PDF")
          )
        )
      )
    ),
    
    nav_panel(
      "Ratings",
      
      # First row: Two boxes with hot tables
      fluidRow(
        column(
          width = 6,
          card(
            card_header("Baseline Ratings"),
            card_body(
              rHandsontableOutput("hotTable1")
            )
          )
        ),
        column(
          width = 6,
          card(
            card_header("New Approach Ratings"),
            card_body(
              rHandsontableOutput("hotTable2")
            )
          )
        )
      ),
      
      # Button row
      fluidRow(
        column(
          width = 12,
          div(
            style = "text-align: center; margin-top: 20px; margin-bottom: 20px;",
            uiOutput("summaryButtons")
          )
        )
      ),
      
      # Second row: One big box with a figure (conditionally displayed)
      conditionalPanel(
        condition = "input.showSideBySide > 0 || input.showOverlapping > 0",
        fluidRow(
          column(
            width = 12,
            card(
              card_header(
                "Visualization",
                class = "d-flex justify-content-between align-items-center",
                div(
                  downloadButton("downloadPlot", "Download PDF", class = "btn-sm")
                )
              ),
              card_body(
                plotOutput("figure", height = "400px")
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Hot table 1 with fixed metrics and dropdown columns
  output$hotTable1 <- renderRHandsontable({
    df1 <- data.frame(
      Metric = c("Crop value", "Costs", "Time/management complexity", 
                 "Immediate usability", "Environmental impact", "User health and safety"),
      #Weight = c(50, 12.5, 6.25, 6.25, 12.5, 12.5),
      Rating = c(NA, NA, NA, NA, NA, NA),
      Confidence = c(NA, NA, NA, NA, NA, NA),
      stringsAsFactors = FALSE
    )
    
    rhandsontable(df1, height = 300) %>%
      hot_col("Metric", readOnly = TRUE) %>%
      #hot_col("Weight", readOnly = TRUE, format = "0.00") %>%
      hot_col("Rating", type = "dropdown", 
              source = c("5 - Highly acceptable", "4 - Acceptable", "3 - Is a consideration", 
                         "2 - Disuaded", "1 - Unacceptable")) %>%
      hot_col("Confidence", type = "dropdown", 
              source = c("Very high", "High", "Medium", "Low"))
  })
  
  # Hot table 2 with fixed metrics and dropdown columns
  output$hotTable2 <- renderRHandsontable({
    df2 <- data.frame(
      Metric = c("Crop value", "Costs", "Time/management complexity", 
                 "Immediate usability", "Environmental impact", "User health and safety"),
      #Weight = c(50, 12.5, 6.25, 6.25, 12.5, 12.5),
      Rating = c(NA, NA, NA, NA, NA, NA),
      Confidence = c(NA, NA, NA, NA, NA, NA),
      stringsAsFactors = FALSE
    )
    
    rhandsontable(df2, height = 300) %>%
      hot_col("Metric", readOnly = TRUE) %>%
      #hot_col("Weight", readOnly = TRUE, format = "0.00") %>%
      hot_col("Rating", type = "dropdown", 
              source = c("5 - Highly acceptable", "4 - Acceptable", "3 - Is a consideration", 
                         "2 - Disuaded", "1 - Unacceptable")) %>%
      hot_col("Confidence", type = "dropdown", 
              source = c("Very high", "High", "Medium", "Low"))
  })
  
  # Check if all required cells are filled
  all_data_complete <- reactive({
    req(input$hotTable1, input$hotTable2)
    
    table1_data <- hot_to_r(input$hotTable1)
    table2_data <- hot_to_r(input$hotTable2)
    
    # Check if Rating and Confidence columns have no NA values
    table1_complete <- !any(is.na(table1_data$Rating)) && !any(is.na(table1_data$Confidence))
    table2_complete <- !any(is.na(table2_data$Rating)) && !any(is.na(table2_data$Confidence))
    
    return(table1_complete && table2_complete)
  })
  
  # Render buttons conditionally based on data completeness
  output$summaryButtons <- renderUI({
    if (all_data_complete()) {
      tagList(
        actionButton("showSideBySide", "Make side by side summary", class = "btn-primary btn-lg"),
        tags$span(style = "display: inline-block; width: 20px;"),
        actionButton("showOverlapping", "Make overlapping summary", class = "btn-primary btn-lg")
      )
    } else {
      tags$div(
        actionButton("showSideBySide", "Make side by side summary", class = "btn-secondary btn-lg"),
        tags$span(style = "display: inline-block; width: 20px;"),
        actionButton("showOverlapping", "Make overlapping summary", class = "btn-secondary btn-lg"),
        tags$p("Please fill in all cells in both tables to enable the summaries.", 
               style = "color: #6c757d; margin-top: 10px;")
      )
    }
  })
  
  # Reactive value to track which button was clicked
  plot_type <- reactiveVal("none")
  
  # Observe side by side button
  observeEvent(input$showSideBySide, {
    req(all_data_complete())
    plot_type("side_by_side")
  })
  
  # Observe overlapping button
  observeEvent(input$showOverlapping, {
    req(all_data_complete())
    plot_type("overlapping")
  })
  
  # Prepare data for plotting (shared by renderPlot and downloadHandler)
  prepared_data <- reactive({
    req(plot_type() != "none")
    
    # Get data from both hot tables
    table1_data <- hot_to_r(input$hotTable1)
    table2_data <- hot_to_r(input$hotTable2)
    
    bind_rows(table1_data, table2_data) |> 
      mutate(title = c(rep("Baseline", 6), rep("New approach", 6))) |> 
      separate(col = Rating, into = c("rating_1to5", "rating_text"), sep = "-") |> 
      mutate(rating_numeric = parse_number(rating_1to5),
             confidence = case_when(
               Confidence == "Very high" ~ "vh",
               Confidence == "High" ~ "h",
               Confidence == "Medium" ~ "m",
               Confidence == "Low" ~ "l")
      ) |> 
      rename(metric = Metric
             #weight = Weight
             ) |> 
      #mutate(weight = as.numeric(weight)) |> 
      select(title, metric, 
             #weight, 
             rating_numeric, confidence) |> 
      as_tibble()
  })
  
  # Function to generate the plot (shared by renderPlot and downloadHandler)
  generate_plot <- function() {
    all_data <- prepared_data()
    
    if (plot_type() == "side_by_side") {
      fxn_Make_Paired_Ridge_Plots(data = all_data, betas = data_betas)
    } else if (plot_type() == "overlapping") {
      fxn_Make_Overlapping_Ridge_Plots(data = all_data, betas = data_betas)
    }
  }
  
  # Figure that appears after clicking one of the buttons
  output$figure <- renderPlot({
    generate_plot()
  })
  
  # Download handler for PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("figure_", plot_type(), "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Save the plot as PDF
      pdf(file, width = 10, height = 6)
      print(generate_plot())
      dev.off()
    }
  )
}

shinyApp(ui, server)
