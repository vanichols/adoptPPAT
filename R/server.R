# server.R


#### Server ####################################################################

server <- function(input, output, session) {
  
  # Single substance tab =======================================================
  
  #--Populate filter lists (runs once at app startup)
  
  
  observeEvent(TRUE, {
    # Substance category filter
    updateSelectInput(session,
                      "substance_category",
                      choices = unique(data_hpli$compound_category) |>
                        sort())
    # Substance origin filter
    updateSelectInput(session,
                      "substance_origins",
                      choices = unique(data_hpli$compound_origin) |>
                        sort())
  }, once = TRUE)
  
  
  
  ###### Populate list of substance (reacts on filters) ######
  #--data for second tab, 1st choice
  substance_choices <- reactive({
    data_hpli_filtered <- data_hpli
    
    # Filter by origin only if an origin is selected
    if (!is.null(input$substance_origins) &&
        length(input$substance_origins) > 0) {
      data_hpli_filtered <-
        data_hpli_filtered |>
        dplyr::filter(compound_origin %in% input$substance_origins)
    }
    
    # Filter by category only if a category is selected
    if (!is.null(input$substance_category) &&
        length(input$substance_category) > 0) {
      data_hpli_filtered <-
        data_hpli_filtered |>
        dplyr::filter(compound_category %in% input$substance_category)
    }
    
    
    # Format final substance list
    data_hpli_filtered |>
      dplyr::pull(compound) |>
      unique() |>
      sort()
  })
  
  ###### Selected substance based on user choice ######
  observe({
    choices <- substance_choices()
    selected <- isolate(input$substance_single)
    if (!is.null(selected))
      selected <- selected[selected %in% choices]
    updateSelectInput(session,
                      "substance_single",
                      choices = choices,
                      selected = selected)
    updateSelectInput(session, "substances_compare", choices = choices)
  })
  
  # If current selection is no longer valid (e.g. after a new filter is applied), clear it
  observe({
    valid_choices <- substance_choices()
    current <- input$substance_single
    if (!is.null(current) && !current %in% valid_choices) {
      updateSelectInput(session, "substance_single", selected = "")
      #updateSelectInput(session, "substances_compare", selected = "")
    }
  })
  
  ###### Reduce data based on selected substance ######
  single_substance_data <- reactive({
    req(input$substance_single)
    data_hpli <- data_hpli
    data_hpli[data_hpli$compound == input$substance_single, ]
  })
  
  ###### Display substance data ######
  output$substance_info <- renderText({
    # Make it reactive to both inputs
    choices <- substance_choices()
    selected <- input$substance_single
    # Clear out if nothing selected or selection invalid
    if (is.null(selected) ||
        selected == "" || !selected %in% choices) {
      return("")
    }
    # Normal case (if a substance is selected)
    data_sub <- single_substance_data()
    if (nrow(data_sub) > 0) {
      paste0(
        "Substance: ",
        input$substance_single,
        "\n\n",
        "      CAS: ",
        unique(data_sub$cas),
        "\n",
        " Category: ",
        unique(data_sub$compound_type),
        "\n",
        "   Origin: ",
        unique(data_sub$compound_origin),
        "\n",
        #" Sub type: ", unique(data_sub$sub_compound_category), "\n",
        "   Family: ",
        unique(data_sub$compound_group),
        "\n\n",
        "     Load: ",
        round(unique(data_sub$load_score), 3)
      )
    }
  })
  
  ###### Display load visualization as rose plot ######
  output$rose_plot <- renderPlot({
    req(input$substance_single)
    fxn_Make_Rose_Plot(compound_name = input$substance_single,
                       data = data_hpli)
  })
  
  ###### Display load on distribution ######
  output$dist_plot <- renderPlot({
    req(input$substance_single)
    fxn_Make_Distribution_Plot(compound_names = input$substance_single,
                               data = data_hpli)
  })
  ###### Download data option ######
  output$download_data <- downloadHandler(
    filename = function() {
      req(input$substance_single)
      paste0(
        "load_score_details_",
        gsub("[^A-Za-z0-9]", "_", input$substance_single),
        "_",
        Sys.Date(),
        ".tsv"
      )
    },
    content = function(file) {
      req(input$substance_single)
      data_sub <- single_substance_data()
      display_data <-
        data_sub |>
        dplyr::mutate_if(is.numeric, round, 3) |>
        dplyr::select(
          compound,
          compound_type,
          env_raw,
          eco.terr_raw,
          eco.aqua_raw,
          hum_raw,
          load_score,
          missing_share
        )
      
      write.table(
        display_data,
        file,
        sep = "\t",
        row.names = FALSE,
        col.names = TRUE,
        quote = FALSE
      )
    }
  )
  
  # Compare substances tab =====================================================
  
  ###### Populate filter lists (runs once at app startup) ######
  
  observeEvent(TRUE, {
    # Substance category filter
    updateSelectInput(session,
                      "substance_category1",
                      choices = unique(data_hpli$compound_category) |>
                        sort())
    # Substance origin filter
    updateSelectInput(session,
                      "substance_origins1",
                      choices = unique(data_hpli$compound_origin) |>
                        sort())
    
  }, once = TRUE)
  
  observeEvent(TRUE, {
    # Substance type filter
    updateSelectInput(session,
                      "substance_category2",
                      choices = unique(data_hpli$compound_category) |>
                        sort())
    # Substance origin filter
    updateSelectInput(session,
                      "substance_origins2",
                      choices = unique(data_hpli$compound_origin) |>
                        sort())
    
  }, once = TRUE)
  
  
  
  ###### Populate list of substance (reacts on filters) ######
  #--data for second tab, 1st choice
  substance_choices1 <- reactive({
    data_hpli_filtered1 <- data_hpli
    
    # Filter by origin only if an origin is selected
    if (!is.null(input$substance_origins1) &&
        length(input$substance_origins1) > 0) {
      data_hpli_filtered1 <-
        data_hpli_filtered1 |>
        dplyr::filter(compound_origin %in% input$substance_origins1)
    }
    
    # Filter by type only if a category is selected
    if (!is.null(input$substance_category1) &&
        length(input$substance_category1) > 0) {
      data_hpli_filtered1 <-
        data_hpli_filtered1 |>
        dplyr::filter(compound_category %in% input$substance_category1)
    }
    
    
    
    # Format final substance list
    data_hpli_filtered1 |>
      dplyr::pull(compound) |>
      unique() |>
      sort()
  })
  
  #--data for second tab, 2nd choice
  substance_choices2 <- reactive({
    data_hpli_filtered2 <- data_hpli
    
    # Filter by origin only if an origin is selected
    if (!is.null(input$substance_origins2) &&
        length(input$substance_origins2) > 0) {
      data_hpli_filtered2 <-
        data_hpli_filtered2 |>
        dplyr::filter(compound_origin %in% input$substance_origins2)
    }
    
    # Filter by type only if a category is selected
    if (!is.null(input$substance_category2) &&
        length(input$substance_category2) > 0) {
      data_hpli_filtered2 <-
        data_hpli_filtered2 |>
        dplyr::filter(compound_category %in% input$substance_category2)
    }
    
    
    
    # Format final substance list
    data_hpli_filtered2 |>
      dplyr::pull(compound) |>
      unique() |>
      sort()
  })
  
  ###### Selected substance1 based on user choice ######
  observe({
    choices1 <- substance_choices1()
    selected1 <- isolate(input$substance_double1)
    if (!is.null(selected1))
      selected1 <- selected1[selected1 %in% choices1]
    updateSelectInput(session,
                      "substance_double1",
                      choices = choices1,
                      selected = selected1)
  })
  
  # If current selection is no longer valid (e.g. after a new filter is applied), clear it
  observe({
    valid_choices <- substance_choices1()
    current <- input$substance_double1
    if (!is.null(current) && !current %in% valid_choices) {
      updateSelectInput(session, "substance_double1", selected = "")
    }
  })
  
  ###### Selected substance2 based on user choice ######
  observe({
    choices2 <- substance_choices2()
    selected2 <- isolate(input$substance_double2)
    if (!is.null(selected2))
      selected2 <- selected2[selected2 %in% choices2]
    updateSelectInput(session,
                      "substance_double2",
                      choices = choices2,
                      selected = selected2)
  })
  
  # If current selection is no longer valid (e.g. after a new filter is applied), clear it
  observe({
    valid_choices <- substance_choices2()
    current <- input$substance_double2
    if (!is.null(current) && !current %in% valid_choices) {
      updateSelectInput(session, "substance_double2", selected = "")
    }
  })
  
  
  ###### Display HPL visualisation graph ######
  output$rose_plot1 <- renderPlot({
    req(input$substance_double1)
    fxn_Make_Rose_Plot(compound_name = input$substance_double1,
                       data = data_hpli)
  })
  
  output$dist_plot_both <- renderPlot({
    req(input$substance_double1)
    fxn_Make_Distribution_Plot(
      compound_names = c(input$substance_double1, input$substance_double2),
      data = data_hpli
    )
  })
  
  output$rose_plot2 <- renderPlot({
    req(input$substance_double2)
    fxn_Make_Rose_Plot(compound_name = input$substance_double2,
                       data = data_hpli)
  })
  
  ###### Download data option ######
  output$download_data2 <- downloadHandler(
    filename = function() {
      req(input$substance_double1)
      req(input$substance_double2)
      paste0(
        "load_score_details_", #--make sure only allowed characters in name
        gsub(
          "[^A-Za-z0-9]",
          "_",
          input$substance_double1,
          "_",
          input$substance_double2
        ),
        "_",
        Sys.Date(),
        ".tsv"
      )
    },
    content = function(file) {
      req(input$substance_double1)
      req(input$substance_double2)
      #--what should go here?
      data_sub <-
        data_hpli |>
        filter(compound_name %in% c(input$substance_double1, input$substance_double2))
      display_data2 <-
        data_sub |>
        dplyr::mutate_if(is.numeric, round, 3) |>
        dplyr::select(
          compound,
          compound_type,
          env_raw,
          eco.terr_raw,
          eco.aqua_raw,
          hum_raw,
          load_score,
          missing_share
        )
      
      write.table(
        display_data2,
        file,
        sep = "\t",
        row.names = FALSE,
        col.names = TRUE,
        quote = FALSE
      )
    }
  )
  
  # Pesticide data entry tab ===================================================
  
  # Initialize reactive values for first set of inputs
  values1 <- reactiveValues()
  
  # Initialize the data frame
  observe({
    if (is.null(values1$data)) {
      # Create initial data frame with 5 rows
      initial_rows <- 5
      values1$data <- data.frame(
        Compound = rep("", initial_rows),
        Load_Score = rep(0, initial_rows),
        Quantity_Applied = rep(0, initial_rows),
        Risk_Score = rep(0, initial_rows),
        stringsAsFactors = FALSE
      )
    }
  })
  
  # Add row functionality
  observeEvent(input$add_row, {
    if (nrow(values1$data) < input$max_rows) {
      new_row <- data.frame(
        Compound = "",
        Load_Score = 0,
        Quantity_Applied = 0,
        Risk_Score = 0,
        stringsAsFactors = FALSE
      )
      values1$data <- rbind(values1$data, new_row)
    }
  })
  
  # Remove row functionality
  observeEvent(input$remove_row, {
    if (nrow(values1$data) > 1) {
      values1$data <- values1$data[-nrow(values1$data), ]
    }
  })
  
  # Render the handsontable
  output$hot_table <- renderRHandsontable({
    if (!is.null(values1$data)) {
      # Update calculations before rendering
      for (i in 1:nrow(values1$data)) {
        if (values1$data$Compound[i] != "" &&
            !is.na(values1$data$Quantity_Applied[i])) {
          # Look up load score based on Compound
          Load_Score <- data_hpli$load_score[data_hpli$compound == values1$data$Compound[i]]
          if (length(Load_Score) > 0) {
            values1$data$Load_Score[i] <- Load_Score
            # Calculate Risk_Score
            values1$data$Risk_Score[i] <- values1$data$Load_Score[i] * values1$data$Quantity_Applied[i]
          }
        }
      }
      
      rhandsontable(
        values1$data,
        rowHeaders = TRUE,
        height = 350,
        # Set column widths: Compound wider, others standard
        colWidths = c(180, 100, 140, 100)
      ) %>%
        hot_col(
          "Compound",
          type = "dropdown",
          source = as.character(data_hpli$compound),
          allowInvalid = FALSE
        ) %>%
        hot_col(
          "Load_Score",
          readOnly = TRUE,
          type = "numeric",
          format = "0.000"
        ) %>%
        hot_col("Quantity_Applied", type = "numeric", format = "0.000") %>%
        hot_col("Risk_Score", readOnly = TRUE, format = "0.000") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    }
  })
  
  # Update data when table is edited
  observeEvent(input$hot_table, {
    if (!is.null(input$hot_table)) {
      # Get the updated data
      updated_data <- hot_to_r(input$hot_table)
      
      # Process each row for auto-population and calculations
      for (i in 1:nrow(updated_data)) {
        if (!is.na(updated_data$Compound[i]) &&
            updated_data$Compound[i] != "") {
          # Auto-populate load score based on Compound
          matching_row <- data_hpli[data_hpli$compound == updated_data$Compound[i], ]
          if (nrow(matching_row) > 0) {
            updated_data$Load_Score[i] <- matching_row$load_score[1]
          }
          
          # Calculate Risk_Score (Load_Score * Quantity_Applied)
          if (!is.na(updated_data$Quantity_Applied[i]) &&
              updated_data$Quantity_Applied[i] > 0) {
            updated_data$Risk_Score[i] <- updated_data$Load_Score[i] * updated_data$Quantity_Applied[i]
          } else {
            updated_data$Risk_Score[i] <- 0
          }
        } else {
          # Reset values1 if no Compound selected
          updated_data$Load_Score[i] <- 0
          updated_data$Risk_Score[i] <- 0
        }
      }
      
      values1$data <- updated_data
    }
  })
  
  # Summary output
  output$summary <- renderText({
    if (!is.null(values1$data)) {
      # Filter to only filled rows (compounds that have been selected)
      filled_data <- values1$data[values1$data$Compound != "" &
                                   !is.na(values1$data$Compound), ]
      
      if (nrow(filled_data) > 0) {
        grand_total <- sum(values1$data$Risk_Score, na.rm = TRUE)
        
        # Find min and max risk scores among filled rows
        risk_min <- min(filled_data$Risk_Score, na.rm = TRUE)
        risk_max <- max(filled_data$Risk_Score, na.rm = TRUE)
        
        # Find compounds with min and max risk scores
        min_compound <- filled_data$Compound[which(filled_data$Risk_Score == risk_min)[1]]
        max_compound <- filled_data$Compound[which(filled_data$Risk_Score == risk_max)[1]]
        
        paste(
          "Total Risk Score: ",
          format(grand_total, digits = 2, nsmall = 2),
          "\n==================",
          "\nLowest Risk Compound:",
          "\n",
          min_compound,
          " (",
          format(risk_min, digits = 2, nsmall = 2),
          ")",
          "\n\nHighest Risk Compound:",
          "\n",
          max_compound,
          " (",
          format(risk_max, digits = 2, nsmall = 2),
          ")"
        )
      } else {
        "No compounds have been selected yet."
      }
    }
  })
  
  # Value boxes for dashboard display
  output$total_risk <- renderValueBox({
    if (!is.null(values1$data)) {
      grand_total <- sum(values1$data$Risk_Score, na.rm = TRUE)
      valueBox(
        value = format(grand_total, digits = 2, nsmall = 2),
        subtitle = "Total Risk Score",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    }
  })
  
  output$item_count <- renderValueBox({
    if (!is.null(values1$data)) {
      total_items <- sum(values1$data$Quantity_Applied, na.rm = TRUE)
      valueBox(
        value = format(total_items, digits = 2, nsmall = 2),
        subtitle = "Total Quantity of Compounds Applied",
        icon = icon("cubes"),
        color = "blue"
      )
    }
  })
  
  output$filled_rows <- renderValueBox({
    if (!is.null(values1$data)) {
      filled_rows <- sum(values1$data$Compound != "", na.rm = TRUE)
      valueBox(
        value = filled_rows,
        subtitle = "Number of Applications Entered",
        icon = icon("list"),
        color = "yellow"
      )
    }
  })
  
  # # Initialize reactive values for second set of inputs NOT DPONE YET
  # values1 <- reactiveValues()
  # 
  # # Initialize the data frame
  # observe({
  #   if (is.null(values1$data)) {
  #     # Create initial data frame with 5 rows
  #     initial_rows <- 5
  #     values1$data <- data.frame(
  #       Compound = rep("", initial_rows),
  #       Load_Score = rep(0, initial_rows),
  #       Quantity_Applied = rep(0, initial_rows),
  #       Risk_Score = rep(0, initial_rows),
  #       stringsAsFactors = FALSE
  #     )
  #   }
  # })
  # 
  # # Add row functionality
  # observeEvent(input$add_row, {
  #   if (nrow(values1$data) < input$max_rows) {
  #     new_row <- data.frame(
  #       Compound = "",
  #       Load_Score = 0,
  #       Quantity_Applied = 0,
  #       Risk_Score = 0,
  #       stringsAsFactors = FALSE
  #     )
  #     values1$data <- rbind(values1$data, new_row)
  #   }
  # })
  # 
  # # Remove row functionality
  # observeEvent(input$remove_row, {
  #   if (nrow(values1$data) > 1) {
  #     values1$data <- values1$data[-nrow(values1$data), ]
  #   }
  # })
  # 
  # # Render the handsontable
  # output$hot_table <- renderRHandsontable({
  #   if (!is.null(values1$data)) {
  #     # Update calculations before rendering
  #     for (i in 1:nrow(values1$data)) {
  #       if (values1$data$Compound[i] != "" &&
  #           !is.na(values1$data$Quantity_Applied[i])) {
  #         # Look up load score based on Compound
  #         Load_Score <- data_hpli$load_score[data_hpli$compound == values1$data$Compound[i]]
  #         if (length(Load_Score) > 0) {
  #           values1$data$Load_Score[i] <- Load_Score
  #           # Calculate Risk_Score
  #           values1$data$Risk_Score[i] <- values1$data$Load_Score[i] * values1$data$Quantity_Applied[i]
  #         }
  #       }
  #     }
  #     
  #     rhandsontable(
  #       values1$data,
  #       rowHeaders = TRUE,
  #       height = 350,
  #       # Set column widths: Compound wider, others standard
  #       colWidths = c(180, 100, 140, 100)
  #     ) %>%
  #       hot_col(
  #         "Compound",
  #         type = "dropdown",
  #         source = as.character(data_hpli$compound),
  #         allowInvalid = FALSE
  #       ) %>%
  #       hot_col(
  #         "Load_Score",
  #         readOnly = TRUE,
  #         type = "numeric",
  #         format = "0.000"
  #       ) %>%
  #       hot_col("Quantity_Applied", type = "numeric", format = "0.000") %>%
  #       hot_col("Risk_Score", readOnly = TRUE, format = "0.000") %>%
  #       hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  #   }
  # })
  # 
  # # Update data when table is edited
  # observeEvent(input$hot_table, {
  #   if (!is.null(input$hot_table)) {
  #     # Get the updated data
  #     updated_data <- hot_to_r(input$hot_table)
  #     
  #     # Process each row for auto-population and calculations
  #     for (i in 1:nrow(updated_data)) {
  #       if (!is.na(updated_data$Compound[i]) &&
  #           updated_data$Compound[i] != "") {
  #         # Auto-populate load score based on Compound
  #         matching_row <- data_hpli[data_hpli$compound == updated_data$Compound[i], ]
  #         if (nrow(matching_row) > 0) {
  #           updated_data$Load_Score[i] <- matching_row$load_score[1]
  #         }
  #         
  #         # Calculate Risk_Score (Load_Score * Quantity_Applied)
  #         if (!is.na(updated_data$Quantity_Applied[i]) &&
  #             updated_data$Quantity_Applied[i] > 0) {
  #           updated_data$Risk_Score[i] <- updated_data$Load_Score[i] * updated_data$Quantity_Applied[i]
  #         } else {
  #           updated_data$Risk_Score[i] <- 0
  #         }
  #       } else {
  #         # Reset values1 if no Compound selected
  #         updated_data$Load_Score[i] <- 0
  #         updated_data$Risk_Score[i] <- 0
  #       }
  #     }
  #     
  #     values1$data <- updated_data
  #   }
  # })
  # 
  # # Summary output
  # output$summary <- renderText({
  #   if (!is.null(values1$data)) {
  #     # Filter to only filled rows (compounds that have been selected)
  #     filled_data <- values1$data[values1$data$Compound != "" &
  #                                   !is.na(values1$data$Compound), ]
  #     
  #     if (nrow(filled_data) > 0) {
  #       grand_total <- sum(values1$data$Risk_Score, na.rm = TRUE)
  #       
  #       # Find min and max risk scores among filled rows
  #       risk_min <- min(filled_data$Risk_Score, na.rm = TRUE)
  #       risk_max <- max(filled_data$Risk_Score, na.rm = TRUE)
  #       
  #       # Find compounds with min and max risk scores
  #       min_compound <- filled_data$Compound[which(filled_data$Risk_Score == risk_min)[1]]
  #       max_compound <- filled_data$Compound[which(filled_data$Risk_Score == risk_max)[1]]
  #       
  #       paste(
  #         "Total Risk Score: ",
  #         format(grand_total, digits = 2, nsmall = 2),
  #         "\n==================",
  #         "\nLowest Risk Compound:",
  #         "\n",
  #         min_compound,
  #         " (",
  #         format(risk_min, digits = 2, nsmall = 2),
  #         ")",
  #         "\n\nHighest Risk Compound:",
  #         "\n",
  #         max_compound,
  #         " (",
  #         format(risk_max, digits = 2, nsmall = 2),
  #         ")"
  #       )
  #     } else {
  #       "No compounds have been selected yet."
  #     }
  #   }
  # })
  # 
  # # Value boxes for dashboard display
  # output$total_risk <- renderValueBox({
  #   if (!is.null(values1$data)) {
  #     grand_total <- sum(values1$data$Risk_Score, na.rm = TRUE)
  #     valueBox(
  #       value = format(grand_total, digits = 2, nsmall = 2),
  #       subtitle = "Total Risk Score",
  #       icon = icon("exclamation-triangle"),
  #       color = "red"
  #     )
  #   }
  # })
  # 
  # output$item_count <- renderValueBox({
  #   if (!is.null(values1$data)) {
  #     total_items <- sum(values1$data$Quantity_Applied, na.rm = TRUE)
  #     valueBox(
  #       value = format(total_items, digits = 2, nsmall = 2),
  #       subtitle = "Total Quantity of Compounds Applied",
  #       icon = icon("cubes"),
  #       color = "blue"
  #     )
  #   }
  # })
  # 
  # output$filled_rows <- renderValueBox({
  #   if (!is.null(values1$data)) {
  #     filled_rows <- sum(values1$data$Compound != "", na.rm = TRUE)
  #     valueBox(
  #       value = filled_rows,
  #       subtitle = "Number of Applications Entered",
  #       icon = icon("list"),
  #       color = "yellow"
  #     )
  #   }
  # })
  
}
