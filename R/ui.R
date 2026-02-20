# ui.R

# Gina Nichols, adapted from code created by
# Noé Vandevoorde octobre 2025


#### User Interface ############################################################

ui <- shinydashboard::dashboardPage(
  #skin = "green", # Try: "blue", "black", "purple", "yellow", "red", "green"
  
  
  ###### Header ##################################################################
  
  shinydashboard::dashboardHeader(title = "ADOPT-IPM online tool"),
  
  
  ###### Sidebar #################################################################
  
  shinydashboard::dashboardSidebar(
    ### Menu ###
    
    shinydashboard::sidebarMenu(
      menuItem(
        "  Explore Pesticide Compounds",
        tabName = "single",
        icon = icon("flask")
      )
      ,
      menuItem(
        "  Compare Pesticide Compounds",
        tabName = "compare",
        icon = icon("flask-vial")
      )
      ,
      menuItem(
        "  Pesticide Data Analyses",
        tabName = "pest_table",
        icon = icon("bugs")
      )
      ,
      menuItem(
        "  Qualitative Data Analyses",
        tabName = "qual_data",
        icon = icon("leaf")
      )
    ),
    
    # Pesticide data entry specific sidebar content
    conditionalPanel(
      condition = "input.sidebar_menu == 'pest_table'",
      br(),
      h4("Table Instructions", style = "padding-left: 15px; color: white;"),
      div(
        style = "padding-left: 15px; color: white; font-size: 12px;",
        p("• Select a compound from the dropdown"),
        p("• Load score will auto-populate"),
        p(
          "• Enter the quantity of compound applied (in consistent units for the entire table)"
        ),
        p("• The compound's risk score will be calculated automatically"),
        p(
          "• The total risk score for the pesticide package is displayed at the bottom"
        )
      ),
      br(),
      div(
        style = "padding-left: 15px;",
        actionButton("add_row", "Add Row", class = "btn-primary btn-sm", style = "margin-bottom: 10px;"),
        br(),
        actionButton("remove_row", "Remove Row", class = "btn-warning btn-sm", style = "margin-bottom: 15px;"),
        br(),
        numericInput(
          "max_rows",
          "Max Rows:",
          value = 5,
          min = 1,
          max = 50,
          width = "150px"
        )
      )
    ),
    
    ### Credit info, ADOPT IPM logo ###
    
    div(
      style = "position: fixed;
               bottom: 15px;
               left: 15px;
               font-size: 12px;
               color: #888;
               z-index: 1000;",
      # Add the adopt ipm logo (not working)
      #img(src = "adopt-ipm_logo-clean.png", height = "50px", width = "auto", style = "margin-bottom: 5px;"),
      br(),
      HTML(
        "<a href='https://adopt-ipm.eu/' target='_blank'>adopt-ipm.eu</a><br>
             Nichols and Vandevoorde (2025)<br>
            Last updated: Nov 2025<br>"
      )
    )
  ),
  
  
  ###### Body ####################################################################
  
  shinydashboard::dashboardBody(tabItems(
    ###### Body: Single Substance Tab ######
    tabItem(
      tabName = "single",
      ## First row
      fluidRow(
        # Substance selection box
        box(
          title = "Substance Selection",
          status = "primary",
          # "info",
          solidHeader = TRUE,
          width = 4,
          height = "275px",
          # Added consistent height
          
          # Filter options
          selectizeInput(
            "substance_category",
            label = NULL,
            choices = NULL,
            # populated from data in the server
            multiple = TRUE,
            selected = NULL,
            options = list(placeholder = "Filter by category")
          ),
          selectizeInput(
            "substance_origins",
            label = NULL,
            choices = NULL,
            # populated from data in the server
            multiple = TRUE,
            selected = NULL,
            options = list(placeholder = "Filter by origin")
          ),
          
          # Substance selection
          selectInput(
            "substance_single",
            "Select Substance:",
            choices = NULL,
            # populated from data in the server
            selected = NULL
          )
        ),
        
        # Substance information box
        box(
          title = "Substance Information",
          status = "primary",
          # "info",
          solidHeader = TRUE,
          width = 4,
          height = "275px",
          # Added consistent height
          verbatimTextOutput("substance_info")
        ),
        # Download Data box - replaced the data table
        box(
          title = "Download Load Score Details",
          status = "primary",
          solidHeader = TRUE,
          width = 4,
          height = "275px",
          # Added consistent height
          div(
            style = "text-align: center; padding: 20px;",
            p("Download the detailed load score data for the selected substance:"),
            br(),
            downloadButton(
              "download_data",
              "Download Data (TSV)",
              class = "btn-success btn-lg",
              # Changed to green
              icon = icon("download"),
              style = "background-color: #ffd74a; border-color: #ffd74a;"  # Custom green color
            )  
            
          )
        )
      ),
      ## Second row, two graphs (one rose and one distribution), blank area not sure what to do with
      fluidRow(
        #--Rose plot box
        box(
          title = "Load Scores by Compartment",
          status = "primary",
          solidHeader = TRUE,
          width = 4,
          plotOutput("rose_plot", height = "500px")
        ),
        #--Distribution box
        box(
          title = "Load Score Relative to All Substances",
          status = "primary",
          solidHeader = TRUE,
          width = 4,
          plotOutput("dist_plot", height = "500px")
        ),
        # Information and links box
        box(
          title = "Additional Resources",
          status = "info",
          solidHeader = TRUE,
          width = 4,
          div(
            style = "padding: 15px;",
            h4("About Load Scores"),
            p("Load scores represent a relative toxicity burden ."),
            p(
              "The visualization shows a substance's load scores for each compartment, as calculated by Vandervoode et al. (in review)"
            ),
            br(),
            h4("Useful Links"),
            tags$ul(tags$li(
              tags$a(
                "Pesticide Properties Database",
                href = "https://sitem.herts.ac.uk/aeru/ppdb/",
                target = "_blank"
              )
            ), tags$li(
              tags$a(
                "PhD manuscript with more details and background",
                href = "https://sytra.be/publication/three-tools-reduction-pesticide-impacts/",
                target = "_blank"
              )
            )),
            br(),
            p(
              strong(
                "To calculate the overall load, multiply the amount of substance applied by its load score."
              ),
              "See A FORTHCOMING PUBLICATION for more details."
            )
          )
        )
      )
      
    ),
    #--end of first tab
    
    ###### Body: Two Substance Tab ######
    
    tabItem(
      tabName = "compare",
      fluidRow(
        # Substance1 selection
        box(
          title = "First substance selection",
          status = "primary",
          # "info",
          solidHeader = TRUE,
          width = 4,
          height = "275px",
          
          # Filter options
          selectizeInput(
            "substance_category1",
            label = NULL,
            choices = NULL,
            # populated from data in the server
            multiple = TRUE,
            selected = NULL,
            options = list(placeholder = "Filter by category")
          ),
          selectizeInput(
            "substance_origins1",
            label = NULL,
            choices = NULL,
            # populated from data in the server
            multiple = TRUE,
            selected = NULL,
            options = list(placeholder = "Filter by origin")
          ),
          selectInput(
            "substance_double1",
            "Select Substance:",
            choices = NULL,
            # populated from data in the server
            selected = NULL
          )
        ),
        
        # Substance2 selection
        box(
          title = "Second substance selection",
          status = "primary",
          # "info",
          solidHeader = TRUE,
          width = 4,
          height = "275px",
          
          # Filter options
          selectizeInput(
            "substance_category2",
            label = NULL,
            choices = NULL,
            # populated from data in the server
            multiple = TRUE,
            selected = NULL,
            options = list(placeholder = "Filter by category")
          ),
          selectizeInput(
            "substance_origins2",
            label = NULL,
            choices = NULL,
            # populated from data in the server
            multiple = TRUE,
            selected = NULL,
            options = list(placeholder = "Filter by origin")
          ),
          
          # Substance selection
          selectInput(
            "substance_double2",
            "Select Substance:",
            choices = NULL,
            # populated from data in the server
            selected = NULL
          )
        ),
        
        # Blank space
        #column(width = 4)
        
        # Download Data box (replaced the blank space)
        box(
          title = "Download Load Score Details",
          status = "primary",
          solidHeader = TRUE,
          width = 4,
          height = "275px",
          # Added consistent height
          div(
            style = "text-align: center; padding: 20px;",
            p("Download the detailed load score data for the selected substance:"),
            br(),
            downloadButton(
              "download_data2",
              "Download Data (TSV)",
              class = "btn-success btn-lg",
              # Changed to green
              icon = icon("download"),
              style = "background-color: #ffd74a; border-color: #ffd74a;"  # Custom green color
            )  
            
          )
        )
        
      ),
      
      fluidRow(
        # Rose plot first substance
        box(
          title = "First Substance Load Scores",
          status = "primary",
          solidHeader = TRUE,
          width = 4,
          plotOutput("rose_plot1", height = "500px")
        ),
        
        # Rose plot second substance
        box(
          title = "Second Substance Load Scores",
          status = "primary",
          solidHeader = TRUE,
          width = 4,
          plotOutput("rose_plot2", height = "500px")
        ),
        #--figure with distributions
        box(
          title = "Load Score(s) Relative to All Substances",
          status = "primary",
          solidHeader = TRUE,
          width = 4,
          plotOutput("dist_plot_both", height = "500px")
        ),
      )
      
      
    ),
    #--end of tab
    
    
    ###### Body: Pesticide table tab ######
    tabItem(
      tabName = "pest_table",
      # First row: Table and Summary Statistics side by side for both packages
      fluidRow(
        box(
          title = "Editable Table with Calculations",
          status = "primary",
          solidHeader = TRUE,
          width = 4,
          height = "500px",
          rHandsontableOutput("hot_table1")
        ),
        box(
          title = "Summary Statistics",
          status = "info",
          solidHeader = TRUE,
          width = 2,
          height = "500px",
          verbatimTextOutput("summary1")
        ),
        box(
          title = "Editable Table with Calculations",
          status = "primary",
          solidHeader = TRUE,
          width = 4,
          height = "500px",
          rHandsontableOutput("hot_table2")
        ),
        box(
          title = "Summary Statistics",
          status = "info",
          solidHeader = TRUE,
          width = 2,
          height = "500px",
          verbatimTextOutput("summary2")
        )
      ),
      # Second row: Data Information spanning full width
      fluidRow(
        box(
          title = "Data Information",
          status = "success",
          solidHeader = TRUE,
          width = 6,
          valueBoxOutput("total_risk1"),
          valueBoxOutput("item_count1"),
          valueBoxOutput("filled_rows1")
        ),
        box(
          title = "Data Information",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          valueBoxOutput("total_risk2"),
          valueBoxOutput("item_count2"),
          valueBoxOutput("filled_rows2")
        )
      )
      
    ) #--end of tab
  ))   #--end of dashboard body
  
  
)
