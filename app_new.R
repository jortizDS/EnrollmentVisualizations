#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# load libraries
library(shiny)
library(readxl)
library(dplyr)
library(DT)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(scales)
library(stringr)
library(shinyjs)
library(purrr)
library(readr)
#library(plotly)

# load data 
data_names <- list.files("Data/", pattern = ".RDS", full.names = TRUE)

df <- readRDS("Data/clean.ethsexsp25.RDS")
colleges_df <- df %>%
  filter(Degree == "College total")

colleges_vec <- setNames(colleges_df$Coll, colleges_df$`Major Name`)

college_abb_newest = list(KL="ACES", KM="BUS", KN ="EDUC", KP="ENGR", KR="FAA", KS = "Grad", KT = "MDIA", KU = "LAW",
    KV = "LAS", KW = "DGS", KY="AHS", LC="VETMED", LG="LER", LL="SOCW", LN="CITL", LP="IS", LT="CIMED", NB = "Provost",
    LE = "Aviation")

# college_abb_newest = list(KL="ACES", KM="Gies", KN ="Education", KP="Grainger", KR="FAA", KS = "Graduate", KT = "Media", KU = "Law", 
#                           KV = "LAS", KW = "DGS", KY="AHS", LC="Vet Med", LG="LER", LL="Social Work", LN="CITL", LP="iSchool", LT="Carle Illinois", NB = "Provost")

input = data.frame(level = "Graduate",
                   college = "KV",
                   major = "Statistics",
                   degree = "",
                   levelYN = F,
                   collegeYN = F,
                   majorYN = F,
                   conc = F,
                   degreeYN = F,
                   semester = "Fall",
                   year = 2018)

# code ----
ui <- fluidPage(
  titlePanel("UIUC Enrollment Explorer"),
  # tags$h4("Explore student enrollment statistics for the University of Illinois Urbana-Champaign by semester, college, degree level, major, and demographic characteristics"),
  tags$h4(HTML(
    "Explore student enrollment at the University of Illinois Urbana-Champaign by semester, college, degree level, major, and demographic characteristics.
   <br><small>Data source: <a href='https://dmi.illinois.edu/stuenr/#race' target='_blank'>Division of Management Information</a></small>"
  )),
  
  
  
  
  # Entire screen is a fluidRow: left = sidebar + URM, right = plots
  fluidRow(
    # LEFT SIDE: Sidebar + URM Plot
    column(width = 2,
           wellPanel(
             helpText("Select one or more filters to customize the enrollment insights"),
             fluidRow(
               column(width = 6,
                      selectInput("year", "Year", choices = rev(c(2004 : 2025)), selected = 2025)),
               column(width = 6,
                      selectInput("semester", "Semester",
                                  choices = NULL)),
                                 #   c("Fall", "Spring", "Summer"), selected = "Spring"))
             ),
             checkboxInput('levelYN', label = "View Campus enrollment by Degree level", value = FALSE, width = NULL),
             # conditionalPanel(
             #   condition = "input.level == ''",
             #   checkboxInput('levelYN', label = "View Campus Enrollment by Degree level", value = FALSE, width = NULL)
             # ),
             selectizeInput('level', 
                            label = HTML("1. Filter by Degree Level <span title='After selecting major, if more than one degree is available \nyou will have the choice to filter by Degree type'>⍰</span>"), 
                            choices = c("Show all degree levels" = "", c("Undergraduate", "Graduate", "Nondegree")),
                            selected = ""),
             checkboxInput('collegeYN', label = "View enrollment by College", value = FALSE, width = NULL),
             selectizeInput('college', "2. Filter by College", choices = c("Show all colleges" = "")),
             # checkboxInput('collegeDegreeYN', label = "View college enrollment by degree type", value = FALSE, width = NULL),
             checkboxInput('majorYN', label = "View enrollment by major", value = FALSE, width = NULL),
             selectizeInput('major', "3. Filter by Major", choices = c("Show all majors" = "")),
             uiOutput("degree_type_ui"),  # placeholder for conditional dropdown
             #uiOutput("conc_ui")  # placeholder for conditional dropdown
             useShinyjs(),
             hidden(checkboxInput('conc', label = "View enrollment by concentration", value = FALSE, width = NULL))
               # selectizeInput('conc',
               #              label = HTML("4. Filter by Concentration (if applicable) <span title='Concentration is applied after major is selected. The \nvalue \"None\" identifies majors without a concentration'>⍰</span>"),
               #              choices = c("Show all concentrations" = "")))
             
             
           ),
           actionButton("reset_filters", "Reset All Filters")
    ),
   
    # RIGHT SIDE: Main content
    column(width = 10,
           fluidRow(
             column(12,
                    h4("Summary"),
                    uiOutput("summary_text")
             )
           ),
           
           # Row 1: Sex and Residency plots
           fluidRow(
             column(6,
                    h3("Enrollment by Sex"),
                    uiOutput("nodata_msg"),
                    uiOutput("sexplot_ui") 
                    # div(
                    #   style = "height: 40vh; overflow-y: auto;",
                    #   shinycssloaders::withSpinner(
                    #     sexplot_ui
                    #    # plotOutput("sexplot", height = "45vh"),  # Set larger height than container so scroll activates
                    #     type = 1, color = "#007bff", size = 0.5
                    #   )
                    # )
             ),
             column(6,
                    h3("Enrollment by Illinois residency"),
                    uiOutput("resplot_ui") 
                    
                    # shinycssloaders::withSpinner(
                    #   plotOutput("resplot", height = "25vh"),
                    #   type = 1, color = "#007bff", size = 0.5)
             )
           ),
           
           uiOutput("race_urm_ui"),
           uiOutput("timeseries_ui") # ,
          # plotOutput("timeSeriesPlot"),
           
           
           # # Row 3: Data Table
           # fluidRow(
           #   column(12,
           #          h3("View Data"),
           #          shinycssloaders::withSpinner(
           #            DT::dataTableOutput("mytable"),
           #            type = 4, color = "#007bff", size = 0.5)
           #   )
           # )
    )
    
  )
)


# Define server logic 
server <- function(input, output, session) {
  
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "year", selected = "2025")         # or default year
    updateSelectInput(session, "semester", selected = "Spring")   # or default semester
    
    updateSelectInput(session, "degree", selected = NULL)
    updateSelectInput(session, "college", selected = "")
    updateSelectInput(session, "level", selected = "")
    updateSelectInput(session, "major", selected = "")
    updateCheckboxInput(session, "conc", value = FALSE)
    updateCheckboxInput(session, "levelYN", value = FALSE)
    updateCheckboxInput(session, "collegeYN", value = FALSE)
    updateCheckboxInput(session, "majorYN", value = FALSE)
    updateCheckboxInput(session, "degreeYN", value = FALSE)
    hideElement("conc")
  })

  
  output$race_urm_ui <- renderUI({
    # Get filtered data
    data <- final_data()
    req(nrow(data) > 0)
    
    n_groups <- length(unique(final_data()$Degree))
    
    plot_height <- if (n_groups > 10 & !input$collegeYN) {
      paste0(40 + (n_groups - 10) * 3, "vh")  # grow height with groups
    } else {
      "25vh"  # default
    }
    
    
    if ((input$year == 2020 & input$semester == "Fall") | input$year > 2020) {
      # Show race and URM side-by-side
      
      
      fluidRow(
        column(7,
               h3("Enrollment by Race and Ethnicity"),
               div(
                 style = paste0("height:", "25vh", "; overflow-y:auto;"),
                 shinycssloaders::withSpinner(
                   plotOutput("raceplot", height = plot_height),  # tall enough to scroll
                   type = 1, color = "#007bff", size = 0.5
                 )
               )
        ),
        column(5,
               h3(HTML("Underrepresented Minority Breakdown <span title='URM includes American Indian & Alaskan Native, Native Hawaiian & Pacific Islander, African American, and Hispanic/Latino. Multi-racial persons are included if one selected group is URM. Foreign students are counted separately. This information is available post Fall 2020.' style='cursor: help;'>⍰</span>")),
               div(
                 style = paste0("height:", "25vh", "; overflow-y:auto;"),
                 shinycssloaders::withSpinner(
                   plotOutput("URM_plot", height = plot_height),  # tall enough to scroll
                   type = 1, color = "#007bff", size = 0.5
                 )
               )
        )
      )
    } else {
      # Only show race plot full width
      fluidRow(
        column(12,
               h3("Enrollment by Race and Ethnicity"),
               div(
                 style = paste0("height:", "25vh", "; overflow-y:auto;"),
                 shinycssloaders::withSpinner(
                   plotOutput("raceplot", height = plot_height),  # tall enough to scroll
                   type = 1, color = "#007bff", size = 0.5
                 )
               )
        )
      )
    }
  })
  
  output$timeseries_ui <- renderUI({
    # Get filtered data

    if (input$major != "") {
      n_groups <- length(unique(final_data()$Degree))
      
      plot_height <- if ((n_groups > 10 & !input$collegeYN)) {
        paste0(40 + (n_groups - 10) * 3, "vh")  # grow height with groups
      } else if (input$conc | (!is.null(input$degreeYN) && input$degreeYN)) {
        paste0(40 + (n_groups - 1) * 20, "vh")  # grow height with groups
      } else {
        "25vh"  # default
      }
      
      if (input$conc | (!is.null(input$degreeYN) && input$degreeYN)) {
        # Show race and URM side-by-side
        fluidRow(
          column(12,
                 h3("Enrollment over Time"),
                 div(
                   style = paste0("height:", plot_height, "; overflow-y:auto;"),
                   shinycssloaders::withSpinner(
                     plotOutput("timeSeriesPlot", height = plot_height),  # tall enough to scroll
                     type = 1, color = "#007bff", size = 0.5
                   )
                 )
          )
        )
      } else {
        # Show race and URM side-by-side
        fluidRow(
          column(12,
                 h3("Enrollment over Time"),
                 div(
                   style = paste0("height:", "30vh", "; overflow-y:auto;"),
                   shinycssloaders::withSpinner(
                     plotOutput("timeSeriesPlot", height = plot_height),  # tall enough to scroll
                     type = 1, color = "#007bff", size = 0.5
                   )
                 )
          )
        )
      }
      

    }
  })
  
  
  
  # Dynamically choose height based on group count
  output$sexplot_ui <- renderUI({
    n_groups <- length(unique(final_data()$Degree))
    
    plot_height <- if (n_groups > 10 & !input$collegeYN) {
      paste0(40 + (n_groups - 10) * 3, "vh")  # grow height with groups
    } else {
      "25vh"  # default
    }
    
    div(
      style = paste0("height:", "25vh", "; overflow-y:auto;"),
      shinycssloaders::withSpinner(
        plotOutput("sexplot", height = plot_height),  # tall enough to scroll
        type = 1, color = "#007bff", size = 0.5
      )
    )
  })
  
  # Dynamically choose height based on group count
  output$resplot_ui <- renderUI({
    n_groups <- length(unique(final_data()$Degree))
    
    plot_height <- if (n_groups > 10 & !input$collegeYN) {
      paste0(40 + (n_groups - 10) * 3, "vh")  # grow height with groups
    } else {
      "25vh"  # default
    }
    
    div(
      style = paste0("height:", "25vh", "; overflow-y:auto;"),
      shinycssloaders::withSpinner(
        plotOutput("resplot", height = plot_height),  # tall enough to scroll
        type = 1, color = "#007bff", size = 0.5
      )
    )
  })
  

  
  enrollment_data <- reactive({
    req(input$year, input$semester)  # wait for user input
    
    up_semester <- ifelse(input$semester == "Fall", "fa", 
                          ifelse(input$semester == "Spring", "sp", 
                                 ifelse(input$semester == "Summer", "su", NA)))
    
    # Build file path
    filepath <- paste0("Data/clean.ethsex", up_semester, substring(input$year, 3, 4), ".RDS")
    
    if (input$year == 2004) {
      filepath <- paste0("Data/clean.ethsexfa04.RDS")
    }
    
    if (input$year == 2025) {
      filepath <- paste0("Data/clean.ethsexsp25.RDS")
    }
    
    
    # Optional: Add file existence check
    if (!file.exists(filepath)) {
      showNotification("Data file not found.", type = "error")
      return(NULL)
    }
    
    # Read the data
    semester_df = readRDS(filepath)
    
    # update 
    #semester_df[grepl("Professional", semester_df$`Major Name`), 'Major Name'] <- ""
    
    level_abb = c("Undergraduate" = "Undergraduate","Graduate" = "Graduate","Professional"="Nondegree")
    semester_df$programtype <- ifelse(semester_df$`Major Name` %in% names(level_abb), level_abb[semester_df$`Major Name`], semester_df$programtype)
    semester_df$Degree <- ifelse(semester_df$`Major Name` %in% names(level_abb), level_abb[semester_df$`Major Name`], semester_df$Degree)
    
    
    
    semester_df
    
      
    ## create observe that creates filter of data, therefore same filter can be applied here to create time series plot
    #figure out formatting issues, why is somteimte "fall" sepreater 
    
  })
  
  output$timeSeriesPlot <- renderPlot({
    req(input$major != "")
    
    files <- list.files("Data/", pattern = "\\.RDS$", full.names = TRUE)
    
    read_filter <- function(file) {
      df <- readRDS(file)
      
      df <- df %>%
        filter(Coll %in% input$college,
               `Major Name` %in% input$major,
               programtype %in% input$level) %>%
        group_by(`Major Name`, `Major code`) 
      
      if (!is.null(input$degree) && input$degree != "") {
        df <- df %>%
          filter(Coll %in% input$college,
                 `Major Name` %in% input$major,
                 programtype %in% input$level,
                 Degree %in% input$degree) %>%
          group_by(`Major Name`, `Major code`)
      }

      if (!is.null(input$conc) && input$conc) {
        df <- df %>%
          filter(Coll %in% input$college,
                 `Major Name` %in% input$major,
                 programtype %in% input$level) %>%
          group_by(`Major Name`, `Major code`, `Concentration Name (if any)`)
      }


      if (!is.null(input$degreeYN) && input$degreeYN) {
        df <- df %>%
          filter(Coll %in% input$college,
                 `Major Name` %in% input$major,
                 programtype %in% input$level) %>%
          group_by(`Major Name`, `Major code`, Degree)
      }
      
      df %>%
      #  group_by(`Major Name`, `Major code`) %>%
        summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep") %>%
        mutate(Semester = str_extract(basename(file), "(fa|sp|su)"),
               Year = str_extract(basename(file), "\\d{2}")) %>%
        ungroup() %>%
        select(any_of(c("Major Name", "Major code", "Total", "Year", "Semester", "Degree", "Concentration Name (if any)"))) %>%
        ungroup() %>%
        mutate(FiscalYear = ifelse(Semester == "fa", as.numeric((Year))+ 1, as.numeric(Year))) %>%
        mutate(FY = paste0("'", dataRetrieval::zeroPad(FiscalYear, padTo = 2 )),
               Semester = factor(Semester, levels = rev(c("fa", "sp", "su")), ordered = TRUE))
    }
    
    all_data <- purrr::map_dfr(files, read_filter)
    
    validate(
      need(nrow(all_data) > 0, "No data available for this selection")
    )
    
    #######
    
    # all_data %>% filter(Semester == up_semester & Year == substring(input$year, 3, 4))
    # scale_fill_manual(values = c(
    #   "su" = "#9d7b68",    
    #   "sp" = "#715335",  
    #   "fa" = "#514420"
    # ))
    
    #############
    
    TSplot <- all_data %>%
      ggplot(aes(x = FY, y = Total, fill = Semester)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Major Enrollment over Time", x = "Fiscal Year", y = "Enrollment",
           subtitle = "Please note: Fall semesters are shown under the next Fiscal Year e.g. Fall 2024 appears in FY25") + 
          theme(axis.text.y = element_text(size = 12),
                axis.text.x = element_text(size = 15),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 12)) 
     
    
    if (input$conc) {
      conc_orders <- all_data %>%
        #conc_list[[1]] %>%
        filter(Semester ==  ifelse(input$semester == "Fall", "fa", 
             ifelse(input$semester == "Spring", "sp", 
                    ifelse(input$semester == "Summer", "su", NA)))) %>%
        filter(Year == substr(input$year, 3, 4)) %>%
        arrange(desc(Total)) %>%
        pull(`Concentration Name (if any)`)
      
      if (length(conc_orders) > 1) {
        
        ## insert grid.arrange
        all_data$`Concentration Name (if any)` = factor(all_data$`Concentration Name (if any)`, levels = (conc_orders), ordered = T)
        
       conc_list <- split(all_data, all_data$`Concentration Name (if any)`)
        
       min_max = sort(unique(all_data$Year))
        plot_list <- lapply(conc_list, function(df) {
        # conc_list[[6]] %>% 
          #ungroup() %>%
          #df = df %>%
           # mutate(FYear = as.numeric(paste0("20", substr(Year, 2, 3))),
            #       Semester = factor(Semester, levels = rev(c("fa", "sp", "su")), ordered = TRUE)) 
          
          missing_years =  min_max[!(min_max %in% df$Year)]
          
          OG_copy <- df[1,] %>% mutate(Year  = NA, Semester = "sp",
                                                   FiscalYear = NA, FY = NA,
                                                   Total = 0)
          
          
          
          if (length(missing_years) > 0) {
            df = bind_rows(lapply(1:length(missing_years), function(x) {
              OG_copy_x = OG_copy  %>%
                mutate(Year  = as.character(missing_years[x]),
                       FY = paste0("'", as.character(missing_years[x])))
              OG_copy_x
            })%>% bind_rows(), df)
          }
          
          #conc_list[[2]] %>%
         df %>%
            ggplot(aes(x = FY, y = Total, fill = Semester)) +
            geom_col() +
            theme_minimal() +
            # Major_total
            labs(title = paste(unique(df$`Major Name`), "Major concentration:", unique(df$`Concentration Name (if any)`)), 
                 x = "Fiscal Year", y = "Enrollment",
                 subtitle = "Please note: Fall semesters are shown under the next Fiscal Year e.g. Fall 2024 appears in FY25") +
            theme(legend.position = "right", legend.justification = "top",
                  strip.text.x = element_text(size = 15),
                  axis.text.y = element_text(size = 12),
                  axis.text.x = element_text(size = 15),
                  legend.text = element_text(size = 12),
                  legend.title = element_text(size = 12),
                  axis.title = element_text(size = 13),
                  strip.background = element_rect(fill = "lightgray", color = NA)) +
           # xlim(((min_max[1]-1)), min_max[2]+1)  +
            geom_text(
              data = df %>%
                dplyr::group_by(FY) %>%
                dplyr::mutate(TotalBar = sum(Total), .groups = "drop"),
              aes(x = FY, y = TotalBar, 
                  label = ifelse(substr(input$year, 3, 4) == (Year) & grepl(substr(input$semester, 1, 2), Semester, ignore.case = T), 
                                 paste0(toupper(substr(input$semester, 1, 2)), Year, ": \n", Total), NA)),
              inherit.aes = FALSE,   # <-- important
              vjust = -0.5, size = 3
            )
            # geom_text(aes(label = ifelse(input$year == FYear & 
            #                                grepl(substr(input$semester, 1, 2), Semester, ignore.case = T), paste0(toupper(Semester), ": \n", Total), NA)), 
            #           vjust = -.5, size = 3)
        })
        
        # Step 3: Arrange plots
        grid.arrange(grobs = plot_list, ncol = 1)
        
        
      } else {
        TSplot +
          # Major_total
          labs(title = "Enrollment over Time by Major concentration", x = "Fiscal Year", y = "Enrollment",
               subtitle = "Please note: Fall semesters are shown under the next Fiscal Year e.g. Fall 2024 appears in FY25") +
          facet_wrap(~ factor(`Concentration Name (if any)`, levels = conc_orders, ordered = T), scales = "free_y", ncol = 1) +
          theme(legend.position = "right", legend.justification = "top",
                strip.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 12),
                axis.text.x = element_text(size = 15),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 12),
                axis.title = element_text(size = 13),
                strip.background = element_rect(fill = "lightgray", color = NA)) +
          geom_text(
            data = df %>%
              dplyr::group_by(FY) %>%
              dplyr::mutate(TotalBar = sum(Total), .groups = "drop"),
            aes(x = FY, y = TotalBar, 
                label = ifelse(substr(input$year, 3, 4) == Year & grepl(substr(input$semester, 1, 2), Semester, ignore.case = T), 
                               paste0(toupper(substr(input$semester, 1, 2)), Year, ": \n", Total), NA)),
            inherit.aes = FALSE,   # <-- important
            vjust = -0.5, size = 3
          )
          #geom_text(aes(label = ifelse(substr(input$year, 3, 4) == substr(Year, 2,3 ) & grepl(substr(input$semester, 1, 2), Semester, ignore.case = T), paste0(toupper(Semester), ": \n", Total), NA)), vjust = -0.2, size = 3)
      }

      
    } 
    else if (!is.null(input$degreeYN) && input$degreeYN) {
      
      degree_orders <- all_data %>%
        #conc_list[[1]] %>%
        filter(Semester ==  ifelse(input$semester == "Fall", "fa", 
                                   ifelse(input$semester == "Spring", "sp", 
                                          ifelse(input$semester == "Summer", "su", NA)))) %>%
        filter(Year == substr(input$year, 3, 4)) %>%
        arrange(desc(Total)) %>%
        pull(Degree)
      
      if (length(degree_orders) > 1) {
        
        ## insert grid.arrange
        all_data$Degree = factor(all_data$Degree, levels = (degree_orders), ordered = T)
        
        degree_list <- split(all_data, all_data$Degree)
        
        deg_min_max = sort(unique(all_data$Year))
        deg_plot_list <- lapply(degree_list, function(df) {
         #degree_list[[1]] %>% 
          
          missing_years =  deg_min_max[!(deg_min_max %in% df$Year)]
          
          OG_copy <- df[1,] %>% mutate(Year  = NA, Semester = "sp",
                                       FiscalYear = NA, FY = NA,
                                       Total = 0)
          
          
          
          if (length(missing_years) > 0) {
            df = bind_rows(lapply(1:length(missing_years), function(x) {
              OG_copy_x = OG_copy  %>%
                mutate(Year  = as.character(missing_years[x]),
                       FY = paste0("'", as.character(missing_years[x])))
              OG_copy_x
            })%>% bind_rows(), df)
          }
          
          
          df %>%
            ggplot(aes(x = FY, y = Total, fill = Semester)) +
            geom_col() +
            theme_minimal() +
            # Major_total
            labs(title = paste(input$level, unique(df$`Major Name`), "Program enrollment by Degree:", unique(df$Degree)), 
                 x = "Fiscal Year", y = "Enrollment",
                 subtitle = "Please note: Fall semesters are shown under the next Fiscal Year e.g. Fall 2024 appears in FY25") +
            theme(legend.position = "right", legend.justification = "top",
                  strip.text.x = element_text(size = 15),
                  axis.text.y = element_text(size = 12),
                  axis.text.x = element_text(size = 15),
                  legend.text = element_text(size = 12),
                  legend.title = element_text(size = 12),
                  axis.title = element_text(size = 13),
                  strip.background = element_rect(fill = "lightgray", color = NA)) +
            geom_text(
              data = df %>%
                dplyr::group_by(FY) %>%
                dplyr::mutate(TotalBar = sum(Total), .groups = "drop"),
              aes(x = FY, y = TotalBar, 
                  label = ifelse(substr(input$year, 3, 4) == Year & grepl(substr(input$semester, 1, 2), Semester, ignore.case = T), 
                                 paste0(toupper(substr(input$semester, 1, 2)), Year, ": \n", Total), NA)),
              inherit.aes = FALSE,   # <-- important
              vjust = -0.5, size = 3
            )
            #geom_text(aes(label = ifelse(input$year == FYear & grepl(substr(input$semester, 1, 2), Semester, ignore.case = T), paste0(toupper(Semester), ": \n", Total), NA)), vjust = -0.2, size = 3)
        })
        
        # Step 3: Arrange plots
        grid.arrange(grobs = deg_plot_list, ncol = 1)
        
        
      } else {
        TSplot +
          # Major_total
          labs(title = "Enrollment over Time by Degree", x = "Fiscal Year", y = "Enrollment",
               subtitle = "Please note: Fall semesters are shown under the next Fiscal Year e.g. Fall 2024 appears in FY25") +
          facet_wrap(~ factor(Degree, levels = degree_orders, ordered = T), scales = "free_y", ncol = 1) +
          theme(legend.position = "right", legend.justification = "top",
                strip.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 12),
                axis.text.x = element_text(size = 15),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 12),
                axis.title = element_text(size = 13),
                strip.background = element_rect(fill = "lightgray", color = NA)) +
          geom_text(
            data = all_data %>%
              dplyr::group_by(Year) %>%
              dplyr::mutate(TotalBar = sum(Total), .groups = "drop"),
            aes(x = Year, y = TotalBar, 
                label = ifelse(substr(input$year, 3, 4) == substr(Year, 2,3 ) & grepl(substr(input$semester, 1, 2), Semester, ignore.case = T), 
                               paste0(toupper(substr(input$semester, 1, 2)), ": \n", Total), NA)),
            inherit.aes = FALSE,   # <-- important
            vjust = -0.5, size = 3
          )
        
          #geom_text(aes(label = ifelse(substr(input$year, 3, 4) == substr(Year, 2,3 ) & grepl(substr(input$semester, 1, 2), Semester, ignore.case = T), paste0(toupper(Semester), ": \n", Total), NA)), vjust = -0.2, size = 3)
        
      }

     
    } else {
      TSplot +
        geom_text(
          data = all_data %>%
            dplyr::group_by(FY) %>%
            dplyr::mutate(TotalBar = sum(Total), .groups = "drop"),
          aes(x = FY, y = TotalBar, 
              label = ifelse(substr(input$year, 3, 4) == Year & grepl(substr(input$semester, 1, 2), Semester, ignore.case = T), 
                             paste0(toupper(substr(input$semester, 1, 2)), substr(input$year, 3, 4), ": \n", Total), NA)),
          inherit.aes = FALSE,   # <-- important
          vjust = -0.5, size = 3
        )
        
        # geom_text(aes(label = ifelse(substr(input$year, 3, 4) == substr(Year, 2,3 ) & 
        #                                grepl(substr(input$semester, 1, 2), Semester, ignore.case = T), 
        #                              paste0(toupper(Semester), ": \n", Total), NA)), vjust = ifelse(input$semester == "Summer", -6, -4), size = 3)
    }
    
  })
  
  
  # timeSeriesPlot <- observeEvent({
  #   req(input$major != "")
  # 
  #   
  # 
  #     # 1. List all CSV (or Excel, etc.) files in your folder
  #     files <- list.files("Data/", pattern = "\\.RDS$", full.names = TRUE)
  # 
  #     read_filter <- function(file) {
  # 
  #       if (input$major == "" ) {
  #         df <- data.frame(NA)
  #       }
  # 
  #       if (!is.null(input$degree) && input$degree != "") {
  # 
  #         df <- readRDS(file) %>%
  #           filter(Coll %in% c(input$college) & (`Major Name` %in%  c(input$major)) &
  #                    (programtype %in%  c(input$level)) & (Degree %in% c(input$degree))) %>%
  #           group_by(`Major Name`, `Major code`) %>%
  #           summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep") %>%
  #           #filter(`Major code` == major_code) %>%
  #           mutate(Semester = str_extract(basename(file), "(fa|sp|su)"),
  #                  Year = str_extract(basename(file), "\\d{2}"))
  #       } else {
  #         df <- readRDS(file) %>%
  #           filter(Coll %in% c(input$college) & (`Major Name` %in%  c(input$major)) & (programtype %in% c(input$level))) %>%
  #           group_by(`Major Name`, `Major code`) %>%
  #           summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep") %>%
  #           #filter(`Major code` == major_code) %>%
  #           mutate(Semester = str_extract(basename(file), "(fa|sp|su)"),
  #                  Year = str_extract(basename(file), "\\d{2}"))
  #       }
  #       return(df)
  #     }
  # 
  # 
  #     # 3. Apply to all files
  #     all_data <- map_dfr(files, read_filter)
  # 
      # if (nrow(all_data) > 0) {
      #   
      #   all_data %>%
      #     mutate(Semester = factor(Semester, levels = (c("fa", "sp", "su")), ordered = T)) %>%
      #     mutate(FY = ifelse(Semester %in% c("sp", "su"), as.numeric(paste0("20", Year)), as.numeric(paste0("20", Year))+1) ) %>%
      #     mutate(FY = paste0("'", substr(FY, 3, 4))) %>%
      #     ggplot(aes(x = Semester, y = Total, fill = Semester)) +
      #     #geom_line() +
      #     geom_col() +
      #     theme_minimal() +
      #     labs(title = paste0("Major Enrollment over Time: "), x = "Year", y = "Enrollment") +
      #     facet_wrap(~FY, nrow = 1, scales = "free_x") +
      #     theme(axis.text.x = element_blank())
      # }
  # 
  # 
  # })
  

  
  # Updata Selectize choices ---------------------
  
  # update year choices
  observeEvent(input$year, {
    if (input$year == 2004) {
      
      # This will stop execution if year is 2004 and semester is spring or summer
      req(!(input$year == 2004 & input$semester %in% c("Spring", "Summer")))
      
      updateSelectInput(session, "semester", 
                        choices = c("Fall"), 
                        selected = "Fall")
    } else if (input$year == 2025) {
      updateSelectInput(session, "semester", 
                        choices = c("Spring"), 
                        selected = "Spring")
    } else {
      updateSelectInput(session, "semester", 
                        choices = c("Fall", "Spring", "Summer"), selected = input$semester)
    }
  })
  
  
  ## Update college choices
  observeEvent(c(input$level), {
    # Ensure inputs are available
    req(input$level)  
    
    college_names_df <- enrollment_data() %>%
      filter(Degree == "College total")
    
    coll_names_vec <- setNames(college_names_df$`Major Name`, college_names_df$Coll)
    
    colleges_df <- enrollment_data() %>%
      filter(programtype == input$level) %>%
      filter(Degree != "College total") %>%
      group_by(Coll) %>%
      summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
      ungroup()
    
    # ex <- unique(iris$Species)
    # names(ex) <- month.abb[1:3]
    # 
    # ex[month.abb[1:2]]
    college_choices <- c("Show all colleges" = "", setNames(names(coll_names_vec[colleges_df$Coll]), coll_names_vec[colleges_df$Coll]))
    
    # Extract majors safely as a vector
    # college_choices <- c("Show all majors" = "", as.vector(filtered_colleges))
    
    updateSelectInput(session,
                      inputId = "college",
                      choices = college_choices,
                      selected = "")
    
    # # Update the select input
    # if (input$levelYN != TRUE) {
    #   updateSelectInput(session,
    #                     inputId = "college",
    #                     choices = college_choices,
    #                     selected = "")
    # } else {
    #   updateSelectInput(session,
    #                     inputId = "college",
    #                     choices = c("Show all colleges" = ""),
    #                     selected = "")
    # } 

  })

  ## Update major choices
  observeEvent(c(input$college, input$level), {
    # Ensure inputs are available
    req(input$college, input$level)  
    
    # Filter the data
    filtered_majors <- enrollment_data() %>%
      filter(Coll == input$college & programtype == input$level) %>%
      select(`Major Name`) %>%
      na.omit() %>%
      distinct() %>% 
      arrange(`Major Name`) %>% pull()
    
    # Extract majors safely as a vector
    major_choices <- c("Show all majors" = "", as.vector(filtered_majors))
    
    # Update the select input
    updateSelectInput(session,
                      inputId = "major",
                      choices = major_choices,
                      selected = "")
  })
  
  
  # Update conc choices
  observeEvent(c(input$college, input$level, input$major, input$degree), {
    # Only require college, level, and major; degree may be optional
    req(input$college != "", input$level != "", input$major != "")

    # Filter the data
    filtered_concs_df <- enrollment_data() %>%
     # data %>%
        filter(Coll == input$college,
               programtype == input$level,
               `Major Name` == input$major) %>%
      pull(`Concentration Name (if any)`) %>%
      unique()
    if (length(as.vector(filtered_concs_df)) > 1) {
      if (!is.null(input$degree)) {
        
        if (input$degree != "") {
          updateCheckboxInput(session, "conc", value = FALSE)
          
          showElement("conc")
        } else {
          hideElement("conc")
        }
        
        
      } else {
        showElement("conc")
      }
    }
    
    
    
    
    # if (length(as.vector(filtered_concs)) > 1) {
    #   # Set dropdown choices
    #   conc_choices <- c("Show all concentrations" = "", as.vector(filtered_concs))
    #   
    #   updateCheckboxInput(session, "conc", value = FALSE)
    #   
    #   showElement("conc")
    # } else {
    #   hideElement("conc")
    # }


  })
  
  
  observeEvent(c(input$college, input$level, input$major), {
    # req(input$level != "" & input$college != "")  # proceed only if level is chosen
    
    # Filter the degrees available for this level
    filtered_degrees_df <- enrollment_data() %>%
      filter(Coll == input$college,
             programtype == input$level,
             `Major Name` == input$major) %>%
      distinct(Degree) 
    
    
    if (nrow(filtered_degrees_df) > 1) {
      filtered_degrees <- filtered_degrees_df %>%
        pull(Degree)
      
      output$degree_type_ui <- renderUI({
        tagList(
          checkboxInput("degreeYN", label = "View major enrollment by degree", value = FALSE),
          selectizeInput("degree", label = "4. Filter by Degree", 
                         choices = c("Choose a degree type" = "", filtered_degrees),
                         selected = NULL)
        )
      })
    } else {
      output$degree_type_ui <- renderUI({ NULL })
      
      updateSelectInput(session, "degree", choices = "", selected = NULL)
      updateCheckboxInput(session, "degreeYN", value = FALSE)
    }
    
    ##################
    # if (nrow(filtered_degrees_df) > 1) {
    #   # Render the selectize input only if more than one option exists
    #   filtered_degrees <- filtered_degrees_df %>%
    #     pull(Degree)
    #   
    #   output$degree_type_ui <- renderUI({
    #     selectizeInput("degree", label = "4. Filter by Degree", choices = c("Choose a degree type" = "", filtered_degrees),
    #                    selected = NULL)
    #   })
    # } else {
    #   # Optionally render nothing or a hidden input
    #   output$degree_type_ui <- renderUI({
    #      NULL
    #   })
    # 
    #   # Optionally store the only value if needed downstream
    #   updateSelectInput(session, "degree", choices = "", selected = "")
    # }
  })
  
  
  # output$mytable <- DT::renderDataTable({
  #   DT::datatable(final_data(),
  #                 extensions = 'Buttons',
  #                 options = list(
  #                   dom = 'Bfrtip',
  #                   buttons = c('csv', 'excel'),
  #                   scrollX = TRUE
  #                 )
  #   )
  # })
  
  # observeEvent(input$level, {
  #   if (input$levelYN && input$level != "") {
  #     updateCheckboxInput(session, "levelYN", value = FALSE)
  #   }
  # })
  
  
  prevLevel    <- reactiveVal("")
  prevCheckbox <- reactiveVal(FALSE)

  observeEvent(list(input$level, input$levelYN), {
    lvl    <- input$level
    chk    <- input$levelYN
    oldLvl <- prevLevel()
    oldChk <- prevCheckbox()

    
    
    # 1) If the user just went from non-empty -> empty checkbox, clear the level
    if (oldLvl == "" & lvl != "" & chk) {
      updateCheckboxInput(session, "levelYN", value = FALSE)
    } else if (chk & (lvl != "")) {
       updateSelectizeInput(session, "level", selected = "")
      updateSelectizeInput(session, "college", selected = "")
      updateSelectizeInput(session, "major", selected = "")
      updateSelectizeInput(session, "degree", selected = "")
      updateCheckboxInput(session, "conc", value = FALSE)
      updateCheckboxInput(session, "collegeYN", value = FALSE)
      updateCheckboxInput(session, "majorYN", value = FALSE)
      hideElement("conc")
    
    } else if (chk) {
      updateSelectizeInput(session, "college", choices =  c("Show all colleges" = ""), selected = "")
      updateSelectizeInput(session, "major", choices =  c("Show all majors" = ""), selected = "")
      updateSelectizeInput(session, "degree", choices =  c("Choose a degree type" = ""), selected = "")
      updateCheckboxInput(session, "conc", value = FALSE)
    }
    
    # if (chk & (input$college == "")) {
    #   updateSelectizeInput(session, "college", c("Show all concentrations" = ""))
    # }
    
    
    # # 2) Else if they went from no-level -> level *while* box was checked, uncheck it
    # else if (oldLvl == "" && lvl != "" && chk) {
    #   updateCheckboxInput(session, "levelYN", value = FALSE)
    # }

    prevLevel(lvl)
    prevCheckbox(chk)
  }, ignoreInit = TRUE)

  prevColl    <- reactiveVal("")
  
  observeEvent(list(input$college, input$collegeYN), {
    col    <- input$college
    col_chk    <- input$collegeYN
    oldCol <- prevColl()
    
    # 1) If the user just went from non-empty -> empty checkbox, clear the level
    if (oldCol == "" & col != "" & col_chk) {
      updateCheckboxInput(session, "collegeYN", value = FALSE)
    } else if (col_chk & (col != "")) {
      updateSelectizeInput(session, "college", selected = "")
      updateSelectizeInput(session, "major", selected = "")
      updateSelectizeInput(session, "degree", selected = "")
      updateCheckboxInput(session, "conc", value = FALSE)
      updateCheckboxInput(session, "majorYN", value = FALSE)
      hideElement("conc")
      
    } else if (col_chk) {
      updateSelectizeInput(session, "major", choices =  c("Show all majors" = ""), selected = "")
      updateSelectizeInput(session, "degree", choices =  c("Choose a degree type" = ""), selected = "")
      updateCheckboxInput(session, "conc", value = FALSE)
    }
    
    
    prevColl(col)
  }, ignoreInit = TRUE)
  
  prevMajor    <- reactiveVal("")
  
  observeEvent(list(input$major, input$majorYN), {
    maj    <- input$major
    maj_chk    <- input$majorYN
    oldMajor <- prevMajor()
    
    # 1) If the user just went from non-empty -> empty checkbox, clear the level
    if (oldMajor == "" & maj != "" & maj_chk) {
      updateCheckboxInput(session, "majorYN", value = FALSE)
    } else if (maj_chk & (maj != "")) {
      updateSelectizeInput(session, "major", selected = "")
      updateSelectizeInput(session, "degree", selected = "")
      updateCheckboxInput(session, "conc", value = FALSE)
      hideElement("conc")
      
    } else if (maj_chk) {
      updateSelectizeInput(session, "degree", choices =  c("Choose a degree type" = ""), selected = "")
      updateCheckboxInput(session, "conc", value = FALSE)
    }
    
    
    prevMajor(maj)
  }, ignoreInit = TRUE)
  
  # by degree 
  prevDegree    <- reactiveVal("")
  observeEvent(list(input$degree, input$degreeYN, input$conc), {
  req(!is.null(input$degreeYN))
    
    deg    <- input$degree
    deg_chk    <- input$degreeYN
    oldDegree <- prevDegree()
    
    # 1) If the user just went from non-empty -> empty checkbox, clear the level
    if (oldDegree == "" & deg != "" & deg_chk  ) {
      updateCheckboxInput(session, "degreeYN", value = FALSE)
    } else if (deg_chk & (deg != "") ) {
      updateSelectizeInput(session, "degree", selected = "")
      updateCheckboxInput(session, "conc", value = FALSE)
      
    } 
    
    if (deg_chk & (input$conc) ) {
      updateCheckboxInput(session, "degreeYN", value = FALSE)
    }
    
    prevDegree(deg)
    
  }, ignoreInit = TRUE)
  


  final_data <- reactive({
    
    filtered_df <- enrollment_data()
    req(nrow(filtered_df) > 0)  # This line prevents the rest of the code if data is empty
    
    # case 1 - campus total
    if (input$college == "" & input$level == "") {
      if (input$levelYN) {
        final = filtered_df %>%
            filter(`Major Name` %in% c("Undergraduate", "Graduate","Professional")) %>% 
            select(-Degree) %>%
            rename(Degree = `Major Name`) %>%
            group_by(Degree) %>%
            summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep")
        } else {
          final = filtered_df %>%
            filter(`Major Name` %in% c("Campus total"))
        }
      
    } else if ((input$college == "") & (input$level != "")) { 
      # case 2 - degree total
      final <- filtered_df %>%
        filter(`Major Name` %in% c("Undergraduate", "Graduate","Professional")) %>%
        filter(`Major Name` == ifelse(input$level == "Nondegree", "Professional", input$level)) %>%
        ungroup()
    
    } else if ((input$college != "") & (input$level != "") & (input$major == "")) { 
      # case NEW - college and degree type total
      final <- filtered_df %>%
        # filter(Coll %in% c("KL") & (programtype %in% c("Undergraduate"))) %>%
        filter(Coll %in% c(input$college) & (programtype %in% c(input$level))) %>%
        select(-Degree) %>%
        mutate(Degree = unlist(college_abb_newest[Coll])) %>%
        group_by(Degree) %>%
        summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep") 
      
    } else if ((input$college != "") & (input$level != "") & (input$major != "")) { 
      # case 3 - college, degree type, and major (without conc)
      final <- filtered_df %>%
        # filter(Coll %in% c("KV") & (programtype %in% c("Undergraduate")) & (`Major Name` %in%  c("Statistics") )) %>%
        filter(Coll %in% c(input$college) & (`Major Name` %in%  c(input$major)) & (programtype %in% c(input$level))) %>%
        select(-Degree) %>%
        rename(Degree := `Major Name`) %>%
        group_by(Degree) %>%
        summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep")
      
      if (!is.null(input$degree) && input$degree != "") {
        final <- filtered_df %>%
          #  filter(Coll %in% c("KV") & (programtype %in% c("Graduate")) & (`Major Name` %in%  c("Statistics") ) & Degree == "MS") %>%
          filter(Coll %in% c(input$college) & (`Major Name` %in%  c(input$major)) &
                   (programtype %in%  c(input$level)) & (Degree %in% c(input$degree))) %>%
          group_by(Degree) %>%
          summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep")
      }
      
    } else {#else if ((input$college != "") & (input$level != "") & (input$major != "") & input$degree != ""){
      # selected degree
      # final <- filtered_df %>%
      # #  filter(Coll %in% c("KV") & (programtype %in% c("Graduate")) & (`Major Name` %in%  c("Statistics") ) & Degree == "MS") %>%
      #   filter(Coll %in% c(input$college) & (`Major Name` %in%  c(input$major)) &
      #                         (programtype %in%  c(input$level)) & (Degree %in% c(input$degree))) %>%
      #   group_by(Degree) %>%
      #   summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep")
      final <- data.frame()
    }
    
    # Jaqueline 
    if ((input$collegeYN) & input$level != "") {
      final <- filtered_df %>% # df
        filter(!(`Major Name` %in% c("Undergraduate", "Graduate","Professional", "Campus total"))) %>%
        filter(Degree != "College total") %>%
        #filter((programtype %in% c("Graduate")) ) %>%
        filter(programtype %in% c(input$level)) %>%
        group_by(Coll) %>%
        summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(Degree = unlist(college_abb_newest[Coll]))#names(colleges_vec[grepl(Coll, colleges_vec)])) 
        
    
      #unlist(college_abb_newest["KL"])
    }
    if ((input$majorYN) & input$college != "") {
      final <- filtered_df[ (filtered_df$Coll %in% c(input$college)) & (filtered_df$programtype %in%  c(input$level)), ] %>%
        select(-Degree) %>%
        rename(Degree = `Major Name`) %>%
        group_by(Degree) %>%
        summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep") 
      
      #unlist(college_abb_newest["KL"])
    }
    
    if ((input$conc)) {
      
      if (!is.null(input$degree) && input$degree != "") {
        final <- filtered_df[ (filtered_df$Coll %in% c(input$college)) & (filtered_df$`Major Name` %in%  c(input$major)) &
                                (filtered_df$programtype %in%  c(input$level)) & (filtered_df$Degree %in% c(input$degree)), ] %>%
          group_by(programtype, `Major Name`, `Concentration Name (if any)`) %>%
          summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep") %>%
          ungroup() %>%
          mutate(Degree = `Concentration Name (if any)`)
      } else {
        final <- filtered_df[ (filtered_df$Coll %in% c(input$college)) & (filtered_df$`Major Name` %in%  c(input$major)) &
                                (filtered_df$programtype %in%  c(input$level)), ] %>%
          group_by(programtype, `Major Name`, `Concentration Name (if any)`) %>%
          summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep") %>%
          ungroup() %>%
          mutate(Degree = `Concentration Name (if any)`)
      }
      
      # Add if else to include group by degree
    }
    
    if (!is.null(input$degreeYN) && input$degreeYN) {
      final <- filtered_df[ (filtered_df$Coll %in% c(input$college)) & (filtered_df$`Major Name` %in%  c(input$major)) &
                              (filtered_df$programtype %in%  c(input$level)), ] %>%
        group_by(Degree) %>%
        summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep") %>%
        ungroup() 
    }
    
    # # check level YN
    # if (input$levelYN) {
    #   final = filtered_df %>%
    #     filter(`Major Name` %in% c("Undergraduate", "Graduate","Professional")) %>% 
    #     #"Undergraduate"="Undergraduate", "Graduate"="Graduate","Professional"="Nondegree")) %>% 
    #     group_by(Degree,  programtype, `Major Name`) %>%
    #     summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep") 
    # }
    
    # if (!is.null(input$degree) && input$degree != "") {
    #   final <- final %>% filter(Degree == input$degree)
    # }
    return(final)
  })
  
  # output message
  output$nodata_msg <- renderUI({
    if (nrow(final_data()) == 0 & (input$conc) ) {
      tags$div(
        style = "color: red; font-weight: bold; padding: 10px;",
        "Please select a concentration to display results."
      )
    } else if (nrow(final_data()) == 0 & ((input$level != "") ) ) {
      tags$div(
        style = "color: red; font-weight: bold; padding: 10px;",
        "Please select a major to display results."
      )
    }else {
      NULL  # Don't show anything if data exists
    }
  })
  
  # Visualizations
  # Render the plot
  output$sexplot <- renderPlot({
    data <- final_data()
    req(nrow(data) > 0)  # This line prevents the rest of the code if data is empty
    
    # case 1 - campus total
    # if (input$college == "" & input$level == "") {
    #   sex_df = data %>%
    #     select(Degree, programtype, `Major Name`, Total, Men, Women, `Sex Unknown`) %>%
    #     rename(Unknown = `Sex Unknown`)  
    #   
    # } else if ((input$college == "") & (input$level != "") & (!input$collegeYN)) { 
    #   # case 2 - degree total
    #   sex_df = data %>%
    #     select(`Major Name`,Degree, Total, Men, Women, `Sex Unknown`) %>%
    #     rename(Unknown = `Sex Unknown`) 
    #   
    # } else if ((input$college != "") & (input$level != "") & (input$major == "") & (!input$collegeYN)) { 
    #   # case NEW - college and degree type total
    #   sex_df = data %>%
    #     select(programtype, Degree, Total, Men, Women, `Sex Unknown`) %>%
    #     rename(Unknown = `Sex Unknown`)  
    #   
    # } else if ((input$college != "") & (input$level != "") & (input$major != "") & (!input$collegeYN)) { 
    #   # case 3 - college, degree type, and major (without conc)
    #   sex_df = data %>%
    #     ungroup() %>%
    #     select(programtype, `Major Name`, Total, Men, Women, `Sex Unknown`) %>%
    #     rename(Unknown = `Sex Unknown`,
    #            Degree   = `Major Name`) 
    #     
    #   
    # } else {
    #   sex_df <- data %>%
    #     ungroup() %>%
    #     select(programtype, Degree, Total, Men, Women, `Sex Unknown`) %>%
    #     rename(Unknown = `Sex Unknown`,
    #            Degree   = `Major Name`) 
    # }
    # 
    # if (input$collegeYN & (input$level != "")) {
    #   sex_df = data %>%
    #     select(Degree, programtype, Total, Men, Women, `Sex Unknown`) %>%
    #     rename(Unknown = `Sex Unknown`)
    # 
    # } 
    # 
    # if ((input$conc)) {
    #   sex_df = data %>%
    #     select(Degree, programtype, Total, Men, Women, `Sex Unknown`) %>%
    #     rename(Unknown = `Sex Unknown`)
    # }
    
    
    ################################
    sex_df = data %>% 
          select(Degree, Total, Men, Women, `Sex Unknown`) %>%
          rename(Unknown = `Sex Unknown`)
      
    if ((nrow(sex_df) > 4) & input$college != "") {
      # sex_df %>%
      #   ungroup() %>%
      #   tidyr::pivot_longer(c("Men", "Women", "Unknown"), names_to = "sex", values_to = "total") %>%
      #   mutate(perc = round(100*total/Total, 1)) %>%
      #   ggplot(aes(y=Degree, x=perc, fill=factor(sex))) +
      #   geom_col() +
      #   theme_minimal() +
      #   theme(axis.title.y = element_blank()) +
      #   labs(x = "Total",
      #        fill = "Gender") + 
      #   scale_y_discrete(drop = TRUE) +
      #   geom_text(
      #     aes(label = ifelse(perc > 50.0, paste0(perc, "%"), "")),
      #     position = position_stack(vjust = 0.5),
      #     hjust = 0.5,
      #     color = "black"
      #   ) +
      #   scale_fill_manual(values = c("Men" = "#4A90E2",    # Blue
      #                                "Women" = "#FF69B4",  # Pink
      #                                "Unknown" = "#B0B0B0")) # Grey
      sex_df %>%
        ungroup() %>%
        tidyr::pivot_longer(
          c("Men", "Women", "Unknown"),
          names_to = "sex",
          values_to = "total"
        ) %>%
        mutate(perc = round(100 * total / Total, 1),
               Degree = reorder(Degree, Total)) %>%
        ggplot(aes(y = Degree, x = perc, fill = factor(sex))) +
        geom_col() +
        theme_minimal() +
      #  theme(axis.title.y = element_blank()) +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_text(size = 10),
              axis.text.x = element_text(size = 10),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10)) +
        labs(x = "Percentage & (total)", fill = "Gender") +
        scale_y_discrete(drop = TRUE) +
        
        # Label inside stacks (optional, as you already had)
        geom_text(
          aes(label = ifelse(perc > 50.0, paste0(perc, "%"), "")),
          position = position_stack(vjust = 0.5),
          color = "black"
        ) +
        
        # Add total label at top of each bar
        geom_text(
          data = function(df) df %>%
            group_by(Degree) %>%
            summarise(total_label = paste0("",scales::comma(sum(total))), .groups = "drop"),
          aes(y = Degree, x = 105, label = total_label),  # x=100 since perc is on x-axis
          inherit.aes = FALSE,
          vjust = 0.5
        ) +
        
        scale_fill_manual(values = c(
          "Men" = "#4A90E2",    
          "Women" = "#FF69B4",  
          "Unknown" = "#B0B0B0"
        )) +
        xlim(0,107) +
        theme(legend.position = "right", legend.justification = "top")
      
    } else {
      # p <- sex_df %>%
      #   tidyr::pivot_longer(c(Men, Women, Unknown), names_to = "sex", values_to = "total") %>%
      #   mutate(perc = round(100*total/Total, 1)) %>%
      #   mutate(sex = factor(sex, ordered = TRUE, levels = c("Men", "Women", "Unknown"))) %>%
      #   ggplot(aes(x=Degree, y=perc, fill=factor(sex, ordered = TRUE, levels = c("Men", "Women", "Unknown")))) +
      #   geom_col() +
      #   #facet_wrap(~ Degree, scales = "free_y") +  # Allows different total heights
      #   theme_minimal() +
      #   theme(
      #     axis.title.x = element_blank()#,
      #     #axis.text.x = element_blank(),
      #     #axis.ticks.x = element_blank()
      #   ) +
      #   labs(y = "Percentage",
      #        fill = "Sex") + 
      #   scale_x_discrete(drop = TRUE) +
      #   geom_text(aes(label = ifelse(perc > 1.0, paste0(perc, "%"), "")), position = position_stack(vjust=0.5)) +
      #   scale_fill_manual(values = c("Men" = "#4A90E2",    # Blue
      #                                "Women" = "#FF69B4",  # Pink
      #                                "Unknown" = "#B0B0B0"))  # Grey
      # p
      
      sex_df %>%
        ungroup() %>%
        tidyr::pivot_longer(
          c("Men", "Women", "Unknown"),
          names_to = "sex",
          values_to = "total"
        ) %>%
        mutate(perc = round(100 * total / Total, 1),
               Degree = reorder(Degree, -Total)) %>%
        ggplot(aes(x = Degree, y = perc, fill = factor(sex))) +
        geom_col() +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              axis.text.y = element_text(size = 10),
              axis.text.x = element_text(size = 10),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10)) +
        labs(y = "Total", fill = "Gender") +
        scale_x_discrete(drop = TRUE) +
        
        # Label inside stacks (optional, as you already had)
        geom_text(
          aes(label = ifelse(perc > 50.0, paste0(perc, "%"), "")),
          position = position_stack(vjust = 0.5),
          color = "black"
        ) +
        
        # Add total label at top of each bar
        geom_text(
          data = function(df) df %>%
            group_by(Degree) %>%
            summarise(total_label = paste0("",scales::comma(sum(total))), .groups = "drop"),
          aes(x = Degree, y = 105, label = total_label),  # x=100 since perc is on x-axis
          inherit.aes = FALSE,
          vjust = 0.5
        ) +
        
        scale_fill_manual(values = c(
          "Men" = "#4A90E2",    
          "Women" = "#FF69B4",  
          "Unknown" = "#B0B0B0"
        )) +
        ylim(0,115)
      
    }
    

  })
  
  output$raceplot <- renderPlot({
    data <- final_data()
    req(nrow(data) > 0)  # This line prevents the rest of the code if data is empty
    
    # # case 1 - campus total
    # if (input$college == "" & input$level == "") {
    #   race_df = data %>%
    #     ungroup() %>%
    #     #select(Coll, everything()) %>%
    #     select("Degree", "programtype", "Major Name", "Total", "Caucasian":"Race Unknown") %>%
    #            # "Asian American","African American","Hispanic","Native American","Hawaiian/Pacific Isl",
    #            # "Multiracial", "International", "Race Unknown") %>%
    #     rename(Unknown = `Race Unknown`)  
    #   
    # } else if ((input$college == "") & (input$level != "") & (!input$collegeYN)) { 
    #   # case 2 - degree total
    #   race_df = data %>%
    #     #select(everything()) %>%
    #     ungroup() %>%
    #     select(`Major Name`,Degree, "Total", "Caucasian":"Race Unknown") %>%
    #            #"Caucasian","Asian American","African American","Hispanic","Native American","Hawaiian/Pacific Isl","Multiracial", "International", "Race Unknown") %>%
    #     rename(Unknown = `Race Unknown`) 
    #   
    # } else if ((input$college != "") & (input$level != "") & (input$major == "")) { 
    #   # case NEW - college and degree type total
    #   race_df = data %>%
    #    # select(Coll, everything()) %>%
    #     ungroup() %>%
    #     select(any_of(c("programtype", "Degree", "Major Name", "Concentration Name (if any)", "Total", 
    #                     "Caucasian","Asian American","African American","Hispanic","Native American","Hawaiian/Pacific Isl","Multiracial", "International", "Race Unknown"))) %>%
    #     #select("Coll", "programtype", "Degree", "Total", "Caucasian":"Race Unknown") %>%
    #            #"Caucasian","Asian American","African American","Hispanic","Native American","Hawaiian/Pacific Isl","Multiracial", "International", "Race Unknown") %>%
    #     rename(Unknown = `Race Unknown`)  
    #   
    # } else if ((input$college != "") & (input$level != "") & (input$major != "") ) { 
    #   # case 3 - college, degree type, and major (without conc)
    #   race_df = data %>%
    #     ungroup() %>%
    #     #select(Coll, everything()) %>%
    #     select("programtype", "Major Name", "Total", "Caucasian":"Race Unknown") %>%
    #            #"Caucasian","Asian American","African American","Hispanic","Native American","Hawaiian/Pacific Isl","Multiracial", "International", "Race Unknown") %>%
    #     rename(Unknown = `Race Unknown`,
    #            Degree   = `Major Name`) 
    #   
    # } else {
    #   race_df <- NULL
    # }
    # 
    # 
    # 
    # if ((input$conc)) {
    #   race_df = data %>%
    #     select(Degree, "Total", "Caucasian":"Race Unknown") %>%
    #     rename(Unknown = `Race Unknown`)
    # }
    # race_df = data %>%
    #   ungroup() %>%
    #   select(Degree, "Total", "Caucasian":"Race Unknown") %>%
    #   rename(Unknown = `Race Unknown`) 
    # 
    # if ((input$collegeYN & input$level != "") | input$majorYN) {
    #   
    
    
      race_df = data %>%
        #select(everything()) %>%
        ungroup() %>%
        select(Degree, "Total", "Caucasian":"Race Unknown") %>%
        #"Caucasian","Asian American","African American","Hispanic","Native American","Hawaiian/Pacific Isl","Multiracial", "International", "Race Unknown") %>%
        rename(Unknown = `Race Unknown`) 
      
      pivot_cols <- race_df %>% 
        select(where(is.numeric))  %>% #select("Caucasian":"Unknown") %>% colnames(.)
        colnames(.)
      pivot_cols <- setdiff(pivot_cols, "Total")
      
      
      col_values = c("African American" = "#C5B0D5", "Asian American" = "#FF9896",
        "Caucasian"="#17BECF", "Hawaiin/Pacific Isl"="#D62728",
        "Hispanic"="#98DF8A", "International"="#FF7F0E",
        "Multiracial"="#BCBD22", "Native American"="#006BA4", 
        "Unknown"="#595959")
      
      race_df %>%
        ungroup() %>%
        tidyr::pivot_longer(all_of(pivot_cols), names_to = "race", values_to = "total") %>%
        mutate(perc = round(100 * total / Total, 1),
               Degree = reorder(Degree, Total)) %>%
        ggplot(aes(y = Degree, x = perc, fill = factor(race))) +
        geom_col() +
        theme_minimal() +
        #  theme(axis.title.y = element_blank()) +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_text(size = 10),
              axis.text.x = element_text(size = 10),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10)) +
        labs(x = "Percentage & (total)", fill = "Race") +
        scale_y_discrete(drop = TRUE) +
        
        # Label inside stacks (optional, as you already had)
        geom_text(
          aes(label = ifelse(perc > 5.0, paste0(perc, "%"), "")),
          position = position_stack(vjust = 0.5),
          color = "black"
        ) +
        
        # Add total label at top of each bar
        geom_text(
          data = function(df) df %>%
            group_by(Degree) %>%
            summarise(total_label = paste0("",scales::comma(sum(total))), .groups = "drop"),
          aes(y = Degree, x = 105, label = total_label),  # x=100 since perc is on x-axis
          inherit.aes = FALSE,
          vjust = 0.5
        ) +
        
        scale_fill_manual(values = 
                            col_values
                           # setNames(palette.colors(palette = "Okabe-Ito"), sort(unique(pivot_cols), decreasing = TRUE))
                          ) +
        xlim(0,107) +
        theme(legend.position = "right", legend.justification = "top")
    #   
    # 
    # } else {
    #   race_df = data %>%
    #     #select(everything()) %>%
    #     ungroup() %>%
    #     select(Degree, "Total", "Caucasian":"Race Unknown") %>%
    #     #"Caucasian","Asian American","African American","Hispanic","Native American","Hawaiian/Pacific Isl","Multiracial", "International", "Race Unknown") %>%
    #     rename(Unknown = `Race Unknown`) 
    #   
    #   pivot_cols <- race_df %>% 
    #     select(where(is.numeric))  %>% #select("Caucasian":"Unknown") %>% colnames(.)
    #     colnames(.)
    #   pivot_cols <- setdiff(pivot_cols, "Total")
    #   
    #   race_df %>%
    #     ungroup() %>%
    #     tidyr::pivot_longer(all_of(pivot_cols), names_to = "race", values_to = "total") %>%
    #     mutate(perc = round(100 * total / Total, 1),
    #            Degree = reorder(Degree, Total)) %>%
    #     ggplot(aes(x = Degree,  y= perc, fill = factor(race))) +
    #     geom_col() +
    #     theme_minimal() +
    #     #  theme(axis.title.y = element_blank()) +
    #     theme(axis.title.x = element_blank(),
    #           axis.text.y = element_text(size = 10),
    #           axis.text.x = element_text(size = 10),
    #           legend.text = element_text(size = 10),
    #           legend.title = element_text(size = 10)) +
    #     labs(y = "Percentage & (total)", fill = "Race") +
    #     scale_x_discrete(drop = TRUE) +
    #     
    #     # Label inside stacks (optional, as you already had)
    #     geom_text(
    #       aes(label = ifelse(perc > 5.0, paste0(perc, "%"), "")),
    #       position = position_stack(vjust = 0.5),
    #       color = "black"
    #     ) +
    #     
    #     # Add total label at top of each bar
    #     geom_text(
    #       data = function(df) df %>%
    #         group_by(Degree) %>%
    #         summarise(total_label = paste0("",scales::comma(sum(total))), .groups = "drop"),
    #       aes(x = Degree, y = 105, label = total_label),  # x=100 since perc is on x-axis
    #       inherit.aes = FALSE,
    #       vjust = 0.5
    #     ) +
    #     
    #     scale_fill_manual(values = setNames(palette.colors(palette = "Okabe-Ito"), sort(unique(pivot_cols), decreasing = TRUE))) +
    #     ylim(0,107) +
    #     theme(legend.position = "right", legend.justification = "top")

    # }
    
      
    # pivot_cols <- race_df %>% 
    #   select(where(is.numeric))  %>% #select("Caucasian":"Unknown") %>% colnames(.)
    #   colnames(.)
    # pivot_cols <- setdiff(pivot_cols, "Total")
    # 
    # race_df %>%
    #   ungroup() %>%
    #   tidyr::pivot_longer(all_of(pivot_cols), names_to = "race", values_to = "total") %>% 
    #     #c("Caucasian","Asian American","African American","Hispanic","Native American","Hawaiian/Pacific Isl","Multiracial", "International", "Unknown"),
    #   mutate(perc = round(100*total/Total, 2)) %>%
    # ggplot(aes(x = reorder(race,-total), y=total, fill = race)) +
    #   geom_bar(stat="identity") +
    #   theme_minimal() +
    #   guides(x =  guide_axis(angle = 45)) +
    #   theme(legend.position = "none") +
    #   geom_text(aes(label = paste0(perc, "%")), position = position_stack(vjust=0.5), size=3) +
    #   facet_wrap(~ Degree, scales = "free_y") +
    #   labs(x = "Race",
    #        y = "Total")
    
  })
  
  output$resplot <- renderPlot({
    
    data <- final_data()
    req(nrow(data) > 0)  # This line prevents the rest of the code if data is empty
    
    
    # case 1 - campus total
    # if (input$college == "" & input$level == "") {
    #   residency_df = data %>%
    #     select(Degree, programtype, `Major Name`, Total, "Illinois", "Non-Illinois") 
    #   
    # } else if ((input$college == "") & (input$level != "") & (!input$collegeYN)) { 
    #   # case 2 - degree total
    #   residency_df = data %>%
    #     select(`Major Name`,Degree, Total, "Illinois", "Non-Illinois")
    #   
    # } else if ((input$college != "") & (input$level != "") & (input$major == "")) { 
    #   # case NEW - college and degree type total
    #   residency_df = data %>%
    #     select(programtype, Degree, Total, "Illinois", "Non-Illinois") 
    #   
    # } else if ((input$college != "") & (input$level != "") & (input$major != "") ) { 
    #   # case 3 - college, degree type, and major (without conc)
    #   residency_df = data %>%
    #     ungroup() %>%
    #     select(programtype, `Major Name`, Total, "Illinois", "Non-Illinois") %>%
    #     rename(Degree   = `Major Name`) 
    #   
    # } else {
    #   residency_df <- NULL
    # }
    # 
    # if (input$collegeYN & input$level != "") {
    #   residency_df = data %>%
    #     select(Degree, Total, "Illinois", "Non-Illinois")
    # } 
    # 
    # if ((input$conc)) {
    #   residency_df = data %>%
    #     select(Degree, Total, "Illinois", "Non-Illinois")
    # }
    residency_df = data %>%
      ungroup() %>%
          select(Degree, Total, "Illinois", "Non-Illinois")

    
    if ((nrow(residency_df) > 4 ) & input$college != "") {
      
      residency_df %>%
        ungroup() %>%
        tidyr::pivot_longer(c("Illinois", "Non-Illinois"), names_to = "residency", values_to = "total") %>%
        mutate(perc = round(100 * total / Total, 1),
               Degree = reorder(Degree, Total)) %>%
        ggplot(aes(y = Degree, x = perc, fill = factor(residency))) +
        geom_col() +
        theme_minimal() +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_text(size = 10),
              axis.text.x = element_text(size = 10),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10)) +
        labs(x = "Total", fill = "Residency") +
        scale_y_discrete(drop = TRUE) +
        
        # Label inside stacks (optional, as you already had)
        geom_text(
          aes(label = ifelse(perc > 50.0, paste0(perc, "%"), "")),
          position = position_stack(vjust = 0.5),
          color = "black"
        ) +
        
        # Add total label at top of each bar
        geom_text(
          data = function(df) df %>%
            group_by(Degree) %>%
            summarise(total_label = paste0("",scales::comma(sum(total))), .groups = "drop"),
          aes(y = Degree, x = 105, label = total_label),  # x=100 since perc is on x-axis
          inherit.aes = FALSE,
          vjust = 0.5
        ) +
        
        scale_fill_manual(values = c("Illinois" = "#E84A27",    # Orange
                                     "Non-Illinois" = "#13294B"))  +
        xlim(0,115) +
        theme(legend.position = "right", legend.justification = "top")
      
      ################ old below $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

    } else {
      
      residency_df %>%
        ungroup() %>%
        tidyr::pivot_longer(c("Illinois", "Non-Illinois"), names_to = "residency", values_to = "total") %>%
        mutate(perc = round(100 * total / Total, 1),
               Degree = reorder(Degree, -Total)) %>%
        ggplot(aes(x = Degree, y = perc, fill = factor(residency))) +
        geom_col() +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              axis.text.y = element_text(size = 10),
              axis.text.x = element_text(size = 10),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10)) +
        labs(y = "Total", fill = "Residency") +
        scale_x_discrete(drop = TRUE) +
        
        # Label inside stacks (optional, as you already had)
        geom_text(
          aes(label = ifelse(perc > 50.0, paste0(perc, "%"), "")),
          position = position_stack(vjust = 0.5),
          color = "black"
        ) +
        
        # Add total label at top of each bar
        geom_text(
          data = function(df) df %>%
            group_by(Degree) %>%
            summarise(total_label = paste0("",scales::comma(sum(total))), .groups = "drop"),
          aes(x = Degree, y = 105, label = total_label),  # x=100 since perc is on x-axis
          inherit.aes = FALSE,
          vjust = 0.5
        ) +
        
        scale_fill_manual(values = c("Illinois" = "#E84A27",    # Orange
                                     "Non-Illinois" = "#13294B"))  +
        ylim(0,115)
      
      

    }

  })
  
  # Render the plot
  output$URM_plot <- renderPlot({
    data <- final_data()
    # Ensure data is not empty and "URM" column exists
    req(nrow(data) > 0, "URM" %in% colnames(data))
  #   
  # if (!(input$collegeYN | input$majorYN )) {
  #   if (((input$college == "") & (input$level != ""))) {
  #     URM_df = data %>%
  #       select(`Major Name`,Degree, Total, URM) %>%
  #       mutate(`Not URM` = Total - URM) %>%
  #       pivot_longer(c("Not URM", "URM"), names_to = "group", values_to = "total")
  #     
  #   } else if ("Concentration Name (if any)" %in% colnames(data)) {
  #     URM_df = data %>%
  #       select(programtype, Degree,`Major Name`, `Concentration Name (if any)`, Total, URM) %>%
  #       mutate(`Not URM` = Total - URM) %>%
  #       pivot_longer(c("Not URM", "URM"), names_to = "group", values_to = "total")
  #   } else if (!("Degree" %in% colnames(data) & "Major Name" %in% colnames(data))) {
  #     # NEW
  #     URM_df = data %>%
  #       select(programtype, Degree, Total, URM) %>%
  #       mutate(`Not URM` = Total - URM) %>%
  #       pivot_longer(c("Not URM", "URM"), names_to = "group", values_to = "total")
  #   } else if (!("Degree" %in% colnames(data))) {
  #     URM_df = data %>%
  #       select(programtype, Degree, Total, URM) %>%
  #       mutate(`Not URM` = Total - URM) %>%
  #       pivot_longer(c("Not URM", "URM"), names_to = "group", values_to = "total")
  #   } else {
  #     URM_df = data %>%
  #       #ungroup
  #       select(programtype, Degree, `Major Name`, Total, URM) %>%
  #       mutate(`Not URM` = Total - URM) %>%
  #       pivot_longer(c("Not URM", "URM"), names_to = "group", values_to = "total")
  #   }
  # }
  #   
    # if ((input$conc)) {
    #   URM_df = data %>%
    #     select(Degree, Total, URM) %>%
    #     mutate(`Not URM` = Total - URM) %>%
    #     pivot_longer(c("Not URM", "URM"), names_to = "group", values_to = "total") %>%
    #     ungroup()
    # }
    
    # URM_df = data %>%
    #   select(Degree, Total, URM) %>%
    #   mutate(`Not URM` = Total - URM) %>%
    #   pivot_longer(c("Not URM", "URM"), names_to = "group", values_to = "total") %>%
    #   ungroup()
    # 
    # urm_length <- nrow(URM_df)
    # 
    # if (urm_length > 4) {
    #   
    #   
    #   # Jaqueline Tuesday
    #   if (input$collegeYN) {
    #     
    #     URM_df %>%
    #       mutate(perc = round(100 * total / Total, 1),
    #              Degree = reorder(Degree, -Total)) %>%
    #       ggplot(aes(x = Degree, y = perc, fill = factor(group))) +
    #       geom_col() +
    #       theme_minimal() +
    #       theme(axis.title.x = element_blank(),
    #             axis.text.y = element_text(size = 10),
    #             axis.text.x = element_text(size = 10),
    #             legend.text = element_text(size = 10),
    #             legend.title = element_text(size = 10)) +
    #       labs(y = "Total", fill = "group") +
    #       scale_x_discrete(drop = TRUE) +
    #       
    #       # Label inside stacks (optional, as you already had)
    #       geom_text(
    #         aes(label = ifelse(perc > 50.0, paste0(perc, "%"), "")),
    #         position = position_stack(vjust = 0.5),
    #         color = "black"
    #       ) +
    #       
    #       # Add total label at top of each bar
    #       geom_text(
    #         data = function(df) df %>%
    #           group_by(Degree) %>%
    #           summarise(total_label = paste0("",scales::comma(sum(total))), .groups = "drop"),
    #         aes(x = Degree, y = 105, label = total_label),  # x=100 since perc is on x-axis
    #         inherit.aes = FALSE,
    #         vjust = 0.5
    #       ) +
    #       scale_fill_manual("", values = (c("mediumpurple", "lightgreen"))) +
    #       ylim(0,115)
    #     
    #   
    #   } else {
        URM_df = data %>%
          select(Degree, Total, URM) %>%
          mutate(`Not URM` = Total - URM) %>%
          pivot_longer(c("Not URM", "URM"), names_to = "group", values_to = "total") %>%
          ungroup()
        
        urm_length <- nrow(URM_df)
        
        if (urm_length > 4) {
          
          
          # Jaqueline Tuesday
          # if ((!input$collegeYN) | input$collegeYN) {
          #   
            URM_plot <- URM_df %>%
              mutate(perc = round(100 * total / Total, 1),
                     Degree = reorder(Degree, Total)) %>%
              ggplot(aes(y = Degree, x = perc, fill = factor(group))) +
              geom_col() +
              theme_minimal() +
              theme(axis.title.y = element_blank(),
                    axis.text.y = element_text(size = 10),
                    axis.text.x = element_text(size = 10),
                    legend.text = element_text(size = 10),
                    legend.title = element_text(size = 10)) +
              labs(x = "Total", fill = "group") +
              scale_y_discrete(drop = TRUE) +
              
              # Label inside stacks (optional, as you already had)
              geom_text(
                aes(label = ifelse(perc > 50.0, paste0(perc, "%"), "")),
                position = position_stack(vjust = 0.5),
                color = "black"
              ) +
              
              # Add total label at top of each bar
              geom_text(
                data = function(df) df %>%
                  group_by(Degree) %>%
                  summarise(total_label = paste0("",scales::comma(sum(total))), .groups = "drop"),
                aes(y = Degree, x = 108, label = total_label),  # x=100 since perc is on x-axis
                inherit.aes = FALSE,
                vjust = 0.5
              ) +
              scale_fill_manual("", values = (c("mediumpurple", "lightgreen"))) +
              xlim(0,115)
            
            if (input$majorYN) {
              URM_plot + theme(legend.position = "right", legend.justification = "top")
            } else {
              URM_plot
            }
        # URM_df %>%
        #   ungroup() %>%
        #   mutate(perc = round(100*total/Total, 1)) %>%
        #   ggplot(aes(y=Degree, x=perc, fill=(factor(group, levels = c("URM", "Not URM"), ordered = TRUE)))) +
        #   geom_col() +
        #   theme_minimal() +
        #   theme(axis.title.y = element_blank()) +
        #   labs(x = "Total",
        #        fill = "group") + 
        #   scale_y_discrete(drop = TRUE) +
        #   geom_text(
        #     aes(label = ifelse(perc >= 5.0, paste0(perc, "%"), "")),
        #     position = position_stack(vjust = 0.5),
        #     hjust = 0.5,
        #     color = "black"
        #   ) + 
        #   scale_fill_manual("", values = (c("mediumpurple", "lightgreen"))) 
          # } else {
          #   URM_df %>%
          #     mutate(perc = round(100 * total / Total, 1),
          #            Degree = reorder(Degree, -Total)) %>%
          #     ggplot(aes(x = Degree, y = perc, fill = factor(group))) +
          #     geom_col() +
          #     theme_minimal() +
          #     theme(axis.title.x = element_blank(),
          #           axis.text.y = element_text(size = 10),
          #           axis.text.x = element_text(size = 10),
          #           legend.text = element_text(size = 10),
          #           legend.title = element_text(size = 10)) +
          #     labs(y = "Total", fill = "group") +
          #     scale_x_discrete(drop = TRUE) +
          #     
          #     # Label inside stacks (optional, as you already had)
          #     geom_text(
          #       aes(label = ifelse(perc > 50.0, paste0(perc, "%"), "")),
          #       position = position_stack(vjust = 0.5),
          #       color = "black"
          #     ) +
          #     
          #     # Add total label at top of each bar
          #     geom_text(
          #       data = function(df) df %>%
          #         group_by(Degree) %>%
          #         summarise(total_label = paste0("",scales::comma(sum(total))), .groups = "drop"),
          #       aes(x = Degree, y = 105, label = total_label),  # x=100 since perc is on x-axis
          #       inherit.aes = FALSE,
          #       vjust = 0.5
          #     ) +
          #     scale_fill_manual("", values = (c("mediumpurple", "lightgreen"))) +
          #     ylim(0,115)
          # }
      #guides(x =  guide_axis(angle = -90)) 
      
      # BOOKMARK JAQUELINE
          
    } else {
      # Step 1: Add percentage column to each subset
      URM_df <- URM_df %>%
        group_by(Degree) %>%
        mutate(perc = total / sum(total) * 100,
               label = paste0(round(perc, 1), "%")) %>%
        ungroup()
      
      # Step 2: Create pie charts with percentage labels
      degree_list <- split(URM_df, URM_df$Degree)
      
      plot_list <- lapply(degree_list, function(df) {
        ggplot(df, aes(x = "", y = total, fill = group)) +
          geom_col(width = 1) +
          coord_polar(theta = "y") +
          geom_text(aes(label = ifelse(perc > 50.0, paste0(round(perc, 1), "%"), "")), 
                    position = position_stack(vjust = 0.5),
                    hjust = 0.1,     # <-- nudges labels to the right
                    size = 4, color = "black") +
          theme_void() +
          
          labs(title = unique(df$Degree)) +
          scale_fill_manual("", values = c("mediumpurple", "lightgreen"))
      })
      
      # Step 3: Arrange plots
      grid.arrange(grobs = plot_list, ncol = ifelse(length(plot_list) == 1, 1, 2))
    }
    
  #   # Create pie chart
  # if ((input$collegeYN | input$majorYN) ) {
  # 
  #   
  # } else {
  #   # Step 1: Add percentage column to each subset
  #   URM_df <- URM_df %>%
  #     group_by(Degree) %>%
  #     mutate(perc = total / sum(total) * 100,
  #            label = paste0(round(perc, 1), "%")) %>%
  #     ungroup()
  #   
  #   # Step 2: Create pie charts with percentage labels
  #   degree_list <- split(URM_df, URM_df$Degree)
  #   
  #   plot_list <- lapply(degree_list, function(df) {
  #     ggplot(df, aes(x = "", y = total, fill = group)) +
  #       geom_col(width = 1) +
  #       coord_polar(theta = "y") +
  #       geom_text(aes(label = ifelse(perc > 50.0, paste0(round(perc, 1), "%"), "")), 
  #                 position = position_stack(vjust = 0.5),
  #                 hjust = 0.1,     # <-- nudges labels to the right
  #                 size = 4, color = "black") +
  #       theme_void() +
  #       
  #       labs(title = unique(df$Degree)) +
  #       scale_fill_manual("", values = rev(c("mediumpurple", "lightgreen")))
  #   })
  #   
  #   # Step 3: Arrange plots
  #   grid.arrange(grobs = plot_list, ncol = ifelse(length(plot_list) == 1, 1,2 ))
  # }
    
    
  })
  
  output$summary_text <- renderUI({
    
    data <- final_data()
    total <- sum(data$Total, na.rm = TRUE)
    firstsentence <- paste0("During the <i>", input$semester," ", input$year, "</i> semester, there were <b>", scales::comma(total), "</b>")
    
    # case 1 - campus total
    if (input$college == "" & input$level == "") {
      if (input$levelYN) {
        text <- paste(firstsentence, " students enrolled at the <u>University of Illinois at Urbana-Champaign</u> across 3 degree levels: Undergraduate, Graduate, and Non-degree (Professional).")

      } else {
        text <- paste(firstsentence, " students enrolled at the <u>University of Illinois at Urbana-Champaign</u>.")
        
      }
      
    } else if ((input$college == "") & (input$level != "")) { 
      # case 2 - degree total
      text <- paste0(firstsentence, " students enrolled at the <u>", (input$level), " level</u>.")
      
      
    } else if ((input$college != "") & (input$level != "") & (input$major == "")) { 
      # case NEW - college and degree type total
      text <- paste0(firstsentence, " <i>", (input$level), "</i> students enrolled in the <u>college of ", unlist(college_abb_newest[(input$college)]),
                     "</u>.") 
      
    } else if ((input$college != "") & (input$level != "") & (input$major != "")) { 
      # case 3 - college, degree type, and major (without conc)
      text <- paste0(firstsentence, " <i>", (input$level), "</i> students enrolled in the <u>", input$major, " major</u>.")
      
      if (!is.null(input$degree) && input$degree != "") {
        text <- paste0(firstsentence, " <i>", (input$degree), "</i> students enrolled in the <u>", input$major, " major</u>.")
      }
      
    } else {#else if ((input$college != "") & (input$level != "") & (input$major != "") & input$degree != ""){
      # selected degree
      # final <- filtered_df %>%
      # #  filter(Coll %in% c("KV") & (programtype %in% c("Graduate")) & (`Major Name` %in%  c("Statistics") ) & Degree == "MS") %>%
      #   filter(Coll %in% c(input$college) & (`Major Name` %in%  c(input$major)) &
      #                         (programtype %in%  c(input$level)) & (Degree %in% c(input$degree))) %>%
      #   group_by(Degree) %>%
      #   summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep")
      text <- paste("")
    }
    
    
    ### Fix all texts below and doublecheck above
    # Jaqueline 
    if ((input$collegeYN) & input$level != "") {
      text <- paste0(firstsentence, " students enrolled at the <u>", (input$level), " level</u> across ",
               length(data$Degree), " colleges.")
      
      #unlist(college_abb_newest["KL"])
    }
    if ((input$majorYN) & input$college != "") {
      text <- paste0(firstsentence, " <i>", (input$level), "</i> students enrolled in the <u>college of ", unlist(college_abb_newest[(input$college)]),
                     "</u> across ", length(data$Degree),  " majors.")
      
      #unlist(college_abb_newest["KL"])
    }
    
    if ((input$conc)) {
      
      if (!is.null(input$degree) && input$degree != "") {
        text <- paste0(firstsentence, " <i>", (input$degree), "</i> students enrolled in the <u>", 
               input$major, " major</u>  across ", length(data$Degree), " concentrations.")
      } else {
        text = paste0(firstsentence, " <i>", (input$level), "</i> students enrolled in the <u>", 
                      input$major, " major</u> across ", length(data$Degree), " concentrations.")
      }
      
      # Add if else to include group by degree
    }
    
    if (!is.null(input$degreeYN) && input$degreeYN) {
      text = paste0(firstsentence, " <i>", (input$level), "</i> students enrolled in the <u>", 
                    input$major, " major</u> across ", length(data$Degree), " degrees.")
    }
    ###########
    
    # if (input$college == "" & input$level == "") {
    #   # Case 1 - campus total
    #   if (input$levelYN) { 
    #     # by program type
    #     data_ord = arrange(data, desc(Total))
    #     text <- paste(firstsentence, paste(c(paste0(". There were ", data_ord[1, "Total"], " students enrolled in a ", data_ord[1, "Major Name"], " program"), 
    #                                    paste(sapply(2:nrow(data_ord), function(x) paste0(data_ord[x, "Total"], " students enrolled in a ",
    #                                                                                      data_ord[x, "Major Name"], " program.")), collapse = ", and ")), collapse = ", "))
    #   } else {
    #     text <- paste(firstsentence, " students enrolled in the University of Illinois at Urbana-Champaign campus.")
    #   }
    #   
    # }  else if (nrow(data) == 0){
    #   return("")
    #   
    # } else if ((input$college == "") & (input$level != "")) {
    #   # Case 2 - degree total
    #   text <- paste(firstsentence, "students enrolled in an", input$level, "program.")
    # 
    #  
    # 
    # } else if ((input$college != "") & (input$level != "") & (input$major == "")) {
    #   # Case NEWWWW - major without concentration total
    #   text <- paste0(firstsentence, " students enrolled in a ", tolower(input$level), 
    #                 " program through the college of ", names(colleges_vec[grepl(input$college, colleges_vec)]), ".")
    #   
    # } else if (input$college != "" & input$level != "" ) {
    #   # Case 3 - major without concentration total
    #   text <- paste0(firstsentence, " students enrolled in a ", tolower(input$level), 
    #                  " program with a major in ",  input$major, " through the college of ",
    #                  names(colleges_vec[grepl(input$college, colleges_vec)]), ".")
    #   
    # } else if (input$college != "" & input$level != "" ) { # & input$conc != "None"
    #   # Case 4a - major with concentration total
    # 
    #   text <- paste0(firstsentence, " students enrolled in a ", tolower(input$level), 
    #                  " program with a major in ",  input$major, ", ", input$conc,  " concentration through the college of ",
    #                  names(colleges_vec[grepl(input$college, colleges_vec)]), ".")
    #   
    # } else if (input$college != "" & input$level != "" ) {  #& input$conc == "None"
    #   # Case 4b - major with concentration total (edited text to remove mention of conc when equals "None")
    #   text <-  paste0(firstsentence, " students enrolled in a ", tolower(input$level), 
    #                   " program with a major in ",  input$major, " through the college of ",
    #                   names(colleges_vec[grepl(input$college, colleges_vec)]), ".")
    #   
    # }
    # else {
    #   text <- "No data available for the selected criteria."
    #   
    # }
    # # check if student(s)
    # # graduate college
    # # return
    # 
    # if (grepl("were 1 students", text)) {
    #   text <- str_replace(string = text, pattern = "were 1 students", replacement = "was 1 student")
    # }
    # 
    # if (grepl("college of Graduate College", text)) {
    #   text <- str_replace(string = text, pattern = "college of Graduate College", replacement = "Graduate College")
    # }
    # 
    # if (grepl("college of School of", text)) {
    #   text <- str_replace(string = text, pattern = "college of School of", replacement = "School of")
    # }
    # 
    #return(text)
    HTML(text)
    #HTML("This is <b>bold</b> text!")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

