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

# load data 
data_names <- list.files("Data/", pattern = ".RDS", full.names = TRUE)

df <- readRDS("Data/clean.ethsexsp25.RDS")
colleges_df <- df %>%
  filter(Degree == "College total")

colleges_vec <- setNames(colleges_df$Coll, colleges_df$`Major Name`)
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
    column(width = 3,
           wellPanel(
             helpText("Select one or more filters to customize the enrollment insights"),
             fluidRow(
               column(width = 6,
                      selectInput("year", "Select Year", choices = rev(c(2004 : 2025)), selected = 2025)),
               column(width = 6,
                      selectInput("semester", "Select Semester",
                                  choices = NULL)),
                                 #   c("Fall", "Spring", "Summer"), selected = "Spring"))
             ),
             conditionalPanel(
               condition = "input.level == ''",
               checkboxInput('levelYN', label = "View Campus Enrollment by Degree level", value = FALSE, width = NULL)
             ),
             selectizeInput('level', 
                            label = HTML("2. Filter by Degree Level <span title='After selecting degree level and major, if more than one degree \nis available you will have the choice to filter by Degree type'>⍰</span>"), 
                            choices = c("Show all degree levels" = "", c("Undergraduate", "Graduate", "Nondegree"), selected = "Undergraduate")),
             selectizeInput('college', "1. Filter by College", choices = c("Show all colleges" = "")),
             selectizeInput('major', "3. Filter by Major", choices = c("Show all majors" = "")),
             uiOutput("degree_type_ui"),  # placeholder for conditional dropdown
             selectizeInput('conc', 
                            label = HTML("4. Filter by Concentration (if applicable) <span title='Concentration is applied after major is selected. The \nvalue \"None\" identifies majors without a concentration'>⍰</span>"), 
                            choices = c("Show all concentrations" = ""))
             
           ),
           actionButton("reset_filters", "Reset All Filters")
    ),
   
    # RIGHT SIDE: Main content
    column(width = 9,
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
                    shinycssloaders::withSpinner(
                      plotOutput("sexplot", height = "25vh"),
                      type = 1, color = "#007bff", size = 0.5)
             ),
             column(6,
                    h3("Enrollment by Illinois residency"),
                    shinycssloaders::withSpinner(
                      plotOutput("resplot", height = "25vh"),
                      type = 1, color = "#007bff", size = 0.5)
             )
           ),
           
           uiOutput("race_urm_ui"),
           
           
           # Row 3: Data Table
           fluidRow(
             column(12,
                    h3("View Data"),
                    shinycssloaders::withSpinner(
                      DT::dataTableOutput("mytable"),
                      type = 4, color = "#007bff", size = 0.5)
             )
           )
    )
    
  )
)


# Define server logic 
server <- function(input, output, session) {
  
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "year", selected = "2025")         # or default year
    updateSelectInput(session, "semester", selected = "Spring")   # or default semester
    
    updateSelectInput(session, "degree", selected = "")
    updateSelectInput(session, "college", selected = "")
    updateSelectInput(session, "levelYN", selected = "")
    updateSelectInput(session, "level", selected = "")
    updateSelectInput(session, "major", selected = "")
    updateSelectInput(session, "conc", selected = "")
  })
  
  output$race_urm_ui <- renderUI({
    # Get filtered data
    data <- final_data()
    req(nrow(data) > 0)

    if ((input$year == 2020 & input$semester == "Fall") | input$year > 2020) {
      # Show race and URM side-by-side
      fluidRow(
        column(8,
               h3("Enrollment by Race and Ethnicity"),
               shinycssloaders::withSpinner(
                 plotOutput("raceplot", height = "40vh"),
                 type = 1, color = "#007bff", size = 0.5)
        ),
        column(4,
               h3(HTML("Underrepresented Minority Breakdown <span title='URM includes American Indian & Alaskan Native, Native Hawaiian & Pacific Islander, African American, and Hispanic/Latino. Multi-racial persons are included if one selected group is URM. Foreign students are counted separately. This information is available post Fall 2020.' style='cursor: help;'>⍰</span>")),
               shinycssloaders::withSpinner(
                 plotOutput("URM_plot", height = "32vh"),
                 type = 1, color = "#007bff", size = 0.5
               )
        )
      )
    } else {
      # Only show race plot full width
      fluidRow(
        column(12,
               h3("Enrollment by Race and Ethnicity"),
               shinycssloaders::withSpinner(
                 plotOutput("raceplot", height = "40vh"),
                 type = 1, color = "#007bff", size = 0.5)
        )
      )
    }
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
    
    level_abb = c("Undergraduate" = "Undergraduate","Graduate" = "Graduate","Professional"="Nondegree")
    semester_df$programtype <- ifelse(semester_df$`Major Name` %in% names(level_abb), level_abb[semester_df$`Major Name`], semester_df$programtype)
    semester_df$Degree <- ifelse(semester_df$`Major Name` %in% names(level_abb), level_abb[semester_df$`Major Name`], semester_df$Degree)
    
    semester_df
  })
  
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
                        choices = c("Fall", "Spring", "Summer"), 
                        selected = "Fall")
    }
  })
  
  
  ## Update college choices
  observeEvent(c(input$level), {
    # Ensure inputs are available
    req(input$level)  
    
    college_names_df <- df %>% #enrollment_data() %>%
      filter(Degree == "College total")
    
    coll_names_vec <- setNames(college_names_df$`Major Name`, college_names_df$Coll)
    
    colleges_df <- df %>% #enrollment_data() %>%
      filter(programtype == "Undergraduate") %>% #input$level) %>%
      filter(Degree != "College total") %>%
      group_by(Coll) %>%
      summarise(across(where(is.numeric), sum, na.rm = TRUE))
    
    # ex <- unique(iris$Species)
    # names(ex) <- month.abb[1:3]
    # 
    # ex[month.abb[1:2]]
    college_choices <- c("Show all colleges" = "", setNames(names(coll_names_vec[colleges_df$Coll]), coll_names_vec[colleges_df$Coll]))
    
    # Extract majors safely as a vector
    # college_choices <- c("Show all majors" = "", as.vector(filtered_colleges))
    
    # Update the select input
    updateSelectInput(session,
                      inputId = "college",
                      choices = college_choices,
                      selected = "")
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
  
  
  ## Update conc choices
  observeEvent(c(input$college, input$level, input$major, input$degree), {
    # Only require college, level, and major; degree may be optional
    req(input$college != "", input$level != "", input$major != "")
    
    # Filter the data
    if (!is.null(input$degree) && input$degree != "") {
      filtered_concs <- enrollment_data() %>%
        filter(Coll == input$college,
               programtype == input$level,
               `Major Name` == input$major,
               Degree == input$degree) %>%
        select(`Concentration Name (if any)`) %>%
        na.omit() %>%
        distinct() %>%
        arrange(`Concentration Name (if any)`) %>%
        pull()
    } else {
      filtered_concs <- enrollment_data() %>%
        filter(Coll == input$college,
               programtype == input$level,
               `Major Name` == input$major) %>%
        select(`Concentration Name (if any)`) %>%
        na.omit() %>%
        distinct() %>%
        arrange(`Concentration Name (if any)`) %>%
        pull()
    }
    
    # Set dropdown choices
    conc_choices <- c("Show all concentrations" = "", as.vector(filtered_concs))
    
    updateSelectInput(session,
                      inputId = "conc",
                      choices = conc_choices,
                      selected = NULL)
  })
  
  observeEvent(c(input$college, input$level, input$major), {
    #req(input$level != "" & input$college != "")  # proceed only if level is chosen
    
    # Filter the degrees available for this level
    filtered_degrees <- enrollment_data() %>%
      filter(Coll == input$college,
             programtype == input$level,
             `Major Name` == input$major) %>%
      distinct(Degree) %>%
      pull(Degree)
    
    if (length(filtered_degrees) > 1) {
      # Render the selectize input only if more than one option exists
      output$degree_type_ui <- renderUI({
        selectizeInput("degree", "Select Degree Type",
                       choices = c("Choose a degree type" = "", filtered_degrees),
                       selected = NULL)
      })
    } else {
      # Optionally render nothing or a hidden input
      output$degree_type_ui <- renderUI({
         NULL
      })

      # Optionally store the only value if needed downstream
      updateSelectInput(session, "degree", choices = "", selected = "")
    }
  })
  
  
  output$mytable <- DT::renderDataTable({
    DT::datatable(final_data(),
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    buttons = c('csv', 'excel'),
                    scrollX = TRUE
                  )
    )
  })
  

  final_data <- reactive({
    
    filtered_df <- enrollment_data()
    req(nrow(filtered_df) > 0)  # This line prevents the rest of the code if data is empty
    
    # case 1 - campus total
    if (input$college == "" & input$level == "") {
      if (input$levelYN) {
        final = filtered_df %>%
            filter(`Major Name` %in% c("Undergraduate"="Undergraduate", "Graduate"="Graduate","Professional"="Nondegree")) %>% 
            group_by(Degree,  programtype, `Major Name`) %>%
            summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep")
        } else {
          final = filtered_df %>%
            filter(`Major Name` %in% c("Campus total"))
        }
      
    } else if ((input$college == "") & (input$level != "")) { 
      # case 2 - degree total
      final <- filtered_df %>%
        filter(`Major Name` %in% c("Undergraduate"="Undergraduate", "Graduate"="Graduate","Professional"="Nondegree")) %>%
        filter(`Major Name` == input$level) %>%
        ungroup()
      
    } else if ((input$college != "") & (input$level != "") & (input$major == "")) { 
      # case NEW - college and degree type total
      final <- filtered_df[ (filtered_df$Coll %in% c(input$college)) & (filtered_df$programtype %in%  c(input$level)), ] %>%
        group_by(Degree,  programtype) %>%
        summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep")
      
    } else if ((input$college != "") & (input$level != "") & (input$major != "") & (input$conc == "")) { 
      # case 3 - college, degree type, and major (without conc)
      final <- filtered_df[ (filtered_df$Coll %in% c(input$college)) & (filtered_df$`Major Name` %in%  c(input$major)) & (filtered_df$programtype %in%  c(input$level)), ] %>%
        group_by( programtype, Degree, `Major Name`) %>%
        summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep")
      
    } else if ((input$college != "") & (input$level != "") & (input$major != "") & (input$conc != "")) { 
      # case 4 - college, degree type, major, and conc
      final <- filtered_df[ (filtered_df$Coll %in% c(input$college)) & (filtered_df$`Major Name` %in%  c(input$major)) &
                     (filtered_df$programtype %in%  c(input$level)) & (filtered_df$`Concentration Name (if any)` %in%  c(input$conc)), ] %>%
        group_by(programtype, Degree, `Major Name`, `Concentration Name (if any)`) %>%
        summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "keep")
    }
    else {
      final <- filtered_df[0,] #df[df$Coll %in% c(input$college), ]
    }
    
    if (!is.null(input$degree) && input$degree != "") {
      final <- final %>% filter(Degree == input$degree)
    }
    return(final)
  })
  
  # output message
  output$nodata_msg <- renderUI({
    if (nrow(final_data()) == 0 & (input$conc != "") ) {
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
    if (input$college == "" & input$level == "") {
      sex_df = data %>%
        select(Degree, programtype, `Major Name`, Total, Men, Women, `Sex Unknown`) %>%
        rename(Unknown = `Sex Unknown`)  
      
    } else if ((input$college == "") & (input$level != "")) { 
      # case 2 - degree total
      sex_df = data %>%
        select(`Major Name`,Degree, Total, Men, Women, `Sex Unknown`) %>%
        rename(Unknown = `Sex Unknown`) 
      
    } else if ((input$college != "") & (input$level != "") & (input$major == "")) { 
      # case NEW - college and degree type total
      sex_df = data %>%
        select(programtype, Degree, Total, Men, Women, `Sex Unknown`) %>%
        rename(Unknown = `Sex Unknown`)  
      
    } else if ((input$college != "") & (input$level != "") & (input$major != "") & (input$conc == "")) { 
      # case 3 - college, degree type, and major (without conc)
      sex_df = data %>%
        select(programtype, Degree, `Major Name`, Total, Men, Women, `Sex Unknown`) %>%
        rename(Unknown = `Sex Unknown`) 
      
    } else if ((input$college != "") & (input$level != "") & (input$major != "") & (input$conc != "")) { 
      sex_df = data %>%
        select(programtype, Degree, `Major Name`, `Concentration Name (if any)`, Total,
               Men, Women, `Sex Unknown`) %>%
        rename(Unknown = `Sex Unknown`)
    } else {
      sex_df <- NULL
    }
    
 
    sex_df %>%
      tidyr::pivot_longer(c(Men, Women, Unknown), names_to = "sex", values_to = "total") %>%
      mutate(perc = round(100*total/Total, 2)) %>%
      mutate(sex = factor(sex, ordered = TRUE, levels = c("Men", "Women", "Unknown"))) %>%
      ggplot(aes(x=Degree, y=perc, fill=factor(sex, ordered = TRUE, levels = c("Men", "Women", "Unknown")))) +
      geom_col() +
      facet_wrap(~ Degree, scales = "free_y") +  # Allows different total heights
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      labs(y = "Percentage",
           fill = "Sex") + 
      scale_x_discrete(drop = TRUE) +
      geom_text(aes(label = paste0(perc, "%")), position = position_stack(vjust=0.5)) +
      scale_fill_manual(values = c("Men" = "#4A90E2",    # Blue
                                   "Women" = "#FF69B4",  # Pink
                                   "Unknown" = "#B0B0B0"))  # Grey

  })
  
  output$raceplot <- renderPlot({
    data <- final_data()
    req(nrow(data) > 0)  # This line prevents the rest of the code if data is empty
    
    # case 1 - campus total
    if (input$college == "" & input$level == "") {
      race_df = data %>%
        ungroup() %>%
        #select(Coll, everything()) %>%
        select("Degree", "programtype", "Major Name", "Total", "Caucasian":"Race Unknown") %>%
               # "Asian American","African American","Hispanic","Native American","Hawaiian/Pacific Isl",
               # "Multiracial", "International", "Race Unknown") %>%
        rename(Unknown = `Race Unknown`)  
      
    } else if ((input$college == "") & (input$level != "")) { 
      # case 2 - degree total
      race_df = data %>%
        #select(everything()) %>%
        ungroup() %>%
        select(`Major Name`,Degree, "Total", "Caucasian":"Race Unknown") %>%
               #"Caucasian","Asian American","African American","Hispanic","Native American","Hawaiian/Pacific Isl","Multiracial", "International", "Race Unknown") %>%
        rename(Unknown = `Race Unknown`) 
      
    } else if ((input$college != "") & (input$level != "") & (input$major == "")) { 
      # case NEW - college and degree type total
      race_df = data %>%
       # select(Coll, everything()) %>%
        ungroup() %>%
        select(any_of(c("programtype", "Degree", "Major Name", "Concentration Name (if any)", "Total", 
                        "Caucasian","Asian American","African American","Hispanic","Native American","Hawaiian/Pacific Isl","Multiracial", "International", "Race Unknown"))) %>%
        #select("Coll", "programtype", "Degree", "Total", "Caucasian":"Race Unknown") %>%
               #"Caucasian","Asian American","African American","Hispanic","Native American","Hawaiian/Pacific Isl","Multiracial", "International", "Race Unknown") %>%
        rename(Unknown = `Race Unknown`)  
      
    } else if ((input$college != "") & (input$level != "") & (input$major != "") & (input$conc == "")) { 
      # case 3 - college, degree type, and major (without conc)
      race_df = data %>%
        ungroup() %>%
        #select(Coll, everything()) %>%
        select("programtype", "Degree", "Major Name", "Total", "Caucasian":"Race Unknown") %>%
               #"Caucasian","Asian American","African American","Hispanic","Native American","Hawaiian/Pacific Isl","Multiracial", "International", "Race Unknown") %>%
        rename(Unknown = `Race Unknown`) 
      
    } else if ((input$college != "") & (input$level != "") & (input$major != "") & (input$conc != "")) { 
      race_df = data %>%
        ungroup() %>%
       # select(Coll, everything()) %>%
        select(any_of(c("programtype", "Degree", "Major Name", "Concentration Name (if any)", "Total", 
               "Caucasian","Asian American","African American","Hispanic","Native American","Hawaiian/Pacific Isl","Multiracial", "International", "Race Unknown"))) %>%
        rename(Unknown = `Race Unknown`)
    } else {
      race_df <- NULL
    }
    
    pivot_cols <- race_df %>% 
      select(where(is.numeric))  %>% #select("Caucasian":"Unknown") %>% colnames(.)
      colnames(.)
    pivot_cols <- setdiff(pivot_cols, "Total")
    
    race_df %>%
      ungroup() %>%
      tidyr::pivot_longer(all_of(pivot_cols), names_to = "race", values_to = "total") %>% 
        #c("Caucasian","Asian American","African American","Hispanic","Native American","Hawaiian/Pacific Isl","Multiracial", "International", "Unknown"),
      mutate(perc = round(100*total/Total, 2)) %>%
    ggplot(aes(x = reorder(race,-total), y=total, fill = race)) +
      geom_bar(stat="identity") +
      theme_minimal() +
      guides(x =  guide_axis(angle = 45)) +
      theme(legend.position = "none") +
      geom_text(aes(label = paste0(perc, "%")), position = position_stack(vjust=0.5), size=3) +
      facet_wrap(~ Degree, scales = "free_y") +
      labs(x = "Race",
           y = "Total")
    
  })
  
  output$resplot <- renderPlot({
    
    data <- final_data()
    req(nrow(data) > 0)  # This line prevents the rest of the code if data is empty
    
    
    # case 1 - campus total
    if (input$college == "" & input$level == "") {
      residency_df = data %>%
        select(Degree, programtype, `Major Name`, Total, "Illinois", "Non-Illinois") 
      
    } else if ((input$college == "") & (input$level != "")) { 
      # case 2 - degree total
      residency_df = data %>%
        select(`Major Name`,Degree, Total, "Illinois", "Non-Illinois")
      
    } else if ((input$college != "") & (input$level != "") & (input$major == "")) { 
      # case NEW - college and degree type total
      residency_df = data %>%
        select(programtype, Degree, Total, "Illinois", "Non-Illinois") 
      
    } else if ((input$college != "") & (input$level != "") & (input$major != "") & (input$conc == "")) { 
      # case 3 - college, degree type, and major (without conc)
      residency_df = data %>%
        select(programtype, Degree, `Major Name`, Total, "Illinois", "Non-Illinois")
      
    } else if ((input$college != "") & (input$level != "") & (input$major != "") & (input$conc != "")) { 
      residency_df = data %>%
        select(programtype, Degree, `Major Name`, `Concentration Name (if any)`, Total,
               "Illinois", "Non-Illinois")
    } else {
      residency_df <- NULL
    }

    residency_df %>%
      tidyr::pivot_longer(c("Illinois", "Non-Illinois"), names_to = "residency", values_to = "total") %>%
      mutate(perc = round(100*total/Total, 2)) %>%
    ggplot(aes(x=Degree, y=perc, fill=factor(residency))) +
      geom_col() +
      facet_wrap(~ Degree, scales = "free_y") +  # Allows different total heights
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      labs(y = "Percentage",
           fill = "Residency") + 
      scale_x_discrete(drop = TRUE) +
      facet_wrap(~Degree) +
      geom_text(aes(label = paste0(perc, "%")), position = position_stack(vjust=0.5), color = "white") +
      scale_fill_manual(values = c("Illinois" = "#E84A27",    # Orange
                                   "Non-Illinois" = "#13294B"))  # Blue
  })
  
  # Render the plot
  output$URM_plot <- renderPlot({
    data <- final_data()
    # Ensure data is not empty and "URM" column exists
    req(nrow(data) > 0, "URM" %in% colnames(data))
    
    if (((input$college == "") & (input$level != ""))) {
      URM_df = data %>%
        select(`Major Name`,Degree, Total, URM) %>%
        mutate(`Not URM` = Total - URM) %>%
        pivot_longer(c("Not URM", "URM"), names_to = "group", values_to = "total")
      
    } else if ("Concentration Name (if any)" %in% colnames(data)) {
      URM_df = data %>%
        select(programtype, Degree,`Major Name`, `Concentration Name (if any)`, Total, URM) %>%
        mutate(`Not URM` = Total - URM) %>%
        pivot_longer(c("Not URM", "URM"), names_to = "group", values_to = "total")
    } else if (!("Degree" %in% colnames(data) & "Major Name" %in% colnames(data))) {
      # NEW
      URM_df = data %>%
        select(programtype, Degree, Total, URM) %>%
        mutate(`Not URM` = Total - URM) %>%
        pivot_longer(c("Not URM", "URM"), names_to = "group", values_to = "total")
    } else if (!("Degree" %in% colnames(data))) {
      URM_df = data %>%
        select(programtype, Degree, Total, URM) %>%
        mutate(`Not URM` = Total - URM) %>%
        pivot_longer(c("Not URM", "URM"), names_to = "group", values_to = "total")
    } else {
      URM_df = data %>%
        select(programtype, Degree, `Major Name`, Total, URM) %>%
        mutate(`Not URM` = Total - URM) %>%
        pivot_longer(c("Not URM", "URM"), names_to = "group", values_to = "total")
    }
    
    # Create pie chart
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
        geom_text(aes(label = label), 
                  position = position_stack(vjust = 0.5),
                  hjust = 0.1,     # <-- nudges labels to the right
                  size = 4, color = "black") +
        theme_void() +
        
        labs(title = unique(df$Degree)) +
        scale_fill_manual("", values = c("purple", "yellow"))
    })
    
    # Step 3: Arrange plots
    grid.arrange(grobs = plot_list, ncol = ifelse(length(plot_list) == 1, 1,2 ))
  })
  
  output$summary_text <- renderUI({
    
    data <- final_data()
    total <- sum(data$Total, na.rm = TRUE)
    firstsentence <- paste0("During the ", input$semester," ", input$year, " semester, there were <b>", scales::comma(total), "</b>")
    if (input$college == "" & input$level == "") {
      # Case 1 - campus total
      text <- paste(firstsentence, " students enrolled in the University of Illinois at Urbana-Champaign campus.")
    }  else if (nrow(data) == 0){
      return("")
      
    } else if ((input$college == "") & (input$level != "")) {
      # Case 2 - degree total
      text <- paste(firstsentence, "students enrolled in an", input$level, "program.")
    
    } else if ((input$college != "") & (input$level != "") & (input$major == "")) {
      # Case NEWWWW - major without concentration total
      text <- paste0(firstsentence, " students enrolled in a ", tolower(input$level), 
                    " program through the college of ", names(colleges_vec[grepl(input$college, colleges_vec)]), ".")
      
    } else if (input$college != "" & input$level != "" & input$conc == "") {
      # Case 3 - major without concentration total
      text <- paste0(firstsentence, " students enrolled in a ", tolower(input$level), 
                     " program with a major in ",  input$major, " through the college of ",
                     names(colleges_vec[grepl(input$college, colleges_vec)]), ".")
      
    } else if (input$college != "" & input$level != "" & input$conc != "" & input$conc != "None") {
      # Case 4a - major with concentration total
  
      text <- paste0(firstsentence, " students enrolled in a ", tolower(input$level), 
                     " program with a major in ",  input$major, ", ", input$conc,  " concentration through the college of ",
                     names(colleges_vec[grepl(input$college, colleges_vec)]), ".")
      
    } else if (input$college != "" & input$level != "" & input$conc != "" & input$conc == "None") {
      # Case 4b - major with concentration total (edited text to remove mention of conc when equals "None")
      text <-  paste0(firstsentence, " students enrolled in a ", tolower(input$level), 
                      " program with a major in ",  input$major, " through the college of ",
                      names(colleges_vec[grepl(input$college, colleges_vec)]), ".")
      
    }
    else {
      text <- "No data available for the selected criteria."
      
    }
    # check if student(s)
    # graduate college
    # return
    
    if (grepl("were 1 students", text)) {
      text <- str_replace(string = text, pattern = "were 1 students", replacement = "was 1 student")
    }
    
    if (grepl("college of Graduate College", text)) {
      text <- str_replace(string = text, pattern = "college of Graduate College", replacement = "Graduate College")
    }
    
    if (grepl("college of School of", text)) {
      text <- str_replace(string = text, pattern = "college of School of", replacement = "School of")
    }
    
    #return(text)
    HTML(text)
    #HTML("This is <b>bold</b> text!")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

