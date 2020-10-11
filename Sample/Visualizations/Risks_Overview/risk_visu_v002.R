# File: risk_visu_v002.R
# Demo of Shiny Dashboard (for RStudio interactive session) for Risk Register visualization

# This demo includes quite a few sample things:
#  - reactiveVal() for updating dataset used by the dashboard
#  - filter for project selection (we could use similar approaches for risk type, etc.)
#  - file input for a better UX, as opposed to having her editing the code
#  - sample tryCatch() code for controlling incorrect input (INCOMPLETE, but this is a demo)
#  - interactive Risk visualization in GGPlot as scatterplot of probability * impact with Jitter (noise) added to positions to avoid overlap
#  - nearest point selection based on x-y clicked position

library(data.table)
library(lubridate)
library(ggplot2)
# # install.packages("ggthemes") # Nice visualization setups
library(ggthemes)
# #install.packages("ggExtra") # Was missing that one...
# library(ggExtra)
# # install.packages("plotly")
# library(plotly)
library(shiny)
library(dplyr)

##
# General Variables Section
## 

# Keep track of errors, to be displayed later to the user
errors_set <- c()

## ----
# Functions to be used later on:
## ----

# First we'll create a function to pass a factor or string to numeric format.
# Will return 0 on error or warning:
make_numeric <- function(x) {
  tryCatch(
    { as.numeric(as.character(x)) },
    error=function(cond) {
      message(paste("Error: Probability does not seem numeric"))
      message(cond)
      errors_set <- c(errors_set, "Probability in wrong format.")
      # Choose a return value in case of error
      return(0)
    },
    warning=function(cond) {
      message(paste("Warning: Probability does not seem numeric"))
      message(cond)
      errors_set <<- c(errors_set, "Probability in wrong format.")
      # Choose a return value in case of warning
      return(0)
    },
    finally={
    })
}

## ----
# Main Functional Code
## ----

# Demo Data
Sys.getlocale("LC_TIME")
# Global Variable. Now this is not ideal, but this is to keep track of file uploaded:
main_demo_rr <- NULL



# Let's get to the visualization: ----
# Now make it more interactive / visually simpler:

## ----
# UI
## ----
ui <- fluidPage(
  fluidRow(h4("Shiny Dashboard for Risk Register")),
  hr(),
  fluidRow(column(6,
                  fileInput("risk_register_table", "Choose CSV File",
                            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                  checkboxInput("header", "Header", TRUE)),
           column(6,
                  selectInput("project_filter", "Project:",
                              c("All" = "All"))),
           textOutput("num_risks")
  ),
  hr(),
  fluidRow(
    column(6,
           plotOutput("risk_plot1", 
                      height = "300px", 
                      click = "plot1_click")
    ),
    column(6,
           "Risk Details:", br(),
           verbatimTextOutput("info")
    )),
  fluidRow(
    plotOutput("risk_plot2", height = "250px")
  )
)

## ----
# Shiny Server
## ----
server <- function(input, output, session) {
  
  # One of the neatest "tricks in the book" for Shiny, the reactiveVal()
  # We will then work with demo_rr(), calling the values associated to it:
  demo_rr <- reactiveVal(NULL)
  
  # To update demo_rr(), we can use a function like so:
  observeEvent(input$risk_register_table, { #----
    inFile <- input$risk_register_table
    if (is.null(inFile)) {
      demo_rr(NULL)
    } else {
      demo_rr <- as.data.table(read.csv2(inFile$datapath, header = TRUE))
      # We could use: header = input$header; But this does not apply in our demo.
      
      str(demo_rr) # Dates and Probability could be of type Factor
      demo_rr[] <- lapply(demo_rr[], as.character) # Quick casting
      
      # Let's fix the dates
      Sys.setenv(TZ='UTC')
      demo_rr[, Date_identified := ymd(Date_identified)]
      demo_rr[, Date_closed := ymd(Date_closed)]
      
      # Let's suppose Probability has some incompatible text format, recognized as Factor.
      # Any bad text will be set to 0.
      if(!is.numeric(demo_rr$Probability)) { demo_rr[, Probability := sapply(Probability, make_numeric)] }
      if(!is.numeric(demo_rr$Impact)) { demo_rr[, Impact := sapply(Impact, make_numeric)] }
      
      # Let's add the Risk Value to the dataset:
      demo_rr[, Risk := Probability * Impact]
      
      # Let's discard any risk with value 0
      demo_rr <- demo_rr[Risk != 0,]
      
      ##
      # Prepping for Visualization
      ## ----
      demo_rr[, Probability := sapply(Probability, jitter, 2)]
      demo_rr[, Impact := sapply(Impact, jitter, 2)]
      
      # Update Projects List upon load:
      updateSelectInput(session,
                        "project_filter",
                        label = "Project:",
                        choices = c("All", sort(unique(demo_rr$Project_Name))),
                        selected = "All")
      
      # Keep main copy of demo_rr for re-use upon filtering:
      main_demo_rr <<- demo_rr
      
      demo_rr(demo_rr) # Not to be confusing: demo_rr() contains updated demo_rr object.
    }
  }) #----
  
  output$num_risks <- renderText ( if(!is.null(demo_rr())) { paste("Number or Risks in register:", nrow(demo_rr())) })

  # To filter demo_rr(), we can use another function like so:
  observeEvent(input$project_filter, { #----
    demo_rr <- main_demo_rr # We use the global copy of the variable
    if(!is.null(demo_rr) & input$project_filter != "All") {
      demo_rr <- demo_rr[Project_Name == input$project_filter]
    } else {
      demo_rr <- main_demo_rr
    }
    demo_rr(demo_rr) # Not to be confusing: demo_rr() contains updated demo_rr object.
  }) #----
  
  output$info <- renderText( #----
    if(!is.null(demo_rr())) {
      # Let's find the nearest point to the user's click position:
      risk_selected <- which.min(abs(demo_rr()$Probability - input$plot1_click$x) +
                                   abs(demo_rr()$Impact - input$plot1_click$y))
      
      paste("Position clicked\nProb.:", input$plot1_click$x, "\nImp.:", input$plot1_click$y,
            "\n\nRisk ID:", demo_rr()[risk_selected]$Risk_ID,
            "\nRisk Name: ", demo_rr()[risk_selected]$Risk_Name, 
            "\n\nRisk Description:", demo_rr()[risk_selected]$Description,
            "\nRisk Type: ", demo_rr()[risk_selected]$Risk_type, 
            "\n\nRisk Probability:", round(demo_rr()[risk_selected]$Probability, 0),
            "\nRisk Impact:", round(demo_rr()[risk_selected]$Impact, 0),
            "\nRisk Level:", demo_rr()[risk_selected]$Risk)
    }) #----
  
  output$risk_plot1 <- renderPlot( #----
    if(!is.null(demo_rr())) {
      demo_plot <- ggplot(demo_rr(), aes(x = Probability, y = Impact, colour = Risk))
      demo_plot <- demo_plot + geom_point(alpha = 0.3, size = 5)
      demo_plot <- demo_plot + xlim(0.5, 3.5) + ylim(0.5, 3.5)
      demo_plot <- demo_plot + scale_color_gradient(low="green", high="red")
      
      demo_plot
    }) #----
  
  output$risk_plot2 <- renderPlot( #----
    if(!is.null(demo_rr())) {
      time_line_open <- demo_rr()[, c("Project_Name", "Date_identified")] %>% 
        group_by(Date_identified) %>% 
        tally()
      demo_timeline <- ggplot(time_line_open, aes(x = Date_identified, y = n))
      demo_timeline <- demo_timeline + geom_line(colour = "blue")
      demo_timeline <- demo_timeline + theme_economist()
      
      demo_timeline
    }) #----
  
  
  output$info <- renderText( #----
    if(!is.null(demo_rr())) {
      # Let's find the nearest point to the user's click position:
      risk_selected <- which.min(abs(demo_rr()$Probability - input$plot1_click$x) +
                                   abs(demo_rr()$Impact - input$plot1_click$y))
      
      paste("Position clicked\nProb.:", input$plot1_click$x, "\nImp.:", input$plot1_click$y,
            "\n\nRisk ID:", demo_rr()[risk_selected]$Risk_ID,
            "\nRisk Name: ", demo_rr()[risk_selected]$Risk_Name, 
            "\n\nRisk Description:", demo_rr()[risk_selected]$Description,
            "\nRisk Type: ", demo_rr()[risk_selected]$Risk_type, 
            "\n\nRisk Probability:", round(demo_rr()[risk_selected]$Probability, 0),
            "\nRisk Impact:", round(demo_rr()[risk_selected]$Impact, 0),
            "\nRisk Level:", demo_rr()[risk_selected]$Risk)
    }) #----

}
## ----
shinyApp(ui = ui, server = server)
