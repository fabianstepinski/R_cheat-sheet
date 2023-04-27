########################################################################################                                                    #
# Data Professor                                                                       #
# http://youtube.com/dataprofessor                                                     #
# http://github.com/dataprofessor                                                      #
# THIS VIDEO : https://www.youtube.com/watch?v=9uFQECk30kA&ab_channel=freeCodeCamp.org #
########################################################################################

#CONTINUE MIN:

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load & install packages
#install.packages("shiny")
#install.packages("shinythemes")
library(shiny)
library(shinythemes)

#USER INTERFACE_____________________________________________________________####

# Define UI
ui <- fluidPage(theme = shinytheme("superhero"), #other themes under https://rstudio.github.io/shinythemes/
                  navbarPage( # navbarPage
                    "My first app",
                    tabPanel("Navbar 1", # Navbar 1, tabPanel
                             sidebarPanel( # sidebarPanel
                               tags$h3("Input:"),
                               textInput("txt1", "Given Name:", ""), #will be sent to the server
                               textInput("txt2", "Middle Name:", ""), #will be sent to the server
                               textInput("txt3", "Surname:", "")),
                             mainPanel( # mainPanel
                               h1("Header 1"),
                               h4("Output 1"),
                               verbatimTextOutput("txtout"))),#is generated form the server), # Navbar 1, tabPanel
                    tabPanel("Navbar 2", "This panel is intentionally left blank"),
                    tabPanel("Navbar 3", "This panel is intentionally left blank")) # navbarPage
  ) # fluidPage

#SERVER COMPONENT___________________________________________________________####

# Define server function  
server <- function(input, output) { # server
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, input$txt3, sep = " " ) 
  })}

#SHINY APPLICATION FUNCTION_________________________________________________####

# Create Shiny object
shinyApp(ui = ui, server = server)