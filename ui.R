#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(splines2)
library(MASS)


# Define UI for application
shinyUI(navbarPage(
    # Application title
    titlePanel(title = h2("Shiny App based on USArrests data set", align = "center")),
    sidebarLayout(
        sidebarPanel(
            helpText("Select a crime rate to visualize the data and plots on the right panel"),
            
            selectInput("var",
                        label = "Crime drop-down box",
                        choices = c("Murder" = 1, "Assault" = 2,"Rape" = 4),
                        selected = 1)
        ),
        
        mainPanel(tabsetPanel(type = "tabs", 
                              tabPanel("Data",tableOutput("Data")),
                              tabPanel("Map",plotOutput("Map")),
                              tabPanel("Regression Plot",plotOutput("Crimeplot")),
                              tabPanel("Linear Regression Diagnostics",plotOutput("Diagnostics")),
                              tabPanel("Documentation", verbatimTextOutput("Documentation"))
                              
        ))
        )
))