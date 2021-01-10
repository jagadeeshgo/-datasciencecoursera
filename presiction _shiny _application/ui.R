#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("predict ozone level in air quality"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("sliderTemp",
                        "Enter the temperature",
                        min = 56,
                        max = 97,
                        value = 10),
            submitButton("Submit")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("Plot1"),
            h4("predicted ozone level from the model(Red Circle):"),
            textOutput("pred1")
        )
    )
))
