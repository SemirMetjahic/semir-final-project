#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
rm(list=ls())
library(shiny)
library(tidyverse)
library(rvest)

df <- read.csv("data.csv")

model <- lm(Game_Score ~ Field_Goals_Attempted + Offensive_Rebounds + 
              Minutes_Played + Three_Point_Field_Goals_Made + 
              Personal_Fouls + Two_Point_Field_Goal_Percentage, df)


pred_score <- function(Field_Goals, Off_Rebounds, 
                       Min_Played, Three_Points_Made, 
                       Per_Fouls, Two_Point_Percentage ){
  newdata <- data.frame(
    Field_Goals_Attempted = Field_Goals, 
    Offensive_Rebounds = Off_Rebounds,
    Minutes_Played = Min_Played,
    Three_Point_Field_Goals_Made = Three_Points_Made,
    Personal_Fouls = Per_Fouls,
    Two_Point_Field_Goal_Percentage = Two_Point_Percentage
  )
  print(newdata)
  predict(model, newdata)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          sliderInput("Field_Goals_Attempted",
                      "field goals:",
                      min = 1,
                      max = 50,
                      value = 45),
            sliderInput("Offensive_Rebounds",
                        "offesive rebounds:",
                        min = 1,
                        max = 50,
                        value = 18),
          sliderInput("Minutes_Played",
                      "Minutes Played:",
                      min = 1,
                      max = 50,
                      value = 18),
          sliderInput("Three_Point_Field_Goals_Made",
                      "Three point fg made:",
                      min = 1,
                      max = 50,
                      value = 18),
          sliderInput("Two_Point_Field_Goal_Percentage",
                      "Two Point fg percentage:",
                      min = 1,
                      max = 50,
                      value = 18),
          sliderInput("Free_Throws_Made",
                        "Free_Throws:",
                        min = 1,
                        max = 50,
                        value = 35),
          sliderInput("Assists",
                      "assists:",
                      min = 1,
                      max = 50,
                      value = 15),
          sliderInput("Def_Rebounds",
                      "defensive rebounds:",
                      min = 1,
                      max = 50,
                      value = 12),
          sliderInput("Personal_Fouls",
                      "personal fouls:",
                      min = 1,
                      max = 50,
                      value = 12)
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           strong(textOutput("prediction"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
  output$prediction <- renderText({ 
    # pred_score(input$field_goals, 
    #            input$off_rebounds)
    # "hello"
   paste("predicted fantasy score: ",
    pred_score(input$Field_Goals_Attempted,
               input$Offensive_Rebounds,
               input$Minutes_Played,
               input$Three_Point_Field_Goals_Made,
               input$Personal_Fouls,
               input$Two_Point_Field_Goal_Percentage))
  })
  
  
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
