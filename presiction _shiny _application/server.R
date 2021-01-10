#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
data(airquality)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    air<-na.omit(airquality)
    oz<-airquality$Ozone
    temp<-airquality$Temp
    model1<-lm(oz~temp)
    
    model1pred<-reactive({
        TempInput<-input$sliderTemp
        predict(model1,newdata<-data.frame(temp=TempInput))
    })
    
    output$Plot1<-renderPlot({
        TempInput<-input$sliderTemp
        plot(airquality$Temp,airquality$Ozone,xlab="temperature",ylab="Ozone",bty="n",pch=16)
        points(TempInput,model1pred(),col="red",pch=16,cex=4)
    })
   
    output$pred1<-renderText({
        model1pred()
    })
    })


