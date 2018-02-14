#My first Shiny app!  
#StatR 502

#Load packages 
library(datasets)
library(zoo)
library(dplyr)
library(ggplot2)
library(mgcv)


#Prep work 
#Import data
lakehuron <- as.data.frame(datasets::LakeHuron)

#extract columns 
lakehuron$year <- as.yearmon(time(lakehuron$x)) %>% 
  as.numeric(format(., "%Y"))

lakehuron$x <- as.numeric(lakehuron$x)

colnames(lakehuron) <- c("elevation","year")


#Ui 
ui <- shinyUI(fluidPage(
  
  titlePanel("Lake Huron Elevation Model Smoothing"),
  
  sidebarPanel(
    sliderInput("span",
                "Span: (Loess)",
                min = 0,
                max = 1,
                value = 0.25, 
                step = 0.05)
  ,
    sliderInput("knots",
                  "Knots: (Cubic Spline)",
                  min = 0,
                  max = 100,
                  value = 20)
  ),
  
  
  mainPanel(
    plotOutput("lakeplot")
  )
))



#Server
server <- shinyServer(function(input, output) {
  
  output$lakeplot <- renderPlot({ 
    
    #Loess
    lakehuron.lo <- loess(elevation ~ year, data = lakehuron, span = input$span)
    lakehuron$pred.loess <- predict(lakehuron.lo)
    
    
    
    #Spline (w 20 knots)
    lake.spl <- gam(elevation ~ s(year, bs = "cr", k = input$knots), data = lakehuron)
    lakehuron$pred.spl <- predict(lake.spl)
    
    #Base plot  
    lake.plot <-  ggplot(data = lakehuron, aes(x = year, y = elevation)) + 
      geom_point()  
    
    #Add smoother lines
    lake.plot %+% 
      geom_line(aes(y = pred.loess), size = 1, col = "red", data = lakehuron) %+%  #Loess
      geom_line(aes(y = pred.spl), size = 1, col = "blue", data = lakehuron) + #Spline 
      ggtitle("Elevation by Year for Lake Huron") + 
      theme_classic() 
    
    
  })
})


#Run App 

shinyApp(ui = ui, server = server)