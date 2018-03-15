#StatR 502
#Final Project - shiny app 
#Rebecca Hadi

#Load packages 
library(ggplot2)
library(dplyr)
library(scales)
library(broom)
library(ggthemes)
library(vcd)
library(broom)
library(car)
library(arm) #invlogit
library(kableExtra)
library(shiny)


#Prep work 
#Import data
movies.shiny <- read.csv("https://gist.githubusercontent.com/bhadi26/2537a988522aabea361e47ad4d03b83a/raw/c95515296fa02fa0a695548320a374d00e4f3ed6/processed_movies.csv")


#Ui 
ui <- shinyUI(fluidPage(
  titlePanel("TMDB Movies Data Set Exploration and Modeling"),
    sidebarPanel(
      h4("Exploratory Filters (does not impact model)"),
      sliderInput("year",
                  "Year",
                  min = 2000,
                  max = 2016,
                  value = c(2000,2016), 
                  step = 1)
  ,
    sliderInput("revenue",
                "Revenue",
                min = 10000,
                max = 3000000000,
                value = c(10000,3000000000), 
                step = 1)
  
  ,
  sliderInput("budget",
              "Budget",
              min = 10000,
              max = 380000000,
              value = c(10000,380000000), 
              step = 1)
  , 
  selectInput("genres", 
              "Genre",
              c("All","Action/Adventure","Drama", "Family/Animation","Sci-fi/Fantasy","Crime","Romance", 
                "Comedy", "Thriller/Mystery/Horror","Western","Music","War/History","Documentary"))
  ,h4("Model Selection (Exploratory filters don't impact)")
  ,selectInput("modeltype",  
               "Select Model Variables", 
               c("Revenue (Log transform), Vote Average, Vote Count, Run Time",
                 "Revenue (Log transform)",
                 "Revenue (Log transform) + Budget",
                 "Revenue (Log transform) + Vote Average (centered/scaled)", 
                 "Revenue (Log transform) + Vote Count (centered/scaled)"))
  ),
  mainPanel(
    tabsetPanel(
    tabPanel("Exploratory", plotOutput("mplot"),plotOutput("medianplot")),
    tabPanel("Model", plotOutput("smoothplot"),plotOutput("residplot")))
  )
))


#Server
server <- shinyServer(function(input, output) {
  #movie plot 
  genrelist <-  c("Documentary","Action/Adventure","Drama", "Family/Animation","Sci-fi/Fantasy","Crime","Romance", 
                  "Comedy", "Thriller/Mystery/Horror","Western","Music","War/History") 
  
  movies.shiny$vote_count.z <- (movies.shiny$vote_count - mean(movies.shiny$vote_count)) / sd(movies.shiny$vote_count)
  movies.shiny$runtime.z <- (movies.shiny$runtime - mean(movies.shiny$runtime)) / sd(movies.shiny$runtime)
  movies.shiny$vote_average.z <- (movies.shiny$vote_average - mean(movies.shiny$vote_average)) / sd(movies.shiny$vote_average)
  
  m <- reactive({movies.shiny %>% 
       filter(release_year >= input$year[1], 
              release_year <= input$year[2], 
              revenue >= input$revenue[1],
              revenue <= input$revenue[2], 
              budget >= input$budget[1],
              budget <= input$budget[2]) 
  })
    
  m1 <- reactive({if(input$genres != "All") {
         m() %>% filter(genre == input$genres)
          } else { 
            m()      
            }  
        })
    
  output$mplot <- renderPlot({ 
    #create plot based on filtered data
     ggplot(data = m1(), aes(x = budget / 1000000, y = revenue / 1000000, col = profitable)) + 
      geom_point(alpha = 0.5) +  
      scale_x_continuous(label = unit_format(unit = "M")) + 
      scale_y_continuous(label = unit_format(unit = "M")) +  
      theme_few() +  
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      ggtitle("Revenue vs. Budget - Profitability") +
      facet_wrap (~ genre) + 
      labs(x = "Budget (in Millions)", y = "Revenue (in Millions)")
    
    })
  
    #Summary stats plot (used same filtered data)
   output$medianplot <- renderPlot({
      ggplot(data = m1(), aes(x = genre, y = vote_count)) + 
      geom_boxplot() + 
      theme_few() + 
      coord_flip()  +  
      labs(x = "Genre", y = "Vote Count") +
      facet_wrap(~profitable) + 
      ggtitle("Summary of Votes by Genre and Profitability")
  
  })
   
  #Residual Plot and smoother 
   fmod <-  glm(data = movies.shiny, profitable.ind ~ log(revenue)  +  runtime.z + vote_average.z + vote_count.z, family = binomial(link = "logit"))
   rev <-  glm(data = movies.shiny,profitable.ind ~ log(revenue), family = binomial(link = "logit"))
   voteaverage <-  glm(data = movies.shiny,profitable.ind ~ log(revenue) + vote_average.z, family = binomial(link = "logit"))    
   votecount <-  glm(data = movies.shiny,profitable.ind ~ log(revenue) + vote_count.z, family = binomial(link = "logit"))  
   budget <-  glm(data = movies.shiny, profitable.ind ~ log(revenue) + budget, family = binomial(link = "logit"))            
   
   #parse out which model was chosen
   chosen.mod <- reactive({ 
     if(input$modeltype == "Revenue (Log transform), Vote Average, Vote Count, Run Time") {
       fmod} 
     else if (input$modeltype == "Revenue (Log transform)") {
       rev
     }
     else if (input$modeltype == "Revenue (Log transform) + Budget") {
       budget
     }else if (input$modeltype == "Revenue (Log transform) + Vote Average (centered/scaled)"){
       voteaverage
     }else if (input$modeltype == "Revenue (Log transform) + Vote Count (centered/scaled)"){
       votecount} 
     else {fmod}
   })
   
   
   #augment based on model chosen
   aug.reactive <- reactive({augment(chosen.mod(), type.predict = "response")})
   
   output$smoothplot <- renderPlot({ 
     ggplot(data = aug.reactive(), aes(x = log.revenue., y = profitable.ind)) + 
       geom_jitter(height = 0.02) +  
       geom_point(data = aug.reactive(), aes(x = log.revenue., y = .fitted), color = "#00cdcd", alpha = 0.2) +
       geom_smooth(method = "glm", se = F, 
                   method.args = list(family = "binomial")) + 
       theme_few() + 
       labs(x = "Log Revenue", y = "Probability of Profitability") +
       ggtitle("Profitability - Actual vs. Predicted")
     
   })
   
   #residuals
   profit.bin <- reactive({aug.reactive() %>%
       mutate(q_group = cut(log.revenue.,
                            breaks = quantile(log.revenue., seq(0, 1, length.out = 30),
                                              include.lowest = TRUE))) %>%
       group_by(q_group) %>%
       summarize(.resid = mean(.resid),
                 log.revenue = mean(log.revenue.))
   })
   
   #plot
   output$residplot <- renderPlot({ ggplot(profit.bin(), aes(x = log.revenue, y = .resid)) +
       geom_point() +
       geom_smooth() + 
       ggtitle("Residual Plot") + 
       labs(x = "Log Revenue", y = "Residual") + 
       theme_few()
   })
   
   

#close server
})

#Run App 
shinyApp(ui = ui, server = server)
