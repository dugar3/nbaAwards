library(shiny)

library(gridExtra)
library(shinyWidgets)
#shiny background color

ui = fluidPage(
  tags$h2(""),
  setBackgroundColor("snow"),
  titlePanel("NBA Awards Predictor"), # Title
  
  sidebarLayout(
    sidebarPanel(
      h3("Award Selection"),
      
      # Dropdowns
      selectInput(inputId ="award", "What award do you want to predict?", 
                  choices = c("MVP", "Rookie of the Year", "Defensive Player of the Year",
                              "Sixth Man of the Year")),
      
      numericInput(inputId = "year", label = ("Year (e.g. 1980)"), 
                   value = 2019, min = 1989, max = 2019),
      
      actionButton("button", "Load Preview Awards")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Summary",
      "Top Six Predictions",
      verbatimTextOutput("predictions")
        ),
      tabPanel(
      "Radar Plot of 1st Place",
      plotlyOutput("radar1"),
      style = "border-color: white"
      ),
      tabPanel(
        "Radar Plot of 2nd Place",
        plotlyOutput("radar2"),
        style = "border-color: white"
      ),
      tabPanel(
        "Radar Plot of 3rd Place",
        plotlyOutput("radar3"),
        style = "border-color: white"
      ),
      tabPanel(
        "Radar Plot of 4th Place",
        plotlyOutput("radar4"),
        style = "border-color: white"
      ),
      tabPanel(
        "Radar Plot of 5th Place",
        plotlyOutput("radar5"),
        style = "border-color: white"
      ),
      tabPanel(
        "Radar Plot of 6th Place",
        plotlyOutput("radar6"),
        style = "border-color: white"
      )
      )
    )
  )
)

# Define server logic ----
server = function(input, output, session) {
  
  awards_classification = eventReactive(input$button, input$award)

  active_year = eventReactive(input$button, input$year)
  
  #matching dataset year
  active_dataset = eventReactive(input$button, {
    switch(paste0("players_", active_year()),
           "players_1989" = players_1989,
           "players_1990" = players_1990,
           "players_1991" = players_1991,
           "players_1992" = players_1992,
           "players_1993" = players_1993,
           "players_1994" = players_1994,
           "players_1995" = players_1995,
           "players_1996" = players_1996,
           "players_1997" = players_1997,
           "players_1998" = players_1998,
           "players_1999" = players_1999,
           "players_2000" = players_2000,
           "players_2001" = players_2001,
           "players_2002" = players_2002,
           "players_2003" = players_2003,
           "players_2004" = players_2004,
           "players_2005" = players_2005,
           "players_2006" = players_2006,
           "players_2007" = players_2007,
           "players_2008" = players_2008,
           "players_2009" = players_2009,
           "players_2010" = players_2010,
           "players_2011" = players_2011,
           "players_2012" = players_2012,
           "players_2013" = players_2013,
           "players_2014" = players_2014,
           "players_2015" = players_2015,
           "players_2016" = players_2016,
           "players_2017" = players_2017,
           "players_2018" = players_2018,
           "players_2019" = players_2019
           
    )
  })
  #fit_mvp = eventReactive(input$perform, ) if we can generalize the fit, then develop this
  

  active_awards = eventReactive(input$button, {
    switch(input$award,
           "MVP" = active_dataset()$mvp, 
           "Rookie of the Year" = active_dataset()$roy, 
           "Defensive Player of the Year"= active_dataset()$dpoy,
           "Sixth Man of the Year" = active_dataset()$smoy
    )
  })
  
  active_predictions = eventReactive(input$button, {
    switch(input$award,
           "MVP" = active_dataset()$mvp_odds, 
           "Rookie of the Year" = active_dataset()$roy_odds, 
           "Defensive Player of the Year"= active_dataset()$dpoy_odds,
           "Sixth Man of the Year" = active_dataset()$smoy_odds
    )
  })
  
  df_predictions = eventReactive(input$button,{
    df_predictions = data.frame(Player = active_dataset()$player, 
                             Position = active_dataset()$pos, 
                             Age = active_dataset()$age,
                             Probability = active_predictions()) 
    return(df_predictions)
  })

  cleaning_df = eventReactive(input$button,{
    cleaning_df = data.frame(Player = active_dataset()$player, 
                    Position = active_dataset()$pos, 
                    Age = active_dataset()$age,
                    Probability = active_awards()) 
    return(cleaning_df)
  })

  
  df_graph = eventReactive(input$button, {
    df_graph = data.frame(player = active_dataset()$player,
                          per = active_dataset()$per, 
                          tspercent= active_dataset()$tspercent, 
                          pts = active_dataset()$pts, 
                          ast = active_dataset()$ast, 
                          trb = active_dataset()$trb,
                          Probability = active_predictions())
    df_graph = df_graph[with(df_graph, order(-Probability)),]
    df_graph = df_graph[,-7]
    return(df_graph)
  })
  
  
 
  active_radar = eventReactive(input$button, {
    mvp_1990_graph_display = df_graph()
    for (i in colnames(mvp_1990_graph_display)) {
      if (i == "player") {
        
        next;
      }
      mvp_1990_graph_display[,i] = normalize_data(mvp_1990_graph_display[,i])
    }
    return(mvp_1990_graph_display)
  })
  #create column for players and percentages
  output$predictions = renderPrint(
      head(df_predictions()[with(df_predictions(), order(-Probability)),])
  )
  
  output$winner = renderPrint(
    cleaning_df()[cleaning_df()$Probability == 1,]
  )
  
  output$radar1 = renderPlotly(
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) %>%
      add_trace(
        r = as.numeric(active_radar()[1, c("per", "tspercent", "pts", "ast", "trb")]),
        theta = c('per','tspercent','pts', 'ast', "trb"),
        name = active_radar()[1, "player"]
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0, 100)
          )
        )
      )
  )
  
  output$radar2 = renderPlotly(
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) %>%
      add_trace(
        r = as.numeric(active_radar()[2, c("per", "tspercent", "pts", "ast", "trb")]),
        theta = c('per','tspercent','pts', 'ast', "trb"),
        name = active_radar()[2, "player"]
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0, 100)
          )
        )
      )
  )
  
  output$radar3 = renderPlotly(
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) %>%
      add_trace(
        r = as.numeric(active_radar()[3, c("per", "tspercent", "pts", "ast", "trb")]),
        theta = c('per','tspercent','pts', 'ast', "trb"),
        name = active_radar()[3, "player"]
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0, 100)
          )
        )
      )
  )
  
  output$radar4 = renderPlotly(
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) %>%
      add_trace(
        r = as.numeric(active_radar()[4, c("per", "tspercent", "pts", "ast", "trb")]),
        theta = c('per','tspercent','pts', 'ast', "trb"),
        name = active_radar()[4, "player"]
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0, 100)
          )
        )
      )
  )
  
  output$radar5 = renderPlotly(
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) %>%
      add_trace(
        r = as.numeric(active_radar()[5, c("per", "tspercent", "pts", "ast", "trb")]),
        theta = c('per','tspercent','pts', 'ast', "trb"),
        name = active_radar()[5, "player"]
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0, 100)
          )
        )
      )
  )
  
  output$radar6 = renderPlotly(
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) %>%
      add_trace(
        r = as.numeric(active_radar()[6, c("per", "tspercent", "pts", "ast", "trb")]),
        theta = c('per','tspercent','pts', 'ast', "trb"),
        name = active_radar()[6, "player"]
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0, 100)
          )
        )
      )
  )
  

}
# Launch the App
shinyApp(ui, server)
