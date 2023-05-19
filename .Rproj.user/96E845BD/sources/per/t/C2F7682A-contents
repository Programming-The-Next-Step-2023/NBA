
# load packages
library(shiny)
library(tidyverse)
library(nbastatR)
library(jsonlite)
library(httr)
library(vroom)
library(ggforce)
library(sportyR)

pbpdatgame1 <- filter(pbpdatgame1, pbpdatgame1$period == 2)

# create function that gets pbp logs 
get_data <- function(id) {
  headers = c(
    `Connection` = 'keep-alive',
    `Accept` = 'application/json, text/plain, */*',
    `x-nba-stats-token` = 'true',
    `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
    `x-nba-stats-origin` = 'stats',
    `Sec-Fetch-Site` = 'same-origin',
    `Sec-Fetch-Mode` = 'cors',
    `Referer` = 'https://stats.nba.com/players/leaguedashplayerbiostats/',
    `Accept-Encoding` = 'gzip, deflate, br',
    `Accept-Language` = 'en-US,en;q=0.9'
  )
  
  # get game logs from the reg season
  game_logs <- game_logs(seasons = 2023, 
                         result_types = 'team', 
                         season_types = "Regular Season")
  
  # Get a list of distinct game ids 
  game_ids <- game_logs %>% 
    select(idGame) %>% 
    distinct() 
  
  url <- paste0("https://cdn.nba.com/static/json/liveData/playbyplay/playbyplay_00", id, ".json")
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_resp <- fromJSON(content(res, "text"))
  df <- data.frame(json_resp[["game"]][["actions"]])
  
  df$gameid <- id
  
  return(df)
}

# get data for specific game
game_data <- function(id){
  
  pbpdat <- map_df(id, get_data)
  return(pbpdat)
}


game_coordinates <- function(data, period, time, team){
  
  pbpdat <- data
  
  #Transform coordinates
  
  pbpdat$x <- ifelse(pbpdat$x < 50, pbpdat$x + 1.5, pbpdat$x - 1.5)
  
  pbpdat$y <- pbpdat$y/2
  
  teams <- unique(pbpdat$teamTricode)
  
  # split by period
  if(period != "all"){
    if(period == 1){
      pbpdat <- filter(pbpdat, pbpdat$period == 1)
    }
    else if(period == 2){
      pbpdat <- filter(pbpdat, pbpdat$period == 2)
    }
    else if(period == 3){
      pbpdat <- filter(pbpdat, pbpdat$period == 3)
    }
    else if(period == 4){
      pbpdat <- filter(pbpdat, pbpdat$period == 4)
    }
  }
  
  # split by player
  
  # split by team
  
  if(team != "both"){
    pbpdat <- filter(pbpdat, pbpdat$teamTricode == team)
    
  }
  
  #split by time left in period
  
  if(time != "all"){
    pbpdat <- filter(pbpdat, pbpdat$clock == time)
  }
  
  return(pbpdat)
}


#Draw Court
draw_court <- function(dat) {
  
  geom_basketball(league = "NBA", x_trans = 50, y_trans = 25) + geom_point(data = dat, aes(x=x, y=y, color = shotResult )) + scale_color_manual(values = c("Missed" = "red", "Made" = "green"))+ theme_void()
  
}



#description


#action_number



# shinyapp

ui <- fluidPage(
  selectInput("gameid", "Fill in gameid", game_ids),
  selectInput("period", "select period", c("all", 1, 2, 3, 4, 5, 6)),
  selectInput("time", "select time left in period", NULL),
  selectInput("team", "Team", NULL), #need dynamic ui to change team codes
  plotOutput("plot", width = "400px")
)
server <- function(input, output, session) {
  
  gamedata <- reactive(game_data(input$gameid))
  
  period <- reactive({
    filter(gamedata(), period == input$period)
  })
  observeEvent(period(), {
    choices <- unique(period()$clock)
    updateSelectInput(inputId = "time", choices = c("all", choices))
  })
  
  team <- reactive({
    unique(gamedata()$teamTricode)
  })
  observeEvent(team(), {
    choices <- unique(gamedata()$teamTricode)
    updateSelectInput(inputId = "team", choices = c("both", choices))
  })
  
  coordinates <- reactive(game_coordinates(gamedata(), input$period, input$time, input$team))
  
  output$plot <- renderPlot(draw_court(coordinates()))
  
}

shinyApp(ui, server)