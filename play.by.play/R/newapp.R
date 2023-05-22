
# load packages
library(shiny)
library(tidyverse)
library(nbastatR)
library(jsonlite)
library(httr)
library(vroom)
library(ggforce)
library(sportyR)

# get player picture from the ballr package by Todd Schneider

#player_photo_url = function(player_id) {
#  paste0("https://stats.nba.com/media/players/230x185/", player_id, ".png")
#"https://stats.nba.com/media/img/teams/logos/season/2022-23/LAL_logo.png"
#}

# get team logo from ccagrawal/nbaTools

#GetTeamLogo <- function(team.id) {
#
#  url <- gsub('###', team.id, 'http://a.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/###.png')
#  temp <- tempfile()
#  download.file(url, temp, mode = "wb")
#  pic <- readPNG(temp)
#  file.remove(temp)
#
# return(rasterGrob(pic, interpolate = TRUE))
#}

# create function that gets pbp logs from Owen Phillips How To: Accessing Live NBA Play-By-Play Data

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



get_data <- function(id) {

  url <- paste0("https://cdn.nba.com/static/json/liveData/playbyplay/playbyplay_00", id, ".json")
  res <- GET(url = url, add_headers(.headers=headers))

  json_resp <- fromJSON(content(res, "text"))
  df <- data.frame(json_resp[["game"]][["actions"]])

  df$gameid <- id

  return(df)
}


#From here everything is my own code, the draw basketball court function is from the sportyR package.

#get distinct dates

game_dates <- game_logs %>%
  select(dateGame) %>%
  distinct()

#get game id by matchup

get_id <- function (matchup){
  game_logs <- filter(game_logs, slugMatchup == matchup)
  id <- unique(game_logs$idGame)

  return(id)
}

# get data for specific game

game_data <- function(id){

  pbpdat <- map_df(id, get_data)
  return(pbpdat)
}

# team logo url
#get_logo_home <- function (game_id, game_logs){
#  game_logs <- filter(game_logs, game_logs$idGame == game_id)
#  game_logs <- filter(game_logs, game_logs$locationGame == "H")
#  team_id <- game_logs$idTeam
#  src <- paste0("https://cdn.nba.com/logos/nba/", team_id,"/global/L/logo.svg")
 # return(src)
#}

get_logo_home <- function(game_id) {
  filtered_logs <- filter(game_logs, idGame == game_id)
  filtered_logs <- filter(filtered_logs, locationGame == "H")
  team_id <- filtered_logs$idTeam
  src <- paste0("https://cdn.nba.com/logos/nba/", team_id, "/global/L/logo.svg")
  return(src)
}

get_logo_away <- function (game_id){
  game_logs <- filter(game_logs, game_logs$idGame == game_id)
  game_logs <- filter(game_logs, game_logs$locationGame == "A")
  team_id <- na.omit(unique(game_logs$idTeam))
  src <- paste0("https://cdn.nba.com/logos/nba/", team_id,"/global/L/logo.svg")
  return(src)
}

# player picture

get_player_picture <- function(data, player){
  pbpdat <- data
  pbpdat <- filter(pbpdat, pbpdat$playerNameI == player)
  player_id <- unlist(na.omit(unique(pbpdat$personId)))
  src <- paste0("https://cdn.nba.com/headshots/nba/latest/260x190/", player_id, ".png")
  return(src)
}


#'Computes the x and y coordinates of shots, based on NBA play by play data
#'
#'@param data dataframe with complete NBA play by play data
#'
#'@param period input variable to filter coordinates by period
#'
#'@param time input variable to filter coordinates by time left in period
#'
#'@param team input variable to filter coordinates by team
#'
#'@return dataframe with adjusted x and y coordinates to fit on basketball court plot

game_coordinates <- function(data, period, time, team, player){

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

  if(player != "all"){
    pbpdat <- filter(pbpdat, pbpdat$playerNameI == player)

  }

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
  titlePanel("NBA play-by-by"),
      fluidRow(
        column(3,
               htmlOutput("team_away"),
        ),
        column(3,
               htmlOutput("team_home"),
        ),
        column(3,
              conditionalPanel(condition = "input$player == 'all'",
              htmlOutput("player_default_picture")),
              conditionalPanel(condition = "input$player != 'all'",
              htmlOutput("player_picture"))
        )
      ),
      fluidRow(
        column(8,
               plotOutput("plot"),
        ),
        column(4,
               selectInput("date", "select date", game_dates),
               selectInput("game", "Game", NULL),
               selectInput("team", "Team", NULL),
               selectInput("player", "Player", NULL)
        )
      ),
      fluidRow(
        column(8,
               textOutput("description"),
        ),
        column(4,
               selectInput("period", "select period", c("all", 1, 2, 3, 4, 5, 6)),
               selectInput("time", "select time left in period", NULL),
        )
      )
    )

server <- function(input, output, session) {

  date <- reactive({
    filter(game_logs, dateGame == input$date)
  })
  observeEvent(date(), {
    choices <- unique(date()$slugMatchup)
    freezeReactiveValue(input, "game")
    updateSelectInput(inputId = "game", choices = choices)
  })

  game_id <- reactive(get_id(input$game))


  gamedata <- reactive(game_data(game_id()))

  period <- reactive({
    req(input$game)
    filter(gamedata(), period == input$period)
  })
  observeEvent(period(), {
    choices <- unique(period()$clock)
    freezeReactiveValue(input, "time")
    updateSelectInput(inputId = "time", choices = c("all", choices))
  })

  team <- reactive({
    req(input$game)
    unique(gamedata()$teamTricode)
  })
  observeEvent(team(), {
    choices <- unique(gamedata()$teamTricode)
    freezeReactiveValue(input, "team")
    updateSelectInput(inputId = "team", choices = c("both", choices))
  })

  team_selected <- reactive({
    req(input$game)
    filter(gamedata(), teamTricode == input$team )
  })
  observeEvent(team_selected(), {
    choices <- unique(team_selected()$playerNameI)
    freezeReactiveValue(input, "player")
    updateSelectInput(inputId = "player", choices = c("all", choices))
  })

  coordinates <- reactive(game_coordinates(gamedata(), input$period, input$time, input$team, input$player))

  output$plot <- renderPlot({
    draw_court(coordinates())
    }, res = 96)

  description <- reactive({
    req(input$game)
    filter(gamedata(), clock == input$time)
  })

  output$description <- renderText({
    description()$description
  })

  game_logs <- game_logs(seasons = 2023,
                         result_types = 'team',
                         season_types = "Regular Season")


  src_away <- reactive(get_logo_away(game_id()))

  src_home <- reactive(get_logo_home(game_id()))

  src_player <- reactive(get_player_picture(gamedata(), input$player))



#  output$team_away<-renderUI({
#      paste0('<img src="',src_away(),'">')
#  })

 # output$team_home<-renderUI({
#  paste0('<img src="',src_home(),'">')
# })

  output$team_home<-renderUI({
  tags$img(src = src_home(),
           alt = "photo",
           style = "width: 200px; height: 200px;")
  })

  output$team_away<-renderUI({
    tags$img(src = src_away(),
             alt = "photo",
             style = "width: 200px; height: 200px;")
  })

  output$player_picture <- renderUI({
    if(input$player != "all"){
      tags$img(src = src_player(),
               alt = "picture",
               style = "width: auto; height: 200px;")
    }
  })

  output$player_default_picture <- renderUI({
    if(input$player == "all"){
      tags$img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/0/03/National_Basketball_Association_logo.svg/315px-National_Basketball_Association_logo.svg.png?20221026000552",
             alt = "df",
             style = "width: auto; height: 200px;")
    }
  })
}



shinyApp(ui, server)
