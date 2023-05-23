reactiveConsole(TRUE)

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

logs <- readRDS(file = "C:/Users/henry/OneDrive/Dokumente/NBA/play.by.play/data/game_logs.Rds")

get_data <- function(id) {

  url <- paste0("https://cdn.nba.com/static/json/liveData/playbyplay/playbyplay_00", id, ".json")
  res <- httr::GET(url = url, httr::add_headers(.headers=headers))

  json_resp <- jsonlite::fromJSON(httr::content(res, "text"))
  df <- data.frame(json_resp[["game"]][["actions"]])

  df$gameid <- id

  return(df)
}


#From here everything is my own code, the draw basketball court function is from the sportyR package.

#get distinct dates

game_dates <- dplyr::distinct(dplyr::select(logs, dateGame))

#get game id by matchup

get_id <- function (matchup, in_date){
  logs <- dplyr::filter(logs, slugMatchup == matchup)
  logs <- dplyr::filter(logs, dateGame == in_date)
  id <- unique(logs$idGame)

  return(id)
}

# get data for specific game

game_data <- function(id){

  pbpdat <- purrr::map_df(id, get_data)
  return(pbpdat)
}

get_logo_home <- function(data, game_id) {
  data <- dplyr::filter(data, idGame == game_id)
  data <- dplyr::filter(data, grepl("H", locationGame))
  team_id <- na.omit(unique(data$idTeam))
  src <- paste0("https://cdn.nba.com/logos/nba/", team_id,"/global/L/logo.svg")
  return(src)
}

get_logo_away <- function (data, game_id){
  data <- dplyr::filter(data, idGame == game_id)
  data <- dplyr::filter(data, locationGame == "A")
  team_id <- na.omit(unique(data$idTeam))
  src <- paste0("https://cdn.nba.com/logos/nba/", team_id,"/global/L/logo.svg")
  return(src)
}

# player picture

get_player_picture <- function(data, player){
  pbpdat <- data
  pbpdat <- dplyr::filter(pbpdat, pbpdat$playerNameI == player)
  player_id <- unlist(na.omit(unique(pbpdat$personId)))
  src <- paste0("https://cdn.nba.com/headshots/nba/latest/260x190/", player_id, ".png")
  return(src)
}


#'Computes the x and y coordinates of shots, based on NBA play by play data
#'
#'@param data gamedata() with complete NBA play by play data
#'
#'@param period input variable to dplyr::filter coordinates by period
#'
#'@param time input variable to dplyr::filter coordinates by time left in period
#'
#'@param team input variable to dplyr::filter coordinates by team
#'
#'@return gamedata() with adjusted x and y coordinates to fit on basketball court plot

game_coordinates <- function(data, period, time, team, player){

  pbpdat <- data

  #Transform coordinates

  pbpdat$x <- ifelse(pbpdat$x < 50, pbpdat$x + 1.5, pbpdat$x - 1.5)

  pbpdat$y <- pbpdat$y/2

  teams <- unique(pbpdat$teamTricode)

  # split by period
  if(period != "all"){
    if(period == 1){
      pbpdat <- dplyr::filter(pbpdat, pbpdat$period == 1)
    }
    else if(period == 2){
      pbpdat <- dplyr::filter(pbpdat, pbpdat$period == 2)
    }
    else if(period == 3){
      pbpdat <- dplyr::filter(pbpdat, pbpdat$period == 3)
    }
    else if(period == 4){
      pbpdat <- dplyr::filter(pbpdat, pbpdat$period == 4)
    }
  }

  # split by player

  if(player != "all"){
    pbpdat <- dplyr::filter(pbpdat, pbpdat$playerNameI == player)

  }

  # split by team

  if(team != "both"){
      pbpdat <- dplyr::filter(pbpdat, pbpdat$teamTricode == team)

  }

  #split by time left in period

   if(time != "all"){
    pbpdat <- dplyr::filter(pbpdat, pbpdat$clock == time)
  }

  return(pbpdat)
}


  #Draw Court
draw_court <- function(dat) {

sportyR::geom_basketball(league = "NBA", x_trans = 50, y_trans = 25) + ggplot2::geom_point(data = dat, ggplot2::aes(x=x, y=y, color = shotResult )) + ggplot2::scale_color_manual(values = c("Missed" = "red", "Made" = "green"))+ ggplot2::theme_void()

}


#description

getRowData <- function(row_index) {
  row <- df[row_index, ]
  return(row)
}

#action_number



# shinyapp

ui <- shiny::fluidPage(
  shiny::titlePanel("NBA play-by-by"),
  shiny::fluidRow(
    shiny::column(3,
                  shiny::htmlOutput("team_away"),
        ),
    shiny::column(3,
                  shiny::htmlOutput("team_home"),
        ),
    shiny::column(3,
                  shiny::conditionalPanel(condition = "input$player == 'all'",
                                          shiny::htmlOutput("player_default_picture")),
                  shiny::conditionalPanel(condition = "input$player != 'all'",
                                          shiny::htmlOutput("player_picture"))
        )
      ),
  shiny::fluidRow(
    shiny:: column(8,
                   shiny::plotOutput("plot"),
        ),
    shiny::column(4,
                  shiny::selectInput("date", "select date", game_dates),
                  shiny::selectInput("game", "Game", NULL),
                  shiny::selectInput("team", "Team", NULL),
                  shiny::selectInput("player", "Player", NULL)
        )
      ),
  shiny::fluidRow(
    shiny::column(8,
                  shiny::tableOutput("description"),
                  shiny::numericInput("play", "Play", NULL),
                  shiny::tableOutput("description_pbp")
        ),
    shiny::column(4,
                  shiny::selectInput("period", "select period", c("all", 1, 2, 3, 4, 5, 6)),
                  shiny::selectInput("time", "select time left in period", NULL),
        )
      )
    )

server <- function(input, output, session) {

  date <- shiny::reactive({
    dplyr::filter(logs, input$date == dateGame & grepl("@", slugMatchup))
  })
  shiny::observeEvent(date(), {
    choices <- unique(date()$slugMatchup)
    freezeReactiveValue(input, "game")
    updateSelectInput(inputId = "game", choices = choices)
  })

  game_id <- shiny::reactive(get_id(input$game, input$date))

  gamedata <- shiny::reactive(game_data(game_id()))

  period <- shiny::reactive({
    req(input$game)
    dplyr::filter(gamedata(), period == input$period)
  })
  shiny::observeEvent(period(), {
    choices <- unique(period()$clock)
    shiny::freezeReactiveValue(input, "time")
    shiny::updateSelectInput(inputId = "time", choices = c("all", choices))
  })

  play <- shiny::reactive({
    req(input$play)
  })
  shiny::observeEvent(play(), {
    choices <- unique(description_pbp()$clock)
    shiny::freezeReactiveValue(input, "time")
    shiny::updateSelectInput(inputId = "time", choices = choices)
  })

  team <- shiny::reactive({
    shiny::req(input$game)
    unique(gamedata()$teamTricode)
  })
  shiny::observeEvent(team(), {
    choices <- unique(gamedata()$teamTricode)
    shiny::freezeReactiveValue(input, "team")
    shiny::updateSelectInput(inputId = "team", choices = c("both", choices))
  })

  team_selected <- shiny::reactive({
    shiny::req(input$game)
    dplyr::filter(gamedata(), teamTricode == input$team )
  })
  shiny::observeEvent(team_selected(), {
    choices <- unique(team_selected()$playerNameI)
    shiny::freezeReactiveValue(input, "player")
    shiny::updateSelectInput(inputId = "player", choices = c("all", choices))
  })

  coordinates <- shiny::reactive(game_coordinates(gamedata(), input$period, input$time, input$team, input$player))

  output$plot <- shiny::renderPlot({
    draw_court(coordinates())
    }, res = 96)


  #play by play functionality

  pbp_num <- shiny::reactive(1:nrow(gamedata()))

  pbp_data <- shiny::reactive(cbind(gamedata(), pbp_num()))

  description_pbp <- shiny::reactive({
    dplyr::filter(pbp_data(), as.numeric(pbp_num()) == input$play)
  })

  output$description_pbp <- shiny:: renderTable({
    dplyr::select(description_pbp(), clock, description, scoreAway, scoreHome)
  })

  #pictures

  src_away <- shiny::reactive(get_logo_away(logs, game_id()))

  src_home <- shiny::reactive(get_logo_home(logs, game_id()))

  src_player <- shiny::reactive(get_player_picture(gamedata(), input$player))


  output$team_home<-shiny::renderUI({
  tags$img(src = src_home(),
           alt = "photo",
           style = "width: 200px; height: 200px;")
  })

  output$team_away<-shiny::renderUI({
    tags$img(src = src_away(),
             alt = "photo",
             style = "width: 200px; height: 200px;")
  })

  output$player_picture <- shiny::renderUI({
    if(input$player != "all"){
      tags$img(src = src_player(),
               alt = "picture",
               style = "width: auto; height: 200px;")
    }
  })

  output$player_default_picture <- shiny::renderUI({
    if(input$player == "all"){
      tags$img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/0/03/National_Basketball_Association_logo.svg/315px-National_Basketball_Association_logo.svg.png?20221026000552",
             alt = "df",
             style = "width: auto; height: 200px;")
    }
  })
}

#'@export
run_NBA_pbp <- function() {
  shiny::shinyApp(ui = ui, server = server)
}

run_NBA_pbp()

shiny::shinyApp(ui = ui, server = server)
