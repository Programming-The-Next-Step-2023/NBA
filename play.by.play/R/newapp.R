
# create function that gets pbp logs from: Owen Phillips How To: Accessing Live NBA Play-By-Play Data

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


#'Uses the game id to return the full play by play data for this id from nba-stats.com
#'
#'@param id numeric, valid NBA game id
#'
#'@return df with play by play data

get_data <- function(id) {

  url <- paste0("https://cdn.nba.com/static/json/liveData/playbyplay/playbyplay_00", id, ".json")
  res <- httr::GET(url = url, httr::add_headers(.headers=headers))

  json_resp <- jsonlite::fromJSON(httr::content(res, "text"))
  df <- data.frame(json_resp[["game"]][["actions"]])

  df$gameid <- id

  return(df)
}


#From here everything is my own code

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

logs <- nbastatR::game_logs(seasons = 2023, result_types = "team")

game_dates <- unique(dplyr::select(logs, dateGame))


#get game id by matchup

#'Uses the matchup and date of a game to determine the game id
#'
#'@param matchup based on NBA game log data, in the format "XXX ad YYY" using team tri-codes.
#'
#'@return id of the game

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

#'Uses the game id and matchup to provid a link to the home teams logo
#'
#'@param game_id numeric, valid NBA game id
#'
#'@param data game logs of the NBA season
#'
#'@return src, source of link to picture of the home teams logo

get_logo_home <- function(data, game_id) {
  data <- dplyr::filter(data, idGame == game_id)
  data <- dplyr::filter(data, grepl("H", locationGame))
  team_id <- na.omit(unique(data$idTeam))
  src <- paste0("https://cdn.nba.com/logos/nba/", team_id,"/global/L/logo.svg")
  return(src)
}

#'Uses the game id and matchup to provid a link to the away teams logo
#'
#'@param game_id numeric, valid NBA game id
#'
#'@param data game logs of the NBA season
#'
#'@return src, source of link to picture of the away teams logo

get_logo_away <- function (data, game_id){
  data <- dplyr::filter(data, idGame == game_id)
  data <- dplyr::filter(data, locationGame == "A")
  team_id <- na.omit(unique(data$idTeam))
  src <- paste0("https://cdn.nba.com/logos/nba/", team_id,"/global/L/logo.svg")
  return(src)
}

# player picture

#'Uses the game id and play by play data to provid a link to the players photo
#'
#'@param game_id numeric, valid NBA game id
#'
#'@param data play by play data of a NBA game
#'
#'@return src, source of link to picture of the the players photo

get_player_picture <- function(data, player){
  pbpdat <- data
  pbpdat <- dplyr::filter(pbpdat, pbpdat$playerNameI == player)
  player_id <- unlist(na.omit(unique(pbpdat$personId)))
  src <- paste0("https://cdn.nba.com/headshots/nba/latest/260x190/", player_id, ".png")
  return(src)
}

get_court_home <- function(data, game_id) {
  data <- dplyr::filter(data, idGame == game_id)
  data <- dplyr::filter(data, grepl("H", locationGame))
  team_code <- na.omit(unique(data$slugTeam))
  src <- system.file("www", paste0(team_code, ".png"), package = "play.by.play")
  return(src)
}

get_points_home <- function(data, game_id){
  data <- dplyr::filter(data, idGame == game_id)
  data <- dplyr::filter(data, grepl("H", locationGame))
  return(data$ptsTeam)
}

get_points_away <- function(data, game_id){
  data <- dplyr::filter(data, idGame == game_id)
  data <- dplyr::filter(data, grepl("A", locationGame))
  return(data$ptsTeam)
}

team_stats <- function(data, game_id, team){
  data <- dplyr::filter(data, idGame == game_id)
  data <- dplyr::filter(data, slugTeam == team)
  data <- dplyr::select(data, ptsTeam, pctFGTeam, pctFG3Team, pctFTTeam, orebTeam, drebTeam, astTeam, stlTeam, blkTeam, tovTeam, plusminusTeam)
}

player_stats <- function(data, game_id, player){
  pbpdat <- data
  pbpdat <- dplyr::filter(pbpdat, pbpdat$playerNameI == player)
  player_id <- unlist(na.omit(unique(pbpdat$personId)))
  box_data <- nbastatR::box_scores(game_id, box_score_types = "Traditional", result_types = "player")
  box_data <- as.data.frame(box_data)
  dataBoxScore <- box_data$dataBoxScore
  dataBoxScore <- as.data.frame(dataBoxScore)
  dataBoxScore <- dplyr::filter(dataBoxScore, dataBoxScore $idPlayer == player_id)
  box_score <- dplyr::select(dataBoxScore, pts, pctFG, pctFG3, pctFT, oreb, dreb, ast, stl, blk, tov, plusminus)
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


#'Uses the play by play data to draw a NBA court with coordinates of shot locations on it
#'
#'@param data play by play data of a NBA game

draw_court <- function(dat, src_home_court) {

p <- ggplot2::ggplot(data = dat, ggplot2::aes(x=x, y=y, color = shotResult)) +
     ggplot2::geom_point() +
     ggplot2::scale_color_manual(values = c("Missed" = "red", "Made" = "green"))+
     ggplot2::xlim( -6, 112)+
     ggplot2::ylim( -9, 99)+
     ggplot2::theme_void()+
     ggplot2::theme(
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      legend.title.align = 0.7,
      legend.background = ggplot2::element_rect(color = "white", linetype = "solid")
     )
p <- p + ggplot2::theme(legend.background = ggplot2::element_rect(color = "white", linetype = "solid"))
img <- magick::image_read(src_home_court)
cowplot::ggdraw()+
  cowplot::draw_image(img, scale = 0.95, halign = -0.1 )+
  cowplot::draw_plot(p)
}


# shinyapp

ui <- shiny::fixedPage(
  shiny::titlePanel("NBA play-by-by"),
  shiny::fixedRow(
      shiny::column(2,
                shiny::span(shiny::textOutput("final_score"), style = "font-size:20px; font-weight:bold")
    ),
  ),
  shiny::fixedRow(
    style = "display: flex; align-items: center;",
    shiny::column(3,
                    shiny::htmlOutput("team_away"),

    ),
    shiny::column(2,
                  shiny::span(shiny::textOutput("score"), style = "font-size:20px; font-weight:bold")
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
  shiny::fixedRow(
    shiny:: column(8,
                   shiny::plotOutput("plot"),
        ),
    shiny::column(4,
                  shiny::selectInput("date", "Select the game date", game_dates),
                  shiny::selectInput("game", "Select the game", NULL),
                  shiny::selectInput("team", "Select the team", NULL),
                  shiny::selectInput("player", "Select the player", NULL),
                  shiny::selectInput("period", "Select the period", c("all", 1, 2, 3, 4, 5, 6)),
                  shiny::selectInput("time", "Select the time left in period", NULL),
        )
      ),
  shiny::fixedRow(
    shiny::column(
      width = 12,
      htmltools::h3("Team Box Scores"),
      shiny::conditionalPanel(
        condition = "input$team != 'both'",
        shiny::tableOutput("team_stats")
      )
    )
  ),
  shiny::fixedRow(
    shiny::column(
      width = 12,
      htmltools::h3("Player Box Scores"),
      shiny::conditionalPanel(
        condition = "input$player != 'all'",
        shiny::tableOutput("player_stats")
      )
    )
  ),
  shiny::fixedRow(
      shiny::column(4,
                    shiny::numericInput("play", "Select the play", NULL),
                    shiny::tableOutput("description_pbp")
      ),
    ),
  )

server <- function(input, output, session) {

  output$final_score <- shiny::renderText("FINAL SCORE")

  date <- shiny::reactive({
    dplyr::filter(logs, input$date == dateGame & grepl("@", slugMatchup))
  })
  shiny::observeEvent(date(), {
    choices <- unique(date()$slugMatchup)
    choices <- na.omit(choices)
    freezeReactiveValue(input, "game")
    updateSelectInput(inputId = "game", choices = choices)
  })

  game_id <- shiny::reactive(get_id(input$game, input$date))

  pts_away <- shiny::reactive(get_points_away(logs, game_id()))
  pts_home <- shiny::reactive(get_points_home(logs, game_id()))

  output$score <- shiny::renderText(paste0(pts_away()," - ",pts_home()))

  gamedataf <- shiny::reactive(game_data(game_id())[-1,])

  gamedata <- shiny::reactive(gamedataf()[-1,])

  period <- shiny::reactive({
    req(input$game)
    dplyr::filter(gamedata(), period == input$period)
  })
  shiny::observeEvent(period(), {
    choices <- unique(period()$clock)
    choices <- na.omit(choices)
    shiny::freezeReactiveValue(input, "time")
    shiny::updateSelectInput(inputId = "time", choices = c("all", choices))
  })

  play <- shiny::reactive({
    req(input$play)
  })
  shiny::observeEvent(play(), {
    choices_c <- unique(description_pbp()$clock)
    choices_p <- unique(description_pbp()$playerNameI)
    shiny::freezeReactiveValue(input, "time")
    shiny::updateSelectInput(inputId = "time", choices = choices_c)
    shiny::freezeReactiveValue(input, "player")
    shiny::updateSelectInput(inputId = "player", choices = choices_p)
  })

  team <- shiny::reactive({
    shiny::req(input$game)
    unique(gamedata()$teamTricode)
  })
  shiny::observeEvent(team(), {
    choices <- unique(gamedata()$teamTricode)
    choices <- na.omit(choices)
    shiny::freezeReactiveValue(input, "team")
    shiny::updateSelectInput(inputId = "team", choices = c("both", choices))
  })

  team_selected <- shiny::reactive({
    shiny::req(input$game)
    dplyr::filter(gamedata(), teamTricode == input$team )
  })
  shiny::observeEvent(team_selected(), {
    choices <- unique(team_selected()$playerNameI)
    choices <- na.omit(choices)
    shiny::freezeReactiveValue(input, "player")
    shiny::updateSelectInput(inputId = "player", choices = c("all", choices))
  })

  coordinates <- shiny::reactive(game_coordinates(gamedata(), input$period, input$time, input$team, input$player))

  src_home_court <- shiny::reactive(get_court_home(logs, game_id()))

  output$plot <- shiny::renderPlot({
    draw_court(coordinates(),  src_home_court())
    }, width = 750,
    height = 400,
    res = 96)


  #play by play functionality

  pbp_num <- shiny::reactive({
    if (input$team != "both") {
      team_plays <- dplyr::filter(gamedata(), teamTricode == input$team)
      seq_len(nrow(team_plays))
    } else {
      seq_len(nrow(gamedata()))
    }
  })

  pbp_data <- shiny::reactive({
    if (input$team != "both") {
      team_plays <- dplyr::filter(gamedata(), teamTricode == input$team)
      cbind(team_plays, pbp_num = seq_len(nrow(team_plays)))
    } else {
      cbind(gamedata(), pbp_num = pbp_num())
    }
  })

  description_pbp <- shiny::reactive({
    dplyr::filter(pbp_data(), as.numeric(pbp_num()) == input$play)
  })

  output$description_pbp <- shiny::renderTable({
    description_pbp_data <- description_pbp()
    description_pbp_data <- dplyr::select(description_pbp_data, clock, description, scoreAway, scoreHome)
    colnames(description_pbp_data) <- c("Clock", "Description", "Score Away", "Score Home")
    description_pbp_data
  })


  team_st <- shiny::reactive(team_stats(logs, game_id(), input$team))

  output$team_stats <- shiny::renderTable({
    team_st_data <- team_st()
    colnames(team_st_data) <- c("Points","Field goal %", "3 point %", "Freethrow %", "Offensive rebounds", "Defensive rebounds", "Assists", "Steals", "Blocks", "Turnovers", "Plus/Minus")
    if (input$team != "both"){
      team_st_data
    }
})
  player_box <- shiny::reactive({
    req(input$player)
  })

  output$player_stats <- shiny::renderTable({
    if (!is.null(player_box()) && input$player != "all") {
      player_st <- shiny::reactive(player_stats(gamedata(), game_id(), input$player))
      player_st_data <- player_st()
      colnames(player_st_data) <- c("Points", "Field goal %", "3 point %", "Freethrow %", "Offensive rebounds", "Defensive rebounds", "Assists", "Steals", "Blocks", "Turnovers", "Plus/Minus")
      player_st_data
    }
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
    } else if(is.na(input$player)){
      tags$img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/0/03/National_Basketball_Association_logo.svg/315px-National_Basketball_Association_logo.svg.png?20221026000552",
               alt = "df",
               style = "width: auto; height: 200px;")
    }
  })
}

#'Function that opens the shiny app, this is the only function that is exported and needs to be run to use the app.
#'It takes no arguments and directly opens the app in the browser for you.
#'@export
#'
#'@return the R shiny app

run_NBA_pbp <- function() {
  shiny::shinyApp(ui = ui, server = server)
}

shiny::shinyApp(ui = ui, server = server)



