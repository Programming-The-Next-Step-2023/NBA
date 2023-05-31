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

#' logs Dataset Description
#'
#' The gamelogs for the 2022-23 NBA season
#'
#' @format A data frame
#' @source From the NBA stats website
# @export
"logs"

logs <- play.by.play::logs

game_dates <- play.by.play::game_dates


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
  src <- paste0("www/", team_code,".png")
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
  data <- dplyr::select(data, pctFGTeam, pctFG3Team, pctFTTeam, orebTeam, drebTeam, astTeam, stlTeam, blkTeam, tovTeam, plusminusTeam)
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
  
  #pbpdat$x <- ifelse(pbpdat$x < 50, pbpdat$x + 1.5, pbpdat$x - 1.5)
  
  #pbpdat$y <- pbpdat$y/2
  
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
