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
  
  gamedata <- shiny::reactive(game_data(game_id()))
  
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
  
  pbp_num <- shiny::reactive(1:nrow(gamedata()))
  
  pbp_data <- shiny::reactive(cbind(gamedata(), pbp_num()))
  
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
    colnames(team_st_data) <- c("Field goal %", "3 point %", "Freethrow %", "Offensive rebounds", "Defensive rebounds", "Assists", "Steals", "Blocks", "Turnovers", "Plus/Minus")
    if (input$team != "both"){
      team_st_data
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
