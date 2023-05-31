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
    shiny::conditionalPanel(
      condition = "input$team != 'both'",
      shiny::tableOutput("team_stats")
    )
  ),
  shiny::fixedRow(
    shiny::column(4,
                  shiny::numericInput("play", "Select the play", NULL),
                  shiny::tableOutput("description_pbp")
    ),
  ),
)
