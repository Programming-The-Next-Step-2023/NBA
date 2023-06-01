# NBA
NBA play-by-play shiny app that gets its information from the NBA stats page and can display the shot location, outcome, and player name per game in the order of the plays. This enables users to have a more interactive and visually pleasing way to get an overview of the game.
The app allows you to look at all the games of the 2022-23 regular season and visualize the whole game, sorted by team, player, period or minute. Additionally, the app can display the Box Scores for the  game for a selected team or player.
The play-by-play functionality allows you to go through the game step by step, showing you the picture of the involved player, and the description of the action as well as the score of the game at this point in time. If a team is selected, the play-by-play only shows plays that where initiate by that team. 

To install the app, you need R and R studio as well as the devtools package. If you have installed that, run devtools::install_github("Programming-The-Next-Step-2023/NBA/play.by.play") in your console. 

After the package is installed, you can use the run_NBA_pbp() function to launch the app. 
