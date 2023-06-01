---
title: "NBA_play-by-play_Vignette"
output: rmarkdown::html_vignette 
runtime: shiny
vignette: >
  %\VignetteIndexEntry{NBA_play-by-play_Vignette}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




```r
library(play.by.play)
```

NBA play-by-play uses the NBA live play-by-play data from [NBA stats](https://www.nba.com/stats) to visualize the NBA games of the 2022-23 NBA season. 

Use the app by using the run run_NBA_pbp() function.


```r

run_NBA_pbp()

```
For this, the app has two main functionalities.

First, visualizing an overview of the whole game. 
It is possible to filter the shots taken in the game by team, player, peroid and minutes left in the period.
Furthermore, the plot distinguishes between made shots (green) and missed shots(red).

## Full App

![Full app, showing the whole game](img/full_app.png)

## Team and Player selected

![Showing only the shots for this player of the team, and their picture.](img/team_player.png)

## Period and minute left in period selected

![Showing only the shot for this specific point in time in the period.](img/period_minute.png)

Second, it is possible to click through the individual plays of the game in chronological order. In this case, A description of the play, the current score of the game and minutes left in the period are displayed in a table. If the current play is a shot attempt, the plot also shows the shot coordinated and shot result.

## Play-by-play

![showing a description and coordinates for a single selected play](img/single_play.png)
## Box Scores

To better be able to interpret the Box Scores, here are the  definitions from [Masterclass](https://www.masterclass.com/articles/how-to-read-a-basketball-box-score).

Points: The total number of points scored by a player or team.

Field goal percentage: The field goal percentage refers to the field goal attempts made by a player or team and gives direct insight into how well a player or team performed in a game. To calculate field goal percentages, divide field goals made by field goals attempted.

3-point field goal percentage: The percentage of three-point field goal attempts made by a player or team. To calculate three-point field goal percentage, divide three-point field goals made by three-point field goals attempted.

Free throw percentage: Free throw percentage refers to the number of free throw attempts made by a player or team. To calculate free throw percentage, divide free throws made by the number of free throws attempted.

Offensive rebounds: The total number of rebounds collected by a player or team while playing offense.
Defensive rebounds: The total number of rebounds collected by a player or team while playing defense.

Assists: The total number of assists made by a player or team. An assist only occurs when a pass leads directly to a teammate's scored basket. If a player is simply the last player to possess the ball before their teammate score, they will not receive an assist.
STL (Steals): STL refers to the total number of steals made by a player or team. A steal occurs when a defensive player takes the ball away from an offensive player by either intercepting a pass or stealing the offensive player's dribble.

Blocks: The total number of blocked field goals made by a defensive player or team. A blocked shot occurs when an offensive player shoots a legitimate field goal attempt and a defensive player tips or deflects the ball. Even if the defensive player's team does not recover the deflected ball, it still counts as a blocked shot. NBA box scores also include a stat called "BLKA" (Blocks Against), which is an offensive player or team’s total number of attempted field goals that get blocked by a defender.

Turnovers: The total number of turnovers made by a player or team. A turnover occurs when an offensive player loses possession of the ball to the defense before the offensive player attempts a shot. Some actions that result in a turnover by an offensive player include: having the ball stolen while dribbling or throwing a bad pass, stepping out of bounds, throwing the ball out of bounds, committing an offensive foul, committing a traveling violation, committing a double-dribble violation, committing a shot clock violation, committing a backcourt violation, and committing a three or five-second violation.


Plus/minus: The total point differential for the time that a specific player is on the court. This statistic measures a player's impact on the game. To calculate +/-, subtract the difference between a player’s team's total points and their opponent's total points when that player is in the game. For example, if a player's team outscored their opponents by eight points when the player is on the court, the player will receive a +8 point differential. If a player's team is outscored by five points when that player is on the court, the player will receive a -5 point differential.



