library(tidyverse)
library(rvest)



# data <- get_data(url)

get_data_from_one_page <- function(url = "https://www.teamrankings.com/nba/player-stat/rebounds-offensive"){
 page <- read_html(url)
 table <- (page |> html_table())[[1]]
 table <- table[,2:5] 
  title <- page %>% 
  html_element('title') %>% 
  html_text() %>%
  sub(pattern = "NBA Basketball Player Stats - ",
      replacement = "") %>%
    gsub(pattern = " " ,replacement = "_")
 rename_with(table, ~paste0(title), "Value")
 } 
# df <- get_data_from_one_page()

             
urls <- c(
  "https://www.teamrankings.com/nba/player-stat/rebounds-offensive",
  "https://www.teamrankings.com/nba/player-stat/points",
  "https://www.teamrankings.com/nba/player-stat/assists",
  "https://www.teamrankings.com/nba/player-stat/rebounds-defensive",
  "https://www.teamrankings.com/nba/player-stat/field-goal-percentage",
  "https://www.teamrankings.com/nba/player-stat/three-point-field-goals-made",
  "https://www.teamrankings.com/nba/player-stat/free-throws-made",
  "https://www.teamrankings.com/nba/player-stat/turnovers",
  "https://www.teamrankings.com/nba/player-stat/steals",
  "https://www.teamrankings.com/nba/player-stat/fouls-personal",
  "https://www.teamrankings.com/nba/player-stat/rebounds",
  "https://www.teamrankings.com/nba/player-stat/field-goals-made",
  "https://www.teamrankings.com/nba/player-stat/free-throw-percentage",
  "https://www.teamrankings.com/nba/player-stat/blocks",
  "https://www.teamrankings.com/nba/player-stat/three-point-field-goals-attempted",
  "https://www.teamrankings.com/nba/player-stat/games-played",
  "https://www.teamrankings.com/nba/player-stat/minutes-played",
  "https://www.teamrankings.com/nba/player-stat/fouls-technical",
  "https://www.teamrankings.com/nba/player-stat/field-goals-attempted",
  "https://www.teamrankings.com/nba/player-stat/two-point-field-goal-percentage",
  "https://www.teamrankings.com/nba/player-stat/game-score"
)

df <- get_data_from_one_page(urls[1])

for(url in urls[2:length(urls)]){
  df2 <- get_data_from_one_page(url)
  df <- full_join(df2,df)
}

df <- df %>% 
  mutate(Field_Goal_Percentage = as.numeric(sub("%", "", Field_Goal_Percentage)),
         Two_Point_Field_Goal_Percentage = as.numeric(sub("%", "", Two_Point_Field_Goal_Percentage))
         )

write.csv(df, 'Eco 365 final project/data.csv')

