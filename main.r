library(tidyverse)
library(rvest)



model <- lm(Game_Score ~ Field_Goals_Attempted + Offensive_Rebounds, df)


pred_score <- function(field_goals,off_rebounds){
  predict(model, data.frame(Field_Goals_Attempted = Field_Goals, Offensive_Rebounds = off_rebounds))
}

summary(model)




