## rank based predictor

# load libs
library(nnet)
library(tidyverse)

# rank diff for home team and get a regulation result to model on draws
results <- results %>%
  mutate(home_rank_diff = home_rank-away_rank,
         home_result_reg = case_when(
           home_score > away_score ~ 'W',
           away_score > home_score ~ 'L',
           home_score == away_score ~ 'D')
  )

# Prepare Training and Test Data
training <- results %>% filter(edition %in% c("germany2006","koreajapan2002","france1998"))
test <- results %>% filter(edition == "southafrica2010")

# multinominal regression
multinomModel <- multinom(home_result_reg ~ home_rank_diff, data=training) # multinom Model
summary (multinomModel) # model summary

# predictions using random draw based on probabilities rather than always choosing highest probabilty
predicted_scores <- predict (multinomModel, test, "probs")
predicted_scores <- predicted_scores %>% 
  as.tibble() %>%
  mutate(prob_w = 1-W) %>%
  mutate(chance = runif(n = nrow(predicted_scores))) %>%
  mutate(pick = case_when(
    chance < D ~ 'D',
    chance > prob_w ~ 'W',
    TRUE ~ 'L'
  ))

# predictions to actual results matric
table(predicted_scores$pick, test$home_result_reg)

# accuracy (high 50% - high 60% correct predictions)
mean(as.character(predicted_scores$pick) != as.character(test$home_result_reg))

## subset the first round - where a draw is possible
test_fr <- cbind(test,predicted_scores$pick)
test_fr <- test_fr %>% 
  filter(str_detect(round,"^Group")) %>%
  rename(pick = `predicted_scores$pick`)

# binomial as well to predict on later rounds where a draw is not possible

# Prepare Training and Test Data
training <- results %>% 
  filter(edition %in% c("germany2006","koreajapan2002","france1998")) %>%
  filter(home_result != 'D') %>%
  mutate(home_result_bin = if_else(home_result == 'W',1,0))

test <- results %>% 
  filter(edition == "southafrica2010") %>%
  filter(home_result != 'D') %>%
  mutate(home_result_bin = if_else(home_result == 'W',1,0))

#Build logistic model based on seed difference
rank.model <- glm(home_result_bin ~ home_rank_diff, data = training, family="binomial")
summary(rank.model)

# predict
rank.prediction <- predict(rank.model, test, type="response")
rank.bin <- if_else(rank.prediction > .64,1,0)  # splitting on 65% prob+ == winner since it had best results 

# table predictions
table(rank.bin, test$home_result_bin)

# make tibble with accuracy column
predictions <- tibble(
  preds = rank.bin,
  results = test$home_result_bin,
  correct = if_else(preds == results,1,0)
)

# get % of correct predictions
sum(predictions$correct)/nrow(predictions)

# subset later rounds where a draw is not possible
test_lr <- cbind(test,rank.bin)
test_lr <- test_lr %>% filter(!str_detect(round,"^Group"))

## get columns ready for stacking
test_lr$home_result_bin <- NULL
test_lr$pick <- if_else(test_lr$rank.bin == 1,'W','L')
test_lr$rank.bin <- NULL

# stack first rounds and later rounds
test_preds <- bind_rows(test_fr,test_lr)

# select a few columns for easier reading
test_preds <- test_preds %>% 
  select(match_no, home_rank, home_team, home_score, away_score, away_team, away_rank, home_result, pick)

## check accuracy
sum(if_else(test_preds$home_result == test_preds$pick,1,0))/nrow(test_preds)

## create a file with these baseline preds
write_csv(test_preds, "baseline_preds.csv")
