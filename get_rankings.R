# load libraries
library(tidyverse) # for data manipulation
library(rvest) # for web scraping

# url for FIFA teams

url <- "http://www.fifa.com/fifa-world-ranking/ranking-table/men/index.html"

# xpath for the rankings table
# //*[@id="profile"]/div[2]/table

rankings <- url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="profile"]/div[2]/table') %>%
  html_table(fill = TRUE)

# remove columns with no value  
rankings <- rankings[,c(2,3,5,6,7,8,10:17,20)]

# rename columns
colnames(rankings) <- c("rank","team","abb","points","prev_pnts","points_change","avg18","wgt_avg18","avg17","wgt_avg17","avg16","wgt_avg16","avg15","wgt_avg15","conf")

# create a tibble
rankings <- rankings %>% as.tibble()

# create a csv
write_csv(rankings,"fifa_team_rankings.csv")
