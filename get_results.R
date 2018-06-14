# load libs
library(tidyverse)
library(rvest)

# get the section of the url that we need for as many cups as we want -- here I use the last five
wc <- c("brazil2014","southafrica2010","germany2006","koreajapan2002","france1998")

# function to get all columns
get_results <- function(wc) {
url <- paste0("https://www.fifa.com/worldcup/archive/",wc,"/matches/index.html")

# datetimes
all_datetimes <- url %>%
  read_html() %>%
  html_nodes(".mu-i-datetime") %>%
  html_text()

# dates
all_dates <- url %>%
  read_html() %>%
  html_nodes(".mu-i-date") %>%
  html_text()

# match number
all_match_numbers <- url %>%
  read_html() %>%
  html_nodes(".mu-i-matchnum") %>%
  html_text()

# round
all_rounds <- url %>%
  read_html() %>%
  html_nodes(".mu-i-group") %>%
  html_text()

# long and short notes for matches that end aet or with pens
all_tiebreakers <- url %>%
  read_html() %>%
  html_nodes(".text-reasonwin") %>%
  html_text()

# scores as text
all_scores <- url %>%
  read_html() %>%
  html_nodes(".s-scoreText") %>%
  html_text()

# teams
all_teams <- url %>%
  read_html() %>%
  html_nodes(".t-nText ") %>%
  html_text()

# team codes
all_codes <- url %>%
  read_html() %>%
  html_nodes(".t-nTri") %>%
  html_text()

# indices to split vectors with odd/even pattern
home_index <- seq(1,length(all_teams)-1,2)
away_index <- seq(2,length(all_teams),2)

# split all_teams to home/away
home_teams <- all_teams[home_index]
away_teams <- all_teams[away_index]

# split all_codes to home/away
home_codes <- all_codes[home_index]
away_codes <- all_codes[away_index]

# split notes for games that end after 90' into long and short
tiebreak_long <- all_tiebreakers[home_index]
tiebreak_short <- all_tiebreakers[away_index]

# create the tibble
tibble(
  edition = wc,
  datetime = all_datetimes,
  date = all_dates,
  match_no = all_match_numbers,
  round = all_rounds,
  home_team = home_teams,
  home_code = home_codes,
  # get number before the hyphen - start of character string - as integer: home_score
  home_score = as.integer(str_extract(all_scores,"^[:digit:]")),
  score = all_scores,
  # get number after the hyphen - end of character string - as integer: away_score
  away_score = as.integer(str_extract(all_scores,"[:digit:]$")),
  away_team = away_teams,
  away_code = away_codes,
  tiebreak_long = tiebreak_long,
  tiebreak_short = tiebreak_short
)
}
# map over get_results function to get results for all cups included in wc vector
results <- map_df(wc, get_results)

# a few duplicate rows so remove those here
results <- distinct(results)

# use score to get win/lose/draw columns
results <- results %>%
  mutate(winner = case_when(
    home_score > away_score ~ home_team,
    away_score > home_score ~ away_team,
    home_score == away_score ~ str_extract(tiebreak_long, "[:print:]+(?= win?)")
  ),
  home_result = case_when(
    home_score > away_score ~ 'W',
    away_score > home_score ~ 'L',
    home_score == away_score & tiebreak_long == " " ~ 'D',
    home_score == away_score & str_extract(tiebreak_long, "[:print:]+(?= win?)") == home_team ~ 'W',
    home_score == away_score & str_extract(tiebreak_long, "[:print:]+(?= win?)") != home_team ~ 'L'
  ),
  away_result = case_when(
    away_score > home_score ~ 'W',
    home_score > away_score ~ 'L',
    away_score == home_score & tiebreak_long == " " ~ 'D',
    away_score == home_score & str_extract(tiebreak_long, "[:print:]+(?= win?)") == away_team ~ 'W',
    away_score == home_score & str_extract(tiebreak_long, "[:print:]+(?= win?)") != away_team ~ 'L'
  )
  ## add in code to add a tag for expected result or upset (ask Joe about when a draw is an upset)
  )


## ratings which have to be gathered one by one because they are not stored uniformily in wikitables

# Brazil 2014:

url <- "https://en.wikipedia.org/wiki/2014_FIFA_World_Cup_seeding"

ratings <- url %>%
read_html() %>%
html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
html_table(fill = TRUE) %>%
as.tibble(
)

ratings <- ratings %>%
  mutate(Team = str_extract(Team, '[^\\(]+'), Team = str_trim(Team, side = "right"), edition = "brazil2014") %>%
  rename(team = Team, ranking = `FIFA Ranking\nOctober 2013`) %>%
  mutate(team = case_when(
    team == "United States" ~ "USA",
    team == "Iran" ~ "IR Iran",
    team == "South Korea" ~ "Korea Republic",
    team == "Ivory Coast" ~ "Côte d'Ivoire",
    TRUE ~ as.character(team)
  ))

ratings14 <- ratings

# South Africa 2010:

url <- "https://en.wikipedia.org/wiki/2010_FIFA_World_Cup_seeding"

ratings <- url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table(fill = TRUE) %>%
  as.tibble(
  )

ratings <- ratings %>%
  mutate(Association = str_extract(Association, '[^\\(]+'), Association = str_trim(Association, side = "right"), edition = "southafrica2010") %>%
  rename(team = Association, ranking = `FIFA Ranking\nOctober 2009`) %>%
  mutate(team = case_when(
    team == "United States" ~ "USA",
    team == "South Korea" ~ "Korea Republic",
    team == "North Korea" ~ "Korea DPR",
    team == "Ivory Coast" ~ "Côte d'Ivoire",
    TRUE ~ as.character(team)
  ))

ratings10 <- ratings

# Germany 2006:

url <- "https://en.wikipedia.org/wiki/2006_FIFA_World_Cup_seeding"

ratings <- url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  html_table(fill = TRUE)

ratings <- ratings[,c(2,12)]

colnames(ratings) <- c('team','ranking')

ratings <- ratings %>%
  slice(3:34) %>%
  mutate(edition = "germany2006", ranking = as.integer(ranking)) %>%
  mutate(team = case_when(
    team == "United States" ~ "USA",
    team == "Iran" ~ "IR Iran",
    team == "South Korea" ~ "Korea Republic",
    team == "Ivory Coast" ~ "Côte d'Ivoire",
    TRUE ~ as.character(team)
  ))

## add extra row for Iran name mismatch
rating_extra_iran_row <- tribble(
  ~team, ~ranking, ~edition,
  "Iran",   19,     "germany2006" 
)

ratings06 <- ratings

# Korea/Japan 2002:

url <- 'https://en.wikipedia.org/wiki/2002_FIFA_World_Cup_seeding'

ratings <- url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  html_table(fill = TRUE)

ratings <- ratings[,c(2,12)]

colnames(ratings) <- c('team','ranking')

## results for 2002 have Iran listed as Iran and IR Iran
ratings <- ratings %>%
  slice(3:34) %>%
  mutate(edition = "koreajapan2002", ranking = as.integer(ranking)) %>%
  mutate(team = case_when(
    team == "United States" ~ "USA",
    team == "Iran" ~ "IR Iran",
    team == "South Korea" ~ "Korea Republic",
    team == "Ivory Coast" ~ "Côte d'Ivoire",
    TRUE ~ as.character(team)
  ))

ratings02 <- ratings

# France 1998:

url <- 'https://en.wikipedia.org/wiki/1998_FIFA_World_Cup_seeding'

ratings <- url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  html_table(fill = TRUE)

ratings <- ratings[,c(2,12)]

colnames(ratings) <- c('team','ranking')

ratings <- ratings %>%
  slice(3:34) %>%
  mutate(edition = "france1998", ranking = as.integer(ranking)) %>%
  mutate(team = case_when(
    team == "United States" ~ "USA",
    team == "South Korea" ~ "Korea Republic",
    team == "Ivory Coast" ~ "Côte d'Ivoire",
    TRUE ~ as.character(team)
  ))

ratings98 <- ratings

# USA 1994:

# get these later - need to find a better table

# url <- 'https://en.wikipedia.org/wiki/1994_FIFA_World_Cup'
# 
# ratings <- url %>%
#   read_html() %>%
#   html_node(xpath = '//*[@id="mw-content-text"]/div/table[3]') %>%
#   html_table(fill = TRUE)
# 
# ratings94 <- ratings

## bind all ratings tables

ratings <- bind_rows(ratings14,ratings10,ratings06,ratings02,ratings98,rating_extra_iran_row)

## get all results one by one and bind_rows

results <- results %>%
  left_join(ratings, by = c("home_team" = "team", "edition" = "edition")) %>%
  rename(home_rank = ranking) %>%
  left_join(ratings, by = c("away_team" = "team", "edition" = "edition")) %>%
  rename(away_rank = ranking)


## expected or upset
# when is an upset and upset -- when is a draw an upset (is it ever?)

results <- results %>%
  mutate(result_type = case_when(
    home_result == 'W' & home_rank < away_rank ~ "expected",
    home_result == 'D' & home_rank < away_rank ~ "expected",
    home_result == 'W' & home_rank > away_rank ~ "upset",
    home_result == 'D' & home_rank > away_rank ~ "upset",
    home_result == 'L' & home_rank < away_rank ~ "upset",
    home_result == 'L' & home_rank > away_rank ~ "expected"
  ),
  rank_diff = abs(home_rank-away_rank),
  score_diff = abs(home_score-away_score)
  )

write_csv(results,"historical_results.csv")

### read in results after they have been created ###
# results <- read_csv("historical_results.csv")

upsets_tally <- results %>%
  mutate(year = str_extract(edition,'[:digit:]+')) %>%
  group_by(edition, year, round) %>%
  summarize(expected = sum(if_else(result_type == "expected",1,0)), 
            upsets = sum(if_else(result_type == "upset",1,0)),
            draws = sum(if_else(home_result == "D",1,0))) %>%
  arrange(desc(year)) %>%
  ungroup()

write_csv(upsets_tally,"upsets_tally_table.csv")
