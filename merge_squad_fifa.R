# load libs
library(tidyverse)
library(janitor)
library(rvest)

# read in data
all_squads <- read_csv("squads.csv")
fifa <- read_csv("CompleteDataset_FIFA_17_18.csv")

# clean column names
fifa <- fifa %>% clean_names

# filter to portugal squad and create position column with no leading number for matching
por_sq <- all_squads %>% filter(Year == 2018, Country == "Portugal")
por_sq <- por_sq %>% mutate(position = str_extract(Pos, "[:alpha:]+"))

# filter to portugal fifa data, make clean column names
por_pros <- fifa %>% filter(Nationality == "Portugal")
por_pros <- por_pros %>% clean_names() ## rewrite the csv

# write these two files to csv
write_csv(por_sq, "portugal_squad.csv")
write_csv(por_pros, "portugal_players.csv")

## In Terminal run csvlink using the following command:
# csvlink portugal_squad.csv portugal_players.csv --config_file=config.json

# create a config.json file like the one for portugal

# check the merged file
check_merge <- read_csv("port_mrg2.csv")

# match worked well and only missed Jose Fonte



## try a merge with string distance

## PRE_WORK: checking for country consistency

all_fifa_countries <- unique(distinct_fifa$nationality)

all_wc_countries <- unique(current$Country)

country_check <- all_fifa_countries[all_fifa_countries %in% all_wc_countries]

all_wc_countries[!all_wc_countries %in% country_check]

all_fifa_countries[all_fifa_countries == "Korea Republic"]

## South Korea and Korea Republic are the non-matches so change one to match

# NOTE: first do a tranform to get everything in the same lettercase
library(stringdist)
library(tidyverse)
library(janitor)

# read in data
all_squads <- read_csv("squads.csv")

# 2018 squads
current <- all_squads %>% 
  filter(Year == 2018) %>%
  mutate(Country = if_else(Country == "South Korea","Korea Republic",Country))

# all fifa data
fifa <- read_csv("CompleteDataset_FIFA_17_18.csv")

# clean column names
fifa <- fifa %>% clean_names

distinct_fifa <- fifa %>%
  distinct(name, .keep_all = TRUE)

# a few numbers to try the map

s <- 1:736

# rns for debug 
rns <- 616

get_fifa_match <- function(rns) {
## get four string distances
#jw_dist <- stringdist(distinct_fifa$name,current$Player[rns], method = "jw", p=0.25)
#jaccard_dist <- stringdist(distinct_fifa$name,current$Player[rns], method = "jaccard", q = 1)
qgram_dist <- stringdist(distinct_fifa$name,current$Player[rns], method = "qgram", q=2)
lcs_dist <- stringdist(distinct_fifa$name,current$Player[rns], method = "lcs")

## make a table with all results  
match_tbl <- tibble(
  join_id = rns,
  wc_name = current$Player[rns],
  fifa_name = distinct_fifa$name,
  wc_country = current$Country[rns],
  fifa_country = distinct_fifa$nationality,
  wc_club = current$Club[rns],
  fifa_club = distinct_fifa$club,
  #jw_dist = jw_dist,
  #jaccard_dist = jaccard_dist,
  qgram_dist_norm = 1/(max(qgram_dist)-min(qgram_dist))*(qgram_dist-max(qgram_dist))+1,
  lcs_dist_norm = 1/(max(lcs_dist)-min(lcs_dist))*(lcs_dist-max(lcs_dist))+1,
  #avg_dist = (jw_dist+jaccard_dist+qgram_dist_norm+lcs_dist_norm)/4,
  ql_dist = (qgram_dist_norm+lcs_dist_norm)/2
  #qgram_dist_raw = qgram_dist,
  #lcs_dist_raw = lcs_dist
)

# trial for a few cases -- then try for the full join
match_tbl <- match_tbl %>%
  filter(fifa_country == current$Country[rns]) %>%
  mutate(club_match = if_else(fifa_club == current$Club[rns],1,0)) %>%
  arrange(ql_dist) %>%
  filter(row_number() == 1)

tibble(
  join_id = match_tbl$join_id,
  wc_name = match_tbl$wc_name,
  fifa_name = match_tbl$fifa_name,
  wc_country = match_tbl$wc_country,
  fifa_country = match_tbl$fifa_country,
  wc_club = match_tbl$wc_club,
  fifa_club = match_tbl$fifa_club,
  qgram_dist_norm = match_tbl$qgram_dist_norm,
  lcs_dist_norm = match_tbl$lcs_dist_norm,
  ql_dist = match_tbl$ql_dist
)
}

# try function
fm_table <- map_df(s,get_fifa_match)


## from fm_table link back the fifa data

## then combine with wc_data

## get four slices

double_zeroes <- fm_table %>% filter(ql_dist == 0)

l_zero <- fm_table %>% filter(lcs_dist_norm == 0 & ql_dist > 0 & !join_id %in% c(525,184,521,522,513,44,137)) %>% arrange(qgram_dist_norm)

q_zero <- fm_table %>% filter(qgram_dist_norm == 0 & ql_dist > 0 & !join_id %in% c(47,364,646) & lcs_dist_norm > 0.049) %>% arrange(lcs_dist_norm)

no_zeroes <- fm_table %>% filter(lcs_dist_norm > 0 & qgram_dist_norm > 0 & ql_dist < 0.154761 & !join_id %in% c(277,398,313,215,12,300,455,733,20,93,19,321,15,372,73)) %>% arrange(ql_dist)

true_matches <- bind_rows(double_zeroes,l_zero,q_zero,no_zeroes)


stragglers <- fm_table %>% filter(join_id %in% c(163,276,650,158,201,483,722,653,44,52)) %>% arrange(ql_dist)

true_matches <- bind_rows(true_matches,stragglers)

## get the smaller subset and then run the full function again with club match

leftovers <- fm_table %>% filter(!join_id %in% true_matches$join_id) %>% arrange(fifa_country)


## try one last match

current2 <- current %>%
  inner_join(leftovers, by = c("Player" = "wc_name"))

get_fifa_match2 <- function(rns) {
  ## get four string distances
  jw_dist <- stringdist(distinct_fifa$name,current2$Player[rns], method = "jw", p=0.25)
  jaccard_dist <- stringdist(distinct_fifa$name,current2$Player[rns], method = "jaccard", q = 1)
  qgram_dist <- stringdist(distinct_fifa$name,current2$Player[rns], method = "qgram", q=2)
  lcs_dist <- stringdist(distinct_fifa$name,current2$Player[rns], method = "lcs")
  
  ## make a table with all results  
  match_tbl <- tibble(
    join_id = current2$join_id[rns],
    wc_name = current2$Player[rns],
    fifa_name = distinct_fifa$name,
    wc_country = current2$Country[rns],
    fifa_country = distinct_fifa$nationality,
    wc_club = current2$Club[rns],
    fifa_club = distinct_fifa$club,
    jw_dist = jw_dist,
    jaccard_dist = jaccard_dist,
    qgram_dist_norm = 1/(max(qgram_dist)-min(qgram_dist))*(qgram_dist-max(qgram_dist))+1,
    lcs_dist_norm = 1/(max(lcs_dist)-min(lcs_dist))*(lcs_dist-max(lcs_dist))+1,
    avg_dist = (jw_dist+jaccard_dist+qgram_dist_norm+lcs_dist_norm)/4,
    ql_dist = (qgram_dist_norm+lcs_dist_norm)/2,
    qgram_dist_raw = qgram_dist,
    lcs_dist_raw = lcs_dist
  )
  
  # trial for a few cases -- then try for the full join
  match_tbl <- match_tbl %>%
    filter(fifa_country == current2$Country[rns]) %>%
    mutate(club_match = if_else(fifa_club == current2$Club[rns],1,0)) %>%
    arrange(-club_match, avg_dist) %>%
    filter(row_number() == 1)
  
  tibble(
    join_id = match_tbl$join_id,
    wc_name = match_tbl$wc_name,
    fifa_name = match_tbl$fifa_name,
    wc_country = match_tbl$wc_country,
    fifa_country = match_tbl$fifa_country,
    wc_club = match_tbl$wc_club,
    fifa_club = match_tbl$fifa_club,
    qgram_dist_norm = match_tbl$qgram_dist_norm,
    lcs_dist_norm = match_tbl$lcs_dist_norm,
    ql_dist = match_tbl$ql_dist
  )
}

t <- 1:91

fm_table2 <- map_df(t,get_fifa_match2)

final_rows <- fm_table2 %>% filter(join_id %in% c(513,733))

true_matches <- bind_rows(true_matches,final_rows)

true_matches <- true_matches %>% distinct(join_id, .keep_all = TRUE)

# write the true matches
# write_csv(true_matches, "fifa_data_matching_table.csv")

## join in the fifa data

current_pd <- current %>%
  mutate(join_id = 1:736) %>%
  left_join(true_matches, by="join_id") %>%
  left_join(distinct_fifa, by=c("fifa_name"="name"))


## get NAs by country
team_nas <- current_pd %>%
  mutate(na_bin = if_else(is.na(overall),1,0)) %>%
  group_by(Country) %>%
  summarize(na_count = sum(na_bin)) %>%
  ungroup() %>%
  arrange(-na_count)

## get off power and def power (later find the ncaa script for this)

# first get mean by groups then deal with GK (just one) -- and imputations in two passes (median and mean-sd with noise)

od_table <- current_pd %>%
  filter(Pos %in% c("3MF","4FW")) %>%
  dplyr::group_by(Country) %>%
  summarise(off_score = mean(overall, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(-off_score)

df_score <- current_pd %>%
  filter(Pos %in% c("3MF","2DF","1GK")) %>%
  dplyr::group_by(Country) %>%
  summarise(def_score = mean(overall, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(-def_score)

ofdf_tbl <- od_table %>%
  inner_join(df_score) %>%
  mutate(Country = if_else(Country == "Iran","IR Iran",Country))

# url for all matches
url <- 'https://www.fifa.com/worldcup/matches/'

#mw-content-text > div > div:nth-child(83) > table > tbody > tr:nth-child(1) > th:nth-child(1) > span > a

#mw-content-text > div > div:nth-child(83) > table > tbody > tr:nth-child(1) > th:nth-child(3) > span > span > a

#mw-content-text > div > div:nth-child(84) > table > tbody > tr:nth-child(1) > th:nth-child(1) > span > a


# <span class="fi-t__nText ">Russia</span>

# all lhs team
all_lhs_teams <- url %>%
  read_html() %>%
  html_nodes(".fi-t__nText") %>%
  html_text()

# get the groups
# <div class="fi__info__group">Group A</div>
groups <- url %>%
  read_html() %>%
  html_nodes(".fi__info__group") %>%
  html_text()
  

lhs_teams <- all_lhs_teams[seq(1,127,2)]
rhs_teams <- all_lhs_teams[seq(2,128,2)]

rankings <- read_csv("fifa_team_rankings.csv")

rnk <- rankings %>% select(rank,team,prev_pnts)

group_matches <- tibble (
  lhs_team = lhs_teams,
  rhs_team = rhs_teams,
  group = groups
)

group_matches <- group_matches %>%
  inner_join(rnk, by=c("lhs_team"="team")) %>%
  rename(lhs_rank = rank, lhs_points = prev_pnts) %>%
  inner_join(rnk, by=c("rhs_team"="team")) %>%
  rename(rhs_rank = rank, rhs_points = prev_pnts) %>%
  mutate(rank_diff = lhs_rank-rhs_rank,
         o_team = if_else(rank_diff<0, lhs_team, rhs_team),
         d_team = if_else(rank_diff<0, rhs_team, lhs_team)) %>%
  select(group,o_team,d_team) %>%
  inner_join(rnk, by=c("o_team"="team")) %>%
  rename(o_rank = rank, o_points = prev_pnts) %>%
  inner_join(rnk, by=c("d_team"="team")) %>%
  rename(d_rank = rank, d_points = prev_pnts) %>%
  mutate(rank_diff = o_rank-d_rank) %>%
  inner_join(ofdf_tbl, by=c("o_team"="Country")) %>%
  select(-def_score) %>%
  rename(o_score = off_score) %>%
  inner_join(ofdf_tbl, by=c("d_team"="Country")) %>%
  select(-off_score) %>%
  rename(d_score = def_score) %>%
  mutate(score_diff = o_score-d_score) %>%
  arrange(score_diff) %>%
  ####
  inner_join(ofdf_tbl, by=c("o_team"="Country")) %>%
  select(-off_score) %>%
  rename(df_score = def_score) %>%
  inner_join(ofdf_tbl, by=c("d_team"="Country")) %>%
  select(-def_score) %>%
  rename(of_score = off_score) %>%
  mutate(upset_option = of_score-df_score,
         team_a = if_else(score_diff < 2 & upset_option > 3, d_team, o_team),
         diff_dist = abs(score_diff-upset_option),
         a_points = if_else(diff_dist < 5,1,3),
         team_b = if_else(score_diff < 2 & upset_option > 3, o_team, d_team),
         b_points = if_else(diff_dist < 5,1,0))

# write group stage points data
# write_csv(group_matches, "group_stage_points_data.csv")
  
top_rows <- tibble(
  group = group_matches$group,
  team = group_matches$team_a,
  points = group_matches$a_points,
  gd = group_matches$diff_dist
)

bottom_rows <- tibble(
  group = group_matches$group,
  team = group_matches$team_b,
  points = group_matches$b_points,
  gd = -group_matches$diff_dist
)

group_points <- bind_rows(top_rows,bottom_rows)
  
group_tables <- group_points %>%
  group_by(group,team) %>%
  summarise(total_points = sum(points), 
            total_gd = sum(gd), 
            goal_difference = round(sum(gd)/10)) %>%
  arrange(group,-total_points, -total_gd) %>%
  ungroup() %>%
  select(-total_gd)

write_csv(group_tables,"group_stage_table.csv")

group_match_results <- group_matches %>%
  arrange(group) %>%
  select(group,team_a,a_points,team_b,b_points)

write_csv(group_match_results,"group_stage_match_results.csv")

## 12 draws  --> 6 draw upsets and 4 win upsets

## next join in the rankings

## set up all matches

## hopefully get all and join rankings

## get the difference to rearrange lower seed on the lhs and higher on rhs

## upset is possible if you limit opportunities

# goals by app = expected off output (in theory, i need to penalize Chicarito since all goals against terrible teams)

# look at chicarito fifa data

chicarito <- distinct_fifa %>% filter(name == "J. Hernández")


## higher seed: diff btw scores for strikers and scores for other team defenders  (FW and MF vs. MF and DF and GK (top overall))

## larger the margin = more quality chances

## lower seed: same diff

## larger the margin = more chance for the upset

## get upsets by round -- find these differences



#trials:
current$Player[317]

current[317,]

## trials for club matching

all_fifa_clubs <- unique(distinct_fifa$club)

all_wc_clubs <- unique(current$Club)

club_check <- all_fifa_clubs[all_fifa_clubs %in% all_wc_clubs]

all_wc_clubs[!all_wc_clubs %in% club_check]




## trials for png images

## URL is 404

library(png) # install.packages("png")


con <- url("https://cdn.sofifa.org/24/18/teams/243.png",
           open='rb')

rawpng <- readBin(con, what='raw', n=50000)

close(con)

png1 <- readPNG(rawpng)