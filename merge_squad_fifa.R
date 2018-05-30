# load libs
library(tidyverse)
library(janitor)

# read in data
all_squads <- read_csv("squads.csv")
fifa <- read_csv("CompleteDataset_FIFA_17_18.csv")

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

