# eda  (compare age, caps, clubs)

# libs
library(tidyverse) # for data manipulation
library(lubridate) # to work with the birthdates
# library(patchwork)  # devtools::install_github("thomasp85/patchwork") # object ‘ggplot_add’ is not exported by 'namespace:ggplot2'
# library(gridExtra) # install.packages("gridExtra")
library(plotly) # install.packages("plotly")
library(knitr) # htmnl tables

# load data
all_squads <- read_csv("squads.csv")

## below are plots used in blog posts

## at the end, are drafts

## try boxplot with jitter and mean  (this could actually be the best way)

# ------------------------------------------------- median and average age over all time
ggplot(all_squads, aes(x=Year, y=age)) +
  geom_jitter(alpha=0.2, color="#171714") +
  stat_summary(fun.y=mean, geom="line", size=1, color="#D30208") +
  stat_summary(fun.y=median, geom="line", size=1, color="#E5C685") +
  # note on mean line
  annotate("rect", xmin = 1928, xmax = 1932, ymin = 41, ymax = 43,
           color = "#D30208", fill = "#D30208") +
  annotate("text", x = 1934, y = 42, label = "average age", color="#171714", hjust = 0, size = 4) +
  # note on median line
  annotate("rect", xmin = 1928, xmax = 1932, ymin = 37, ymax = 39,
           color = "#E5C685", fill = "#E5C685") +
  annotate("text", x = 1934, y = 38, label = "median age", color="#171714", hjust = 0, size = 4) +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "#171714", size = 0.25), 
        legend.position="none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Year", y = "Age", title = "Age of World Cup Players")

## --------------------------------------------------- players over some age
all_squads %>%
  filter(age > 34) %>%
  group_by(Year) %>%
  summarize(`Players 35 and older` = n_distinct(Player)) %>%
  ungroup() %>%
  arrange(desc(Year)) %>%
  kable()

##-------------------------------------------------- age by team (2018)

today <- today()

# elapsed.time <- wc18_age$dob %--% today  ## the `%--%` causing issues

elapsed.time <- difftime(today, wc18_age$dob, units="days")/365.25

wc18_adj_age <- all_squads %>%
  filter(Year == 2018) %>%
  mutate(Country = fct_reorder(Country, desc(Country)), Pos = str_extract(Pos,"[:alpha:]+"), adj_age = as.duration(elapsed.time) / dyears(1))

wc18_adj_age_plot <- ggplot(wc18_adj_age, aes(Country, adj_age)) +
  geom_point(color = "#015386") + 
  stat_summary(fun.y = min, colour = "#D30208", geom = "point", size = 3, shape=21, show.legend = TRUE) +
  stat_summary(fun.y = max, colour = "#E5C685", geom = "point", size = 3, shape=21, show.legend = TRUE) +
  stat_summary(fun.y = mean, colour = "#D30208", geom = "point", size = 3, shape=23, show.legend = TRUE) +
  stat_summary(fun.y = median, colour = "#E5C685", geom = "point", size = 3, shape=23, show.legend = TRUE) +
  coord_flip() +
  theme(text = element_text(color = "#171714"),
        panel.background = element_blank(), 
        panel.grid.major.x = element_line(color = "#171714", size = 0.15), 
        legend.position="none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Ages for World Cup 2018 Teams")

wc18_adj_age_plot <- ggplotly(wc18_adj_age_plot)

text_vec <- paste0("Country: ",wc18_adj_age$Country,"<br />Name: ",wc18_adj_age$Player,"<br />Position: ",wc18_adj_age$Pos,"<br />Age: ",round(wc18_adj_age$adj_age,2))

wc18_adj_age_plot$x$data[1][[1]][[3]] <- text_vec

wc18_adj_age_plot

# ------------------------------------ same but by position

wc18_adj_age <- wc18_adj_age %>%
  mutate(Pos = fct_relevel(Pos, "GK","DF","MF","FW"))

wc18_adj_age_plot <- ggplot(wc18_adj_age, aes(Pos, adj_age)) +
  geom_point(color = "#015386") + 
  stat_summary(fun.y = min, colour = "#D30208", geom = "point", size = 3, shape=21, show.legend = TRUE) +
  stat_summary(fun.y = max, colour = "#E5C685", geom = "point", size = 3, shape=21, show.legend = TRUE) +
  stat_summary(fun.y = mean, colour = "#D30208", geom = "point", size = 3, shape=23, show.legend = TRUE) +
  stat_summary(fun.y = median, colour = "#E5C685", geom = "point", size = 3, shape=23, show.legend = TRUE) +
  coord_flip() +
  theme(text = element_text(color = "#171714"),
        panel.background = element_blank(), 
        panel.grid.major.x = element_line(color = "#171714", size = 0.15), 
        legend.position="none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Ages for World Cup 2018 Players by Position")

wc18_adj_age_plot <- ggplotly(wc18_adj_age_plot)

text_vec <- paste0("Country: ",wc18_adj_age$Country,"<br />Name: ",wc18_adj_age$Player,"<br />Position: ",wc18_adj_age$Pos,"<br />Age: ",round(wc18_adj_age$adj_age,2))

wc18_adj_age_plot$x$data[1][[1]][[3]] <- text_vec

wc18_adj_age_plot

## -------------------------------------------------- gladwell test (birth month)
all_squads %>%
  filter(!is.na(month), !is.na(birth_month)) %>%
  mutate(month_abbr = month(birth_month, label = TRUE, abbr = TRUE)) %>%
  group_by(month_abbr, birth_month) %>%
  tally() %>%
  arrange(birth_month) %>%
  ggplot(aes(x = month_abbr, y = n)) +
  geom_point(size = 3) +
  theme(text = element_text(color = "#171714"),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "#171714", size = 0.15), 
        legend.position="none",
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Birth Month", y = "Count of Players born in this Month", title = "WC Players by Birth Month")


## --------------------------- ## DRAFTS ## ------------------------------- ##

# average age
a1 <- all_squads %>%
  group_by(Year) %>%
  summarize(avg_age = mean(age, na.rm = TRUE), median_age = median(age, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = avg_age)) +
  geom_point() +
  ylim(24,28) +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "light grey", size = 0.25),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none") +
  labs(y = "Average Age")

# median age
a2 <- all_squads %>%
  group_by(Year) %>%
  summarize(avg_age = mean(age, na.rm = TRUE), median_age = median(age, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = median_age)) +
  geom_point() +
  ylim(24,28) +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "light grey", size = 0.25), 
        legend.position="none") + 
  labs(x = "Year", y = "Median Age")

# grid.arrange(a1,a2)

subplot(a1, a2, nrows = 2)

## try with plotly native  (works alright but quite a bit of cleanup required)

# need to remove extra unneeded x axis labels, name the legend text and recolor the second plot dots, bigger dots, x and y lims

av_ag <- all_squads %>%
  group_by(Year) %>%
  summarize(avg_age = mean(age, na.rm = TRUE), median_age = median(age, na.rm = TRUE)) %>%
  ungroup()

a1 <- plot_ly(data = av_ag
        ,type = 'scatter'
        ,mode = 'markers' 
        ,hoverinfo = 'text'
        ,x = ~Year
        ,y = ~avg_age
        ,colors = "#0074B1"
        ,text = ~paste("Year: ", Year
                       ,'\n Average Age: ', round(avg_age,2) )) %>% 
  layout(title = "Average Age of WC Players by Year"
         , xaxis = list(title = "Year")
         , yaxis = list(title = "Average Age"))
 
a2 <- plot_ly(data = av_ag
               ,type = 'scatter'
               ,mode = 'markers' 
               ,hoverinfo = 'text'
               ,x = ~Year
               ,y = ~median_age
               ,colors = "#D30208"
               ,text = ~paste("Year: ", Year
                              ,'\n Median Age: ', round(median_age,2) )) %>% 
   layout(title = "Median Age of WC Players by Year"
          , xaxis = list(title = "Year")
          , yaxis = list(title = "Median Age"))

subplot(a1, a2, nrows = 2)




### ages by team


wc18_age <- all_squads %>%
  filter(Year == 2018) %>%
  mutate(Country = fct_reorder(Country, desc(Country)), Pos = str_extract(Pos,"[:alpha:]+"))

wc18_age_plot <- ggplot(wc18_age, aes(Country, age)) +
  geom_point(color = "#015386") + 
  coord_flip() +
  theme(text = element_text(color = "#171714"),
        panel.background = element_blank(), 
        panel.grid.major.x = element_line(color = "#171714", size = 0.15), 
        legend.position="none",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Ages for World Cup 2018 Teams")

wc18_age_plot <- ggplotly(wc18_age_plot)

text_vec <- paste0("Country: ",wc18_age$Country,"<br />Name: ",wc18_age$Player,"<br />Position: ",wc18_age$Pos,"<br />Age: ",wc18_age$age)

wc18_age_plot$x$data[1][[1]][[3]] <- text_vec

wc18_age_plot





## plot out a jitter (play more later)
p <- plot_ly(y = ~rnorm(50), type = "box", boxpoints = "all", jitter = 0.3,
             pointpos = -1.8)


# average age by position by country for 2018
#!!# change order of countries #!!#   -- make an interactive plot where end user picks position
all_squads %>%
  filter(Year == 2018) %>%
  group_by(Country, Pos) %>%
  summarize(avg_age = mean(age, na.rm = TRUE)) %>%
  arrange(desc(Country)) %>%
  ggplot(aes(x = fct_reorder(Country, desc(Country)), y = avg_age)) +
  geom_point() +
  coord_flip() +
  theme(panel.background = element_blank(), 
        panel.grid.major.x = element_line(color = "light grey", size = 0.25), 
        legend.position="none") + 
  facet_grid(~Pos)

# count clubs
all_squads %>%
  filter(Year == 2018) %>%
  group_by(Club) %>%
  tally() %>%
  arrange(n) %>%
  top_n(11) %>%
  ggplot(aes(x = factor(Club,levels=Club,ordered=TRUE), y = n)) +
  geom_point() +
  scale_y_continuous(breaks=seq(8,17,1)) +
  coord_flip() +
  theme(panel.background = element_blank(), 
        panel.grid.major.x = element_line(color = "light grey", size = 0.25), 
        legend.position="none") + 
  labs(x = "Clubs", y = "Players in WC Squads", title = "WC Player Count by Club (Top 12)")

# average caps (over all time) 
all_squads %>%
  group_by(Year) %>%
  summarize(avg_caps = mean(Caps, na.rm = TRUE), median_caps = median(Caps, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = avg_caps)) +
  geom_point() +
  theme(panel.background = element_blank(), 
        panel.grid.major.x = element_line(color = "light grey", size = 0.25), 
        legend.position="none") + 
  labs(x = "Year", y = "Average Caps")

# median caps (over all time)   ## plot for Twitter ##
all_squads %>%
  group_by(Year) %>%
  summarize(avg_caps = mean(Caps, na.rm = TRUE), median_caps = median(Caps, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = median_caps)) +
  geom_point() +
  theme(panel.background = element_blank(), 
        panel.grid.major.x = element_line(color = "light grey", size = 0.25), 
        legend.position="none") + 
  labs(x = "Year", y = "Median Caps")

## avg caps and median caps 

# average cap
all_squads %>%
  filter(Year == 2018) %>%
  group_by(Country) %>%
  summarize(avg_caps = mean(Caps, na.rm = TRUE), median_caps = median(Caps, na.rm = TRUE)) %>%
  arrange(avg_caps) %>%
  ggplot(aes(x = fct_reorder(Country, avg_caps), y = avg_caps)) +
  geom_point() +
  coord_flip() +
  theme(panel.background = element_blank(), 
        panel.grid.major.x = element_line(color = "light grey", size = 0.25), 
        legend.position="none") + 
  labs(x = "Country", y = "Average Caps")

# median cap
all_squads %>%
  filter(Year == 2018) %>%
  group_by(Country) %>%
  summarize(avg_caps = mean(Caps, na.rm = TRUE), median_caps = median(Caps, na.rm = TRUE)) %>%
  arrange(median_caps) %>%
  ggplot(aes(x = fct_reorder(Country, median_caps), y = median_caps)) +
  geom_point() +
  coord_flip() +
  theme(panel.background = element_blank(), 
        panel.grid.major.x = element_line(color = "light grey", size = 0.25), 
        legend.position="none") + 
  labs(x = "Country", y = "Average Caps")




### ages by team (2018)

wc18_age <- all_squads %>%
  filter(Year == 2018) %>%
  mutate(Country = fct_reorder(Country, desc(Country)), Pos = str_extract(Pos,"[:alpha:]+"))

wc18_age_plot <- ggplot(wc18_age, aes(Country, age)) +
  geom_point(color = "#015386") + 
  coord_flip() +
  theme(text = element_text(color = "#171714"),
        panel.background = element_blank(), 
        panel.grid.major.x = element_line(color = "#171714", size = 0.15), 
        legend.position="none",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Ages for World Cup 2018 Teams")

wc18_age_plot <- ggplotly(wc18_age_plot)

text_vec <- paste0("Country: ",wc18_age$Country,"<br />Name: ",wc18_age$Player,"<br />Position: ",wc18_age$Pos,"<br />Age: ",wc18_age$age)

wc18_age_plot$x$data[1][[1]][[3]] <- text_vec

wc18_age_plot

