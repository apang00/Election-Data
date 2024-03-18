#### Preamble ####
# Author: Yi Fei Pang
# Date: 2024-03-17
# Student ID: 1005182598
# Source: Cohn, Nate. 2016. “We Gave Four Good Pollsters the Same Raw Data. 
#   They Had Four Different Results.” The New York Times, September. 
#   https://www.nytimes.com/interactive/2016/09/20/upshot/the-error-the-polling-world-rarely-talks-about.html.

#### Description ####
# Since the data we will be looking at will be election results between two
# parties, I would be using a logistic regression to display my findings.
# This is because the outcome is binary as there are two main candidate and 
# will have the best results for interpretability.

# Similar to Nate Cohn's New York Time's article, I will be looking at election
# data from the United States. I will be focusing on election data from 2020 
# with Biden and Trump as the binary outcomes and use the results from every
# electoral state.

library(boot)
library(collapse)
library(dataverse)
library(gutenbergr)
library(janitor)
library(knitr)
library(marginaleffects)
library(modelsummary)
library(rstanarm)
library(tidybayes)
library(tidyverse)
library(dplyr)
library(ggplot2)

### Data Cleaning ###

# Selects only the data for year of 2020 and filters out columns that
# are not useful
filtered_data <- X1976_2020_president %>% filter(year == 2020) %>%
  select(year, state, candidate, candidatevotes, totalvotes)

# adds percentage of ballots won
filtered_data$win_perc <- round(filtered_data$candidatevotes / 
                                  filtered_data$totalvotes * 100, digits = 0)

filtered_data <- filtered_data %>%
  filter(filtered_data$candidate == "BIDEN, JOSEPH R. JR" |
           filtered_data$candidate == "TRUMP, DONALD J.")

# replaces Bidens with 0 and Trumps with 1
for (i in 1:nrow(filtered_data)) {
  if (filtered_data$candidate[i] == "BIDEN, JOSEPH R. JR") {
    filtered_data$candidate[i] <- 0
  }
  else if (filtered_data$candidate[i] == "TRUMP, DONALD J.") {
    filtered_data$candidate[i] <- 1
  }
}

# To replicate the New York Time's model, I will create a new column that shows
# the win percent of one party over another.

filtered_data$win_margin <- NA


for (i in seq(1, nrow(filtered_data), by = 2)) {

  value1 <- filtered_data$win_perc[i]
  value2 <- filtered_data$win_perc[i + 1]
  
  win_margin <- max(value1, value2) - min(value1, value2)
  
  if (value1 > value2) {
    filtered_data$win_margin[i] <- win_margin
    filtered_data$win_margin[i + 1] <- NA
  } else {
    filtered_data$win_margin[i] <- NA
    filtered_data$win_margin[i + 1] <- win_margin
  }
}

winners <- subset(filtered_data, !is.na(win_margin))

### Estimation ###
# I estimate that based on the election results, I will see significantly more
# states have votes for Biden over Trump, or more blue bars than red bars
# on the bar graph below regardless of marginal win percent.

# Furthermore, I definitely expect a correlation between the winning margin 
# percent to the results of the state (of course because a +winning margin 
# means the candidate won the state, and a -winnning margin means they lost)

### Graphing ###

bar_graph <- ggplot(winners, aes(x = factor(state), y = win_margin, fill = factor(candidate))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) + 
  labs(x = "State", y = "Win Margin By %", title = "Win Margin Per State") +  
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(fill = "Candidates")

bar_graph


### Analysis ###

# From the bar graph, it seems like in most states, the voter percentage 
# difference lies between 10-30% with some exceptions. For example The District
# Of Columbia where Biden wins by over 80% margin and Wyoming where Trump wins 
# by over 40% margin. There are also a few instances where there are ties such 
# as Arizona and Georgia where the voters who voted for Trump and Biden are 
# equal in percentage, thus not producing a margin. 

# Compared to Nate Cohn's findings, the % differences in margin in my findings
# were much greater. However, this makes sense because that data was gathered 
# in polls held in smaller vicinities compared to ballots across whole states.

### Second Graph ###
winners$candidate <- as.integer(winners$candidate)

for (i in 1:nrow(winners)) {
  if (winners$candidate[i] == 0) {
    winners$win_margin[i] <- winners$win_margin[i] * -1
  }
}

trump_or_biden <- glm(candidate ~ win_perc, 
                      data = winners,
                      family = "binomial")
summary(trump_or_biden1)

trump_or_biden_prediction <- 
  predictions(trump_or_biden) |> as_tibble()

trump_or_biden_prediction

# Panel (a)
trump_or_biden_prediction |>
  ggplot(aes(x = win_perc, y = candidate, color = factor(candidate))) +
  geom_jitter(width = 0.01, height = 0.01, alpha = 0.3) +
  labs(
    x = "Win Percentage",
    y = "Candidate",
    color = "Candidate"
  ) +
  theme_classic() +
  scale_color_brewer(palette = "Set1", direction = -1) +  
  theme(legend.position = "bottom")

# Panel (b)
trump_or_biden_prediction |>
  ggplot(aes(x = win_perc, y = candidate, color = factor(candidate))) +
  stat_ecdf(geom = "point", alpha = 0.75) +
  labs(
    x = "Win Percentage",
    y = "Candidate",
    color = "Candidate"
  ) +
  theme_classic() +
  scale_color_brewer(palette = "Set1", direction = -1) +  
  theme(legend.position = "bottom")