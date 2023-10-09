library(tidyverse)
library(ggthemes)

# Load data
goals <- read_csv("data/events.csv") %>%
  filter(event_type == 1) %>%
  filter(shot_outcome == 1)

goals %>%
  group_by(time, side) %>%
  summarize(goals = n()) %>%
  ungroup() %>%
  rename(minutes = time) %>%
  ggplot() +
  geom_bar(aes(x = minutes,
                 y = goals,
                 fill = factor(side,labels = c("Home","Away"))),
           stat="identity") +
  theme_fivethirtyeight()+
  theme(legend.title = element_blank()) +
  labs(title= "Goals per Minute by Game Time",
       subtitle = "Excess Represent Goals in Stoppage Time")


rem_goals_time <- goals %>%
  group_by(time) %>%
  summarize(goals = n()) %>%
  mutate(cum_goals = cumsum(goals)) %>%
  mutate(rem_goals = max(cum_goals)-cum_goals) %>%
  mutate(rem_time = 90-time) %>%
  filter(rem_time >= 0)

  rem_goals_time %>% 
  ggplot()+
  geom_point(aes(x=time,y=rem_goals),color = "#008FD5")+
  geom_abline(intercept = max(rem_goals_time$rem_goals), 
              slope = -max(rem_goals_time$rem_goals)/90,
              color = "#FF2700")+
    xlab("Game Time")+
    ylab("Remaining Goals") + 
  labs(title = "All Database Goals by Minute",
       subtitle = "Expected Goals by Minute Represented by Red Line")+
  theme_fivethirtyeight()

rem_goals_time %>%
  ggplot()+
  geom_point(aes(x=time,y=cum_goals))
