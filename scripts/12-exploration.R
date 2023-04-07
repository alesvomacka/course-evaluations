# Data and Packages -------------------------------------------------------
library(tidyverse)
library(brms)
library(marginaleffects)
library(tidybayes)
library(avom)
library(patchwork)
library(scales)

courses <- read_rds("data-output/course_evals.rds")

theme_set(theme_avom(text = element_text(family = "Fira Sans"),
                     axis.title = element_blank()))

primary_color <- "#83a598"

update_geom_defaults("point", list(color = primary_color))
update_geom_defaults("col", list(fill = primary_color))
update_geom_defaults("line", list(color = primary_color))
update_geom_defaults("smooth", list(color = primary_color))

# Removing Missing Values -------------------------------------------------
courses <- filter(courses, !is.na(lecturer_rating))

# Exploration -------------------------------------------------------------
courses |> 
  select(title, gender, semester) |> 
  pivot_longer(cols = everything()) |> 
  count(name, value) |> 
  mutate(value = if_else(is.na(value),
                         true = "Unknown",
                         false = value),
         value = as_factor(value),
         value = fct_relevel(value, "Unknown", after = Inf),
         value = fct_relabel(value, str_to_title),
         name = fct_relabel(name, str_to_title)) |> 
  mutate(freq = n / sum(n),
         freq_label = percent(freq, accuracy = 1),
         .by = name) |> 
  ggplot(aes(x = value,
             y = freq,
             label = freq_label)) +
  facet_wrap(~name, ncol = 2,
             scales = "free_x") +
  geom_col() +
  geom_text(vjust = -1) +
  scale_x_discrete(labels = ~str_wrap(.,10)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(NA, 0.62))

