# Data and Packages -------------------------------------------------------
library(tidyverse)
library(stringi)
library(httr2)

courses <- map(list.files("data-input", pattern = "courses", full.names = T),
               read_rds)

names(courses) <- str_extract(list.files("data-input", pattern = "courses"),
                              pattern = "(\\d+).+(summer|winter)")

courses <- bind_rows(courses, .id = "year_semester")

# Get Lecturers' Gender ---------------------------------------------------

courses <- courses %>% 
  mutate(name = str_replace(lecturer, " et ", " "),
         name = str_replace(name, "PD", "P.D."),
         name = str_extract(name, "[:alpha:]+[^\\.] "),
         name = str_squish(name),
         name = str_to_lower(name),
         name = stri_trans_general(name, "Latin-ASCII"))

first_names <- courses %>% 
  filter(!is.na(name)) %>% 
  pull(name) %>% 
  unique()

api_calls <- paste0("https://api.genderize.io?name=", first_names, "&country_id=CZ")

genderize <- map(api_calls,
                 ~{request(.) %>% 
                     req_perform()})

genderized_names <- genderize %>% 
  map(resp_body_json) %>% 
  map(unlist) %>% 
  map_df(~tibble(name = .["name"],
              gender = .["gender"],
              probability = .["probability"],
              count = .["count"])) %>% 
  bind_rows() %>%
  mutate(across(c(gender, probability, count),
                ~if_else(count == "0", NA_character_, .)))

# Some names haven't appeared in Czech data, need a broader search.

foreign_names <- genderized_names %>% 
  filter(is.na(gender)) %>% 
  pull(name)

api_calls <- paste0("https://api.genderize.io?name=", foreign_names)

genderize <- map(api_calls,
                 ~{request(.) %>% 
                     req_perform()})

genderized_foreign <- genderize %>% 
  map(resp_body_json) %>% 
  map(unlist) %>% 
  map_df(~tibble(name = .["name"],
                 gender = .["gender"],
                 probability = .["probability"],
                 count = .["count"])) %>% 
  bind_rows() %>%
  mutate(across(c(gender, probability, count),
                ~if_else(count == "0", NA_character_, .)))

# Merging data frames -----------------------------------------------------

genderized_names <- genderized_names %>% 
  filter(!is.na(gender)) %>% 
  bind_rows(genderized_foreign) %>% 
  distinct()

courses <- left_join(courses, genderized_names)

# Data export -------------------------------------------------------------

write_rds(courses, "data-input/courses-genderized.rds")
