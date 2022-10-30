# Data and packages -------------------------------------------------------
library(tidyverse)

courses <- read_rds("data-input/courses-genderized.rds")


# Cleaning ----------------------------------------------------------------

courses <- courses %>% 
  mutate(across(.cols = c(positive, negative),
                .fns  = ~map(., ~as.character(unlist(.x)))),
         across(.cols = c(positive, negative),
                .fns  = ~map(., ~ifelse(length(.x) == 0,
                                        yes = NA,
                                        no  = as.character(list(.x))))),
         title = case_when(str_detect(lecturer, "prof.") ~ "prof.",
                           str_detect(lecturer, "doc.")  ~ "doc.",
                           str_detect(lecturer, "PhDr.") ~ "Ph.D.",
                           str_detect(lecturer, "Ph.D.") ~ "Ph.D.",
                           str_detect(lecturer, "MUDr.") ~ "Ph.D.",
                           str_detect(lecturer, "PaedDr") ~ "Ph.D.",
                           str_detect(lecturer, "Dr.")   ~ "Ph.D.",
                           str_detect(lecturer, "Mgr.")  ~ "Mgr.",
                           str_detect(lecturer, "M.Sc.") ~ "Mgr.",
                           str_detect(lecturer, "Mag.") ~ "Mgr.",
                           str_detect(lecturer, "M.A.") ~ "Mgr.",
                           str_detect(lecturer, "Ing.") ~ "Ing.",
                           str_detect(lecturer, "Bc.")  ~ "Bc.",
                           str_detect(lecturer, "BA")   ~ "Bc.",
                           TRUE ~ NA_character_),
         lecturer_rating = if_else(is.na(total_val),
                                   true = NA_real_,
                                   false = lecturer_rating)) %>% 
  relocate(lecturer, title, gender, lecturer_rating, course, department, semester) %>%
  select(-c(year_semester, count, name, probability)) %>%
  relocate(enrolled, total_responses, .after = semester) %>% 
  relocate(url, .after = positive)

# Data export -------------------------------------------------------------

write_rds(courses, "data-output/course_evals.rds")
