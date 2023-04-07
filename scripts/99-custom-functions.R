# Scrapes data from course webpage and returns a tidy dataframe
scrape_course <- function(course_url, question_wording = NULL) {
  
  if (is.null(question_wording)) {stop("Provide wording for questions to be extracted.")}
  
  print(paste("Scraping", course_url))
  
  # Navigate to course webpage --------------------------------------------
  client$navigate(course_url)
  Sys.sleep(1.5) # Leave time for the webpage to load
  
  # Scrape Data -----------------------------------------------------------
  ## Get course name and semester
  course_name <- client$findElement("class", "ev--page-title")$getElementText()[[1]]
  semester <- client$findElement("class", "ev--page-subtitle")$getElementText() %>%
    str_remove_all("Výsledky . ")
  
  ## Get lecturer(s) name(s)
  teacher_box <- client$findElements("class", "teacher-boxs") %>% 
    map(~.x$getElementText()) %>% 
    map(~str_remove_all(.x, pattern = "\nodrazující\nvynikající\n")) %>%
    map(~str_split(.x, "\\n")) %>% 
    flatten()
  
  if (length(teacher_box) == 0) {
    df <- tibble(course = course_name,
                 url = course_url,
                 semester = semester)
    
    resp_variables <- c("total_val", "useful_val", "overlap_val", "difficulty_val", "total_resp",
                        "useful_resp", "overlap_resp", "difficulty_resp",
                        "lecturer", "lecturer_rating", "positive", "negative",
                        "total_responses", "enrolled")
    for (i in resp_variables) {
      df[i] <- NA
    }
    
    return(df)
  }
  
  ## Average course score
  avg_course <- client$findElement("class", "course-average-rating")$getElementText()
  avg_course <- str_extract(avg_course[[1]], pattern = "\\d.+$")
  
  ## Total number of enrolled students and number of filled out questionaires
  n_rating <- client$findElements("class", "ta-r") %>%
    map(~.x$getElementText())
  
  ## Questionnaire responses
    rating <- client$findElements("class", "results-question-item") %>% 
      map(~.x$getElementText()) %>%
      map(~str_split(.x, pattern = "\\n")) %>% 
      flatten() %>% 
      map(~tibble(var = .[1],
                  resp = .[2],
                  val  = .[3])) %>%
      bind_rows() %>%
      filter(!str_detect(resp, 'Koment') & !str_detect(var, "Zhodnoťte, prosíme, pedagogické působení vyučujícího.")) %>% 
      mutate(across(c(resp, val),
                    str_extract, "\\d+\\.?\\d*"),
             var = case_when(var == questions_wording[1] ~ "total",
                             var == questions_wording[2] ~ "useful",
                             var == questions_wording[3] ~ "overlap",
                             var == questions_wording[4] ~ "difficulty"))
  
  ## Getting the comments -------------------------------------------------
  # 
  # positive <- client$findElements("class", "blue-hotfix-button")
  # if(length(positive) > 0) {
  #   ## show comments for positives
  #   client$findElement("class", "blue-hotfix-button")$clickElement()
  #   Sys.sleep(0.5)
  # 
  #   ## scrape positive comments
  #   positive <- client$findElements("css", ".ev--dark-text p") %>%
  #     map(~.x$getElementText())
  # 
  #   ## toggle comments for positive - hide them
  #   client$findElement("class", "blue-hotfix-button")$clickElement()
  #   Sys.sleep(0.5)
  # } else {positive = list(NULL)}
  # 
  # negative <- client$findElements("css", ".results-question-item+ .results-question-item .clickable")
  # if(length(negative) > 0) {
  #   ## show comments to negative
  #   client$findElement("css", ".results-question-item+ .results-question-item .clickable")$clickElement()
  #   Sys.sleep(0.5)
  # 
  #   ## scrape negative comments
  #   negative <- client$findElements("css", ".ev--dark-text p") %>%
  #     map(~.x$getElementText())
  #   Sys.sleep(0.5)
  # } else {negative = list(NULL)}
  # 
  # ## Check if positive comments are present. If not, then the negative comments 
  # ## have been accidentaly labeled as positive.
  # questions <- client$findElements("css", ".question-text")
  # questions <- sapply(questions, function(x) x$getElementText())
  # positive_present <- any(str_detect(questions, "Co na předmětu a vyučujícím nejvíce oceňujete?"))
  # 
  # Merging Data ----------------------------------------------------------
  
  ## Cleaning lecturer data
  lecturer_stats <- tibble(course = course_name,
                           url = course_url,
                           lecturer = teacher_box) %>%
    unnest(everything()) %>% 
    mutate(lecturer_rating = as.numeric(str_extract(lecturer, "\\d+\\.?\\d*")),
           lecturer = str_trim(str_remove(lecturer, "\\d+\\.?\\d*%")))
  
  # comments <- tibble(course = course_name,
  #                    negative =  ifelse(is.null(negative), NA, list(flatten(negative))),
  #                    positive =  ifelse(is.null(positive), NA, list(flatten(positive))))
  # 
  # if (!positive_present) {
  #   comments$negative <- comments$positive
  #   comments$positive <- list(NULL)
  #   }
  
  ## Cleaning enrollment stats
  enrolled <- tibble(total_responses = str_extract(n_rating[[1]][[1]], "\\d+"),
                     enrolled = str_extract(n_rating[[2]][[1]], "\\d+"),
                     course = course_name)
  
  ## Putting everything together.
    rating %>% 
      filter(!is.na(var)) %>% # If a specific question didn't have enough responses, results in row NAs
      pivot_wider(names_from = var, values_from = c(val, resp),
                  names_glue = "{var}_{.value}") %>% 
      mutate(course = course_name,
             semester = semester,
             url = course_url) %>% 
      left_join(lecturer_stats, by = c("course", "url")) %>% 
#      left_join(comments, by = "course") %>%
      left_join(enrolled, by = "course")
}
