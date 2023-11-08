# Packages and browser setup ----------------------------------------------

library(tidyverse)
library(RSelenium)

source("scripts/99-custom-functions.R")

driver <- rsDriver(port = 4445L, browser = "firefox")
client <- driver$client

# Getting department list -------------------------------------------------

client$navigate("https://evaluace.ff.cuni.cz/results/evaluation/summer-term-20222023/department")

departments <- client$findElements("css", ".ev--click a")

department_names <- sapply(departments, function(x) x$getElementText())
department_names <- unlist(department_names)

department_urls <- sapply(departments, function(x) x$getElementAttribute("href"))
names(department_urls) <- department_names

# Getting course list -----------------------------------------------------

course_urls <- map(department_urls,
                   ~{client$navigate(.)
                     Sys.sleep(1.5)
                     
                     courses_urls <- client$findElements("css", ".div-table-course-block .div-table-course-gl a")
                     department_ratings <- sapply(courses_urls, function(x) x$getElementAttribute("href"))
                   }
)

# Scrape courses -----------------------------------------------------------

questions_wording <- c("S uvážením všeho, co jste do této chvíle vyplnil/a, zhodnoťte, prosíme, předmět celkově.",
                       "Byl pro Vás předmět přínosný?",
                       "Překrýval se podle Vás obsah výuky v tomto předmětu zbytečně s obsahem výuky v jiném předmětu?",
                       "Jaká byla podle Vás náročnost předmětu v porovnání s ostatními předměty?",
                       "Zhodnoťte, prosíme, pedagogické působení vyučujícího.")

safely_scrape_course <- safely(scrape_course, otherwise = "Results Not Published")

courses <- map(course_urls,
               ~map(., safely_scrape_course, questions_wording))

# Data cleaning -----------------------------------------------------------

courses_complete <- map(courses, 
                        ~map(., ~.$result))
courses_complete <- map(courses_complete, bind_rows)
courses_complete <- bind_rows(courses_complete, .id = "department")

# Data export -------------------------------------------------------------

write_rds(courses_complete, "data-input/courses-2022-2023-summer.rds")
