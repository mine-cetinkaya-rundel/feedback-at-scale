# load packages ----------------------------------------------------------------

library(tidyverse)
library(learnrhash)
library(tidytext)
library(glue)
library(googlesheets4)
library(here)

# load data --------------------------------------------------------------------

student_submissions <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1xwkw2TQW71lVa0REVvYep-8ODXEoS_kI1Uz024F45Yg/edit?usp=sharing",
  col_names = c("timestamp", "name", "student_id", "hash", "feedback"),
  skip = 1
  )

# set answer key ---------------------------------------------------------------

tutorial_key <- tribble(
  ~question_id, ~correct_answer,
  "q1", "An individual Airbnb listing",
  "q2", c("Right skewed", "Unimodal"),
  "q3", "More than 25% of listings have a nightly rate above £500."
  )

# function: check_answer -------------------------------------------------------
# compare student answer and key
# works for single answers and list of multiple answers, in any order

check_answer <- function(x, y) {
  check_1 <- length(setdiff(x, y)) == 0
  check_2 <- length(setdiff(y, x)) == 0
  check_1 & check_2
}

# function: grade_tutorial -----------------------------------------------------

grade_tutorial <- function(student, key) {

  student_answers <- student %>%
    select(student_id, name, hash) %>%
    learnrhash::extract_questions(hash) %>%
    left_join(key, by = "question_id") %>%
    arrange(student_id, question_id) %>%
    mutate(
      answer = map(answer, str_remove_all, "\\n"),
      answer = map(answer, str_squish),
      correct = map2_lgl(answer, correct_answer, check_answer)
    )

  student_scores <- student_answers %>%
    group_by(student_id) %>%
    summarise(score = sum(correct), .groups = "drop") %>%
    left_join(student, by = "student_id") %>%
    select(student_id, name, score, feedback)

  feedback_summary <- student_scores %>%
    unnest_tokens(bigram, feedback, token = "ngrams", n = 2) %>%
    count(bigram, sort = TRUE) %>%
    filter(
      !is.na(bigram),
      !str_detect(bigram, paste0("^", as.character(get_stopwords()$word %>% glue_collapse(sep = " |^")), " | ", as.character(get_stopwords()$word %>% glue_collapse(sep = "$| ")), "$"))
    )

  return(
    list(
      student_answers = student_answers,
      feedback_summary = feedback_summary,
      student_scores  = student_scores
    )
  )
}

# grade student submissions ----------------------------------------------------

tutorial_marks <- grade_tutorial(
  student = student_submissions,
  key = tutorial_key
)

write_rds(tutorial_marks, here::here("learnr/www/tutorial-marks.rds"))

tutorial_marks$student_scores
tutorial_marks$feedback_summary %>%
  filter(n > 1)
