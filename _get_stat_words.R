library(tidyverse)
library(tidytext)

Bland <- tibble(word=c("fig", "size", "false", "lesson","sec", "mathbb",
                       "result", "people", "cap", "text","cancer", "echo",
                       "function", "label", "instance", "planet", "results",
                       "called", "age", "study", "color", "aes", "callout", "include",
                       "fraction","time", "true", "width", "gestation", "negative",
                       "note", "source", "www", "blue", "height", "list_price",
                       "https", "larger", "lessons", "research", "single", "world",
                       "students", "account", "consent", "knitr", "patient", "report",
                       "sex", "start", "form", "individual"))
Keepers <- tibble(word=c("shuffle", "reject", "samples", "specificity",
                         "categorical", "covariates", "experiment", "control",
                         "sensitivity", "training", "random", "causation", "path", "paths",
                         "significant", "estimate", "odds", "factor", "hypotheses",
                         "population", "testing", "average", "classifier", "precision",
                         "accuracy", "screening", "se", "terms", "covariate",
                         "classifier", "sensitivity", "specificity", "correlation",
                         "simulation", "risk", "residual", "trial",
                         "loss", "deviation", "tests", "tilde", "evidence",
                         "nodes", "edges", "observation", "bayes", "confounding", "shuffling",
                         "shuffle", "simulation", "power", "record", "residual", "fitted",
                         "intervention", "likelihood", "term", "statisticians", "bias",
                         "correlations", "distribution", "intercept", "quantitative", "numeric",
                         "specification", "discernible", "jitter", "alpha", "regression", "relationships",
                         "anova", "bounds", "density", "exogenous", "causality", "fit", "significantly",
                         "variances", "variance", "acyclic", "bootstrapping", "evaluate",
                         "interaction", "predicted", "specimen", "association", "null", "alternative",
                         "experiments", "assignment", "threshold", "bayes", "bayesian", "binomial", "logistic",
                         "collider", "confounder"), keep=1)


Lessons <- c(19:26, 28:38)
Chapter_files <- paste0("Lesson-", Lessons, ".qmd")
Instructor_notes_files <- glue::glue("../Math-300Z/Day-by-day/Lesson-{Lessons}/Teaching-notes-{Lessons}.qmd")
Worksheets <- glue::glue("../Math-300Z/Worksheets/Worksheet-{setdiff(Lessons, 31)}.Rmd")
Takeaways <- glue::glue("../LSTbookblog/posts/Takeaways-{setdiff(Lessons, c(27, 29,30,31,33,37,38))}/index.qmd")

All_docs <- c(Chapter_files, Instructor_notes_files, Worksheets, Takeaways)
Chapters <- All_docs |>
  map_chr(~ read_file(.)) |>
  tibble(text = _)
Chapters$titles <- gsub("\\.[Rq]md$", "", All_docs)

Tokenized <- Chapters |> unnest_tokens(token="words", output="word", input="text")

Simplified <- Tokenized |> anti_join(stop_words) |>
  filter(!grepl("[0-9]", word))

Counts <- Simplified |>
  summarize(n=n(), .by = word) |>
  left_join(Keepers) |>
  filter(n > 35 | keep == 1) |>
  anti_join(Bland) |>
  mutate(stem = SnowballC::wordStem(word)) |>
  mutate(stem_count = sum(n), .by = stem)

long_pattern <- paste0(
  setdiff(unique(Counts$stem), "se"), collapse="|")


Bigrams <- Chapters |> unnest_tokens(token="ngrams", output="word", input="text", n=2)

Pairs <- Bigrams |>
  filter(!grepl("[0-9_]", word)) |>
  mutate(first = gsub(" [^ ]*$", "", word),
         second = gsub("^[^ ]* ", "", word)) |>
  filter(grepl(long_pattern, first), grepl(long_pattern, second))

Phrase_counts <- Pairs |>
  mutate(word = gsub("s$| $|^ |ing$", "", word)) |>
  summarize(n=n(), .by = word)

Phrases <- tibble(
  phrase=c("directed acyclic graph", "alternative hypothesis", "assign treatment",
           "bayes factor", "likelihood ratio", "confidence interval", "causal pathway",
           "correlation coefficient", "standard error", "margin of error", "data frame",
           "null hypothesis", "exogenous node", "fit a model", "train a model",
           "hypothesis test", "significance test", "null hypothesis test", "intermediate node",
           "prediction interval", "confidence band", "prediction band", "sampling variation",
           "sample variance", "model coefficient", "null model", "model specification",
           "loss function", "power", "random assignment", "regression coefficient",
           "response variable", "explanatory variable", "risk factor", "random sampling",
           "sample statistic", "sampling variance", "sampling variation", "screening test", "statistically significant",
           "statistically discernible", "standard deviation", "statistical model",
           "logistic regression"
           )
)




