# ==============================================================================
# smartvote-00042-script-antwortverteilung_gewaehlte_kand_standard-4.R
#
# Skript zur Visualisierung der Antwortverteilung gewaehlte Kandidierende - Fragetyp Standard-4
# 
# R-Script - Bachelor Thesis Production - smartvote
#
# Author: wackt1.bfh@gmail.com
# Date: 19.09.2020
# ==============================================================================

# ------------------------------------------------------------------------------
# IMPORT PACKAGES
# ------------------------------------------------------------------------------
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(yaml)
library(viridis)

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------
config_file = yaml.load_file("_parameter.yml")

PARAM_JSON_URL_CANDIDATES <- config_file$param_JSON_URL$JSON_URL_CANDIDATES_DATA
PARAM_JSON_URL_QUESTIONS <- config_file$param_JSON_URL$JSON_URL_QUESTIONS_DATA
PARAM_DISTRICT <- config_file$params_wahlkreis$WAHLKREIS

# ------------------------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------------------------
wrapper <- function(x, ...) 
{
    paste(strwrap(x, ...), collapse = "\n")
}

# ------------------------------------------------------------------------------
# MAIN
# ------------------------------------------------------------------------------

# --- read input
smartvotedata_json_candidates <- jsonlite::fromJSON(PARAM_JSON_URL_CANDIDATES)
smartvotedata_json_questions <- jsonlite::fromJSON(PARAM_JSON_URL_QUESTIONS)

# --- preprocess
if(PARAM_DISTRICT != "NA") {
    smartvotedata_json_candidates <- filter(smartvotedata_json_candidates, district == PARAM_DISTRICT)
}

# --- analyze
data_unnested <- unnest(smartvotedata_json_candidates, answers)

data_unnested_questions <- select(data_unnested, "questionId")
data_unnested_questions %<>%
    count(questionId) %>%
    select(-n) %>%
    as_tibble()

data_unnested_filtered <- select(data_unnested, "party_short", "elected", "questionId", "answer")
data_unnested_filtered %<>%
    filter(questionId == 3392, elected == TRUE) %>%
    count(answer, party_short, elected) %>%
    group_by(party_short) %>%
    mutate(order_var = answer) %>%
    as_tibble()

data_unnested_filtered$order_var[data_unnested_filtered$order_var == 0] <- -2
data_unnested_filtered$order_var[data_unnested_filtered$order_var == 25] <- -1
data_unnested_filtered$order_var[data_unnested_filtered$order_var == 75] <- 1
data_unnested_filtered$order_var[data_unnested_filtered$order_var == 100] <- 2

data_unnested_filtered %<>%
    group_by(party_short) %>%
    mutate(order_weight = mean(n*order_var)) %>%
    as_tibble()

smartvotedata_json_questions %<>%
    filter(ID_question == 3392) %>%
    as_tibble()

data_unnested_filtered$answer[data_unnested_filtered$answer == 25] <- 33.333333333
data_unnested_filtered$answer[data_unnested_filtered$answer == 75] <- 66.666666666

question_title <- smartvotedata_json_questions$question

question_chart_vis_party <- ggplot(data = data_unnested_filtered, aes(x = answer, y = reorder(party_short, order_weight), fill = n)) + 
    geom_tile(color = "white") +
    scale_fill_distiller(palette = "Blues", trans = "reverse", name = "Anzahl\nKandidierende") +
    scale_x_continuous(breaks = c(0, 33.333333333, 66.666666666, 100), labels = c("Nein", "Eher Nein", "Eher Ja", "Ja")) +
    xlab(paste0("Antwort auf Frage")) +
    ylab("Partei\n(Aufsteigend nach gewichteter Zustimmung)") +
    ggtitle(wrapper(paste0("Antwortverteilung (gew�hlte Kandidierende) zur Frage: ", question_title), width = 50)) +
    theme_classic() +
    theme(plot.title = element_text(size = 12))

# --- visualize
question_chart_vis_party

# ==============================================================================
# END
# ==============================================================================

