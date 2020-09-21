# ==============================================================================
# smartvote-00045-script-antwortverteilung_alle_kand_slider-7.R
#
# Skript zur Visualisierung der Antwortverteilung alle Kandidierende - Fragetyp Slider-7
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
PARAM_PARTY_SELECTOR <- config_file$param_party_selector$PARTIES
if(PARAM_PARTY_SELECTOR != "NA") {
    PARAM_PARTY_SELECTOR <- as.character(unlist(strsplit(PARAM_PARTY_SELECTOR, ', ')))
}

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

data_unnested_filtered <- select(data_unnested, "party_short", "questionId", "answer")
data_unnested_filtered %<>%
    filter(questionId == 3388) %>%
    count(answer, party_short) %>%
    group_by(party_short) %>%
    mutate(order_var = answer) %>%
    as_tibble()

if(PARAM_PARTY_SELECTOR != "NA") {
    data_unnested_filtered <- filter(data_unnested_filtered, party_short %in% PARAM_PARTY_SELECTOR)
}

data_unnested_filtered$order_var[data_unnested_filtered$order_var == 0] <- -3
data_unnested_filtered$order_var[data_unnested_filtered$order_var == 17] <- -2
data_unnested_filtered$order_var[data_unnested_filtered$order_var == 33] <- -1
data_unnested_filtered$order_var[data_unnested_filtered$order_var == 50] <- 0
data_unnested_filtered$order_var[data_unnested_filtered$order_var == 67] <- 1
data_unnested_filtered$order_var[data_unnested_filtered$order_var == 83] <- 2
data_unnested_filtered$order_var[data_unnested_filtered$order_var == 100] <- 3

data_unnested_filtered %<>%
    group_by(party_short) %>%
    mutate(order_weight = mean(n*order_var)) %>%
    as_tibble()

smartvotedata_json_questions %<>%
    filter(ID_question == 3388) %>%
    as_tibble()

question_title <- smartvotedata_json_questions$question

question_chart_vis_party <- ggplot(data = data_unnested_filtered, aes(x = answer, y = reorder(party_short, order_weight), fill = n)) + 
    geom_tile(color = "white") +
    scale_fill_distiller(palette = "Blues", trans = "reverse", name = "Anzahl\nKandidierende") +
    scale_x_continuous(breaks = c(0, 100), labels = c("Gar nicht\neinverstanden", "Vollständig\neinverstanden")) +
    xlab(paste0("Antwort auf Frage")) +
    ylab("Partei\n(Aufsteigend nach gewichteter Zustimmung)") +
    ggtitle(wrapper(paste0("Antwortverteilung (alle Kandidierende) zur Frage: ", question_title), width = 50)) +
    theme_classic() +
    theme(plot.title = element_text(size = 12))

# --- visualize
question_chart_vis_party

# ==============================================================================
# END
# ==============================================================================

