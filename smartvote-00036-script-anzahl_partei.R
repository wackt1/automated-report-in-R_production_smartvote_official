# ==============================================================================
# smartvote-00036-script-anzahl_partei.R
#
# Skript zur tabellarischen Visualisierung der smartvote-Teilnahme pro Partei 
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
library(ggpubr)
library(scales)
library(knitr)
library(kableExtra)

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------
config_file = yaml.load_file("_parameter.yml")

PARAM_JSON_URL_CANDIDATES <- config_file$param_JSON_URL$JSON_URL_CANDIDATES_DATA
PARAM_DISTRICT <- config_file$params_wahlkreis$WAHLKREIS

# ------------------------------------------------------------------------------
# MAIN
# ------------------------------------------------------------------------------

# --- read input
smartvotedata_json_candidates <- jsonlite::fromJSON(PARAM_JSON_URL_CANDIDATES)

# --- preprocess
if(PARAM_DISTRICT != "NA") {
    smartvotedata_json_candidates <- filter(smartvotedata_json_candidates, district == PARAM_DISTRICT)
}

# --- analyze
data_subset_party <- select(smartvotedata_json_candidates, "party_short", "elected", "n_answers")
data_subset_party_all <- data_subset_party
data_subset_party_confirmed <- data_subset_party
data_subset_party_elected <- data_subset_party
data_subset_party_elected_confirmed <- data_subset_party

data_subset_party_all %<>%
    group_by(party_short) %>%
    tally()

data_subset_party_confirmed %<>%
    mutate(confirmed = (data_subset_party_confirmed$n_answers == 75)) %>%
    as_tibble(data_subset_party_confirmed) %>%
    filter(confirmed == TRUE) %>%
    group_by(party_short,  confirmed) %>%
    tally()

data_subset_party_elected %<>%
    filter(elected == TRUE) %>%
    group_by(party_short) %>%
    tally()

data_subset_party_elected_confirmed %<>%
    mutate(confirmed = (data_subset_party_elected_confirmed$n_answers == 75)) %>%
    as_tibble(data_subset_party_confirmed) %>%
    filter(confirmed == TRUE, elected == TRUE) %>%
    group_by(party_short, elected, confirmed) %>%
    tally()

data_subset_party_confirmed_perc <-
    merge(data_subset_party_all, data_subset_party_confirmed, by="party_short", all.x = TRUE)

data_subset_party_elected_confirmed_perc <-
    merge(data_subset_party_elected, data_subset_party_elected_confirmed, by="party_short", all.x = TRUE)

data_subset_party_confirmed_perc %<>%
    select(-c(confirmed)) %>%
    mutate(percentage = n.y / n.x * 100) %>%
    mutate(percentage = round(percentage, digits = 0))

data_subset_party_elected_confirmed_perc %<>%
    select(-c(confirmed, elected)) %>%
    mutate(percentage = n.y / n.x * 100) %>%
    mutate(percentage = round(percentage, digits = 0))

data_total <-
    merge(data_subset_party_confirmed_perc, data_subset_party_elected_confirmed_perc, by="party_short", all.x = TRUE)

# --- visualize
vis_data_frame <- setNames(data_total, c("Liste","Anzahl", "Teilnahme", "Prozent", "Anzahl", "Teilnahme", "Prozent"))
vis_data_frame[is.na(vis_data_frame)] <- 0

kable(vis_data_frame, escape = TRUE, booktabs = TRUE, longtable = TRUE) %>%
    kable_styling(font_size = 7.5, latex_options = c("repeat_header")) %>%
    kable_styling(latex_options = "striped") %>%
    add_header_above(c(" ", "Alle Kandidierende" = 3, "Gewählte Kandidierende" = 3)) %>%
    column_spec(1, width = "20em") %>%
    column_spec(4, bold = T) %>%
    column_spec(7, bold = T)

# ==============================================================================
# END
# ==============================================================================

