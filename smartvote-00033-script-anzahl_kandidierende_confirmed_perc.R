# ==============================================================================
# smartvote-00033-script-anzahl_kandidierende_confirmed_perc.R
#
# Skript zur Berechnung der prozentualen smartvote-Teilnahme
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
library(scales)
library(yaml)

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
total_candidates <- count(smartvotedata_json_candidates)

confirmed_cand_subset <- subset(smartvotedata_json_candidates, n_answers == 75, drop = TRUE)

number_of_candidates_participating <- count(confirmed_cand_subset)

percentage_participation_candidates <- as.numeric((number_of_candidates_participating / total_candidates))

result <- percent(percentage_participation_candidates, accuracy = 0.01)

# --- visualize
final_result_00033 <- toString(result)

# ==============================================================================
# END
# ==============================================================================

