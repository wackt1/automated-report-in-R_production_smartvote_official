# ==============================================================================
# smartvote-00011-script-durchschnittsalter_alle_kand.R
#
# Skript zur Berechnung des Durschnittalters aller Kandidierenden
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

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------
config_file = yaml.load_file("_parameter.yml")

PARAM_JSON_URL_CANDIDATES <- config_file$param_JSON_URL$JSON_URL_CANDIDATES_DATA
PARAM_DISTRICT <- config_file$params_wahlkreis$WAHLKREIS
YEAR_OF_ELECTION = 2019

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
yob_cand_data <- smartvotedata_json_candidates$year_of_birth

mean_yob_cand <- mean(yob_cand_data)

mean_age_cand <- round(YEAR_OF_ELECTION - mean_yob_cand, digits = 2)

result <- as.numeric(mean_age_cand)

# --- visualize
final_result_00011 <- toString(result)

# ==============================================================================
# END
# ==============================================================================

