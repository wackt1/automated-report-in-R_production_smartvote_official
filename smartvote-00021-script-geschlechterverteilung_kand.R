# ==============================================================================
# smartvote-00021-script-geschlechterverteilung_kand.R
# 
# R-Script - Bachelor Thesis - smartvote
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
library(ggpubr)
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
elected_cand_subset <- subset(smartvotedata_json_candidates, elected == TRUE)

gender_distribution_data <- select(smartvotedata_json_candidates, "ID_Candidate", "gender") %>%
    mutate(gender = replace(gender, gender == "m", "M")) %>%
    mutate(gender = replace(gender, gender == "f", "F")) %>%
    group_by(gender) %>%
    count(gender) %>%
    rename(Geschlecht = gender)

gender_distribution_data_elected <- select(elected_cand_subset, "ID_Candidate", "gender") %>%
    mutate(gender = replace(gender, gender == "m", "M")) %>%
    mutate(gender = replace(gender, gender == "f", "F")) %>%
    group_by(gender) %>%
    count(gender) %>%
    rename(Geschlecht = gender)

# --- visualize
gender_distribution_vis_tot <- ggplot(gender_distribution_data, aes(x=2, y=n, fill=Geschlecht)) +
    geom_bar(stat="identity", color = "white", alpha = 0.8) +
    scale_fill_manual(values = c("#56b1f7", "#132b43")) +
    coord_polar(theta = "y", start = 0) +
    geom_text(aes(label = n), color = "white", size = 5, position = position_stack(vjust = 0.5)) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5), plot.margin=margin(0,0,0,0,"cm")) +
    xlim(0.7, 2.5) +
    ggtitle("Alle Kandidierende")

gender_distribution_vis_elected <- ggplot(gender_distribution_data_elected, aes(x=2, y=n, fill=Geschlecht)) +
    geom_bar(stat="identity", color = "white", alpha = 0.8) +
    scale_fill_manual(values = c("#56b1f7", "#132b43")) +
    coord_polar(theta = "y", start = 0)+
    geom_text(aes(label = n), color = "white", size = 5, position = position_stack(vjust = 0.5))+
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5), plot.margin=margin(0,0,0,0,"cm")) +
    xlim(0.7, 2.5) +
    ggtitle("Gewählte Kandidierende") 

ggarrange(gender_distribution_vis_tot, gender_distribution_vis_elected, ncol = 2, nrow = 1)

# ==============================================================================
# END
# ==============================================================================

