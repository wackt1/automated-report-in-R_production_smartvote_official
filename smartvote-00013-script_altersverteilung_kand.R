# ==============================================================================
# smartvote-00013-script_altersverteilung_kand.R
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
library(yaml)
library(viridis)

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------
config_file = yaml.load_file("_parameter.yml")

PARAM_JSON_URL_CANDIDATES <- config_file$param_JSON_URL$JSON_URL_CANDIDATES_DATA
PARAM_DISTRICT <- config_file$params_wahlkreis$WAHLKREIS
YEAR_OF_ELECTION = 2019

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

# --- preprocess
if(PARAM_DISTRICT != "NA") {
    smartvotedata_json_candidates <- filter(smartvotedata_json_candidates, district == PARAM_DISTRICT)
}

# --- analyze
yob_cand_data <- select(smartvotedata_json_candidates, "elected", "year_of_birth")
yob_cand_data %<>%
    mutate(age = YEAR_OF_ELECTION - year_of_birth) %>%
    mutate(elected = ifelse(elected, 'Ja', 'Nein')) %>%
    as_tibble(yob_cand_data) %>%
    rename(Gewählt = elected)

mean_age_elected <- subset(yob_cand_data, Gewählt == 'Ja', drop = TRUE)
mean_age_elected_calc <- mean(mean_age_elected$age)
mean_age_elected_calc <- round(mean_age_elected_calc, digits = 2)

mean_age_all <- mean(yob_cand_data$age)
mean_age_all <- round(mean_age_all, digits = 2)

# --- visualize
age_distribution_cand <- ggplot(yob_cand_data, aes(age, fill = Gewählt)) +
    geom_histogram(binwidth = 3, col = "white", size = 0.7, alpha = 0.8, position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = c("#56b1f7", "#132b43")) +
    scale_x_continuous(breaks = seq(0, 100, by = 5)) +
    ggtitle(wrapper(paste0("Altersverteilung aller Kandidierenden und der gewählten Kandidierenden"))) +
    xlab("Alter Kandidierende") +
    ylab("Anzahl Kandidierende") +
    theme_classic()
    
age_distribution_cand

# ==============================================================================
# END
# ==============================================================================

