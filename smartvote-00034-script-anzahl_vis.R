# ==============================================================================
# smartvote-00034-script-anzahl_vis.R
#
# Skript zur Visualisierung der smartvote-Teilnahme Kandidierende und Gewaehlte
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
library(ggpubr)

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
data_subset_all <- select(smartvotedata_json_candidates, "n_answers")
data_subset_all %<>%
    mutate(confirmed = (data_subset_all$n_answers == 75)) %>%
    as_tibble(data_subset_all) %>%
    count(confirmed) %>%
    mutate(confirmed_label = confirmed) %>%
    mutate(confirmed = ifelse(confirmed, 'Ja', 'Nein')) %>%
    mutate(percentage = n / sum(n)) %>%
    mutate(percentage = round(percentage, digits = 2)) %>%
    mutate(label_text_1 = paste(n, "(")) %>%
    mutate(label_text_2 = paste(label_text_1, percentage, "%", ")", sep="")) %>%
    rename(Bestätigt = confirmed)

data_subset_elected <- select(smartvotedata_json_candidates, "elected", "n_answers")
data_subset_elected %<>%
    mutate(confirmed = (data_subset_elected$n_answers == 75)) %>%
    filter(elected == TRUE) %>%
    as_tibble(data_subset_elected) %>%
    count(confirmed) %>%
    mutate(confirmed_label = confirmed) %>%
    mutate(confirmed = ifelse(confirmed, 'Ja', 'Nein')) %>%
    mutate(percentage = n / sum(n)) %>%
    mutate(percentage = round(percentage, digits = 2)) %>%
    mutate(label_text_1 = paste(n, "(")) %>%
    mutate(label_text_2 = paste(label_text_1, percentage, "%", ")", sep="")) %>%
    rename(Bestätigt = confirmed)

colors_chart <- c("#56b1f7", "#132b43")

# --- visualize
plot_all_vis <- ggplot(data_subset_all, aes(x = "", y = n, fill = Bestätigt, label = paste(percentage*100, "%"))) + 
    geom_bar(stat = "identity", width = 0.3) +
    geom_text(size = 4.5, position = position_stack(vjust = 0.5), color="white") +
    scale_fill_manual(values = colors_chart) +
    ggtitle("smartvote-Teilnahme aller Kandidierenden") +
    xlab("") + 
    ylab("Anzahl Kandidierende") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.margin=grid::unit(c(0,0,10,0), "mm")) +
    coord_flip()

plot_elected_vis <- ggplot(data_subset_elected, aes(x = "", y = n, fill = Bestätigt, label = paste(percentage*100, "%"))) + 
    geom_bar(stat = "identity", width = 0.3) +
    geom_text(size = 4.5, position = position_stack(vjust = 0.5), color="white") +
    scale_fill_manual(values = colors_chart) +
    ggtitle("smartvote-Teilnahme der gewählten Kandidierenden") +
    xlab("") + 
    ylab("Anzahl gewählte Kandidierende") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.margin=grid::unit(c(0,0,10,0), "mm")) +
    coord_flip()

ggarrange(plot_all_vis, plot_elected_vis, ncol = 1, nrow = 2)

# ==============================================================================
# END
# ==============================================================================
