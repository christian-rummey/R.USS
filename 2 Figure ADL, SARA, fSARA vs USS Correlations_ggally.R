# -----------------------------------------------------------------------
#
# USS Correlation Analysis - ADL, SARA, fSARA vs USS
#
# Purpose: Generate correlation plots between Upright Stability Score (USS) and
#          other clinical assessment scales (ADL, SARA, fSARA)
# Output:  Combined correlation plot showing all scale relationships
#
# -----------------------------------------------------------------------

# Setup and configuration -----------------------------------------------------

# Clear environment and load project settings
rm(list = ls())
source('.project.settings.R')

library(patchwork)  # for combining plots
library(ggside)     # for marginal densities

# Configuration
# Define clinical scales and their display labels
CLINICAL_SCALES <- c('ADL', 'SARA', 'SARA.ax', 'SARA.ki', 'FARS.E', 'fSARA')
SCALE_LABELS <- c('ADL', 'SARA', 'SARA.axial', 'SARA.appendicular', 'USS', 'fSARA')

# Optional parameters to control which plots to include
include_SARA_axial <- TRUE          # Include SARA.axial plot
include_SARA_appendicular <- TRUE   # Include SARA.appendicular plot

# USS plot limits (based on scale range)
USS_YLIM <- c(0, 36)

# Data loading and preprocessing ---------------------------------------------

# Load preprocessed clinical data
dt_raw <- readRDS('DATA derived/dt.all.visits.rds') %>%
  # Exclude non-ambulatory patients from correlation analysis
  filter(!is.nonamb) %>%
  # Create ambulation status categories
  mutate(status = case_when(
    is.preataxic ~ 'preataxic',
    is.nonamb    ~ 'non-ambulatory',
    .default     = 'ambulatory'
  )) %>%
  droplevels()

# Apply scale labels and filter for relevant parameters
dt_processed <- dt_raw %>%
  mutate(paramcd = factor(paramcd,
                         levels = CLINICAL_SCALES,
                         labels = SCALE_LABELS)) %>%
  filter(!is.na(paramcd))

# DATA QUALITY CHECKS -------------------------------------

# Check data availability by study and visit
data_summary <- dt_processed %>%
  select(study, sjid, avisitx, paramcd, aval) %>%
  spread(paramcd, aval) %>%
  filter(!is.na(USS)) %>%
  distinct() %>%
  ungroup() %>%
  select(study, avisitx) %>%
  count(study, avisitx, name = "n_subjects")

print("Data availability by study and visit:")
print(data_summary)

# Prepare analysis dataset ---------------------------------------------=====

# Convert to wide format for correlation analysis
dt_wide <- dt_processed %>%
  group_by(study, sjid, avisitn) %>%
  spread(paramcd, aval)

# Prepare correlation analysis dataset
# Focus on baseline visits and ambulatory patients
dt_correlation <- dt_wide %>%
  filter(!is.nonamb) %>%
  # Convert back to long format for the scales of interest
  gather(paramcd, aval, ADL, SARA, SARA.axial, SARA.appendicular, fSARA) %>%
  # Use baseline visits only
  group_by(sjid) %>%
  filter(avisitn == min(avisitn)) %>%
  ungroup() %>%
  # Remove missing values
  filter(!is.na(aval), !is.na(USS)) %>%
  # Filter scales based on inclusion settings
  filter(paramcd %in% {
    scales_to_include <- c('SARA', 'fSARA', 'ADL')
    if(include_SARA_axial) scales_to_include <- c(scales_to_include, 'SARA.axial')
    if(include_SARA_appendicular) scales_to_include <- c(scales_to_include, 'SARA.appendicular')
    scales_to_include
  }) %>%
  mutate(paramcd = factor(paramcd, levels = {
    factor_levels <- c('SARA')
    if(include_SARA_axial) factor_levels <- c(factor_levels, 'SARA.axial')
    if(include_SARA_appendicular) factor_levels <- c(factor_levels, 'SARA.appendicular')
    factor_levels <- c(factor_levels, 'fSARA', 'ADL')
    factor_levels
  }))

# Exclude preataxic patients from correlation analysis (optional)
dt_plot <- dt_correlation %>%
  filter(!is.preataxic)

# Correlation plotting functions ---------------------------------------------

#' Create correlation plot for a single clinical scale vs USS
#' @param data Dataset containing USS, scale scores, and study information
#' @param scale_name Name of the clinical scale to plot
#' @return ggplot object
create_single_correlation_plot <- function(data, scale_name) {

  # Filter data for the specific scale
  plot_data <- data %>%
    filter(paramcd == scale_name)

  # Create base plot
  p <- plot_data %>%
    ggplot(aes(x = aval, y = USS, color = study)) +
    geom_point(alpha = 0.7, size = 2) +
    # Add color scheme
    ggsci::scale_color_d3() +
    # Add correlation statistics
    ggpmisc::stat_correlation(
      aes(label = paste(after_stat(rr.label))),
      size = 10 / .pt,
      family = theme_get()$text$family,
      alpha = NA,
      data = plot_data %>% droplevels()
    ) +
    # Add regression line
    geom_smooth(method = lm, se = FALSE, alpha = 0.8) +
    # Set axis limits and labels
    coord_cartesian(ylim = USS_YLIM) +
    labs(
      x = scale_name,
      y = "Upright Stability Score (USS)",
      color = "study",
      title = paste(scale_name, "vs USS Correlation")
    ) +
    # Apply custom theme (assuming .leg() is defined in project settings)
    .leg('lr')+
    .theme()

  return(p)
}

# Generate individual correlation plots ======================================

# Create individual plots for each scale
plot_ADL <- create_single_correlation_plot(dt_plot, "ADL")
plot_SARA <- create_single_correlation_plot(dt_plot, "SARA")
if(include_SARA_axial) {
  plot_SARA.ax <- create_single_correlation_plot(dt_plot, "SARA.axial")
}
if(include_SARA_appendicular) {
  plot_SARA.ap <- create_single_correlation_plot(dt_plot, "SARA.appendicular")
}
plot_fSARA <- create_single_correlation_plot(dt_plot, "fSARA")

# # Display individual plots (for verification)
# print("Individual correlation plots:")
# print(plot_ADL)
# print(plot_SARA)
# print(plot_fSARA)

# Create combined plot ---------------------------------------------=========

# Optional parameter to add marginal density plots
add_marginal_densities <- TRUE

# Create individual plots for each scale with side densities
plot_SARA_clean <- dt_plot %>%
  filter(paramcd == "SARA") %>%
  ggplot(aes(x = aval, y = USS, color = study, fill = study)) +
  geom_point(alpha = 0.7, size = 1.5) +
  ggsci::scale_color_d3(name = "study") +
  ggsci::scale_fill_d3(name = "study") +
  geom_smooth(method = lm, se = FALSE, alpha = 0.8) +
  ggpmisc::stat_correlation(
    aes(label = paste(after_stat(rr.label))),
    size = 10 / .pt,
    family = theme_get()$text$family,
    alpha = NA,
    data = dt_plot %>% filter(paramcd == "SARA") %>% droplevels()
  ) +
  geom_xsidedensity(alpha = 0.7) +
  # No y-density for SARA (top-left)
  scale_xsidey_continuous(labels = NULL, breaks = NULL) +
  coord_cartesian(ylim = USS_YLIM) +
  labs(x = "SARA", y = "USS", color = "study") +
  .leg('none') +
  .theme() +
  theme(ggside.panel.scale = 0.3)

plot_fSARA_clean <- dt_plot %>%
  filter(paramcd == "fSARA") %>%
  ggplot(aes(x = aval, y = USS, color = study, fill = study)) +
  geom_point(alpha = 0.7, size = 1.5) +
  ggsci::scale_color_d3(name = "study") +
  ggsci::scale_fill_d3(name = "study") +
  geom_smooth(method = lm, se = FALSE, alpha = 0.8) +
  ggpmisc::stat_correlation(
    aes(label = paste(after_stat(rr.label))),
    size = 10 / .pt,
    family = theme_get()$text$family,
    alpha = NA,
    data = dt_plot %>% filter(paramcd == "fSARA") %>% droplevels()
  ) +
  geom_xsidedensity(alpha = 0.7) +
  # No y-density for fSARA (bottom-left)
  scale_xsidey_continuous(labels = NULL, breaks = NULL) +
  coord_cartesian(ylim = USS_YLIM) +
  labs(x = "fSARA", y = "USS", color = "study") +
  .leg('none') +
  .theme() +
  theme(ggside.panel.scale = 0.3)

# Create SARA axial plot if included
if(include_SARA_axial) {
  plot_SARA_ax_clean <- dt_plot %>%
    filter(paramcd == "SARA.axial") %>%
    ggplot(aes(x = aval, y = USS, color = study, fill = study)) +
    geom_point(alpha = 0.7, size = 1.5) +
    ggsci::scale_color_d3() +
    ggsci::scale_fill_d3() +
    geom_smooth(method = lm, se = FALSE, alpha = 0.8) +
    ggpmisc::stat_correlation(
      aes(label = paste(after_stat(rr.label))),
      size = 10 / .pt,
      family = theme_get()$text$family,
      alpha = NA,
      data = dt_plot %>% filter(paramcd == "SARA.axial") %>% droplevels()
    ) +
    geom_xsidedensity(alpha = 0.7) +
    # No y-density for SARA.axial (top-middle)
    scale_xsidey_continuous(labels = NULL, breaks = NULL) +
    coord_cartesian(ylim = USS_YLIM) +
    labs(x = "SARA.axial", y = "USS", color = "study") +
    .leg('none') +
    .theme() +
    theme(ggside.panel.scale = 0.3)
}

plot_ADL_clean <- dt_plot %>%
  filter(paramcd == "ADL") %>%
  ggplot(aes(x = aval, y = USS, color = study, fill = study)) +
  geom_point(alpha = 0.7, size = 1.5) +
  ggsci::scale_color_d3(name = "study") +
  ggsci::scale_fill_d3(name = "study") +
  geom_smooth(method = lm, se = FALSE, alpha = 0.8) +
  ggpmisc::stat_correlation(
    aes(label = paste(after_stat(rr.label))),
    size = 10 / .pt,
    family = theme_get()$text$family,
    alpha = NA,
    data = dt_plot %>% filter(paramcd == "ADL") %>% droplevels()
  ) +
  geom_xsidedensity(alpha = 0.7) +
  geom_ysidedensity(alpha = 0.7) +
  scale_xsidey_continuous(labels = NULL, breaks = NULL) +
  scale_ysidex_continuous(labels = NULL, breaks = NULL) +
  coord_cartesian(ylim = USS_YLIM) +
  labs(x = "ADL", y = "USS", color = "study") +
  .leg('none') +
  .theme() +
  theme(ggside.panel.scale = 0.3)

# Create SARA appendicular plot if included
if(include_SARA_appendicular) {
  plot_SARA_ap_clean <- dt_plot %>%
    filter(paramcd == "SARA.appendicular") %>%
    ggplot(aes(x = aval, y = USS, color = study, fill = study)) +
    geom_point(alpha = 0.7, size = 1.5) +
    ggsci::scale_color_d3() +
    ggsci::scale_fill_d3() +
    geom_smooth(method = lm, se = FALSE, alpha = 0.8) +
    ggpmisc::stat_correlation(
      aes(label = paste(after_stat(rr.label))),
      size = 10 / .pt,
      family = theme_get()$text$family,
      alpha = NA,
      data = dt_plot %>% filter(paramcd == "SARA.appendicular") %>% droplevels()
    ) +
    geom_xsidedensity(alpha = 0.7) +
    geom_ysidedensity(alpha = 0.7) +
    scale_xsidey_continuous(labels = NULL, breaks = NULL) +
    scale_ysidex_continuous(labels = NULL, breaks = NULL) +
    coord_cartesian(ylim = USS_YLIM) +
    labs(x = "SARA.appendicular", y = "USS", color = "study") +
    .leg('none') +
    .theme() +
    theme(ggside.panel.scale = 0.3)
}

# Create layout based on inclusion settings - 3 cols x 2 rows
# Top row: SARA, SARA.axial (if included), SARA.appendicular (if included)
# Bottom row: fSARA, ADL, (empty)

# Create layout based on inclusion settings - 3 cols x 2 rows
# Top row: SARA, SARA.axial (if included), SARA.appendicular (if included)
# Bottom row: fSARA, ADL, (empty)

if(include_SARA_axial && include_SARA_appendicular) {
  # Full layout: all 5 plots
  design <- "
ABC
DE#
"
  combined_plot <- plot_SARA_clean + plot_SARA_ax_clean + plot_SARA_ap_clean +
                   plot_fSARA_clean + plot_ADL_clean +
                   plot_layout(design = design)
} else if(include_SARA_axial && !include_SARA_appendicular) {
  # SARA + SARA.axial only
  design <- "
AB#
CD#
"
  combined_plot <- plot_SARA_clean + plot_SARA_ax_clean +
                   plot_fSARA_clean + plot_ADL_clean +
                   plot_layout(design = design)
} else if(!include_SARA_axial && include_SARA_appendicular) {
  # SARA + SARA.appendicular only
  design <- "
A#B
CD#
"
  combined_plot <- plot_SARA_clean + plot_SARA_ap_clean +
                   plot_fSARA_clean + plot_ADL_clean +
                   plot_layout(design = design)
} else {
  # Only SARA (minimal)
  design <- "
A##
BC#
"
  combined_plot <- plot_SARA_clean + plot_fSARA_clean + plot_ADL_clean +
                   plot_layout(design = design)
}

# Add legend centered above all plots
combined_plot_with_legend <- combined_plot +
  plot_layout(guides = "collect") &
  theme(legend.position = "top", legend.justification = "center")

# Show combined plot with densities
print("Combined plot with marginal densities:")
print(combined_plot_with_legend)

# Assign to 'p' for .sp() function
p <- combined_plot_with_legend

# Notes and interpretation -----------------------------------------------
#
# This analysis examines the correlation between the Upright Stability Score (USS)
# and other clinical assessment scales:
# - ADL: Activities of Daily Living scale
# - SARA: Scale for the Assessment and Rating of Ataxia
# - fSARA: Functional SARA (abbreviated version)
# - SARA.axial: SARA axial function subscore
# - SARA.appendicular: SARA appendicular function subscore
#
# Key findings:
# - Correlations are calculated using baseline visits only
# - Non-ambulatory and preataxic patients are excluded from analysis
# - Separate correlations are shown by study (CRCSCA vs UNIFAI)
# - Statistical significance is indicated on each plot
#
# Expected relationships:
# - Higher USS scores should correlate with higher SARA/fSARA scores (more ataxia)
# - Higher USS scores should correlate with higher ADL scores (more disability)
#
# -----------------------------------------------------------------------

# for exporting pptx slide
# .sp( ti = "Figure 2", i = 1, l = 'F')