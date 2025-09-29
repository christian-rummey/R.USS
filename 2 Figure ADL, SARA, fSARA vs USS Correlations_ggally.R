# ==============================================================================
# USS CORRELATION ANALYSIS - ADL, SARA, fSARA vs USS
# ==============================================================================
# Purpose: Generate correlation plots between Upright Stability Score (USS) and
#          other clinical assessment scales (ADL, SARA, fSARA)
# Output:  Combined correlation plot showing all three scale relationships
# ==============================================================================

# SETUP AND CONFIGURATION =====================================================

# Clear environment and load project settings
rm(list = ls())
source('.project.settings.R')

# Required packages (ensure these are loaded via .project.settings.R)
# - tidyverse (for data manipulation)
# - magrittr (for pipe operators)
# - ggplot2 (for plotting)
# - ggpmisc (for correlation statistics)
# - ggsci (for color palettes)
# - ggExtra (for marginal plots)

# CONFIGURATION
# Define clinical scales and their display labels
CLINICAL_SCALES <- c('ADL', 'SARA', 'FARS.E', 'fSARA')
SCALE_LABELS <- c('ADL', 'SARA', 'USS', 'fSARA')

# USS plot limits (based on scale range)
USS_YLIM <- c(0, 36)

# DATA LOADING AND PREPROCESSING ==============================================

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

# DATA QUALITY CHECKS ========================================================

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

# PREPARE ANALYSIS DATASET ===================================================

# Convert to wide format for correlation analysis
dt_wide <- dt_processed %>%
  group_by(study, sjid, avisitn) %>%
  spread(paramcd, aval)

# Prepare correlation analysis dataset
# Focus on baseline visits and ambulatory patients
dt_correlation <- dt_wide %>%
  filter(!is.nonamb) %>%
  # Convert back to long format for the three scales of interest
  gather(paramcd, aval, ADL, SARA, fSARA) %>%
  # Use baseline visits only
  group_by(sjid) %>%
  filter(avisitn == min(avisitn)) %>%
  ungroup() %>%
  # Remove missing values
  filter(!is.na(aval), !is.na(USS)) %>%
  # Ensure scale order for plotting
  filter(paramcd %in% c('ADL', 'SARA', 'fSARA')) %>%
  mutate(paramcd = factor(paramcd, levels = c('ADL', 'SARA', 'fSARA')))

# Exclude preataxic patients from correlation analysis (optional)
dt_plot <- dt_correlation %>%
  filter(!is.preataxic)

# CORRELATION PLOTTING FUNCTIONS ==============================================

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
      color = "Study",
      title = paste(scale_name, "vs USS Correlation")
    ) +
    # Apply custom theme (assuming .leg() is defined in project settings)
    .leg('lr')

  return(p)
}

# GENERATE INDIVIDUAL CORRELATION PLOTS ======================================

# Create individual plots for each scale
plot_ADL <- create_single_correlation_plot(dt_plot, "ADL")
plot_SARA <- create_single_correlation_plot(dt_plot, "SARA")
plot_fSARA <- create_single_correlation_plot(dt_plot, "fSARA")

# Display individual plots (for verification)
print("Individual correlation plots:")
print(plot_ADL)
print(plot_SARA)
print(plot_fSARA)

# CREATE FACETED PLOT ========================================================

# Optional parameter to add marginal density plots
add_marginal_densities <- TRUE

# Create faceted plot: SARA and fSARA in first row, ADL in second row
faceted_plot <- dt_plot %>%
  ggplot(aes(x = aval, y = USS, color = study)) +
  geom_point(alpha = 0.7, size = 1.5) +
  ggsci::scale_color_d3() +
  geom_smooth(method = lm, se = FALSE, alpha = 0.8) +
  facet_wrap(~ paramcd, scales = "free_x", nrow = 2, ncol = 2) +
  coord_cartesian(ylim = USS_YLIM) +
  labs(
    x = "Clinical Scale Score",
    y = "Upright Stability Score (USS)",
    color = "Study"
  ) +
  .leg('lr')

# Add marginal density plots if requested
if (add_marginal_densities) {
  faceted_plot <- faceted_plot %>%
    ggExtra::ggMarginal(type = 'density', groupFill = TRUE)
}

print(faceted_plot)

# ==============================================================================
# NOTES AND INTERPRETATION
# ==============================================================================
#
# This analysis examines the correlation between the Upright Stability Score (USS)
# and three other clinical assessment scales:
# - ADL: Activities of Daily Living scale
# - SARA: Scale for the Assessment and Rating of Ataxia
# - fSARA: Functional SARA (abbreviated version)
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
# ==============================================================================