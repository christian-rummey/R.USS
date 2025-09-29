
# ==============================================================================
# USS Paper - Data Management Script
# Purpose: Prepare ataxia scale data for USS (Upright Stability Score) analysis
# Studies: CRCSCA (Spinocerebellar Ataxia) and UNIFAI (Friedreich's Ataxia)
# ==============================================================================

# Load project settings and themes
source('.project.settings.R')

# Required packages (add explicit loading for clarity)
library(tidyverse)
library(magrittr)

# CONFIGURATION ================================================================

# Define clinical scales to include in analysis
# Core scales: USS (FARS.E), SARA, fSARA, ADL + subscales
scales.list <- c(
  # Total scores
  'mFARS', 'SARA', 'fSARA', 'FARS.E', 'ADL',

  # SARA subscales
  'SARA.ax',   # Axial function
  'SARA.ki',   # Kinetic function (appendicular)
  'SARA.ku',   # Upper limb coordination

  # ADL subscales
  'ADL.upper', 'ADL.lower', 'ADL.bulbar', 'ADL.other',

  # Individual items from custom lists
  .l.sara,           # SARA individual items
  .l.fsara[c(1,2,4)], # Selected fSARA items
  .l.adl,            # ADL individual items
  .l.FARS.E          # USS individual items
)

# SCALE METADATA ==============================================================

# Load and process scale metadata
scales <- .rt('../../Ataxia/DATA other/scales.txt') %>%
  select(-pl2) %>%
  filter(paramcd %in% scales.list) %>%
  # Classify scales by instrument type
  mutate(score = case_when(
    paramcd %in% scales.list[c(1, 4, 6, 8)]  ~ 'mFARS',
    paramcd %in% scales.list[c(2, 5, 7, 9)]  ~ 'SARA',
    paramcd %in% scales.list[c(3)]           ~ 'fSARA',
    .default = NA_character_
  )) %>%
  # Classify by functional domain
  mutate(score.type = case_when(
    paramcd %in% scales.list[c(4, 5)]  ~ 'Axial Function',
    paramcd %in% scales.list[c(6, 7)]  ~ 'Appendicular Function',
    paramcd %in% scales.list[c(8, 9)]  ~ 'Speech Disorder',
    .default = 'Total Score'
  )) %>%
  mutate(paramcd = factor(paramcd, scales.list))

# Save processed scale metadata
scales %>%
  write.table('DATA derived/scales.txt', sep = '\t', row.names = FALSE)

# LOAD RAW DATA ===============================================================

# Studies to include
target_studies <- c('CRCSCA', 'UNIFAI')

# Load and combine clinical assessment data from multiple sources
dt. <- bind_rows(
  .dd('fars') %>% filter(study %in% target_studies),  # USS/mFARS data
  .dd('sara') %>% filter(study %in% target_studies),  # SARA data
  .dd('adl')  %>% filter(study %in% target_studies)   # ADL data
) %>%
  select(-age, -time.)  # Remove columns that will be recalculated

# Apply temporal filters and data selection
dt. <- dt. %>%
  select(-avisit) %>%
  # Include all UNIFAI data OR CRCSCA data from March 2024 onwards
  filter(adt >= '2024-03-01' | study == 'UNIFAI') %>%
  # Retain only scales defined in our configuration
  filter(paramcd %in% scales$paramcd)

# ADD DEMOGRAPHICS AND TIME VARIABLES =========================================

# Process UNIFAI (Friedreich's Ataxia) data
dt.FA <- dt. %>%
  filter(study == 'UNIFAI') %>%
  filter(!is.na(adt)) %>%
  # Join demographic data
  left_join(
    .dd('demo') %>%
      select(study, sjid, dob, aoo, sev.o) %>%
      mutate(subtype = as.character(sev.o)),
    by = c("study", "sjid")
  ) %>%
  # Calculate age and disease duration
  mutate(
    age = as.numeric(adt - dob) / 365.25,  # Age in years
    dur = age - aoo                        # Disease duration
  ) %>%
  # Calculate time from first visit per subject
  group_by(sjid) %>%
  mutate(time. = age - min(age, na.rm = TRUE)) %>%
  ungroup() %>%
  select(study, sjid, subtype, avisitn, time., age, dur, paramcd, aval)

# Process CRCSCA (Spinocerebellar Ataxia) data
dt.SCA <- dt. %>%
  filter(study == 'CRCSCA') %>%
  filter(!is.na(adt)) %>%
  # Complex join to get demographics and baseline visit dates
  left_join(
    .dd('demo.sca') %>%
      filter(study == 'CRCSCA') %>%
      select(study, sjid, sca, aoo) %>%
      left_join(
        .dd('visit.dates.CRCSCA') %>%
          group_by(sjid) %>%
          filter(avisitn == min(avisitn)) %>%
          ungroup() %>%
          select(study, sjid, adt, age_bl),
        by = c("study", "sjid")
      ) %>%
      select(study, sjid, adt_bl = adt, age_bl, aoo, sca) %>%
      mutate(subtype = as.character(sca)),
    by = c("study", "sjid")
  ) %>%
  arrange(study, sjid, adt) %>%
  # Calculate time variables
  group_by(sjid) %>%
  mutate(
    time. = as.numeric(adt - min(adt, na.rm = TRUE)) / 365.25,  # Time from baseline
    age   = age_bl + time.,                                      # Current age
    dur   = age - aoo                                           # Disease duration
  ) %>%
  ungroup() %>%
  select(study, sjid, subtype, avisitn, time., age, dur, paramcd, aval)

# COMBINE AND CLEAN DATA =====================================================

# Combine both study datasets
dt. <- bind_rows(dt.SCA, dt.FA) %>%
  # Ensure only configured parameters are retained
  filter(paramcd %in% scales$paramcd) %>%
  # Remove records with missing age data
  filter(!is.na(age))

# DATA QUALITY CHECKS AND FIXES ==============================================

# Check for subjects with missing age (for transparency)
missing_age_subjects <- dt. %>%
  filter(is.na(age)) %>%
  select(sjid, subtype) %>%
  distinct()

if (nrow(missing_age_subjects) > 0) {
  warning(paste("Subjects with missing age data:", nrow(missing_age_subjects)))
}

# Identify and resolve duplicate baseline records
duplicate_baseline <- dt. %>%
  group_by(sjid) %>%
  filter(paramcd == 'FARS.E' & study == 'CRCSCA') %>%
  filter(age == min(age)) %>%
  group_by(sjid) %>%
  filter(n() > 1) %>%
  ungroup()

# HARDCODED FIX: Remove specific problematic record
# TODO: Investigate why subject JH115 visit 1 is problematic
dt. <- dt. %>%
  filter(!(sjid == 'JH115' & avisitn == 1))

# ADD FUNCTIONAL DISABILITY STAGING ==========================================

# Convert from long to wide format to enable cross-scale calculations
dt. <- dt. %>%
  spread(paramcd, aval) %>%
  ungroup() %>%
  # Add functional disability staging (FDS) data
  left_join(
    .dd('steps') %>%
      filter(study %in% target_studies) %>%
      select(study, sjid, avisitn, fds) %>%
      filter(!is.na(fds)) %>%
      distinct(),  # Remove any duplicates
    by = c("study", "sjid", "avisitn")
  ) %>%
  # Use fane7 (gait score) as backup when FDS is missing
  mutate(fds = if_else(is.na(fds), fane7, fds)) %>%
  droplevels()

# DEFINE ANALYSIS COHORT ==================================================

# Require both USS (FARS.E) and SARA scores for inclusion
dt. <- dt. %>%
  filter(!is.na(FARS.E)) %>%
  filter(!is.na(SARA))

# CREATE DERIVED VARIABLES ===================================================

# Define clinical status categories based on scale scores
dt. <- dt. %>%
  mutate(
    # Preataxic: No detectable ataxia symptoms
    is.preataxic = (FARS.E == 0 & SARA == 0),

    # Non-ambulatory: Unable to walk unassisted (based on gait score)
    is.nonamb = (fane7 >= 5),

    # Can stand: Able to maintain standing position
    can.stand = (fane2a < 4),

    # Mild USS score: Less than 31 points (out of possible range)
    is.30ol = (FARS.E < 31)
  ) %>%
  # Convert back to long format for analysis
  gather(paramcd, aval, all_of(scales$paramcd)) %>%
  filter(!is.na(aval))

# CALCULATE VISIT NUMBERS FROM BASELINE =====================================

dt. <- dt. %>%
  group_by(study, sjid) %>%
  mutate(avisitx = avisitn - min(avisitn)) %>%
  ungroup() %>%
  select(study, sjid, avisitn, avisitx, paramcd, aval, everything())

# CREATE AMBULATION STATUS VARIABLE ========================================

dt. <- dt. %>%
  mutate(status = case_when(
    is.preataxic ~ 'preataxic',
    is.nonamb    ~ 'non-ambulatory',  # Fixed typo
    .default     = 'ambulatory'
  ))

# RESTRICT TO BASELINE VISITS ONLY =========================================

dt. <- dt. %>%
  filter(avisitx == 0)

# Report final sample size
final_n_subjects <- length(unique(dt.$sjid))
message(paste("Final analysis dataset includes", final_n_subjects, "subjects"))

# FIX DATA ENTRY ERRORS ==================================================

# Correct obvious FDS data entry errors (decimal point issues)
dt. <- dt. %>%
  mutate(
    fds = case_when(
      fds == 45 ~ 4.5,  # 45 should be 4.5
      fds == 25 ~ 2.5,  # 25 should be 2.5
      .default = fds
    )
  )

# SAVE PROCESSED DATASET ==================================================

dt. %>%
  write_rds('DATA derived/dt.all.visits.rds')

message("Data processing complete. Dataset saved to 'DATA derived/dt.all.visits.rds'")

# ==============================================================================
# END OF DATA PROCESSING
# ==============================================================================

# Optional: Display final dataset summary
if (interactive()) {
  cat("\n=== FINAL DATASET SUMMARY ===\n")

  # Study distribution
  cat("\nSubjects by study:\n")
  print(dt. %>% distinct(sjid, study) %>% count(study))

  # Status distribution
  cat("\nSubjects by ambulation status:\n")
  print(dt. %>% distinct(sjid, status) %>% count(status))

  # Available scales
  cat("\nAvailable parameters:\n")
  print(dt. %>% count(paramcd, sort = TRUE))
}
