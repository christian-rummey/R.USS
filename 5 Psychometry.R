# -----------------------------------------------------------------------
# Psychometric properties for USS (FARS.E), SARA, fSARA
# Baseline-only (earliest visit per subject), ambulatory cohort
# -----------------------------------------------------------------------
rm(list = ls())
source(".project.settings.R")

suppressPackageStartupMessages({
  library(tidyverse)
  library(psych)
})

# -----------------------------------------------------------------------
# INPUTS (assumed available in environment / project settings)
# - dt.all.visits.rds: long table with at least:
#   sjid, avisitn, is.preataxic, has.both, is.nonamb, subtype, study (optional),
#   paramcd (item code), aval (item score), age, dur, fds
# - Item vectors: .l.FARS.E, .l.sara, .l.fsara (character vectors of item names)
# If you also have theoretical maxima, set them here; otherwise observed maxima will be used.
# -----------------------------------------------------------------------

dt. <- readRDS("DATA derived/dt.all.visits.rds") %>%
  filter(!is.preataxic, has.both, !is.nonamb) %>%
  filter(!is.na(subtype), !subtype %in% c("SCA10","SCA7","SCA8")) %>%
  droplevels()

# Keep only items we need
items.keep <- unique(c(.l.FARS.E, .l.sara, .l.fsara, .l.adl))

# Baseline (earliest visit per subject)
dt.base <- dt. %>%
  group_by(sjid) %>%
  filter(avisitn == min(avisitn, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(paramcd %in% items.keep)

# Wide item matrix per subject
X <- dt.base %>%
  select(sjid, age, dur, fds, paramcd, aval) %>%
  mutate(paramcd = as.character(paramcd)) %>%
  distinct() %>%
  pivot_wider(names_from = paramcd, values_from = aval) %>%
  arrange(sjid)

# Convenience accessors
safe_select <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (length(cols) == 0) tibble(.dummy = numeric(0)) else dplyr::select(df, all_of(cols))
}
items <- list(
  USS   = .l.FARS.E,
  SARA  = .l.sara,
  fSARA = .l.fsara,
  ADL   = .l.adl
)

# -----------------------------------------------------------------------
# Totals (row sums) and analysis frame
# -----------------------------------------------------------------------
scores <- X %>%
  mutate(
    USS   = rowSums(safe_select(., items$USS),   na.rm = TRUE),
    SARA  = rowSums(safe_select(., items$SARA),  na.rm = TRUE),
    fSARA = rowSums(safe_select(., items$fSARA), na.rm = TRUE),
    ADL   = rowSums(safe_select(., items$ADL), na.rm = TRUE)
  ) %>%
  # Keep metadata first
  relocate(sjid, age, dur, fds, USS, SARA, fSARA, ADL)

# -----------------------------------------------------------------------
# 0) Number of items per scale
# -----------------------------------------------------------------------
t0.NRitems <- tibble(
  USS   = length(items$USS),
  SARA  = length(items$SARA),
  fSARA = length(items$fSARA),
  ADL   = length(items$ADL)
)

# -----------------------------------------------------------------------
# Helper: alpha and item-total correlations per scale
# -----------------------------------------------------------------------
alpha_itcorr <- function(df_items) {
  df_items <- df_items %>% as.data.frame()
  if (ncol(df_items) == 0 || nrow(df_items) == 0) {
    return(list(alpha = NA_real_, item_total = tibble(item = character(), r_drop = numeric())))
  }
  a <- suppressWarnings(psych::alpha(df_items, check.keys = FALSE, warnings = FALSE))
  # std.alpha is generally preferred
  alpha_val <- unname(a$total$std.alpha)
  it <- tibble(
    item   = rownames(a$item.stats),
    r_drop = a$item.stats$r.drop
  )
  list(alpha = alpha_val, item_total = it)
}

# Compute per scale
res.USS   <- alpha_itcorr(safe_select(X, items$USS))
res.SARA  <- alpha_itcorr(safe_select(X, items$SARA))
res.fSARA <- alpha_itcorr(safe_select(X, items$fSARA))
res.ADL   <- alpha_itcorr(safe_select(X, items$ADL))

# -----------------------------------------------------------------------
# 1) Item–total correlations: mean and range per scale
# -----------------------------------------------------------------------
summ_it <- function(it_tbl) {
  if (nrow(it_tbl) == 0) return(tibble(m = NA_real_, rng = NA_character_))
  m   <- mean(it_tbl$r_drop, na.rm = TRUE)
  mn  <- min (it_tbl$r_drop, na.rm = TRUE)
  mx  <- max (it_tbl$r_drop, na.rm = TRUE)
  tibble(m = round(m, 2), rng = paste0(round(mn, 2), "-", round(mx, 2)))
}

t1.it.corr <- bind_rows(
  USS   = summ_it(res.USS$item_total),
  SARA  = summ_it(res.SARA$item_total),
  fSARA = summ_it(res.fSARA$item_total),
  ADL   = summ_it(res.ADL$item_total),
  .id   = "scale"
) %>%
  mutate(
    m   = sprintf("%.2f", m),   # keep 2 dp and make character
    rng = as.character(rng)
  ) %>%
  pivot_longer(-scale, names_to = "stat", values_to = "value") %>%
  pivot_wider(names_from = scale, values_from = value) %>%
  select(stat, USS, SARA, fSARA, ADL)

# -----------------------------------------------------------------------
# 2) Cronbach’s alpha per scale
# -----------------------------------------------------------------------
t4.alpha <- tibble(
  USS   = round(res.USS$alpha,   2),
  SARA  = round(res.SARA$alpha,  2),
  fSARA = round(res.fSARA$alpha, 2),
  ADL   = round(res.fSARA$alpha, 2)
)

# -----------------------------------------------------------------------
# 3) Mean (SD), observed range, skewness of totals
# -----------------------------------------------------------------------

desc_tot <- psych::describe(scores %>% select(USS, SARA, fSARA, ADL))

t2.m.sd.skew <- desc_tot %>%
  as.data.frame() %>%
  rownames_to_column("scale") %>%
  transmute(
    scale,
    mean.sd  = sprintf("%.2f (%.2f)", mean, sd),
    observed = sprintf("%.2f-%.2f",  min,  max),
    skew     = sprintf("%.2f", skew)   # <- make character
  ) %>%
  pivot_longer(-scale, names_to = "stat", values_to = "value") %>%
  pivot_wider(names_from = scale, values_from = value) %>%
  select(stat, USS, SARA, fSARA, ADL)

# -----------------------------------------------------------------------
# 4) Floor / Ceiling effects on totals
# If you have theoretical maxima, put them here; otherwise use observed maxima.
# Example (uncomment & adjust if known):
# scale_max <- c(USS = 36, SARA = 40, fSARA = 40)
# -----------------------------------------------------------------------
# 1) Get theoretical maxima (preferred)
#    Expecting your ../DATA other/scales.txt with columns: paramcd, maxscore
# Get observed maxima
obs_max <- scores %>%
  summarise(across(c(USS, SARA, fSARA, ADL), ~max(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "scale", values_to = "mx")

# Floor / ceiling effects using observed max per scale
t3.flcl <- scores %>%
  select(USS, SARA, fSARA, ADL) %>%
  pivot_longer(everything(), names_to = "scale", values_to = "aval") %>%
  left_join(obs_max, by = "scale") %>%
  group_by(scale) %>%
  summarise(
    floor   = mean(aval <= 0, na.rm = TRUE)         * 100,
    floor10 = mean(aval <= 0.10 * mx, na.rm = TRUE) * 100,
    ceil90  = mean(aval >= 0.90 * mx, na.rm = TRUE) * 100,
    ceil    = mean(aval >= 1.00 * mx, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(across(-scale, ~sprintf("%.1f", .))) %>%
  arrange(factor(scale, levels = c("USS","SARA","fSARA","ADL")))


# -----------------------------------------------------------------------
# 5) Correlations of totals with age / duration / fds
# -----------------------------------------------------------------------
cor_with <- function(df, y_cols, x_cols = c("age","dur","fds")) {
  dd <- df %>% select(all_of(c(x_cols, y_cols))) %>% drop_na()
  if (nrow(dd) < 3) return(matrix(NA_real_, nrow = length(x_cols), ncol = length(y_cols),
                                  dimnames = list(x_cols, y_cols)) %>% as_tibble(rownames = "rowname"))
  M <- suppressWarnings(cor(dd[, y_cols, drop = FALSE], dd[, x_cols, drop = FALSE],
                            use = "pairwise.complete.obs", method = "pearson"))
  # cor() with two matrices returns |Y| x |X|
  as_tibble(t(M), rownames = "rowname")
}

t6.cor.ads <- cor_with(scores, y_cols = c("USS","SARA","fSARA")) %>%
  filter(rowname %in% c("age","dur","fds")) %>%
  rename(USS = USS, SARA = SARA, fSARA = fSARA) %>%
  select(rowname, USS, SARA, fSARA) %>%
  mutate(across(-rowname, ~round(., 2))) %>%
  select(-rowname)  # keep same wide shape as before if you prefer

# -----------------------------------------------------------------------
# OPTIONAL: detailed item-total tables per scale (for inspection)
# -----------------------------------------------------------------------
it.USS   <- res.USS$item_total   %>% mutate(r_drop = round(r_drop, 3))
it.SARA  <- res.SARA$item_total  %>% mutate(r_drop = round(r_drop, 3))
it.fSARA <- res.fSARA$item_total %>% mutate(r_drop = round(r_drop, 3))
it.ADL <- res.ADL$item_total %>% mutate(r_drop = round(r_drop, 3))

# -----------------------------------------------------------------------
# OUTPUT OBJECTS (mirroring your previous naming style)
#   - t0.NRitems      : number of items per scale
#   - t1.it.corr      : item-total mean and range
#   - t2.m.sd.skew    : mean(SD), observed range, skew
#   - t3.flcl         : floor/ceiling percentages
#   - t4.alpha        : Cronbach alpha per scale
#   - t6.cor.ads      : correlations with age/dur/fds
#   - it.*            : item-level r_drop per scale
#   - scores          : subject-level totals (USS/SARA/fSARA) + metadata
# -----------------------------------------------------------------------
