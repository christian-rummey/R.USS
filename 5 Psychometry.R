# -----------------------------------------------------------------------
# Psychometric properties for USS (FARS.E), SARA, fSARA, ADL
# Baseline (earliest visit per subject), ambulatory cohort
# -----------------------------------------------------------------------
rm(list = ls())
source(".project.settings.R")

suppressPackageStartupMessages({
  library(tidyverse)
  library(psych)
})

# --- config --------------------------------------------------------------
scale_order <- c("USS","SARA","fSARA","ADL")
items <- list(USS = .l.FARS.E, SARA = .l.sara, fSARA = .l.fsara, ADL = .l.adl)

safe_select <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (!length(cols)) tibble(.dummy = numeric(0)) else dplyr::select(df, all_of(cols))
}
alpha_itcorr <- function(df_items) {
  df_items <- as.data.frame(df_items)
  if (!nrow(df_items) || !ncol(df_items)) return(list(alpha = NA_real_, item_total = tibble(item=character(), r_drop=numeric())))
  a <- suppressWarnings(psych::alpha(df_items, check.keys = FALSE, warnings = FALSE))
  list(alpha = unname(a$total$std.alpha),
       item_total = tibble(item = rownames(a$item.stats), r_drop = a$item.stats$r.drop))
}

# --- data ---------------------------------------------------------------
dt. <- readRDS("DATA derived/dt.all.visits.rds") %>%
  filter(!is.preataxic, has.both, !is.nonamb) %>%
  filter(!is.na(subtype), !subtype %in% c("SCA10","SCA7","SCA8")) %>%
  droplevels()

items.keep <- unique(unlist(items, use.names = FALSE))

dt.base <- dt. %>%
  group_by(sjid) %>% filter(avisitn == min(avisitn, na.rm = TRUE)) %>%
  ungroup() %>% filter(paramcd %in% items.keep)

X <- dt.base %>%
  select(sjid, age, dur, fds, paramcd, aval) %>%
  mutate(paramcd = as.character(paramcd)) %>%
  distinct() %>%
  pivot_wider(names_from = paramcd, values_from = aval) %>%
  arrange(sjid)

# --- totals -------------------------------------------------------------
scores <- X %>%
  mutate(
    USS   = rowSums(safe_select(., items$USS),   na.rm = TRUE),
    SARA  = rowSums(safe_select(., items$SARA),  na.rm = TRUE),
    fSARA = rowSums(safe_select(., items$fSARA), na.rm = TRUE),
    ADL   = rowSums(safe_select(., items$ADL),   na.rm = TRUE)
  ) %>%
  relocate(sjid, age, dur, fds, all_of(scale_order))

# --- 0) number of items -------------------------------------------------
t0.NRitems <- tibble(
  USS   = length(items$USS),
  SARA  = length(items$SARA),
  fSARA = length(items$fSARA),
  ADL   = length(items$ADL)
)

# --- item-total + alpha per scale --------------------------------------
res.USS   <- alpha_itcorr(safe_select(X, items$USS))
res.SARA  <- alpha_itcorr(safe_select(X, items$SARA))
res.fSARA <- alpha_itcorr(safe_select(X, items$fSARA))
res.ADL   <- alpha_itcorr(safe_select(X, items$ADL))

it.USS   <- res.USS$item_total   %>% mutate(r_drop = round(r_drop, 3))
it.SARA  <- res.SARA$item_total  %>% mutate(r_drop = round(r_drop, 3))
it.fSARA <- res.fSARA$item_total %>% mutate(r_drop = round(r_drop, 3))
it.ADL   <- res.ADL$item_total   %>% mutate(r_drop = round(r_drop, 3))

t1.it.corr <- bind_rows(
  USS   = {m <- mean(it.USS$r_drop,   na.rm=TRUE); tibble(mean=sprintf("%.2f", m), range=sprintf("%.2f–%.2f", min(it.USS$r_drop,na.rm=T),   max(it.USS$r_drop,na.rm=T)))},
  SARA  = {m <- mean(it.SARA$r_drop,  na.rm=TRUE); tibble(mean=sprintf("%.2f", m), range=sprintf("%.2f–%.2f", min(it.SARA$r_drop,na.rm=T),  max(it.SARA$r_drop,na.rm=T)))},
  fSARA = {m <- mean(it.fSARA$r_drop, na.rm=TRUE); tibble(mean=sprintf("%.2f", m), range=sprintf("%.2f–%.2f", min(it.fSARA$r_drop,na.rm=T), max(it.fSARA$r_drop,na.rm=T)))},
  ADL   = {m <- mean(it.ADL$r_drop,   na.rm=TRUE); tibble(mean=sprintf("%.2f", m), range=sprintf("%.2f–%.2f", min(it.ADL$r_drop,na.rm=T),   max(it.ADL$r_drop,na.rm=T)))},
  .id   = "scale"
) %>%
  pivot_longer(-scale, names_to = "stat", values_to = "value") %>%
  pivot_wider(names_from = scale, values_from = value) %>%
  select(stat, all_of(scale_order))

t4.alpha <- tibble(
  USS   = sprintf("%.2f", round(res.USS$alpha,   2)),
  SARA  = sprintf("%.2f", round(res.SARA$alpha,  2)),
  fSARA = sprintf("%.2f", round(res.fSARA$alpha, 2)),
  ADL   = sprintf("%.2f", round(res.ADL$alpha,   2))
)

# --- 3) descriptives of totals -----------------------------------------
desc_tot <- psych::describe(scores %>% select(all_of(scale_order)))

t2.m.sd.skew <- desc_tot %>%
  as.data.frame() %>% rownames_to_column("scale") %>%
  transmute(
    scale,
    mean_sd  = sprintf("%.2f (%.2f)", mean, sd),
    observed = sprintf("%.0f–%.0f",  min,  max),
    skew     = sprintf("%.2f", skew)
  ) %>%
  pivot_longer(-scale, names_to = "stat", values_to = "value") %>%
  pivot_wider(names_from = scale, values_from = value) %>%
  select(stat, all_of(scale_order))

# --- 4) floor/ceiling with observed maxima ------------------------------
obs_max <- scores %>%
  summarise(across(all_of(scale_order), ~max(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "scale", values_to = "mx")

t3.flcl <- scores %>%
  select(all_of(scale_order)) %>%
  pivot_longer(everything(), names_to = "scale", values_to = "aval") %>%
  left_join(obs_max, by = "scale") %>%
  group_by(scale) %>%
  summarise(
    `Floor (%)`              = mean(aval <= 0,        na.rm = TRUE) * 100,
    `Floor (10th perc, %)`   = mean(aval <= 0.10*mx,  na.rm = TRUE) * 100,
    `Ceiling (90th perc, %)` = mean(aval >= 0.90*mx,  na.rm = TRUE) * 100,
    `Ceiling (%)`            = mean(aval >= 1.00*mx,  na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(across(-scale, ~sprintf("%.0f", .))) %>%
  pivot_longer(-scale, names_to = "stat", values_to = "value") %>%
  pivot_wider(names_from = scale, values_from = value) %>%
  select(stat, all_of(scale_order))

# --- 5) correlations with age/dur/fds -----------------------------------
t6.cor.ads <- scores %>%
  select(age, dur, fds, all_of(scale_order)) %>%
  drop_na() %>%
  { 
    Y <- as.matrix(select(., all_of(scale_order)))
    X <- as.matrix(select(., age, dur, fds))
    M <- suppressWarnings(cor(Y, X, use = "pairwise.complete.obs", method = "pearson"))
    as_tibble(t(round(M, 2)), rownames = "Row")
  } %>%
  mutate(across(all_of(scale_order), ~sprintf("%.2f", .))) %>%
  mutate(Row = recode(Row, age = "Age", dur = "Disease duration", fds = "Disability staging (fds)"))

# --- 6) intercorrelations among totals ----------------------------------
t5.inter <- scores %>%
  select(all_of(scale_order)) %>%
  { suppressWarnings(cor(., use = "pairwise.complete.obs")) } %>%
  round(2) %>%
  as_tibble(rownames = "Row") %>%
  mutate(across(all_of(scale_order), ~sprintf("%.2f", .))) %>%
  { # blank diagonal, keep lower triangle
    df <- .
    for (i in seq_along(scale_order)) for (j in seq_len(i)) df[i, scale_order[j]] <- ifelse(i==j, "", df[i, scale_order[j]][[1]])
    df
  }

# --- Final publication-style table tibble -------------------------------
# tab2 <- bind_rows(
#   
#   tibble(Row = "No. of items",               !!!as.list(setNames(rep("", length(scale_order)), scale_order))),
#   (t0.NRitems %>% mutate(Row = "No. of items") %>% select(Row, all_of(scale_order))),
#   
#   tibble(Row = "Corrected item–total correlations", !!!as.list(setNames(rep("", length(scale_order)), scale_order))),
#   (t1.it.corr %>% mutate(Row = stringr::str_to_sentence(stat)) %>% select(Row, all_of(scale_order))),
#   
#   tibble(Row = "Subscale characteristics",   !!!as.list(setNames(rep("", length(scale_order)), scale_order))),
#   
#   (t2.m.sd.skew %>% mutate(Row = recode(stat, mean_sd="Mean score (SD)", observed="Observed score (max)", skew="Skewness")) %>% select(Row, all_of(scale_order))),
#   
#   (t3.flcl %>% rename(Row = stat) %>% select(Row, all_of(scale_order))),
#   
#   (t4.alpha %>% mutate(Row = "Cronbach α") %>% select(Row, all_of(scale_order))),
#   
#   tibble(Row = "Subscale intercorrelations", !!!as.list(setNames(rep("", length(scale_order)), scale_order))),
#   (bind_rows(tibble(Row = "", !!!as.list(setNames(rep("", length(scale_order)), scale_order))), t5.inter %>% select(Row, all_of(scale_order)))),
#   
#   tibble(Row = "Correlation with other parameters", !!!as.list(setNames(rep("", length(scale_order)), scale_order))),
#   (t6.cor.ads %>% select(Row, all_of(scale_order)))
# )
# 
# # Objects you keep:
# # - scores, it.USS/it.SARA/it.fSARA/it.ADL
# # - t0.NRitems, t1.it.corr, t2.m.sd.skew, t3.flcl, t4.alpha, t5.inter, t6.cor.ads
# # - tab2
