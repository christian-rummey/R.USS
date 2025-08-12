# -----------------------------------------------------------------------
# TABLE 2 — Scaling characteristics for USS, SARA, fSARA, ADL
# -----------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse); library(psych); library(flextable); library(officer)
})

# --- helpers -------------------------------------------------------------
ch <- function(x) if (is.null(x)) NA_character_ else as.character(x)
fmt_num <- function(x, d = 2) ifelse(is.na(x), "", sprintf(paste0("%.", d, "f"), x))

scale_order <- c("USS","SARA","fSARA","ADL")

# Ensure ADL present in scores (as single total variable)
if (!"ADL" %in% names(scores)) {
  # Try to pull directly from wide X if present
  if (exists("X") && "ADL" %in% names(X)) scores$ADL <- X$ADL else scores$ADL <- NA_real_
}

# Recompute descriptive block including ADL
desc_tot <- psych::describe(scores %>% select(all_of(scale_order)))

t2.m.sd.skew <- desc_tot %>%
  as.data.frame() %>% rownames_to_column("scale") %>%
  transmute(
    scale,
    mean_sd  = sprintf("%.2f (%.2f)", mean, sd),
    observed = sprintf("%.0f–%.0f", min, max),
    skew     = sprintf("%.2f", skew)
  )

# Floors/ceilings (use observed maxima)
obs_max <- scores %>% summarise(across(all_of(scale_order), ~max(.x, na.rm = TRUE)))
t3.flcl <- scores %>%
  select(all_of(scale_order)) %>%
  pivot_longer(everything(), names_to = "scale", values_to = "aval") %>%
  left_join(pivot_longer(obs_max, everything(), names_to = "scale", values_to = "mx"), by = "scale") %>%
  group_by(scale) %>%
  summarise(
    floor    = mean(aval <= 0, na.rm = TRUE) * 100,
    floor10  = mean(aval <= 0.10 * mx, na.rm = TRUE) * 100,
    ceil90   = mean(aval >= 0.90 * mx, na.rm = TRUE) * 100,
    ceil     = mean(aval >= 1.00 * mx, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(across(-scale, ~sprintf("%.0f", .)))

# Item–total summaries (only for itemized scales)
summ_it <- function(it_tbl) {
  if (!is.null(it_tbl) && nrow(it_tbl)) {
    m  <- round(mean(it_tbl$r_drop, na.rm = TRUE), 2)
    mn <- round(min (it_tbl$r_drop, na.rm = TRUE), 2)
    mx <- round(max (it_tbl$r_drop, na.rm = TRUE), 2)
    tibble(mean = sprintf("%.2f", m), range = sprintf("%.2f–%.2f", mn, mx))
  } else tibble(mean = "", range = "")
}

t1.it <- bind_rows(
  USS   = summ_it(it.USS),
  SARA  = summ_it(it.SARA),
  fSARA = summ_it(it.fSARA),
  ADL   = tibble(mean = "", range = ""),
  .id   = "scale"
)

# No. of items (ADL blank unless you have an item list)
t0.NRitems <- tibble(
  scale = scale_order,
  n_items = c(length(.l.FARS.E), length(.l.sara), length(.l.fsara), "")
)

# Cronbach alpha (ADL blank)
t4.alpha <- tibble(
  scale = scale_order,
  alpha = c(
    ifelse(is.null(res.USS$alpha),  NA, sprintf("%.2f", res.USS$alpha)),
    ifelse(is.null(res.SARA$alpha), NA, sprintf("%.2f", res.SARA$alpha)),
    ifelse(is.null(res.fSARA$alpha),NA, sprintf("%.2f", res.fSARA$alpha)),
    ""
  )
)

# Subscale intercorrelations (matrix among totals)
tot_mat <- scores %>% select(all_of(scale_order))
cor_tot <- suppressWarnings(cor(tot_mat, use = "pairwise.complete.obs", method = "pearson")) %>%
  round(2)
t5.inter <- as_tibble(cor_tot, rownames = "rowname") %>%
  filter(rowname %in% scale_order) %>%
  mutate(across(all_of(scale_order), ~sprintf("%.2f", .)))

# Correlation with age / duration / fds (fallback to stage)
have_fds <- "fds" %in% names(scores)
xvars <- c("dur","age", if (have_fds) "fds" else "stage")
lab_stage <- if (have_fds) "Disability staging (fds)" else "Disability staging"

cors_ext <- function(var) {
  v <- scores[[var]]
  out <- sapply(scale_order, function(s) suppressWarnings(cor(scores[[s]], v, use="pairwise.complete.obs")))
  tibble(var = var, USS = out[1], SARA = out[2], fSARA = out[3], ADL = out[4])
}
t6.cor.ads <- bind_rows(lapply(xvars, cors_ext)) %>%
  mutate(across(-var, ~sprintf("%.2f", .))) %>%
  mutate(var = recode(var, dur = "Disease duration", age = "Age", fds = lab_stage, stage = lab_stage))

# -----------------------------------------------------------------------
# Assemble single table with section headers (like the example)
# -----------------------------------------------------------------------

# a) header rows
H1 <- tibble(Row = "No. of items",       USS = "", SARA = "", fSARA = "", ADL = "")
H2 <- tibble(Row = "Corrected item–total correlations", USS = "", SARA = "", fSARA = "", ADL = "")
H3 <- tibble(Row = "Subscale characteristics", USS = "", SARA = "", fSARA = "", ADL = "")
H4 <- tibble(Row = "Subscale intercorrelations", USS = "", SARA = "", fSARA = "", ADL = "")
H5 <- tibble(Row = "Correlation with other parameters", USS = "", SARA = "", fSARA = "", ADL = "")

# b) blocks
B_items <- t0.NRitems %>% pivot_wider(names_from = scale, values_from = n_items) %>%
  mutate(Row = "No. of items") %>% select(Row, all_of(scale_order))

B_itcorr <- t1.it %>% pivot_longer(-scale, names_to = "stat") %>%
  pivot_wider(names_from = scale, values_from = value) %>%
  mutate(Row = stringr::str_to_sentence(stat)) %>% select(Row, all_of(scale_order))

B_stats <- t2.m.sd.skew %>%
  pivot_longer(-scale, names_to = "stat") %>%
  pivot_wider(names_from = scale, values_from = value) %>%
  mutate(Row = case_when(
    stat == "mean_sd"  ~ "Mean score (SD)",
    stat == "observed" ~ "Observed score (max)",
    stat == "skew"     ~ "Skewness",
    TRUE ~ stat
  )) %>%
  select(Row, all_of(scale_order))

B_flcl <- t3.flcl %>% pivot_longer(-scale, names_to = "stat") %>%
  pivot_wider(names_from = scale, values_from = value) %>%
  mutate(Row = case_when(
    stat == "floor"   ~ "Floor (%)",
    stat == "floor10" ~ "Floor (10th percentile, %)",
    stat == "ceil90"  ~ "Ceiling (90th percentile, %)",
    stat == "ceil"    ~ "Ceiling (%)",
    TRUE ~ stat
  )) %>% select(Row, all_of(scale_order))

B_alpha <- t4.alpha %>% pivot_wider(names_from = scale, values_from = alpha) %>%
  mutate(Row = "Cronbach α") %>% select(Row, all_of(scale_order))

# Intercorrelations: show lower triangle (diag blank)
Lt <- t5.inter
for (i in seq_along(scale_order)) {
  for (j in seq_len(i)) Lt[i, scale_order[j]] <- ifelse(i==j, "", Lt[i, scale_order[j]][[1]])
}
B_inter <- bind_rows(
  tibble(Row = "", USS = "", SARA = "", fSARA = "", ADL = ""),
  Lt %>% rename(Row = rowname) %>% select(Row, all_of(scale_order))
)

# External correlations
B_ext <- t6.cor.ads %>% rename(Row = var) %>% select(Row, all_of(scale_order))

# Final table
tab2 <- bind_rows(
  H1, B_items,
  H2, B_itcorr,
  H3, B_stats, B_flcl, B_alpha,
  H4, B_inter,
  H5, B_ext
)

# --- Flextable formatting (compact, resembles journal layout) -----------
ft <- flextable(tab2) |>
  theme_vanilla() |>
  fontsize(part = "all", size = 9) |>
  bold(i = ~ Row %in% c("No. of items",
                        "Corrected item–total correlations",
                        "Subscale characteristics",
                        "Subscale intercorrelations",
                        "Correlation with other parameters"),
       part = "body") |>
  align(align = "center", part = "all") |>
  align(j = "Row", align = "left", part = "all") |>
  autofit()

ft
