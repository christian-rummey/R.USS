# -----------------------------------------------------------------------
# Helper: run your whole pipeline on a filtered dataset and return tab2
# -----------------------------------------------------------------------
make_tab2_for <- function(dt.filter, suffix) {
  
  # (everything from your existing pipe workflow, starting from dt.filter instead of reading dt.)
  
  items <- list(USS = .l.FARS.E, SARA = .l.sara, fSARA = .l.fsara, ADL = .l.adl)
  scale_order <- c("USS","SARA","fSARA","ADL")
  
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
  
  items.keep <- unique(unlist(items, use.names = FALSE))
  
  dt.base <- dt.filter %>%
    group_by(sjid, paramcd) %>% filter(avisitn == min(avisitn, na.rm = TRUE)) %>%
    ungroup() %>% filter(paramcd %in% items.keep)
  
  X <- dt.base %>%
    select(sjid, age, dur, fds, paramcd, aval) %>%
    mutate(paramcd = as.character(paramcd)) %>%
    distinct() %>%
    pivot_wider(names_from = paramcd, values_from = aval) %>%
    arrange(sjid)
  
  scores <- X %>%
    mutate(
      USS   = rowSums(safe_select(., items$USS),   na.rm = F),
      SARA  = rowSums(safe_select(., items$SARA),  na.rm = F),
      fSARA = rowSums(safe_select(., items$fSARA), na.rm = F),
      ADL   = rowSums(safe_select(., items$ADL),   na.rm = F)
    ) %>%
    relocate(sjid, age, dur, fds, all_of(scale_order))
  
  t0.NRitems <- tibble(
    USS   = length(items$USS),
    SARA  = length(items$SARA),
    fSARA = length(items$fSARA),
    ADL   = length(items$ADL)
  )
  
  res.USS   <- alpha_itcorr(safe_select(X, items$USS))
  res.SARA  <- alpha_itcorr(safe_select(X, items$SARA))
  res.fSARA <- alpha_itcorr(safe_select(X, items$fSARA))
  res.ADL   <- alpha_itcorr(safe_select(X, items$ADL))
  
  it.USS   <- res.USS$item_total
  it.SARA  <- res.SARA$item_total
  it.fSARA <- res.fSARA$item_total
  it.ADL   <- res.ADL$item_total
  
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
  
  t5.inter <- scores %>%
    select(all_of(scale_order)) %>%
    { suppressWarnings(cor(., use = "pairwise.complete.obs")) } %>%
    round(2) %>%
    as_tibble(rownames = "Row") %>%
    mutate(across(all_of(scale_order), ~sprintf("%.2f", .))) %>%
    { df <- .; for (i in seq_along(scale_order)) for (j in seq_len(i)) df[i, scale_order[j]] <- ifelse(i==j, "", df[i, scale_order[j]][[1]]); df }
  
  # --- build simple tibble (like tab2) ---
  header <- function(txt) tibble(Row = txt) %>% add_column(!!!setNames(rep("", length(scale_order)), scale_order))
  cast_chr <- function(df) df %>% mutate(across(all_of(scale_order), as.character))
  
  # Compute number of non-NA rows per scale
  t0.Nobs <- scores %>%
    summarise(across(all_of(scale_order), ~sum(!is.na(.)))) %>%
    mutate(Row = "Number of observations") %>%
    select(Row, all_of(scale_order)) %>%
    mutate(across(everything(), as.character))  # cast to character to match other rows
  
  rows <- list(
    header("Observations"),
    t0.Nobs,
    t0.NRitems %>% mutate(across(everything(), as.character), Row = "No. of items") %>% select(Row, all_of(scale_order)),
    header("Corrected item–total correlations"),
    t1.it.corr %>% mutate(Row = stringr::str_to_sentence(stat)) %>% select(Row, all_of(scale_order)),
    header("Subscale characteristics"),
    t2.m.sd.skew %>% mutate(Row = recode(stat, mean_sd="Mean score (SD)", observed="Observed score (max)", skew="Skewness")) %>% select(Row, all_of(scale_order)),
    t3.flcl %>% rename(Row = stat) %>% select(Row, all_of(scale_order)),
    t4.alpha %>% mutate(Row = "Cronbach α") %>% select(Row, all_of(scale_order)),
    header("Subscale intercorrelations"),
    bind_rows(header(""), t5.inter %>% select(Row, all_of(scale_order))),
    header("Correlation with other parameters"),
    t6.cor.ads %>% select(Row, all_of(scale_order))
  )
  
  tab2 <- rows %>% map(~cast_chr(.x)) %>% bind_rows()
  
  # rename scale columns with suffix
  tab2 %>% rename_with(~paste0(.x, ".", suffix), .cols = scale_order)
}

# read dataset ------------------------------------------------------------

source('.project.settings.R')
scale_order <- c("USS","SARA","fSARA","ADL")
items       <- list(USS = .l.FARS.E, SARA = .l.sara, fSARA = .l.fsara, ADL = .l.adl)
items.keep  <- unique(unlist(items, use.names = FALSE))

dt. <- readRDS("DATA derived/dt.all.visits.rds") %>%
  # filter( !is.preataxic  ) %>%
  # filter(  has.both      ) %>%
  filter( !is.nonamb     ) %>%
  # filter( !is.na(subtype), !subtype %in% c("SCA10","SCA7","SCA8")) %>%
  filter(paramcd %in% items.keep) %>% 
  droplevels()

# params. <- c('FARS.E','SARA','fSARA','ADL')
# labs.   <- c('USS'   ,'SARA','fSARA','ADL')

dt. %<>% 
  group_by(sjid, paramcd) %>% 
  filter(avisitn == min(avisitn))

# dt. %<>%
#   filter(is.30ol)

# -----------------------------------------------------------------------
# Run for each subset
# -----------------------------------------------------------------------



tab2.SCA  <- make_tab2_for(dt. %>% filter(study == "CRCSCA"), "SCA")
tab2.FRDA <- make_tab2_for(dt. %>% filter(study == "UNIFAI"), "FRDA")

dt. %>% .ug %>% 
  filter(paramcd == 'fane7') %>% 
  select(fds, study ) %>% 
  .tab


# join side-by-side on Row
tab2.combined <- bind_cols(tab2.SCA, tab2.FRDA) %>% 
  select(-Row...6) %>% 
  select(
    Property = Row...1,
    starts_with('USS'),
    starts_with('SARA'),
    starts_with('fSARA'),
    starts_with('ADL')
    ) %>% 
  slice(-1) %>% 
  filter(Property != '') 
tab2.combined %>%
  .p
  
