# . -----------------------------------------------------------------------
rm(list = ls())
source('.project.settings.R')

dt. <- readRDS('DATA derived/dt.all.visits.rds') %>%
  # filter(!is.preataxic) %>%
  # filter(!is.nonamb) %>%  # Now filtered in 0.DM.USS.Paper.R
  mutate( status = ifelse(is.preataxic, 'preataxic', ifelse(is.nonamb, 'non-ambualtory', 'ambulatory' ))) %>%
  droplevels()

params. <- c('FARS.E','SARA','fSARA','ADL')
labs.   <- c('USS'   ,'SARA','fSARA','ADL')

dt. %<>% 
  mutate( paramcd = factor(paramcd, 
                           labels = labs.,
                           levels = params.,
  )) %>% 
  filter( !is.na(paramcd ) )

# dt. %>% filter(!has.USS)

dt. %<>% 
  group_by(avisitn, paramcd) %>% 
  filter(avisitn == min(avisitn))

dt. %<>% 
  # filter(study == 'CRCSCA') %>% 
  filter(!is.na(subtype)) %>% 
  filter(!subtype %in% c('SCA10','SCA7', 'SCA8'))

# . -----------------------------------------------------------------------

dt.cum <- dt. %>%
  mutate(aval = round(aval,0)) %>% 
  group_by(sjid, paramcd) %>% filter(avisitn == min(avisitn)) %>% ungroup %>% 
  filter( paramcd %in% c('SARA','USS','fSARA', 'ADL') ) %>%
  
  mutate( paramcd = factor(paramcd, labs.)) %>% 
  mutate( subtype = ifelse(study == 'UNIFAI', 'FRDA',subtype)) %>% 
  group_by( study, subtype, paramcd ) %>% 
  # Count how many patients have each aval value
  count(aval, name = "n") %>%
  # Arrange by increasing aval
  arrange(aval) %>%
  # Calculate cumulative sum
  mutate(
    cum_n = cumsum(n),
    pct_cum = cum_n / sum(n) * 100
  )

# do this by duration groups!
dt.cum %<>%
  left_join(
    .rt('../../Ataxia/DATA other/scales.txt') %>% 
      select(paramcd, maxscore) %>% 
      mutate(paramcd = ifelse (paramcd == 'FARS.E', 'USS', paramcd))
  ) %>% 
  # filter( paramcd != 'ADL' ) %>% 
  mutate( paramcd = factor(paramcd, labs.)) %>% 
  mutate( pct.score = 100*(aval/maxscore)) %>% 
  arrange(-pct.score)


xlims <- tibble(
  paramcd = c("USS", "SARA", "fSARA",'ADL'),
  xmax    = c(36, 40, 16, 36)
)

order.scales    <- c("USS", "SARA", "fSARA", 'ADL')
order.genotypes <- c("SCA1", "SCA2", "SCA3", "SCA6", "SCA27B", "RFC1", 'FRDA')

# Option to show percentage axis
show_percentage_axis <- FALSE

dt.cum2 <- dt.cum %>%
  left_join(xlims, by = "paramcd") %>%
  mutate(
    paramcd = factor(paramcd, levels = order.scales),
    subtype = factor(subtype, levels = order.genotypes)
  )

# Create breaks for vertical lines at 25%, 50%, 75%, 100% per facet
vline_data <- dt.cum2 %>%
  distinct(paramcd, maxscore) %>%
  crossing(pct = c(25, 50, 75, 100)) %>%
  mutate(score_value = maxscore * pct / 100)

# Create labels for 25th, 50th, 75th percentile lines
vline_labels <- vline_data %>%
  filter(pct %in% c(25, 50, 75)) %>%
  distinct(paramcd, pct, score_value) %>%
  mutate(
    label = paste0(pct, "th"),
    y = 5
  )

# Add facet labels A, B, C, D
label_data <- data.frame(
  paramcd = factor(c("USS", "SARA", "fSARA", "ADL"), levels = order.scales),
  label = c("A", "B", "C", "D"),
  x = -Inf,
  y = Inf
)

# Calculate overall (all patients) cumulative data
dt.cum.all <- dt. %>%
  mutate(aval = round(aval,0)) %>%
  group_by(sjid, paramcd) %>% filter(avisitn == min(avisitn)) %>% ungroup %>%
  filter( paramcd %in% c('SARA','USS','fSARA', 'ADL') ) %>%
  mutate( paramcd = factor(paramcd, labs.)) %>%
  group_by( paramcd ) %>%
  count(aval, name = "n") %>%
  arrange(aval) %>%
  mutate(
    cum_n = cumsum(n),
    pct_cum = cum_n / sum(n) * 100
  ) %>%
  mutate( paramcd = factor(paramcd, levels = order.scales))

p <- ggplot(dt.cum2, aes(x = aval, y = pct_cum, color = subtype)) +
  geom_line(size = 0.55) +
  geom_vline(data = vline_data, aes(xintercept = score_value),
             linetype = "dashed", alpha = 0.3, color = "grey50") +
  geom_line(data = dt.cum.all, aes(x = aval, y = pct_cum),
            color = "black", size = 1, inherit.aes = FALSE) +
  geom_text(
    data = vline_labels,
    aes(x = score_value, y = y, label = label),
    color = "grey50", size = 3, hjust = -0.1, vjust = 0,
    family = "Tenorite", inherit.aes = FALSE
  ) +
  geom_text(
    data = label_data,
    aes(x = x, y = y, label = label),
    hjust = -0.5, vjust = -0.5,
    size = 9, fontface = "bold", family = "Tenorite",
    inherit.aes = FALSE
  ) +
  facet_wrap(~paramcd, ncol = 2, scales = "free_x") +
  labs(y = "Cumulative Proportions of Patients (%)",
       x = "Score", color = "Genotype") +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  # scale_color_brewer(palette = "Set1") +
  # scale_color_manual(values = c(
  #   "#E69F00", "#56B4E9", "#009E73",
  #   "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
  # )) +
  guides(color = guide_legend(nrow = 1)) +
  geom_hline(yintercept = 50) +
  coord_cartesian(ylim = c(0, 100)) +
  .theme(base_size = 14)

# Conditionally add secondary axis
if (show_percentage_axis) {
  p <- p + scale_x_continuous(
    sec.axis = sec_axis(~ . / max(.) * 100,
                        name = "% of Maximum Score",
                        breaks = c(0, 25, 50, 75, 100))
  )
}

p

# Calculate at which score/percentage each genotype reaches 90% cumulative
ceiling_90pct <- dt.cum2 %>%
  group_by(paramcd, subtype) %>%
  filter(pct_cum >= 90) %>%
  slice_min(pct_cum, n = 1) %>%
  ungroup() %>%
  select(paramcd, subtype, aval, pct.score, pct_cum) %>%
  rename(
    Score = aval,
    `% of Max Score` = pct.score,
    `Cumulative %` = pct_cum
  )

print("Score at which 90% of patients are reached by genotype:")
ceiling_90pct %>%
  arrange(paramcd, subtype) %>%
  print(n = Inf)

# Summary statistics to identify floor effects
print("\nSummary: 90th percentile score by genotype and scale")
ceiling_90pct %>%
  select(paramcd, subtype, `% of Max Score`) %>%
  pivot_wider(names_from = paramcd, values_from = `% of Max Score`) %>%
  arrange(USS) %>%
  print(n = Inf)

# Look at the actual data distribution
print("\nData preview - first and last observations by genotype/scale:")
dt.cum2 %>%
  group_by(paramcd, subtype) %>%
  slice(c(1, n())) %>%
  select(paramcd, subtype, aval, pct.score, pct_cum, n) %>%
  arrange(paramcd, subtype) %>%
  print(n = Inf)

# Export to PowerPoint
library(officer)

.ppt.template.file <- "C:\\Users\\ChristianRummey\\OneDrive - CDS\\Projects/_templates/CR.template.pptx"

# fixes minus signs
.fix_plot_minuses <- function(p) {
  p + labs(
    title    = gsub("-", "\u2212", p$labels$title),
    subtitle = gsub("-", "\u2212", p$labels$subtitle),
    caption  = gsub("-", "\u2212", p$labels$caption),
    x        = gsub("-", "\u2212", p$labels$x),
    y        = gsub("-", "\u2212", p$labels$y)
  )
}

pp <- .fix_plot_minuses(p)

# Create second plot with rescaled individual facets
# Filter vlines to only show within actual data range per facet
vline_data_rescaled <- dt.cum2 %>%
  group_by(paramcd) %>%
  summarise(min_aval = min(aval), max_aval = max(aval), .groups = 'drop') %>%
  left_join(dt.cum2 %>% distinct(paramcd, maxscore), by = "paramcd") %>%
  crossing(pct = c(25, 50, 75, 100)) %>%
  mutate(score_value = maxscore * pct / 100) %>%
  filter(score_value >= min_aval & score_value <= max_aval)

# Create labels for rescaled version
vline_labels_rescaled <- vline_data_rescaled %>%
  filter(pct %in% c(25, 50, 75)) %>%
  distinct(paramcd, pct, score_value) %>%
  mutate(
    label = paste0(pct, "th"),
    y = 5
  )

p_rescaled <- ggplot(dt.cum2, aes(x = aval, y = pct_cum, color = subtype)) +
  geom_line(size = 0.55) +
  geom_vline(data = vline_data_rescaled, aes(xintercept = score_value),
             linetype = "dashed", alpha = 0.3, color = "grey50") +
  geom_line(data = dt.cum.all, aes(x = aval, y = pct_cum),
            color = "black", size = 1, inherit.aes = FALSE) +
  geom_text(
    data = vline_labels_rescaled,
    aes(x = score_value, y = y, label = label),
    color = "grey50", size = 3, hjust = -0.1, vjust = 0,
    family = "Tenorite", inherit.aes = FALSE
  ) +
  geom_text(
    data = label_data,
    aes(x = x, y = y, label = label),
    hjust = -0.5, vjust = -0.5,
    size = 9, fontface = "bold", family = "Tenorite",
    inherit.aes = FALSE
  ) +
  facet_wrap(~paramcd, ncol = 2, scales = "free_x") +
  labs(y = "Cumulative Proportions of Patients (%)",
       x = "Score", color = "Genotype") +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  guides(color = guide_legend(nrow = 1)) +
  geom_hline(yintercept = 50) +
  coord_cartesian(ylim = c(0, 100)) +
  .theme(base_size = 14) +
  theme(axis.text.y = element_text(color = ifelse(
    rep(c(TRUE, FALSE), length.out = 4),
    "black", "transparent"
  )))

pp_rescaled <- .fix_plot_minuses(p_rescaled)

target_file <- "7.Genotype.Specific.Ceiling.Effects(Figure3).pptx"

ppt <- read_pptx(.ppt.template.file) %>%
  add_slide(layout = "TTE", master = "CR") %>%
  ph_with(
    dml(print(pp_rescaled, newpage = FALSE)),
    location = ph_location_type(type = "body", type_idx = 1)
  ) %>%
  ph_with(
    "Genotype Specific Ceiling Effects (Rescaled Axes)",
    location = ph_location_type(type = "title")
  ) %>%
  set_notes(
    value = paste("Created at", format(Sys.time(), "%Y-%m-%d %H-%M-%S")),
    location = notes_location_type("body")
  ) %>%
  add_slide(layout = "TTE", master = "CR") %>%
  ph_with(
    dml(print(pp, newpage = FALSE)),
    location = ph_location_type(type = "body", type_idx = 1)
  ) %>%
  ph_with(
    "Genotype Specific Ceiling Effects",
    location = ph_location_type(type = "title")
  ) %>%
  set_notes(
    value = paste("Created at", format(Sys.time(), "%Y-%m-%d %H-%M-%S")),
    location = notes_location_type("body")
  ) %>%
  print(target = target_file)

print(paste("PowerPoint saved as:", target_file))
