
# . -----------------------------------------------------------------------

rm(list = ls())

source('.project.settings.R')

theme_set(
  theme_minimal(base_size = 10) %+replace%
    theme(
      legend.position  = "top",
      legend.box      = "horizontal",
      legend.direction= "horizontal",
      complete         = TRUE
    )
)

dt. <- readRDS('DATA derived/dt.all.visits.rds') %>%
  # filter(subtype == 'RFC1') %>%
  # filter(has.both) %>%
  # filter(is.30ol) %>%
  # filter(can.stand) %>%
  # filter(!is.nonamb) %>%  # Now filtered in 0.DM.USS.Paper.R
  droplevels()

stance.labs. <- c(
  'gait (E7)','feet apart','feet apart,\neyes closed','feet together','feet together,\neyes closed',
  'in tandem', 'on one foot'
)

stance.labs.all <- c(
  'sitting',
  'tandem walk','gait',
  'feet apart','feet apart,\neyes closed','feet together','feet together,\neyes closed',
  'in tandem', 'on one foot')

params.    <- c('fane7', .l.FARS.E[c(2:7)])
params.all <- c('fane1','fane6','fane7', .l.FARS.E[c(2:7)])

# ===== Figure 2: Main figure (subset of items) =====

dt.main <- dt. %>%
  mutate( paramcd = factor(paramcd,
          labels = stance.labs.,
          levels = params.,
          )) %>%
  filter( !is.na(paramcd ) )

# floor values            ----------------------------------------------

dt.tmp <- dt.main %>%
  mutate(aval = floor(aval)) %>%
  .ug

# graph ----------------------------------------------------------------

# Custom palette: light to dark green for 0–4, grey for 5
green_grey_palette <- c(
  "0" = "#e5f5e0",
  "1" = "#a1d99b",
  "2" = "#74c476",
  "3" = "#31a354",
  "4" = "#006d2c",
  "5" = "grey30"
)

x_map <- tibble::tibble(
  paramcd = levels(dt.tmp$paramcd),
  xpos = c(1, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5)
)

lines.df <- dt.tmp %>%
  count(study, paramcd, aval) %>%
  filter((paramcd == "feet apart" & aval == 4) | (paramcd == "gait" & aval == 5)) %>%
  group_by(study, paramcd) %>%
  summarise(yintercept = sum(n), .groups = "drop")


# Add labels for A and B
label_data <- data.frame(
  study = c("CRCSCA", "UNIFAI"),
  label = c("A", "B"),
  x = -Inf,
  y = Inf
)

p1 <- dt.tmp %>%
  left_join(x_map, by = "paramcd") %>%

  ggplot(aes(x = xpos, fill = factor(aval))) +
  geom_bar(color = 'black') +
  scale_fill_manual(values = green_grey_palette) +
  facet_wrap(~study, scales = "free_y", ncol = 2) +
  geom_text(
    data = label_data,
    aes(x = x, y = y, label = label),
    hjust = -0.5, vjust = -0.5,
    size = 12, fontface = "bold", family = "Tenorite",
    inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = x_map$xpos,
    labels = levels(dt.tmp$paramcd)
  ) +
  geom_hline(
    data = lines.df,
    aes(yintercept = yintercept),
    linetype = "dashed",
    color = "darkred",
    size = 1
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))+
  labs(
    fill = "Result",
    x = "Stance Position",
    y = "Number of Participants"
  )+
  .theme()

p1



# number of ptcpants that can not stand -----------------------------------

dt.tmp %>% 
  filter(paramcd == 'feet together,\neyes closed') %>%
  # filter(paramcd == 'in tandem') %>%
  # filter(paramcd == 'feet apart') %>%
  select(study, aval) %>% 
  group_by(study) %>% 
  mutate(N=n()) %>% 
  filter(aval==0) %>% 
  group_by(study, N) %>% 
  summarise(n=n()) %>% 
  mutate(n/N)

dt.tmp %>% 
  # filter(paramcd == 'feet together,\neyes closed') %>%
  # filter(paramcd == 'in tandem') %>%
  filter(paramcd == 'feet apart') %>%
  select(study, aval) %>% 
  group_by(study) %>% 
  mutate(N=n()) %>% 
  filter(aval==4) %>% 
  group_by(study, N) %>% 
  summarise(n=n()) %>% 
  mutate(n/N)

# ===== Supplemental Figure 1: All items =====

dt.all <- dt. %>%
  mutate( paramcd = factor(paramcd,
          labels = stance.labs.all,
          levels = params.all,
          )) %>%
  filter( !is.na(paramcd ) )

dt.tmp.all <- dt.all %>%
  mutate(aval = floor(aval)) %>%
  .ug

x_map.all <- tibble::tibble(
  paramcd = levels(dt.tmp.all$paramcd),
  xpos = 1:length(levels(dt.tmp.all$paramcd))
)

lines.df.all <- dt.tmp.all %>%
  count(study, paramcd, aval) %>%
  filter((paramcd == "feet apart" & aval == 4) | (paramcd == "gait" & aval == 5)) %>%
  group_by(study, paramcd) %>%
  summarise(yintercept = sum(n), .groups = "drop")

p2 <- dt.tmp.all %>%
  left_join(x_map.all, by = "paramcd") %>%

  ggplot(aes(x = xpos, fill = factor(aval))) +
  geom_bar(color = 'black') +
  scale_fill_manual(values = green_grey_palette) +
  facet_wrap(~study, scales = "free_y", ncol = 2) +
  geom_text(
    data = label_data,
    aes(x = x, y = y, label = label),
    hjust = -0.5, vjust = -0.5,
    size = 12, fontface = "bold", family = "Tenorite",
    inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = x_map.all$xpos,
    labels = levels(dt.tmp.all$paramcd)
  ) +
  geom_hline(
    data = lines.df.all,
    aes(yintercept = yintercept),
    linetype = "dashed",
    color = "darkred",
    size = 1
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))+
  labs(
    fill = "Result",
    x = "Stance Position",
    y = "Number of Participants"
  )+
  .theme()

p2

# ===== Export to PowerPoint =====

library(officer)

.ppt.template.file <- "C:\\Users\\ChristianRummey\\OneDrive - CDS\\Projects/_templates/CR.template.pptx"

# fixes minus signs (most often)
.fix_plot_minuses <- function(p) {
  p + labs(
    title    = gsub("-", "\u2212", p$labels$title),
    subtitle = gsub("-", "\u2212", p$labels$subtitle),
    caption  = gsub("-", "\u2212", p$labels$caption),
    x        = gsub("-", "\u2212", p$labels$x),
    y        = gsub("-", "\u2212", p$labels$y)
  )
}

# Export Figure 2
pp1 <- .fix_plot_minuses(p1)
target_file1 <- "3.Figure.Green.Barchart(Figure2).pptx"

ppt1 <- read_pptx(.ppt.template.file) %>%
  add_slide(layout = "F", master = "CR") %>%
  ph_with(
    dml(print(pp1, newpage = FALSE)),
    location = ph_location_type(type = "body", type_idx = 1)
  ) %>%
  ph_with(
    "Figure 2",
    location = ph_location_type(type = "title")
  ) %>%
  set_notes(
    value = paste("Created at", format(Sys.time(), "%Y-%m-%d %H-%M-%S")),
    location = notes_location_type("body")
  ) %>%
  print(target = target_file1)

print(paste("PowerPoint saved as:", target_file1))

# Export Supplemental Figure 1
pp2 <- .fix_plot_minuses(p2)
target_file2 <- "3.Figure.Green.Barchart(SuppFig1).pptx"

ppt2 <- read_pptx(.ppt.template.file) %>%
  add_slide(layout = "F", master = "CR") %>%
  ph_with(
    dml(print(pp2, newpage = FALSE)),
    location = ph_location_type(type = "body", type_idx = 1)
  ) %>%
  ph_with(
    "Supplemental Figure 1",
    location = ph_location_type(type = "title")
  ) %>%
  set_notes(
    value = paste("Created at", format(Sys.time(), "%Y-%m-%d %H-%M-%S")),
    location = notes_location_type("body")
  ) %>%
  print(target = target_file2)

print(paste("PowerPoint saved as:", target_file2))

