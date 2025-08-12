
# . -----------------------------------------------------------------------

rm(list = ls())

source('.project.settings.R')

dt. <- readRDS('DATA derived/dt.all.visits.rds') %>% 
  # filter(has.both) %>% 
  filter(is.nonamb) %>%
  droplevels()

stance.labs. <- c(
  'gait (E7)','feet apart','feet apart,\neyes closed','feet together','feet together,\neyes closed',
  'in tandem', 'on one foot'
)

params. <- c('fane7', .l.FARS.E[c(2:7)])

dt. %<>% 
  mutate( paramcd = factor(paramcd, 
          labels = stance.labs.,
          levels = params.,
          )) %>% 
  filter( !is.na(paramcd ) ) %>% 
  left_join(.dd('steps') %>% select(study, sjid, avisitn, amb))

# selection ---------------------------------------------------------------

dt.tmp <- dt. %>%
  mutate(aval = floor(aval)) %>%
  .gs %>% 
  # filter(avisitx != 0 & (avisitn == min(avisitn)))
  filter(avisitn == min(avisitn)) %>%
  # filter   (study != 'FACOMS') %>%
  # group_by(sjid) %>% filter(adt == min(adt)) %>%
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
  filter((paramcd == "feet apart" & aval == 4) | (paramcd == "gait (E7)" & aval == 5)) %>%
  group_by(study, paramcd) %>%
  summarise(yintercept = sum(n), .groups = "drop")


dt.tmp %>%
  left_join(x_map, by = "paramcd") %>%
  .gs %>%
  # mutate(aval = round(aval)) %>%
  
  ggplot(aes(x = xpos, fill = factor(aval))) +
  geom_bar(color = 'black') +
  scale_fill_manual(values = green_grey_palette) +
  facet_wrap(~study, scales = "free_y", ncol = 2) +
  theme(base_size = 14)+
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
    fill = "Result (4 = Unable to Stand)",
    x = "Stance Position",
    y = NULL
  )

# .sp(l = 'F', i = 1)
