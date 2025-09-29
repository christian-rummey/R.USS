
# . -----------------------------------------------------------------------

rm(list = ls())

source('.project.settings.R')

dt. <- readRDS('DATA derived/dt.all.visits.rds') %>% 
  # filter(has.both) %>% 
  filter(!is.nonamb) %>%
  droplevels()

score.labs. <- c(
  'SARA','f-SARA','USS','ADL'
)

params. <- c('SARA','fSARA','FARS.E','ADL')

dt. %<>% 
  # mutate( paramcd = factor(paramcd, 
  #                          labels = score.labs.,
  #                          levels = params.,
  # )) %>% 
  filter( paramcd %in% params. ) 

# # group_by(sjid, study, avisitn, paramcd) %>% 
# # filter(n()>1)
# # filter(sjid == 4218) %>% .p
# # .ug %>% slice(1283)
# left_join(
#   .dd('steps') %>% select(study, sjid, avisitn, amb) %>% 
#     filter(!is.na(amb)) %>%
#     filter(study %in% c('CRCSCA','UNIFAI')) %>%
#     group_by(sjid, study, avisitn) %>%
#     unique 
#   # %>% 
#   #   filter(n()>1) %>% .ug %>% select(study) %>% .tab
#   )


# floor values            ----------------------------------------------

dt.tmp <- dt. %>%
  # filter(can.stand, is.nonamb) %>%
  # filter(!is.nonamb) %>%
  # filter(is.30ol) %>% 
  mutate(aval = floor(aval)) %>%
  .gs %>% 
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


dt.tmp %>%
  filter(!is.na(subtype)) %>% 
  # filter(subtype == 'SCA27B') %>% 
  # mutate(study = ifelse(subtype == 'SCA27B', paste(study, subtype), study)) %>% 
  # filter(paramcd == 'USS') %>% 
  # left_join(x_map, by = "paramcd") %>%
  .gs %>%
  
  # ggplot(aes(x = aval, fill = study)) +
  # facet_wrap(paramcd~., scales = "free", ncol = 2) +
  ggplot(aes(x = aval, fill = paramcd)) +
  facet_wrap(study~., scales = "free", ncol = 2) +
  geom_density(alpha = .5) +
  # geom_histogram() +
  scale_fill_manual()+.sfbs1 +
  # theme(base_size = 14)+
  # scale_x_continuous(
  #   breaks = x_map$xpos,
  #   labels = levels(dt.tmp$paramcd)
  # ) +
  # geom_hline(
  #   data = lines.df,
  #   aes(yintercept = yintercept),
  #   linetype = "dashed",
  #   color = "darkred",
  #   size = 1
  # ) +
  # guides(fill = guide_legend(nrow = 1, byrow = TRUE))+
  # labs(
  #   fill = "Result",
  #   x = "Stance Position",
  #   y = NULL
  # )+
  .theme()


