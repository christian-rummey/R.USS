
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
  filter(!is.na(subtype)) %>% .gs %>% 
  ggplot(aes(x = aval, fill = paramcd)) +
  facet_wrap(study~., scales = "free", ncol = 2) +
  geom_density(alpha = .5) +
  .sfbs1 +
  .theme()


dt.tmp %>%
  filter(subtype %in% levels(factor(dt.tmp$subtype))[c(1,2,3,4,5,6,8,9,10,11)]) %>% 
  filter(paramcd %in% c('SARA','FARS.E')) %>% 
  filter(!is.na(subtype)) %>% .gs %>%
  
  ggplot() +geom_density(alpha = .5) +
  .sfbs1 +
  aes(x = aval, fill = paramcd)+
  facet_wrap(~subtype) +
  # .sfbs1 +
  .theme()


