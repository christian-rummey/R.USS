


# . -----------------------------------------------------------------------

rm(list = ls())

source('.project.settings.R')

dt. <- readRDS('DATA derived/dt.all.visits.rds') %>% 
  filter(has.both) %>%
  filter(!is.nonamb) %>%
  droplevels()

params. <- c('ADL','SARA','FARS.E','fSARA')
labs.   <- c('ADL','SARA','USS','fSARA')

dt. %<>% 
  mutate( paramcd = factor(paramcd, 
                           labels = labs.,
                           levels = params.,
  )) %>% 
  filter( !is.na(paramcd ) )

dt. %>% 
  select( study, sjid, avisitn, aval, starts_with('has') ) %>% 
  filter(study == 'UNIFAI') %>% 
  left_join(.dd('demo') %>% select(study, sjid, site)) %>% 
  .ug %>% 
  group_by(site) %>% 
  summarise( s = length(unique(sjid)), n = n()) %>% 
  arrange(-s)

dt. %>% 
  select( study, sjid, avisitn, aval, starts_with('has') ) %>% 
  filter(study == 'CRCSCA') %>% 
  left_join(.dd('demo.sca') %>% select(study, sjid, site, sca)) %>% 
  filter(sca == 'SCA27B') %>% 
  .ug %>% 
  group_by(site, sca) %>% 
  summarise( s = length(unique(sjid)), n = n()) %>% 
  arrange(-s)



