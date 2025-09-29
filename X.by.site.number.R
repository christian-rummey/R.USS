
# . -----------------------------------------------------------------------

rm(list = ls())

source('.project.settings.R')

dt. <- readRDS('DATA derived/dt.all.visits.rds') %>% 
  filter(!is.nonamb) 

dt. <- bind_rows(
  dt. %>% 
    select( study, sjid, avisitn ) %>% unique %>% 
    filter(study == 'UNIFAI') %>% 
    left_join(.dd('demo') %>% select(study, sjid, site)), 
  dt. %>% 
    select( study, sjid, avisitn ) %>% unique %>% 
    filter(study == 'CRCSCA') %>%
    left_join(.dd('demo.sca') %>% select(study, sjid, site, sca))
)

# by_site_number ----------------------------------------------------------

dt. %>% 
  .ug %>% 
  group_by(study, site) %>% 
  summarise( s = length(unique(sjid)), n = n()) %>% 
  arrange(-s) %>% 
  group_by(site) %>% mutate(studies = n()) %>% 
  mutate(studies = ifelse(studies==1,NA,'both')) %>% 
  .ug %>% 
  select(-n) %>% spread (study, s) %>% 
  mutate(total.n = rowSums(cbind(CRCSCA, UNIFAI), na.rm = TRUE)) %>% 
  select(site, total.n, CRCSCA, UNIFAI) %>% 
  arrange(-total.n) %>% .p


# by genotype ----------------------------------------------------------

dt. %>% 
  .ug %>% 
  group_by(study, sca) %>% 
  summarise( s = length(unique(sjid)), n = n()) %>% 
  arrange(-s) %>% 
  group_by(sca) %>% mutate(studies = n()) %>% 
  mutate(studies = ifelse(studies==1,NA,'both')) %>% 
  .ug %>% 
  select(-n) %>% spread (study, s) %>% 
  mutate(total.n = rowSums(cbind(CRCSCA, UNIFAI), na.rm = TRUE)) %>% 
  select(sca, total.n, CRCSCA, UNIFAI) %>% 
  arrange(-total.n) %>% .p

# by study subgroups ------------------------------------------------------

dt. %>% 
  filter(!is.n)
  .ug %>% 
  filter( sca %in% c('SCA1', 'SCA2', 'SCA3', 'SCA6', 'SCA27B', 'RFC1')) %>% 
  group_by(study) %>% 
  summarise( s = length(unique(sjid)), n = n())
%>% 
  arrange(-s) %>% 
  group_by(study) %>% mutate(studies = n()) %>% 
  mutate(total.n = rowSums(cbind(CRCSCA, UNIFAI), na.rm = TRUE)) %>% 
  select(study, total.n, CRCSCA, UNIFAI) %>% 
  arrange(-total.n) %>% .p





