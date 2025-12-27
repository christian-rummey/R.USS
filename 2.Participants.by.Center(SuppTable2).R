
# . -----------------------------------------------------------------------

rm(list = ls())

source('.project.settings.R')

dt. <- readRDS('DATA derived/dt.all.visits.rds') 

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

site_table <- dt. %>%
  .ug %>%
  group_by(study, site) %>%
  summarise( s = length(unique(sjid)), n = n(), .groups = 'drop') %>%
  arrange(-s) %>%
  group_by(site) %>% mutate(studies = n()) %>%
  mutate(studies = ifelse(studies==1,NA,'both')) %>%
  .ug %>%
  select(-n) %>% spread (study, s) %>%
  mutate(total.n = rowSums(cbind(CRCSCA, UNIFAI), na.rm = TRUE)) %>%
  select(site, total.n, CRCSCA, UNIFAI) %>%
  arrange(-total.n)

print(site_table)

# by genotype ----------------------------------------------------------

genotype_table <- dt. %>%
  .ug %>%
  group_by(study, sca) %>%
  summarise( s = length(unique(sjid)), n = n(), .groups = 'drop') %>%
  arrange(-s) %>%
  group_by(sca) %>% mutate(studies = n()) %>%
  mutate(studies = ifelse(studies==1,NA,'both')) %>%
  .ug %>%
  select(-n) %>% spread (study, s) %>%
  mutate(total.n = rowSums(cbind(CRCSCA, UNIFAI), na.rm = TRUE)) %>%
  select(sca, total.n, CRCSCA, UNIFAI) %>%
  arrange(-total.n)

print(genotype_table)

# by study subgroups ------------------------------------------------------

study_table <- dt. %>%
  .ug %>%
  filter( sca %in% c('SCA1', 'SCA2', 'SCA3', 'SCA6', 'SCA27B', 'RFC1')) %>%
  group_by(study) %>%
  summarise( s = length(unique(sjid)), n = n(), .groups = 'drop') %>%
  arrange(-s)

print(study_table)

# Export to Word ----------------------------------------------------------

library(flextable)
library(officer)

# Create flextable for site table
site_ft <- site_table %>%
  select( site, CRCSCA, UNIFAI, total.n ) %>% 
  as.data.frame() %>%
  flextable() %>%
  set_header_labels(site = "Center", total.n = "Total N", CRCSCA = "CRC-SCA", UNIFAI = "UNIFAI") %>%
  theme_vanilla() %>%
  fontsize(size = 10, part = "all") %>%
  align(align = "center", part = "header") %>%
  align(j = 1, align = "left", part = "body") %>%
  align(align = "center", part = "body", j = -1) %>%
  autofit()

# Create docx with site table
read_docx() %>%
  body_add_par("Supplemental Table 2: Participants by Center", style = "heading 1") %>%
  body_add_flextable(site_ft) %>%
  print(target = "2.Participants.by.Center(SuppTable2).docx")

print("Table exported to: 2.Participants.by.Center(SuppTable2).docx")





