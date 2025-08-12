# . -----------------------------------------------------------------------
rm(list = ls())
source('.project.settings.R')

dt. <- readRDS('DATA derived/dt.all.visits.rds') %>% 
  filter(!is.preataxic) %>% 
  filter(has.both) %>%
  filter(!is.nonamb) %>%
  droplevels()

params. <- c('FARS.E','SARA','fSARA','ADL')
labs.   <- c('USS'   ,'SARA','fSARA','ADL')

dt. %<>% 
  mutate( paramcd = factor(paramcd, 
                           labels = labs.,
                           levels = params.,
  )) %>% 
  filter( !is.na(paramcd ) )

dt. %>% filter(!has.USS)

dt. %<>% 
  group_by(avisitn, paramcd) %>% 
  filter(avisitn == min(avisitn))

dt. %<>% 
  # filter(study == 'CRCSCA') %>% 
  filter(!is.na(subtype)) %>% 
  filter(!subtype %in% c('SCA10','SCA7', 'SCA8'))

# . -----------------------------------------------------------------------

dt.cum <- dt. %>%
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
dt.cum %>%
  left_join(
    .rt('../../Ataxia/DATA other/scales.txt') %>% 
      select(paramcd, maxscore)
  ) %>% 
  filter( paramcd != 'ADL' ) %>% 
  mutate( paramcd = factor(paramcd, labs.)) %>% 
  # mutate(aval = aval/maxscore) %>% 
  
  ggplot()+geom_line(size = 1)+
  aes(x = aval)+
  aes(color = subtype)+
  aes(y = pct_cum)+
  facet_wrap(~paramcd, ncol = 3, scales = 'free_x')+
  guides(color = guide_legend(nrow = 1))+
  labs(color = NULL, y= "Cumulative Proportions of Patients", x = 'Score')

# .sp( ti = 'Figure 3 - ADL vs USS, SARA, f-SARA', l = "F", i = 1)

# . -----------------------------------------------------------------------
