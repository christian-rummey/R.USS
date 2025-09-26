# . -----------------------------------------------------------------------
rm(list = ls())
source('.project.settings.R')

dt. <- readRDS('DATA derived/dt.all.visits.rds') %>% 
  # filter(!is.preataxic) %>% 
  # filter(has.both) %>%
  # filter(!is.nonamb) %>%
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
  filter(dur<10) %>%
  filter(status == 'ambulatory') %>%
  # filter(is.preataxic) %>% 
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
  # filter( paramcd != 'ADL' ) %>% 
  mutate( paramcd = factor(paramcd, labs.)) %>% 
  # mutate(aval = aval/maxscore) %>% 
  
  ggplot()+geom_line(size = 1)+
  aes(x = aval)+
  aes(color = subtype)+
  aes(y = pct_cum)+
  facet_wrap(~paramcd, ncol = 2, scales = 'free_x')+
  guides(color = guide_legend(nrow = 1))+
  .theme(base_size = 14)+.leg('none')+
  labs(color = NULL, y= "Cumulative Proportions of Patients", x = 'Score')

# .sp( ti = 'Figure 3 - ADL vs USS, SARA, f-SARA', l = "F", i = 1)
# .sp( ti = 'Figure 3 - ADL vs USS, SARA, f-SARA')

# . -----------------------------------------------------------------------

# do this by duration groups!
dt.cum %>%
  mutate(subtype = factor(subtype, c('FRDA', 'RFC1', 'SCA27B','SCA1','SCA2','SCA3','SCA6'))) %>% 
  left_join(
    .rt('../../Ataxia/DATA other/scales.txt') %>% 
      select(paramcd, maxscore)
  ) %>% 
  # filter( paramcd != 'ADL' ) %>% 
  mutate( paramcd = factor(paramcd, labs.)) %>% 
  # mutate(aval = aval/maxscore) %>% 
  
  ggplot()+geom_line(size = 1)+
  aes(x = aval)+
  aes(color = paramcd)+scale_color_brewer(palette = 'Set2')+
  aes(y = pct_cum)+
  facet_wrap(~subtype, ncol = 2, scales = 'free_x')+
  guides(color = guide_legend(nrow = 1))+
  labs(color = NULL, y= "Cumulative Proportions of Patients", x = 'Score')

dt.cum %>%
  left_join(
    .rt('../../Ataxia/DATA other/scales.txt') %>% 
      select(paramcd, maxscore)
  ) %>% 
  mutate(
    subtype = factor( subtype,
                      # c('FRDA', 'RFC1', 'SCA27B', ' ', 'SCA1','SCA2','SCA3','SCA6')  # add dummy level
                      c('FRDA', 'RFC1', 'SCA27B', ' ', 'SCA1','SCA2','SCA3','SCA6')  # add dummy level
    ),
    paramcd = factor( paramcd,
                      c('fSARA', 'SARA', 'ADL', 'USS')  # add dummy level
    )
  ) %>%
  # mutate(paramcd = factor(paramcd, labs.)) %>% 
  ggplot() +
  geom_line(size = 1) +
  aes(x = aval, y = pct_cum, color = paramcd) +
  scale_color_brewer(palette = 'Set2') +
  # facet_wrap(~subtype, ncol = 2, scales = 'free_x', drop = F) +
  facet_wrap(~subtype, ncol = 4, scales = 'free_x', drop = F) +
  guides(color = guide_legend(nrow = 2)) +
  .theme()+.leg('tr')+
  labs(color = NULL, y = "Cumulative Proportions of Patients", x = "Score")
# .sp( ti = 'Ceiling by Genotype', l = "F", i = 1)
