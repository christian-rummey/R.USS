
# . -----------------------------------------------------------------------

rm(list = ls())

source('.project.settings.R')

dt. <- readRDS('DATA derived/dt.all.visits.rds') %>% 
  # filter(has.both) %>% 
  # filter(is.nonamb) %>%
  droplevels()

params. <- c('ADL','SARA','FARS.E','fSARA')
labs.   <- c('ADL','SARA','USS','fSARA')

dt. %<>% 
  mutate( paramcd = factor(paramcd, 
                           labels = labs.,
                           levels = params.,
  )) %>% 
  filter( !is.na(paramcd ) )

# no flooring in corelations! ---------------------------------------------

# dt. %<>%
#   mutate(aval = floor(aval)) %>%
#   .gs %>% 
#   # filter(avisitx != 0 & (avisitn == min(avisitn)))
#   # filter(avisitn == min(avisitn)) %>%
#   .ug

# . -----------------------------------------------------------------------

dt. %>%
  select( study, sjid, avisitx, paramcd, aval ) %>% 
  spread( paramcd, aval ) %>% 
  filter( !is.na(USS) ) %>% 
  unique() %>% 
  .ug %>% 
  select( study, avisitx ) %>% .tab

dt. %<>% 
  .gsv %>% 
  spread( paramcd, aval ) 


# SARA, fSARA //SARA.ax/app vs ADL ---------------------------------------------

dt.tmp <- dt. %>% 
  gather( paramcd, aval, USS, SARA, fSARA ) %>% 
  filter(has.both) %>%
  .gs %>% filter(avisitn == min(avisitn)) %>% ungroup %>% 
  filter(!is.na(aval)) %>% 
  filter( paramcd %in% c('USS','SARA','fSARA')) %>%
  mutate( paramcd = factor(paramcd, c('USS','SARA','fSARA')))

dt.tmp %>% 
  ggplot()+geom_point()+
  aes ( y = ADL, x = aval )+
  aes ( color = is.nonamb )+ggsci::scale_color_d3()+
  facet_wrap( ~paste(study, paramcd, sep=', '), scales = 'free_x', ncol = 3 )+
  ggpmisc::stat_correlation(
    aes(label = paste(after_stat(rr.label))),
        size =  10 / .pt,
        family = theme_get()$text$family, 
    data = dt.tmp %>% 
      filter(!is.nonamb)
      )+
  geom_smooth(method = lm, se =F)+
  coord_cartesian(ylim = c(0, 36))+
  labs(color = 'Ambulation (by E7)', y = "Upright Stability Score")+
  .leg('none')

# .sp( ti = 'ADL vs USS, SARA, f-SARA', l = "F", i = 1)
