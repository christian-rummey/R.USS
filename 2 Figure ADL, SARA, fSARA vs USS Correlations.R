
# . -----------------------------------------------------------------------

rm(list = ls())

source('.project.settings.R')

dt. <- readRDS('DATA derived/dt.all.visits.rds') %>% 
  # filter(has.both) %>%
  # filter(!is.nonamb) %>%
  # filter(!is.preataxic) %>% 
  mutate( status = ifelse(is.preataxic, 'preataxic', ifelse(is.nonamb, 'non-ambualtory', 'ambulatory' ))) %>% 
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


# SARA, fSARA //SARA.ax/app vs USS ---------------------------------------------

dt.tmp <- dt. %>% 
  filter(!is.nonamb) %>% 
  gather( paramcd, aval, ADL, SARA, fSARA ) %>% 
  .gs %>% filter(avisitn == min(avisitn)) %>% ungroup %>% 
  filter(!is.na(aval)) %>% 
  filter( paramcd %in% c('ADL','SARA','fSARA')) %>%
  mutate( paramcd = factor(paramcd, c('ADL','SARA','fSARA')))

dt.tmp %>% 
  ggplot()+geom_point()+
  aes ( y = USS, x = aval )+
  aes ( color = status )+ggsci::scale_color_d3()+
  # facet_wrap( ~paste(study, paramcd, sep=', '), scales = 'free_x', ncol = 3 )+
  facet_grid( study~paramcd, scales = 'free_x')+
  aes(alpha = is.30ol)+scale_alpha_manual(values = c(0.55, 1))+
  ggpmisc::stat_correlation(
    aes(label = paste(after_stat(rr.label))),
        size =  10 / .pt,
        family = theme_get()$text$family, 
    alpha = NA,
    data = dt.tmp %>% 
      # filter( status == 'ambulatory') %>% 
      droplevels()
      )+
  geom_smooth(method = lm, se =F,     alpha = NA)+
  coord_cartesian(ylim = c(0, 36))+
  labs(color = 'Ambulation (by E7)', y = "Upright Stability Score")+
  .leg('none')

# .sp( ti = 'Figure 2: USS vs ADL, SARA, f-SARA', l = "F", i = 1)




