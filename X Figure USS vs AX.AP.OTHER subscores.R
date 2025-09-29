

# . -----------------------------------------------------------------------

rm(list = ls())

source('.project.settings.R')

dt. <- readRDS('DATA derived/dt.all.visits.rds') %>% 
  # filter(has.both) %>%
  filter(!is.nonamb) %>%
  filter(!is.preataxic)

params. <- c('ADL','SARA','FARS.E','fSARA')
labs.   <- c('ADL','SARA','USS','fSARA')

# dt. %<>% 
#   mutate( paramcd = factor(paramcd, 
#                            labels = labs.,
#                            levels = params.,
#   )) %>% 
#   filter( !is.na(paramcd ) )


dt.tmp <- dt. %>% 
  spread( paramcd, aval ) %>% 
  rename(USS = FARS.E) %>% 
  ungroup() %>% 
  mutate( ADL.ax   = rowSums( .[c("a6.fall","a7.walk")], na.rm=T)) %>% # do all, with na.rm  
  mutate( ADL.ap   = rowSums( .[c("a3.eat", "a4.dress", "a5.body")], na.rm=T)) %>% # do all, with na.rm  
  mutate( ADL.bl   = rowSums( .[c("a1.speech", "a2.swallow")], na.rm=T)) %>% # do all, with na.rm  
  mutate( fSARA.ax = rowSums( .[c("s1f.gait",   "s2f.stance", "s3.sit")], na.rm=T)) %>% # do all, with na.rm  
  select(
    study, sjid, avisitn, avisitx, subtype, time., age, dur, fds, 
    USS,
    ADL.bl, ADL.ap, ADL.ax,
    SARA.ax, SARA.ki, s4.speech, 
    fSARA.ax, s4f.speech
  ) %>% 
  gather(paramcd, aval, 
         ADL.ax, ADL.ap, ADL.bl,  
         SARA.ax, SARA.ki, s4.speech, 
         fSARA.ax, s4f.speech
  )


dt.tmp %<>% 
  mutate( paramcd = factor(
    paramcd, 
    levels = c(
      "ADL.ax", "ADL.ap", "ADL.bl",  
      "SARA.ax", "SARA.ki", "s4.speech", 
      "fSARA.ax", "dummy","s4f.speech"
    )
  )
  )

dt.tmp %>% 
  ggplot()+geom_point(size = 1)+
  aes ( y = USS, x = aval )+
  aes ( color = study )+ggsci::scale_color_d3(palette = "category20c")+
  facet_wrap( ~paramcd, ncol = 3, drop =F, scales = 'free_x' )+
  geom_smooth( formula = y ~ poly(x, 2), method = 'lm')+
  ggpmisc::stat_correlation(
    aes(label = paste(after_stat(rr.label))),
    size =  7 / .pt,
    family = theme_get()$text$family
  )+
  labs(color = 'Genotype', y = "Upright Stability Score")+
  theme_set(
    theme_minimal(base_size = 6)
  )+
  .leg('none')

# .sp()
