
# all everything ----------------------------------------------------------

source('.project.settings.R')

# scales data 

scales.list <- c('mFARS'   ,'SARA'      , 'fSARA',
                 'FARS.E'  ,
                 'SARA.ax'   ,
                 # 'FARS.BC' ,
                 'SARA.ki'   ,
                 'SARA.ku'   ,
                 # 'FARS.Am' ,#'s4.speech' ,
                 'ADL',
                 'ADL.upper','ADL.lower','ADL.bulbar','ADL.other',
                 .l.sara,
                 .l.fsara[c(1,2,4)],
                 .l.adl,
                 .l.FARS.E)

scales <- .rt('../../Ataxia/DATA other/scales.txt') %>% 
  select( -pl2 ) %>% 
  filter( paramcd %in% scales.list ) %>% 
  mutate( score = case_when(
    paramcd %in% scales.list[c( 1, 4, 6, 8 )]      ~ 'mFARS',
    paramcd %in% scales.list[c( 2, 5, 7, 9 )]        ~ 'SARA',
    paramcd %in% scales.list[c( 3 )]    ~ 'fSARA'
  )) %>%
  # mutate(score.max = case_when(
  #   score == 'mFARS' ~  93,
  #   score == 'SARA'  ~  40,
  #   score == 'ICARS' ~ 100
  # )) %>% 
  mutate( score.type = case_when(
    paramcd %in% scales.list[c(  4, 5 )]  ~ 'Axial Function',
    paramcd %in% scales.list[c(  6, 7 )]  ~ 'Appendicular Function',
    paramcd %in% scales.list[c(  8, 9 )]  ~ 'Speech Disorder',
    TRUE ~ 'Total Score'
  )) %>% 
  mutate(paramcd = factor(paramcd, scales.list))

scales %>% 
  write.table('DATA derived/scales.txt', sep = '\t')

# USS data ----------------------------------------------------------------

dt. <- bind_rows(
  .dd('fars') %>% filter(study %in% c('CRCSCA','UNIFAI')),
  .dd('sara') %>% filter(study %in% c('CRCSCA','UNIFAI')),
  .dd('adl')  %>% filter(study %in% c('CRCSCA','UNIFAI')) 
  ) %>% 
  select( -age, -time.)

dt. %<>% 
  select( -avisit ) %>% 
  filter( adt >= '2024-03-01' | study == 'UNIFAI')

# add age/dur--------------------------------------------------------------

dt. %<>%  
  filter( paramcd %in% scales$paramcd )

dt.FA <- dt. %>% 
  filter(study == 'UNIFAI') %>% 
  filter(!is.na(adt)) %>% 
  left_join(
    .dd('demo') %>% 
      select( study, sjid, dob, aoo, sev.o ) %>% 
      mutate( subtype = as.character( sev.o ) )
  ) %>% 
  mutate( age = as.numeric( adt - dob )/365.25 ) %>% 
  mutate( dur = age - aoo ) %>% 
  .gs %>% 
  mutate( time. = age - min( age ) ) %>% 
  .ug %>% 
  select( study, sjid, subtype, avisitn, time., age, dur, paramcd, aval )

dt.SCA <- dt. %>% 
  filter(study == 'CRCSCA') %>% 
  filter(!is.na(adt)) %>% 
  left_join(
    
      .dd('demo.sca') %>% 
        filter(study == 'CRCSCA') %>% 
        select(study, sjid, sca, aoo) %>% 
        left_join(
          
          .dd('visit.dates.CRCSCA') %>% 
            filter(avisitn == min(avisitn)) %>% 
            select(study, sjid, adt, age_bl)
          
          ) %>%
      select( study, sjid, adt_bl = adt, age_bl, aoo, sca ) %>% 
      mutate( subtype = as.character( sca ) )
      
  ) %>% 
  arrange( study, sjid, adt ) %>% 
  .gs %>% 
  mutate( time. = as.numeric (adt - min( adt )) / 365.25 ) %>% 
  mutate( age   = as.numeric( age_bl + time. ) ) %>% 
  mutate( dur = age - aoo ) %>% 
  .ug %>% 
  select( study, sjid, subtype, avisitn, time., age, dur, paramcd, aval )

# bind --------------------------------------------------------------------

dt. <- bind_rows ( dt.SCA, dt.FA )

dt. %<>%  
  filter( paramcd %in% scales$paramcd )

# . -----------------------------------------------------------------------

dt. %>% 
  filter( is.na(age) ) %>% 
  select( sjid, subtype ) %>% 
  unique

dt. %<>% 
  filter( !is.na(age) )

dt. %>% 
  .gs %>% 
  filter(paramcd == 'FARS.E' & study == 'CRCSCA' ) %>% 
  filter(age == min(age)) %>% 
  group_by(sjid) %>% filter(n()>1)

dt. %<>% 
  filter( !( sjid == 'JH115' & avisitn == 1 ) )

# spread and add fds / replacement ----------------------------------------

dt. %<>% 
  spread( paramcd, aval ) %>% 
  .ug %>%
  left_join(
    .dd('steps') %>% 
      filter(study %in% c('CRCSCA','UNIFAI')) %>% 
      select(study, sjid, avisitn, fds) %>% 
      filter(!is.na(fds)) %>% 
      # .gsv %>% filter(n()>1) %>%
      # slice(1) %>% 
      unique
  ) %>% 
  mutate( fds = ifelse( is.na(fds), fane7, fds ) ) %>%
  # filter( is.na(fds) ) %>% 
  droplevels

# select whom to analyze --------------------------------------------------

dt. %<>% 
  filter( !is.na(FARS.E) ) %>% 
  filter( !is.na(SARA)   )

# label missing stuff -----------------------------------------------------
  # mutate( 
  #   # total.c = count(.[c('FARS.E','fSARA','SARA','ADL')]),
  #   total   = rowSums(.[c('FARS.E','fSARA','SARA','ADL')], na.rm=T)
  #   ) %>% 
  # filter( SARA <= 3 ) %>% arrange(SARA) %>% print ( n = 50)
  # filter(is.preataxic) %>% select(ADL, SARA, fSARA, FARS.E)

dt. %<>% 
  mutate( is.preataxic = ifelse(FARS.E == 0 & SARA == 0, T, F)) %>%
  # mutate( has.USS  = !is.na(FARS.E) ) %>% 
  # mutate( has.SARA = !is.na(SARA)   ) %>% 
  # mutate( has.both = !is.na(SARA) & !is.na(FARS.E) ) %>% 
  mutate( is.nonamb  = ifelse(fane7  >= 5, T, F) ) %>%
  mutate( can.stand  = ifelse(fane2a  < 4, T, F) ) %>%
  mutate( is.30ol    = ifelse(FARS.E < 31, T, F) ) %>% 
  gather( paramcd, aval, scales$paramcd) %>% 
  filter( !is.na(aval))

dt. %<>% 
  group_by( study, sjid ) %>% 
  mutate  ( avisitx = avisitn - min(avisitn) ) %>% 
  select ( study, sjid, avisitn, avisitx, paramcd, aval, everything() )

# status ------------------------------------------------------------------

dt. %<>% 
  mutate( status = ifelse(is.preataxic, 'preataxic', ifelse(is.nonamb, 'non-ambualtory', 'ambulatory' )))

dt. %<>% 
  filter( avisitx == 0 )

dt.$sjid %>% unique %>% length()


# fixstuff ----------------------------------------------------------------

dt. %<>% 
  mutate(fds = ifelse( fds == 45, 4.5, fds)) %>% 
  mutate(fds = ifelse( fds == 25, 2.5, fds))
# write -------------------------------------------------------------------

dt. %>% 
  write_rds('DATA derived/dt.all.visits.rds')



# . -----------------------------------------------------------------------


# dt. %>% 
#   ungroup %>% 
#   filter(has.SARA) %>% 
#   select(study, paramcd) %>% .tab
# 
# dt. %>% 
#   filter(has.both) %>% 
#   ungroup %>% 
#   select(study, paramcd) %>% .tab
# 
# dt. %>% 
#   filter(has.USS) %>% 
#   ungroup %>% 
#   select(study, paramcd) %>% .tab
