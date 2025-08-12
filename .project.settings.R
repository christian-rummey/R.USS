
.pt <- 2.845276

theme_set(
  theme_minimal(base_size = 16) %+replace%
    theme(
      legend.position  = "top",
      legend.box      = "horizontal",
      legend.direction= "horizontal",
      complete         = TRUE
    )
)

# .theme <- function(base_size = 16, ...) {
#   theme_minimal(base_size = base_size) %+replace%
#     theme(
#       legend.position  = "top",
#       complete         = TRUE
#     )
# }

# Tip: If you want the label to scale consistently with base_size = 10, 
# use size = 10 / .pt (where .pt = 2.845276, a ggplot2-internal conversion
# factor from points to mm). Or simply use size ~3–4 for standard appearance.




# .vnames <- function ( df ) {
#   df %<>%
#     left_join(.rt('../DATA other/FACHILD.FACOMS.vnames.txt')) %>%
#     mutate(vname = factor(vname, c('BL','6m','1y','18m','2y','3y','3y+'))) %>%
#     select(study, vname, everything()) %>% 
#     mutate(avisit = vname) %>% 
#     select(-vname)
#   return (df)
# }

# parlst. <- c('mFARS Total'                    , 'mFARS',
#              'Upright Stability (FARS.E)'     , 'FARS.E',
#              'Upper Limbs (FARS.B)'           , 'FARS.B',
#              'Lower Limbs (FARS.C)'           , 'FARS.C',
#              'Bulbar (FARS.Am)'               , 'FARS.Am',
#              'Timed 25 Foot Walk (m/s)'       , 'w25.i',
#              '9 Hole Peg Test (1/min)'        , 'hpt.i',
#              # '9 Hole Peg Test, dom. (1/min)'  , 'hpt.id',
#              # '9 Hole Peg Test, ndom. (1/min)' , 'hpt.in',
#              # 'Timed 25 Foot Walk (m/s) unable', 'w25.iu',
#              # '9 Hole Peg Test (1/min) unable' , 'hpt.iu',
#              'Timed up and Go (1/s)'          , 'tug.i',
#              'One Minute Walk (m)'            , 'w1m',
#              '6 Minute Walk (m)'              , 'w6m',
#              'Berg Balance Scale'             , 'bbs',
#              'Activities of Daily Living'     , 'ADL',
#              'PQL Total Score C'              , 'TOT.C',
#              'PQL Total Score P'              , 'TOT.P',
#              'PQL Physical C'                 , 'PF.C',
#              'PQL Physical P'                 , 'PF.P',
#              'PQL Emotional C'                , 'EF.C',
#              'PQL Emotional P'                , 'EF.P',
#              'PQL Social C'                   , 'SO.C',
#              'PQL Social P'                   , 'SO.P',
#              'PQL School C'                   , 'SC.C',
#              'PQL School P'                   , 'SC.P'
# )
# 
# params. <- parlst.[c(seq(1,44,2))]
# pars.   <- parlst.[c(seq(2,45,2))]
# 
# rm(parlst.)
