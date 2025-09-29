
# . -----------------------------------------------------------------------

rm(list = ls())
library(ggcorrplot)

source('.project.settings.R')

dt. <- readRDS('DATA derived/dt.all.visits.rds') %>% 
  # filter( !is.na(subtype), !subtype %in% c("SCA10","SCA7","SCA8")) %>%
  filter(!is.nonamb) %>%
  filter(sjid !='LA125') %>% 
  # filter(!is.preataxic) %>%
  
  droplevels()

dt.wide <- dt. %>% 
  spread( paramcd, aval ) %>% rename(USS = FARS.E) %>% 
  .ug %>% 
  select( study, sjid, fds, SARA, ADL, USS, SARA.ax, SARA.ki, SARA.ku, ADL.bulbar,  ADL.lower,  ADL.other,  ADL.upper,"s4.speech" ) %>% 
  droplevels()

library(tidyverse)

dt.wide %>% 
  ggplot(aes(x = SARA, y = USS))+geom_point()


# keep only numeric scales

dt.wide.all <- bind_rows(
  dt.wide %>% filter(study == 'UNIFAI'), 
  dt.wide %>% filter(study == 'CRCSCA'),
  dt.wide %>% mutate(study = 'BOTH')
  )

# helper: compute correlation + p-value + n -----------------------------

cor_test <- function(x, y) {
  complete <- complete.cases(x, y)
  x2 <- x[complete]
  y2 <- y[complete]
  n <- length(x2)
  
  if (n < 3) {
    return(tibble(rho = NA_real_, p = NA_real_, n = n))
  }
  
  test <- cor.test(x2, y2, method = "pearson", exact = FALSE)
  tibble(rho = unname(test$estimate), p = test$p.value, n = n)
}

# scales of interest / order
# vars <- c("USS", "SARA", "fSARA", "ADL")
# vars <- c('USS', 'SARA', 'SARA.ax', 'SARA.ki', 'SARA.ku','ADL' ,'ADL.bulbar',  'ADL.lower',  'ADL.other',  'ADL.upper')

vars <- c('fds','USS', 'SARA', 'SARA.ax', 'SARA.ku', 's4.speech','ADL' ,'ADL.bulbar',  'ADL.lower',  'ADL.other',  'ADL.upper')

# compute correlations by study
cors <- dt.wide.all %>%
  group_by(study) %>%
  group_modify(~{
    dat <- .x %>% select(all_of(vars)) %>% mutate(across(everything(), as.numeric))
    
    expand_grid(var1 = vars, var2 = vars) %>%
      rowwise() %>%
      mutate(stats = list(cor_test(dat[[var1]], dat[[var2]]))) %>%
      unnest(stats)
  }) %>%
  ungroup() %>%
  mutate(
    label = case_when(
      var1 == var2 ~ "1.00",
      is.na(rho)   ~ "NA",
      p < 0.05     ~ sprintf("%.2f", rho),
      TRUE         ~ sprintf("%.2f (n.s.)", rho)
    ),
    var1 = factor(var1, levels = vars),
    var2 = factor(var2, levels = vars)
  )

  # order_vars <- c('SARA', 'SARA.ax','ADL.lower', 'USS',  'SARA.ki', 'SARA.ku', 'ADL.upper', 'ADL.bulbar', 'ADL.other')
order_vars <- c(
  'fds',
  'USS' ,
  'SARA','SARA.ax', 'SARA.ku','s4.speech',
  'ADL','ADL.lower','ADL.upper',   'ADL.bulbar', 'ADL.other'
)

label_vars <- c(
  'FDS',
  'USS' ,
  'SARA','SARA\naxial', 'SARA\nupper limbs','SARA\nspeech',
  'ADL','walk, fall','food,\ndressing,\nhygiene',   'speech,\nswallow', 'sit,\nbladder'
)

# plot one heatmap per study
cors %>% 
  filter(!var2 %in% c( 'ADL','ADL.other' , 'ADL.bulbar', 'ADL.upper', 'ADL.lower')) %>%
  filter(var1 %in% c( 'ADL', 'ADL.other' , 'ADL.bulbar', 'ADL.upper', 'ADL.lower')) %>%
  mutate(var1 = factor(var1,    (order_vars), label_vars)) %>% 
  mutate(var2 = factor(var2, rev(order_vars), rev(label_vars))) %>% 
  ggplot() +
  geom_tile(color = "black") +
  aes(x = var1, y = var2, fill = rho)+
  geom_text(aes(label = label), size = 5, color = "black") +
  scale_fill_gradient(low = "white", high = "blue", limits = c(0.3,1), na.value = "grey90") +
  coord_fixed() +
  facet_wrap(~study) +
  .theme() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold")) +
  labs(fill = "Pearson r")

# .sp(l = 'F', ti='Pearson')


# dt.wide.all %>%
#   filter(study == "BOTH") %>%   # pick one study at a time
#   select(all_of(vars)) %>%
#   GGally::ggpairs(progress = FALSE,
#                   lower = list(continuous = GGally::wrap("smooth", alpha = 0.3, size = 0.5)),
#                   diag = list(continuous = "densityDiag"),
#                   upper = list(continuous = "cor"))


# cors <- cors %>%
#   mutate(
#     var1 = factor(var1, levels = order_vars),
#     var2 = factor(var2, levels = order_vars)
#   )
# 
# cors %>%
#   # arrange(-p)
#   filter(!var2 %in% c( 'ADL','ADL.other' , 'ADL.bulbar', 'ADL.upper', 'ADL.lower')) %>% 
#   filter(var1 %in% c( 'ADL', 'ADL.other' , 'ADL.bulbar', 'ADL.upper', 'ADL.lower')) %>% 
# # plot tiles ------------------------------------------------------------
# ggplot( aes(x = var1, y = var2, fill = rho)) +
#   geom_tile(color = "black") +
#   geom_text(aes(label = label), size = 5, color = "black") +
#   scale_fill_gradient(low = "white", high = "#386cb0", limits = c(0, 1)) +
#   # scale_fill_gradient(aesthetics = 'fill') +
#   coord_fixed() +
#   theme_minimal() +
#   theme(axis.title = element_blank(),
#         panel.grid = element_blank()) +
#   labs(fill = "Spearman rho")
# # .sp()


# # lineplot ----------------------------------------------------------------
# levels(cors$var2)
# 
# dt.tmp <- cors %>%
#   filter(var1!=var2) %>% 
#   group_by(var1) %>% 
#   mutate(overall = mean(rho)) %>% 
#   filter(var2 %in% c('USS','SARA','ADL','SARA.ax','ADL.lower','ADL.upper'))
# 
# dt.tmp %>% 
#   # geom_col( aes(x = var1, y = overall, fill = NULL), data = dt.tmp %>% slice(1) )
#   ggplot( )+geom_point()+
#   aes(x = var1, y = rho)+
#   geom_line(aes(color = var2, group = var2))+
#   # coord_fixed() +
#   theme(axis.title = element_blank(),
#         panel.grid = element_blank())
# # .sp()