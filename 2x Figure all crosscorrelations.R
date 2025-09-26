
# . -----------------------------------------------------------------------

rm(list = ls())
library(ggcorrplot)

source('.project.settings.R')

dt. <- readRDS('DATA derived/dt.all.visits.rds') %>% 
  
  filter(!is.nonamb) %>%
  filter(!is.preataxic) %>%
  
  droplevels()

dt.wide <- dt. %>% 
  spread( paramcd, aval ) %>% rename(USS = FARS.E) %>% 
  .ug %>% 
  select( study, sjid, SARA, ADL, USS, SARA.ax, SARA.ki, SARA.ku, ADL.bulbar,  ADL.lower,  ADL.other,  ADL.upper ) %>% 
  droplevels()

library(tidyverse)

# keep only numeric scales

scales <- bind_rows(
  dt.wide %>% 
    filter(study == 'UNIFAI') %>% 
    select(SARA, ADL, USS, SARA.ax, SARA.ki, SARA.ku, ADL.bulbar,  ADL.lower,  ADL.other,  ADL.upper) %>%
    mutate(across(everything(), as.numeric)),
  dt.wide %>% 
    filter(study == 'CRCSCA') %>% 
    select(SARA, ADL, USS, SARA.ax, SARA.ki, SARA.ku, ADL.bulbar,  ADL.lower,  ADL.other,  ADL.upper) %>%
    mutate(across(everything(), as.numeric)),
  dt.wide %>% 
    mutate(study == 'BOTH') %>% 
    select(SARA, ADL, USS, SARA.ax, SARA.ki, SARA.ku, ADL.bulbar,  ADL.lower,  ADL.other,  ADL.upper) %>%
    mutate(across(everything(), as.numeric))
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

# compute all pairwise correlations
cors <- expand_grid(var1 = names(scales), var2 = names(scales)) %>%
  rowwise() %>%
  mutate(stats = list(cor_test(scales[[var1]], scales[[var2]]))) %>%
  unnest(stats) %>%
  ungroup() %>%
  mutate(label = case_when(
    var1 == var2 ~ "1.00",
    is.na(rho) ~ "NA",
    p < 0.05 ~ sprintf("%.2f", rho),
    TRUE ~ sprintf("%.2f (n.s.)", rho)
  ))

# ensure order
cors <- cors %>%
  mutate(
    var1 = factor(var1, levels = names(scales)),
    var2 = factor(var2, levels = names(scales))
  )

# order_vars <- c('SARA', 'SARA.ax','ADL.lower', 'USS',  'SARA.ki', 'SARA.ku', 'ADL.upper', 'ADL.bulbar', 'ADL.other')
order_vars <- c(
  'SARA.ki', 'SARA.ku','SARA.ax','SARA', 
  'USS' ,
  'ADL','ADL.lower','ADL.upper',   'ADL.bulbar', 'ADL.other'
  )

cors <- cors %>%
  mutate(
    var1 = factor(var1, levels = order_vars),
    var2 = factor(var2, levels = order_vars)
  )

cors %>%
  # arrange(-p)
  filter(!var2 %in% c( 'ADL','ADL.other' , 'ADL.bulbar', 'ADL.upper', 'ADL.lower')) %>% 
  filter(var1 %in% c( 'ADL', 'ADL.other' , 'ADL.bulbar', 'ADL.upper', 'ADL.lower')) %>% 
# plot tiles ------------------------------------------------------------
ggplot( aes(x = var1, y = var2, fill = rho)) +
  geom_tile(color = "black") +
  geom_text(aes(label = label), size = 5, color = "black") +
  scale_fill_gradient(low = "white", high = "#386cb0", limits = c(0, 1)) +
  # scale_fill_gradient(aesthetics = 'fill') +
  coord_fixed() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank()) +
  labs(fill = "Spearman rho")
# .sp()


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
