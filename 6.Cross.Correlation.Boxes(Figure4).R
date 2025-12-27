
# . -----------------------------------------------------------------------

rm(list = ls())
library(ggcorrplot)

source('.project.settings.R')

dt. <- readRDS('DATA derived/dt.all.visits.rds') %>%
  # filter( !is.na(subtype), !subtype %in% c("SCA10","SCA7","SCA8")) %>%
  # filter(!is.nonamb) %>%  # Now filtered in 0.DM.USS.Paper.R
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

cor_test <- function(x, y, method = "pearson") {
  complete <- complete.cases(x, y)
  x2 <- x[complete]
  y2 <- y[complete]
  n <- length(x2)

  if (n < 3) {
    return(tibble(rho = NA_real_, p = NA_real_, n = n))
  }

  test <- cor.test(x2, y2, method = method, exact = FALSE)
  tibble(rho = unname(test$estimate), p = test$p.value, n = n)
}

# scales of interest / order
# vars <- c("USS", "SARA", "fSARA", "ADL")
# vars <- c('USS', 'SARA', 'SARA.ax', 'SARA.ki', 'SARA.ku','ADL' ,'ADL.bulbar',  'ADL.lower',  'ADL.other',  'ADL.upper')

vars <- c('fds','USS', 'SARA', 'SARA.ax', 'SARA.ku', 's4.speech','ADL' ,'ADL.bulbar',  'ADL.lower',  'ADL.other',  'ADL.upper')

# compute Pearson correlations by study
cors <- dt.wide.all %>%
  group_by(study) %>%
  group_modify(~{
    dat <- .x %>% select(all_of(vars)) %>% mutate(across(everything(), as.numeric))

    expand_grid(var1 = vars, var2 = vars) %>%
      rowwise() %>%
      mutate(stats = list(cor_test(dat[[var1]], dat[[var2]], method = "pearson"))) %>%
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

# compute Spearman correlations by study
cors_spearman <- dt.wide.all %>%
  group_by(study) %>%
  group_modify(~{
    dat <- .x %>% select(all_of(vars)) %>% mutate(across(everything(), as.numeric))

    expand_grid(var1 = vars, var2 = vars) %>%
      rowwise() %>%
      mutate(stats = list(cor_test(dat[[var1]], dat[[var2]], method = "spearman"))) %>%
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

# Slide 1: Pearson r - BOTH only
p1 <- cors %>%
  filter(study == "BOTH") %>%
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
  .theme() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank()) +
  labs(fill = "Pearson r")

p1

# Slide 2: Pearson r - UNIFAI vs CRCSCA
p2 <- cors %>%
  filter(study %in% c("UNIFAI", "CRCSCA")) %>%
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

p2

# Add R² column to cors
cors_r2 <- cors %>%
  mutate(
    r2 = rho^2,
    label_r2 = case_when(
      var1 == var2 ~ "1.00",
      is.na(rho)   ~ "NA",
      p < 0.05     ~ sprintf("%.2f", r2),
      TRUE         ~ sprintf("%.2f (n.s.)", r2)
    )
  )

# Slide 3: R² - BOTH only
p3 <- cors_r2 %>%
  filter(study == "BOTH") %>%
  filter(!var2 %in% c( 'ADL','ADL.other' , 'ADL.bulbar', 'ADL.upper', 'ADL.lower')) %>%
  filter(var1 %in% c( 'ADL', 'ADL.other' , 'ADL.bulbar', 'ADL.upper', 'ADL.lower')) %>%
  mutate(var1 = factor(var1,    (order_vars), label_vars)) %>%
  mutate(var2 = factor(var2, rev(order_vars), rev(label_vars))) %>%
  ggplot() +
  geom_tile(color = "black") +
  aes(x = var1, y = var2, fill = r2)+
  geom_text(aes(label = label_r2), size = 5, color = "black") +
  scale_fill_gradient(low = "white", high = "blue", limits = c(0.09,1), na.value = "grey90") +
  coord_fixed() +
  .theme() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank()) +
  labs(fill = "R²")

p3

# Slide 4: R² - UNIFAI vs CRCSCA
p4 <- cors_r2 %>%
  filter(study %in% c("UNIFAI", "CRCSCA")) %>%
  filter(!var2 %in% c( 'ADL','ADL.other' , 'ADL.bulbar', 'ADL.upper', 'ADL.lower')) %>%
  filter(var1 %in% c( 'ADL', 'ADL.other' , 'ADL.bulbar', 'ADL.upper', 'ADL.lower')) %>%
  mutate(var1 = factor(var1,    (order_vars), label_vars)) %>%
  mutate(var2 = factor(var2, rev(order_vars), rev(label_vars))) %>%
  ggplot() +
  geom_tile(color = "black") +
  aes(x = var1, y = var2, fill = r2)+
  geom_text(aes(label = label_r2), size = 5, color = "black") +
  scale_fill_gradient(low = "white", high = "blue", limits = c(0.09,1), na.value = "grey90") +
  coord_fixed() +
  facet_wrap(~study) +
  .theme() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold")) +
  labs(fill = "R²")

p4

# Slide 5: Spearman rho - BOTH only
p5 <- cors_spearman %>%
  filter(study == "BOTH") %>%
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
  .theme() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank()) +
  labs(fill = "Spearman ρ")

p5

# Slide 6: Spearman rho - UNIFAI vs CRCSCA
p6 <- cors_spearman %>%
  filter(study %in% c("UNIFAI", "CRCSCA")) %>%
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
  labs(fill = "Spearman ρ")

p6

# Export to PowerPoint - single file with 6 slides
library(officer)

.ppt.template.file <- "C:\\Users\\ChristianRummey\\OneDrive - CDS\\Projects/_templates/CR.template.pptx"

# fixes minus signs
.fix_plot_minuses <- function(p) {
  p + labs(
    title    = gsub("-", "\u2212", p$labels$title),
    subtitle = gsub("-", "\u2212", p$labels$subtitle),
    caption  = gsub("-", "\u2212", p$labels$caption),
    x        = gsub("-", "\u2212", p$labels$x),
    y        = gsub("-", "\u2212", p$labels$y)
  )
}

target_file <- "6.Cross.Correlation.Boxes(Figure4).pptx"

ppt <- read_pptx(.ppt.template.file)

# Slide 1: Pearson r - All
pp1 <- .fix_plot_minuses(p1)
ppt <- ppt %>%
  add_slide(layout = "F", master = "CR") %>%
  ph_with(
    dml(print(pp1, newpage = FALSE)),
    location = ph_location_type(type = "body", type_idx = 1)
  ) %>%
  ph_with(
    "Pearson r - All",
    location = ph_location_type(type = "title")
  ) %>%
  set_notes(
    value = paste("Created at", format(Sys.time(), "%Y-%m-%d %H-%M-%S")),
    location = notes_location_type("body")
  )

# Slide 2: Pearson r - UNIFAI vs CRCSCA
pp2 <- .fix_plot_minuses(p2)
ppt <- ppt %>%
  add_slide(layout = "F", master = "CR") %>%
  ph_with(
    dml(print(pp2, newpage = FALSE)),
    location = ph_location_type(type = "body", type_idx = 1)
  ) %>%
  ph_with(
    "Pearson r - UNIFAI vs CRCSCA",
    location = ph_location_type(type = "title")
  ) %>%
  set_notes(
    value = paste("Created at", format(Sys.time(), "%Y-%m-%d %H-%M-%S")),
    location = notes_location_type("body")
  )

# Slide 3: R² - All
pp3 <- .fix_plot_minuses(p3)
ppt <- ppt %>%
  add_slide(layout = "F", master = "CR") %>%
  ph_with(
    dml(print(pp3, newpage = FALSE)),
    location = ph_location_type(type = "body", type_idx = 1)
  ) %>%
  ph_with(
    "R² - All",
    location = ph_location_type(type = "title")
  ) %>%
  set_notes(
    value = paste("Created at", format(Sys.time(), "%Y-%m-%d %H-%M-%S")),
    location = notes_location_type("body")
  )

# Slide 4: R² - UNIFAI vs CRCSCA
pp4 <- .fix_plot_minuses(p4)
ppt <- ppt %>%
  add_slide(layout = "F", master = "CR") %>%
  ph_with(
    dml(print(pp4, newpage = FALSE)),
    location = ph_location_type(type = "body", type_idx = 1)
  ) %>%
  ph_with(
    "R² - UNIFAI vs CRCSCA",
    location = ph_location_type(type = "title")
  ) %>%
  set_notes(
    value = paste("Created at", format(Sys.time(), "%Y-%m-%d %H-%M-%S")),
    location = notes_location_type("body")
  )

# Slide 5: Spearman ρ - All
pp5 <- .fix_plot_minuses(p5)
ppt <- ppt %>%
  add_slide(layout = "F", master = "CR") %>%
  ph_with(
    dml(print(pp5, newpage = FALSE)),
    location = ph_location_type(type = "body", type_idx = 1)
  ) %>%
  ph_with(
    "Spearman ρ - All",
    location = ph_location_type(type = "title")
  ) %>%
  set_notes(
    value = paste("Created at", format(Sys.time(), "%Y-%m-%d %H-%M-%S")),
    location = notes_location_type("body")
  )

# Slide 6: Spearman ρ - UNIFAI vs CRCSCA
pp6 <- .fix_plot_minuses(p6)
ppt <- ppt %>%
  add_slide(layout = "F", master = "CR") %>%
  ph_with(
    dml(print(pp6, newpage = FALSE)),
    location = ph_location_type(type = "body", type_idx = 1)
  ) %>%
  ph_with(
    "Spearman ρ - UNIFAI vs CRCSCA",
    location = ph_location_type(type = "title")
  ) %>%
  set_notes(
    value = paste("Created at", format(Sys.time(), "%Y-%m-%d %H-%M-%S")),
    location = notes_location_type("body")
  )

# Save PowerPoint
ppt %>% print(target = target_file)

print(paste("PowerPoint saved as:", target_file))
