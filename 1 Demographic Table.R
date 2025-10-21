
source('.project.settings.R')
rm(list = ls())

dt. <- readRDS("DATA derived/dt.all.visits.rds") %>%
  # filter( !is.preataxic  ) %>%
  # filter(  has.both      ) %>%
  filter( !is.nonamb       ) %>%
  # filter( !is.na(subtype), !subtype %in% c("SCA10","SCA7","SCA8")) %>%
  filter(paramcd %in% c('SARA')) %>%
  droplevels()

# Load tableone package
library(tableone)

# Prepare data for demographic table
dt_demo <- dt. %>%
  mutate(
    # Ensure subtype factor has correct levels and order (including UNIFAI/FRDA subtypes)
    subtype = factor(subtype, levels = c("0-7y", "8-14y", "15-24y", ">24y", "SCA1", "SCA2", "SCA3", "SCA6", "SCA7", "SCA8", "SCA10", "SCA27B", "RFC1")),
    
    # Ensure FDS is a factor
    fds = factor(fds, levels = 0:6)
  )

# Define variables for the table
vars_to_include <- c(
  "age", "dur", "subtype", "fds"
  # ,
  # "is.preataxic", "is.nonamb", "can.stand", "is.30ol"
)

LABELS <- c("Age (mean, SD)", "Disease Duration (mean, SD)", "Genotype/AOO Group (%)", "Functional Disease Staging (%)")

# Create the demographic table comparing CRC-SCA vs UNIFAI
demo_table <- CreateTableOne(
  vars = vars_to_include,
  strata = "study",
  data = dt_demo,
  factorVars = c("subtype", "fds", "is.preataxic", "is.nonamb", "can.stand", "is.30ol"),
  includeNA = FALSE
)

# Print the table with missing data indicators
print(demo_table, showAllLevels = TRUE, printToggle = FALSE, missing = TRUE)

# For docx output - better formatting with preserved labels
library(flextable)
library(officer)

# Get the table matrix and preserve row names/labels
table_matrix <- print(demo_table, showAllLevels = TRUE, printToggle = FALSE, missing = TRUE, quote = FALSE, noSpaces = TRUE, format = "p", varLabels = TRUE)

# Convert to data frame and replace variable names with custom labels
table_df <- data.frame(
  Characteristic = rownames(table_matrix),
  table_matrix,
  stringsAsFactors = FALSE,
  row.names = NULL
)

# Debug: check what the actual row names are
print("Row names in table:")
print(unique(table_df$Characteristic))
print("Variables to replace:")
print(vars_to_include)

# Create a mapping to replace main variable rows with custom labels
# Need to match the exact format tableone uses
table_df$Characteristic <- case_when(
  grepl("^age", table_df$Characteristic, ignore.case = TRUE) ~ LABELS[1],
  grepl("^dur", table_df$Characteristic, ignore.case = TRUE) ~ LABELS[2],
  grepl("^subtype", table_df$Characteristic, ignore.case = TRUE) ~ LABELS[3],
  grepl("^fds", table_df$Characteristic, ignore.case = TRUE) ~ LABELS[4],
  TRUE ~ table_df$Characteristic
)

# Create a well-formatted flextable with category separators
demo_flextable <- flextable(table_df) %>%
  theme_vanilla() %>%
  fontsize(size = 10, part = "all") %>%
  align(align = "center", part = "header") %>%
  align(j = 1, align = "left", part = "body") %>%
  align(align = "center", part = "body", j = -1) %>%
  # Add horizontal lines between major categories
  hline(i = ~ grepl("^age|^dur|^subtype|^fds", Characteristic),
        border = fp_border_default(width = 0.5, color = "gray")) %>%
  # Bold the main variable categories
  bold(i = ~ grepl("^age$|^dur$|^subtype$|^fds$", Characteristic), part = "body") %>%
  autofit() %>%
  set_table_properties(layout = "autofit", width = 1)

# Print and save functionality (similar to script 2A)
print("Demographic table created successfully")

# Export to Word
read_docx() %>%
  body_add_flextable(demo_flextable) %>%
  print(target = "2 Demographic Table (Supp Taple 1).docx")

print("Docx file saved as: 2 Demographic Table (Supp Taple 1) ")
