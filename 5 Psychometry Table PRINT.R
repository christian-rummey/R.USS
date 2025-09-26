library(flextable)
library(officer)

# Create flextable scaled to full page width
colnames(tab2.combined) <- gsub("\\.", "\n", colnames(tab2.combined))

# Get the original data
df <- tab2.combined

# Extract column names and split by \n
col_names <- colnames(df)
header_df <- tibble(
  col_keys = col_names,
  level1 = if_else(str_detect(col_names, "\n"), str_extract(col_names, "^[^\n]+"), ""),  # before \n
  level2 = if_else(str_detect(col_names, "\n"), str_extract(col_names, "[^\n]+$"), col_names)  # after \n
)

ft <- flextable(df) %>%
  set_header_df(mapping = header_df, key = "col_keys") %>%
  theme_vanilla() %>%
  fontsize(size = 8, part = "all") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(i = ~ grepl("No\\. of items|Corrected item|Subscale characteristics|Subscale intercorrelations|Correlation with other parameters", Property),
       part = "body") %>%
  set_table_properties(layout = "autofit", width = 1) %>%
  merge_h(part = "header") %>%      # Merge same top-level headers across columns
  merge_v(j = 1, part = "body")     # Optional: merge identical cells in first column

ft %<>% 
  set_table_properties(layout = "autofit", width = 1)  # make table 100% page width

# Export to Word in portrait orientation
read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "Psychometric_Properties_SCA_vs_FRDA.docx")
