# Figure 6:  Carbon and nitrogen values for historical plants and  --------

birdsForest <- birds[birds$Sample.Type == "Animal - feather",] %>%
  filter(!(Species == "Cyanoramphus novaezelandiae" & percentageNitrogen < 10))

plantsRangitāhua <- plants %>%
  filter(Species != "Pyrrosia serpens") %>%
  filter(Locality.Main2 == "Rangitāhua")

birdsForestBackup <- birdsForest
plantsRangitāhuaBackup <- plantsRangitāhua

# Mutating the label just for birds as they were collected a tad later than plants
birdsForest <- birdsForest %>%
  mutate(
    Period = case_when(
      Period == "Period 1\n(1854-1920)" ~ "Period 1\n(1887-1920)",
      TRUE ~ Period
    ))

unique(birdsForest$Period)
unique(plantsRangitāhua$Period)

# Now convert to factors
birdsForest$Period <- as.factor(birdsForest$Period)
plantsRangitāhua$Period <- as.factor(plantsRangitāhua$Period)

# Define colors for all periods
period_colors <- c(
  "Period 1\n(1854-1920)" = "#2C3E50", 
  "Period 2\n(1921-1983)" = "#5D8AA8",   # Light blue
  "Period 3\n(1984-2001)" = "#F4D03F",    # Golden yellow
  "Period 4\n(2002-2023)" = "#E74C3C",     # Bright red
  "Period 1\n(1887-1920)" = "#2C3E50"
)


# Define the significance symbol function
get_significance_symbol <- function(p_value) {
  if (p_value < 0.001) return("***")
  else if (p_value < 0.01) return("**") 
  else if (p_value < 0.05) return("*")
  else return(NA)  # No symbol for non-significant comparisons
}

# Function to perform pairwise Wilcoxon tests and extract significant comparisons
perform_wilcoxon_tests <- function(data, variable_name) {
  # First, remove any periods with only 1 or 0 observations
  period_counts <- table(data$Period)
  valid_periods <- names(period_counts[period_counts > 1])
  
  # Skip if no valid data
  if (length(valid_periods) < 2) {
    return(list(
      comparisons = list(), 
      annotations = c(),
      valid_periods = valid_periods
    ))
  }
  
  # Create a filtered dataset with only valid periods
  filtered_data <- data[data$Period %in% valid_periods, ]
  
  # Make sure Period is a factor with only the levels that exist in the filtered data
  filtered_data$Period <- factor(filtered_data$Period)
  
  # Create empty lists for results
  comparisons <- list()
  annotations <- c()
  
  # Run pairwise Wilcoxon test
  pairwise_result <- pairwise.wilcox.test(
    filtered_data[[variable_name]], 
    filtered_data$Period,
    p.adjust.method = "bonferroni",
    paired = FALSE
  )
  
  # Extract p-values from the result
  p_values <- pairwise_result$p.value
  
  # Check if we have any results (the matrix might be empty if no valid comparisons)
  if (nrow(p_values) > 0 && ncol(p_values) > 0) {
    # Process p-values matrix to find significant results
    for (i in 1:nrow(p_values)) {
      row_name <- rownames(p_values)[i]
      for (j in 1:ncol(p_values)) {
        col_name <- colnames(p_values)[j]
        
        # Skip if NA (diagonal or redundant comparison)
        if (!is.na(p_values[i, j])) {
          # Get p-value and determine if significant
          p_value <- p_values[i, j]
          symbol <- get_significance_symbol(p_value)
          
          if (!is.na(symbol)) {
            # Add to comparison list
            comparisons <- c(comparisons, list(c(row_name, col_name)))
            annotations <- c(annotations, symbol)
          }
        }
      }
    }
  }
  
  return(list(
    comparisons = comparisons, 
    annotations = annotations,
    valid_periods = valid_periods
  ))
}

# Let's check which periods actually exist in the data
cat("Periods in bird data:", levels(droplevels(birdsForest$Period)), "\n")
cat("Periods in plant data:", levels(droplevels(plantsRangitāhua$Period)), "\n")

# Run the tests on the datasets
# 1. Forest birds δ15N
birds_d15N_test <- perform_wilcoxon_tests(
  birdsForest, 
  "d15N"
)

# 2. Forest birds δ13C
birds_d13C_test <- perform_wilcoxon_tests(
  birdsForest, 
  "d13CSeussCorrected"
)

# 3. Terrestrial plants δ15N
plants_d15N_test <- perform_wilcoxon_tests(
  plantsRangitāhua, 
  "d15N"
)

# 4. Terrestrial plants δ13C
plants_d13C_test <- perform_wilcoxon_tests(
  plantsRangitāhua, 
  "d13CSeussCorrected"
)



# Create a function to generate the plots that handles missing data gracefully
create_boxplot <- function(data, y_var, title, y_label, test_results, panel_label, keep_all_levels = FALSE) {
  
  # Filter for non-NA y values (needed for calculating data-driven limits)
  data_filtered <- data[!is.na(data[[y_var]]), ]
  
  # Calculate sample sizes for each period (only for periods with data)
  sample_sizes <- data %>%
    group_by(Period) %>%
    summarise(n = n(), .groups = 'drop')
  
  # Create labels with sample sizes
  sample_sizes$label <- paste0("n=", sample_sizes$n)
  
  # Create the boxplot using the original data to maintain all factor levels
  p <- ggplot(data, aes(x = Period, y = .data[[y_var]], fill = Period)) +
    geom_boxplot(outlier.shape = NA) +
    scale_fill_manual(values = period_colors, na.value = "grey90") +
    labs(
      title = paste(panel_label, title),
      x = "",
      y = y_label
    ) +
    {if(grepl("δ¹⁵N|δ15N", title)) ylim(-5, 20)} +
    {if(grepl("δ¹³C|δ13C", title)) ylim(-35, -10)} +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(hjust = 1, size = 14),
      axis.title.y = element_text(size = 14),
      legend.position = "none"
    )
  
  # Determine y-limits based on plot type
  if(grepl("δ¹⁵N|δ15N", title)) {
    y_min <- -5
    y_max <- 20
  } else if(grepl("δ¹³C|δ13C", title)) {
    y_min <- -35
    y_max <- -10
  } else {
    # For other plot types, use data-driven limits
    if (nrow(data_filtered) > 0) {
      y_min <- min(data_filtered[[y_var]], na.rm = TRUE)
      y_max <- max(data_filtered[[y_var]], na.rm = TRUE)
    } else {
      # Fallback if no data
      y_min <- 0
      y_max <- 1
    }
  }
  
  # Create sample size labels for all periods (including those with n=0, if any)
  all_sample_sizes <- data %>%
    group_by(Period) %>%
    summarise(n = sum(!is.na(.data[[y_var]])), .groups = 'drop')
  
  all_sample_sizes$label <- paste0("n=", all_sample_sizes$n)
  
  # Add sample size labels using the y-limits
  p <- p + 
    geom_text(data = all_sample_sizes, 
              aes(x = Period, y = y_min + 0.00001 * (y_max - y_min), 
                  label = label), 
              inherit.aes = FALSE, 
              size = 4, 
              vjust = 0)
  
  # Add significance indicators if there are any
  if (length(test_results$comparisons) > 0) {
    # Calculate appropriate y_position for each comparison
    y_positions <- seq(from = y_max - 0.1 * (y_max - y_min), 
                       by = -0.08 * (y_max - y_min), 
                       length.out = length(test_results$comparisons))
    
    p <- p +
      geom_signif(
        comparisons = test_results$comparisons,
        annotations = test_results$annotations,
        map_signif_level = FALSE,
        step_increase = 0,  # Set to 0 to prevent automatic positioning
        tip_length = 0.01,
        vjust = 0.3,
        y_position = y_positions  # Use our calculated positions
      )
  }
  
  return(p)
}



# 1. Terrestrial plants δ13C
p1 <- create_boxplot(
  data = plantsRangitāhua,
  y_var = "d13CSeussCorrected",
  title = "(A) Terrestrial Plants δ¹³C",
  y_label = expression(delta^13*C~"(‰)"),
  test_results = plants_d13C_test,
  panel_label = ""
)

# 2. Forest birds δ13C
p2 <- create_boxplot(
  data = birdsForest,
  y_var = "d13CSeussCorrected",
  title = "(B) Terrestrial Birds δ¹³C",
  y_label = expression(delta^13*C~"(‰)"),
  test_results = birds_d13C_test,
  panel_label = ""
  #keep_all_levels = TRUE
)

# 3. Terrestrial plants δ15N
p3 <- create_boxplot(
  data = plantsRangitāhua,
  y_var = "d15N",
  title = "(C) Terrestrial Plants δ¹⁵N",
  y_label = expression(delta^15*N~"(‰)"),
  test_results = plants_d15N_test,
  panel_label = ""
)

# 4. Forest birds δ15N
p4 <- create_boxplot(
  data = birdsForest,
  y_var = "d15N",
  title = "(D) Terrestrial Birds δ¹⁵N",
  y_label = expression(delta^15*N~"(‰)"),
  test_results = birds_d15N_test,
  panel_label = ""
  #keep_all_levels = TRUE
)

# Add a check for empty plots before combining
plot_list <- list(p1, p2, p3, p4)
empty_plots <- sapply(plot_list, function(p) {
  is.null(p$data) || nrow(p$data) == 0
})

if (all(empty_plots)) {
  # All plots are empty, create a message
  cat("ERROR: No valid data found for any of the plots.\n")
  cat("Please check the datasets to ensure they contain the expected columns and values.\n")
} else {
  # Combine the plots with shared titles and overall caption
  require(patchwork)
  
  # Create the combined plot with shared titles
  combined_plot <- p1 + p2 + p3 + p4 + 
    plot_annotation(
      theme = theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text = element_text(size = 14)
      )
    )
  
  print(combined_plot)
}



# And then, the stats used ------------------------------------------------


birdsForest1 <- birdsForest %>%
  filter(Period == "Period 1\n(1887-1920)")

birdsForest2 <- birdsForest %>%
  filter(Period == "Period 2\n(1921-1983)")

birdsForest3 <- birdsForest %>%
  filter(Period == "Period 3\n(1984-2001)")

birdsForest4 <- birdsForest %>%
  filter(Period == "Period 4\n(2002-2023)")


median(birdsForest1$d13CSeussCorrected)
median(birdsForest2$d13CSeussCorrected)
median(birdsForest3$d13CSeussCorrected)
median(birdsForest4$d13CSeussCorrected)

median(birdsForest1$d15N)
median(birdsForest2$d15N)
median(birdsForest3$d15N)
median(birdsForest4$d15N)






### ===== Wilcoxon birds d15N 
d15N_bf_12 <- wilcox.test(birdsForest1$d15N, birdsForest2$d15N)
d15N_bf_12$statistic
d15N_bf_12$p.value
p.adjust(d15N_bf_12$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d15N_bf_13 <- wilcox.test(birdsForest1$d15N, birdsForest3$d15N)
d15N_bf_13$statistic
d15N_bf_13$p.value
p.adjust(d15N_bf_13$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d15N_bf_14 <- wilcox.test(birdsForest1$d15N, birdsForest4$d15N)
d15N_bf_14$statistic
d15N_bf_14$p.value
p.adjust(d15N_bf_14$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d15N_bf_23 <- wilcox.test(birdsForest2$d15N, birdsForest3$d15N)
d15N_bf_23$statistic
d15N_bf_23$p.value
p.adjust(d15N_bf_23$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d15N_bf_24 <- wilcox.test(birdsForest2$d15N, birdsForest4$d15N)
d15N_bf_24$statistic
d15N_bf_24$p.value
p.adjust(d15N_bf_24$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d15N_bf_34 <- wilcox.test(birdsForest3$d15N, birdsForest4$d15N)
d15N_bf_34$statistic
d15N_bf_34$p.value
p.adjust(d15N_bf_34$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons






### ===== Wilcoxon birds d13C
d13C_bf_12 <- wilcox.test(birdsForest1$d13CSeussCorrected, birdsForest2$d13CSeussCorrected)
d13C_bf_12$statistic
d13C_bf_12$p.value
p.adjust(d13C_bf_12$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d13C_bf_13 <- wilcox.test(birdsForest1$d13CSeussCorrected, birdsForest3$d13CSeussCorrected)
d13C_bf_13$statistic
d13C_bf_13$p.value
p.adjust(d13C_bf_13$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d13C_bf_14 <- wilcox.test(birdsForest1$d13CSeussCorrected, birdsForest4$d13CSeussCorrected)
d13C_bf_14$statistic
d13C_bf_14$p.value
p.adjust(d13C_bf_14$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d13C_bf_23 <- wilcox.test(birdsForest2$d13CSeussCorrected, birdsForest3$d13CSeussCorrected)
d13C_bf_23$statistic
d13C_bf_23$p.value
p.adjust(d13C_bf_23$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d13C_bf_24 <- wilcox.test(birdsForest2$d13CSeussCorrected, birdsForest4$d13CSeussCorrected)
d13C_bf_24$statistic
d13C_bf_24$p.value
p.adjust(d13C_bf_24$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d13C_bf_34 <- wilcox.test(birdsForest3$d13CSeussCorrected, birdsForest4$d13CSeussCorrected)
d13C_bf_34$statistic
d13C_bf_34$p.value
p.adjust(d13C_bf_34$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons







unique(plantsRangitāhua$Period)

plantsRangitāhua1 <- plantsRangitāhua %>%
  filter(Period == "Period 1\n(1854-1920)")

plantsRangitāhua2 <- plantsRangitāhua %>%
  filter(Period == "Period 2\n(1921-1983)")

plantsRangitāhua3 <- plantsRangitāhua %>%
  filter(Period == "Period 3\n(1984-2001)")

plantsRangitāhua4 <- plantsRangitāhua %>%
  filter(Period == "Period 4\n(2002-2023)")


median(plantsRangitāhua1$d13CSeussCorrected)
median(plantsRangitāhua2$d13CSeussCorrected)
median(plantsRangitāhua3$d13CSeussCorrected)
median(plantsRangitāhua4$d13CSeussCorrected)


median(plantsRangitāhua1$d15N)
median(plantsRangitāhua2$d15N)
median(plantsRangitāhua3$d15N)
median(plantsRangitāhua4$d15N)


### ===== Wilcoxon plants d15N 
d15N_pr_12 <- wilcox.test(plantsRangitāhua1$d15N, plantsRangitāhua2$d15N)
d15N_pr_12$statistic
d15N_pr_12$p.value
p.adjust(d15N_pr_12$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d15N_pr_13 <- wilcox.test(plantsRangitāhua1$d15N, plantsRangitāhua3$d15N)
d15N_pr_13$statistic
d15N_pr_13$p.value
p.adjust(d15N_pr_13$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d15N_pr_14 <- wilcox.test(plantsRangitāhua1$d15N, plantsRangitāhua4$d15N)
d15N_pr_14$statistic
d15N_pr_14$p.value
p.adjust(d15N_pr_14$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d15N_pr_23 <- wilcox.test(plantsRangitāhua2$d15N, plantsRangitāhua3$d15N)
d15N_pr_23$statistic
d15N_pr_23$p.value
p.adjust(d15N_pr_23$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d15N_pr_24 <- wilcox.test(plantsRangitāhua2$d15N, plantsRangitāhua4$d15N)
d15N_pr_24$statistic
d15N_pr_24$p.value
p.adjust(d15N_pr_24$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d15N_pr_34 <- wilcox.test(plantsRangitāhua3$d15N, plantsRangitāhua4$d15N)
d15N_pr_34$statistic
d15N_pr_34$p.value
p.adjust(d15N_pr_34$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons






### ===== Wilcoxon plants d13C
d13C_pr_12 <- wilcox.test(plantsRangitāhua1$d13CSeussCorrected, plantsRangitāhua2$d13CSeussCorrected)
d13C_pr_12$statistic
d13C_pr_12$p.value
p.adjust(d13C_pr_12$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d13C_pr_13 <- wilcox.test(plantsRangitāhua1$d13CSeussCorrected, plantsRangitāhua3$d13CSeussCorrected)
d13C_pr_13$statistic
d13C_pr_13$p.value
p.adjust(d13C_pr_13$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d13C_pr_14 <- wilcox.test(plantsRangitāhua1$d13CSeussCorrected, plantsRangitāhua4$d13CSeussCorrected)
d13C_pr_14$statistic
d13C_pr_14$p.value
p.adjust(d13C_pr_14$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d13C_pr_23 <- wilcox.test(plantsRangitāhua2$d13CSeussCorrected, plantsRangitāhua3$d13CSeussCorrected)
d13C_pr_23$statistic
d13C_pr_23$p.value
p.adjust(d13C_pr_23$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d13C_pr_24 <- wilcox.test(plantsRangitāhua2$d13CSeussCorrected, plantsRangitāhua4$d13CSeussCorrected)
d13C_pr_24$statistic
d13C_pr_24$p.value
p.adjust(d13C_pr_24$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons

d13C_pr_34 <- wilcox.test(plantsRangitāhua3$d13CSeussCorrected, plantsRangitāhua4$d13CSeussCorrected)
d13C_pr_34$statistic
d13C_pr_34$p.value
p.adjust(d13C_pr_34$p.value, method = "bonferroni", n = 6) #6 pairwise comparisons



