# Isotope Analysis: Carbon and Nitrogen Plots

prepare_data <- function(data) {
  # Find guilds with sufficient samples in both locations
  valid_groups <- data %>%
    group_by(Group_Guild, Locality.Main2) %>%
    summarise(count = n(), .groups = "drop") %>%
    filter(count >= 3) %>%  # Minimum 3 samples per group
    group_by(Group_Guild) %>%
    filter(n() == 2) %>%  # Must appear in both locations
    ungroup()
  
  valid_guild_names <- unique(valid_groups$Group_Guild)
  
  # Filter data and calculate sample counts
  filtered_data <- data %>%
    filter(Group_Guild %in% valid_guild_names)
  
  sample_counts <- filtered_data %>%
    group_by(Group_Guild, Locality.Main2) %>%
    summarise(n = n(), .groups = "drop")
  
  list(
    data = filtered_data,
    sample_counts = sample_counts,
    valid_guilds = valid_guild_names
  )
}

create_split_labels <- function(labels, isotope_type = "carbon") {
  # Apply consistent three-line formatting using direct vector replacement
  result <- labels
  
  # Define all replacements
  replacements <- c(
    "Algae (Primary producer)" = "Algae\n(Primary\nproducer)",
    "Plants (Primary producer)" = "Plants\n(Primary\nproducer)",
    "Terrestrial Invertebrates (Omnivore/Detritivore)" = "Terrestrial\nInvertebrates\n(Omni./Det.)",
    "Terrestrial Invertebrates (Herbivore)" = "Terrestrial\nInvertebrates\n(Herbivore)",
    "Terrestrial Invertebrates (Carnivore)" = "Terrestrial\nInvertebrates\n(Carnivore)",
    "Fish (Carnivore)" = "Fish\n(Carnivore)",
    "Fish (Omnivore/Planktivore)" = "Fish\n(Omni./Plank.)",
    "Marine Invertebrates (Omnivore/Detritivore)" = "Marine\nInvertebrates\n(Omni./Det.)",
    "Marine Invertebrates (Herbivore)" = "Marine\nInvertebrates\n(Herbivore)",
    "Marine Invertebrates (Carnivore)" = "Marine\nInvertebrates\n(Carnivore)"
  )
  
  # Replace known patterns
  for (i in seq_along(replacements)) {
    result[result == names(replacements)[i]] <- replacements[i]
  }
  
  # For any remaining labels with parentheses, do a simple split
  needs_splitting <- grepl("\\(", result) & !grepl("\n", result)
  if (any(needs_splitting)) {
    result[needs_splitting] <- gsub(" \\(", "\n\\(", result[needs_splitting])
  }
  
  return(result)
}



# Guild Ordering ----
get_guild_order <- function(valid_guilds, exclude_primary_producers = FALSE) {
  full_order <- c(
    # Terrestrial primary producers
    "Plants (Primary producer)",
    # Terrestrial consumers
    "Terrestrial Invertebrates (Herbivore)",
    "Terrestrial Invertebrates (Omnivore/Detritivore)",
    "Terrestrial Invertebrates (Carnivore)",
    # Marine primary producers
    "Algae (Primary producer)",
    # Marine consumers
    "Marine Invertebrates (Primary producer)",
    "Marine Invertebrates (Herbivore)",
    "Marine Invertebrates (Omnivore/Detritivore)",
    "Marine Invertebrates (Carnivore)",
    "Marine Invertebrates (Planktivore)",
    "Fish (Primary producer)",
    "Fish (Herbivore)",
    "Fish (Omnivore/Detritivore)",
    "Fish (Omnivore/Planktivore)",
    "Fish (Planktivore)",
    "Fish (Carnivore)"
  )
  
  if (exclude_primary_producers) {
    full_order <- full_order[!grepl("Primary producer", full_order)]
  }
  
  # Return only guilds present in data, maintaining order
  full_order[full_order %in% valid_guilds]
}


create_isotope_plot <- function(data, sample_counts, results_df, isotope_var, 
                                isotope_label, isotope_type = "carbon") {
  # Get y-axis limits
  y_values <- data[[isotope_var]]
  min_y <- min(y_values, na.rm = TRUE)
  max_y <- max(y_values, na.rm = TRUE)
  
  # Prepare guild ordering
  valid_guilds <- unique(data$Group_Guild)
  guild_order <- get_guild_order(valid_guilds)
  
  # Create split labels
  data$Group_Guild_split <- create_split_labels(data$Group_Guild, isotope_type)
  sample_counts$Group_Guild_split <- create_split_labels(sample_counts$Group_Guild, isotope_type)
  split_levels <- create_split_labels(guild_order, isotope_type)
  
  # Identify significant groups
  sig_guilds <- results_df %>%
    filter(p_value < 0.05) %>%
    pull(Group)
  
  # Base plot
  p <- ggplot(data, aes(x = factor(Group_Guild_split, levels = split_levels), 
                        y = .data[[isotope_var]])) +
    geom_boxplot(
      aes(fill = Locality.Main2),
      alpha = 0.8, 
      outlier.shape = 21, 
      position = position_dodge(width = 0.8)
    ) +
    geom_text(
      data = sample_counts,
      aes(x = Group_Guild_split, y = -Inf, label = paste0("n=", n), 
          group = Locality.Main2),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 3.5
    )
  
  # Add significance brackets if needed
  if (length(sig_guilds) > 0) {
    annotation_data <- results_df %>%
      filter(Group %in% sig_guilds) %>%
      mutate(
        Group_Guild_split = create_split_labels(Group, isotope_type),
        y_pos = if (isotope_type == "carbon") max_y + 2 else max_y,
        label = case_when(
          p_value < 0.001 ~ "***",
          p_value < 0.01 ~ "**",
          p_value < 0.05 ~ "*",
          TRUE ~ "ns"
        )
      )
    
    p <- p +
      geom_bracket(
        data = annotation_data,
        aes(xmin = Group_Guild_split, xmax = Group_Guild_split, 
            y.position = y_pos, label = label),
        inherit.aes = FALSE,
        tip.length = 0.02,
        bracket.size = 0.5
      )
  }
  
  # Styling and labels
  y_limit_upper <- if (isotope_type == "carbon") max_y + 5 else max_y
  y_limit_lower <- if (isotope_type == "carbon") min_y - 2 else NA
  
  p +
    scale_fill_manual(
      values = c("Rangitāhua" = "#2E8B57", "Meyer Islands" = "#4169E1"),
      name = "Location"
    ) +
    coord_cartesian(ylim = c(y_limit_lower, y_limit_upper)) +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 14, face = "bold"),
      axis.title.x = element_blank(),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      panel.grid.minor = element_blank()
    ) +
    labs(y = isotope_label)
}


prepared_data <- prepare_data(dataForWilcoxon2002)
filtered_data <- prepared_data$data
filtered_sample_counts <- prepared_data$sample_counts

# Create Carbon plot
C_combined <- create_isotope_plot(
  data = filtered_data,
  sample_counts = filtered_sample_counts,
  results_df = d13C_results_df,
  isotope_var = "d13CSeussCorrected",
  isotope_label = expression(delta^13*C~("‰")),
  isotope_type = "carbon"
)

# Create Nitrogen plot
N_combined <- create_isotope_plot(
  data = filtered_data,
  sample_counts = filtered_sample_counts,
  results_df = d15N_results_df,
  isotope_var = "d15N",
  isotope_label = expression(delta^15*N~("‰")),
  isotope_type = "nitrogen"
)

# Display plots
C_combined
N_combined


island_summaries <- filtered_data %>%
  group_by(Group_Guild, Locality.Main2) %>%
  summarise(
    sample_size = n(),
    median_d13C = median(d13CSeussCorrected, na.rm = TRUE),
    median_d15N = median(d15N, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Group_Guild, Locality.Main2)


