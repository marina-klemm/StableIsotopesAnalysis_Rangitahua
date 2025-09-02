# Figure 3: Soil Properties across Rangitāhua -----------------------------


RangitāhuaSoil <- read_xlsx("rawData/completeRangitahuaMeyersSoil.xlsx")
glimpse(RangitāhuaSoil)
summary(RangitāhuaSoil)

RangitāhuaSoil <- RangitāhuaSoil %>%
  mutate(
    Island = case_when(
      Island == "Rangitāhua Island" ~ "Rangitāhua",
      TRUE ~ Island
    ),
    Island = case_when(
      Island == "Meyer Islands" ~ "Meyer Islands",
      TRUE ~ Island
    )
  )

RangitāhuaSoil$`Total N`


# First, check assumptions for each variable
##### pH 
shapiro.test(RangitāhuaSoil$ph[RangitāhuaSoil$Island == "Meyer Islands"])
shapiro.test(RangitāhuaSoil$ph[RangitāhuaSoil$Island == "Rangitāhua"])

# Since both have a normal distribution use Welch's t-test
t.test(ph ~ Island, data = RangitāhuaSoil)

##### total N 
shapiro.test(RangitāhuaSoil$`Total N`[RangitāhuaSoil$Island == "Meyer Islands"])
shapiro.test(RangitāhuaSoil$`Total N`[RangitāhuaSoil$Island == "Rangitāhua"])

# Since both have a normal distribution use Welch's t-test
t.test(`Total N` ~ Island, data = RangitāhuaSoil)


##### `Total P` 
shapiro.test(RangitāhuaSoil$`Total P`[RangitāhuaSoil$Island == "Meyer Islands"])
shapiro.test(RangitāhuaSoil$`Total P`[RangitāhuaSoil$Island == "Rangitāhua"])

# Since both have a normal distribution use Welch's t-test
t.test(`Total P` ~ Island, data = RangitāhuaSoil)


##### `Organic C`
shapiro.test(RangitāhuaSoil$`Organic C`[RangitāhuaSoil$Island == "Meyer Islands"])
shapiro.test(RangitāhuaSoil$`Organic C`[RangitāhuaSoil$Island == "Rangitāhua"])

# Since both have a normal distribution use Welch's t-test
t.test(`Organic C` ~ Island, data = RangitāhuaSoil)


### Testing them all, then:


# Function to test difference between islands for a variable
test_variable <- function(var_name) {
  # Handle variables with special characters
  if(grepl(":", var_name) || grepl(" ", var_name)) {
    formula <- as.formula(paste("`", var_name, "`", " ~ Island", sep=""))
  } else {
    formula <- as.formula(paste(var_name, "~ Island"))
  }
  
  result <- t.test(formula, data = RangitāhuaSoil)
  
  # Extract key information
  Meyer_mean <- result$estimate[1]
  Rangitāhua_mean <- result$estimate[2]
  p_value <- result$p.value
  significant <- ifelse(p_value < 0.05, "Yes", "No")
  
  # Return as a data frame row
  data.frame(
    Variable = var_name,
    Meyer_Mean = round(Meyer_mean, 2),
    Rangitāhua_Mean = round(Rangitāhua_mean, 2),
    P_value = format(p_value, digits = 4),
    Significant = significant,
    stringsAsFactors = FALSE
  )
}

# Variables to test - must be in quotes
variables <- c("ph", "Total N", "Total P", "Organic C", "C:N ratio")

# Run tests and combine results
results <- do.call(rbind, lapply(variables, test_variable))

# Print nicely formatted table
print(results)


soilSummary <- RangitāhuaSoil %>%
  group_by(Island) %>%
  summarise(
    n = n(),
    pH_mean = mean(ph, na.rm = TRUE),
    water_content_mean = mean(`Air dry soil water content`, na.rm = TRUE),
    organic_C_mean = mean(`Organic C`, na.rm = TRUE),
    organic_C_mean_se = sd(`Organic C`, na.rm = TRUE),
    total_N_mean = mean(`Total N`, na.rm = TRUE),
    CN_ratio_mean = mean(`C:N ratio`, na.rm = TRUE),
    total_P_mean = mean(`Total P`, na.rm = TRUE)
  )

print(soilSummary)




###### Plots

# Calculate summary statistics for each variable
summary_stats <- RangitāhuaSoil %>%
  group_by(Island) %>%
  summarise(
    # pH statistics
    ph_mean = mean(ph, na.rm = TRUE),
    ph_se = sd(ph, na.rm = TRUE) / sqrt(sum(!is.na(ph))),
    ph_ci_lower = ph_mean - 1.96 * ph_se,
    ph_ci_upper = ph_mean + 1.96 * ph_se,
    ph_n = sum(!is.na(ph)),
    
    # Total P statistics
    p_mean = mean(`Total P`, na.rm = TRUE),
    p_se = sd(`Total P`, na.rm = TRUE) / sqrt(sum(!is.na(`Total P`))),
    p_ci_lower = p_mean - 1.96 * p_se,
    p_ci_upper = p_mean + 1.96 * p_se,
    p_n = sum(!is.na(`Total P`)),
    
    # Total N statistics
    n_mean = mean(`Total N`, na.rm = TRUE),
    n_se = sd(`Total N`, na.rm = TRUE) / sqrt(sum(!is.na(`Total N`))),
    n_ci_lower = n_mean - 1.96 * n_se,
    n_ci_upper = n_mean + 1.96 * n_se,
    n_n = sum(!is.na(`Total N`))
  )

# Function to perform t-test and get p-value
get_p_value <- function(data, variable) {
  Meyer_data <- data[data$Island == "Meyer Islands", variable]
  Rangitāhua_data <- data[data$Island == "Rangitāhua", variable]
  
  # Remove NA values
  Meyer_clean <- Meyer_data[!is.na(Meyer_data)]
  Rangitāhua_clean <- Rangitāhua_data[!is.na(Rangitāhua_data)]
  
  if(length(Meyer_clean) > 1 & length(Rangitāhua_clean) > 1) {
    test_result <- t.test(Meyer_clean, Rangitāhua_clean)
    return(test_result$p.value)
  } else {
    return(NA)
  }
}

# Get p-values for each variable
ph_p <- get_p_value(RangitāhuaSoil, "ph")
p_p <- get_p_value(RangitāhuaSoil, "Total P")
n_p <- get_p_value(RangitāhuaSoil, "Total N")

# Function to format p-values
format_p <- function(p) {
  if(is.na(p)) return("ns")
  if(p < 0.001) return("***")
  if(p < 0.01) return("**")
  if(p < 0.05) return("*")
  return("ns")
}

# Color palette
colors <- c("Meyer Islands" = "#4169E1", "Rangitāhua" = "#2E8B57")

# pH plot
ph_plot <- ggplot(summary_stats, aes(x = Island, y = ph_mean, fill = Island)) +
  geom_col(alpha = 0.7, width = 0.6) +
  geom_errorbar(aes(ymin = ph_ci_lower, ymax = ph_ci_upper), 
                width = 0.2, size = 0.1) +
  #geom_text(aes(label = paste0("n=", ph_n)), 
  #         vjust = -3, size = 5) +
  scale_fill_manual(values = colors) +
  labs(title = "pH", 
       y = "",
       x = "") +
  annotate("text", x = 1.5, y = max(summary_stats$ph_ci_upper) * 1.1, 
           label = format_p(ph_p), size = 4) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

# Total Phosphorus plot
p_plot <- ggplot(summary_stats, aes(x = Island, y = p_mean, fill = Island)) +
  geom_col(alpha = 0.7, width = 0.6) +
  geom_errorbar(aes(ymin = p_ci_lower, ymax = p_ci_upper), 
                width = 0.2, size = 0.5) +
  #geom_text(aes(label = paste0("n=", p_n)), 
  #         vjust = -3, size = 5) +
  scale_fill_manual(values = colors) +
  labs(title = "Total Phosphorus", 
       y = "mg/kg",
       x = "") +
  annotate("text", x = 1.5, y = max(summary_stats$p_ci_upper) * 1.1, 
           label = format_p(p_p), size = 4) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

# Total Nitrogen plot
n_plot <- ggplot(summary_stats, aes(x = Island, y = n_mean, fill = Island)) +
  geom_col(alpha = 0.7, width = 0.6) +
  geom_errorbar(aes(ymin = n_ci_lower, ymax = n_ci_upper), 
                width = 0.2, size = 0.5) +
  #geom_text(aes(label = paste0("n=", n_n)), 
  #         vjust = -3, size = 5) +
  scale_fill_manual(values = colors) +
  labs(title = "Total Nitrogen", 
       y = "%",
       x = "") +
  annotate("text", x = 1.5, y = max(summary_stats$n_ci_upper) * 1.1, 
           label = format_p(n_p), size = 4) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

combined_plot <- grid.arrange(
  ph_plot, p_plot, n_plot,
  ncol = 3
  #top = textGrob("Key Soil Properties Across Islands", gp = gpar(fontsize = 18, fontface = "bold"))
)

print("Summary statistics:")
print(summary_stats)
print(paste("pH p-value:", round(ph_p, 4)))
print(paste("Total P p-value:", round(p_p, 4)))
print(paste("Total N p-value:", round(n_p, 4)))



