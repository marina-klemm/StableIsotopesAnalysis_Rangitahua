# Table for the paper with average d15N and d13C for each spp -------------

# In this study, the datasets used were:

birdsForest
plantsRangitāhua
#combined_data: actually not, I filtered it further to keep only n=3 for both motu
filtered_data

## Combine filtered_data, plantsRangitāhua and forestBirdsRangitāhua:
# Add the GroupGuild column to plantsRangitāhua and forestBirdsRangitāhua if it doesn't exist
if(!"GroupGuild" %in% colnames(plantsRangitāhua)) {
  plantsRangitāhua$GroupGuild <- paste(plantsRangitāhua$Group, plantsRangitāhua$Guild, sep = "_")
}

if(!"GroupGuild" %in% colnames(birdsForest)) {
  birdsForest$GroupGuild <- paste(birdsForest$Group, birdsForest$Guild, sep = "_")
}

allDataThisPaper <- bind_rows(birdsForest, plantsRangitāhua, filtered_data)

allDataThisPaper <- allDataThisPaper %>%
  distinct(Identifier.1, .keep_all = TRUE)
str(allDataThisPaper)

## Change SampleType for this table:

allDataThisPaper$SampleTypeForTable <- allDataThisPaper$SampleType

allDataThisPaper <- allDataThisPaper %>%
  mutate(SampleTypeForTable = SampleTypeForTable %>%
           # Remove all unwanted prefixes and suffixes in sequence
           gsub("Animal - ", "", ., ignore.case = TRUE) %>%
           gsub("Plant - ", "", ., ignore.case = TRUE) %>%
           gsub("Zooplankton - ", "", ., ignore.case = TRUE) %>%
           gsub(", terrestrial", "", ., ignore.case = TRUE) %>%
           gsub(", marine", "", ., ignore.case = TRUE)
  )

allDataThisPaper <- allDataThisPaper %>%
  mutate(Species = ifelse(grepl("Turdus philomelos ", 
                                Species, ignore.case=TRUE),
                          "Turdus philomelos", Species))

#Then, capitalize the first word

allDataThisPaper <- allDataThisPaper %>%
  mutate(SampleTypeForTable = str_to_sentence(SampleTypeForTable))

unique(allDataThisPaper$SampleTypeForTable)

# Create a function to convert sample types to their initials
convert_to_initial <- function(sample_type) {
  if(is.na(sample_type)) return(NA)
  
  # Create a mapping of sample types to their initials
  initial_map <- c(
    "Guano" = "G",
    "Feather" = "F", 
    "Hair/fur" = "H",
    "Stem/stipe" = "S",
    "Leaf/blade" = "L",
    "Whole organism" = "W",
    "Muscle" = "M",
    "Fin clip" = "FC",
    "Community homogenate" = "CH",
    "Cartilage" = "Ca",
    "Claw" = "Cl",
    "Zooplankton, individual" = "Z"
  )
  
  return(initial_map[sample_type])
}

# Store the original sample types for reference before changing to initials
allDataThisPaper$OriginalSampleType <- allDataThisPaper$SampleTypeForTable

# Convert sample types to initials
allDataThisPaper <- allDataThisPaper %>%
  mutate(SampleTypeForTable = sapply(SampleTypeForTable, convert_to_initial))

# Print unique sample types before and after conversion
cat("Original sample types:\n")
print(unique(allDataThisPaper$OriginalSampleType))
cat("\nSample type initials:\n")
print(unique(allDataThisPaper$SampleTypeForTable))

# Function to extract period numbers
extract_period_numbers <- function(periods) {
  if(length(periods) == 0 || all(is.na(periods))) return(NA)
  
  period_numbers <- c()
  for(period in periods) {
    if(is.na(period)) next
    # Extract the period number
    match <- regexpr("Period (\\d+)", period)
    if(match != -1) {
      number <- as.numeric(substr(period, match + 7, match + 7))
      period_numbers <- c(period_numbers, number)
    }
  }
  
  if(length(period_numbers) == 0) return(NA)
  
  # Sort the period numbers and return as comma-separated string
  return(paste(sort(unique(period_numbers)), collapse = ", "))
}


# Create trophic level ordering function
create_trophic_order <- function(guild) {
  trophic_order <- c("Primary producer",
                     "Herbivore",
                     "Omnivore/Detritivore",
                     "Omnivore/Planktivore",
                     "Planktivore",
                     "Carnivore/Planktivore",
                     "Carnivore")
  
  # Return the position in the trophic order, or a high number if not found
  position <- match(guild, trophic_order)
  if(is.na(position)) {
    return(999) # Put unknown guilds at the end
  }
  return(position)
}

# Add trophic ordering to the data
allDataThisPaper <- allDataThisPaper %>%
  mutate(TrophicOrder = sapply(GuildCapitalized, create_trophic_order))

# Create trophic level ordering function
create_group_order <- function(guild) {
  group_order <- c("Plant",
                   "Macroalga - brown",
                   "Macroalga - green",
                   "Macroalga - red",
                   "Ant",
                   "Bee",
                   "Beetle",
                   "Planthoppers",
                   "Bird",
                   "Mussel",
                   "Limpet",
                   "Oyster",
                   "Zooplankton",
                   "Sea urchin",
                   "Sea snail",
                   "Starfish",
                   "Rock crab",
                   "Hermit crab",
                   "Barnacle",
                   "Fish")
  
  # Return the position in the trophic order, or a high number if not found
  position <- match(guild, group_order)
  if(is.na(position)) {
    return(999) # Put unknown guilds at the end
  }
  return(position)
}

# Add trophic ordering to the data
allDataThisPaper <- allDataThisPaper %>%
  mutate(GroupOrder = sapply(CommonGroupName, create_group_order))

allDataThisPaper <- allDataThisPaper %>%
  arrange(-TrophicOrder, -GroupOrder)

# Function to calculate statistics for each group
calculate_isotope_stats <- function(data) {
  data %>%
    # Add GuildCapitalized to the grouping
    group_by(Terres.Marine, GuildCapitalized, CommonGroupName, Species, TrophicOrder) %>%
    summarize(
      n = n(),
      CN_mean = mean(as.numeric(C.N.mass.ratio), na.rm = TRUE),
      CN_sd = sd(as.numeric(C.N.mass.ratio), na.rm = TRUE),
      d13C_min = min(d13CSeussCorrected, na.rm = TRUE),
      d13C_max = max(d13CSeussCorrected, na.rm = TRUE),
      d13C_mean = mean(d13CSeussCorrected, na.rm = TRUE),
      d13C_sd = sd(d13CSeussCorrected, na.rm = TRUE),
      d15N_min = min(d15N, na.rm = TRUE),
      d15N_max = max(d15N, na.rm = TRUE),
      d15N_mean = mean(d15N, na.rm = TRUE),
      d15N_sd = sd(d15N, na.rm = TRUE),
      # Calculate the count for each locality
      Meyers_count = sum(Locality.Main2 == "Meyer Islands", na.rm = TRUE),
      Rangitāhua_count = sum(Locality.Main2 == "Rangitāhua", na.rm = TRUE),
      # Collect all periods for the group and format them
      Periods_Sampled = extract_period_numbers(unique(Period)),
      # Collect all sample types for the group as initials and combine into a single string
      Sample_Types = paste(sort(unique(SampleTypeForTable[!is.na(SampleTypeForTable)])), collapse = ", ")
    ) %>%
    ungroup() %>%
    # First arrange by Terres.Marine, then by GuildCapitalized, then by CommonGroupName, then by Species
    arrange(Terres.Marine, TrophicOrder, GuildCapitalized, CommonGroupName, Species)
}

# Calculate statistics
isotope_stats <- calculate_isotope_stats(allDataThisPaper)

# Format the table
format_isotope_table <- function(stats) {
  # Create terrestrial and marine datasets
  terrestrial_data <- stats %>% filter(Terres.Marine == "Terrestrial")
  marine_data <- stats %>% filter(Terres.Marine == "Marine")
  
  # Add code numbers - reset for each guild within terrestrial/marine
  terrestrial_data <- terrestrial_data %>%
    group_by(GuildCapitalized) %>%
    mutate(Code = seq_len(n())) %>%
    ungroup()
  
  marine_data <- marine_data %>%
    group_by(GuildCapitalized) %>%
    mutate(Code = seq_len(n())) %>%
    ungroup()
  
  # Function to format numbers to 2 decimal places
  format_number <- function(x) {
    if(is.na(x)) return("-")
    sprintf("%.2f", x)
  }
  
  # Function to format a group into a table with feeding guild headers
  format_group <- function(data, group_name) {
    if(nrow(data) == 0) {
      return(data.frame(
        Code = paste("*No", tolower(group_name), "specimens were found in the dataset*"),
        Taxa = "", 
        Common_Name = "",
        Feeding_Guild = "", # New column
        n = "", 
        Meyers = "",
        Rangitāhua = "",
        Periods = "", # New column
        CN = "", 
        d13C_min = "", d13C_max = "", d13C_mean = "", d13C_sd = "",
        d15N_min = "", d15N_max = "", d15N_mean = "", d15N_sd = "",
        Sample_Type = ""
      ))
    }
    
    # Create rows for the table
    rows <- data.frame()
    
    # Get unique guilds
    guilds <- unique(data$GuildCapitalized)
    
    for(guild in guilds) {
      # Add a guild header row
      guild_header <- data.frame(
        Code = "",
        Taxa = guild, 
        Common_Name = "",
        Feeding_Guild = "",
        n = "", 
        Meyers = "",
        Rangitāhua = "",
        Periods = "",
        CN = "", 
        d13C_min = "", d13C_max = "", d13C_mean = "", d13C_sd = "",
        d15N_min = "", d15N_max = "", d15N_mean = "", d15N_sd = "",
        Sample_Type = ""
      )
      
      rows <- rbind(rows, guild_header)
      
      # Filter data for this guild
      guild_data <- data %>% filter(GuildCapitalized == guild)
      
      for(i in 1:nrow(guild_data)) {
        # Scientific name row
        sci_row <- data.frame(
          Code = guild_data$Code[i],
          Taxa = guild_data$Species[i],
          Common_Name = guild_data$CommonGroupName[i],
          Feeding_Guild = guild_data$GuildCapitalized[i],
          n = guild_data$n[i],
          Meyers = guild_data$Meyers_count[i],
          Rangitāhua = guild_data$Rangitāhua_count[i],
          Periods = guild_data$Periods_Sampled[i],
          CN = paste0(format_number(guild_data$CN_mean[i]), "±", format_number(guild_data$CN_sd[i])),
          d13C_min = format_number(guild_data$d13C_min[i]),
          d13C_max = format_number(guild_data$d13C_max[i]),
          d13C_mean = format_number(guild_data$d13C_mean[i]),
          d13C_sd = format_number(guild_data$d13C_sd[i]),
          d15N_min = format_number(guild_data$d15N_min[i]),
          d15N_max = format_number(guild_data$d15N_max[i]),
          d15N_mean = format_number(guild_data$d15N_mean[i]),
          d15N_sd = format_number(guild_data$d15N_sd[i]),
          Sample_Type = guild_data$Sample_Types[i]
        )
        
        rows <- rbind(rows, sci_row)
      }
    }
    
    return(rows)
  }
  
  # Format terrestrial and marine data
  terrestrial_formatted <- format_group(terrestrial_data, "Terrestrial ecosystem")
  marine_formatted <- format_group(marine_data, "Marine ecosystem")
  
  # Create header rows
  terrestrial_header <- data.frame(
    Code = "Terrestrial ecosystem",
    Taxa = "", 
    Common_Name = "",
    Feeding_Guild = "",
    n = "", 
    Meyers = "",
    Rangitāhua = "",
    Periods = "",
    CN = "", 
    d13C_min = "", d13C_max = "", d13C_mean = "", d13C_sd = "",
    d15N_min = "", d15N_max = "", d15N_mean = "", d15N_sd = "",
    Sample_Type = ""
  )
  
  marine_header <- data.frame(
    Code = "Marine ecosystem",
    Taxa = "", 
    Common_Name = "",
    Feeding_Guild = "",
    n = "", 
    Meyers = "",
    Rangitāhua = "",
    Periods = "",
    CN = "", 
    d13C_min = "", d13C_max = "", d13C_mean = "", d13C_sd = "",
    d15N_min = "", d15N_max = "", d15N_mean = "", d15N_sd = "",
    Sample_Type = ""
  )
  
  # Combine all parts
  final_table <- rbind(
    terrestrial_header,
    terrestrial_formatted,
    marine_header,
    marine_formatted
  )
  
  return(final_table)
}

# Create the formatted table
formatted_table <- format_isotope_table(isotope_stats)

# Print the table with proper formatting
print_table <- function(data) {
  # Use the alternative method which is more reliable
  colnames(data) <- c("Code", "Taxa", "Common Name", "Feeding Guild", "n", "Meyer Islands", "Rangitāhua", 
                      "Periods Sampled", "C:N", 
                      "Min", "Max", "Mean", "SD",  # d13C columns
                      "Min", "Max", "Mean", "SD",  # d15N columns
                      "Sample Type")
  
  # Create the table with kable
  table <- kable(data, format = "html", escape = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    add_header_above(c(" " = 9, "δ¹³C (‰)" = 4, "δ¹⁵N (‰)" = 4, " " = 1))
  
  return(table)
}

# Generate the final table
isotope_table <- print_table(formatted_table)

# Display the table
isotope_table



# Function to export the formatted table to Excel with styling
export_to_excel <- function(data, filename = "isotope_table.xlsx") {
  # Create a new workbook
  wb <- createWorkbook()
  
  # Add a worksheet
  addWorksheet(wb, "Isotope Data")
  
  # Create header row with merged cells for d13C and d15N
  writeData(wb, "Isotope Data", data, startRow = 3, startCol = 1, headerStyle = NULL)
  
  # Add main headers in row 1
  headerRow1 <- c("", "", "", "", "", "", "", "", "", rep("δ¹³C (‰)", 4), rep("δ¹⁵N (‰)", 4), "")
  writeData(wb, "Isotope Data", t(headerRow1), startRow = 1, startCol = 1)
  
  # Add secondary header in row 2 - use column names that match the formatted_table structure
  headerRow2 <- c("Code", "Species", "Common Name", "Feeding Guild", "n", "Meyer Islands", "Rangitāhua", 
                  "Periods Sampled", "C:N", 
                  "Min", "Max", "Mean", "SD",  # d13C columns
                  "Min", "Max", "Mean", "SD",  # d15N columns
                  "Sample Type")
  writeData(wb, "Isotope Data", t(headerRow2), startRow = 2, startCol = 1)
  
  # Merge cells for the main headers (δ¹³C and δ¹⁵N)
  mergeCells(wb, "Isotope Data", cols = 10:13, rows = 1)  # δ¹³C
  mergeCells(wb, "Isotope Data", cols = 14:17, rows = 1)  # δ¹⁵N
  
  # Set column widths
  setColWidths(wb, "Isotope Data", cols = 1, widths = 8)      # Code
  setColWidths(wb, "Isotope Data", cols = 2, widths = 25)     # Taxa
  setColWidths(wb, "Isotope Data", cols = 3, widths = 20)     # Common Name
  setColWidths(wb, "Isotope Data", cols = 4, widths = 18)     # Feeding Guild
  setColWidths(wb, "Isotope Data", cols = 5:7, widths = 10)   # n, Meyers, Rangitāhua
  setColWidths(wb, "Isotope Data", cols = 8, widths = 15)     # Periods Sampled
  setColWidths(wb, "Isotope Data", cols = 9, widths = 10)     # C:N
  setColWidths(wb, "Isotope Data", cols = 10:17, widths = 10) # d13C and d15N columns
  setColWidths(wb, "Isotope Data", cols = 18, widths = 15)    # Sample Type
  
  # Create styles
  headerStyle <- createStyle(
    fontName = "Arial", fontSize = 11, textDecoration = "bold",
    halign = "center", valign = "center", 
    border = "bottom", borderStyle = "medium"
  )
  
  subheaderStyle <- createStyle(
    fontName = "Arial", fontSize = 11, textDecoration = "bold",
    halign = "center", valign = "center",
    border = "bottom", borderStyle = "thin"
  )
  
  ecosystemHeaderStyle <- createStyle(
    fontName = "Arial", fontSize = 12, textDecoration = "bold",
    fgFill = "#DDEBF7", halign = "left", valign = "center"
  )
  
  guildHeaderStyle <- createStyle(
    fontName = "Arial", fontSize = 11, textDecoration = "bold",
    fgFill = "#E2EFDA", halign = "left", valign = "center"
  )
  
  normalStyle <- createStyle(
    fontName = "Arial", fontSize = 10,
    halign = "center", valign = "center",
    border = "bottom", borderStyle = "hair"
  )
  
  taxaStyle <- createStyle(
    fontName = "Arial", fontSize = 10, textDecoration = "italic",
    halign = "left", valign = "center"
  )
  
  commonNameStyle <- createStyle(
    fontName = "Arial", fontSize = 10,
    halign = "left", valign = "center"
  )
  
  # Apply styles
  addStyle(wb, "Isotope Data", headerStyle, rows = 1, cols = 1:18, gridExpand = TRUE)
  addStyle(wb, "Isotope Data", subheaderStyle, rows = 2, cols = 1:18, gridExpand = TRUE)
  
  # Find ecosystem header rows (they start with "**Terrestrial ecosystem**" or "**Marine ecosystem**")
  ecosystem_rows <- which(grepl("^\\*\\*.*ecosystem\\*\\*$", as.character(data$Code)))
  ecosystem_rows <- ecosystem_rows + 2  # Adjust for the header rows
  
  # Find guild header rows (where Taxa has "**" but Code is empty)
  guild_rows <- which(grepl("^\\*\\*", as.character(data$Taxa)) & data$Code == "")
  guild_rows <- guild_rows + 2  # Adjust for the header rows
  
  # Apply ecosystem header style
  for(row in ecosystem_rows) {
    addStyle(wb, "Isotope Data", ecosystemHeaderStyle, rows = row, cols = 1:18, gridExpand = TRUE)
  }
  
  # Apply guild header style
  for(row in guild_rows) {
    addStyle(wb, "Isotope Data", guildHeaderStyle, rows = row, cols = 1:18, gridExpand = TRUE)
  }
  
  # Apply normal style to all data cells, excluding ecosystem and guild headers
  data_rows <- 3:(nrow(data) + 2)
  data_rows <- data_rows[!data_rows %in% c(ecosystem_rows, guild_rows)]  # Exclude headers
  addStyle(wb, "Isotope Data", normalStyle, rows = data_rows, cols = 1:18, gridExpand = TRUE)
  
  # Apply taxa style to the Taxa column
  addStyle(wb, "Isotope Data", taxaStyle, rows = data_rows, cols = 2, gridExpand = TRUE)
  
  # Apply common name style to the Common Name column
  addStyle(wb, "Isotope Data", commonNameStyle, rows = data_rows, cols = 3, gridExpand = TRUE)
  
  # Apply alternating row colors
  for(i in seq(1, length(data_rows), by=2)) {
    if(i <= length(data_rows)) {
      row <- data_rows[i]
      addStyle(wb, "Isotope Data", 
               createStyle(fgFill = "#F2F2F2"), 
               rows = row, cols = 1:18, gridExpand = TRUE)
    }
  }
  
  # Save the workbook
  saveWorkbook(wb, filename, overwrite = TRUE)
  cat("Excel file saved as:", filename, "\n")
}

# Clean up the formatted_table to prepare for Excel
prepare_for_excel <- function(formatted_table) {
  # Check if the input is a data frame, otherwise throw an error
  if (!is.data.frame(formatted_table)) {
    stop("The input must be a data frame (formatted_table). Make sure you're not using the HTML kable object.")
  }
  
  # Convert markdown formatting to plain text for Excel
  # Remove markdown formatting but keep the text structure
  excel_table <- formatted_table
  
  # Remove asterisks but keep italic formatting for taxa (Excel will handle this through styles)
  excel_table$Taxa <- gsub("\\*", "", excel_table$Taxa)
  
  # Convert bold markdown for ecosystem headers but keep the text
  excel_table$Code <- gsub("\\*\\*", "", excel_table$Code)
  
  return(excel_table)
}

# Alternative version with grouped locality columns
export_grouped_localities_to_excel <- function(data, filename = "isotope_table_grouped_Aug25.xlsx") {
  # Create a new workbook
  wb <- createWorkbook()
  
  # Add a worksheet
  addWorksheet(wb, "Isotope Data")
  
  # Write data starting at row 3
  writeData(wb, "Isotope Data", data, startRow = 3, startCol = 1, headerStyle = NULL)
  
  # Add main headers in row 1
  headerRow1 <- c("", "", "", "", "Locality", "", "", "Period", "", rep("δ¹³C (‰)", 4), rep("δ¹⁵N (‰)", 4), "")
  writeData(wb, "Isotope Data", t(headerRow1), startRow = 1, startCol = 1)
  
  # Add secondary header in row 2
  headerRow2 <- c("Code", "Species", "Common Name", "Feeding Guild", "Total", "Meyer Islands", "Rangitāhua", 
                  "Periods Sampled", "C:N", 
                  "Min", "Max", "Mean", "SD",  # d13C columns
                  "Min", "Max", "Mean", "SD",  # d15N columns
                  "Sample Type")
  writeData(wb, "Isotope Data", t(headerRow2), startRow = 2, startCol = 1)
  
  # Merge cells for the main headers
  mergeCells(wb, "Isotope Data", cols = 5:7, rows = 1)    # Locality
  mergeCells(wb, "Isotope Data", cols = 10:13, rows = 1)  # δ¹³C
  mergeCells(wb, "Isotope Data", cols = 14:17, rows = 1)  # δ¹⁵N
  
  # Set column widths
  setColWidths(wb, "Isotope Data", cols = 1, widths = 8)      # Code
  setColWidths(wb, "Isotope Data", cols = 2, widths = 25)     # Taxa
  setColWidths(wb, "Isotope Data", cols = 3, widths = 20)     # Common Name
  setColWidths(wb, "Isotope Data", cols = 4, widths = 18)     # Feeding Guild
  setColWidths(wb, "Isotope Data", cols = 5:7, widths = 10)   # Total, Meyers, Rangitāhua
  setColWidths(wb, "Isotope Data", cols = 8, widths = 15)     # Periods Sampled
  setColWidths(wb, "Isotope Data", cols = 9, widths = 10)     # C:N
  setColWidths(wb, "Isotope Data", cols = 10:17, widths = 10) # d13C and d15N columns
  setColWidths(wb, "Isotope Data", cols = 18, widths = 15)    # Sample Type
  
  # Create styles - same as in the previous function
  headerStyle <- createStyle(
    fontName = "Arial", fontSize = 11, textDecoration = "bold",
    halign = "center", valign = "center", 
    border = "bottom", borderStyle = "medium"
  )
  
  subheaderStyle <- createStyle(
    fontName = "Arial", fontSize = 11, textDecoration = "bold",
    halign = "center", valign = "center",
    border = "bottom", borderStyle = "thin"
  )
  
  ecosystemHeaderStyle <- createStyle(
    fontName = "Arial", fontSize = 12, textDecoration = "bold",
    fgFill = "#DDEBF7", halign = "left", valign = "center"
  )
  
  guildHeaderStyle <- createStyle(
    fontName = "Arial", fontSize = 11, textDecoration = "bold",
    fgFill = "#E2EFDA", halign = "left", valign = "center"
  )
  
  normalStyle <- createStyle(
    fontName = "Arial", fontSize = 10,
    halign = "center", valign = "center",
    border = "bottom", borderStyle = "hair"
  )
  
  taxaStyle <- createStyle(
    fontName = "Arial", fontSize = 10, textDecoration = "italic",
    halign = "left", valign = "center"
  )
  
  commonNameStyle <- createStyle(
    fontName = "Arial", fontSize = 10,
    halign = "left", valign = "center"
  )
  
  # Apply styles
  addStyle(wb, "Isotope Data", headerStyle, rows = 1, cols = 1:18, gridExpand = TRUE)
  addStyle(wb, "Isotope Data", subheaderStyle, rows = 2, cols = 1:18, gridExpand = TRUE)
  
  # Find ecosystem header rows
  ecosystem_rows <- which(grepl("^\\*\\*.*ecosystem\\*\\*$", as.character(data$Code)))
  ecosystem_rows <- ecosystem_rows + 2  # Adjust for the header rows
  
  # Find guild header rows (where Taxa has "**" but Code is empty)
  guild_rows <- which(grepl("^\\*\\*", as.character(data$Taxa)) & data$Code == "")
  guild_rows <- guild_rows + 2  # Adjust for the header rows
  
  # Apply ecosystem header style
  for(row in ecosystem_rows) {
    addStyle(wb, "Isotope Data", ecosystemHeaderStyle, rows = row, cols = 1:18, gridExpand = TRUE)
  }
  
  # Apply guild header style
  for(row in guild_rows) {
    addStyle(wb, "Isotope Data", guildHeaderStyle, rows = row, cols = 1:18, gridExpand = TRUE)
  }
  
  # Apply normal style to all data cells
  data_rows <- 3:(nrow(data) + 2)
  data_rows <- data_rows[!data_rows %in% c(ecosystem_rows, guild_rows)]  # Exclude headers
  addStyle(wb, "Isotope Data", normalStyle, rows = data_rows, cols = 1:18, gridExpand = TRUE)
  
  # Apply taxa style to the Taxa column
  addStyle(wb, "Isotope Data", taxaStyle, rows = data_rows, cols = 2, gridExpand = TRUE)
  
  # Apply common name style to the Common Name column
  addStyle(wb, "Isotope Data", commonNameStyle, rows = data_rows, cols = 3, gridExpand = TRUE)
  
  # Apply alternating row colors
  for(i in seq(1, length(data_rows), by=2)) {
    if(i <= length(data_rows)) {
      row <- data_rows[i]
      addStyle(wb, "Isotope Data", 
               createStyle(fgFill = "#F2F2F2"), 
               rows = row, cols = 1:18, gridExpand = TRUE)
    }
  }
  
  # Save the workbook
  saveWorkbook(wb, filename, overwrite = TRUE)
  cat("Excel file with grouped localities saved as:", filename, "\n")
}
# 
# After creating the formatted_table with format_isotope_table function:
formatted_table <- format_isotope_table(isotope_stats)
#
# Create a timestamp
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")

# Define file names with timestamp
filename_main <- paste0("isotope_analysis_table_", timestamp, ".xlsx")
filename_grouped <- paste0("isotope_analysis_grouped_", timestamp, ".xlsx")

# Export
excel_ready_table <- prepare_for_excel(formatted_table)
export_to_excel(excel_ready_table, filename_main)
export_grouped_localities_to_excel(excel_ready_table, filename_grouped)


