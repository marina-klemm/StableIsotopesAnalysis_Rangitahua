# Load and clean the dataset ---------------------------------------

stableIsotopesSpecimensRaw <- read.csv(file = 
                                         "rawData/completeRangitahuaJuly2025.txt",
                                       header = TRUE,
                                       sep= "\t")

# Explore data structure and unique values
str(stableIsotopesSpecimensRaw)
sapply(stableIsotopesSpecimensRaw[c("CollectionYear", "Species", "SpeciesAlgaeCorrectedSafePaste", 
                                    "Locality.main", "SampleType", "Feeding.habit.Niche", 
                                    "GuildCapitalized")], function(x) length(unique(x)))

# Filter and count Kermadec Islands records
stableIsotopesSpecimensRaw %>%
  filter(Locality.main == "Kermadec Islands") %>%
  count(CommonGroupName, CollectionYear)

# Initialize working dataset
stableIsotopesSpecimens <- stableIsotopesSpecimensRaw

# ======# LOCALITY CLEANUP

# Define locality standardization mapping
locality_replacements <- list(
  "Meyer" = "Meyer Islands",
  "Raoul" = "Rangitāhua", 
  "Herald" = "Herald Island",
  "Macauley" = "Macauley Island",
  "Kermadec Islands" = "Rangitāhua"  # Early collections likely from Rangitāhua
)

# Apply locality standardization
for (pattern in names(locality_replacements)) {
  stableIsotopesSpecimens <- stableIsotopesSpecimens %>%
    mutate(Locality.main = ifelse(grepl(pattern, Locality.main, ignore.case = TRUE), 
                                  locality_replacements[[pattern]], Locality.main))
}

# Clean sub-locality
stableIsotopesSpecimens$Locality.sub <- ifelse(stableIsotopesSpecimens$Locality.sub == 0, 
                                               "Unknown", stableIsotopesSpecimens$Locality.sub)

# =======# FEEDING GUILD CLEANUP


# Standardize feeding guilds
stableIsotopesSpecimens <- stableIsotopesSpecimens %>%
  mutate(
    GuildCapitalized = case_when(
      grepl("Omnivore|Detritivore", GuildCapitalized, ignore.case = TRUE) ~ "Omnivore/Detritivore",
      grepl("Predator", GuildCapitalized, ignore.case = TRUE) ~ "Carnivore",
      grepl("Can be scavenger, predator or algae feeders", Ecological.metadata, ignore.case = TRUE) ~ "Omnivore/Detritivore",
      TRUE ~ GuildCapitalized
    ),
    # Standardize group names
    Group = case_when(
      grepl("Terrestrial invertebrates", Group, ignore.case = TRUE) ~ "Terrestrial Invertebrates",
      grepl("Terrestrial/Marine invertebrate", Group, ignore.case = TRUE) ~ "Terrestrial/Marine Invertebrate",
      TRUE ~ Group
    )
  )

# =======# SPECIES NAME CORRECTIONS

# Define species name corrections
species_corrections <- list(
  "Cyanoramphus novaezelandie" = "Cyanoramphus novaezelandiae",
  "Turdus philomenos" = "Turdus philomelos",
  "Turdus philomelos " = "Turdus philomelos",
  "Pheloide megacephala" = "Pheidole megacephala", 
  "Heliocidaris tuberculata " = "Heliocidaris tuberculata",
  "Tripneustes gratilla" = "Tripneustes australiae",
  "Puweto" = "Zapornia tabuensis plumbea",
  "Caulerpa webbiaana" = "Caulerpa webbiana",
  "Myoporum rapense subsp. kermadecense" = "Myoporum rapense subsp. kermadecense",
  "Pyrrosia eleagnifolia" = "Pyrrosia elaeagnifolia",
  "Melicytus ramiflorus ramiflorus" = "Melicytus ramiflorus",
  "Ugyops (Paracona) raouli" =  "Ugyops sp.",
  "Ugyops" = "Ugyops sp."
)

# Remove Pyrrosia serpens 
stableIsotopesSpecimens <- stableIsotopesSpecimens %>%
  filter(Species != "Pyrrosia serpens")

# Apply species corrections
for (old_name in names(species_corrections)) {
  stableIsotopesSpecimens <- stableIsotopesSpecimens %>%
    mutate(Species = ifelse(grepl(old_name, Species, ignore.case = TRUE), 
                            species_corrections[[old_name]], Species))
}

# Replace species names with algae-corrected versions where available
stableIsotopesSpecimens$Species <- ifelse(
  stableIsotopesSpecimens$SpeciesAlgaeCorrectedSafePaste != "" & 
    !is.na(stableIsotopesSpecimens$SpeciesAlgaeCorrectedSafePaste),
  stableIsotopesSpecimens$SpeciesAlgaeCorrectedSafePaste,
  stableIsotopesSpecimens$Species
)

# Remove phytoplankton records as they were low quality
stableIsotopesSpecimens <- stableIsotopesSpecimens %>%
  filter(Species != "Phytoplankton")

# Remove zooplankton records too - only bad data in this one, as the good
# zooplankton is from a single location (between Meyers and Rangitāhua)
stableIsotopesSpecimens <- stableIsotopesSpecimens %>%
  filter(Species != "Zooplankton")

# =========# SPECIFIC TAXONOMIC GROUP CORRECTIONS

# Update specific species classifications
stableIsotopesSpecimens <- stableIsotopesSpecimens %>%
  mutate(
    CommonGroupName = case_when(
      Species == "Euphausiids" ~ "Krill",
      TRUE ~ CommonGroupName
    ),
    GuildCapitalized = case_when(
      Species == "Zooplankton" ~ "Omnivore/Detritivore",
      CommonGroupName == "Shrimp" ~ "Omnivore/Detritivore",
      TRUE ~ GuildCapitalized
    )
  )

# ===========# FISH-SPECIFIC CORRECTIONS

# Create Locality.Main2 column for mobile fish species
stableIsotopesSpecimens$Locality.Main2 <- stableIsotopesSpecimens$Locality.main

# Update locality for highly mobile fish species (likely moved between islands)
mobile_fish_species <- c("Arripis xylabion", "Seriola lalandi", "Chromis dispilus", "Gymnothorax nubilus")
stableIsotopesSpecimens$Locality.Main2[stableIsotopesSpecimens$Species %in% mobile_fish_species] <- "Rangitāhua"

# Update fish feeding guilds with specific ecological data
fish_guild_updates <- data.frame(
  Species = c("Arripis xylabion", "Chrysiptera rapanui", "Engyprosopon raoulense", 
              "Pseudocaranx sp.", "Scorpis violacea"),
  NewGuild = c("Carnivore/Planktivore", "Omnivore/Planktivore", "Carnivore", 
               "Carnivore/Planktivore", "Planktivore"),
  stringsAsFactors = FALSE
)

# Apply fish guild updates
for (i in 1:nrow(fish_guild_updates)) {
  stableIsotopesSpecimens$GuildCapitalized[stableIsotopesSpecimens$Species == fish_guild_updates$Species[i]] <- 
    fish_guild_updates$NewGuild[i]
}



# ===========# FINAL DATA PROCESSING

# Clean C:N mass ratio data
stableIsotopesSpecimens$C.N.mass.ratio[stableIsotopesSpecimens$C.N.mass.ratio %in% c("ND", "#DIV/0!")] <- NA

# Add temporal period classification
stableIsotopesSpecimens$Period <- case_when(
  stableIsotopesSpecimens$CollectionYear <= 1920 ~ "Period 1\n(1854-1920)",
  stableIsotopesSpecimens$CollectionYear >= 1921 & stableIsotopesSpecimens$CollectionYear <= 1983 ~ "Period 2\n(1921-1983)",
  stableIsotopesSpecimens$CollectionYear >= 1984 & stableIsotopesSpecimens$CollectionYear <= 2001 ~ "Period 3\n(1984-2001)",
  stableIsotopesSpecimens$CollectionYear >= 2002 ~ "Period 4\n(2002-2023)"
)

# Create backup of processed data
stableIsotopesSpecimensBackup <- stableIsotopesSpecimens

# Summary of final dataset
cat("Total records:", nrow(stableIsotopesSpecimens), "\n")
cat("Unique species:", length(unique(stableIsotopesSpecimens$Species)), "\n")
cat("Unique localities:", length(unique(stableIsotopesSpecimens$Locality.main)), "\n")
cat("Unique guilds:", length(unique(stableIsotopesSpecimens$GuildCapitalized)), "\n")
table(stableIsotopesSpecimens$Period)


# Lipid correction and seuss effect adjustment ----------------------------


# Initialize dataset and rename columns
lipidCorrectedData <- stableIsotopesSpecimens %>%
  rename(
    d15N = normalised.d15N,
    d13C = normalised.d13C
  )

# Apply lipid correction where available (replace d13C with lipid-corrected values)
lipidCorrectedData$d13C <- ifelse(
  is.na(lipidCorrectedData$Lipid.corrected.d13C.where.mass.C.N.ratio..3.5), 
  lipidCorrectedData$d13C, 
  lipidCorrectedData$Lipid.corrected.d13C.where.mass.C.N.ratio..3.5
)

# Remove rows with missing isotope values and ensure numeric format
seussCorrectedData <- lipidCorrectedData %>%
  filter(!is.na(d13C), !is.na(d15N)) %>%
  mutate(
    d13C = as.numeric(d13C),
    d15N = as.numeric(d15N),
    # Apply Seuss Effect Correction: d13C - 1 + 1.1 * (2024 - CollectionYear) * 0.027
    d13CSeussCorrected = d13C - 1 + 1.1 * (2024 - CollectionYear) * 0.027
  )


cat("Original records:", nrow(stableIsotopesSpecimens), "\n")
cat("Records after lipid correction:", nrow(lipidCorrectedData), "\n")
cat("Lipid corrections applied:", sum(!is.na(stableIsotopesSpecimens$Lipid.corrected.d13C.where.mass.C.N.ratio..3.5)), "\n")

# Display sample of corrections (first 5 rows)
sample_data <- seussCorrectedData %>%
  select(d13C, d15N, CollectionYear, d13CSeussCorrected) %>%
  slice_head(n = 5)

cat("\nSample of corrected values:\n")
print(sample_data)

seussCorrectedData <- seussCorrectedData %>%
  rename(
    percentageCarbon = `All.Correct.Amt..C.Flash.TCD`,
    percentageNitrogen = `All.Correct.Amt..N.Flash.TCD`
  )







# Remove localities that are not Rangitāhua or Meyers for rest of the analysis --------

## For the other analysis, drop the rows where LocalityTerrestrial is not Rangitāhua or Meyers:
unique(seussCorrectedData$Locality.Main2)

seussCorrectedData <- seussCorrectedData %>%
  filter(Locality.Main2 %in% c("Rangitāhua", "Meyer Islands"))

unique(seussCorrectedData$LostSample)

seussCorrectedData <- seussCorrectedData %>%
  filter(LostSample %in% c("0"))

unique(seussCorrectedData$GuildCapitalized)
unique(seussCorrectedData$CollectionYear)
unique(seussCorrectedData$Locality.Main2)
unique(seussCorrectedData$Sample.ID)


# Remove the arthropods duplicated samples --------------------------------

seussCorrectedData %>%
  filter(Box.Name == "Batch test - invertebrates") %>% 
  pull(Sample.ID) %>%      # Extract the SampleID column
  unique()                # Get unique SampleIDs

set.seed(123)  # Set a seed for reproducibility

# Ensure all BaseIDs are accounted for
filteredBox11RandomSelection <- seussCorrectedData %>%
  filter(Box.Name == "Batch test - invertebrates") %>%  # Filter rows by Box.Name
  mutate(BaseID = gsub(" t$", "", Sample.ID)) %>%       # Create BaseID by removing " t"
  group_by(BaseID) %>%
  mutate(group_size = n()) %>%                         # Count rows in each group
  ungroup() %>%
  group_by(BaseID) %>%
  # Randomly sample one row per group, but keep all rows for single-value groups
  filter(group_size == 1 | row_number() == sample(1:n(), 1)) %>%
  ungroup() %>%
  select(-group_size)  # Remove temporary column

print(filteredBox11RandomSelection$Sample.ID)

# Create a dataset with the values left out from the filtering
box11RemainingRows <- seussCorrectedData %>%
  filter(Box.Name == "Batch test - invertebrates") %>% # Filter rows by Box.Name
  anti_join(filteredBox11RandomSelection, by = "Sample.ID") # Exclude selected rows


print(box11RemainingRows$Sample.ID)


seussCorrectedDataBox11Removed <- seussCorrectedData %>%
  anti_join(box11RemainingRows, by = "Sample.ID")


## Check before going on with the analysis
seussCorrectedDataBox11Removed %>%
  filter(Box.Name == "Batch test - invertebrates") %>% 
  pull(Sample.ID) %>%      # Extract the SampleID column
  unique()   


seussCorrectedData <- seussCorrectedDataBox11Removed

seussCorrectedDataBackup <- seussCorrectedData




# A few more subsets of data ----------------------------------------------

# ===========# Specimens from each motu
specimensMeyers <- seussCorrectedData %>%
  filter(Locality.Main2 == "Meyer Islands")

specimensRangitāhua <- seussCorrectedData %>%
  filter(Locality.Main2 == "Rangitāhua")

# ===========# Groups that are common in both motu
groupsMeyers <- unique(specimensMeyers$CommonGroupName)
unique(specimensMeyers$CommonGroupName)

groupsRangitāhua <- unique(specimensRangitāhua$CommonGroupName)
unique(specimensRangitāhua$CommonGroupName)

# Find the common groups present in both Meyers and Rangitāhua
commonGroupsRangitāhuaMeyers <- intersect(groupsMeyers, groupsRangitāhua)

# Filter the original data to keep only these common groups
commonGroupsRangitāhuaMeyers <- seussCorrectedData[seussCorrectedData$CommonGroupName %in% commonGroupsRangitāhuaMeyers, ]

# Display the filtered data with common groups
head(commonGroupsRangitāhuaMeyers)
unique(commonGroupsRangitāhuaMeyers$CommonGroupName)

rm(groupsMeyers)
rm(groupsRangitāhua)

# ===========# Filter for each period:

# Function to create all period-island dataset combinations
create_period_island_datasets <- function(data) {
  
  # Clean period names for dataset naming
  period_mapping <- c(
    "Period 1\n(1854-1920)" = "Period1", 
    "Period 2\n(1921-1983)" = "Period2",
    "Period 3\n(1984-2001)" = "Period3",
    "Period 4\n(2002-2023)" = "Period4"
  )
  
  # Clean island names for dataset naming
  island_mapping <- c(
    "Rangitāhua" = "Rangitāhua",
    "Meyer Islands" = "Meyer Islands"
  )
  
  # Get unique periods and islands
  periods <- unique(data$Period)
  islands <- unique(data$Locality.Main2)
  
  # Initialize list to store all datasets
  datasets <- list()
  
  # Create datasets for each period (all localities combined)
  cat("Creating period datasets (all localities):\n")
  for (period in periods) {
    clean_period <- period_mapping[period]
    dataset_name <- clean_period
    
    datasets[[dataset_name]] <- data %>%
      filter(Period == period)
    
    cat(sprintf("  %s: %d rows\n", dataset_name, nrow(datasets[[dataset_name]])))
  }
  
  cat("\n")
  
  # Create datasets for each period-island combination
  cat("Creating period-island combination datasets:\n")
  for (period in periods) {
    for (island in islands) {
      clean_period <- period_mapping[period]
      clean_island <- island_mapping[island]
      dataset_name <- paste0(clean_period, clean_island)
      
      datasets[[dataset_name]] <- data %>%
        filter(Period == period, Locality.Main2 == island)
      
      cat(sprintf("  %s: %d rows\n", dataset_name, nrow(datasets[[dataset_name]])))
    }
  }
  
  cat("\n")
  
  # Summary
  cat("SUMMARY:\n")
  cat("========\n")
  cat(sprintf("Total datasets created: %d\n", length(datasets)))
  cat(sprintf("Period-only datasets: %d\n", length(periods)))
  cat(sprintf("Period-island datasets: %d\n", length(periods) * length(islands)))
  
  cat("\nDataset names:\n")
  for (name in names(datasets)) {
    cat(sprintf("  %s\n", name))
  }
  
  return(datasets)
}

# Function to assign datasets to global environment
assign_datasets_globally <- function(dataset_list) {
  cat("\nAssigning datasets to global environment:\n")
  for (name in names(dataset_list)) {
    assign(name, dataset_list[[name]], envir = .GlobalEnv)
    cat(sprintf("  %s assigned\n", name))
  }
  cat("All datasets are now available in the workspace!\n")
}

# Function to get dataset summary
get_datasets_summary <- function(dataset_list) {
  summary_df <- data.frame(
    Dataset = names(dataset_list),
    Rows = sapply(dataset_list, nrow),
    Periods = sapply(dataset_list, function(x) length(unique(x$Period))),
    Islands = sapply(dataset_list, function(x) length(unique(x$Locality.Main2))),
    stringsAsFactors = FALSE
  )
  
  return(summary_df)
}

# Create all datasets
all_datasets <- create_period_island_datasets(seussCorrectedData)

# Assign them to global environment so we can use them directly
assign_datasets_globally(all_datasets)

# Get summary table
summary_table <- get_datasets_summary(all_datasets)

print(summary_table)

# ===========# Groups subsamples:

birds <- seussCorrectedData[seussCorrectedData$CommonGroupName == "Bird",] %>%
  filter(!(Species == "Prosthemadera novaeseelandiae" & percentageNitrogen < 10))

birdsForest <- birds[birds$Sample.Type == "Animal - feather",] %>%
  filter(!(Species == "Cyanoramphus novaezelandiae" & percentageNitrogen < 10))

plants <- seussCorrectedData[seussCorrectedData$CommonGroupName == "Plant",]


# Explanation of each dataset: --------------------------------------------

# Note that for the datasets below, the one after always carries the filtering/editing of
# the previous, unless specified


## stableIsotopesSpecimensRaw: raw data with NIWA results, no cleanup
## stableIsotopesSpecimens: LocalityTerrestrial synonymizing, Kermadec == Rangitāhua, species renaming, typos corrrection
## stableIsotopesSpecimensBackup: as above, not to be edited
## lipidCorrectedData: columns renamed (d13C and 15N), lipid corrected d13C replaced the original 
## seussCorrectedData: correction for seuss effect, removal of NA d13C and d15N values, 
# Rangitāhua and Meyers only, removal of half of box 11 samples
## box11Samples: samples from box 11, which was the invertebrates (muscle x rest
# of the body) box
## box11PairedData: box 11 samples, but paired so to remove the duplication from the
# rest of the comparison
## seussCorrectedDataBackup: all the filtering above as backup: LocalityTerrestrial (Rangitāhua and
# Meyers only), species cleanup, typos cleanup, Seuss corrected, lipid corrected,
# only half of box 11 samples.


## specimensRangitāhua: seussCorrectedData, LocalityTerrestrial filtered for Rangitāhua
## specimensMeyers: seussCorrectedData, LocalityTerrestrial filtered for Meyers

## seussCorrectedDataCommonRangitāhuaMeyers: same as seussCorrectedData, but with filtering
# for CommonGroupName that were present in both Rangitāhua and Meyers omly
## neodataCommon: same as seusCorrectedDataCommonRangitāhuaMeyers, but with collection year > 2002

