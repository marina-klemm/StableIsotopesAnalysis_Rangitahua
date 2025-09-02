# Additional table with voucher numbers -----------------------------------
# Create a summary table with unique species and their voucher numbers
species_voucher_table <- allDataThisPaper %>%
  # Remove rows where Species or IdentifierEditedMK are missing
  filter(!is.na(Species) & !is.na(IdentifierEditedMK) & 
           Species != "" & IdentifierEditedMK != "") %>%
  # Group by species
  group_by(Species) %>%
  # Combine all voucher numbers for each species, removing duplicates
  summarise(
    VoucherNumbers = paste(unique(IdentifierEditedMK), collapse = ", "),
    .groups = "drop"
  ) %>%
  # Sort alphabetically by species name
  arrange(Species)

# Display the table
print(species_voucher_table)

# Create a formatted table for publication
formatted_table <- species_voucher_table %>%
  kable(
    col.names = c("Species", "Voucher Numbers"),
    caption = "Species and their corresponding voucher numbers",
    format = "html"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "left"
  ) %>%
  column_spec(1, italic = TRUE) %>%  # Italicize species names
  column_spec(2, width = "400px")    # Set width for voucher numbers column

# Print the formatted table
formatted_table

# Alternative: Simple table for LaTeX/Word documents
simple_table <- species_voucher_table %>%
  kable(
    col.names = c("Species", "Voucher Numbers"),
    caption = "Species and their corresponding voucher numbers",
    format = "simple"
  )

print(simple_table)

# Function to export species and voucher numbers to Excel
export_species_vouchers_to_excel <- function(data, filename = "species_vouchers_table.xlsx") {
  # Create a new workbook
  wb <- createWorkbook()
  
  # Add a worksheet
  addWorksheet(wb, "Species Vouchers")
  
  # Write data starting at row 2
  writeData(wb, "Species Vouchers", data, startRow = 2, startCol = 1, headerStyle = NULL)
  
  # Add main header in row 1
  headerRow <- c("Species", "Voucher Numbers")
  writeData(wb, "Species Vouchers", t(headerRow), startRow = 1, startCol = 1)
  
  # Set column widths
  setColWidths(wb, "Species Vouchers", cols = 1, widths = 35)  # Species
  setColWidths(wb, "Species Vouchers", cols = 2, widths = 60)  # Voucher Numbers
  
  # Create styles
  headerStyle <- createStyle(
    fontName = "Arial", fontSize = 12, textDecoration = "bold",
    halign = "center", valign = "center", 
    border = "bottom", borderStyle = "medium",
    fgFill = "#D9E1F2"
  )
  
  speciesStyle <- createStyle(
    fontName = "Arial", fontSize = 11, textDecoration = "italic",
    halign = "left", valign = "center",
    border = "bottom", borderStyle = "hair"
  )
  
  voucherStyle <- createStyle(
    fontName = "Arial", fontSize = 11,
    halign = "left", valign = "center",
    border = "bottom", borderStyle = "hair"
  )
  
  # Apply header style
  addStyle(wb, "Species Vouchers", headerStyle, rows = 1, cols = 1:2, gridExpand = TRUE)
  
  # Apply species style (italic for scientific names)
  data_rows <- 2:(nrow(data) + 1)
  addStyle(wb, "Species Vouchers", speciesStyle, rows = data_rows, cols = 1, gridExpand = TRUE)
  
  # Apply voucher style
  addStyle(wb, "Species Vouchers", voucherStyle, rows = data_rows, cols = 2, gridExpand = TRUE)
  
  # Apply alternating row colors
  for(i in seq(1, length(data_rows), by=2)) {
    if(i <= length(data_rows)) {
      row <- data_rows[i]
      addStyle(wb, "Species Vouchers", 
               createStyle(fgFill = "#F2F2F2"), 
               rows = row, cols = 1:2, gridExpand = TRUE)
    }
  }
  
  # Save the workbook
  saveWorkbook(wb, filename, overwrite = TRUE)
  cat("Species and voucher numbers Excel file saved as:", filename, "\n")
}

# Create a timestamp
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")

# Define file name with timestamp
filename_species <- paste0("species_voucher_numbers_", timestamp, ".xlsx")

# Export to Excel
export_species_vouchers_to_excel(species_voucher_table, filename_species)
