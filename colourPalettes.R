# Plots synthax -----------------------------------------------------------

unique(seussCorrectedData$GuildCapitalized)

# Create standardized color and shape scales that I can reuse
guild_shapes <- c(
  "Guano sample" = 8,                  # Star
  "Herbivore" = 17,                    # Filled triangle
  "Omnivore/Detritivore" = 15,         # Filled square
  "Carnivore" = 16,                    # Filled circle
  "Primary producer" = 11,             # Empty circle
  "Carnivore/Planktivore" = 23,        # Filled diamond (point up)
  "Omnivore/Planktivore" = 18,         # Filled diamond
  "Planktivore" = 25                  # Filled triangle (point down)
)

guild_colors <- c(
  "Primary producer" = "#009E73",       # Green
  "Herbivore" = "#F0E442",             # Yellow
  "Guano sample" = "#CC79A7",          # Pink
  "Omnivore/Detritivore" = "#0072B2",  # Blue
  "Carnivore" = "#D55E00",             # Orange
  "Omnivore/planktivore" = "#56B4E9",  # Light blue
  "Planktivore" = "#0072B2",           # Blue
  "Carnivore/planktivore" = "#E69F00", # Amber
  "Planktivore/Omnivore" = "#3287BC",  # Medium blue
  "Detritivore" = "#009292",           # Teal
  "Omnivore" = "#882255",              # Burgundy
  "Detritivore or omnivore" = "#004C66", # Dark blue
  "Predator" = "#FF0000",              # Red
  "Unknown" = "#999999"                # Gray
)

unique(stableIsotopesSpecimens$Group)

group_colors <- c(
  "Birds" = "darkorchid2",                          
  "Fish" = "blue4",                           
  "Marine Invertebrates" = "cornflowerblue",           
  "Terrestrial Invertebrates" = "brown2",      
  "Terrestrial/Marine Invertebrate" = "darkorange2", 
  "Algae" = "chartreuse3",               
  "Zooplankton" = "cadetblue3",
  "Plants" = "darkgreen",
  "Rats" = "maroon4"
)

# Create standardized color and shape scales that I can reuse
plantsSpeciesShapes <- c(
  "Coprosma acutifolia" = 16,
  "Corynocarpus laevigatus" = 19,
  "Melicytus ramiflorus" = 17, 
  "Melicytus ramiflorus ramiflorus" = 17,
  "Metrosideros kermadecensis" = 15,         
  "Myoporum rapense subsp. kermadecense" = 18,
  "Myrsine kermadecensis" = 13,
  "Piper excelsum psittacorum" = 8,                 
  "Pyrrosia elaeagnifolia" = 14,                     
  "Pyrrosia eleagnifolia" = 14,              
  "Pyrrosia serpens" = 14                        
)

plantsSpeciesColors <- c(
  "Coprosma acutifolia" = "darkorchid2",
  "Corynocarpus laevigatus" = "blue4",
  "Melicytus ramiflorus" = "cornflowerblue", 
  "Melicytus ramiflorus ramiflorus" = "cornflowerblue",
  "Metrosideros kermadecensis" = "brown2",         
  "Myoporum rapense subsp. kermadecense" = "darkorange2",
  "Myrsine kermadecensis" = "chartreuse3",
  "Piper excelsum psittacorum" = "aquamarine4",                  
  "Pyrrosia elaeagnifolia" = "darkgreen",                     
  "Pyrrosia eleagnifolia" = "darkgreen",              
  "Pyrrosia serpens" ="darkgreen"
)

birdsSpeciesColors <- c(
  "Gygis alba" = "darkorchid2",
  "Prosthemadera novaeseelandiae" = "blue4",
  "Todiramphus sanctus" = "cornflowerblue", 
  "Melicytus ramiflorus ramiflorus" = "cornflowerblue",
  "Metrosideros kermadecensis" = "brown2",
  "Onychoprion fuscatus" = "darkorange2",
  "Cyanoramphus novaezelandiae" = "chartreuse3",
  "Pterodroma neglecta" = "aquamarine4",
  "Turdus merula" = "black",
  "Pterodroma nigripennis" = "darkgrey",
  "Turdus philomelos" = "darkgoldenrod",
  "Procelsterna albivitta" = "white",
  "Zapornia tabuensis plumbea" = 'sienna'
)


environment_colors <- c(
  "Terrestrial" = "#006400",
  "Marine" = "#00008B",
  "Terrestrial/Marine" = "#D2691E"
)