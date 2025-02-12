# Carr et al search terms testing
# 0) Set up workspace ####
# 0.1) Load packages #####
# Load necessary packages
# 0.1) Load packages #####
pacman::p_load(data.table,openalexR)

# 0.2) Create functions #####
add_quotes <- function(vector) {
  sapply(vector, function(term) {
    if (grepl("\\s", term)) {
      return(shQuote(term, type = "cmd"))
    } else {
      return(term)
    }
  }, USE.NAMES = FALSE)
}

# Define the path for saving CSV files
search_data_dir <- "doc"

# Create the directory if it does not exist
if (!dir.exists(search_data_dir)) {
  dir.create(search_data_dir, recursive = TRUE)
}

# 1) Create terms ####
# Read in additional animal breed terms provided by Claudia Arndt's team
Productivity_Economics <- c(
  "Benefit-cost analysis", "Benefit-cost ratio", "Bottom line", "Break even period", "Breakeven period",
  "Capital destruction", "Discount cash flow", "Economic analysis", "Economic evaluation", "Economic valuation",
  "Economic impact", "Full time equivalent", "Gross added value", "Gross margin", "Interest rate",
  "Kilograms per acre", "Kilograms per hectare", "Man day", "Man power", "Net added value",
  "Net present value", "Net worth", "Partial budget", "Payback period", "Turnoff rate",
  "Willingness to pay", "Working day", "Direct use valuation", "Passive use valuation",
  "Non-market valuation", "Contingent valuation", "Consumptive valuation", "Subsistence valuation",
  "Livelihood valuation", "Cost")

Resilience_1 <- c(
  "Adaptation capacity", "Adaptive management", "Capacity building", "Indigenous knowledge",
  "Local knowledge", "Traditional knowledge", "Ecological knowledge", "Community awareness",
  "Community assessment", "Vulnerability assessment", "Risk assessment", "Participatory assessment",
  "Socio-ecological systems", "Global warming", "Changing climate", "Climate change",
  "Drought resistance", "Heat resistance", "Flood resistance", "Waterlogging resistance",
  "Weather resilience", "Dry spell resilience", "Erosion risk reduction"
)

Resilience_2 <- c(
  "Bulk density", "Livelihood diversification", "Water productivity", "Livestock infestation",
  "Crop infestation", "Soil loss", "Soil formation", "Soil aggregation", "Soil fertility",
  "Soil degradation", "Soil decline", "Potassium uptake", "Phosphorus uptake", "Nitrogen uptake",
  "Smallholder diet", "Household diet", "Agricultural diet", "Labour", "Micronutrient food",
  "Gender participation", "Gender budgeting", "Gender equity", "Yield loss cost", "Biodiversity",
  "Cash flow", "Climate smart agriculture", "Food access", "Food affordability", "Food availability",
  "Food consumption", "Food distribution", "Food expenditure", "Food insecurity", "Food intake",
  "Food safety", "Food scarcity", "Food security", "Food stability", "Food systems",
  "Species diversity", "Species evenness", "Species presence", "Species resilience",
  "Species resistance", "Species richness", "Species tolerance", "Water availability",
  "Water conservation", "Water consumption", "Water footprint", "Water loss", "Water recycling",
  "Water reuse", "Water stress", "Water uptake", "Water use")

Agroforestry <- c(
  "Agroforestry", "Evergreen agriculture", "Farmer managed natural regeneration", "FMNR",
  "Taungya", "Alley cropping", "Alley farming", "Boundary planting", "Living fences",
  "Hedgerows", "Riparian buffer strips", "Riparian forest buffers", "Windbreaks",
  "Shelterbelts", "Improved fallow", "Silvopastoral systems", "Silvoarable systems",
  "Tree belts", "Parkland management", "Shaded tree management", "Shrub contour planting",
  "Tree contour planting", "Multifunctional trees", "Multipurpose trees", "Multistrata trees")

Agronomy <- c(
  "Adapted cultivar", "Adapted crop", "Adapted variety", "Adapted breed", "Biological control",
  "Biocontrol", "Natural pest control", "Natural weed control", "Biological pest control",
  "Biopesticide", "Bioinsecticide", "Biological pest management", "Ecological pest control",
  "Conservation agriculture", "Cover cropping", "Crop diversification", "Crop rotation",
  "Dibble stick", "Direct drilling", "Direct planting", "Direct seeding", "Diversified cropping",
  "Diversion ditches", "Double cropping", "Triple cropping", "Drip irrigation", "Erosion control",
  "Fertilizer banding", "Grass strips", "Vegetative barriers", "Green manure", "Ground cover",
  "Integrated pest control", "Integrated pest management", "Integrated soil fertility management",
  "Limited soil disturbance", "Minimum tillage", "No tillage", "Organic amendments",
  "Organic fertilizers", "Permanent ground cover", "Planting basins", "Precision agriculture",
  "Relay cropping", "Residue retention", "Resilient cultivars", "Resilient crops", "Resilient varieties",
  "Resilient breeds", "Selective herbicide", "Selective insecticide", "Selective pesticide",
  "Small-scale irrigation", "Soil amendments", "Soil conservation", "Stale seed bed", "Stone lines",
  "Sustainable pest management", "Targeted herbicide", "Targeted insecticide", "Targeted pesticide",
  "Water conservation", "Water harvesting", "Zero tillage", "Crop succession", "Micro-catchment",
  "Soil organic matter management", "Slow-release fertilizers")

Products <- c(
  "Maize", "Zea mays", "Common beans", "Phaseolus vulgaris", "Cattle", "Bos taurus",
  "Bos indicus", "Bull", "Heifer", "Bovine", "Coffee", "Coffea arabica", "Coffea robusta")

Livestock <- c(
  "Managed grazing", "Rotational grazing", "Holistic planned grazing", "Rational grazing",
  "Intensive grazing", "Short duration grazing", "High intensity low frequency grazing",
  "High intensity grazing", "Mob grazing", "Ultra-high stock density grazing",
  "Biomass accumulation grazing", "Strip grazing")

Countries_Americas <- c(
  "Belize", "Bolivia", "Brazil", "Colombia", "Costa Rica", "Cuba", "Dominica",
  "Dominican Republic", "Ecuador", "El Salvador", "Grenada", "Guatemala", "Guyana",
  "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Paraguay", "Peru",
  "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname", "Venezuela")

# Convert to Boolean expressions
convert_to_boolean <- function(term_list) {
  paste0("(", paste(shQuote(term_list, type = "cmd"), collapse = " OR "), ")")
}

Productivity_Economics_boolean <- convert_to_boolean(Productivity_Economics)
Resilience_1_boolean <- convert_to_boolean(Resilience_1)
Resilience_2_boolean <- convert_to_boolean(Resilience_2)
Agroforestry_boolean <- convert_to_boolean(Agroforestry)
Products_boolean <- convert_to_boolean(Products)
Livestock_boolean <- convert_to_boolean(Livestock)
Countries_Americas_boolean <- convert_to_boolean(Countries_Americas)

# Define search timeframe
from_year <- "1970-01-01"
to_year <- "2024-10-30"

# Ensure doc directory exists
dir.create("doc", showWarnings = FALSE)

# Function to run OpenAlex queries and save results for **each category in ONE query**
run_query <- function(category, boolean_query) {
  cat("🔍 Running query for:", category, "...\n")
  
  api_endpoint <- oa_query(
    entity = "works",
    title_and_abstract.search = boolean_query,
    from_publication_date = from_year,
    to_publication_date = to_year
  )
  
  hits <- oa_request(query_url = api_endpoint)
  hits_tab <- data.table(oa2df(hits, entity = "works"))
  
  # Convert list-type columns to character before saving
  for (col in names(hits_tab)) {
    if (is.list(hits_tab[[col]])) {
      hits_tab[[col]] <- sapply(hits_tab[[col]], function(x) paste(unlist(x), collapse = "; "))
    }
  }
  
  # Save category-wise result in doc folder
  fwrite(hits_tab, paste0("doc/openalex_results_", category, ".csv"))
}

# Run queries separately for each category (One API call per category)
run_query("Productivity_Economics", Productivity_Economics_boolean)
run_query("Resilience_1", Resilience_1_boolean)
run_query("Resilience_2", Resilience_2_boolean)
run_query("Agroforestry", Agroforestry_boolean)
run_query("Products", Products_boolean)
run_query("Livestock", Livestock_boolean)
run_query("Countries_Americas", Countries_Americas_boolean)

# Combine all CSV results and remove duplicates
all_files <- list.files("doc", pattern = "openalex_results_.*\\.csv", full.names = TRUE)
all_data <- rbindlist(lapply(all_files, fread), fill = TRUE)
final_results <- unique(all_data, by = "doi")

# Save final deduplicated results in doc folder
fwrite(final_results, "doc/openalex_results_final.csv")

cat("✅ All searches completed, duplicates removed, and results saved.\n")