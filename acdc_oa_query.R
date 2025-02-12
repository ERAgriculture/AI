# Carr et al search terms testing
# 0) Set up workspace ####
# 0.1) Load packages #####
# Load necessary packages
pacman::p_load(data.table, openalexR)

# Function to chunk search terms within 2000-character API limit
split_terms <- function(term_list, max_length = 2000) {
  chunks <- list()
  chunk <- c()
  length_so_far <- 0
  
  for (term in term_list) {
    new_length <- length_so_far + nchar(term) + 4  # Adding 4 for " OR "
    if (new_length > max_length) {
      chunks <- append(chunks, list(paste0("(", paste(chunk, collapse = " OR "), ")")))
      chunk <- c(term)
      length_so_far <- nchar(term)
    } else {
      chunk <- append(chunk, term)
      length_so_far <- new_length
    }
  }
  
  # Add the final chunk
  if (length(chunk) > 0) {
    chunks <- append(chunks, list(paste0("(", paste(chunk, collapse = " OR "), ")")))
  }
  
  return(chunks)
}

# Function to add quotes to multi-word phrases
add_quotes <- function(vector) {
  sapply(vector, function(term) {
    if (grepl("\\s", term)) {
      return(paste0('"', term, '"'))  # Wrap in double quotes
    } else {
      return(term)
    }
  }, USE.NAMES = FALSE)
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


Productivity_Economics_boolean <- paste0("(", paste(Productivity_Economics, collapse = " OR "), ")")
Resilience_1_boolean <- paste0("(", paste(Resilience_1, collapse = " OR "), ")")
Resilience_2_boolean <- paste0("(", paste(Resilience_2, collapse = " OR "), ")")
Agroforestry_boolean <- paste0("(", paste(Agroforestry, collapse = " OR "), ")")
Products_boolean <- paste0("(", paste(Products, collapse = " OR "), ")")
Livestock_boolean <- paste0("(", paste(Livestock, collapse = " OR "), ")")
Countries_Americas_boolean <- paste0("(", paste(Countries_Americas, collapse = " OR "), ")")

# Step 3: Chunk the Boolean expressions (to fit API limit)
Productivity_Economics_chunks <- split_terms(Productivity_Economics_boolean)
Resilience_1_chunks <- split_terms(Resilience_1_boolean)
Resilience_2_chunks <- split_terms(Resilience_2_boolean)
Agroforestry_chunks <- split_terms(Agroforestry_boolean)
Products_chunks <- split_terms(Products_boolean)
Livestock_chunks <- split_terms(Livestock_boolean)
Countries_Americas_chunks <- split_terms(Countries_Americas_boolean)

# Save terms for reference
terms <- list(
  productivity_economics = Productivity_Economics_chunks,
  resilience_1 = Resilience_1_chunks,
  resilience_2 = Resilience_2_chunks,
  agroforestry = Agroforestry_chunks,
  products = Products_chunks,
  livestock = Livestock_chunks,
  countries = Countries_Americas_chunks
)


save(terms, file = "search_terms.RData")

# Define search timeframe
from_year <- "1970-01-01"
to_year <- "2024-10-30"

# Initialize empty dataframe for storing results
final_results <- data.table()

# Iterate over chunks and query OpenAlex
for (prod in Productivity_Economics_chunks) {
  for (res1 in Resilience_1_chunks) {
    for (res2 in Resilience_2_chunks) {
      for (agro in Agroforestry_chunks) {
        for (prod_item in Products_chunks) {
          for (livestock_item in Livestock_chunks) {
            for (country in Countries_Americas_chunks) {
              
              search_query <- paste(
                paste(prod, collapse = " OR "), "OR",
                paste(res1, collapse = " OR "), "OR",
                paste(res2, collapse = " OR "), "OR",
                paste(agro, collapse = " OR "), "OR",
                paste(prod_item, collapse = " OR "), "OR",
                paste(livestock_item, collapse = " OR "), "OR",
                paste(country, collapse = " OR ")
              )
              
              # Check if query length exceeds OpenAlex limit
              if (nchar(search_query) > 2000) {
                cat("Skipping query due to length:", nchar(search_query), "characters\n")
                next
              }
              
              cat("Running query:", search_query, "\n")
              
              api_endpoint <- oa_query(
                entity = "works",
                title_and_abstract.search = search_query,
                from_publication_date = from_year,
                to_publication_date = to_year
              )
              
              hits <- oa_request(query_url = api_endpoint)
              hits_tab <- data.table(oa2df(hits, entity = "works"))
              
              # Convert list-type columns to character before merging
              for (col in names(hits_tab)) {
                if (is.list(hits_tab[[col]])) {
                  hits_tab[[col]] <- sapply(hits_tab[[col]], function(x) paste(unlist(x), collapse = "; "))
                }
              }
              
              # Append results and remove duplicates
              final_results <- unique(rbind(final_results, hits_tab, fill = TRUE), by = "doi")
            }
          }
        }
      }
    }
  }
}

# Save final deduplicated results
fwrite(final_results, "openalex_results_final.csv")

cat("All searches completed, duplicates removed, and results saved.\n")
########################test
query_test4 <- oa_query(
  entity = "works",
  title_and_abstract.search = paste(
    paste(Productivity_Economics_chunks, collapse = " OR "),
    "OR",
    paste(Resilience_1_chunks, collapse = " OR ")
  ),
  from_publication_date = "2000-01-01",
  to_publication_date = "2024-01-01"
)

test_hits4 <- oa_request(query_url = query_test4)
test_hits4_df <- data.table(oa2df(test_hits4, entity = "works"))

# Print results
print(test_hits4_df)




cat("✅ All searches completed, duplicates removed, and results saved.\n")