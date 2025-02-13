# Carr et al search terms testing
# 0) Set up workspace ####
# 0.1) Load packages #####
# Load required packages
pacman::p_load(data.table, openalexR)

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

# Define the search data directory within the project folder
search_data_dir <- file.path(getwd(), "OA docs")

# Create the directory if it does not exist
if (!dir.exists(search_data_dir)) {
  dir.create(search_data_dir, recursive = TRUE)
}

# Print confirmation that the directory exists
print(dir.exists(search_data_dir))  # Should print TRUE if created successfully

# 1) Define Search Terms ####
Productivity_Economics <- c(
  "Benefit-cost analysis", "Economic impact", "Economic valuation",
  "Net present value", "Cost", "Gross margin", "Interest rate")

Resilience_1 <- c(
  "Climate change", "Global warming", "Drought resistance", 
  "Risk assessment", "Adaptive management", "Capacity building"
)

Resilience_2 <- c(
    "Soil fertility", "Soil degradation", "Soil loss", "Soil aggregation",
    "Water availability", "Water conservation", "Water consumption", 
    "Water footprint", "Water stress")

Agroforestry <- c(
    "Agroforestry", "Evergreen agriculture", "Farmer managed natural regeneration",
    "Silvopastoral systems", "Silvoarable systems", "Parkland management"
  )

Agronomy <- c(
  "Adapted crops", "Resilient crops", "Integrated pest management", 
  # "Ecological pest control", "Conservation agriculture", "Cover cropping", 
  # "Crop diversification", "Crop rotation", "Minimum tillage", "No tillage", 
  # "Precision agriculture", "Soil conservation", "Water conservation", 
  "Erosion control", "Drip irrigation", "Direct planting")

Products <- c(
  "Maize", "Zea mays", "Common beans", "Phaseolus vulgaris", "Cattle", "Bos taurus",
  "Bos indicus", "Bull", "Heifer", "Bovine", "Coffee", "Coffea arabica", "Coffea robusta")

Livestock <- c(
  "Managed grazing", "Rotational grazing", "Holistic planned grazing", "Rational grazing",
  # "Intensive grazing", "Short duration grazing", "High intensity low frequency grazing",
  # "High intensity grazing", "Mob grazing", "Ultra-high stock density grazing",
  "Biomass accumulation grazing", "Strip grazing")

Countries_Americas <- c(
  "Belize", "Bolivia", "Brazil", "Colombia", "Costa Rica", "Cuba", "Dominica",
  # "Dominican Republic", "Ecuador", "El Salvador", "Grenada", "Guatemala", "Guyana",
  # "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Paraguay", "Peru",
  "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname", "Venezuela")


# Convert to Boolean expressions
Countries_Americas_boolean <- paste0("(", paste0(Countries_Americas, collapse = " OR "), ")")
Products_boolean <- paste0("(", paste0(Products, collapse = " OR "), ")")
Productivity_Economics_boolean <- paste0("(", paste0(Productivity_Economics, collapse = " OR "), ")")
Resilience1_boolean <- paste0("(", paste0(Resilience_1, collapse = " OR "), ")")
Resilience2_boolean <- paste0("(", paste0(Resilience_2, collapse = " OR "), ")")
Livestock_boolean <- paste0("(", paste0(Livestock, collapse = " OR "), ")")
Agronomy_boolean <- paste0("(", paste0(Agronomy, collapse = " OR "), ")")
Agroforestry_boolean <- paste0("(", paste0(Agroforestry, collapse = " OR "), ")")

# Final combined search string
final_search_string <- paste(
  Countries_Americas_boolean, 
  "AND", 
  # Products_boolean,
  # "AND",
  # Productivity_Economics_boolean,
  # "AND",
  # Resilience1_boolean,
  # "AND",
  # Resilience2_boolean,
  # "AND",
  Livestock_boolean,
  # "AND",
  #Agronomy_boolean,
  "AND",
  Agroforestry_boolean
)

# Save terms for reference
terms <- list(
  country = Countries_Americas_boolean,
  product = Products_boolean,
  productivity = Productivity_Economics_boolean,
  resilience1 = Resilience1_boolean,
  resilience2 = Resilience2_boolean,
  livestock = Livestock_boolean,
  agronomy = Agronomy_boolean,
  agroforestry = Agroforestry_boolean,
  full_search = final_search_string
)

save(terms, file = file.path(search_data_dir, "search_terms.RData"))

# Set date range for the search
from_year <- "2020-05-01"
to_year <- "2023-10-01"
overwrite <- TRUE  # Set to overwrite existing files if needed

# Set prefix for file naming
prefix <- "openalex"
save_file <- file.path(search_data_dir, paste0(prefix, "_search_terms.csv"))

# Run OpenAlex query
if (!file.exists(save_file) | overwrite) {
  # Create the API endpoint with the search string
  api_endpoint <- oa_query(
    entity = "works",
    title_and_abstract.search = terms$full_search,
    from_publication_date = from_year,
    to_publication_date = to_year
  )
  
  # Check if the search string is within the allowed length
  if (nchar(api_endpoint) > 4000) {
    stop(paste0("Encoded search string has ", nchar(api_endpoint), " characters. Max allowed is 4000."))
  }
  
  # Execute the query and save the results
  hits <- oa_request(query_url = api_endpoint)
  hits_tab <- data.table(oa2df(hits, entity = "works"))
  
  # Select and save relevant fields
  hits_tab <- hits_tab[, .(id, display_name, doi, url, relevance_score, language, type, publication_date)]
  fwrite(hits_tab, file = save_file)
}

write.csv(hits_tab,"OA docs/bundle1.csv")

############################################
test_query <- oa_query(
  entity = "works",
  title_and_abstract.search = "(Belize OR Bolivia OR Brazil) AND (Climate change OR Global warming)"
)

test_results <- oa_request(query_url = test_query)
print(test_results)


query_2 <- oa_query(
  entity = "works",
  title_and_abstract.search = "(Benefit-cost analysis OR Economic impact OR Gross margin) AND (Adaptive management OR Risk assessment OR Socio-ecological systems)"
)

test_results_2 <- oa_request(query_url = query_2)
print(test_results_2)

test_query <- oa_query(
  entity = "works",
  title_and_abstract.search = "(Belize OR Brazil) AND (Climate change OR Economic impact)"
)
test_results <- oa_request(query_url = test_query)
print(test_results)


