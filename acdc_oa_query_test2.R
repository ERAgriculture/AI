###############################################
# 1. Load Required Packages
###############################################
library(openalexR)
library(data.table)

###############################################
# 2. Create Terms and Boolean Expressions
###############################################

# Define your search term vectors
outcomes_terms <- c(
  "Benefit-cost analysis", "Break-even period", "Economic valuation",
  "Economic impact", "Net present value", "Payback period", "Willingness to pay",
  "Gross margin","Kilograms per hectare","Cost",
  "Climate change", "Risk assessment", "Vulnerability assessment",
  "Adaptive management", "Community awareness", "Drought resistance","Indigenous knowledge",
  "Changing climate","Flood resistance", "Soil fertility", "Soil degradation", 
  "Biodiversity","Soil degradation","Yield loss cost","Revenue","Yield",
  "Food security", "Water availability", "Water stress"
)

practices_terms <- c(
  "Agroforestry", "Evergreen agriculture", "Farmer managed natural regeneration",
  "Silvopastoral systems", "Alley cropping", "Crop rotation", "Cover cropping",
  "Integrated pest management", "No tillage","Minimum tillage", "Soil conservation",
  "Adapted cultivar","Crop diversification","Crop rotation","Green manure", 
  "Water harvesting", "Managed grazing", "Irrigation","Residue retention",
  "Rotational grazing", "Intensive grazing", "Biological pest control", "Managed grazing",
  "Organic fertilizers","Short duration grazing","Strip grazing"
)

product_terms <- c(
  "Maize", "Zea mays", "Common beans", "Phaseolus vulgaris", "Cattle", "Bos taurus",
  "Bos indicus", "Bull", "Heifer", "Bovine", "Coffee", "Coffea arabica", "Coffea robusta")

region_terms <- c(
  "Belize", "Bolivia", "Brazil", "Colombia", "Costa Rica", "Cuba", "Dominica",
  "Dominican Republic", "Ecuador", "El Salvador", "Grenada", "Guatemala",
  "Guyana", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Paraguay",
  "Peru", "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname",
  "Venezuela", "Argentina", "Bahamas", "Barbados", "Chile", "Panama",
  "Trinidad and Tobago", "Uruguay"
)

# Wrap each term in quotes and join with OR
outcomes_bool <- paste0('"', outcomes_terms, '"', collapse = " OR ")
practices_bool <- paste0('"', practices_terms, '"', collapse = " OR ")
region_bool   <- paste0('"', region_terms, '"', collapse = " OR ")
product_bool   <- paste0('"', region_terms, '"', collapse = " OR ")
# Combine the three groups into one filter string, using the same syntax as the web interface.
filter_string <- paste0(
  "filter=title_and_abstract.search:(",
  outcomes_bool, 
  ") AND (",
  practices_bool, 
  ") AND (",
  product_bool, 
  ") AND (",
  region_bool, 
  ")"
)

###############################################
# 3. Build the Final API URL
###############################################

base_url <- "https://api.openalex.org/works?"
# We add sorting and request 200 results per page (note the hyphen in per-page)
final_url <- paste0(base_url, filter_string, "&sort=relevance_score:desc&per-page=200")

# Do NOT URLencode the entire URL because that will encode the scheme, causing errors.
cat("Final API URL:\n", final_url, "\n\n")

###############################################
# 4. Define a Function for Cursor-Based Pagination
###############################################

fetch_all_hits <- function(start_url) {
  all_hits <- list()
  cursor <- "*"  # initial cursor value
  
  repeat {
    # URL-encode only the cursor value (so "*" becomes "%2A")
    cursor_encoded <- URLencode(cursor, reserved = TRUE)
    current_url <- paste0(start_url, "&cursor=", cursor_encoded)
    message("Requesting: ", current_url)
    
    # Make the API request
    res <- oa_request(query_url = current_url)
    hits_df <- oa2df(res, entity = "works")
    
    # If no records returned, exit the loop
    if (nrow(hits_df) == 0) break
    
    all_hits[[length(all_hits) + 1]] <- hits_df
    
    # Get the next cursor from the attributes
    new_cursor <- attr(hits_df, "next_cursor")
    if (is.null(new_cursor) || new_cursor == cursor) break
    
    cursor <- new_cursor
    message("Fetched page ", length(all_hits), " with ", nrow(hits_df), " records.")
  }
  
  data.table::rbindlist(all_hits, fill = TRUE)
}

###############################################
# 5. Execute the Query and Retrieve Results
###############################################

results <- fetch_all_hits(final_url)
message("Total records retrieved: ", nrow(results))

# Optionally, select relevant columns and save the results to CSV
if (nrow(results) > 0) {
  results <- results[, .(id, display_name, doi, url, relevance_score, language, type, publication_date,ab)]
  fwrite(results, "OA outputs/openalex_results.csv")
  message("Results saved to 'openalex_results.csv'.")
} else {
  message("No records found.")
}

system("gzip 'OA outputs/openalex_results.csv'")


###############################################
# 6. Testing the Results
###############################################


