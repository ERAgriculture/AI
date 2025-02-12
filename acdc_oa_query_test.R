# Carr et al search terms testing
# 0) Set up workspace ####
# 0.1) Load packages #####
# Use p_load to install if not present and load the packages
pacman::p_load(data.table, openalexR)

# 1) Define Search Terms ####
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

# 2) Convert to Boolean Expressions ####
convert_to_boolean <- function(term_list) {
  paste0("(", paste(shQuote(term_list, type = "cmd"), collapse = " OR "), ")")
}

# Define search timeframe
from_year <- "1970-01-01"
to_year <- "2024-10-30"

# 3) Run Combined Queries Using AND Between Categories ####
run_combined_query <- function(category1, terms1, category2, terms2) {
  cat("🔍 Running combined query for:", category1, "AND", category2, "...\n")
  
  boolean_query1 <- convert_to_boolean(terms1)
  boolean_query2 <- convert_to_boolean(terms2)
  combined_query <- paste0(boolean_query1, " AND ", boolean_query2)
  
  # Check for query length
  if (nchar(combined_query) > 4000) {
    stop("Query too long, needs chunking!")
  }
  cat("Query length:", nchar(combined_query), "characters\n")
  
  api_endpoint <- oa_query(
    entity = "works",
    fulltext.search = combined_query,  # Search in full-text to match OpenAlex Web UI
    from_publication_date = from_year,
    to_publication_date = to_year
  )
  
  # Pagination support
  hits <- oa_request(query_url = api_endpoint, per_page = 200)
  
  # Check if there are additional pages
  while (!is.null(hits$meta$next_cursor)) {
    Sys.sleep(3)  # Avoid API throttling
    next_hits <- oa_request(query_url = paste0(api_endpoint, "&cursor=", hits$meta$next_cursor), per_page = 200)
    hits$results <- c(hits$results, next_hits$results)
    hits$meta$next_cursor <- next_hits$meta$next_cursor
  }
  
  hits_tab <- data.table(oa2df(hits, entity = "works"))
  
  # Convert list-type columns to character before saving
  for (col in names(hits_tab)) {
    if (is.list(hits_tab[[col]])) {
      hits_tab[[col]] <- sapply(hits_tab[[col]], function(x) paste(unlist(x), collapse = "; "))
    }
  }
  
  # Save combined query result
  dir.create("doc", showWarnings = FALSE)
  fwrite(hits_tab, paste0("doc/openalex_results_", category1, "_AND_", category2, ".csv"))
}

# 4) Run the Combined Queries ####
run_combined_query("Productivity_Economics", Productivity_Economics, "Resilience_1", Resilience_1)

# 5) Combine All CSV Results & Remove Duplicates ####
all_files <- list.files("doc", pattern = "openalex_results_.*\.csv", full.names = TRUE)
all_data <- rbindlist(lapply(all_files, fread), fill = TRUE)
final_results <- unique(all_data, by = "doi")

# Save final deduplicated results
fwrite(final_results, "doc/openalex_results_final.csv")

cat("✅ All searches completed, duplicates removed, and results saved.\n")