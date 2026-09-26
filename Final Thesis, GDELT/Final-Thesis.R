# ---------------------------------------------------------------------------
# Final Thesis - What Drives Media Attention to Global Events? (GDELT)
# Henrik B. Sorensen
#
# Code extracted from 'Final Thesis.Rmd'. Prose, chunk headers and knitr
# rendering directives have been removed; only the R that performs the
# analysis remains. Regenerate with:
#   knitr::purl("Final Thesis.Rmd", output = "Final-Thesis.R", documentation = 0)
# ---------------------------------------------------------------------------


# Create a list of required packages
# car         - supporting further analysis and for testing the model
# countrycode - mapping ISO3/FIPS country codes to readable country names
# dplyr       - working with data frames in memory
# ggplot2     - handling plots
# here        - assisting location file system
# lubridate   - handling times and dates
# MASS        - negative binomial regression (called as MASS::glm.nb, since attaching
#               MASS would mask dplyr::select)
# purrr       - handling functional programming
# readr       - reading csv files
# rpart       - regression tree for the third model
# scales      - percent/comma axis labels on the plots
# tidyr       - generally working with structured data
required_packages <- c("here", "readr", "dplyr", "tidyr", "lubridate", "purrr",
                       "ggplot2", "car", "countrycode", "scales", "rpart", "MASS")

# Set default URLs to point to CRAN Mirror (to simplify installing packages)
if (length(getOption("repos")) == 0 || getOption("repos")["CRAN"] == "@CRAN@")
  options(repos = c(CRAN = "https://cloud.r-project.org"))

# Discover missing packages
missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]

# Install any missing packages
if (length(missing_packages))
  install.packages(missing_packages)

# Load the libraries. suppressWarnings() hides the "package X was built under R
# version Y" notices that R raises when a package was compiled against a different
# R build than the one running it. Those are about this machine's installation,
# not about the analysis, and there is one for nearly every package.
invisible(lapply(required_packages,
                 function(pkg) suppressWarnings(library(pkg, character.only = TRUE))))

# Create the table data
tools_tab <- matrix(
  c("Operating System", "Windows 11 Home, 25H2", "Main Operating System",
    "Notepad++", "Notepad++ 8.9.1", "Scratch Pad while editing R/Rmd code",
    "PowerShell", "PowerShell 7.6.3", "Various scripting",
    "RStudio", "RStudio 2026.07.1+147 Pacific Dogwood for Windows", "Main IDE for Developing and running the code",
    "Claude", "Claude for Windows, Version 1.24012.9", "Assistant for validating syntax and web search"),
  ncol = 3, byrow = TRUE)
# Set the column names
colnames(tools_tab) <- c("Tool", "Version", "Usage")

# Create the data table
data_urls_tab <- matrix(
c("GDELT Event Database", "http://data.gdeltproject.org/events/"),  ncol = 2, byrow = TRUE
)
# Set the column names
colnames(data_urls_tab) <- c("Source", "Url")

# First, create the data for the figure
  # Coordinates of the blocks
  # Labels for the blocks
  # Colors
quad_df <- data.frame(
  xmin  = c(0, 1, 0, 1),
  xmax  = c(1, 2, 1, 2),
  ymin  = c(1, 1, 0, 0),
  ymax  = c(2, 2, 1, 1),
  label = c("Verbal\nCooperation\n(QuadClass 1)",
            "Verbal\nConflict\n(QuadClass 3)",
            "Material\nCooperation\n(QuadClass 2)",
            "Material\nConflict\n(QuadClass 4)"),
  fill  = c("#56B4E9", "#E69F00", "#0072B2", "#D55E00")
)

# Render the figure itself
print(ggplot(quad_df) +
  # Render the rectangles
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            color = "white", linewidth = 1.2) +
  # Add labels
  geom_text(aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
            color = "white", fontface = "bold", size = 4, lineheight = 0.9) +
  # Use values without scaling
  scale_fill_identity() +
  # Add annotations
  annotate("text", x = 1, y = 2.15,
           label = "<- Cooperation            Conflict ->",
           size = 3.6, fontface = "italic") +
  annotate("text", x = -0.2, y = 1,
           label = "Material                Verbal",
           angle = 90, size = 3.6, fontface = "italic") +
  # Zoom in
  coord_cartesian(xlim = c(-0.45, 2), ylim = c(0, 2.3), clip = "off") +
  # Blank themes as default
  theme_void() +
  # Set margins of the figure
  theme(plot.margin = margin(10, 10, 10, 25)))


# Create a Matrix for generating the URLs for downloading the code
data_urls_and_methods_tab <- matrix(
c("GDELT Event Database", "http://data.gdeltproject.org/events/", "Download + Unzip (per day)"),  ncol = 3, byrow = TRUE
)
# Assign column names
colnames(data_urls_and_methods_tab) <- c("Source", "Url", "Method")

# Create function for downloading file from url into destination
# Only download file if this is missing from the destination
download_file <- function(url, destination_file) {

  # download.file() does not create missing directories itself - it just
  # errors out, so the destination folder (e.g. "data/" next to this .Rmd)
  # must be created first, before checking whether the file already exists.
  dir.create(dirname(destination_file), recursive = TRUE, showWarnings = FALSE)

  # Download file if missing
  if (!file.exists(destination_file))
    download.file(url, destination_file, mode = "wb")
}


# Create a function which downloads and unpacks one day's GDELT export file, 
# skipping anything already on disk and tolerating days missing from the server
# (early archive gaps).
download_gdelt_day <- function(day, destination_folder = "data/gdelt") {
  # Generate the string representation of the day
  day_str <- format(day, "%Y%m%d")
  # Generate the full url based on the day string
  url <- sprintf("http://data.gdeltproject.org/events/%s.export.CSV.zip", day_str)

  # Generate the full paths for the zip and csv files
  zip_file <- file.path(destination_folder, paste0(day_str, ".export.CSV.zip"))
  csv_file <- file.path(destination_folder, paste0(day_str, ".export.CSV"))

  # Skip if the csv file already has been extracted from a downloaded zip file
  if (file.exists(csv_file)) return(invisible(NULL))

  # Download and unzip the file catching any error.
  tryCatch({
    download_file(url, zip_file)
    unzip(zip_file, exdir = destination_folder)
  }, error = function(e) NULL)
}

# Downloads every day in [start_date, end_date] into destination_folder
download_gdelt_events <- function(start_date, end_date, destination_folder = "data/gdelt") {
  days <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  for (day in days) {
    download_gdelt_day(as.Date(day, origin = "1970-01-01"), destination_folder)
  }
}


# Scope: one complete month (August) to begin with. This can be Widened if
# time and interest permits it
gdelt_start_date <- "2026-08-01"
gdelt_end_date   <- "2026-08-31"

# "Lock file" preventing further download
lock_file <- "data/gdelt.lock"

# Check for presence of lock file.
# If present, don't download data.
# If not present, download data
if (!file.exists(lock_file)) {
  download_gdelt_events(gdelt_start_date, gdelt_end_date)

  # Create file to prevent download
  # Remember to delete this date range is changed/expanded.
  cat("Done", file = lock_file)
}


# Create data set
field_layout <- data.frame(
  # List fields (columns in .csv file)
  Field = c(
    "GlobalEventID", "Day", "MonthYear", "Year", "FractionDate",
    "Actor1Code", "Actor1Name", "Actor1CountryCode", "Actor1KnownGroupCode", "Actor1EthnicCode",
    "Actor1Religion1Code", "Actor1Religion2Code", "Actor1Type1Code", "Actor1Type2Code", "Actor1Type3Code",
    "Actor2Code", "Actor2Name", "Actor2CountryCode", "Actor2KnownGroupCode", "Actor2EthnicCode",
    "Actor2Religion1Code", "Actor2Religion2Code", "Actor2Type1Code", "Actor2Type2Code", "Actor2Type3Code",
    "IsRootEvent", "EventCode", "EventBaseCode", "EventRootCode", "QuadClass",
    "GoldsteinScale", "NumMentions", "NumSources", "NumArticles", "AvgTone",
    "Actor1Geo_Type", "Actor1Geo_Fullname", "Actor1Geo_CountryCode", "Actor1Geo_ADM1Code",
    "Actor1Geo_Lat", "Actor1Geo_Long", "Actor1Geo_FeatureID",
    "Actor2Geo_Type", "Actor2Geo_Fullname", "Actor2Geo_CountryCode", "Actor2Geo_ADM1Code",
    "Actor2Geo_Lat", "Actor2Geo_Long", "Actor2Geo_FeatureID",
    "ActionGeo_Type", "ActionGeo_Fullname", "ActionGeo_CountryCode", "ActionGeo_ADM1Code",
    "ActionGeo_Lat", "ActionGeo_Long", "ActionGeo_FeatureID",
    "DATEADDED", "SOURCEURL"
  ),
  # Whether the column is actually used in the planned analysis (Y) or
  # omitted (N) - e.g. IDs, free-text names, sparse secondary actor codes,
  # and fine-grained geo fields not needed at country-level resolution.
  Used = c(
    "N", "Y", "N", "N", "N",
    "Y", "Y", "Y", "N", "N",
    "N", "N", "Y", "Y", "Y",
    "Y", "Y", "Y", "N", "N",
    "N", "N", "Y", "Y", "Y",
    "Y", "N", "Y", "Y", "Y",
    "Y", "Y", "Y", "Y", "Y",
    "N", "N", "N", "N",
    "N", "N", "N",
    "N", "N", "N", "N",
    "N", "N", "N",
    "Y", "N", "Y", "N",
    "N", "N", "N",
    "N", "Y"
  ),
  # Add description
  Description = c(
    "Globally unique identifier for this event record.",
    "Date the event took place, in YYYYMMDD format.",
    "Date the event took place, in YYYYMM format.",
    "Year the event took place, in YYYY format.",
    "Date expressed as a fraction (YYYY.FFFF) for fractional-year calculations.",
    "Complete raw CAMEO code for Actor1.",
    "Name of Actor1.",
    "3-character CAMEO country code for Actor1's affiliation.",
    "CAMEO code for a known organization Actor1 belongs to (e.g. UN, NATO).",
    "CAMEO code for Actor1's ethnic affiliation, if identified.",
    "CAMEO code for Actor1's primary religious affiliation.",
    "CAMEO code for a secondary religious affiliation of Actor1.",
    "CAMEO code for Actor1's primary role/type (e.g. government, military, rebels).",
    "CAMEO code for a secondary role/type of Actor1.",
    "CAMEO code for a tertiary role/type of Actor1.",
    "Complete raw CAMEO code for Actor2.",
    "Name of Actor2.",
    "3-character CAMEO country code for Actor2's affiliation.",
    "CAMEO code for a known organization Actor2 belongs to.",
    "CAMEO code for Actor2's ethnic affiliation, if identified.",
    "CAMEO code for Actor2's primary religious affiliation.",
    "CAMEO code for a secondary religious affiliation of Actor2.",
    "CAMEO code for Actor2's primary role/type.",
    "CAMEO code for a secondary role/type of Actor2.",
    "CAMEO code for a tertiary role/type of Actor2.",
    "Flag (1/0) marking whether this was the primary 'root' event of the source article.",
    "Raw CAMEO action code describing the interaction between Actor1 and Actor2.",
    "EventCode rolled up to its 3-digit CAMEO base category.",
    "EventCode rolled up to its 2-digit CAMEO root category.",
    "One of four broad classes derived from EventCode: Verbal/Material Cooperation/Conflict.",
    "Fixed theoretical impact score (-10 to +10) assigned once per event type.",
    "Total number of source mentions of this event across all coverage found.",
    "Number of distinct information sources reporting this event.",
    "Total number of source articles containing this event.",
    "Average sentiment tone (-100 to +100) of all articles mentioning the event.",
    "Geographic resolution level of Actor1's location (country, state, city, etc.).",
    "Full human-readable name of Actor1's location.",
    "Country code of Actor1's location.",
    "Country plus first-level administrative division code of Actor1's location.",
    "Latitude of Actor1's location.",
    "Longitude of Actor1's location.",
    "GDELT gazetteer feature ID of Actor1's location.",
    "Geographic resolution level of Actor2's location.",
    "Full human-readable name of Actor2's location.",
    "Country code of Actor2's location.",
    "Country plus first-level administrative division code of Actor2's location.",
    "Latitude of Actor2's location.",
    "Longitude of Actor2's location.",
    "GDELT gazetteer feature ID of Actor2's location.",
    "Geographic resolution level of where the action itself took place.",
    "Full human-readable name of the action's location.",
    "Country code of the action's location.",
    "Country plus first-level administrative division code of the action's location.",
    "Latitude of the action's location.",
    "Longitude of the action's location.",
    "GDELT gazetteer feature ID of the action's location.",
    "Date/time (YYYYMMDDHHMMSS) this record was added to the GDELT master file.",
    "URL of the source news article the event was extracted from."
  )
)

# The raw files have no header, so read_tsv() needs the FULL 58-column
# layout as plain column names to line up every field correctly - cols_only()
# below is what actually keeps only the "Used" subset.
gdelt_all_columns <- field_layout$Field

# Read only the columns we need from every file, then combine into a single table
# Get list of all the unzipped .csv files
gdelt_files <- list.files("data/gdelt", pattern = "\\.export\\.CSV$",
                          full.names = TRUE)

# Read all files into data frame and map the corresponding columns
gdelt_data <- map_dfr(
  #CSV file names
  gdelt_files,
  ~ read_tsv(
      .x,
      col_names = gdelt_all_columns,
      # This list mirrors the "Used" column of the field layout table above:
      # every field marked "Y" there is read here, and nothing else.
      col_types = cols_only(
        # Event timing and identity
        Day               = col_date(format = "%Y%m%d"),
        IsRootEvent       = col_integer(),
        EventBaseCode     = col_character(),
        EventRootCode     = col_character(),
        QuadClass         = col_integer(),
        GoldsteinScale    = col_double(),
        # Coverage and tone - the response variables
        NumMentions       = col_integer(),
        NumSources        = col_integer(),
        NumArticles       = col_integer(),
        AvgTone           = col_double(),
        # Actor 1
        Actor1Code        = col_character(),
        Actor1Name        = col_character(),
        Actor1CountryCode = col_character(),
        Actor1Type1Code   = col_character(),
        Actor1Type2Code   = col_character(),
        Actor1Type3Code   = col_character(),
        # Actor 2
        Actor2Code        = col_character(),
        Actor2Name        = col_character(),
        Actor2CountryCode = col_character(),
        Actor2Type1Code   = col_character(),
        Actor2Type2Code   = col_character(),
        Actor2Type3Code   = col_character(),
        # Geography of the action itself
        ActionGeo_Type    = col_integer(),
        ActionGeo_CountryCode = col_character(),
        # Provenance
        SOURCEURL             = col_character()
      )
  )
)

# Display the top items as a formatted table, split into 5 narrower tables
# (event info / actors / coverage & geography) so each fits a portrait page
# instead of needing a landscape table for all 15 columns at once.
gdelt_preview <- head(gdelt_data)


# Source URLs run to nearly 200 characters, which overflows the page width and
# produces an unreadable table. They are shortened for display only - nothing in
# gdelt_data itself is altered.
url_display_width <- 150
url_preview <- gdelt_preview["SOURCEURL"]
url_preview$SOURCEURL <- ifelse(
  nchar(url_preview$SOURCEURL) > url_display_width,
  paste0(substr(url_preview$SOURCEURL, 1, url_display_width - 3), "..."),
  url_preview$SOURCEURL
)


# Get the number of rows available
cat(format(nrow(gdelt_data), big.mark = ","), " rows of GDELT event data\n")


# CAMEO's international/regional codes (Table 3.3 of the CAMEO manual) - these
# aren't real ISO3 country codes, so countrycode() alone won't resolve them.
cameo_region_codes <- c(
  AFR = "Africa", ASA = "Asia", BLK = "Balkans", CRB = "Caribbean", CAU = "Caucasus",
  CFR = "Central Africa", CAS = "Central Asia", CEU = "Central Europe", EIN = "East Indies",
  EAF = "Eastern Africa", EEU = "Eastern Europe", EUR = "Europe", LAM = "Latin America",
  MEA = "Middle East", MDT = "Mediterranean", NAF = "North Africa", NMR = "North America",
  PGS = "Persian Gulf", SCN = "Scandinavia", SAM = "South America", SAS = "South Asia",
  SEA = "Southeast Asia", SAF = "Southern Africa", WAF = "West Africa", WST = "\"the West\""
)

# Show the top 50 actor country codes (ISO3 + CAMEO region codes), by frequency
# Sort based on Country code and summarize country codes
actor_country_counts <- sort(table(c(gdelt_data$Actor1CountryCode, gdelt_data$Actor2CountryCode)),
                              decreasing = TRUE)

# Select the country codes of the top 15 actor 1
actor_country_top <- head(actor_country_counts, 15)
# Select the country names of the top 15 actor 1
actor_country_names <- countrycode(names(actor_country_top), origin = "iso3c",
                                   destination = "country.name",
                                   # Feed the CAMEO region codes straight in, so AFR,
                                   # MEA and WST resolve here instead of falling through
                                   # as unmatched values and raising a warning.
                                   custom_match = cameo_region_codes)
# Safety net: anything that is neither an ISO3 country nor a known CAMEO
# region code stays NA here and keeps its raw code.
unresolved <- is.na(actor_country_names)
# Find the unresolved country named
actor_country_names[unresolved] <- cameo_region_codes[names(actor_country_top)[unresolved]]


# Not all actor types are observed. So, only select the observed actor types 
observed_actor_types <- sort(unique(c(gdelt_data$Actor1Type1Code, gdelt_data$Actor2Type1Code)))
# Discard the not observed types
observed_actor_types <- observed_actor_types[!is.na(observed_actor_types)]


# Root categories sourced directly from the CAMEO manual (Chapter 6: CAMEO Event Codes)
event_root_defs <- c(
  "01" = "Make Public Statement", "02" = "Appeal", "03" = "Express Intent to Cooperate",
  "04" = "Consult", "05" = "Engage in Diplomatic Cooperation", "06" = "Engage in Material Cooperation",
  "07" = "Provide Aid", "08" = "Yield", "09" = "Investigate", "10" = "Demand",
  "11" = "Disapprove", "12" = "Reject", "13" = "Threaten", "14" = "Protest",
  "15" = "Exhibit Force Posture", "16" = "Reduce Relations", "17" = "Coerce",
  "18" = "Assault", "19" = "Fight", "20" = "Use Unconventional Mass Violence"
)

# Plot the raw data
print(ggplot(gdelt_data, aes(x = NumArticles)) +
  # Histogram of the data
  geom_histogram(bins = 50) +
  # Set legends
  labs(title = "Distribution of Number of Articles", 
       x = "Number of Articles", y = "Count") +
  # Reduce the font size 
  theme_minimal() +
    theme(
      axis.text.y = element_text(size = 6),
      axis.text.y.right = element_text(size = 6),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 10), # Graph title font size
      axis.title = element_text(size = 10)  # X and Y axis title font sizes
    ) )

# Plot logarithmic data
print(ggplot(gdelt_data, aes(x = log1p(NumArticles))) +
  # Histogram of the data
  geom_histogram(bins = 50) +
  # Set legends
  labs(title = "Distribution of Number of Articles", 
       x = "Number of Articles (logarithmic)", y = "Count") +
  # Reduce the font size 
  theme_minimal() +
    theme(
      axis.text.y = element_text(size = 6),
      axis.text.y.right = element_text(size = 6),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 10), # Graph title font size
      axis.title = element_text(size = 10)  # X and Y axis title font sizes
    ))


# Count the distinct Source URLs.
# n_distinct() works directly on the vector - distinct() is the data-frame verb
# and would need distinct(gdelt_data, SOURCEURL) instead.
n_unique <- n_distinct(gdelt_data$SOURCEURL)

# Show number of unique URLs
cat(format(n_unique, big.mark = ","), "unique URLs out of ",
    format(nrow(gdelt_data), big.mark = ","), "entries\n")


# Count how many event rows each unique article produced, then bucket everything
# from 11 upwards into a single "11+" category so the bar chart stays readable.
rows_per_article <- gdelt_data %>%
  # One group per source article
  count(SOURCEURL, name = "event_rows") %>%
  # Collapse the long tail
  mutate(bucket = ifelse(event_rows > 10, "11+", as.character(event_rows))) %>%
  # Count how many articles fall in each bucket
  count(bucket, name = "articles") %>%
  # Force the natural 1,2,...,10,11+ ordering rather than alphabetical
  mutate(bucket = factor(bucket, levels = c(as.character(1:10), "11+")))

# Show the plot
print(ggplot(rows_per_article, aes(x = bucket, y = articles)) +
  # Simple bar chart
  geom_col(fill = "#0072B2") +
  # Thousands separators on the count axis
  scale_y_continuous(labels = scales::comma) +
  # Set legends
  labs(title = "Most articles yield several GDELT event rows",
       x = "Event rows extracted from one article", y = "Articles") +
  theme_minimal()+
  # Reduce the text size
  theme(
      axis.text.y = element_text(size = 6),
      axis.text.y.right = element_text(size = 6),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 10), # Graph title font size
      axis.title = element_text(size = 10)  # X and Y axis title font sizes
    ))

# Report the headline ratio in the text as well
cat(format(nrow(gdelt_data) / n_unique, digits = 2),
    " event rows per unique article\n")

# Sort coverage from most-covered event down, then form the cumulative share
articles_sorted <- sort(gdelt_data$NumArticles, decreasing = TRUE)
# Create data frame of event share and coverage share
event_coverage_shares <- data.frame(
  # Share of events, from the most covered downwards
  event_share    = seq_along(articles_sorted) / length(articles_sorted),
  # Share of total coverage those events account for
  coverage_share = cumsum(articles_sorted) / sum(articles_sorted)
)

# 2.8M points would make an enormous PDF for what is a smooth curve, so thin it out
event_coverage_shares_short <- event_coverage_shares[seq(1, nrow(event_coverage_shares), length.out = 2000), ]

# Plot diagram
print(ggplot(event_coverage_shares_short, aes(x = event_share, y = coverage_share)) +
  # The concentration curve itself
  geom_line(linewidth = 1, colour = "#CC79A7") +
  # Reference: what perfectly even coverage would look like
  geom_abline(linetype = 2, colour = "grey50") +
  # Both axes are shares, so show them as percentages
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  # Set legends
  labs(title = "A small number of events absorb most of the coverage",
       x = "Share of events (most-covered first)",
       y = "Share of all coverage") +
  theme_minimal() +
    # Add styling
    theme(
      axis.text.y = element_text(size = 6),
      axis.text.y.right = element_text(size = 6),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 10), # Graph title font size
      axis.title = element_text(size = 10)  # X and Y axis title font sizes
    ))

# Quote the two numbers worth remembering
top_share <- function(p) {
  round(100 * sum(articles_sorted[1:floor(length(articles_sorted) * p)]) /
        sum(articles_sorted), 1)
}

# Print top x% of events
cat("Top 1% of events carry ", top_share(0.01), "% of all coverage\n", sep = "")
cat("Top 10% of events carry ", top_share(0.10), "% of all coverage\n", sep = "")

# One bar per integer instead of binning - the binning is what hid this
low_counts <- gdelt_data %>%
  # Only the low end, which is where nearly all the mass is
  filter(NumArticles <= 30) %>%
  # Exact count per integer value
  count(NumArticles)

# Plot the diagram
print(ggplot(low_counts, aes(x = NumArticles, y = n)) +
  # Bar per integer value
  geom_col(fill = "#D55E00") +
  # Label every second integer so the axis stays legible
  scale_x_continuous(breaks = seq(0, 30, 2)) +
  scale_y_continuous(labels = scales::comma) +
  # Set legends
  labs(title = "The low end is lumpy, not smooth",
       x = "NumArticles", y = "Events") +
  theme_minimal()+
      # Add styling
      theme(
      axis.text.y = element_text(size = 6),
      axis.text.y.right = element_text(size = 6),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 10), # Graph title font size
      axis.title = element_text(size = 10)  # X and Y axis title font sizes
    ))

# Attach the human-readable action names defined in the reference table above
coverage_by_type <- gdelt_data %>%
  mutate(Action = event_root_defs[EventRootCode]) %>%
  # A handful of rows carry root codes outside the documented 01-20 range
  filter(!is.na(Action))

# Overall median, used as the reference line in this plot and the next
overall_median <- median(log1p(gdelt_data$NumArticles))

# Plot the diagram
print(ggplot(coverage_by_type,
       aes(x = reorder(Action, log1p(NumArticles), median),
           y = log1p(NumArticles))) +
  # Outliers hidden - with millions of rows they would swamp the boxes
  geom_boxplot(outlier.shape = NA, fill = "#56B4E9") +
  # Reference line: the median across every event, regardless of type
  geom_hline(yintercept = overall_median, linetype = 2, colour = "red") +
  # Horizontal layout, clipped to where the boxes actually are
  coord_flip(ylim = c(0, 4)) +
  # Set legends
  labs(title = "Coverage on event types",
       x = NULL, y = "log1p(NumArticles)") +
  theme_minimal() +
    # Add styling
    theme(
      axis.text.y = element_text(size = 6),
      axis.text.y.right = element_text(size = 6),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 10), # Graph title font size
      axis.title = element_text(size = 10)  # X and Y axis title font sizes
    ))

# Both actor plots are identical apart from which column they group by, so the
# body lives in one helper rather than being written out twice.
plot_coverage_by_actor <- function(type_column, plot_title) {
  actor_data <- gdelt_data %>%
    # Keep unresolved actors visible instead of dropping them
    mutate(ActorType = ifelse(is.na(.data[[type_column]]),
                              "(none)", .data[[type_column]])) %>%
    # Count events per role, then drop roles too rare to draw a meaningful box
    add_count(ActorType, name = "n_events") %>%
    filter(n_events >= 1000)

  # Plot diagram
  ggplot(actor_data, aes(x = reorder(ActorType, log1p(NumArticles), median),
                         y = log1p(NumArticles))) +
    # Outliers hidden - with millions of rows they would swamp the boxes
    geom_boxplot(outlier.shape = NA, fill = "#009E73") +
    # Same reference line as the event-type figure, so the two are comparable
    geom_hline(yintercept = overall_median, linetype = 2, colour = "red") +
    # Same y-range as the event-type figure - that comparison is the whole point
    coord_cartesian(ylim = c(0, 4)) +
    # Set legends
    labs(title = plot_title,
         x = "CAMEO actor type code", y = "log1p(NumArticles)") +
    theme_minimal() +
    # Add styling
      theme(
        axis.text.y = element_text(size = 6),
        axis.text.y.right = element_text(size = 6),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size = 10), # Graph title font size
        axis.title = element_text(size = 10)  # X and Y axis title font sizes
      )
}

print(plot_coverage_by_actor("Actor1Type1Code", "Coverage by Actor1 type"))
print(plot_coverage_by_actor("Actor2Type1Code", "Coverage by Actor2 type"))


# Put a number on what the two plots show: how far apart are the per-group medians?
median_spread <- function(group_column) {
  gdelt_data %>%
    # Add group column
    mutate(grp = ifelse(is.na(.data[[group_column]]), "(none)", .data[[group_column]])) %>%
    # Add count column
    add_count(grp, name = "n_events") %>%
    # Filter events with a count below 1000
    filter(n_events >= 1000) %>%
    # Group the data 
    group_by(grp) %>%
    # Collapse each group to one number: its median coverage
    summarise(med = median(log1p(NumArticles)), .groups = "drop") %>%
    # Collapse those group medians to a single number: the range between them
    summarise(spread = max(med) - min(med)) %>%
    # Only extract the spread
    pull(spread)
}

# Print out the medians
cat("Spread of group medians, log1p(NumArticles):\n")
cat("  Actor1 type   : ", round(median_spread("Actor1Type1Code"), 3), "\n", sep = "")
cat("  Actor2 type   : ", round(median_spread("Actor2Type1Code"), 3), "\n", sep = "")
cat("  Event root code: ", round(median_spread("EventRootCode"), 3), "\n", sep = "")


# Pull the host out of the source URL: strip the scheme, then everything from the
# first "/" onwards, then a leading "www." so that www.bbc.co.uk and bbc.co.uk are
# not treated as two different outlets.
host <- sub("^https?://", "", gdelt_data$SOURCEURL)
host <- sub("/.*$", "", host)
gdelt_data$SourceDomain <- sub("^www[.]", "", host)

# Free the intermediate vector again - it holds millions of strings
rm(host)

# Printout distinct sources (domains)
cat(format(n_distinct(gdelt_data$SourceDomain), big.mark = ","),
    " distinct source domains\n")


# The 15 outlets contributing the most event rows
top_domains <- gdelt_data %>%
  count(SourceDomain, sort = TRUE) %>%
  slice_head(n = 15) %>%
  pull(SourceDomain)

# Plot diagram
print(ggplot(gdelt_data %>% filter(SourceDomain %in% top_domains),
       aes(x = reorder(SourceDomain, log1p(NumArticles), median),
           y = log1p(NumArticles))) +
  geom_boxplot(outlier.shape = NA, fill = "#009E73") +
  # Same reference line as the previous figure
  geom_hline(yintercept = overall_median, linetype = 2, colour = "red") +
  # Same axis limits as the previous figure - this is the whole point
  coord_flip(ylim = c(0, 4)) +
  # Set legends
  labs(title = "Source identity separates more than event type does",
       x = NULL, y = "log1p(NumArticles)") +
  theme_minimal() +
    # Add styling
    theme(
      axis.text.y = element_text(size = 6),
      axis.text.y.right = element_text(size = 6),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 10), # Graph title font size
      axis.title = element_text(size = 10)  # X and Y axis title font sizes
    ))

print(gdelt_data %>%
  # Count the Source domains
  count(SourceDomain, sort = TRUE) %>%
  # Take the first 20 instances
  slice_head(n = 20) %>%
  # Plot the information
  ggplot(aes(x = reorder(SourceDomain, n), y = n)) +
  geom_col(fill = "#0072B2") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  # Set legends
  labs(title = "Event rows by source domain (top 20)",
       x = NULL, y = "Event rows") +
  theme_minimal() +
  # Add Styling
    theme(
      axis.text.y = element_text(size = 6),
      axis.text.y.right = element_text(size = 6),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 10), # Graph title font size
      axis.title = element_text(size = 10)  # X and Y axis title font sizes
    ))


print(gdelt_data %>%
  # Drop rows where GDELT could not geolocate the action at all
  filter(!is.na(ActionGeo_CountryCode)) %>%
  # Count the entries sorted by the country code of where, the event happened
  count(ActionGeo_CountryCode, sort = TRUE) %>%
  # Take the top 20 entries
  slice_head(n = 20) %>%
  # Resolve FIPS codes to readable names, keeping the raw code if it will not resolve
  mutate(Country = countrycode(ActionGeo_CountryCode,
                               origin = "fips", destination = "country.name"),
         Country = ifelse(is.na(Country), ActionGeo_CountryCode, Country)) %>%
  # Plot the diagram
  ggplot(aes(x = reorder(Country, n), y = n)) +
  geom_col(fill = "#E69F00") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  # Set legends
  labs(title = "Events by location of the action (top 20)",
       x = NULL, y = "Events") +
  theme_minimal() +
    # Add styling
    theme(
      axis.text.y = element_text(size = 6),
      axis.text.y.right = element_text(size = 6),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 10), # Graph title font size
      axis.title = element_text(size = 10)  # X and Y axis title font sizes
    ))


coverage_by_region <- gdelt_data %>%
  # Roll the FIPS country code up to a World Bank region. A few codes are not
  # countries at all (OS = oceans, plus a handful of disputed or newer territories)
  # and some events are never geolocated, so rather than dropping those rows they
  # are kept visible as their own category.
  mutate(Region = countrycode(ActionGeo_CountryCode,
                              origin = "fips", destination = "region", warn = FALSE),
         Region = ifelse(is.na(Region), "(unresolved)", Region))

# Plot the diagram
print(ggplot(coverage_by_region,
       aes(x = reorder(Region, log1p(NumArticles), median),
           y = log1p(NumArticles))) +
  # Outliers hidden - with millions of rows they would swamp the boxes
  geom_boxplot(outlier.shape = NA, fill = "#E69F00") +
  # Same reference line as the event-type and actor figures
  geom_hline(yintercept = overall_median, linetype = 2, colour = "red") +
  # Same y-range as those figures, so all the coverage plots can be compared
  coord_flip(ylim = c(0, 4)) +
  # Set legends
  labs(title = "Coverage by world region",
       x = NULL, y = "log1p(NumArticles)") +
  theme_minimal() +
    # Add Styling
    theme(
      axis.text.y = element_text(size = 6),
      axis.text.y.right = element_text(size = 6),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 10), # Graph title font size
      axis.title = element_text(size = 10)  # X and Y axis title font sizes
    ))


# Same summary as for the actors, so the three groupings can be compared directly
region_summary <- coverage_by_region %>%
  # Group by region
  group_by(Region) %>%
  # Summarize the number of events and the media coverage
  summarise(Events = n(),
            MedianCoverage = round(median(log1p(NumArticles)), 3),
            .groups = "drop") %>%
  # Sort by Media coverage
  arrange(desc(MedianCoverage))


# Filter out the unresolved regions
real_regions <- region_summary %>% filter(Region != "(unresolved)")

# Print out the spread
cat("Spread of region medians, log1p(NumArticles): ",
    round(max(real_regions$MedianCoverage) - min(real_regions$MedianCoverage), 3),
    "\n", sep = "")

# Print out the median of unresolved locations
cat("Ungeolocated events: median ",
    region_summary$MedianCoverage[region_summary$Region == "(unresolved)"],
    " vs ", round(overall_median, 3), " overall\n", sep = "")


print(gdelt_data %>%
  # Filter sources > 10
  filter(NumSources <= 10) %>%
  # Summarize the result
  count(NumSources) %>%
  # Plot the data
  ggplot(aes(x = NumSources, y = n)) +
  geom_col(fill = "#56B4E9") +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::comma) +
  # Set legends
  labs(title = "Most events are reported by exactly one source",
       x = "NumSources", y = "Events") +
  theme_minimal() +
    # Add Styling
    theme(
      axis.text.y = element_text(size = 6),
      axis.text.y.right = element_text(size = 6),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 10), # Graph title font size
      axis.title = element_text(size = 10)  # X and Y axis title font sizes
    ))

# Print out the share 
cat(round(100 * mean(gdelt_data$NumSources == 1, na.rm = TRUE), 1),
    "% of events have exactly one distinct source\n", sep = "")

# Two of the three coverage columns are near-duplicates of each other
cat("Correlation NumArticles / NumMentions: ",
    round(cor(gdelt_data$NumArticles, gdelt_data$NumMentions,
              use = "complete.obs"), 4), "\n", sep = "")
cat("Identical in ",
    round(100 * mean(gdelt_data$NumArticles == gdelt_data$NumMentions,
                     na.rm = TRUE), 1),
    "% of rows\n", sep = "")


# Find events outside the selected window
outside_window <- sum(gdelt_data$Day < as.Date(gdelt_start_date) |
                      gdelt_data$Day > as.Date(gdelt_end_date), na.rm = TRUE)

# Print out the number
cat(format(outside_window, big.mark = ","), " rows (",
    round(100 * outside_window / nrow(gdelt_data), 2),
    "%) are back-dated outside the download window\n", sep = "")

# Get the daily number of reports on events
daily_volume <- gdelt_data %>%
  # Keep only events actually dated within the month under study
  filter(Day >= as.Date(gdelt_start_date), Day <= as.Date(gdelt_end_date)) %>%
  # Summarize
  count(Day, name = "events") %>%
  # Flag weekends - with week_start = 1 (Monday), Saturday and Sunday are 6 and 7
  mutate(Weekend = wday(Day, week_start = 1) >= 6)

# Plot data
print(ggplot(daily_volume, aes(x = Day, y = events, fill = Weekend)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("FALSE" = "#0072B2", "TRUE" = "#D55E00"),
                    labels = c("Weekday", "Weekend")) +
  # Set legends
  labs(title = "Event volume follows the working week",
       x = NULL, y = "Events", fill = NULL) +
  theme_minimal() +
    # Add Styling
    theme(
      axis.text.y = element_text(size = 6),
      axis.text.y.right = element_text(size = 6),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 10), # Graph title font size
      axis.title = element_text(size = 10)  # X and Y axis title font sizes
    ))


items_to_keep <- 50

# Lump columns together
lump_top <- function(x, n_keep) {
  # Missing values are a category in their own right, not something to discard
  x <- ifelse(is.na(x), "NONE", x)
  # Rank by columns
  keep <- names(sort(table(x), decreasing = TRUE))
  # Keep the n_keep entries
  keep <- head(keep, n_keep)
  
  factor(ifelse(x %in% keep, x, "Other"))
}

# Assemble just the columns the models need, with the response on the log scale
model_data <- gdelt_data %>%
  # Remove entries without Number of Articles and Average Tone
  filter(!is.na(NumArticles), NumArticles > 0, !is.na(AvgTone)) %>%
  # Modify data
  transmute(
    # Response
    Coverage    = log1p(NumArticles),
    EventRoot      = factor(EventRootCode),
    IsRootEvent    = IsRootEvent,
    AvgTone        = AvgTone,
    GoldsteinScale = GoldsteinScale,
    # Actors
    Actor1Type  = lump_top(Actor1Type1Code, items_to_keep),
    Actor2Type  = lump_top(Actor2Type1Code, items_to_keep),
    # Geography
    GeoCountry  = lump_top(ActionGeo_CountryCode, items_to_keep),
    GeoType     = ActionGeo_Type,
    # Timing
    Weekday     = factor(wday(Day, week_start = 1)),
    # Provenance - used by Model #2 and #3 only
    Domain      = lump_top(SourceDomain, items_to_keep),
    # Grouping key for the split - never a predictor
    Article     = SOURCEURL
  ) %>%
  filter(!is.na(GeoType), !is.na(IsRootEvent))

# Print out the number of rows in resulting data set
cat("Rows available for modelling: ", format(nrow(model_data), big.mark = ","), "\n", sep = "")


# Reproducible split
set.seed(1)

# Split on the ARTICLE, not the row
all_articles  <- unique(model_data$Article)
# Assign 20% of the data as the test articles set
test_articles <- sample(all_articles, floor(0.2 * length(all_articles)))
# Split the data into the test and training sets
test_set  <- model_data %>% filter(Article %in% test_articles)
train_set <- model_data %>% filter(!Article %in% test_articles)


# Set limit on number of rows to be fitted
max_fit_rows <- 500000
# Limit fit set
fit_set <- if (nrow(train_set) > max_fit_rows) {
  slice_sample(train_set, n = max_fit_rows)
} else train_set

# Limit the factors
model_factors <- c("EventRoot", "Actor1Type", "Actor2Type", "GeoCountry", 
                   "Weekday", "Domain")
# Remove non used columns
fit_set <- fit_set %>% mutate(across(all_of(model_factors), droplevels))

# Save row count
rows_before <- nrow(test_set)

# Adjust test set
for (v in model_factors) {
  test_set <- test_set[test_set[[v]] %in% levels(fit_set[[v]]), ]
}

# Force the test factors to carry exactly the fit_set levels, in the same order.
test_set <- test_set %>%
  mutate(across(all_of(model_factors), ~ factor(.x, levels = levels(fit_set[[cur_column()]]))))




# Print out number of rows set aside
set_aside <- rows_before - nrow(test_set)

if (set_aside == 0) {
  cat("So, we are using the entire data set.")
} else {
cat(rows_before - nrow(test_set),
    " test rows set aside for carrying a factor level absent from the fitting sample. ",
    sep = "")
}


# RMSE on the log1p scale, used for every model in this project
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))

# Baseline: ignore every predictor and guess the training mean
baseline_prediction <- mean(fit_set$Coverage)
# Calculate the baseline RMSE
rmse_baseline <- rmse(test_set$Coverage, baseline_prediction)

# Model #1 - the event's own attributes only using the lm function
model_1 <- lm(Coverage ~ EventRoot + IsRootEvent + AvgTone +
                         Actor1Type + Actor2Type + GeoCountry + GeoType + Weekday,
              data = fit_set)

# Get the RMSE for the 1st model
rmse_model_1 <- rmse(test_set$Coverage, predict(model_1, test_set))

cat("Baseline (training mean) test RMSE: ", round(rmse_baseline, 4), "\n", sep = "")
cat("Model #1 test RMSE               : ", round(rmse_model_1, 4), "\n", sep = "")
cat("Improvement over baseline        : ",
    round(100 * (1 - rmse_model_1 / rmse_baseline), 2), "%\n", sep = "")
cat("Model #1 training R-squared      : ",
    round(summary(model_1)$r.squared, 4), "\n", sep = "")
cat("Coefficients estimated           : ", model_1$rank, "\n", sep = "")

# Multicollinearity check. Because the model contains factors, car::vif() reports
# GENERALIZED variance-inflation factors: an ordinary VIF is defined per coefficient,
# but a factor spans several dummy columns and its per-dummy VIFs change with whichever
# level happens to be the baseline. A GVIF treats the whole term as one unit and is
# invariant to that arbitrary choice.
#
# The column used below, GVIF^(1/(2*Df)), is rescaled so terms with different degrees
# of freedom are comparable. The (1/Df) power puts it per-coefficient and the remaining
# square root turns variance inflation into STANDARD ERROR inflation - so this figure is
# on the sqrt(VIF) scale, not the VIF scale. The familiar cutoffs move accordingly:
# VIF 5 and 10 correspond to roughly 2.2 and 3.2 here. A value of 1 means no inflation.
vif_values <- car::vif(model_1)
cat("\nStandard-error inflation by predictor (1 = none, ~2.2 = the usual VIF 5 cutoff):\n")
for (v in rownames(vif_values)) {
  cat("  ", formatC(v, width = -22), 
      round(vif_values[v, "GVIF^(1/(2*Df))"], 3), "\n", sep = "")
}


set.seed(2)
# Limit the number of residuals to plot - full data would show up as a smudge. 
diag_sample <- sample(seq_len(nrow(fit_set)), 5000)
# Extract the sub block of data
diag_df <- data.frame(
  Fitted   = fitted(model_1)[diag_sample],
  Residual = residuals(model_1)[diag_sample]
)

# Plot the diagram
print(ggplot(diag_df, aes(x = Fitted, y = Residual)) +
  geom_point(alpha = 0.15, size = 0.6) +
  geom_hline(yintercept = 0, linetype = 2, colour = "red") +
  # Set legends
  labs(title = "Residuals vs fitted values (Model #1)",
       x = "Fitted log1p(NumArticles)", y = "Residual") +
  theme_minimal() +
    # Add Styling
    theme(
      axis.text.y = element_text(size = 6),
      axis.text.y.right = element_text(size = 6),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 10), # Graph title font size
      axis.title = element_text(size = 10)  # X and Y axis title font sizes
    ))



# Model #1 plus the publishing outlet
model_2 <- lm(Coverage ~ EventRoot + IsRootEvent + AvgTone +
                         Actor1Type + Actor2Type + GeoCountry + GeoType + Weekday +
                         Domain,
              data = fit_set)

# Calculate RMSE
rmse_model_2 <- rmse(test_set$Coverage, predict(model_2, test_set))

# Print out the RMSE and other results
cat("Model #2 test RMSE          : ", round(rmse_model_2, 4), "\n", sep = "")
cat("Model #2 training R-squared : ", round(summary(model_2)$r.squared, 4), "\n", sep = "")
cat("Improvement over Model #1   : ",
    round(100 * (1 - rmse_model_2 / rmse_model_1), 2), "%\n", sep = "")


# Use the anova to test
anova_tab <- anova(model_1, model_2)
# Print out results
cat("F-test for adding Domain    : F = ", round(anova_tab$F[2], 1),
    ", p = ", format.pval(anova_tab$`Pr(>F)`[2], digits = 3), "\n", sep = "")


# Fit the new model
model_domain_only <- lm(Coverage ~ Domain, data = fit_set)
# Print out metrics
cat("\nR-squared, Domain alone     : ",
    round(summary(model_domain_only)$r.squared, 4), "\n", sep = "")
cat("R-squared, Model #1 (8 event predictors): ",
    round(summary(model_1)$r.squared, 4), "\n", sep = "")


# Create the 3rd model
model_3 <- rpart::rpart(Coverage ~ EventRoot + IsRootEvent + AvgTone +
                                   Actor1Type + Actor2Type + GeoCountry + GeoType +
                                   Weekday + Domain,
                        data = fit_set, method = "anova",
                        control = rpart::rpart.control(cp = 1e-4, maxdepth = 12,
                                                       minbucket = 200))
# Calculate the RMSE
rmse_model_3 <- rmse(test_set$Coverage, predict(model_3, test_set))

# Printout the metrics of the model
cat("Model #3 test RMSE        : ", round(rmse_model_3, 4), "\n", sep = "")
cat("Splits in the fitted tree : ", nrow(model_3$frame[model_3$frame$var != "<leaf>", ]), "\n", sep = "")
cat("Improvement over Model #2 : ",
    round(100 * (1 - rmse_model_3 / rmse_model_2), 2), "%\n", sep = "")

# Which predictors did the tree actually find useful? Importance is rescaled to
# percent of the total so the figures are readable.
tree_importance <- sort(model_3$variable.importance, decreasing = TRUE)
cat("\nModel #3 variable importance (percent of total):\n")
for (v in names(tree_importance)) {
  cat("  ", formatC(v, width = -22),
      round(100 * tree_importance[[v]] / sum(tree_importance), 1), "%\n", sep = "")
}






# Take the model with the lowest held-out error and look at what it actually predicts
best_predictions <- if (rmse_model_3 < rmse_model_2) {
  predict(model_3, test_set)
} else {
  predict(model_2, test_set)
}

set.seed(3)
# Take a 5000 sample size of the test set
plot_rows <- sample(seq_len(nrow(test_set)), 5000)
# Generate data frame of the actual vs expected coverages
pred_df <- data.frame(
  Actual    = test_set$Coverage[plot_rows],
  Predicted = best_predictions[plot_rows]
)

# Show diagram
print(ggplot(pred_df, aes(x = Predicted, y = Actual)) +
  geom_point(alpha = 0.15, size = 0.6) +
  # A perfect model would put every point on this line
  geom_abline(linetype = 2, colour = "red") +
  coord_cartesian(xlim = c(0, 4), ylim = c(0, 4)) +
  # Set legends
  labs(title = "Predicted vs actual coverage (best model, held-out data)",
       x = "Predicted log1p(NumArticles)", y = "Actual log1p(NumArticles)") +
  theme_minimal() +
    # Add styling
    theme(
      axis.text.y = element_text(size = 6),
      axis.text.y.right = element_text(size = 6),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 10), # Graph title font size
      axis.title = element_text(size = 10)  # X and Y axis title font sizes
    ))


set.seed(4)
# Take a 30000 sample of the fit_set
nb_fit_rows <- sample(seq_len(nrow(fit_set)), min(30000, nrow(fit_set)))
nb_fit <- fit_set[nb_fit_rows, ]

# Take the actual numbers using expm1()
model_nb <- MASS::glm.nb(round(expm1(Coverage)) ~ EventRoot + IsRootEvent + AvgTone +
                           Actor1Type + Actor2Type + GeoCountry + GeoType + Weekday +
                           Domain,
                         data = nb_fit)

# Scored on the same log scale as everything else, so the numbers are comparable
nb_predictions <- log1p(predict(model_nb, test_set, type = "response"))
#Calculate the RMSE
rmse_nb <- rmse(test_set$Coverage, nb_predictions)

# Print out the metrics
cat("Negative binomial test RMSE : ", round(rmse_nb, 4), "\n", sep = "")
cat("Model #2 test RMSE          : ", round(rmse_model_2, 4), "\n", sep = "")
cat("Estimated theta             : ", round(model_nb$theta, 3),
    " (small theta = heavy overdispersion)\n", sep = "")

# Plot the diagram
print(ggplot(model_data, aes(x = AvgTone)) +
  geom_histogram(bins = 60, fill = "#0072B2") +
  # Zero is the natural reference: neutral coverage
  geom_vline(xintercept = 0, linetype = 2, colour = "red") +
  scale_y_continuous(labels = scales::comma) +
  # Set legends
  labs(title = "Media tone is mildly negative on average",
       x = "AvgTone", y = "Events") +
  theme_minimal() +
  # Add styling
    theme(
      axis.text.y = element_text(size = 6),
      axis.text.y.right = element_text(size = 6),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 10), # Graph title font size
      axis.title = element_text(size = 10)  # X and Y axis title font sizes
    ))

# Print out metrics
cat("AvgTone: mean ", round(mean(model_data$AvgTone), 3),
    ", sd ", round(sd(model_data$AvgTone), 3),
    ", range ", round(min(model_data$AvgTone), 1),
    " to ", round(max(model_data$AvgTone), 1), "\n", sep = "")

# Select average tones based on Event type
tone_by_event <- gdelt_data %>%
  filter(!is.na(AvgTone)) %>%
  mutate(Action = event_root_defs[EventRootCode]) %>%
  filter(!is.na(Action)) %>%
  group_by(Action) %>%
  summarise(MeanTone = mean(AvgTone), Events = n(), .groups = "drop")

# Plot the data
print(ggplot(tone_by_event, aes(x = reorder(Action, MeanTone), y = MeanTone)) +
  geom_col(fill = "#D55E00") +
  # Neutral tone, for reference
  geom_hline(yintercept = 0, linetype = 2, colour = "red") +
  coord_flip() +
  # Set legends
  labs(title = "Tone tracks what kind of event it was",
       x = NULL, y = "Mean AvgTone") +
  theme_minimal() +
    # Add Styling
    theme(
      axis.text.y = element_text(size = 6),
      axis.text.y.right = element_text(size = 6),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 10), # Graph title font size
      axis.title = element_text(size = 10)  # X and Y axis title font sizes
    ))


# Baseline again: ignore the predictors, guess the training mean
tone_baseline <- mean(fit_set$AvgTone)
# Calculate the RMSE
rmse_tone_baseline <- rmse(test_set$AvgTone, tone_baseline)

# Create a model of the Average Tone from the Event Root Code
tone_model_1 <- lm(AvgTone ~ EventRoot, data = fit_set)
#Calculate the RMSE
rmse_tone_1 <- rmse(test_set$AvgTone, predict(tone_model_1, test_set))

# Print out metrics
cat("Baseline (training mean) test RMSE: ", round(rmse_tone_baseline, 4), "\n", sep = "")
cat("Tone Model #1 test RMSE           : ", round(rmse_tone_1, 4), "\n", sep = "")
cat("Tone Model #1 training R-squared  : ",
    round(summary(tone_model_1)$r.squared, 4), "\n", sep = "")


# Create the model based on Event Root Type Code and involved actors
tone_model_2 <- lm(AvgTone ~ EventRoot + Actor1Type + Actor2Type, data = fit_set)
# Calculate the RMSE
rmse_tone_2 <- rmse(test_set$AvgTone, predict(tone_model_2, test_set))

# Print out the metrics
cat("Tone Model #2 test RMSE          : ", round(rmse_tone_2, 4), "\n", sep = "")
cat("Tone Model #2 training R-squared : ",
    round(summary(tone_model_2)$r.squared, 4), "\n", sep = "")
cat("Improvement over Tone Model #1   : ",
    round(100 * (1 - rmse_tone_2 / rmse_tone_1), 2), "%\n", sep = "")


# What happened, who was involved, and where
tone_model_3 <- lm(AvgTone ~ EventRoot + Actor1Type + Actor2Type + GeoCountry + GeoType,
                   data = fit_set)
# Calculate the RMSE
rmse_tone_3 <- rmse(test_set$AvgTone, predict(tone_model_3, test_set))

# Print out metrics
cat("Tone Model #3 test RMSE          : ", round(rmse_tone_3, 4), "\n", sep = "")
cat("Tone Model #3 training R-squared : ",
    round(summary(tone_model_3)$r.squared, 4), "\n", sep = "")
cat("Improvement over Tone Model #2   : ",
    round(100 * (1 - rmse_tone_3 / rmse_tone_2), 2), "%\n", sep = "")

# Same nested-model test as used for the coverage models
tone_anova <- anova(tone_model_2, tone_model_3)
# Print results
cat("F-test for adding location       : F = ", round(tone_anova$F[2], 1),
    ", p = ", format.pval(tone_anova$`Pr(>F)`[2], digits = 3), "\n", sep = "")


# Same predictors as Tone Model #3, no additivity assumption
tone_model_4 <- rpart::rpart(AvgTone ~ EventRoot + Actor1Type + Actor2Type +
                                       GeoCountry + GeoType,
                             data = fit_set, method = "anova",
                             control = rpart::rpart.control(cp = 1e-4, maxdepth = 12,
                                                            minbucket = 200))

# Calculate RMSE
rmse_tone_4 <- rmse(test_set$AvgTone, predict(tone_model_4, test_set))

# Print out metrics
cat("Tone Model #4 test RMSE          : ", round(rmse_tone_4, 4), "\n", sep = "")
cat("Improvement over Tone Model #3   : ",
    round(100 * (1 - rmse_tone_4 / rmse_tone_3), 2), "%\n", sep = "")

# Which predictors did the tree actually find useful? Importance is rescaled to
# percent of the total so the figures are readable.
tree_importance <- sort(tone_model_4$variable.importance, decreasing = TRUE)
cat("\nTone Model #4 variable importance (percent of total):\n")
for (v in names(tree_importance)) {
  cat("  ", formatC(v, width = -22),
      round(100 * tree_importance[[v]] / sum(tree_importance), 1), "%\n", sep = "")
}



# Function for lumping actors together.
relump <- function(train_values, n_keep) {
  keep <- head(names(sort(table(train_values), decreasing = TRUE)), n_keep)
  keep <- setdiff(keep, "Other")
  function(x) factor(ifelse(as.character(x) %in% keep, as.character(x), "Other"),
                     levels = c(sort(keep), "Other"))
}


# Lumping actors together
actor_relump <- relump(fit_set$Actor1Type, 15)
# Lumping countries together
geo_relump   <- relump(fit_set$GeoCountry, 15)

# Adding the new columns to the fit/test data
inter_fit  <- fit_set  %>% mutate(Actor1Small = actor_relump(Actor1Type),
                                  GeoSmall    = geo_relump(GeoCountry))
inter_test <- test_set %>% mutate(Actor1Small = actor_relump(Actor1Type),
                                  GeoSmall    = geo_relump(GeoCountry))

# Reduce the data set for easier processing (for this report only)
set.seed(6)
inter_fit <- slice_sample(inter_fit, n = min(60000, nrow(inter_fit)))

# Reduce the data set for easier processing (for this report only)
inter_test <- slice_sample(inter_test, n = min(150000, nrow(inter_test)))

# Print out the metrics
cat("Interaction models fitted on ", format(nrow(inter_fit), big.mark = ","),
    " rows, scored on ", format(nrow(inter_test), big.mark = ","), "\n", sep = "")
cat("Levels: EventRoot ", nlevels(droplevels(inter_fit$EventRoot)),
    ", Actor1Small ", nlevels(droplevels(inter_fit$Actor1Small)),
    ", GeoSmall ", nlevels(droplevels(inter_fit$GeoSmall)), "\n", sep = "")


# Additive reference, refitted on the interaction sample so the F-tests are valid
tone_additive_ref <- lm(AvgTone ~ EventRoot + Actor1Small + Actor2Type + GeoSmall + GeoType,
                        data = inter_fit)



# Let the actor effect depend on the event type
tone_model_5a <- lm(AvgTone ~ EventRoot * Actor1Small + Actor2Type + GeoSmall + GeoType,
                    data = inter_fit)

# Let the location effect depend on the event type
tone_model_5b <- lm(AvgTone ~ EventRoot * GeoSmall + Actor1Small + Actor2Type + GeoType,
                    data = inter_fit)

# Both at once
tone_model_5 <- lm(AvgTone ~ EventRoot * Actor1Small + EventRoot * GeoSmall +
                             Actor2Type + GeoType,
                   data = inter_fit)




# Test for the interactions
inter_anova <- anova(tone_additive_ref, tone_model_5)

# Print out the results
cat("F-test, additive vs both interactions: F = ", round(inter_anova$F[2], 2),
    " on ", inter_anova$Df[2], " and ", inter_anova$Res.Df[2], " df",
    ", p = ", format.pval(inter_anova$`Pr(>F)`[2], digits = 3), "\n", sep = "")

# Calculate the RMSEs for comparisons
# suppressWarnings: these interaction models are knowingly rank-deficient -
# some crossed cells are empty. The number of unestimable coefficients is
# printed below, so the warning adds nothing.
rmse_add_ref   <- rmse(inter_test$AvgTone, suppressWarnings(predict(tone_additive_ref, inter_test)))
rmse_tone_5    <- rmse(inter_test$AvgTone, suppressWarnings(predict(tone_model_5, inter_test)))

# Print out metries
cat("Extra coefficients spent          : ", tone_model_5$rank - tone_additive_ref$rank, "\n", sep = "")
cat("Held-out RMSE improvement         : ",
    round(100 * (1 - rmse_tone_5 / rmse_add_ref), 3), "%\n", sep = "")

# Print out the remaining cells
cat("Coefficients that could not be estimated (empty cells): ",
    sum(is.na(coef(tone_model_5))), "\n", sep = "")

# All four models on the same rows, so coefficient count, training fit and held-out
# error can be read side by side. This is the comparison that shows the interactions
# being statistically real and practically worthless at the same time.
inter_models <- list("Additive reference"     = tone_additive_ref,
                     "+ EventType x Actor1"   = tone_model_5a,
                     "+ EventType x Location" = tone_model_5b,
                     "+ both interactions"    = tone_model_5)

cat("\nInteraction models for tone, all fitted on the same rows:\n")
cat("  ", formatC("model", width = -24), formatC("coefs", width = 7),
    formatC("train R2", width = 11), formatC("test RMSE", width = 11), "\n", sep = "")
for (nm in names(inter_models)) {
  m <- inter_models[[nm]]
  cat("  ", formatC(nm, width = -24),
      formatC(m$rank, width = 7),
      formatC(round(summary(m)$r.squared, 4), width = 11, format = "f", digits = 4),
      formatC(round(rmse(inter_test$AvgTone, suppressWarnings(predict(m, inter_test))), 4),
              width = 11, format = "f", digits = 4), "\n", sep = "")
}


#  Filter out the top roles
top_roles <- inter_fit %>% count(Actor1Small, sort = TRUE) %>%
  filter(Actor1Small != "Other") %>% slice_head(n = 6) %>% pull(Actor1Small)

# Create plot data
inter_plot_data <- inter_fit %>%
  # Only take the actor 1 in top roles
  filter(Actor1Small %in% top_roles) %>%
  # Add action
  mutate(Action = event_root_defs[as.character(EventRoot)]) %>%
  # Remove none valid data
  filter(!is.na(Action)) %>%
  # Group by action/actor 1 and sum up average tone
  group_by(Action, Actor1Small) %>%
  summarise(MeanTone = mean(AvgTone), Events = n(), .groups = "drop") %>%
  # Remove cells with a handful of events. 
  filter(Events >= 50)

# Plot diagram
print(ggplot(inter_plot_data,
       aes(x = reorder(Action, MeanTone), y = MeanTone,
           colour = Actor1Small, group = Actor1Small)) +
  geom_line(alpha = 0.8) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, linetype = 2, colour = "red") +
  coord_flip() +
  # Set legends
  labs(title = "Tone changes based on actor role for event types",
       x = NULL, y = "Mean AvgTone", colour = NULL) +
  theme_minimal() +
    # Add styling
    theme(
      axis.text.y = element_text(size = 6),
      axis.text.y.right = element_text(size = 6),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 10), # Graph title font size
      axis.title = element_text(size = 10)  # X and Y axis title font sizes
    ))


# Calculate the Goldstein/Tone correlation
goldstein_tone_cor <- cor(model_data$GoldsteinScale, model_data$AvgTone,
                          use = "complete.obs")

# Fit the model
tone_goldstein <- lm(AvgTone ~ GoldsteinScale, data = fit_set)

# Print out metrics
cat("Correlation GoldsteinScale / AvgTone : ", round(goldstein_tone_cor, 4), "\n", sep = "")
cat("R-squared, Goldstein alone           : ",
    round(summary(tone_goldstein)$r.squared, 4), "\n", sep = "")
cat("R-squared, EventRoot alone           : ",
    round(summary(tone_model_1)$r.squared, 4), "\n", sep = "")




# Print out the metrics
cat("\nCoverage models, best improvement over baseline: ",
    round(100 * (1 - min(rmse_model_1, rmse_model_2, rmse_model_3) / rmse_baseline), 2),
    "%\n", sep = "")
cat("Tone models, best improvement over baseline    : ",
    round(100 * (1 - min(rmse_tone_1, rmse_tone_2, rmse_tone_3, rmse_tone_4) /
                 rmse_tone_baseline), 2), "%\n", sep = "")


# Select the best tone
best_tone <- if (rmse_tone_4 < rmse_tone_3) {
  predict(tone_model_4, test_set)
} else {
  predict(tone_model_3, test_set)
}

set.seed(5)
# Select a sample of the test set
tone_rows <- sample(seq_len(nrow(test_set)), 5000)
# Create data 
tone_pred_df <- data.frame(
  Actual    = test_set$AvgTone[tone_rows],
  Predicted = best_tone[tone_rows]
)

# Plot the diagram
print(ggplot(tone_pred_df, aes(x = Predicted, y = Actual)) +
  geom_point(alpha = 0.15, size = 0.6) +
  # A perfect model would put every point on this line
  geom_abline(linetype = 2, colour = "red") +
  coord_cartesian(xlim = c(-12, 6), ylim = c(-20, 12)) +
  # Set legends
  labs(title = "Predicted vs actual tone (best model, held-out data)",
       x = "Predicted AvgTone", y = "Actual AvgTone") +
  theme_minimal() +
    # Add styling
    theme(
      axis.text.y = element_text(size = 6),
      axis.text.y.right = element_text(size = 6),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 10), # Graph title font size
      axis.title = element_text(size = 10)  # X and Y axis title font sizes
    ))
