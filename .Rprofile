# Resources
source("renv/activate.R")
library(tidyverse)
library(RSelenium)
library(jsonlite)
library(rvest)
library(feather)

# Resolve conflict for filter
filter <- dplyr::filter

# Google API key
api_key <- Sys.getenv("GOOGLE_API_KEY")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### get_dataset_versions() ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Given a dataset string like "medicare-part-d-prescribers-by-provider", returns
# a table showing all of the years/versions of the dataset and their IDs.
get_dataset_versions <- function(dataset) {
    
    # Create URL to the API docs page for the input dataset
    url <- paste0("https://data.cms.gov/provider-summary-by-type-of-service/medicare-part-d-prescribers/", dataset, "/api-docs")
    
    # Start Selenium server by opening Docker & starting standalone-firefox
    # image.
    message(paste0("Starting Selenium server... "), appendLF = FALSE)
    system("open --background -a Docker", wait = TRUE)
    system("docker run -d -p 4445:4444 selenium/standalone-firefox",
           wait = TRUE)
    message("done.")
    
    # Open connection to server
    message(paste0("Opening connection to server... "), appendLF = FALSE)
    remDr <- remoteDriver(port = 4445L)
    Sys.sleep(5)
    remDr$open()
    message("done.")
    
    # Navigate to URL and wait for page to load
    message(paste0("Opening target URL... "), appendLF = FALSE)
    remDr$navigate(url)
    Sys.sleep(5)
    message("done.")
    
    # Scrape table of dataset versions
    message(paste0("Scraping data... "), appendLF = FALSE)
    versions <- remDr$getPageSource() %>%
        .[[1]] %>%
        read_html() %>%
        html_elements(".container") %>%
        html_table() %>%
        .[[4]] %>%
        mutate(dataset = dataset)
    message("done.")
    
    # Close connection to server, stop server
    message(paste0("Closing connection to server & stopping server... "),
            appendLF = FALSE)
    remDr$close()
    system("docker stop $(docker ps -q)")
    message("done.")
    
    # Return versions table
    return(versions)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### get_data() ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function that takes a version table as input and outputs a list where each
# list item is the data for a year/version.
get_data <- function(version_table) {
    
    # String representing the dataset
    dataset <- version_table %>%
        pull(dataset) %>%
        unique()
    
    # Vector of UUIDs
    uuids <- version_table$UUID
    
    # Iterate over UUIDs
    all_versions <- map_dfr(uuids, function(uuid) {
        
        # Get year for the current UUID
        year <- version_table %>%
            filter(UUID == eval(uuid)) %>%
            pull(Version)
        
        # User message
        message(paste0("Retrieving ", year, " ", dataset, "... "),
                appendLF = FALSE)
        
        # Create URL. the "data.json" portion means that all data will be
        # returned as a single JSON (no pagination). Filters are start with
        # "?filter". Here I'm filtering to only Idaho data.
        url <- paste0("https://data.cms.gov/data-api/v1/dataset/",
                      uuid,
                      "/data.json?filter[Prscrbr_State_Abrvtn]=ID")
        
        # Retrieve data, set cols for year, dataset, uuid, make all cols
        # character to facilitate rbinding.
        current_version <- fromJSON(url) %>%
            as_tibble() %>%
            mutate(year = year,
                   dataset = dataset,
                   uuid = uuid) %>%
            mutate(across(everything(), as.character))
        
        # 2017 by-provider data has this additional column. Add it to other
        # versions that are missing it.
        if(dataset == "medicare-part-d-prescribers-by-provider" &
           !"Prscrbr_Mdcr_Enrl_Stus" %in% names(current_version)) {
            current_version <- current_version %>%
                mutate(Prscrbr_Mdcr_Enrl_Stus = NA_character_)
        }
        message("done.")
        current_version
    })
    return(all_versions)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### get_address_names() ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function that uses the Google Places API to map from addresses to place names.
# Takes a dataset with these address fields as input: Prscrbr_St1, Prscrbr_St2,
# Prscrbr_City, Prscrbr_State_Abrvtn. Outputs a table relating each unique
# address in the input to a name returned from Google.
get_address_names <- function(input_table) {
    
    # Organize input table
    input_table2 <- input_table %>%
        
        # Paste address elements together
        mutate(
            dataset_address = str_squish(
                paste(
                    Prscrbr_St1,
                    Prscrbr_St2,
                    Prscrbr_City,
                    Prscrbr_State_Abrvtn,
                    Prscrbr_zip5
                )
            ),
            
            # API fails on "#", so replace with "Number"
            query_string = str_squish(str_replace_all(dataset_address, "#", "
                                                      Number ")),
            
            # Add "health" to query strings; replace spaces with %20
            query_string = paste(query_string, "health"),
            query_string = str_replace_all(query_string, " ", "%20")
        )
    
    
    # Table relating unique addresses in Idaho 2019 to query strings
    unique_addr <- input_table2 %>%
        count(query_string) %>%
        left_join(input_table2 %>%
                      select(dataset_address, query_string) %>%
                      unique(),
                  by = "query_string") %>%
        select(dataset_address, query_string)
    
    # Read in address-name bank
    bank <- read_feather("Address-name bank.feather")
    
    # Drop rows where dataset_address has already been run through the Places
    # API.
    unique_addr <- unique_addr %>%
        filter(!dataset_address %in% bank$dataset_address)
    
    # Check to see if there are any unmapped addresses in the input
    if (nrow(unique_addr) == 0) {
        message("All addresses have already been queried.")
        
    # Map any unmapped addresses
    } else {
        place_names <- map_dfr(1:nrow(unique_addr), function(row) {
            
            # User message
            message(paste0(
                "Querying ", unique_addr[row, ]$dataset_address, "... "),
                    appendLF = FALSE)
            
            # Create URL
            api_url <-
                paste0(
                    "https://maps.googleapis.com/maps/api/place/findplacefromtext/json?fields=name%2Cformatted_address%2Cplace_id&input=",
                    unique_addr[row, ]$query_string,
                    "&inputtype=textquery&key=",
                    api_key
                )
            
            # Get query results
            results <- fromJSON(api_url)
            
            # Check to see that there's a top candidate in results
            if (results$status == "ZERO_RESULTS") {
                top_candidate <- tibble(
                    formatted_address = NA_character_,
                    name = NA_character_,
                    place_id = NA_character_
                )
            } else {
                top_candidate <- results$candidates[1, ]
            }
            
            # Bind cols representing the top query candidate & status to the
            # current row.
            current_row <- bind_cols(unique_addr[row, ], top_candidate,
                                     status = results$status)
            message("done.")
            
            # Return this
            current_row
            
        }) %>%
            select(name,
                   dataset_address,
                   formatted_address,
                   query_string,
                   place_id,
                   status)
        
        # User message
        message("Updating and saving the address-name bank...",
                appendLF = FALSE)
        bank <- bind_rows(bank, place_names)
        write_feather(bank, "Address-name bank.feather")
        message("done.")
        }
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### update_zero_results() ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function that updates the address-name bank using a table of addresses where
# Google returned ZERO_RESULTS, but some addresses were able to be manually
# identified. Takes a table of input, adds manually updated address-name
# pairings to the bank, then saves the updated bank.
update_zero_results <- function(zero_table) {
    
    # Get bank
    bank <- read_feather("Address-name bank.feather")
    
    # Get resolved names from zero_table.
    resolved <- zero_table %>%
        filter(!is.na(address_name)) %>%
        select(dataset_address, name = address_name)
    
    if(sum(resolved$name %in% bank$name) != nrow(resolved)) {
        
        # User message on number of names to update
        n_names <- nrow(resolved) - sum(resolved$name %in% bank$name)
        message(paste(n_names, "names need to be updated."))
        
        # Loop over resolved addresses and add their names to bank
        map(resolved$dataset_address, function(target_address) {
            message(paste0("Updating name for ", target_address, "..."),
                    appendLF = FALSE)
            
            # Read in the file every loop
            bank <- read_feather("Address-name bank.feather")
            
            # Make update
            bank[bank$dataset_address == target_address,]$name <- resolved[resolved$dataset_address == target_address,]$name
            
            # Write update to feather
            write_feather(bank, "Address-name bank.feather")
            message("done.")
        })
        
        
    } else { message("All names are already up to date.") }
}
