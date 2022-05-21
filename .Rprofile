# Resources
source("renv/activate.R")
library(tidyverse)
library(RSelenium)
library(jsonlite)
library(rvest)
library(feather)
library(readxl)
library(radiant.data)       # For weighted.sd()
library(DT)
library(knitr)              # For kable()
library(brms)
library(lmerTest)
library(rstan)
library(Hmisc)

# Resolve conflicts
filter <- dplyr::filter
select <- dplyr::select
summarize <- dplyr::summarize

# For fitting bayesian models
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Google API key
api_key <- Sys.getenv("GOOGLE_API_KEY")

# Cache downloaded shapefiles
options(tigris_use_cache = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### CI for weighted mean ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

weighted.ci <- function(x, weights, type, conf.level = 0.95) {
    require(Hmisc)
    n <- length(x)
    df <- n - 1
    var <- wtd.var(x, weights, normwt = FALSE, na.rm = TRUE)
    mean <- wtd.mean(x, weights, normwt = FALSE, na.rm = TRUE)
    stderr <- sqrt(var / n)
    tstat <- mean / stderr ## not mx - mu
    alpha <- 1 - conf.level
    cint <- qt(1 - alpha/2, df)
    if(type == "lower") { cint <- tstat - cint}
    else { cint <- tstat + cint }
    cint * stderr
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Custom ggplot2 theme ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Define custom ggplot theme
my_theme <- function(base_size) {
    
    # Replace elements we want to change
    theme_classic(base_size = base_size) %+replace%
        
        theme(
            panel.grid.major = element_line(color = "gray90", size = .2),
            panel.spacing = unit(2, "lines"),
            strip.background = element_blank(),
            axis.title.y = element_text(
                angle = 0, vjust = .5,
                
                # Increase axis title's right margin
                margin = margin(r = 5))
        )
}

# Set to custom theme
theme_set(my_theme(base_size = 14))

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
        
        # Apply these transformations to by-provider dataset
        if(dataset == "medicare-part-d-prescribers-by-provider") {
            current_version <- fromJSON(url) %>%
                as_tibble() %>%
                mutate(year = year,
                       dataset = dataset,
                       uuid = uuid) %>%
                mutate(
                    across(matches("^prscrbr|name$|flag$", ignore.case = TRUE),
                           as.character),
                    across(where(is.character), ~ str_squish(.)),
                    across(matches(
                        "(Clms|Suply|Benes|Cnt|Cst|Rate|Age|Risk_Scre|Fills)$"),
                        ~ as.double(str_remove_all(., "[,%$]"))),
                    Prscrbr_RUCA = as.double(Prscrbr_RUCA))
            
            # 2017 by-provider data has this additional column. Add it to other
            # versions that are missing it.
            if(!"Prscrbr_Mdcr_Enrl_Stus" %in% names(current_version)) {
                current_version <- current_version %>%
                    mutate(Prscrbr_Mdcr_Enrl_Stus = NA_character_)
            }
            
        # Apply these transformations to all other datasets
        } else {
            current_version <- fromJSON(url) %>%
                as_tibble() %>%
                mutate(year = year,
                       dataset = dataset,
                       uuid = uuid) %>%
                mutate(
                    across(matches("^prscrbr|name$|flag$", ignore.case = TRUE),
                           as.character),
                    across(where(is.character), ~ str_squish(.)),
                    across(matches(
                        "(Clms|Suply|Benes|Cnt|Cst|Rate|Age|Risk_Scre|Fills)$"),
                        ~ as.double(str_remove_all(., "[,%$]"))))
        }
        
        # Standardize names of these columns across dataset versions
        if("PRSCRBR_NPI" %in% names(current_version)) {
            current_version <- current_version %>%
                rename(Prscrbr_NPI = PRSCRBR_NPI) }
        if("Prscrbr_zip5" %in% names(current_version)) {
            current_version <- current_version %>%
                rename(Prscrbr_Zip5 = Prscrbr_zip5) }
        if("Prscrbr_Type_src" %in% names(current_version)) {
            current_version <- current_version %>%
                rename(Prscrbr_Type_Src = Prscrbr_Type_src) }
        
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
                    Prscrbr_Zip5
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

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### claims_year() ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Given an input drug class from "Generic drug bank.csv" (e.g., Antibiotics,
# Tetracycline), compute total claims per year for each prescriber using the
# Medicare Part D provider by prescriber and drug data.
claims_year <- function(data, drug_class) {
    data %>%
        filter(!!sym(drug_class) == 1) %>%
        group_by(Prscrbr_NPI, year) %>%
        summarize(
            n_drugs = sum(!is.na(Tot_Clms)),
            tot_clms = sum(Tot_Clms, na.rm = TRUE), .groups = "drop") %>%
        return()
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### claims_1k_summary() ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Given an output table from claims_year(), compute weighted mean claims per 1K
# beneficiaries for each year represented in the data.
claims_1k_summary <- function(data, outcome_var) {
    data %>%
        group_by(year) %>%
        summarize(
            n_miss = sum(is.na(!!sym(outcome_var))),
            n_prscrbr = sum(!is.na(!!sym(outcome_var))),
            mean = weighted.mean(x = !!sym(outcome_var),
                                 w = Tot_Benes_Imp,
                                 na.rm = TRUE),
            sd = weighted.sd(x = !!sym(outcome_var),
                             wt = Tot_Benes_Imp,
                             na.rm = TRUE),
            se = sd / sqrt(n_prscrbr), .groups = "drop") %>%
        return()
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### summarize_provider_data() ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Takes a dataset from Medicare Part D by-providers as input and outputs summary
# tables for lots of different measures. User can use grouping_var to specify a
# grouping variable (in addition to year), and can also specify whether summary
# means & SDs are weighted or unweighted.
summarize_provider_data <- function(providers_data,
                                    type = NULL,
                                    grouping_var = NULL) {
    
    # Add grouping
    if(is.null(grouping_var)) {
        providers_data <- providers_data %>%
            group_by(year, class)
    } else {
        providers_data <- providers_data %>%
            group_by(year, class, !!sym(grouping_var))
    }
    
    # Compute weighted means
    if(type == "weighted") {
        providers_data %>%
            filter(!is.na(Tot_Benes_Imp)) %>%
            summarize(n_providers = sum(!is.na(Prscrbr_NPI)),
                      mean_ben_cnt = mean(
                          Tot_Benes_Imp,
                          na.rm = TRUE),
                      sd_ben_cnt = sd(
                          Tot_Benes_Imp,
                          na.rm = TRUE),
                      min_ben_cnt = min(
                          Tot_Benes_Imp,
                          na.rm = TRUE),
                      max_ben_cnt = max(
                          Tot_Benes_Imp,
                          na.rm = TRUE),
                      mean_ben_age = weighted.mean(
                          x = Bene_Avg_Age,
                          w = Tot_Benes_Imp,
                          na.rm = TRUE),
                      sd_ben_age = weighted.sd(
                          x = Bene_Avg_Age,
                          wt = Tot_Benes_Imp,
                          na.rm = TRUE),
                      mean_percent_fem = weighted.mean(
                          x = percent_fem,
                          w = Tot_Benes_Imp, na.rm = TRUE),
                      mean_hcc = weighted.mean(
                          x = Bene_Avg_Risk_Scre,
                          w = Tot_Benes_Imp,
                          na.rm = TRUE),
                      sd_hcc = weighted.sd(
                          x = Bene_Avg_Risk_Scre,
                          wt = Tot_Benes_Imp,
                          na.rm = TRUE),
                      min_hcc = min(
                          Bene_Avg_Risk_Scre,
                          na.rm = TRUE),
                      max_hcc = max(
                          Bene_Avg_Risk_Scre,
                          na.rm = TRUE),
                      mean_clms = weighted.mean(
                          x = tot_clms,
                          w = Tot_Benes_Imp,
                          na.rm = TRUE),
                      sd_clms = weighted.sd(
                          x = tot_clms,
                          wt = Tot_Benes_Imp,
                          na.rm = TRUE),
                      mean_clms_1k = weighted.mean(
                          x = claims_1k_imp,
                          w = Tot_Benes_Imp,
                          na.rm = TRUE),
                      sd_clms_1k = weighted.sd(
                          x = claims_1k_imp,
                          wt = Tot_Benes_Imp,
                          na.rm = TRUE),
                      min_clms_1k = min(
                          claims_1k_imp,
                          na.rm = TRUE),
                      max_clms_1k = max(
                          claims_1k_imp,
                          na.rm = TRUE),
                      .groups = "drop") %>%
            mutate(type = "weighted") %>%
            return()
        
    # Compute unweighted means
    } else if(type == "unweighted") {
        providers_data %>%
            summarize(n_providers = sum(!is.na(Prscrbr_NPI)),
                      mean_ben_cnt = mean(
                          Tot_Benes_Imp,
                          na.rm = TRUE),
                      sd_ben_cnt = sd(
                          Tot_Benes_Imp,
                          na.rm = TRUE),
                      min_ben_cnt = min(
                          Tot_Benes_Imp,
                          na.rm = TRUE),
                      max_ben_cnt = max(
                          Tot_Benes_Imp,
                          na.rm = TRUE),
                      mean_ben_age = mean(
                          x = Bene_Avg_Age,
                          na.rm = TRUE),
                      sd_ben_age = sd(
                          x = Bene_Avg_Age,
                          na.rm = TRUE),
                      mean_percent_fem = mean(
                          x = percent_fem,
                          na.rm = TRUE),
                      mean_hcc = mean(
                          x = Bene_Avg_Risk_Scre,
                          na.rm = TRUE),
                      sd_hcc = sd(
                          x = Bene_Avg_Risk_Scre,
                          na.rm = TRUE),
                      min_hcc = min(
                          Bene_Avg_Risk_Scre,
                          na.rm = TRUE),
                      max_hcc = max(
                          Bene_Avg_Risk_Scre,
                          na.rm = TRUE),
                      mean_clms = mean(
                          x = tot_clms,
                          na.rm = TRUE),
                      sd_clms = sd(
                          x = tot_clms,
                          na.rm = TRUE),
                      mean_clms_1k = mean(
                          x = claims_1k_imp,
                          na.rm = TRUE),
                      sd_clms_1k = sd(
                          x = claims_1k_imp,
                          na.rm = TRUE),
                      min_clms_1k = min(
                          claims_1k_imp,
                          na.rm = TRUE),
                      max_clms_1k = max(
                          claims_1k_imp,
                          na.rm = TRUE),
                      .groups = "drop") %>%
            mutate(type = "unweighted") %>%
            return()
    } else {
        message("Something went wrong on type logic.")
    }
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### compute_summary_table() ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

compute_summary_table <- function(data, group) {
    sum <- data %>%
        group_by(!!sym(group), class) %>%
        dplyr::summarize(
            n_prscrbr = sum(!is.na(Prscrbr_NPI)),
            n_rows_na_claims = sum(is.na(tot_clms)),
            n_claims = sum(tot_clms, na.rm = TRUE),
            n_rows_na_ben = sum(is.na(Tot_Benes)),
            n_ben = sum(Tot_Benes, na.rm = TRUE),
            n_ben_imp = sum(Tot_Benes_Imp, na.rm = TRUE),
            mean_ben = mean(Tot_Benes, na.rm = TRUE),
            sd_ben = sd(Tot_Benes, na.rm = TRUE),
            min_ben = min(Tot_Benes, na.rm = TRUE),
            max_ben = max(Tot_Benes, na.rm = TRUE),
            n_rows_na_age = sum(is.na(Bene_Avg_Age)),
            mean_age_wt = wtd.mean(Bene_Avg_Age, Tot_Benes,
                                   normwt = FALSE,
                                   na.rm = TRUE),
            sd_age_wt = weighted.sd(Bene_Avg_Age, Tot_Benes,
                                    na.rm = TRUE),
            n_rows_na_hcc = sum(is.na(Bene_Avg_Risk_Scre)),
            mean_hcc_wt = wtd.mean(Bene_Avg_Risk_Scre, Tot_Benes,
                                   normwt = FALSE,
                                   na.rm = TRUE),
            sd_hcc_wt = weighted.sd(Bene_Avg_Risk_Scre, Tot_Benes,
                                    na.rm = TRUE),
            min_hcc = min(Bene_Avg_Risk_Scre, na.rm = TRUE),
            max_hcc = max(Bene_Avg_Risk_Scre, na.rm = TRUE),
            n_rows_na_fem = sum(is.na(percent_fem)),
            mean_fem_wt = wtd.mean(percent_fem, Tot_Benes,
                                   normwt = FALSE,
                                   na.rm = TRUE),
            mean_claims_wt = wtd.mean(tot_clms, Tot_Benes,
                                      normwt = FALSE,
                                      na.rm = TRUE),
            sd_claims_wt = weighted.sd(tot_clms, Tot_Benes, na.rm = TRUE),
            n_rows_na_claims_1k = sum(is.na(claims_1k)),
            mean_claims_1k_wt = wtd.mean(claims_1k, Tot_Benes,
                                         normwt = FALSE,
                                         na.rm = TRUE),
            sd_claims_1k_wt = weighted.sd(claims_1k, Tot_Benes,
                                      na.rm = TRUE),
            ci_claims_1k_lower = weighted.ci(claims_1k, Tot_Benes, "lower",
                                          conf.level = 0.95),
            ci_claims_1k_upper = weighted.ci(claims_1k, Tot_Benes, "upper",
                                             conf.level = 0.95),
            lower_ci_claims_1k_wt = wtd.quantile(claims_1k, Tot_Benes,
                                       probs = c(0.025),
                                       normwt = FALSE,
                                       na.rm = TRUE),
            upper_ci_claims_1k_wt = wtd.quantile(claims_1k, Tot_Benes,
                                       probs = c(0.975),
                                       normwt = FALSE,
                                       na.rm = TRUE),
            min_claims_1k = min(claims_1k, na.rm = TRUE),
            max_claims_1k = max(claims_1k, na.rm = TRUE),
            mean_age_unwt = mean(Bene_Avg_Age, na.rm = TRUE),
            mean_hcc_unwt = mean(Bene_Avg_Risk_Scre, na.rm = TRUE),
            mean_fem_unwt = mean(percent_fem, na.rm = TRUE),
            mean_claims_unwt = mean(tot_clms, na.rm = TRUE),
            mean_ben_unwt = mean(Tot_Benes, na.rm = TRUE),
            mean_claims_1k_unwt = mean(claims_1k, na.rm = TRUE),
            .groups = "drop") %>%
        mutate(se_claims_1k_wt = sd_claims_1k_wt / sqrt(n_prscrbr)) %>%
        group_by(!!sym(group))
    return(sum)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### format_ben_table()  ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

format_ben_table <- function(data) {
    
    # Extract grouping
    group <- data %>% group_keys() %>% names()
    
    if(group == "Year") { data <- data %>% arrange(desc(Year)) }
    if(group == "Provider Type") { data <- data %>% arrange(desc(mean_ben)) }
    
    # Format beneficiary table
    data %>%
        mutate(
            mean_ben = paste0(
                format(
                    mean_ben,
                    nsmall = 2,
                    digits = 2,
                    big.mark = ","
                ),
                " (",
                format(
                    sd_ben,
                    nsmall = 2,
                    digits = 2,
                    big.mark = ","
                ),
                ")"
            ),
            ben_range = paste0(
                format(
                    min_ben,
                    nsmall = 0,
                    digits = 2,
                    big.mark = ","
                ),
                "-",
                format(
                    max_ben,
                    nsmall = 0,
                    digits = 2,
                    big.mark = ","
                )
            ),
            mean_age_wt = paste0(
                format(
                    mean_age_wt,
                    nsmall = 2,
                    digits = 2,
                    big.mark = ","
                ),
                " (",
                format(
                    sd_age_wt,
                    nsmall = 2,
                    digits = 2,
                    big.mark = ","
                ),
                ")"
            ),
            mean_fem_wt = format(mean_fem_wt, nsmall = 1, digits = 2),
            mean_hcc_wt = paste0(
                format(
                    mean_hcc_wt,
                    nsmall = 2,
                    digits = 2,
                    big.mark = ","
                ),
                " (",
                format(
                    sd_hcc_wt,
                    nsmall = 2,
                    digits = 2,
                    big.mark = ","
                ),
                ")"
            ),
            hcc_range = paste0(
                format(
                    min_hcc,
                    nsmall = 2,
                    digits = 2,
                    big.mark = ","
                ),
                "-",
                format(
                    max_hcc,
                    nsmall = 2,
                    digits = 2,
                    big.mark = ","
                )
            )
        ) %>%
        select(!!sym(group),
               class,
               mean_ben,
               ben_range,
               mean_age_wt,
               mean_fem_wt,
               mean_hcc_wt,
               hcc_range) %>%
        datatable(
            options = list(dom = "t"),
            rownames = FALSE,
            colnames = c(
                group,
                "Class",
                "Mean (SD) Beneficiary Count",
                "Range Beneficiary Count",
                "Mean (SD) Beneficiary Age",
                "Sex (% Female)",
                "Mean (SD) Beneficiary HCC Score",
                "Range Beneficiary HCC Score"
            )
        ) %>%
        return()
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### format_use_table()  ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

format_use_table <- function(data) {
    
    # Extract grouping
    group <- data %>% group_keys() %>% names()
    
    if(group == "Year") { data <- data %>% arrange(desc(Year)) }
    if(group == "Provider Type") { data <- data %>%
        arrange(desc(mean_claims_1k_wt)) }
    
    # Format beneficiary table
    data %>%
        mutate(
            mean_claims = paste0(
                format(
                    mean_claims_wt,
                    nsmall = 2,
                    digits = 2,
                    big.mark = ","
                ),
                " (",
                format(
                    sd_claims_wt,
                    nsmall = 2,
                    digits = 2,
                    big.mark = ","
                ),
                ")"
            ),
            mean_ben = paste0(
                format(
                    mean_ben,
                    nsmall = 2,
                    digits = 2,
                    big.mark = ","
                ),
                " (",
                format(
                    sd_ben,
                    nsmall = 2,
                    digits = 2,
                    big.mark = ","
                ),
                ")"
            ),
            mean_claims_1k = paste0(
                format(
                    mean_claims_1k_wt,
                    nsmall = 2,
                    digits = 2,
                    big.mark = ","
                ),
                " (",
                format(
                    sd_claims_1k_wt,
                    nsmall = 2,
                    digits = 2,
                    big.mark = ","
                ),
                ")"
            ),
            claims_1k_ci = paste0(
                format(
                    lower_ci_claims_1k_wt,
                    nsmall = 0,
                    digits = 2,
                    big.mark = ","
                ),
                "-",
                format(
                    upper_ci_claims_1k_wt,
                    nsmall = 0,
                    digits = 2,
                    big.mark = ","
                )
            ),
            claims_1k_range = paste0(
                format(
                    min_claims_1k,
                    nsmall = 0,
                    digits = 2,
                    big.mark = ","
                ),
                "-",
                format(
                    max_claims_1k,
                    nsmall = 0,
                    digits = 2,
                    big.mark = ","
                )
            )) %>%
        select(!!sym(group),
               class,
               mean_claims,
               mean_ben,
               mean_claims_1k,
               claims_1k_ci,
               claims_1k_range) %>%
        datatable(
            options = list(dom = "t"),
            rownames = FALSE,
            colnames = c(
                group,
                "Class",
                "Mean (SD) Claims",
                "Mean (SD) Beneficiary Count",
                "Mean (SD) Claims / 1K Beneficiaries",
                "95% CI, Mean Claims / 1K Beneficiaries",
                "Range Claims / 1K Beneficiaries"
            )
        ) %>%
        return()
}
