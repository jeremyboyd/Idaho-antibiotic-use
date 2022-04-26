# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Use API to get Medicare Part D prescriber data for antibiotic use
# study.

# TODO:
# Automatically check to see if new data versions exist. If so, download them.
# How do I add county? Is there a standardized way to go from zip code to county? Think I could get more location info from a Google place search, including county.

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Read & organize Idaho 2019 providers-by-prescriber ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Read in data
p <- read_feather("Idaho 2019 providers by prescriber data.feather")

# Make sure all addresses have been queried in Google Places API
get_address_names(input_table = p)

# Make sure bank is up-to-date with names from the zero results table
zero <- read_csv("Zero address-name results from Idaho 2019 by-prescribers.csv")
update_zero_results(zero_table = zero)

# Create table with prescriber names, credentials, type, address, and address
# name. This is for use in manually coding different types of prescribers.
p2 <- p %>%
    mutate(
        dataset_address = str_squish(
            paste(
                Prscrbr_St1,
                Prscrbr_St2,
                Prscrbr_City,
                Prscrbr_State_Abrvtn,
                Prscrbr_zip5
            )
        )) %>%
    left_join(bank %>%
                   select(dataset_address, name, status),
              by = "dataset_address") %>%
    select(PRSCRBR_NPI:Prscrbr_Crdntls, Prscrbr_Type, dataset_address,
           address_name = name, status)
write_feather(p2, "Idaho 2019 providers by prescriber data with names.feather")

# Read in bank for copying
bank <- read_feather("Address-name bank.feather")

# CSV versions of the bank and Idaho 2019 with names saved to Box
box_dir <- "~/Library/CloudStorage/Box-Box/IDHW_2022_Idaho_Antibiotic_project/"
write_csv(bank, paste0(box_dir, "Address-name bank.csv"))
write_csv(p2, paste0(
    box_dir,
    "Idaho 2019 providers by prescriber data with address names.csv"))

# Zero table to Box
write_csv(zero, paste0(box_dir, "Zero address-name results from Idaho 2019 by-prescribers.csv"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Antibiotic claims / 1K beneficiaries from by-provider data ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Tot_Benes: total number of unique Medicare Part D beneficiaries with at least
# one drug claim. This means we're probably undercounting the number of
# beneficiaries because there will be some with no drug claims.
# Antbtc_Tot_Clms: total claims of antibiotic drugs, including refills. Values <
# 11 are suppressed.

# Read in data
p <- read_feather("Idaho prescribers by provider data.feather")

# Compute & summarize antibiotic claims per 1K beneficiaries
p_claims_1k_sum <- p %>%
    
    # Compute claims per 1k beneficiaries. Will result in NA for rows that are
    # missing either or both of Antbtc_Tot_Clms and Tot_Benes.
    mutate(claims_1k = Antbtc_Tot_Clms / (Tot_Benes / 1000)) %>%
    
    # Summarize by year
    group_by(year) %>%
    summarize(
        
        # Rows missing claims_1k
        n_miss = sum(is.na(claims_1k)),
        
        # Rows with claims_1k
        n_prscrbr = sum(!is.na(claims_1k)),
        
        # Claims per 1K mean, sd, se
        mean = mean(claims_1k, na.rm = TRUE),
        sd = sd(claims_1k, na.rm = TRUE),
        se = sd / sqrt(n_prscrbr), .groups = "drop") %>%
    mutate(class = "Antibiotic (numerator from prescriber table)")

# Figure
p_claims_1k_sum %>%
    mutate(group = "group") %>%
    ggplot(mapping = aes(x = year, y = mean, group = group)) +
    geom_line() +
    geom_errorbar(aes(ymin = mean - se,
                      ymax = mean + se),
                  width = 0.2, color = "gray50") +
    scale_y_continuous(breaks = seq(420, 500, 20),
                       limits = c(420, 510)) +
    labs(x = "Year",
         y = "Antibiotic claims\nper 1K\nbeneficiaries")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Claims / 1K beneficiaries from by provider & drug data ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Tot_Clms: The number of Medicare Part D claims. This includes original
# prescriptions and refills.
# Tot_Benes: The total number of unique Medicare Part D beneficiaries with at
# least one claim for the drug.

# Read in by provider & drug dataset
pd <- read_feather("Idaho prescribers by provider & drug data.feather")

# Table of generic drugs with coding for antibiotic and antibiotic type
generics <- read_csv(paste0(box_dir, "Generic drug bank.csv")) %>%
    
    # Only keep rows that have been coded
    filter(!is.na(Antibiotic)) %>%
    
    # Get rid of any extra whitespace in character cols. This can cause joins to
    # fail later.
    mutate(across(where(is.character), ~ str_squish(.)))

# Both of the following tables are needed to run claims_year() and
# claims_1k_summary(). pd2 is the source for numerators in the claims per 1K
# beneficiary computation; p_benes is the source for denominators.
pd2 <- pd %>%
    left_join(generics, by = "Gnrc_Name") %>%
    select(Prscrbr_NPI, year, Gnrc_Name, Tot_Clms, `Anti-Infective`:Other)
p_benes <- p %>%
    select(Prscrbr_NPI, year, Tot_Benes)

# Drug classes that we want to compute claims per 1K beneficiaries for
drug_classes <- names(generics)[4:length(names(generics))]

# Loop over drug classes
pd_claims_1k_sum <- map_dfr(drug_classes, function(class) {
    message(paste0("Computing claims per 1K beneficiaries for class ", class,
                   "..."))
    claims_year(drug_class = class) %>%
        claims_1k_summary() %>%
        mutate(class = class)
}) %>%
    mutate(class = if_else(
        class == "Antibiotic",
        "Antibiotic (numerator from prescriber & drug table)",
        class))

# Figure
# This figure could be improved by jittering datapoints & errorbars, and
# replacing the legend with labels for each line.
pd_claims_1k_sum %>%
    ggplot(mapping = aes(x = year,
                         y = mean,
                         group = class,
                         color = class)) +
    geom_line() +
    geom_errorbar(aes(ymin = mean - se,
                      ymax = mean + se),
                  width = 0.2, size = 0.4, color = "gray50") +
    scale_x_continuous(breaks = seq(min(claims_1k_sum$year),
                                  max(claims_1k_sum$year), 1)) +
    labs(x = "Year",
         y = "Claims per\n1K beneficiaries",
         color = "Class")

# Faceted claims per 1K beneficiaries, all classes
bind_rows(pd_claims_1k_sum, p_claims_1k_sum) %>%
    mutate(class = str_wrap(class, 30),
           class = fct_relevel(class, "Other", after = Inf)) %>%
    ggplot(mapping = aes(x = year,
                         y = mean,
                         group = class,
                         color = class)) +
    geom_errorbar(aes(ymin = mean - se,
                      ymax = mean + se),
                  width = 0.2, size = 0.4, color = "gray50") +
    geom_line() +
    scale_x_continuous(breaks = seq(min(claims_1k_sum$year),
                                    max(claims_1k_sum$year), 2)) +
    scale_y_continuous(breaks = seq(0, 1000, 100)) +
    facet_wrap(~ class, ncol = 5) +
    labs(x = "Year",
         y = "Claims per\n1K beneficiaries",
         color = "Class") +
    theme(legend.position = "none")
ggsave(paste0(box_dir, "figures/Claims per 1K beneficiaries, all classes.png"))

# Faceted claims per 1K beneficiaries, antibiotics
bind_rows(pd_claims_1k_sum, p_claims_1k_sum) %>%
    mutate(class = str_wrap(class, 30)) %>%
    filter(str_detect(class, "Antibiotic")) %>%
    ggplot(mapping = aes(x = year,
                         y = mean,
                         group = class,
                         color = class)) +
    geom_errorbar(aes(ymin = mean - se,
                      ymax = mean + se),
                  width = 0.2, size = 0.4, color = "gray50") +
    geom_line() +
    scale_x_continuous(breaks = seq(min(claims_1k_sum$year),
                                    max(claims_1k_sum$year), 1)) +
    facet_wrap(~ class) +
    labs(x = "Year",
         y = "Claims per\n1K beneficiaries",
         color = "Class") +
    theme(legend.position = "none")
ggsave(paste0(box_dir, "figures/Claims per 1K beneficiaries, antibiotics.png"))













# join <- pd %>%
#     left_join(generics, by = "Gnrc_Name") %>%
#     filter(!is.na(Antibiotic))
# no_join <- pd %>%
#     left_join(generics, by = "Gnrc_Name") %>%
#     filter(is.na(Antibiotic))
# 
# join %>%
#     count(Gnrc_Name) %>%
#     arrange(desc(n)) %>%
#     pull(Gnrc_Name)
# 
# no_join %>%
#     count(Gnrc_Name) %>%
#     arrange(desc(n)) %>%
#     pull(Gnrc_Name)

# # Add uncategorized generics to "Generic drug bank.csv"
# # Empty generics table
# empty_gen <- generics[0,]
# 
# # These are joins that are failing. Looks like the drugs aren't listed in the
# # generics table. Probably because they were only used in years < 2019.
# failed_joins <- pd2 %>%
#     filter(is.na(Antibiotic)) %>%
#     count(Gnrc_Name) %>%
#     arrange(desc(n))
# 
# # Put name & n of generics that failed to join in the right format
# failed_joins2 <- map_dfr(1:nrow(failed_joins), function(row) {
#     current_row <- empty_gen[row,]
#     current_row$Gnrc_Name <- failed_joins[row,]$Gnrc_Name
#     current_row$n <- failed_joins[row,]$n
#     current_row
# })
# 
# # Join drugs that failed to join to generics table and write to bank
# generics2 <- bind_rows(generics, failed_joins2)
# write_csv(generics2, paste0(box_dir, "Generic drug bank.csv"))


# Add up total antibiotic claims by prescriber & year
clm_ant_yr <- pd2 %>%
    filter(Antibiotic == 1) %>%
    group_by(Prscrbr_NPI, year) %>%
    summarize(
        n_ant = sum(!is.na(Tot_Clms)),
        tot_ant_clms = sum(Tot_Clms, an.rm = TRUE), .groups = "drop")

# Is it feasible for a provider to have 866 antibiotic claims in a single year?
summary(clm_ant_yr)
clm_ant_yr %>% filter(tot_ant_clms == 866)

# Join Tot_Benes from p_benes & compute claims / 1K benes
clm_ant_yr_sum <- clm_ant_yr %>%
    left_join(p_benes, by = c("Prscrbr_NPI", "year")) %>%
    mutate(claims_1k = tot_ant_clms / (Tot_Benes / 1000)) %>%

    # Summarize by year
    group_by(year) %>%
    summarize(
        
        # Rows missing claims_1k
        n_miss = sum(is.na(claims_1k)),
        
        # Rows with claims_1k
        n_prscrbr = sum(!is.na(claims_1k)),
        
        # Claims per 1K mean, sd, se
        mean_claims_1k = mean(claims_1k, na.rm = TRUE),
        sd_claims_1k = sd(claims_1k, na.rm = TRUE),
        se_claims_1k = sd_claims_1k / sqrt(n_prscrbr), .groups = "drop")

# Figure
# NOTE that this data pattern is quite a bit different from when we rely on the
# Antbtc_Tot_Clms col from the by-providers data. Result could change when I get
# an updated generics table from Karl.
clm_ant_yr_sum %>%
    mutate(group = "group") %>%
    ggplot(mapping = aes(x = year, y = mean_claims_1k, group = group)) +
    geom_line() +
    geom_errorbar(aes(ymin = mean_claims_1k - se_claims_1k,
                      ymax = mean_claims_1k + se_claims_1k),
                  width = 0.2, size = 0.4, color = "gray50") +
    scale_x_continuous(breaks = seq(min(clm_ant_yr_sum$year),
                                    max(clm_ant_yr_sum$year), 1)) +
    scale_y_continuous(breaks = seq(420, 500, 20),
                       limits = c(420, 510)) +
    labs(x = "Year",
         y = "Antibiotic claims\nper 1K\nbeneficiaries")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Turn the above into a function that takes a column in pd2 as input (e.g., Antibiotic, Tetracycline) and computes claims / 1K beneficiaries for it. ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Compute total claims per year for each prescriber for the input drug class
claims_year <- function(drug_class) {
    pd2 %>%
        filter(!!sym(drug_class) == 1) %>%
        group_by(Prscrbr_NPI, year) %>%
        summarize(
            n_drugs = sum(!is.na(Tot_Clms)),
            tot_clms = sum(Tot_Clms, an.rm = TRUE), .groups = "drop") %>%
        return()
}

# Function that takes the table output from claims_year() and computes claims
# per 1K beneficiaries for each year in the data.
claims_1k_summary <- function(table) {
    table %>%
        left_join(p_benes, by = c("Prscrbr_NPI", "year")) %>%
        mutate(claims_1k = tot_clms / (Tot_Benes / 1000)) %>%
        group_by(year) %>%
        summarize(
            n_miss = sum(is.na(claims_1k)),
            n_prscrbr = sum(!is.na(claims_1k)),
            mean_claims_1k = mean(claims_1k, na.rm = TRUE),
            sd_claims_1k = sd(claims_1k, na.rm = TRUE),
            se_claims_1k = sd_claims_1k / sqrt(n_prscrbr), .groups = "drop") %>%
        return()
}

claims_1k_summary(table = x) %>%
    mutate(group = "group") %>%
    ggplot(mapping = aes(x = year, y = mean_claims_1k, group = group)) +
    geom_line() +
    geom_errorbar(aes(ymin = mean_claims_1k - se_claims_1k,
                      ymax = mean_claims_1k + se_claims_1k),
                  width = 0.2, size = 0.4, color = "gray50") +
    scale_x_continuous(breaks = seq(min(clm_ant_yr_sum$year),
                                    max(clm_ant_yr_sum$year), 1)) +
    scale_y_continuous(breaks = seq(420, 500, 20),
                       limits = c(420, 510)) +
    labs(x = "Year",
         y = "Antibiotic claims\nper 1K\nbeneficiaries")








clm_prscrbr_yr <- pd2 %>%
    filter(Antibiotic == 1) %>%
    group_by(Prscrbr_NPI, year) %>%
    summarize(
        n_ant = sum(!is.na(Tot_Clms)),
        tot_ant_clms = sum(Tot_Clms, an.rm = TRUE), .groups = "drop")

# Is it feasible for a provider to have 866 antibiotic claims in a single year?
summary(clm_ant_yr)
clm_ant_yr %>% filter(tot_ant_clms == 866)

# Join Tot_Benes from p_benes & compute claims / 1K benes
clm_ant_yr_sum <- clm_ant_yr %>%
    left_join(p_benes, by = c("Prscrbr_NPI", "year")) %>%
    mutate(claims_1k = tot_ant_clms / (Tot_Benes / 1000)) %>%
    
    # Summarize by year
    group_by(year) %>%
    summarize(
        
        # Rows missing claims_1k
        n_miss = sum(is.na(claims_1k)),
        
        # Rows with claims_1k
        n_prscrbr = sum(!is.na(claims_1k)),
        
        # Claims per 1K mean, sd, se
        mean_claims_1k = mean(claims_1k, na.rm = TRUE),
        sd_claims_1k = sd(claims_1k, na.rm = TRUE),
        se_claims_1k = sd_claims_1k / sqrt(n_prscrbr), .groups = "drop")


































# Antibiotic claims per 1K beneficiaries with at least one antibiotic claim
# NOTE: A better measure might be antibiotic claims per 1K beneficiaries with at
# least one *drug* claim. Would have to get this from the by-providers table.
pd2 %>%
    filter(Antibiotic == 1) %>%
    group_by(Prscrbr_NPI, Antibiotic, year) %>%
    summarize(Tot_Benes = sum(Tot_Benes, na.rm = TRUE),
              Tot_Clms = sum(Tot_Clms, na.rm = TRUE), .groups = "drop") %>%
    
    # When the sums above are computed you can actually get values of 0 if all
    # inputs are NA. Weird behavior! Eliminate all of those.
    filter(Tot_Benes >= 11, Tot_Clms >= 11) %>%
    mutate(benes_1k = Tot_Benes / 1000,
           claims_1k = Tot_Clms / benes_1k) %>%
    group_by(year) %>%
    summarize(n = sum(!is.na(claims_1k)),
              mean_claims_1k = mean(claims_1k, na.rm = TRUE),
              sd_claims_1k = sd(claims_1k, na.rm = TRUE),
              se_claims_1k = sd_claims_1k / sqrt(n), .groups = "drop") %>%
    mutate(group = "group") %>%
    ggplot(mapping = aes(x = year, y = mean_claims_1k, group = group)) +
    geom_line() +
    geom_errorbar(aes(ymin = mean_claims_1k - se_claims_1k,
                      ymax = mean_claims_1k + se_claims_1k),
                  width = 0.2, color = "gray50")
        
# Same for PCN-AminoPCN
pd2 %>%
    filter(`PCN-AminoPCN` == 1) %>%
    group_by(Prscrbr_NPI, `PCN-AminoPCN`, year) %>%
    summarize(Tot_Benes = sum(Tot_Benes, na.rm = TRUE),
              Tot_Clms = sum(Tot_Clms, na.rm = TRUE), .groups = "drop") %>%
    filter(Tot_Benes >= 11, Tot_Clms >= 11) %>%
    mutate(benes_1k = Tot_Benes / 1000,
           claims_1k = Tot_Clms / benes_1k) %>%
    group_by(year) %>%
    summarize(n = sum(!is.na(claims_1k)),
              mean_claims_1k = mean(claims_1k, na.rm = TRUE),
              sd_claims_1k = sd(claims_1k, na.rm = TRUE),
              se_claims_1k = sd_claims_1k / sqrt(n), .groups = "drop") %>%
    mutate(group = "group") %>%
    ggplot(mapping = aes(x = year, y = mean_claims_1k, group = group)) +
    geom_line() +
    geom_errorbar(aes(ymin = mean_claims_1k - se_claims_1k,
                      ymax = mean_claims_1k + se_claims_1k),
                  width = 0.2, color = "gray50")

# Cephalosporin
pd2 %>%
    filter(Cephalosporin == 1) %>%
    group_by(Prscrbr_NPI, Cephalosporin, year) %>%
    summarize(Tot_Benes = sum(Tot_Benes, na.rm = TRUE),
              Tot_Clms = sum(Tot_Clms, na.rm = TRUE), .groups = "drop") %>%
    filter(Tot_Benes >= 11, Tot_Clms >= 11) %>%
    mutate(benes_1k = Tot_Benes / 1000,
           claims_1k = Tot_Clms / benes_1k) %>%
    group_by(year) %>%
    summarize(n = sum(!is.na(claims_1k)),
              mean_claims_1k = mean(claims_1k, na.rm = TRUE),
              sd_claims_1k = sd(claims_1k, na.rm = TRUE),
              se_claims_1k = sd_claims_1k / sqrt(n), .groups = "drop") %>%
    mutate(group = "group") %>%
    ggplot(mapping = aes(x = year, y = mean_claims_1k, group = group)) +
    geom_line() +
    geom_errorbar(aes(ymin = mean_claims_1k - se_claims_1k,
                      ymax = mean_claims_1k + se_claims_1k),
                  width = 0.2, color = "gray50")





    
    
    
    
    
    
    
    
    # 









#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Summarize by-provider data ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Lots of rows missing Bene_Feml_Cnt. Is this due to conversion to/from
# character? Is it missing in the source data?
summary(p2$Bene_Feml_Cnt)

p2 <- p %>%
    mutate(across(matches("Cst|Tot|_Cnt$|Age|Risk_Scre"), as.numeric)) %>%
    mutate(percent_fem = Bene_Feml_Cnt / Tot_Benes)

# Summarize weighted p(female) by year. NAs represent rows where we have
# Tot_Benes, but don't have Bene_Feml_Cnt and so aren't able to estimate percent
# female.
p2 %>%
    filter(!is.na(Tot_Benes)) %>%
    # filter(!is.na(Bene_Female_Cnt)) %>%
    group_by(year) %>%
    summarize(n_providers = sum(!is.na(percent_fem)),
              n_na = sum(is.na(percent_fem)),
              mean_percent_fem = weighted.mean(percent_fem, Tot_Benes,
                                               na.rm = TRUE),
              sd_percent_fem = sd(percent_fem, na.rm = TRUE),
              min_percent_fem = min(percent_fem, na.rm = TRUE),
              max_percent_fem = max(percent_fem, na.rm = TRUE),
              .groups = "drop")

# Unweighted p(female) by year. Seems to consistently overestimate p(female) by
# a bit.
p2 %>%
    group_by(year) %>%
    summarize(n_providers = sum(!is.na(percent_fem)),
              n_na = sum(is.na(percent_fem)),
              mean_percent_fem = mean(percent_fem,
                                               na.rm = TRUE),
              sd_percent_fem = sd(percent_fem, na.rm = TRUE),
              min_percent_fem = min(percent_fem, na.rm = TRUE),
              max_percent_fem = max(percent_fem, na.rm = TRUE),
              .groups = "drop")



# Strategy for missing data:
# For each particular stat (e.g., mean HCC, or mean p(female)), compute weighted means based on all available data.
# Do modeling based on rows where none of the input are missing. Or do we want to try some imputation?







# Bene_Avg_Risk_Scre is suppressed when bene_count is between 1 and 10. A blank
# indicates the value is suppressed. So why are there only NAs in the 2017 data?
# Doesn't make sense that 2017 would be the only year with low bene_counts.

# Seems to be the case that a bunch of rows are missing total beneficiaries, total female, total male in the original data. So for any summary where we want to weight by # of beneficiaries we need to drop those rows
# Weighted mean HCC scores
p2 %>%
    filter(!is.na(Tot_Benes)) %>%
    group_by(year) %>%
    summarize(n_providers = sum(!is.na(Bene_Avg_Risk_Scre)),
              n_na = sum(is.na(Bene_Avg_Risk_Scre)),
              mean_HCC = weighted.mean(x = Bene_Avg_Risk_Scre, w = Tot_Benes,
                                       na.rm = TRUE),
              sd_HCC = sd(Bene_Avg_Risk_Scre, na.rm = TRUE),
              min_HCC = min(Bene_Avg_Risk_Scre, na.rm = TRUE),
              max_HCC = max(Bene_Avg_Risk_Scre, na.rm = TRUE),
              .groups = "drop")

# Shows that ~ 450 rows per year are missing the number of beneficiaries
p2 %>%
    group_by(year) %>%
    summarize(NA_Tot_Benes = sum(is.na(Tot_Benes)), .groups = "drop")

# Unweighted mean HCC scores. Looks like unweighted have somewhat lower scores...
p2 %>%
    group_by(year) %>%
    summarize(n_providers = sum(!is.na(Bene_Avg_Risk_Scre)),
              n_na = sum(is.na(Bene_Avg_Risk_Scre)),
              mean_HCC = mean(x = Bene_Avg_Risk_Scre,
                                       na.rm = TRUE),
              sd_HCC = sd(Bene_Avg_Risk_Scre, na.rm = TRUE),
              min_HCC = min(Bene_Avg_Risk_Scre, na.rm = TRUE),
              max_HCC = max(Bene_Avg_Risk_Scre, na.rm = TRUE),
              .groups = "drop")













# Write a function to take any measure and return n, n_na, mean, sd, min, maz
# NOTE: Don't just want to take means for these measures. Need to weight things by the number of beneficiaries. Otherwise I'm treating each row as if it has equal weights in the computation, when in reality some rows will represent a lot more beneficiaries than others.



































#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Next section ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ,
# 
# # Column types aren't consistent across different years of the
# # same dataset, so enforce consistency by converting to
# # character.
# Prscrbr_State_FIPS = as.character(Prscrbr_State_FIPS),
# Tot_Clms = as.character(Tot_Clms),
# Tot_30day_Fills = as.character(Tot_30day_Fills),
# Tot_Drug_Cst = as.character(Tot_Drug_Cst),
# Tot_Day_Suply = as.character(Tot_Day_Suply),
# Bene_Avg_Age = as.character(Bene_Avg_Age),
# Bene_Avg_Risk_Scre = as.character(Bene_Avg_Risk_Scre)



# Definitions
# Prscrbr_NPI: national provider identifier
# Prscrbr_Ent_Cd = 1 if the provider is an individual; 0 if it's an organization. Shows that all but one of the 1.2M rows is coded "I" for individual. The single O code is for "County Of Salt Lake" in SLC, UT.
# Prscrbr_Last_org_Name: if above is 1 this is the last name of the provider. If 0 it's the name of the organization
# Prescriber_MI is middle initial if the provider is an individual (see above), otherwise blank.
# Prscrbr_Crdntls gives credentials  for individuals, otherwise blank
# Prscrbr_Gndr gives prescriber's gender if individual (F/M), otherwise blank
# Prscrbr_Stat_FIPS. FIPS code for referring provider's state. What's FIPS?
# Prescriber RUCA is rural-urban commuting area codes. Supposed to be a census tract-based rural/urban classification scheme.
# Prscrbr_RUCA_Desc gives labels for the above classification. Shows that RUCA isn't as simple as urban/rural, but could possibly be used to build an urban/rural variable.
# Prscrbr_Type is the prescriber's specialty type, e.g. "Addiction Medicine", "Allergy/ Immunology". Has 218 rows. Possibly this could be simplified?
# Prscrbr_Type_Src. Source of the prescriber specialty. S = medicare specialty code descrption; T = Taxonomy code classification description.
# Antbtc_Tot_Clms. total claims of antibiotic drugs, including refills. Value is only shown if it's >= 11.
# Antbtc_Tot_Benes. Number of medicare beneficiaries filling antibiotic claims. Value is only shown when it's >= 11.
# Bene_Avg_Age. Average age of beneficiaries. Could be used to compute average health score for beneficiaries seeing this provider.
# Bene_Feml_Cnt. Nubmer of female beneficiaries. Could be part of average health score.
# Bene_Male_Cnt
# Race/ethnicity variables. Give numbers of beneficiaries for different categories--e.g., hispanic/not, white, black, asian.
# Bene_Avg_Risk_Score. Average hierarchical condition category (HCC) risk score of beneficiaries. Cool--already contains HCC so I don't have to compute it. Score estimates how beneficiaries FFS (fee for service) spending will compare to the overall average for the entire Medicare population. Larger scores indicate greater risk and expected increased spending. Based on age, sex, elibigility for Medicaid, if they first qualified for Medicare on the basis of disability, livis inan institution (usually a nursing home), beneficiary's diagnoses from previous year. Problem for Kurt's old analysis is inclusion of both HCC and average beneficiary age, since HCC is computed based on age, among other variables. Probably better to break these out.

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Read in & clean up data ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Get 2019 by-provider data for Idaho only.
# NOTE: In the URL below, the part after "dataset" is the UUID. The part that
# says "data.json" means that we're going to be able to get the entire dataset
# without having to manage pagination.
p_id <- fromJSON("https://data.cms.gov/data-api/v1/dataset/be8044a7-89bd-46ea-8a77-8285a100d572/data.json?filter[Prscrbr_State_Abrvtn]=ID") %>%
    as_tibble()

# Eventually substitute the above code for this part. Will be more automated.
# Each row is a prescriber. Read in prescriber NPI and zip code as character
# p <- read_csv("Medicare_Part_D_Prescribers_by_Provider_2019.csv",
#               col_types = cols(.default = "?",
#                                PRSCRBR_NPI = "c",
#                                Prscrbr_zip5 = "c"))

# Do we have one row per prescriber NPI? Yes.
nrow(p_id) == length(unique(p_id$PRSCRBR_NPI))

# No prescribers in Idaho are missing zip codes. But there are NAs
p_id %>% filter(is.na(Prscrbr_zip5))

# Indicators for rural zips from:
# https://www.cms.gov/Medicare/Medicare-Fee-for-Service-Payment/DMEPOSFeeSched/DMEPOS-Fee-Schedule-Items/DME-Rural-Zip-and-Formats
# See file "DMEREADLAYOUTS16-Revised 10-21-2015.pdf" in this directory on how
# rural is defined--seems to be a different method than RUCA.
# This is what Karl used in his previous study. DMEPOS is "durable medical
# equipment, prosthetics/orthotics, and supplies.
rural <- read_csv("DMERuralZIP.csv")

# Shows that all rows are from Q4 of 2015. The whole set of downloadable files
# is labeled as 2016, but the zip code stuff is Q4 2015. Is there an updated
# dataset somewhere? Is this the right way to define rural for this study?
rural %>% count(`YEAR/QTR`)

# Filter to Idaho and repair illegitimate zip codes. Turns out there aren't any
# in Idaho. Seems to be an issue that affects East Coast zips that begin with
# one or more zeros. Some prefix zeros these are incorrectly dropped in the
# source file, so the fix is to put them back in.
p_id2 <- p_id %>%
    
    # Fix zips
    mutate(
        Prscrbr_zip5 = as.character(Prscrbr_zip5),
        precheck_zip = if_else(nchar(Prscrbr_zip5) == 5, 0, 1),
        Prscrbr_zip5 = case_when(
            nchar(Prscrbr_zip5) == 4 ~ paste0("0", Prscrbr_zip5),
            nchar(Prscrbr_zip5) == 3 ~ paste0("00", Prscrbr_zip5),
            TRUE ~ Prscrbr_zip5),
        postcheck_zip = if_else(nchar(Prscrbr_zip5) == 5, 0, 1)) %>%
    
    # Join in rural info
    left_join(rural %>%
                  mutate(rural_dmepos = 1L) %>%
                  select(Prscrbr_zip5 = `DMEPOS RURAL ZIP CODE`, rural_dmepos),
              by = "Prscrbr_zip5") %>%
    
    # Set up different rural indicators. See "Rural definitions.pdf" for more
    # info on the RUCA definition (downloaded from
    # https://www.ers.usda.gov/webdocs/DataFiles/53180/25567_ID.pdf?v). Also see
    # "RUCA2010zipcode.number", downloaded from USDA's Economic Research Service
    # (https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes/).
    # Looks like the most recent RUCA is from 2010. Wouldn't be surprised if
    # Medicare is using this zipcode-RUCA table to assign RUCAs in the
    # prescribers datatsets.
    mutate(rural_dmepos = if_else(is.na(rural_dmepos), 0L, rural_dmepos),
           
           # This definition is given in "Rural definitions.pdf" in this
           # directory. RUCAs 1-3 are all "metropolitan". RUCAs 4-6 are
           # "micropolitan". RUCAs 7-9 are "small towns". RUCA 10 is "rural
           # area".
           rural_ruca = if_else(Prscrbr_RUCA >= 4, 1L, 0L)) %>%
    
    # Only keep rows with > 10 antibiotic claims & beneficiaries
    filter(Antbtc_Tot_Clms > 10,
           Antbtc_Tot_Benes > 10)

# The two rural indicators have similar distributions
p_id2 %>% count(rural_dmepos)
p_id2 %>% count(rural_ruca)

# The two rural indicators are correlated at 0.89
cor(p_id2$rural_dmepos, p_id2$rural_ruca)





# %>%
#     
#     # Categorize prescriber type
#     mutate(Prscrbr_Type2 = case_when(
#         Prscrbr_Type %in% c("Allergy/ Immunology", "Anesthesiology", "Cardiology", "") ~ "Medicine physicians",
#         Prscrbr_Type %in% c() ~ "Surgery",
#         Prscrbr_Type %in% c() ~ "Dentists",
#         Prscrbr_Type %in% c() ~ "PA",
#         Prscrbr_Type %in% c() ~ "NP",
#         TRU ~ "Other")) %>%
#     
#     # Keep these cols
#     select(PRSCRBR_NPI, Prscrbr_Ent_Cd, Prscrbr_Crdntls, Prscrbr_Type,
#            Prscrbr_Gndr, Prscrbr_St1:Prscrbr_State_Abrvtn, Prscrbr_zip5,
#            Prscrbr_Cntry, rural, Antbtc_Tot_Clms, Antbtc_Tot_Benes,
#            Bene_Avg_Risk_Scre, precheck_zip, postcheck_zip)

# Shows that no Idaho zips needed to be repaired
p2 %>% count(precheck_zip)
p2 %>% count(postcheck_zip)

# Distribution of rural
p2 %>% count(rural)

# Write to disk
write_csv(p2, "Medicare Part D Idaho antibiotic use 2019.csv")





# Checking credentials for 
xtabs(~ Prscrbr_Type, p2)

# These are all doctors (DO or MD)
p2 %>%
    filter(Prscrbr_Type == "Allergy/ Immunology") %>%
    count(Prscrbr_Crdntls)

# Dentists are mostly DDS, DMD. One each of MD, RN, NA
p2 %>%
    filter(Prscrbr_Type == "Dentist") %>%
    count(Prscrbr_Crdntls)

p2 %>%
    filter(Prscrbr_Type == "Family Practice") %>%
    count(Prscrbr_Crdntls)






#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Compute summary stats ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Demographics


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Prescribers by provider and drug ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# This is Idaho data only. Parsing errors when I try to read in full dataset.
pd <- read_csv("Medicare_Part_D_Prescribers_by_Provider_and_Drug_2019.csv")

# Table of generic names
pd %>%
    count(Gnrc_Name) %>%
    arrange(desc(n)) %>%
    View()




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Definie rural ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# https://www.ers.usda.gov/webdocs/DataFiles/53180/25567_ID.pdf?v
# RUCA (rural-urban commuting areas). Says RUCA tracts with codes 4-10 are rural; 1-3 are urban.





