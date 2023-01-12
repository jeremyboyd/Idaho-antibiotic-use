# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Analysis of 2020 Medicate Part D prescribing data for Terry Reilly clinics in Idaho, doctors (MD, DO) and PAs only.

df <- read_feather("Idaho prescribers by provider data.feather") %>%
    filter(year == 2020)

# Make sure all addresses have been queried in Google Places API
get_address_names(input_table = df)

# Make sure bank is up-to-date with names from the zero results table
zero <- read_csv("Zero address-name results from Idaho 2019 by-prescribers.csv")
update_zero_results(zero_table = zero)

# Read in bank for copying
bank <- read_feather("Address-name bank.feather")

# Join in names for each address
df2 <- df %>%
    mutate(
        dataset_address = str_squish(
            paste(
                Prscrbr_St1,
                Prscrbr_St2,
                Prscrbr_City,
                Prscrbr_State_Abrvtn,
                Prscrbr_Zip5
            )
        )) %>%
    left_join(bank %>%
                  select(dataset_address, name, status),
              by = "dataset_address") %>%
    rename(clinic = name)

# Google wasn't able to find names for these addresses:
df2 %>% filter(status == "ZERO_RESULTS")

# Table of distinct Terry Reilly names & addresses
df2 %>%
    filter(str_detect(name, "Terry")) %>%
    select(name, dataset_address) %>%
    unique() %>%
    arrange(name)

# Distinct providers
df2 %>%
    filter(str_detect(name, "Terry")) %>%
    select(Prscrbr_NPI) %>%
    unique()

# Distinct provider credentials
df2 %>%
    filter(str_detect(name, "Terry")) %>%
    select(Prscrbr_Type) %>%
    unique() %>%
    View()

# Only doctors & PAs
df2 %>%
    filter(str_detect(name, "Terry"),
           Prscrbr_Type %in% c("Family Practice", "Physician Assistant")) %>%
    select(Prscrbr_Crdntls) %>%
    unique() %>%
    View()


p_types <- read_feather("Idaho provider type bank.feather") %>%
    select(Prscrbr_Type, Std_Provider_Type)

# medical/urgent care as one group, dental as another
# 16th St. has urgent care
# 1st St. has medical & dental
# 23rd St. has medical, urgent care
# Cleveland has medical, urgent care & dental
# Marsing has medical & dental
# Melba has medical & dental
# Middleton has medical & dental
# Since some clinicis provide all of medical/primary, urgent care, and dental, we have to categorize based on provider type & credentials rather than clinic


df2 %>%
    filter(str_detect(name, "Terry")) %>%
    select(Prscrbr_Type, Prscrbr_Crdntls) %>%
    unique() %>%
    View()




df3 <- df2 %>%
    filter(str_detect(clinic, "Terry")) %>%
    
    # Categorize provvider types
    mutate(provider_cat = case_when(
        Prscrbr_Type %in%
            c("Family Practice", "Physician Assistant", "Nurse Practitioner",
              "Certified Clinical Nurse Specialist") ~ "medical",
        Prscrbr_Type %in% c("Dentist") ~ "dental",
        TRUE ~ Prscrbr_Type)) %>%
    
    # Only medical & dental
    filter(provider_cat %in% c("medical", "dental")) %>%
        
    # Clean up clinic info. One Boise dentist seems to have given his home
    # address, but web searches show he's at the Boise dental clinic. Another
    # clinic--simply called "Terry Reilly Health Services"--has a 16th Ave Nampa
    # address, so group it with 16th Ave Clinic.
    mutate(clinic = case_when(
        dataset_address == "2301 N 26th St Ste 102 Boise ID 83702" ~
            "Terry Reilly Health Services - Boise Dental",
        clinic == "Terry Reilly Health Services" ~
            "Terry Reilly Health Services - 16th Ave. Clinic",
        TRUE ~ clinic),
        
        # Compute claims/1K beneficiaries
        claims_1k = Antbtc_Tot_Clms / (Tot_Benes / 1000))



# Look at people with 0 or NA values for claims_1k. NA for Antbtc_Tot_Clms or
# Tot_Benes means that the actual value was < 11 and so is suppressed.
df3 %>%
    filter(is.na(claims_1k) | claims_1k == 0) %>%
    select(Antbtc_Tot_Clms, Tot_Benes, claims_1k) %>%
    View()

# We can work with providers who have 0 for claims_1k, but not NA. Filter out
# the NAs. This leaves 52 providers, with medical and dental providers distributed among clinics like this:
df3 %>%
    filter(!is.na(claims_1k)) %>%
    count(clinic, provider_cat)

df4 <- df3 %>%
    filter(!is.na(claims_1k))

# Do we really want to drop the NAs? It's not like we know nothing for these people--we know that the NA values are all < 11. So maybe impute.
df5 <- df3 %>%
    
    # Create flag for counts < 11
    mutate(low_count = if_else(is.na(claims_1k), 1L, 0L),
           
           # Impute missing counts as 10
           across(.cols = c("Antbtc_Tot_Clms", "Tot_Benes"),
                  ~ if_else(is.na(.x), 10, .x)),
           
           # Recompute claims_1k
           claims_1k = Antbtc_Tot_Clms / (Tot_Benes / 1000))

# Create two datasets to work from, in the same table
df6 <- bind_rows(
    df5 %>% mutate(dataset = "imputed_NAs"),
    df5 %>% filter(low_count != 1) %>% mutate(dataset = "NAs_removed"))

# Compute mean claims_1k by dataset, clinic, provider_cat
df7 <- df6 %>%
    group_by(clinic, provider_cat, dataset) %>%
    summarize(n = sum(!is.na(claims_1k)),
              mean_claims_1k = mean(claims_1k, na.rm = TRUE), .groups = "drop")

# Visualize
df7 %>%
    mutate(clinic = str_remove(clinic, ".+ - ")) %>%
    ggplot(aes(x = clinic, y = mean_claims_1k, label = n)) +
    geom_col(fill = "deepskyblue", alpha = .5) +
    geom_text(nudge_y = 30, size = 3) +
    coord_flip() +
    facet_grid(rows = vars(provider_cat),
               cols = vars(dataset))

# Individual medical providers, NAs removed
df6 %>%
    mutate(clinic = str_remove(clinic, ".+ - "),
           Prscrbr_NPI = fct_reorder(Prscrbr_NPI, -claims_1k)) %>%
    filter(dataset == "NAs_removed",
           provider_cat == "medical") %>%
    ggplot(aes(x = Prscrbr_NPI, y = claims_1k)) +
    geom_col(fill = "deepskyblue", alpha = .5) +
    theme(axis.text.x = element_blank()) +
    labs(title = "Medical provider claims per 1K beneficiaries, NAs removed")
        
# Individual medical providers, NAs imputed
df6 %>%
    mutate(clinic = str_remove(clinic, ".+ - "),
           Prscrbr_NPI = fct_reorder(Prscrbr_NPI, -claims_1k)) %>%
    filter(dataset == "imputed_NAs",
           provider_cat == "medical") %>%
    ggplot(aes(x = Prscrbr_NPI, y = claims_1k)) +
    geom_col(fill = "deepskyblue", alpha = .5) +
    theme(axis.text.x = element_blank()) +
    labs(title = "Medical provider claims per 1K beneficiaries, NAs imputed")

# Individual dental providers, NAs removed
df6 %>%
    mutate(clinic = str_remove(clinic, ".+ - "),
           Prscrbr_NPI = fct_reorder(Prscrbr_NPI, -claims_1k)) %>%
    filter(dataset == "NAs_removed",
           provider_cat == "dental") %>%
    ggplot(aes(x = Prscrbr_NPI, y = claims_1k)) +
    geom_col(fill = "deepskyblue", alpha = .5) +
    theme(axis.text.x = element_blank()) +
    labs(title = "Dental provider claims per 1K beneficiaries, NAs removed")

# Individual dental providers, NAs imputed
df6 %>%
    mutate(clinic = str_remove(clinic, ".+ - "),
           Prscrbr_NPI = fct_reorder(Prscrbr_NPI, -claims_1k)) %>%
    filter(dataset == "imputed_NAs",
           provider_cat == "dental") %>%
    ggplot(aes(x = Prscrbr_NPI, y = claims_1k)) +
    geom_col(fill = "deepskyblue", alpha = .5) +
    theme(axis.text.x = element_blank()) +
    labs(title = "Dental provider claims per 1K beneficiaries, NAs imputed")
























