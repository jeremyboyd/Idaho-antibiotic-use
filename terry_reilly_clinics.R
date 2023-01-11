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
              by = "dataset_address")

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



%>%
    select(Prscrbr_NPI:Prscrbr_Crdntls, Prscrbr_Type, dataset_address,
           address_name = name, status)
write_feather(p2019.2,
              "Idaho 2019 providers by prescriber data with names.feather")
