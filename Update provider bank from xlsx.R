# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Reads in a provider type file that Karl and I have manually coded
# in CSV. Simpflfies and writes to feather.

box_dir <- "~/Library/CloudStorage/Box-Box/IDHW_2022_Idaho_Antibiotic_project/"
read_csv(
    paste0(
        box_dir,
        # "Idaho 2019 providers by prescriber data with address names_kmk.xlsx"
        "Provider type bank.csv")) %>%
    rename(Std_Provider_Type2 = ...3) %>%
    mutate(across(where(is.character), ~ str_squish(.))) %>%
    select(Prscrbr_Type, Std_Provider_Type, Std_Provider_Type2) %>%
    write_feather("Idaho provider type bank.feather")
