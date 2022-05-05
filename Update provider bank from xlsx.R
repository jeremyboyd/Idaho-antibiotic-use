# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Reads in a provider type file that Karl has manually coded in
# xlsx. Simpflfies and writes to feather.

box_dir <- "~/Library/CloudStorage/Box-Box/IDHW_2022_Idaho_Antibiotic_project/"
read_xlsx(
    paste0(
        box_dir,
        "Idaho 2019 providers by prescriber data with address names_kmk.xlsx"),
    sheet = "Provder_Specialty_by_Count") %>%
    rename(Std_Provider_Type2 = ...3) %>%
    mutate(across(where(is.character), ~ str_squish(.)),
           year = 2019L) %>%
    select(Prscrbr_Type, year, Std_Provider_Type, Std_Provider_Type2) %>%
    write_feather("Idaho 2019 provider type bank.feather")
