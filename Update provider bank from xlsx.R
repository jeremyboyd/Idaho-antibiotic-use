# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Reads in a provider type file that Karl has manually coded in
# xlsx. Simpflfies and writes to feather.

box_dir <- "~/Library/CloudStorage/Box-Box/IDHW_2022_Idaho_Antibiotic_project/"
read_xlsx(
    paste0(
        box_dir,
        "Idaho 2019 providers by prescriber data with address names.xlsx"),
    sheet = "Provder_Specialty_by_Count") %>%
    mutate(across(where(is.character), ~ str_squish(.))) %>%
    select(Prscrbr_Type, Standard_Provider_Type) %>%
    write_feather("Idaho 2019 provider type bank.feather")
