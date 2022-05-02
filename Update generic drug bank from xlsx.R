# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Reads in a drug bank file that Karl has coded in xlsx, updates
# NAs for non-antibiotics to 0, then writes to feather.

# Read in table where Karl has manually coded generic drugs for a bunch of
# different drug classes.
box_dir <- "~/Library/CloudStorage/Box-Box/IDHW_2022_Idaho_Antibiotic_project/"
generics <- read_xlsx(paste0(box_dir, "Generic drug bank_kmk.xlsx"),
                      na = "NA") %>%
    
    # Get rid of any extra whitespace in character cols. This can cause joins to
    # fail later.
    mutate(across(where(is.character), ~ str_squish(.))) %>%
    select(Gnrc_Name, Antibiotic:Other)

# Rows coded Antibiotic == 1. Summary shows that columns from Antibiotic:Other
# have zero NAs.
ant1 <- generics %>% filter(Antibiotic == 1)
summary(ant1)

# Rows coded Antibiotic == 0. Summary shows all cols after Antibiotic with NAs.
ant0 <- generics %>% filter(Antibiotic == 0)
summary(ant0)

# Convert NAs to 0
ant0 <- ant0 %>%
    mutate(across(where(is.double), ~ if_else(is.na(.), 0, .)))

# rbind tables and save
bind_rows(ant1, ant0) %>%
    write_feather("Generic drug bank.feather")
