# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Use API to get Medicare Part D prescriber data for antibiotic use
# study.

# TODO:
# Automatically check to see if new data versions exist. If so, download them.
# How do I add county? Is there a standardized way to go from zip code to county? Think I could get more location info from a Google place search, including county.

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Add place names to Idaho 2019 providers-by-prescriber ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Read in 2019 data
p2019 <- read_feather("Idaho prescribers by provider data.feather") %>%
    filter(year == 2019)

# Make sure all addresses have been queried in Google Places API
get_address_names(input_table = p2019)

# Make sure bank is up-to-date with names from the zero results table
zero <- read_csv("Zero address-name results from Idaho 2019 by-prescribers.csv")
update_zero_results(zero_table = zero)

# Read in bank for copying
bank <- read_feather("Address-name bank.feather")

# Create table with prescriber names, credentials, type, address, and address
# name. This is for use in manually coding different types of prescribers.
p2019.2 <- p2019 %>%
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
    select(Prscrbr_NPI:Prscrbr_Crdntls, Prscrbr_Type, dataset_address,
           address_name = name, status)
write_feather(p2019.2,
              "Idaho 2019 providers by prescriber data with names.feather")

# CSV versions of the bank and Idaho 2019 with names saved to Box
box_dir <- "~/Library/CloudStorage/Box-Box/IDHW_2022_Idaho_Antibiotic_project/"
write_csv(bank, paste0(box_dir, "Address-name bank.csv"))
write_csv(p2019.2, paste0(
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

# Table of generic drugs with coding for antibiotic and antibiotic type. Right
# now I'm reading from an Excel version Karl has been editing. Once he's done,
# save as "Generic drug bank.csv". As of today (4/28), he's completed all coding
# for the Antibiotic column. As expected, since there are now more drugs coded
# as antibiotic, we get more claims / 1K beneficiaries, versus when we only have
# the 2019 drugs coded for antibiotic status.
# generics <- read_csv(paste0(box_dir, "Generic drug bank.csv")) %>%
generics <- read_xlsx(paste0(box_dir, "Generic drug bank_kmk.xlsx"),
                      na = "NA") %>%

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

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Model claims_1k ~ year ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Antibiotic claims by prescriber & year
ant_cl_1k <- claims_year(drug_class = "Antibiotic") %>%
    left_join(p_benes, by = c("Prscrbr_NPI", "year")) %>%
    mutate(claims_1k = tot_clms / (Tot_Benes / 1000),
           log_claims_1k = log(claims_1k))

# Visualize with fit lines
ant_cl_1k %>%
    ggplot(aes(x = year, y = claims_1k)) +
    geom_smooth(method = lm) +
    labs(x = "Year",
         y = "Claims per\n1K beneficiaries",
         color = "Class")
ant_cl_1k %>%
    ggplot(aes(x = year, y = log_claims_1k)) +
    geom_smooth(method = lm) +
    labs(x = "Year",
         y = "Claims per\n1K beneficiaries",
         color = "Class")

# What does the distribution look like? Not normal--long tail to the right.
ant_cl_1k %>%
    ggplot(mapping = aes(x = claims_1k)) +
    geom_density()

# Looks better with log transform
ant_cl_1k %>%
    ggplot(mapping = aes(x = log_claims_1k)) +
    geom_density()

# Marginal decrease in claims per 1k beneficiaries over time, p = 0.051.
cl_1k_fit1 <- lm(claims_1k ~ year, data = ant_cl_1k)
summary(cl_1k_fit1)

# Really should be a growth curve model with clusters for Prscrbr_NPI. This
# version throws a singularity warning. Try bayesian fit.
library(lmerTest)
cl_1k_fit_fit2 <- lmer(log_claims_1k ~ year + (year | Prscrbr_NPI),
                       data = ant_cl_1k)
summary(cl_1k_fit_fit2)

# Try brms fit with out-of-the-box priors. Replace with custom! Some warnings:
# (1) "parts of the model have not converged; recommend running more iterations
# and/or setting stronger priors, (2) 25 divergent transitions.
# Results so far show a reliable negative effect of year--i.e., as year
# increases, log(claims_1k) goes down. Can also try lognormal family and logt
# model.
library(brms)
cl_1k_fit3 <- brm(
    log_claims_1k ~ year + (year | Prscrbr_NPI),
    data = ant_cl_1k,
    family = gaussian(),
    control = list(adapt_delta = 0.95),
    chains = 4,
    iter = 2000,
    warmup = 1000,
    cores = 4,
    seed = 1,
    backend = "cmdstanr")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Summarize 2019 Idaho by-provider dataset by Prscrbr_Type ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# NOTE: Karl was going to use prescriber type, credentials, and the name of
# their organization (recovered from Google Places) to to another categorization
# of prescribers by type. Doesn't look like this has been completed yet (as of
# 4/29).

# Maybe create a function to do this where you can just input different variables--e.g., create a summary table by Prscrbr_Type1, or Prscrbr_Type2. That way if a new categorization comes online it'll be easy to feed it into the function.

# For table slide 11, add N to table (number of providers of a specific type). Looks like slides 11-13 are just pulling different numbers from the same table. Function to compute the table, then something like data.table to pull out different tables.
    
# 8.5% of the data have NA values for Tot_Benes, which is why you can't compute a weighted mean. Could address this by dropping rows, computing unweighted, imputing Tot_Benes. Reason 2017 works is that NAs are in exactly the same rows for the x and w values. weighted.sd() seems to automatically remove NAs in both x and wt.
sum(is.na(p$Tot_Benes)) / nrow(p)










p %>%
    filter(year == 2017) %>%
    summarize(weighted.mean(x = Bene_Avg_Age, w = Tot_Benes, na.rm = TRUE))


p %>%
    filter(year == 2019) %>%
    filter(!is.na(Tot_Benes)) %>%
    select(Bene_Avg_Age, Tot_Benes) %>%
    summary()







