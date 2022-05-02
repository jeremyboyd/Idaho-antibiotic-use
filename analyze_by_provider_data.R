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

# Read in generic drug bank
generics <- read_feather("Generic drug bank.feather")

# Both of the following tables are needed to run claims_year() and
# claims_1k_summary(). pd2 is the source for numerators in the claims per 1K
# beneficiary computation; p_benes is the source for denominators.
pd2 <- pd %>%
    left_join(generics, by = "Gnrc_Name") %>%
    select(Prscrbr_NPI, year, Gnrc_Name, Tot_Clms, Antibiotic:Other)
p_benes <- p %>%
    select(Prscrbr_NPI, year, Tot_Benes)

# Drug classes that we want to compute claims per 1K beneficiaries for
drug_classes <- names(generics)[2:length(names(generics))]

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
    scale_x_continuous(breaks = seq(min(pd_claims_1k_sum$year),
                                    max(pd_claims_1k_sum$year), 1)) +
    facet_wrap(~ class) +
    labs(x = "Year",
         y = "Claims per\n1K beneficiaries",
         color = "Class") +
    theme(legend.position = "none")
ggsave(paste0(box_dir, "figures/Claims per 1K beneficiaries, antibiotics.png"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Model claims_1k ~ year ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Table of claims_1k by provider & year with claims from by-provider-and-drug
# and beneficiaries from by-provider.
pd_claims_1k <- map_dfr(drug_classes, function(class) {
    message(paste0("Getting claim & beneficiary data for class ", class, "..."))
    claims_year(drug_class = class) %>%
        left_join(p_benes, by = c("Prscrbr_NPI", "year")) %>%
        mutate(claims_1k = tot_clms / (Tot_Benes / 1000),
               log_claims_1k = log(claims_1k),
               class = class)
}) 

# I'm logging claims_1k because it's much closer to normal then. See these
# examples of unlogged versus logged claims_1k for antibiotics.
pd_claims_1k %>%
    filter(class == "Antibiotic") %>%
    ggplot(mapping = aes(x = claims_1k)) +
    geom_density()
pd_claims_1k %>%
    filter(class == "Antibiotic") %>%
    ggplot(mapping = aes(x = log_claims_1k)) +
    geom_density()

# Model log_claims_1k ~ year for all classes
year_effects <- map_dfr(drug_classes, function(class) {
    class_data <- pd_claims_1k %>%
        filter(class == !!class)
    lm(log_claims_1k ~ year, data = class_data) %>%
    tidy() %>%
    mutate(class = class)
}) %>%
    mutate(
        `p < 0.05` = if_else(p.value < 0.05, 1L, 0L)) %>%
    filter(term == "year") %>%
    arrange(p.value)

# Compute percent decrease from 2013 to 2019
change <- pd_claims_1k_sum %>%
    filter(year %in% c(min(pd_claims_1k_sum$year),
                       max(pd_claims_1k_sum$year))) %>%
    mutate(class = if_else(str_detect(class, "Antibiotic"),
                           "Antibiotic", class)) %>%
    select(year, mean, class) %>%
    pivot_wider(names_from = "year", values_from = "mean") %>%
    mutate(percent_decrease = (`2013` - `2019`) / `2013` * 100)

# Plot claims_1k ~ year, with annotations showing percent decrease and p-values
# for statsig decreases.
pd_claims_1k %>%
    group_by(year, class) %>%
    summarize(
        n_miss = sum(is.na(claims_1k)),
        n_prscrbr = sum(!is.na(claims_1k)),
        mean = mean(claims_1k, na.rm = TRUE),
        sd = sd(claims_1k, na.rm = TRUE),
        se = sd / sqrt(n_prscrbr), .groups = "drop") %>%
    left_join(year_effects, by = "class") %>%
    left_join(change %>%
                  select(class, percent_decrease), by = "class") %>%
    mutate(class2 = if_else(`p < 0.05` == 1,
                            paste0(
                                class, "\n",
                                round(percent_decrease),
                                "% decrease\n",
                                "p = ",
                                format(p.value, scientific = TRUE,
                                       digits = 3)),
                            class),
           class2 = fct_relevel(class2, "Other", after = Inf)) %>%
    ggplot(mapping = aes(x = year,
                         y = mean,
                         group = class2,
                         color = class2)) +
    geom_errorbar(aes(ymin = mean - se,
                      ymax = mean + se),
                  width = 0.2, size = 0.4, color = "gray50") +
    geom_line() +
    scale_x_continuous(breaks = seq(min(pd_claims_1k_sum$year),
                                    max(pd_claims_1k_sum$year), 2)) +
    scale_y_continuous(breaks = seq(0, 1000, 100)) +
    facet_wrap(~ class2, ncol = 4) +
    labs(x = "Year",
         y = "Claims per\n1K beneficiaries",
         color = "Class") +
    theme(legend.position = "none")
ggsave(paste0(box_dir, "figures/Claims per 1K beneficiaries, all classes, year effects.png"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Hierarchical models of claims_1k ~ year ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Really should be a growth curve model with clusters for Prscrbr_NPI. This
# version throws a singularity warning. Try bayesian fit.
cl_1k_fit_fit2 <- lmer(log_claims_1k ~ year_c + (year_c | Prscrbr_NPI),
                       data = ant_cl_1k)
summary(cl_1k_fit_fit2)

# Try brms fit with out-of-the-box priors. Replace with custom! Some warnings:
# (1) "parts of the model have not converged; recommend running more iterations
# and/or setting stronger priors, (2) 25 divergent transitions.
# Results so far show a reliable negative effect of year--i.e., as year
# increases, log(claims_1k) goes down. Can also try lognormal family and logt
# model.

# Specify priors
claims_priors <- c(
    prior(normal(6, 2), class = Intercept),
    prior(normal(0, 1), class = b),
    prior(cauchy(0, 1), class = sd),
    prior(lkj(2), class = cor)
)

# Fit
cl_1k_fit3 <- brm(
    log_claims_1k ~ year_c + (year_c | Prscrbr_NPI),
    data = ant_cl_1k,
    prior = claims_priors,
    # family = gaussian(),
    family = student(),
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

















