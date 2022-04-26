# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Get Idaho Medicare Part D prescriber data. This includes all
# available years of the by-provider & by-provider-and-drug datasets.

# Get all versions (years) of medicare-part-d-prescribers-by-provider
p_ver <- get_dataset_versions(
    dataset = "medicare-part-d-prescribers-by-provider")
p <- get_data(version_table = p_ver)

# Get all versions of medicare-part-d-prescribers-by-provider-and-drug
pd_ver <- get_dataset_versions(
    dataset = "medicare-part-d-prescribers-by-provider-and-drug")
pd <- get_data(version_table = pd_ver)

# Write all tables to file
write_feather(p_ver, "Prescribers by provider versions.feather")
write_feather(p, "Idaho prescribers by provider data.feather")
write_feather(pd_ver, "Prescribers by provider & drug versions.feather")
write_feather(pd, "Idaho prescribers by provider & drug data.feather")
