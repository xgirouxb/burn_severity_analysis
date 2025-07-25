# DATA SOURCES

# ---------------------------------------------------------------------------- #
# Statistics Canada territorial and provincial cartographic boundaries
# Metadata available at: https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21
url_statcan_admin_bounds <- "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lpr_000b21a_e.zip"

# ---------------------------------------------------------------------------- #
# CWFIS National Burned Area Composite (NBAC) polygons
# Metadata available at: https://cwfis.cfs.nrcan.gc.ca/datamart/metadata/nbac
url_nbac_archive <- "https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/"

# ---------------------------------------------------------------------------- #
# Matt Heathcoat's raw BC burn severity dataset 
path_fire_data <- "data/raw_burn_data/BCfiresDF.csv"

# ---------------------------------------------------------------------------- #
# VRI - Historical Vegetation Resource Inventory (2002 - 2023)
# Metadata available at: https://catalogue.data.gov.bc.ca/dataset/vri-historical-vegetation-resource-inventory-2002-2023-
url_vri_archive <- "https://pub.data.gov.bc.ca/datasets/02dba161-fdb7-48ae-a4bb-bd6ef017c36d/"

# ---------------------------------------------------------------------------- #
# VRI - Table with species code keys for VRI leading species
# Extracted from p.214 : https://www2.gov.bc.ca/assets/gov/farming-natural-resources-and-industry/forestry/stewardship/forest-analysis-inventory/data-management/standards/vegcomp_poly_rank1_data_dictionaryv5_2019.pdf
path_vri_species_key <- "data/vri_species_key.csv"

# ---------------------------------------------------------------------------- #
# British Columbia - Consolidated cutblocks
# Metadata available at: https://catalogue.data.gov.bc.ca/dataset/harvested-areas-of-bc-consolidated-cutblocks-
# Dataset UUID can be found using bcdata::bcdc_search("consolidated cutblocks")
uuid_bc_cutblocks <- "b1b647a6-f271-42e0-9cd0-89ec24bce9f7"