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
# British Columbia - Historical Wildfire Fire Perimeters 
# Metadata available at: https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-perimeters-historical
# Dataset UUID can be found using bcdata::bcdc_search("bc-wildfire-fire-perimeters-historical")
uuid_bc_historical_fires <- "22c7cb44-1463-48f7-8e47-88857f207702"

# ---------------------------------------------------------------------------- #
# pre-CanLaD (1965-1984) - NRCAN Canada Landsat Disturbances backcast, version 1
# Metadata available at: 
url_precanlad_1965_1984 <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada_disturbances_1965to1984/v1/"
 
# ---------------------------------------------------------------------------- #
# CanLaD (1985-2024) - NRCAN Canada Landsat Disturbances, version 1
# Metadata available at:
url_canlad_1985_2024 <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canlad_including_insect_defoliation/v1/Disturbances_Time_Series/"

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

# ---------------------------------------------------------------------------- #
# Canada high-resolution annual forest land cover
# Metadata available at: https://gee-community-catalog.org/projects/ca_lc/
# Publication for methodology: https://doi.org/10.1016/j.rse.2021.112780
gee_assetid_land_cover <- "projects/sat-io/open-datasets/CA_FOREST_LC_VLCE2"

# ---------------------------------------------------------------------------- #
# Burn sample points
# Copy of local `burn_sample_points` target sf object uploaded to GEE assets 
gee_assetid_sample_points <- "projects/ee-bc-burn-severity/assets/burn_sample_points"
