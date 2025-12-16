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
# NRCAN Digital Surface Model cloud optimized GeoTIFF 
# Metadata available at: https://app.geo.ca/en-ca/map-browser/record/18752265-bda3-498c-a4ba-9dfe68cb98da
url_nrcan_elevation <- "https://canelevation-dem.s3.ca-central-1.amazonaws.com/mrdem-30/mrdem-30-dsm.tif"
# ---------------------------------------------------------------------------- #
# NRCAN Canadian Vegetation Zones (CVZ) polygons
# Metadata available at: https://open.canada.ca/data/en/dataset/22b0166b-9db3-46b7-9baf-6584a3acc7b1
url_nrcan_vegetation_zones <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Ecology_Ecologie/vegetation_zones_of_canada_dwld/vegetation_zones_of_canada_2020_shp.zip"

# ---------------------------------------------------------------------------- #

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
# British Columbia - RESULTS silvicultural openings
# Metadata available at: https://catalogue.data.gov.bc.ca/dataset/results-openings-svw
# Table of codes for attributes available at: https://www.for.gov.bc.ca/his/results/webhelp/index.htm#t=RESULTS%2FCode_Table%2Fcode_tables.htm
# Dataset UUID can be found using bcdata::bcdc_search("results-openings-svw")
uuid_results_openings <- "53a17fec-e9ad-4ac0-95e6-f5106a97e677"

# ---------------------------------------------------------------------------- #
# British Columbia - RESULTS planting activities
# Metadata available at: https://catalogue.data.gov.bc.ca/dataset/results-planting
# Table of codes for attributes available at: https://www.for.gov.bc.ca/his/results/webhelp/index.htm#t=RESULTS%2FCode_Table%2Fcode_tables.htm
# Dataset UUID can be found using bcdata::bcdc_search("results-planting")
uuid_results_plantings <- "3666c26a-32d8-43e4-b8ad-59a315c7d3ce"

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
# Canadian Council of Forest Minister's forest tenure maps - 2017 and 2020
# Metadata available at: https://open.canada.ca/data/en/dataset/d8fa9a38-c4df-442a-8319-9bbcbdc29060
url_ccfm_forest_tenure_2017 <- "https://ca.nfis.org/fss/fss?command=retrieveById&fss_id=xVtr6R4NLbwZtA7Q-Xt4Tg"
url_ccfm_forest_tenure_2020 <- "https://ca.nfis.org/fss/fss?command=retrieveById&fss_id=Anh-adILivU0opBCuud2oA"

# ---------------------------------------------------------------------------- #
# Burn sample points
# Copy of local `burn_sample_points` target sf object uploaded to GEE assets 
gee_assetid_sample_points <- "projects/ee-bc-burn-severity/assets/burn_sample_points"
