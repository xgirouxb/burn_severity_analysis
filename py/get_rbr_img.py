import ee

# Helper: Scale optical band Digital Numbers (16-bit integers) to surface
#         reflectance using USGS scale factor and offset
def scale_factor(ls_img):
    optical_bands = ls_img.select(["SR_B.*"]).multiply(0.0000275).add(-0.2)
    return ls_img.addBands(optical_bands, None, True)

# Helper: Mask out bad pixels in Landsat Collection 2 Level-2 Tier 1
#         Landsat 4, 5, 7, 8, and 9 Surface Reflectance
def mask_clear_pixels(ls_img):
    
    # Get the pixel QA band
    qa = ls_img.select("QA_PIXEL")
    
    # Get the stored QA bits
    # see https://www.usgs.gov/landsat-missions/landsat-collection-2-quality-assessment-bands
    fill = 1 << 0
    dilated_cloud = 1 << 1
    cirrus = 1 << 2 # (L8/9; unused for L4/5/7)
    cloud = 1 << 3
    cloud_shadow = 1 << 4
    snow = 1 << 5
    water = 1 << 7

    # Retain only clear, snow-free land pixels
    clear_land_pixels = (
        qa.bitwiseAnd(fill).eq(0)
          .And(qa.bitwiseAnd(dilated_cloud).eq(0))
          .And(qa.bitwiseAnd(cirrus).eq(0))
          .And(qa.bitwiseAnd(cloud).eq(0))
          .And(qa.bitwiseAnd(cloud_shadow).eq(0))
          .And(qa.bitwiseAnd(snow).eq(0))
          .And(qa.bitwiseAnd(water).eq(0))
    )

    return (
        ls_img.updateMask(clear_land_pixels)
              .copyProperties(ls_img, ["system:time_start"])
    )

# Helper: Mask pixels where either band used to calculate NBR is saturated
def mask_nbr_saturation(ls_img):

    # Get radiometric saturation QA band
    radsat = ls_img.select("QA_RADSAT")

    # Identify sensor
    spacecraft_id = ee.String(ls_img.get("SPACECRAFT_ID"))

    # NIR saturation bit:
    #   Landsat 8/9: Band 5 -> bit 4
    #   Landsat 4/5/7: Band 4 -> bit 3
    nir_saturation_bit = ee.Number(
        ee.Algorithms.If(
            spacecraft_id.match("LANDSAT_8|LANDSAT_9"),
            1 << 4,
            1 << 3
        )
    )

    # SWIR2 saturation bit:
    #   Band 7 -> bit 6 for all sensors
    swir2_saturation_bit = 1 << 6

    # Retain pixels where both NIR and SWIR2 are unsaturated
    nbr_bands_unsaturated = (
        radsat.bitwiseAnd(nir_saturation_bit).eq(0)
              .And(radsat.bitwiseAnd(swir2_saturation_bit).eq(0))
    )

    return (
        ls_img.updateMask(nbr_bands_unsaturated)
              .copyProperties(ls_img, ["system:time_start"])
    )


# Helper: Mask terrain-occluded pixels in Landsat 8 and 9
#         QA_RADSAT bit 11 = terrain occlusion
def mask_terrain_occlusion(ls_img):
    radsat = ls_img.select("QA_RADSAT")
    terrain_clear = radsat.bitwiseAnd(1 << 11).eq(0)
    return (
        ls_img.updateMask(terrain_clear)
              .copyProperties(ls_img, ["system:time_start"])
    )

# Helper: Compute Normalized Burn Ratio (NBR) for any Landsat SR collections
def compute_nbr(ls_img):
    spacecraft_id = ee.String(ls_img.get('SPACECRAFT_ID'))
    nir_band = ee.Algorithms.If(
        spacecraft_id.match('LANDSAT_8|LANDSAT_9'),
        'SR_B5',   # NIR band for LS8/9 
        'SR_B4'    # NIR band for LS4/5/7
    )
    nbr = ls_img.normalizedDifference([nir_band, 'SR_B7']).toFloat()
    return (
        nbr.rename('nbr')
           .copyProperties(ls_img, ['system:time_start'])
    )

# Main function
def get_rbr_img(fire_polygon, start_day = 152, end_day = 245):
    
    """
    Computes a Relativized Burn Ratio (RBR) image for a fire polygon using
    Landsat Tier 1 Collection 2 Surface Reflectance imagery and exports the
    result to Google Drive. The functions implemented here are derived from 
    work by Parks et al. 2018 (doi.org/10.3390/rs10060879) implemented in Earth
    Engine JavaScript API and shared here:
    https://code.earthengine.google.com/57f962c63a0c9d5c6c559c53497a72df

    Parameters:
        fire_polygon (ee.Feature): Fire polygon feature containing
                                   'fire_year' and 'fire_id' properties.
        start_day (int): Start day-of-year used to filter Landsat imagery.
                         Default is 152.
        end_day (int): End day-of-year used to filter Landsat imagery.
                       Default is 245.

    Returns:
        ee.batch.Task: The Earth Engine export task object.
    """
    
    # Landsat 5, 7, 8, and 9 Surface Reflectance (Level 2) Tier 1 Collection 2 
    ls9_sr = ee.ImageCollection('LANDSAT/LC09/C02/T1_L2')
    ls8_sr = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
    ls7_sr = ee.ImageCollection('LANDSAT/LE07/C02/T1_L2')
    ls5_sr = ee.ImageCollection('LANDSAT/LT05/C02/T1_L2')
    ls4_sr = ee.ImageCollection('LANDSAT/LT04/C02/T1_L2')
    
    # Compute Normalized Burn Ratio for each collection and mask out clouds
    ls9 = (ls9_sr.map(scale_factor)
                 .map(mask_clear_pixels)
                 .map(mask_nbr_saturation)
                 .map(mask_terrain_occlusion)
                 .map(compute_nbr))
    ls8 = (ls8_sr.map(scale_factor)
                 .map(mask_clear_pixels)
                 .map(mask_nbr_saturation)
                 .map(mask_terrain_occlusion)
                 .map(compute_nbr))
    ls7 = (ls7_sr.map(scale_factor)
                 .map(mask_clear_pixels)
                 .map(mask_nbr_saturation)
                 .map(compute_nbr))
    ls5 = (ls5_sr.map(scale_factor)
                 .map(mask_clear_pixels)
                 .map(mask_nbr_saturation)
                 .map(compute_nbr))
    ls4 = (ls4_sr.map(scale_factor)
                 .map(mask_clear_pixels)
                 .map(mask_nbr_saturation)
                 .map(compute_nbr))
    
    # Merge collections
    ls_col = ee.ImageCollection(ls9.merge(ls8).merge(ls7).merge(ls5).merge(ls4))
    
    # Get year before and year after study fire
    fire_year = ee.Number(fire_polygon.get('fire_year')).toInt()
    year_before = fire_year.subtract(1)
    year_after = fire_year.add(1)
    
    # Create and combine pre/post fire mean NBR composite image
    pre_fire_nbr = (
        ls_col.filterBounds(fire_polygon.geometry())
          .filter(ee.Filter.calendarRange(year_before, year_before, 'year'))
          .filter(ee.Filter.dayOfYear(start_day, end_day))
          .mean().rename('pre_nbr')
    )
    post_fire_nbr = (
        ls_col.filterBounds(fire_polygon.geometry())
          .filter(ee.Filter.calendarRange(year_after, year_after, 'year'))
          .filter(ee.Filter.dayOfYear(start_day, end_day))
          .mean().rename('post_nbr')
    )
    
    # Calculate Relativized Burn Ratio
    rbr = (
        pre_fire_nbr.subtract(post_fire_nbr).multiply(1000)
                    .divide(pre_fire_nbr.add(1.001))
                    .rename('rbr').toFloat()
    )
    
    # Export image to Google Drive
    task = ee.batch.Export.image.toDrive(
        image=rbr.select(['rbr']),
        description='rbr_' + str(fire_polygon.get('fire_id').getInfo()),
        folder='ee_bc_burn_severity',
        region=fire_polygon.buffer(3000).geometry(),
        crs='EPSG:3005',
        scale=30,
        maxPixels=1e13
    )
    task.start()

    return task
