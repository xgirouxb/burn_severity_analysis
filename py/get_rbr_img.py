import ee

# Helper: Scale optical band Digital Numbers (16-bit integers) to [0-1] range
def scale_factor(ls_img):
    optical_bands = ls_img.select(["SR_B.*"]).multiply(0.0000275).add(-0.2)
    return ls_img.addBands(optical_bands, None, True)

# Helper: Compute Normalized Burn Ratio (NBR) for any Landsat SR collections
def compute_nbr(ls_img):
    spacecraft_id = ee.String(ls_img.get('SPACECRAFT_ID'))
    nir_band = ee.Algorithms.If(
        spacecraft_id.match('LANDSAT_8|LANDSAT_9'),
        'SR_B5',   # NIR band for LS8/9 
        'SR_B4'    # NIR band for LS4/5/7
    )
    nbr = ls_img.normalizedDifference([nir_band, 'SR_B7']).toFloat()
    qa = ls_img.select(['QA_PIXEL'])
    return (
        nbr.addBands([qa])
           .select([0, 1], ['nbr', 'QA_PIXEL'])
           .copyProperties(ls_img, ['system:time_start'])
    )

# Helper: Mask clear pixels in Landsat surface reflectance images
def mask_clear_pixels(ls_img):
    
    # Bits 3,4,5,7: cloud, cloud shadow, snow, water respectively
    cloud = (1 << 3)
    cloud_shadow = (1 << 4)
    snow = (1 << 5)
    water = (1 << 7)

    # Get the pixel QA band
    qa = ls_img.select('QA_PIXEL')

    # Flags should be set to zero, indicating clear conditions
    clear = (
        qa.bitwiseAnd(cloud).eq(0)
          .And(qa.bitwiseAnd(cloud_shadow).eq(0))
          .And(qa.bitwiseAnd(snow).eq(0))
          .And(qa.bitwiseAnd(water).eq(0))
    )

    return (
        ls_img.updateMask(clear)
              .select([0])
              .copyProperties(ls_img, ["system:time_start"])
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
    ls9 = ls9_sr.map(scale_factor).map(compute_nbr).map(mask_clear_pixels)
    ls8 = ls8_sr.map(scale_factor).map(compute_nbr).map(mask_clear_pixels)
    ls7 = ls7_sr.map(scale_factor).map(compute_nbr).map(mask_clear_pixels)
    ls5 = ls5_sr.map(scale_factor).map(compute_nbr).map(mask_clear_pixels)
    ls4 = ls4_sr.map(scale_factor).map(compute_nbr).map(mask_clear_pixels)
    
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
    pre_post_fire_nbr = pre_fire_nbr.addBands(post_fire_nbr)
    
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
