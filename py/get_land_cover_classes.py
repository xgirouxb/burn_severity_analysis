import ee
ee.Initialize()

# Helper: Add year property to each image
def add_year_prop(img):
    year = ee.Date(img.get('system:time_start')).get('year')
    return img.set('year', year)

# Helper: Remap raw land cover class values into grouped categories
def remap_lc_classes(img):
    original_classes = [0, 20, 31, 32, 33, 40, 50, 80, 81, 100, 210, 220, 230]
    remapped_classes = [0,  0,  0,  0,  0,  1,  1,  2,  2,   1,   3,   4,   4]
    lc_reclass = img.remap(original_classes, remapped_classes).rename('lc')
    return img.addBands(lc_reclass)

# Helper: Convert land cover into binary masks per class
def lc_binary_bands(img):
    nonfuel_lc = img.select('lc').eq(0).rename('nonfuel')
    open_lc = img.select('lc').eq(1).rename('open')
    wetland_lc = img.select('lc').eq(2).rename('wetland')
    coniferous_lc = img.select('lc').eq(3).rename('coniferous')
    deciduous_lc = img.select('lc').eq(4).rename('deciduous')

    return img.addBands([
        nonfuel_lc, open_lc, wetland_lc, coniferous_lc, deciduous_lc
    ])

# Main sampling function
def sample_lc_classes(sample_pts, radius_list=ee.List([1000])):
    """
    Samples landcover around a fire point and exports results to Drive.
    
    Parameters:
        sample_pts (ee.FeatureCollection): Points with 'fire_year' property.
        radius_list (ee.List): List containing radius or radii in meters for
                               buffer(s) used to compute proportions around
                               each sample point.
    
    Returns:
        ee.batch.Task: The export task object.
    """

    # Decode original land cover class codes
    lc_dict = ee.Dictionary({
        0:   'unknown',
        20:  'water',
        31:  'snow_ice',
        32:  'rock_rubble',
        33:  'exposed_barren',
        40:  'bryoid',
        50:  'shrubs',
        80:  'wetland',
        81:  'wetland_treed',
        100: 'herbs',
        210: 'coniferous',
        220: 'broadleaf',
        230: 'mixedwood'
    })

    # Prepare land cover image collection
    forest_lc = (
      ee.ImageCollection("projects/sat-io/open-datasets/CA_FOREST_LC_VLCE2")
        .map(add_year_prop)
        .map(remap_lc_classes)
        .map(lc_binary_bands)
    )
    
    # Sampling function for each point
    def sample_lc_img(sample_pt):

        # Get lc img for year before sample fire_year
        lc_year = ee.Number(sample_pt.get('fire_year')).subtract(1)
        lc_img = ee.Image(forest_lc.filter(ee.Filter.eq('year', lc_year)).first())

        # Get raw land cover class at the point
        lc_pt = lc_img.select('b1').reduceRegion(
            reducer=ee.Reducer.first(),
            geometry=sample_pt.geometry(),
            scale=30,
            maxPixels=1e13
        ).get('b1')
        
        # Get corresponding landcover string
        lc_pt_str = ee.String(lc_dict.get(lc_pt))

        # Helper function to generate landscape proportion variables keys (name) 
        def make_key(prefix, radius):
            return ee.String(prefix).cat(ee.Number(radius).format()).cat('m')

        # Calculate land cover proportions for a given radius
        def lc_props_in_radius(radius):
          
            # Calculate land cover class proportions in buffer
            lc_area = (
                lc_img
                .addBands(ee.Image(1).rename('constant'))
                .select(['constant', 'nonfuel', 'open', 'wetland', 'coniferous', 'deciduous'])
                .multiply(ee.Image.pixelArea())
                .reduceRegion(
                    reducer=ee.Reducer.sum(),
                    geometry=sample_pt.geometry().buffer(ee.Number(radius)),
                    scale=30,
                    maxPixels=1e13
                )
            )
            
            # Extract total pixel area in buffer
            t_area = ee.Number(lc_area.get('constant'))

            return ee.List([
                [make_key('prop_nonfuel_', radius), ee.Number(lc_area.get('nonfuel')).divide(t_area)],
                [make_key('prop_open_', radius), ee.Number(lc_area.get('open')).divide(t_area)],
                [make_key('prop_wetland_', radius), ee.Number(lc_area.get('wetland')).divide(t_area)],
                [make_key('prop_coniferous_', radius), ee.Number(lc_area.get('coniferous')).divide(t_area)],
                [make_key('prop_deciduous_', radius), ee.Number(lc_area.get('deciduous')).divide(t_area)],
            ])
            
        # Map over list of radii, outputs list of lists of key-value pairs
        key_value_lists = radius_list.map(lc_props_in_radius)
    
        # Flatten to single list of key-value pairs, flatten each key-value pair
        key_value_list = key_value_lists.flatten().flatten()
        
        # Convert to dictionary, add landcover class property
        key_value_dict = ee.Dictionary(key_value_list).set('landcover', lc_pt_str)
        
        return sample_pt.set(key_value_dict)
    
    # Map sampling function over burn sample points points
    sampled_lc = sample_pts.map(sample_lc_img)
    
    # Get all property names from the first sample, fetch from EE servers
    property_names = ee.Feature(sampled_lc.first()).propertyNames().getInfo()
    
    # Convert to python list, but remove 'system:index' and '.geo'
    property_names = [p for p in property_names if p not in ('system:index', '.geo')]

    # Export to Google Drive
    task = ee.batch.Export.table.toDrive(
        collection=sampled_lc,
        description='landcover_proportion_samples',
        folder='ee_bc_burn_severity',
        fileFormat='CSV',
        selectors = property_names
    )
    task.start()

    return task
