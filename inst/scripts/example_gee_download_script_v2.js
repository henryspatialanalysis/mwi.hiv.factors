// Load dataset
var dataset = ee.Image('OpenLandMap/CLM/CLM_PRECIPITATION_SM2RAIN_M/v01');

// AGGREGATE ACROSS BANDS (not always necessary)
var mean_precipitation = dataset.reduce(ee.Reducer.mean());

// DESCRIBE OUTPUT LAYER
print(mean_precipitation);

// VISUALIZE OUTPUT LAYER
var visualization = {
  min: 0.0,
  max: 200,
  palette: ['ecffbd', 'ffff00', '3af6ff', '467aff', '313eff', '0008ff']
};
Map.centerObject(dataset);
Map.addLayer(mean_precipitation, visualization, 'Precipitation monthly in mm');

// SAVE OUTPUT LAYER
// Create a bounding box of Malawi that will be used to crop the export
var malawi_bbox = ee.Geometry.Polygon([[
  [32.6737498729305, -17.1254163833137],
  [35.9154165266305, -17.1254163833137],
  [35.9154165266305, -9.36708308101371],
  [32.6737498729305, -9.36708308101371]
]]);
Export.image.toDrive({
  image: mean_precipitation,
  description: 'OpenLandMap_precipitation',
  folder: 'My GEE Exports',
  fileNamePrefix: 'OLM_precipitation_clipped',
  region: malawi_bbox, // Export specifically in Malawi
  scale: 1000, // Resolution in meters per pixel
  crs: 'EPSG:4326', // Coordinate reference system (unprojected lat-long)
  fileFormat: 'GeoTIFF'
});
