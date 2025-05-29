// LOAD AND FILTER THE INPUT LAYER ----------------------------------------------->

// Retrieve the last date from the SPEI dataset.
var dataset = ee.ImageCollection("CSIC/SPEI/2_10").
  filterDate('2022-12-01', '2023-01-01');

// Select the 24-month analysis.
var layer = dataset.select('SPEI_24_month');


// EXPORT TO FILE ---------------------------------------------------------------->

// Create a bounding box of Malawi that will be used to crop the export
var malawi_bbox = ee.Geometry.Polygon([[
  [32.6737498729305, -17.1254163833137], 
  [35.9154165266305, -17.1254163833137], 
  [35.9154165266305, -9.36708308101371], 
  [32.6737498729305, -9.36708308101371]
]]);


// You will need to complete the export under the "tasks" tab to the right
// Once completed, the downloaded .tif file will show up in your Google Drive
//   under a new "My GEE Exports" folder
Export.image.toDrive({
  image: layer.first(), // .first() method converts from ImageCollection to Image
  description: 'SPEI_24-month_precipitation',
  folder: 'My GEE Exports',
  fileNamePrefix: 'SPEI_clipped',
  region: malawi_bbox, // Export specifically in Malawi
  scale: 927.6624, // Resolution in meters per pixel
  crs: 'EPSG:4326', // Coordinate reference system (unprojected lat-long)
  fileFormat: 'GeoTIFF'
});


// VISUALIZATION (this section is not needed to download the raster) ------------->

// Set the visualization ranges and color palette.
var visParams = {
  min: -2.33,
  max:  2.33,
  palette: [
    '8b1a1a', 'de2929', 'f3641d',
    'fdc404', '9afa94', '03f2fd',
    '12adf3', '1771de', '00008b',
  ]
};

// Set the map center to Malawi's location.
Map.setCenter(34.3015, -13.2543, 4);

// Display the SPEI 24-month layer.
Map.addLayer(layer, visParams, 'SPEI 24 month');
