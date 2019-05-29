#Code written by:
#Chris Garrard
#Research Associate
#Remote Sensing / GIS Lab

#Office: Janet Quinney Lawson Building 232
#Phone: 435-797-2602
#Email: chris.garrard@nospam.usu.edu (Just get rid of the "nospam." after the @!)
#http://www.gis.usu.edu/~chrisg/

#Mailing address:
#5275 Old Main Hill
#Utah State University
#Logan, UT 84322-5275


#Individual dSSURGO tiles can be downloaded from stream.princeton.edu/dSSURGO


import os
from osgeo import gdal
import csv
import subprocess
from osgeo.gdalconst import *


#set working directory and environmental variables
os.chdir(r'D:/dSSURGO')
att_fn = r'D:/dSSURGO/mapping.txt'
sr = "GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,298.257223563,AUTHORITY[\"EPSG\",\"7030\"]],AUTHORITY[\"EPSG\",\"6326\"]],PRIMEM[\"Greenwich\",0],UNIT[\"degree\",0.0174532925199433],AUTHORITY[\"EPSG\",\"4326\"]]"


gt = [-110.0, 2.777777777778E-4, 0.0, 37.0, 0.0, 2.777777777778E-4] #Change the 1st and 4th values to the lower right corner of the lat/long tile (found in the file name).

# Get the attribute values from the mapping file and put the values in one list
# and attributes in another.
values = []
atts = []
with open(att_fn, 'rb') as fp:
    reader = csv.reader(fp, delimiter = ' ', quoting=csv.QUOTE_NONNUMERIC)
    for row in reader:
        values.append(row[0])
        atts.append(row[1])

#register drivers
gdal.AllRegister()
driver = gdal.GetDriverByName('gtiff')

ds = gdal.Open("lat3738_lon-110-109.nc") #Change this for each .nc file to extract
subdatasets = ds.GetSubDatasets()
for i in range(6):
    sd = subdatasets[i][0]
    sd_parts = sd.split(':')
    out_name = "{}-{}.tif".format(sd_parts[1].strip('"').replace('.nc', ''),
                                  sd_parts[2].strip("/"))
    if 'class' in sd:
        dt = 'Int16'
        add_atts = True
    else:
        dt = 'Float32'
        add_atts = False
    data = ["C:\Program Files\GDAL\gdal_translate.exe", '-ot', dt, '-of', 'gtiff',
            sd, out_name]
    subprocess.call(data)

    # Add the projection and geotransform info
    tmp_ds = gdal.Open(out_name, GA_Update)
    tmp_ds.SetProjection(sr)
    tmp_ds.SetGeoTransform(gt)

    if add_atts == True:
        # Create a RAT with 2 columns and as many rows as we read in from the mapping
        # file.
        rat = gdal.RasterAttributeTable()
        rat.CreateColumn('VALUE', GFT_Integer, GFU_Name)
        rat.CreateColumn('DESCRIPTION', GFT_String, GFU_Generic)
        rat.SetRowCount(len(values))

        # Put the values and attributes into the table.
        rat.WriteArray(values, 0)
        rat.WriteArray(atts, 1)

        # Add the RAT to the raster file.
        band = tmp_ds.GetRasterBand(1)
        band.SetDefaultRAT(rat)

    # This got moved out of the if-statement because now we always opened tmp_ds so we
    # could add the projection and geotransform info, even if we weren't adding a RAT.
    del tmp_ds
