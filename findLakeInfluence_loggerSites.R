library(rgdal)
library(rgrass7)
library(raster)
library(sp)
source("~/R/projects/findLakeInfluence/grassTools.R")
#"C:/Users/sam/Documents/spatial/r_workspaces/findLakeInfluence/loggerSites/Inputs
targetPointsPath="C:/Users/sam/Documents/spatial/r_workspaces/findLakeInfluence/loggerSites/Inputs/NSV_GC_Loggers.shp"
lakeDefinitionPath="C:/Users/sam/Documents/spatial/r_workspaces/findLakeInfluence/loggerSites/Inputs/lakes_manual_1km2.shp"
minBasinSize=5000 #set this small to include all upstream lakes
minLakeArea=10000

InitGrass_byRaster(rasterPath = "C:/Users/sam/Documents/spatial/r_workspaces/findLakeInfluence/loggerSites/Inputs/BigDemWGS84.tif")
execGRASS("r.watershed",elevation="dem@PERMANENT",threshold=minBasinSize,accumulation="uaa",drainage="flowDir",stream="streams",flags="a")
execGRASS("r.thin",input="streams",output="streams",flags="overwrite")
execGRASS("r.to.vect",input="streams",output="streams_vect",type="line")

execGRASS("v.in.ogr",input=lakeDefinitionPath,output="lakeAreas",flags="o")
execGRASS("v.extract",input="lakeAreas",where=paste0("AREA > ",minLakeArea),output="lakesOK")
execGRASS("v.centroids",input="lakesOK",output="lakeCentroids")
execGRASS("v.to.points",input="lakesOK",output="lakePoints",flags="overwrite")
execGRASS("r.stream.snap",input="lakePoints",output="lakePoints_snap",stream_rast="streams",radius=30,flags="overwrite")

execGRASS("v.in.ogr",input=targetPointsPath,output="targetPoints",flags="o")
targetPoints=readVECT("targetPoints")@data
for(i in 1:nrow(targetPoints)){
  thisTarget=targetPoints[i,]
  execGRASS("r.water.outlet",input="flowDir",output="aboveTarget",coordinates=c(thisTarget$xcoord,thisTarget$ycoord),flags="overwrite")
  execGRASS("r.to.vect",input="aboveTarget",output="aboveTargetVect",type="area",flags="overwrite")
  execGRASS("v.select",ainput="lakePoints_snap",binput="aboveTargetVect",operator="within",output="upLakePoints",flags="overwrite")

  }