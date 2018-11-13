library(rgdal)
library(rgrass7)
library(raster)
library(sp)
source("~/R/projects/findLakeInfluence/grassTools.R")
#"C:/Users/sam/Documents/spatial/r_workspaces/findLakeInfluence/loggerSites/Inputs
targetPointsPath="C:/Users/sam/Documents/spatial/r_workspaces/findLakeInfluence/loggerSites/Inputs/NSV_GC_Loggers.shp"
lakeDefinitionPath="C:/Users/sam/Documents/spatial/r_workspaces/findLakeInfluence/loggerSites/Inputs/lakes_manual_1km2.shp"
minBasinSize=500 #set this small to include all upstream lakes
minLakeArea=10000

InitGrass_byRaster(rasterPath = "C:/Users/sam/Documents/spatial/r_workspaces/findLakeInfluence/loggerSites/Inputs/BigDemWGS84.tif")
execGRASS("r.watershed",elevation="dem@PERMANENT",threshold=minBasinSize,accumulation="uaa",drainage="flowDir",stream="streams",flags=c("a","quiet"))
execGRASS("r.thin",input="streams",output="streams",flags=c("overwrite","quiet"))
execGRASS("r.to.vect",input="streams",output="streams_vect",type="line",flags=c("overwrite","quiet"))
dir.create("C:/Users/sam/Documents/spatial/r_workspaces/findLakeInfluence/loggerSites/outputs/")
execGRASS("v.out.ogr",input="streams_vect"
          ,output="C:/Users/sam/Documents/spatial/r_workspaces/findLakeInfluence/loggerSites/outputs/streams_vect.shp",
          format="ESRI_Shapefile",flags=c("overwrite","quiet"))

execGRASS("v.in.ogr",input=lakeDefinitionPath,output="lakeAreas",flags=c("o","quiet"))
execGRASS("v.extract",input="lakeAreas",where=paste0("AREA > ",minLakeArea),output="lakesOK",flags=c("overwrite","quiet"))

execGRASS("v.patch",input=c("lakesOK","streams_vect"),output="lakeStreamVect",flags=c("overwrite","quiet"))
execGRASS("v.clean",input="lakeStreamVect",output="junk",tool="break",error="lakeStreamIntersect",flags=c("overwrite","quiet"))
execGRASS("v.db.addtable",map="lakeStreamIntersect",flags="quiet")
execGRASS("v.category",input="lakeStreamIntersect",option="add",output="lakePts",flags=c("overwrite","quiet"))
execGRASS("v.to.db",map="lakePts",type="point",option="cat",columns="cat",flags="quiet")
execGRASS("v.db.addcolumn",map="lakePts",columns="lakeCat integer",flags="quiet")
execGRASS("v.distance",from="lakePts",to="lakesOK",upload="cat",column="lakeCat",flags="quiet")

execGRASS("v.sample",input="lakePts",column="cat",output="lakePts_elev",raster="dem@PERMANENT",flags=c("overwrite","quiet"))
execGRASS("v.db.join",map="lakePts",column="cat",other_table="lakePts_elev",other_column="cat",subset_columns="rast_val",flags="quiet")
execGRASS("v.db.renamecolumn",map="lakePts",column="rast_val,elevation",flags="quiet")
execGRASS("v.out.ogr",input="lakePts",
          output="C:/Users/sam/Documents/spatial/r_workspaces/findLakeInfluence/loggerSites/outputs/lakePts.shp",
          format="ESRI_Shapefile",flags=c("overwrite","quiet"))


execGRASS("v.sample",input="lakePts",column="cat",output="lakePts_uaa",raster="uaa",flags=c("overwrite","quiet"))
execGRASS("v.db.join",map="lakePts",column="cat",other_table="lakePts_uaa",other_column="cat",subset_columns="rast_val",flags="quiet")
execGRASS("v.db.renamecolumn",map="lakePts",column="rast_val,uaa",flags="quiet")


execGRASS("v.db.addcolumn",map="lakePts",columns="x double",flags="quiet")
execGRASS("v.db.addcolumn",map="lakePts",columns="y double",flags="quiet")
execGRASS("v.to.db",map="lakePts",option="coor",columns=c("x","y"),flags="quiet")

#lakeData_redundant=readVECT(vname="lakePts")@data
lakeData_redundant=grassTableToDF(execGRASS("v.db.select",map="lakePts",intern = T))

selectLakeOutlet = function(lakeID,redundantDF=lakeData_redundant){
  thisLakePts=redundantDF[redundantDF$lakeCat==lakeID,]
  thisLakePt=thisLakePts[thisLakePts$uaa==max(thisLakePts$uaa),][1,]
  return(thisLakePt)
}

lakeData=selectLakeOutlet(unique(lakeData_redundant$lakeCat)[1])
for(i in 2:length(unique(lakeData_redundant$lakeCat))){
  lakeData=rbind(lakeData,
                 selectLakeOutlet(unique(lakeData_redundant$lakeCat)[i]))
}


execGRASS("v.in.ogr",input=targetPointsPath,output="targetPoints",flags=c("o","quiet"))
execGRASS("r.stream.snap",input="targetPoints",output="targetPoints_snap",stream_rast="streams",accumulation="uaa",threshold=500,radius=25,flags=c("overwrite","quiet"))
#categories are preserved, just no table:
#execGRASS("v.category",input="targetPoints_snap",option="print",flags="quiet")
execGRASS("v.db.addtable",map="targetPoints_snap",flags="quiet")
execGRASS("v.db.join",map="targetPoints_snap",column="cat",other_table="targetPoints",other_column="cat",flags="quiet")
execGRASS("v.db.dropcolumn",map="targetPoints_snap",columns=c("xcoord","ycoord"),flags="quiet")
execGRASS("v.db.addcolumn",map="targetPoints_snap",columns="x double",flags="quiet")
execGRASS("v.db.addcolumn",map="targetPoints_snap",columns="y double",flags="quiet")
execGRASS("v.to.db",map="targetPoints_snap",option="coor",columns=c("x","y"),flags="quiet")
execGRASS("v.out.ogr",input="targetPoints_snap",
          output="C:/Users/sam/Documents/spatial/r_workspaces/findLakeInfluence/loggerSites/outputs/target_snap.shp",
          format="ESRI_Shapefile",flags=c("overwrite","quiet"))

targetPoints=grassTableToDF( execGRASS("v.db.select",map="targetPoints_snap",intern = T))

whichLakesAreLoIs=function(allLakes){
  allLakes=allLakes[order(allLakes$uaa,decreasing=T),]
  LoIs=allLakes[1,]
  allLakes=allLakes[-1,]
  execGRASS("r.water.outlet",input="flowDir",output="aboveLoI",coordinates=c(LoIs$x[1],LoIs$y[1]),flags=c("overwrite","quiet"))
  execGRASS("r.to.vect",input="aboveLoI",output="aboveLoIVect",type="area",flags=c("overwrite","quiet"))
  execGRASS("v.select",ainput="lakePts",binput="aboveLoIvect",operator="within",output="notLoIs",flags=c("overwrite","quiet"))
  #notLoIs=unique(readVECT(vname="notLoIs")@data$lakeCat)
  notLoIs=grassTableToDF(execGRASS("v.db.select",map="notLoIs",intern = T))$lakeCat
  possibleLoIs=allLakes[!allLakes$lakeCat %in% notLoIs,]
  if(length(possibleLoIs$lakeCat!=0)){
    LoIs=rbind(LoIs,whichLakesAreLoIs(possibleLoIs))
  }
  return(LoIs)
}

targetPoints$meanLakeElev=0
targetPoints$LF=0

for(i in 1:nrow(targetPoints)){
  thisTarget=targetPoints[i,]
  print(paste0("Processing lakes above point ",thisTarget$Descriptio))
  execGRASS("r.water.outlet",input="flowDir",output="aboveTarget",coordinates=c(thisTarget$x,thisTarget$y),flags=c("overwrite","quiet"))
  execGRASS("r.to.vect",input="aboveTarget",output="aboveTargetVect",type="area",flags=c("overwrite","quiet"))
  execGRASS("v.select",ainput="lakePts",binput="aboveTargetVect",operator="within",output="upLakePoints",flags=c("overwrite","quiet"))
  # upLakePoints=unique(readVECT(vname="upLakePoints")@data$lakeCat)
  upLakePoints=grassTableToDF(execGRASS("v.db.select",map="upLakePoints",intern = T))$lakeCat
  if(length(upLakePoints)!=0){
    theseLakes=lakeData[lakeData$lakeCat %in% upLakePoints,]
    theseLoIs=whichLakesAreLoIs(theseLakes)
    targetPoints[i,"LF"]=sum(theseLoIs$uaa)/targetPoints[i,"UAA"]
    targetPoints[i,"meanLakeElev"]=stats::weighted.mean(x=theseLoIs$elevation,w=theseLoIs$uaa)
  } else {
    print(paste0("No lakes above point",thisTarget$Descriptio))
    targetPoints[i,"LF"] = 0
    targetPoints[i,"meanLakeElev"] = NA
  }
}
write.csv(targetPoints,"NSV_GC_loggers_LoIs.csv")
