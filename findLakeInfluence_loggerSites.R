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

execGRASS("v.patch",input=c("lakesOK","streams_vect"),output="lakeStreamVect",flags="overwrite")
execGRASS("v.clean",input="lakeStreamVect",output="junk",tool="break",flags="overwrite",error="lakeStreamIntersect")
execGRASS("v.db.addtable",map="lakeStreamIntersect")
execGRASS("v.category",input="lakeStreamIntersect",option="add",output="lakePts")
execGRASS("v.to.db",map="lakePts",type="point",option="cat",columns="cat")
execGRASS("v.db.addcolumn",map="lakePts",columns="lakeCat integer")
execGRASS("v.distance",from="lakePts",to="lakesOK",upload="cat",column="lakeCat")

execGRASS("v.sample",input="lakePts",column="cat",output="lakePts_elev",raster="dem@PERMANENT",flags="overwrite")
execGRASS("v.db.join",map="lakePts",column="cat",other_table="lakePts_elev",other_column="cat",subset_columns="rast_val")
execGRASS("v.db.renamecolumn",map="lakePts",column="rast_val,elevation")

execGRASS("v.sample",input="lakePts",column="cat",output="lakePts_uaa",raster="uaa",flags="overwrite")
execGRASS("v.db.join",map="lakePts",column="cat",other_table="lakePts_uaa",other_column="cat",subset_columns="rast_val")
execGRASS("v.db.renamecolumn",map="lakePts",column="rast_val,uaa")


execGRASS("v.db.addcolumn",map="lakePts",columns="x double")
execGRASS("v.db.addcolumn",map="lakePts",columns="y double")
execGRASS("v.to.db",map="lakePts",option="coor",columns=c("x","y"))

lakeData_redundant=readVECT(vname="lakePts")@data

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


execGRASS("v.in.ogr",input=targetPointsPath,output="targetPoints",flags="o")
execGRASS("r.stream.snap",input="targetPoints",output="targetPoints_snap",stream_rast="streams",accumulation="uaa",threshold=10000,radius=25)

targetPoints=readVECT("targetPoints")@data

whichLakesAreLoIs=function(allLakes){
  allLakes=allLakes[order(allLakes$uaa,decreasing=T),]
  LoIs=allLakes[1,]
  allLakes=allLakes[-1,]
  execGRASS("r.water.outlet",input="flowDir",output="aboveLoI",coordinates=c(LoIs$x[1],LoIs$y[1]),flags=c("overwrite","quiet"))
  execGRASS("r.to.vect",input="aboveLoI",output="aboveLoIVect",type="area",flags=c("overwrite","quiet"))
  execGRASS("v.select",ainput="lakePts",binput="aboveLoIvect",operator="within",output="notLoIs",flags=c("overwrite","quiet"))
  notLoIs=unique(readVECT(vname="notLoIs")@data$lakeCat)
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
  execGRASS("r.water.outlet",input="flowDir",output="aboveTarget",coordinates=c(thisTarget$xcoord,thisTarget$ycoord),flags="overwrite")
  execGRASS("r.to.vect",input="aboveTarget",output="aboveTargetVect",type="area",flags="overwrite")
  execGRASS("v.select",ainput="lakePts",binput="aboveTargetVect",operator="within",output="upLakePoints",flags="overwrite")
  upLakePoints=unique(readVECT(vname="upLakePoints")@data$lakeCat)
  theseLakes=lakeData[lakeData$lakeCat %in%  unique(readVECT(vname="upLakePoints")@data$lakeCat),]
  theseLoIs=whichLakesAreLoIs(theseLakes)
  if(length(theseLoIs$lakeCat!=0)){
  targetPoints[i,"LF"]=sum(theseLoIs$uaa)/targetPoints[i,"UAA"]
  targetPoints[i,"meanLakeElev"]=stats::weighted.mean(x=theseLoIs$elevation,w=theseLoIs$uaa)
  } else {
    targetPoints[i,"LF"]=0
    targetPoints[i,"meanLakeElev"=NA]
  }
}

