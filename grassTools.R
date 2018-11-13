#grass tools
#utility for getting attribute tables from grass to a R data frame type
grassTableToDF=function(grassDF){
  # grassNames=grassDF[1]
  # grassNames=unlist(strsplit(grassNames,'[|]'))
  # first=TRUE
  # for(c in 1:length(grassNames)){
  #   thisName=grassNames[c]
  #   thisData=sapply(sapply(grassDF[-1],strsplit,split='[|]'),'[',i=c)
  #   if(!first){
  #     rDF$tempName=thisData
  #   }
  #   if(first){
  #     rDF=data.frame(tempName=thisData,row.names = NULL, stringsAsFactors = F)
  #     first=FALSE
  #   } 
  #   names(rDF)[names(rDF)=="tempName"]=thisName
  # }
  rDF=read.table(text=grassDF,header=T,sep="|",stringsAsFactors = F)
  return(rDF)
}

#initialize a re-useable workspace named findUpstream for RMNP area w/ dem
InitGrass_byRaster=function(rasterPath="C:/Users/sam/Documents/spatial/r_workspaces/findLakeInfluence/loggerSites/Inputs/BigDemWGS84.tif",grassRasterName="dem"){
  #'read' dem into R w/ rgdal
  #This doesnt actually load the raster into memory initially, but it will if needed
  dem=readGDAL(fname=rasterPath)
  
  #I want to work in the projection of the raster - save projection definition as p4
  p4=proj4string(dem)
  
  ##these are necessary to fully trash a pervious grass session
  unlink_.gislock()
  remove_GISRC()
  
  #initialize grass
  initGRASS("C:/Program Files/GRASS GIS 7.4.1",override=TRUE,mapset="PERMANENT",remove_GISRC = T)
  #note that has no info (yet) about projection, extent, or resolution
  
  #set projection from RGDAL's interpetation of raster proj4string
  execGRASS("g.proj",proj4=p4, flags="c")
  
  #import raster from file, and set extent based on raster.  
  #This DOES NOT set the resolution, why I do not know
  execGRASS("r.in.gdal",input=rasterPath,output=grassRasterName, flags=c("e","o"))
  
  #this sets the resolution
  execGRASS("g.region",raster=grassRasterName)
  
  #we can see the region as lat-long or xy
  execGRASS("g.region", flags='l')
  execGRASS("g.region", flags='p')
  
  #set current region as default before creating new mapset
  execGRASS("g.region",flags='s')
  
  #set up new mapset as workspace, leaving only the dem in PERMANENT mapset
  execGRASS("g.mapset",mapset="wkspace",flags="c")
  
  #this is kinda pointless here as i am just going to trash the whole grass session and rebuild it every time i need it
  #however, mapsets are like folders which can be used to organize spatial objects
  #mapsets can have different extents and resolutions, but I believe they must have the same projection
  
}

getNearby=function(inXY,dataTypes = '%', range=1000){
  xyData=dbGetQuery(leakyDB,paste0("SELECT Points.PointIDX, Data.DataIDX, Data.DataTypeIDX, Coordinates.X, Coordinates.Y, Data.Value FROM
                                   Coordinates LEFT JOIN 
                                   ( Reaches LEFT JOIN ( Data LEFT JOIN Points ON Data.PointIDX = Points.PointIDX ) ON Reaches.ReachIDX = Points.ReachIDX )
                                   ON Coordinates.CoordinateIDX = Reaches.CoordinateIDX
                                   WHERE Data.DataTypeIDX LIKE '",dataTypes,"'"))
  xyData$DistFromInXY=dist(rbind(inXY[c("X","Y")],xyData[,c("X","Y")]))[1:nrow(xyData)]
  xyData=xyData[xyData$DistFromInXY <= range,]
  return(xyData)
}

getUpstream=function(targetPointIDX, dataTypes='%', range=1000, upOnly=T){
  
  #get all points within range
  target_xy=
    dbGetQuery(leakyDB,paste0("SELECT Points.PointIDX, Reaches.ReachName, Coordinates.CoordinateTypeIDX, Coordinates.CoordinateIDX, Coordinates.X, Coordinates.Y, Coordinates.EPSG FROM Points LEFT JOIN (
                              Reaches LEFT JOIN Coordinates ON Reaches.CoordinateIDX = Coordinates.CoordinateIDX
    ) ON Points.ReachIDX = Reaches.ReachIDX WHERE PointIDX = '",targetPointIDX,"'"))
  
  #If the target is a reach, prefer the downstream point, then midpoint, then upstream point, then outflow point
  target_xy$CoordinateTypeIDX=factor(target_xy$CoordinateTypeIDX, levels = c(4,2,3,1))
  target_xy=target_xy[order(target_xy$CoordinateTypeIDX),][1,]
  target_xy$X=as.numeric(target_xy$X)
  target_xy$Y=as.numeric(target_xy$Y)
  if(is.nan(target_xy$X) | is.nan(target_xy$Y)){
    return(data.frame(PointIDX=numeric(),DataIDX=numeric(),DataTypeIDX=numeric(),X=numeric(),Y=numeric(),Value=numeric(),DistFromInXY=numeric()))
  }else{
    
    nearby=getNearby(target_xy,dataTypes=dataTypes, range=range)
    names(nearby)[names(nearby)=="PointIDX:1"]="PointIDX"
    
    if(upOnly){
      #add as vector
      #many ways to do this - here, get it as a Spatial* (sp package) in R, then move it to GRASS
      #readVECT, readRAST read FROM GRASS TO R!  Functions are named from R's perspective !!!!!!!!
      writeVECT(SDF=SpatialPointsDataFrame(coords=target_xy[,c("X","Y")],data=target_xy[,c("PointIDX","ReachName")]),
                vname="targetPoint",v.in.ogr_flags = c("o", "overwrite"))
      
      
      #print(paste("Processing point",target_xy$PointIDX))
      #write nearby points to grass
      pts=SpatialPointsDataFrame(coords=nearby[,c("X","Y")],data=nearby[,c("PointIDX","DataTypeIDX","DistFromInXY","Value")])
      #GRASS is fussy about attribute names - clean them up
      names(pts)=c("PointIDX","DataTypeIDX","Distance","Value")
      writeVECT(SDF=pts,vname="nearPoints",v.in.ogr_flags = c("o", "overwrite"))
      
      #identify area upstream of ER point
      execGRASS("r.water.outlet",input="flowDir",output="above_target",coordinates=as.numeric(c(target_xy$X,target_xy$Y)),flags="overwrite")
      execGRASS("r.to.vect", input="above_target",output="above_target_vect",type="area", flags="overwrite")
      execGRASS("v.select", ainput="nearPoints",binput="above_target_vect",output="nearPoints_upstream", flags="overwrite")
      #read attribute table back to R
      near_up=grassTableToDF(execGRASS("v.db.select",map="nearPoints_upstream",intern = T))
      
      herePoints=nearby$PointIDX[nearby$DistFromInXY==0]
      
      near_up_points=unique(as.numeric(c(near_up$PointIDX,herePoints)))
      
      nearby=nearby[nearby$PointIDX %in% near_up_points,]
    }
    
    return(nearby)
  }
}

#calculate length(s) (segmentwise) of spatialLinesDataFrame
getHeadings=function(feature,smoothScope=1){
  getSegHeadings=function(seg,smoothScope){
    n=nrow(seg@Lines[[1]]@coords)
    if(n>1){
      coordDif=seg@Lines[[1]]@coords[1,]-seg@Lines[[1]]@coords[n,]
      thisHeading=atan2(y=coordDif[2],x=coordDif[1])
      # for(i in 1:(nrow(seg@Lines[[1]]@coords)-1)){
      #   coordDif=seg@Lines[[1]]@coords[(i+1),]-seg@Lines[[1]]@coords[i,]
      #   thisHeading=atan2(y=coordDif[2],x=coordDif[1])
    }
    return(thisHeading*180/pi)
  }
  
  lengths=sapply(feature@lines,getSegHeadings,smoothScope=smoothScope)
  feature$heading_deg=lengths
  return(feature)
}

getValueBreak=function(thisPointIDX,compareDataType=13,hereBelowRange=100,hereAboveRange=900,aboveRange=5900){
  
  this_xy=dbGetQuery(leakyDB,paste0("SELECT Points.PointIDX, Reaches.ReachName, Coordinates.CoordinateTypeIDX, Coordinates.CoordinateIDX, Coordinates.X, Coordinates.Y, Coordinates.EPSG FROM Points LEFT JOIN (
                              Reaches LEFT JOIN Coordinates ON Reaches.CoordinateIDX = Coordinates.CoordinateIDX
  ) ON Points.ReachIDX = Reaches.ReachIDX WHERE PointIDX = '",thisPointIDX,"'"))
  #If the target is a reach, prefer the downstream point, then midpoint, then upstream point, then outflow point
  this_xy$CoordinateTypeIDX=factor(this_xy$CoordinateTypeIDX, levels = c(4,2,3,1))
  this_xy=this_xy[order(this_xy$CoordinateTypeIDX),][1,]
  this_xy$X=as.numeric(this_xy$X)
  this_xy$Y=as.numeric(this_xy$Y)
  near=getNearby(inXY=this_xy,dataTypes = compareDataType,range=aboveRange)
  near=near[base::order(as.numeric(near$DistFromInXY)),]
  

  this_value_veryNear=near$Value[near$DistFromInXY<=hereBelowRange]

  
  writeVECT(SDF=SpatialPointsDataFrame(coords=this_xy[,c("X","Y")],data=this_xy[,c("PointIDX","ReachName")]),
            vname="targetPoint",v.in.ogr_flags = c("o", "overwrite"))
  
  pts=SpatialPointsDataFrame(coords=near[near$DistFromInXY>hereBelowRange,c("X","Y")],
                             data=near[near$DistFromInXY>hereBelowRange,c("PointIDX:1","DataTypeIDX","DistFromInXY","Value")])
  names(pts)=c("PointIDX","DataTypeIDX","Distance","Value")
  writeVECT(SDF=pts,vname="nearPoints",v.in.ogr_flags = c("o", "overwrite"))
  execGRASS("r.water.outlet",input="flowDir",output="above_target",coordinates=as.numeric(c(this_xy$X,this_xy$Y)),flags="overwrite")
  execGRASS("r.to.vect", input="above_target",output="above_target_vect",type="area", flags="overwrite")
  execGRASS("v.select", ainput="nearPoints",binput="above_target_vect",output="nearPoints_upstream", flags="overwrite")
  #read attribute table back to R
  near_up=grassTableToDF(execGRASS("v.db.select",map="nearPoints_upstream",intern = T))
  

  this_value=mean(c(this_value_veryNear,as.numeric(near_up$Value[as.numeric(near_up$Distance)<=hereAboveRange])),na.rm = T)

  up_value=mean(as.numeric(near_up$Value[as.numeric(near_up$Distance)>hereAboveRange]))
  
  #plot(pts)
  #pointLabel(coordinates(pts),labels=as.character(round(pts$DistFromInXY)))

  return(this_value/up_value)
}
