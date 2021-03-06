#Objective: Simplify shapefile w/ iteration, reduce polygons to 11 vertices or less

require(maptools)

#Load Minnesota shapefile
state=readShapeSpatial(file.choose(),proj4string=CRS("+proj=longlat +datum=WGS84"))
plot(state)

#show number of vertices in polygons
verts <- lapply(mn@polygons, function(x) sapply(x@Polygons, function(y) nrow(y@coords)))
quantile(unlist(verts))

#Show coordinates of vertices for polygon i
i=5
lapply(mn@polygons[[i]]@Polygons, function(y) matrix(y@coords, nrow=nrow(y@coords), ncol=2))

#show area of polygons
area <- lapply(mn@polygons, function(x) sapply(x@Polygons, function(y) y@area))
quantile(unlist(area))

#Generalize polygon by reducing vertices
state_simpler = thinnedSpatialPoly(state, tolerance=0.02, minarea=0.00002, topologyPreserve = F, avoidGEOS = F)

####Generalize polygons by reiterating with increasing tolerance and min_area
mn=state
min_tolerance=.0002
min_area= min(unlist(area))*.05

for(number in 1:20) {
   cat("1000") 
   for(i in 1:4109) {
        if(sapply(mn@polygons[[i]]@Polygons[1], function(y) nrow(y@coords)) >11) {
        temp = thinnedSpatialPoly(mn[i,], tolerance=min_tolerance, minarea= min_area, topologyPreserve = F, avoidGEOS = F)
        mn@polygons[[i]]@Polygons = temp@polygons[[1]]@Polygons    
        }
        cat("\b\b\b\b\b", i+1000)
}
   min_tolerance = min_tolerance*1.35
   min_area= min_area*1.35
   verts <- lapply(mn@polygons, function(x) sapply(x@Polygons, function(y) nrow(y@coords)))
   a=quantile(unlist(verts))
   print(number)
   print(a)
}


#Minneapolis zoom Plot w/ transparency
require(scales)
plot(mn2, xlim=c(-94.5,-93), ylim=c(44.5,45))
plot(mn, xlim=c(-93.38,-93.34), ylim=c(44.95,45.05), col=alpha("blue",.6))


#Save first 850 polygons
writeSpatialShape(mn[(1:850),], "MN_Map")
