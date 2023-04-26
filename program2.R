rm(list = ls())
# Programs 2,3 and 4 constitute a 'work flow'
# This program (program2.R) loops over s set of  drawings and store the output as a list of list where each sublist contains results specific to a drawing
# It essentially repeats program1.R but it is recommended to run program1.R line-by-line first to understand how it works. 
#prgram is setup so one can rerun a given drawing or a set of new drawings without overwritting drawings that are already been run.
if (file.exists("allDrawings.Rdata")) { load("allDrawings.Rdata") }

library(magick) # used for photo maniuplate
library(jpeg) # used to plot photo
#######
#Drawings to analyze:
examples <- c("la_tetera.JPG","mujer_y_perro.JPG","Matisse3.png","achilles.obj","el_baile.JPG")

######
#this is the number of clusters used for each drawing. 
#The -99 indicates that the file is not an image but rather an obj file derived from an svg file
#it will be treated differently
noClustersVec = c(300,1000,1000,-99,1200)

######
#indicates the number of breaks for each drawing
noBreaksVec = c(1,0,2,5,1)

scaleDrawing <- function(drawing0) {
  rangeX <- range(drawing0[, 1])
  rangeY <- range(drawing0[, 2])
  maxDiff <- max(rangeX[2] - rangeX[1], rangeY[2] - rangeY[1])
  drawing <- drawing0
  drawing[, 1:2] <- cbind(
    2 * (drawing0[, 1] - rangeX[1] - .5 * (rangeX[2] - rangeX[1])) / maxDiff,
    2 * (drawing0[, 2] - rangeY[1] - .5 * (rangeY[2] - rangeY[1])) / maxDiff
  )
  return(drawing)
}
makeContour2 <- function(tmp, startPoint = 1) {
  ## takes a set of points and reorders them correctly
  ## returns the correct order
  firstPoint <- startPoint
  notUsed <- (1:nrow(tmp))[(1:nrow(tmp) %in% startPoint) == FALSE]
  tmp2 <- tmp
  tmp2[firstPoint, ] <- c(-999, -999) * 100 # over write used points with extreme values
  nextPoint <- which.min(distance(tmp2, tmp[firstPoint, ]))[[1]] # find the point closest to the last point
  usedPoints <- c(firstPoint, nextPoint)
  
  for (i in 1:(nrow(tmp) - 2)) {
    tmp2[usedPoints, ] <- c(-999, -999) * 100
    nextPoint <- which.min(distance(tmp2, tmp[nextPoint, ]))[[1]] # find the point closest to the last point
    usedPoints <- c(usedPoints, nextPoint)
  }
  return(usedPoints)
}
findDistance <- function(contour1) {
  # finds the euclidean distance between consecutive points
  # the first point is set to 0
  deltaX <- sqrt(rowSums(
    (contour1[2:nrow(contour1), 1:2] - contour1[1:(nrow(contour1) - 1), 1:2])^2
  )) # compute distance between x vars
  result <- c(0, deltaX)
  
  return(result)
}
distance <- function(xy, xypoint) {
  # finds the distance between a set of points and one specific point
  return(
    (rowSums(
      (xy -
         cbind(
           rep(as.numeric(xypoint[1]), nrow(xy)),
           rep(as.numeric(xypoint[2]), nrow(xy))
         ))^2
    )
    )^(0.5)
  )
}
makePlot <- function(drawing_in, doLines = T, trans = 1, axes0 = T, polygon = F, lwd = 3) {
  lim0 <- c(-1, 1)
  if (doLines == T) {
    plot(NULL, xlim = lim0, ylim = lim0, axes = axes0)
    if (ncol(drawing_in) == 2) {
      lines(drawing_in, lwd = lwd)
    } else {
      for (i in unique(drawing_in[, 3])) {
        lines(drawing_in[drawing_in[, 3] == i, 1:2], lwd = lwd)
      }
    }
  } else {
    plot(drawing_in, xlim = lim0, ylim = lim0, pch = 4, col = rgb(0, 0, 0, trans))
  }
}
makeObservationsData <-function(drawing_in){
  
  array0 <- as.raster(drawing_in)
  
  # this function an array of 3 x the number of pixels in the image, one for each color.
  array1 <- col2rgb(array0)
  
  # These next steps create a table that will be used to represent the image using a 5 column table
  # one column will indicate the row of the pixel
  # another the column of the pixel
  # and the remaining three are the R, G and B values for the color of the pixel
  dsn <- as.data.frame(
    cbind(
      rep(1:nrow(array0), each = ncol(array0)),
      rep(1:ncol(array0), nrow(array0))
    )
  )
  names(dsn) <- c("Y", "X")
  dsn$red <- array1[1, ]
  dsn$green <- array1[2, ]
  dsn$blue <- array1[3, ]
  dsn$value <- (dsn$red + dsn$green + dsn$blue) / 3 # computes the average of the RGB for each pixel
  nrow(dsn)
  
  # These step drops the light pixels and retains only location of the dark pixels
  # as a two column data frame, reducing the size of the table from 853600 rows to 8101
  dsn2 <- dsn[dsn$value < 100, 1:2]
  nrow(dsn2)
  
  # this step switches the axes and inverts y axis so the picture will plot 'right-side up'
  # it also rescales the drawing
  # obs007 are the 'observations' in a data table suitable for cluster analysis
  obs007 <- scaleDrawing(cbind(dsn2[, 2], -dsn2[, 1]))
  return(obs007)
}
makeClusterData <- function(obs007,noClusters){
  set.seed(1) # setting the seed makes the result reproducible, otherwise they are not.
  mod1 <- kmeans(obs007, noClusters, iter.max = 800, algorithm = c("Hartigan-Wong"))
  dsn_Clusters <- as.matrix(mod1$centers)
  return(dsn_Clusters)
}
findCorrectOrdering <- function(dsn_Clusters){
  # find the correct ordering
  startPoint <- 25
  contour1 <- makeContour2(dsn_Clusters, start = startPoint)
  # length(contour1)
  bestLength <- 10^5
  for (startTst in 1:nrow(dsn_Clusters)) {
    contour_tmp <- makeContour2(dsn_Clusters, start = startTst)
    newLength <- sum(findDistance(dsn_Clusters[contour_tmp, ])^2)
    if (newLength < bestLength) {
      bestStart <- startTst
      bestLength <- newLength
    }
    #    print(startTst)
  }
  contour2 <- makeContour2(dsn_Clusters, start = bestStart)
  return(contour2)
}
makeBreaks <- function(contour2,dsn_Clusters,noBreaks){
if (noBreaks > 0) {
  Dist <- findDistance(dsn_Clusters[contour2, ])
  breakPoints <- which(Dist >= Dist[order(-Dist)][noBreaks])
  lineList <- list()

  for (j in 1:length(breakPoints)) {
    if (j == 1) {
    startTmp <- 1
    } else {
    startTmp <- breakPoints[j - 1]
    }
    lineList[[j]] <- startTmp:(breakPoints[j] - 1)
    }
  lineList[[j + 1]] <- breakPoints[j]:length(contour2)
return(lineList)

} else {return(list(1:length(dsn_Clusters)))}

}
pixels <- "800x"

doOver = 1:5   # will run over the vector doOver
for (iii in doOver){
noClusters <- noClustersVec[iii]
noBreaks = noBreaksVec[iii]

filename <- paste0("", examples[iii])
if (noClusters != -99) {
drawing_Original <- image_read(filename)
drawing_Resized <- image_resize(drawing_Original, pixels)
obs007 = makeObservationsData(drawing_Resized)
drawing = list(filename=filename, noClusters=noClusters, breaks=noBreaks, obs = obs007)

dsn_Clusters = makeClusterData(obs007,noClusters) } 

else {
  drawing = list(filename=filename, noClusters=noClusters, breaks=noBreaks, obs = NA)
  dsn = read.csv(filename,header = T,sep="",stringsAsFactors=F)
  vertices=dsn[dsn$X.=="v",2:4]
  nrow(vertices)
  drawing_tmp =cbind(as.numeric(vertices[,1]),-1*as.numeric(vertices[,3]))
dsn_Clusters = scaleDrawing(drawing_tmp)  

}

drawing$clusters = dsn_Clusters

if (noClusters != -99) {correctOrdering = findCorrectOrdering(dsn_Clusters)} else {correctOrdering = 1:nrow(dsn_Clusters) }


drawing$correctOrdering = correctOrdering

lines = makeBreaks(correctOrdering,dsn_Clusters,noBreaks)

drawing$lines = lines
print(iii)
if (iii == 1 ) {allDrawings=list(drawing)}  else {allDrawings[[iii]] = drawing
}
}

save(allDrawings,file="allDrawings.Rdata")

length(allDrawings)