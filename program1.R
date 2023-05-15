rm(list = ls())

# This program is intended to be run line by line to facilitate seeing directly
# how each step of the program works
# It produces figures 2,3,4,6 and 7 of the paper

library(magick) # used for photo maniuplate
library(jpeg) # used to plot photo

examples <- c("la_tetera.JPG")

# this function takes a set of points and tran{sforms them so that they are centered
# about the orgin and sit within the square defined by the lower left corner being of {-1,-1} and
# the upper right being {1,1}
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

colorVec <- c("black", "white")
lim0 <- c(-1, 1)

i <- 1
pixels <- "800x"
noClusters <- 300


filename <- paste0("", examples[i])
drawing_Original <- image_read(filename)
drawing_Resized <- image_resize(drawing_Original, pixels)
####
# This step converts the image to an array of the drawing to a table where
# each element of the array is a pixel with an RGB color

array0 <- as.raster(drawing_Resized)

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
nrow(obs007)

# MAKE Drawing1
# this sets the parameters for the plot
# I turn off than annotations, set the margins of the plot to 0
# expand the text as we are save the figure as a high resolution JPEG
par(pty = "s", ann = F, mfrow = c(1, 3), bg = "white", mai = 0 * c(1, 1, 1, 1), family = "Times New Roman", cex = 4)
plot(NULL, xlim = lim0, ylim = lim0, col = "black", axes = F)
points(obs007, pch = 1, cex = 1)
text(0, -1, "Scatter Plot Representation of the Drawing")

plot(obs007[obs007[, 2] > -.09, ],
     xlim = .1 * lim0 + .55, ylim = .1 * lim0,
     main = "Zoom", axes = F, pch = 20
)
text(0.55, -.10, "Detail of Drawing")

plot(obs007, xlim = lim0, ylim = lim0, typ = "l", axes = F)
text(0, -1, "Connecting the Dots with Wrong Ordering")

dev.copy(jpeg, "drawing1.JPEG", width = 3000, height = 1500)
dev.off()

# MAKE Clusters using the kmeans function

set.seed(1) # setting the seed makes the result reproducible, otherwise they are not.
mod1 <- kmeans(obs007, noClusters, iter.max = 800, algorithm = c("Hartigan-Wong"))

dsn_Clusters <- as.matrix(mod1$centers)
# names(dsn_Clusters) = c('x','y')
head(dsn_Clusters)
# write.csv(dsn_Clusters,'dsn_Clusters.csv')
dsn_Clusters2 <- read.csv("dsn_Clusters.csv")
# I save the clusters as a csv in case the cluster analysis produces different results in a different environment
# dsn_Clusters2 and dsn_Clusters should be the same
head(dsn_Clusters2)

# MAKE Drawing2

par(pty = "s", ann = F, mfrow = c(1, 2), bg = "white", mai = 0 * c(1, 1, 1, 1), family = "Times New Roman", cex = 3)
plot(NULL, xlim = lim0, ylim = lim0, axes = F)
text(0, -1, "Representing Teapot Using Clusterings")
points(dsn_Clusters, col = rgb(0, 0, 0, 1), pch = 20)

plot(dsn_Clusters, xlim = lim0, ylim = lim0, col = rgb(0, 0, 0, 1), typ = "l", axes = F, lwd = 3)
text(0, -1, "Connecting clusterings with the wrong ordering")

dev.copy(jpeg, "drawing2.JPEG", width = 2000, height = 1000)
dev.off()


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



# MAKE Drawing3

par(
  pty = "s", ann = F, mfrow = c(1, 2), bg = "white", mai = 0 * c(1, 1, 1, 1),
  family = "Times New Roman", cex = 3
)

makePlot(dsn_Clusters[contour1, ], doLines = T, axes0 = F, lwd = 6)
points(dsn_Clusters[startPoint, 1], dsn_Clusters[startPoint, 2], col = "red", pch = 19, cex = 2)
text(0, -1, "Connecting dots using next closest with bad 1st dot")

makePlot(dsn_Clusters[contour2, ], doLines = T, axes0 = FALSE, lwd = 6)
points(dsn_Clusters[bestStart, 1], dsn_Clusters[bestStart, 2], col = "red", pch = 19, cex = 2)
text(0, -1, "Connecting dots using next closest with a best 1st dot")

dev.copy(jpeg, "drawing3.JPEG", width = 2000, height = 1000)
dev.off()

## Find the break point

Dist <- findDistance(dsn_Clusters[contour2, ])
noBreaks <- 1
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
if (breakPoints > 0) {
  lineList[[j + 1]] <- breakPoints[j]:length(contour2)
}
lineLengths <- sapply(lineList, length)
# useful to have the first line be the longest.

lineLengths <- order(-lineLengths)

# create newDrawing which is the data set of points in the correct order
# with a third column representing which line the point belongs to
# the first line will be the longest line.

newDrawing <- dsn_Clusters[contour2[lineList[[lineLengths[1]]]], ]
newDrawing <- cbind(newDrawing, rep(1, nrow(newDrawing)))

for (j in 2:length(lineLengths)) {
  toadd <- dsn_Clusters[contour2[lineList[[lineLengths[j]]]], ]
  toadd <- cbind(toadd, rep(j, nrow(toadd)))
  newDrawing <- rbind(newDrawing, toadd)
}

# MAKE Drawing4

dev.new()
par(pty = "s", ann = F, mfrow = c(1, 2), bg = "white", mai = 0 * c(1, 1, 1, 1), cex = 3)

makePlot(newDrawing, doLines = T, axes0 = FALSE, lwd = 6)
plot(NULL, ylim = lim0, xlim = lim0, axes = F)
lineVec <- unique(newDrawing[, 3])
for (i in lineVec) {
  polygon(newDrawing[newDrawing[, 3] == i, ], col = colorVec[i])
}

dev.copy(jpeg, "drawing4.JPEG", width = 2000, height = 1000)
dev.off()


# Draw Drawing ontop of Photo

shiftX <- 0
photo <- readJPEG("teapotSquare.jpg")
aspect <- 480 / 480 # resolution aspect = 1000 / 665

source("~/dwdRstuff/utilities/shift.r")

range <- .9
tColors <- c(rgb(1, 1, 1, .75), rgb(0, 0, 0, .75))
par(
  pty = "s", ann = F, mfrow = c(1, 1), bg = "white", mai = 0 * c(1, 1, 1, 1),
  family = "Times New Roman"
)
plot(range * c(-1 + shiftX, 1), range * c(-1, 1), type = "n", axes = FALSE)
rasterImage(photo, xleft = -1, xright = 1, ybottom = -1 / aspect, ytop = 1 / aspect)
# points(newDrawing[,1:2])
newdsn2 <- shift(.7 * newDrawing[, 1:2], c(.04, -.04))
lineVec <- unique(newDrawing[, 3])
for (i in lineVec) {
  polygon(newdsn2[newDrawing[, 3] == i, ], col = tColors[i], border = "blue", lwd = 6)
}

dev.copy(jpeg, "drawnTeapot.JPEG", width = 1000, height = 1000)
dev.off()