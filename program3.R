
# this program is intended to collect the information in allDrawings (a list of lists)
# and transform it into a single data file that we will save as a csv.
# it allows for manual overrides in cases where program2 failed to find the correct order/set of lines


rm(list = ls())
load("allDrawings.Rdata")


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

for (iii in 1:length(allDrawings)) {
  drawing_tmp <- allDrawings[[iii]]
  dsn_Clusters <- drawing_tmp$clusters
  lineList <- drawing_tmp$lines
  correctOrdering <- drawing_tmp$correctOrdering
  
  
  if (iii == 4) {
    lineList <- list()
    lineList[[1]] <- c(1:900, 901:999, 1001:1198)
    lineList[[2]] <- c(1199:1787)
    lineList[[3]] <- c(2148:2295)
    lineList[[4]] <- c(1788:2147)
  }
  
  if (iii == 3) {
    lineList <- list()
    lineList[[1]] <- c(965:997, 4:719)
    lineList[[2]] <- c(720:751)
    lineList[[3]] <- c(752:771)
    lineList[[4]] <- c(772:787)
    lineList[[5]] <- c(955:961, 788:847)
    lineList[[6]] <- c(848:858, 862:879, 848)
    lineList[[7]] <- c(881:914)
    lineList[[8]] <- c(915:930, 935:938, 942:954, 915)
  }
  
  
  lineLengths <- sapply(lineList, length)
  # useful to have the first line bethe longest (with most points).
  lineLengths <- order(-lineLengths)
  
  # create newDrawing which is the data set of points in the correct order
  # with a third column representing which line the point belongs to
  # the first line will be the longest line.
  
  
  newDrawing <- dsn_Clusters[correctOrdering[lineList[[lineLengths[1]]]], ]
  newDrawing <- cbind(newDrawing, rep(1, nrow(newDrawing)))
  
  for (j in 2:length(lineLengths)) {
    toadd <- dsn_Clusters[correctOrdering[lineList[[lineLengths[j]]]], ]
    toadd <- cbind(toadd, rep(j, nrow(toadd)))
    newDrawing <- rbind(newDrawing, toadd)
  }
  
  
  makePlot(newDrawing)
  
  lines(newDrawing[newDrawing[, 3] == 4, 1:2], col = "red")
  numbers <- floor(seq(1, length(correctOrdering), 5))
  correct_row <- correctOrdering[numbers]
  # number the points is useful for determining over rides if necesary
  text(dsn_Clusters[correct_row, 1], dsn_Clusters[correct_row, 2], numbers, col = "darkblue")
  
  almostDone <- data.frame(newDrawing)
  names(almostDone) <- c("xx", "yy", "line")
  
  almostDone$name <- drawing_tmp$filename
  almostDone$jjj <- iii
  almostDone$date <- Sys.time()
  
  
  head(almostDone)
  
  if (iii == 1) {
    allDrawings26April2023 <- almostDone
  } else {
    allDrawings26April2023 <- rbind(allDrawings26April2023, almostDone)
  }
}

write.csv(allDrawings26April2023, file = "allDrawings26April2023.csv", row.names = FALSE)