#this program makes a pdf of all the drawings in the csv "allDrawings26April2023.csv"


rm(list = ls())
lim0 <- c(-1, 1)
dsn_drawings <- read.csv("allDrawings26April2023.csv")

dev.new()

pdf("allDrawings.pdf")
JJJs <- unique(dsn_drawings$jjj)
par(
  pty = "s", ann = F, mfrow = c(3, 2), bg = "white", mai = 0 * c(1, 1, 1, 1)
)
colorVec1 <- c("black", rep("white", 10))
colorVec2 <- c("black", "black")

borderColor1 <- rgb(0, 0, 0, 0)
colorVec3 <- rep(borderColor1, 2)
borderColor2 <- "black"

for (j in JJJs) {
  colorVec <- colorVec1
  borderColor <- borderColor1
  doPolygon <- TRUE
  if (j == 5) {
    colorVec <- colorVec2
  }
  if (j == 3) {
    colorVec <- colorVec3
    borderColor <- borderColor2
  }
  if (j == 3) doPolygon <- FALSE
  drawing <- dsn_drawings[dsn_drawings$jjj == j, 1:3]
  lineVec <- unique(drawing$line)
  plot(NULL, ylim = lim0, xlim = lim0, axes = FALSE)
  for (i in lineVec) {
    if (doPolygon == T) {
      polygon(drawing[drawing[, 3] == i, ], col = colorVec[i], border = NA)
    }
    lines(drawing[drawing[, 3] == i, ], col = "black", lwd = 1)
  }
}

dev.off()