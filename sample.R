load("sample.RData")

library(draw3dBubblePlot)
draw3dBubblePlot(testCameraCoordinates, testCameraPosition, mutation, size, 20, testColor, label, c("L1", "L2", "L3"))
