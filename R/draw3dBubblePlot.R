#' Generate camera coordinates
#'
#' This function create a standard matrix of camera coordinates
#' @param cameraCoordinates The axis directions of camera coordinates. Must be a 3*3 matrix. Rows of the matrix represent x, y, z axis direction of the camera coordinates
#' @param cameraPosition The position the camera located. Must be a numeric vector of length 3. 
#' @return a standard matrix of camera coordinates
computeCameraMatrix <- function(cameraCoordinates, cameraPosition){
  for(i in 1:3){
    cameraCoordinates[i, ] <- cameraCoordinates[i, ]/sqrt(sum(cameraCoordinates[i, ]^2))
  }
  cameraMatrix <- cbind(cameraCoordinates, c(0, 0, 0))
  cameraMatrix <- rbind(cameraMatrix, c(cameraPosition, 1))
  cameraMatrix
}

#' Convert 3D positions into 2D positions given a transformation matrix
#' 
#' This function first transforms given 3D positions by the transformation matrix. Then, they were projected onto a canvas and turned into 2D positions.
#' @param transformMatrix A 3*3 matrix transforms 3D positions from one coordinates to another. The matrix can be obtained by calculating the inverse of standard camera coordinates.
#' @param threedPositions A n*3 matrix containing all the positions you want to transform
#' @return A n*2 matrix containing the 2d positions converted from the 3D positions

compute2dCoordinates <- function(transformMatrix, threedPositions){
  transformedCoordinates <- NA
  hold <- NA
  inputCoordinates <- cbind(threedPositions, rep(1, nrow(threedPositions)))
  for(i in 1:nrow(inputCoordinates)){
    if(i == 1){
      hold <- inputCoordinates[1, ]%*%transformMatrix
      transformedCoordinates <- hold[1:3]
    }else{
      hold <- inputCoordinates[i, ]%*%transformMatrix
      hold <- hold[1:3]
      transformedCoordinates <- rbind(transformedCoordinates, hold)
    }
  }
  if(!is.matrix(transformedCoordinates)){
    transformedCoordinates <- matrix(transformedCoordinates, ncol = 3)
  }
  twodCoordinates <- cbind(-transformedCoordinates[, 1]/transformedCoordinates[, 3], -transformedCoordinates[, 2]/transformedCoordinates[, 3])
  as.matrix(twodCoordinates)
}

# plot functions

#' Plot axis
#' @param axisPositions A 4*3 matrix containing absolute positions of the origin, x axis end, y axis end and z axis end
#' @param axisColors A length 3 character vector containing three colors. The colors are represented by hex such as "#ffffff"
#' @param axisLabels A length 3 character vector containing the label of the three axis
plotAxis <- function(axisPositions, axisColors, axisLabels){
  # plot x, y and z axis
  for(i in 2:4){
    arrows(axisPositions[1, 1], axisPositions[1, 2], axisPositions[i, 1], axisPositions[i, 2], col=axisColors[i-1], lwd = 2, angle = 30, length = 0.15)
    # label the axis
    text(axisPositions[i, 1] + (axisPositions[i, 1]-0.5)*0.05, axisPositions[i, 2] + (axisPositions[i, 2]-0.5)*0.05, axisLabels[i-1], col = axisColors[i-1], cex = 0.8)
  }
}

#' Mix colors by position
#' 
#' This function return a mixed color by adding colors of the axes. The mixed color depends on the position of the bubble.
#' @param axisColors A length 3 character vector containing the colors of the axes.
#' @param position A length 3 numeric vector containing the position of the bubble
#' @return A mixed color in hex
colorMixer <- function(axisColors, position){
  r <- paste("0x",substr(axisColors, 2, 3), sep = "")
  g <- paste("0x",substr(axisColors, 4, 5), sep = "")
  b <- paste("0x",substr(axisColors, 6, 7), sep = "")
  # hex to rgb
  r <- strtoi(r)
  g <- strtoi(g)
  b <- strtoi(b)
  
  normalizedPosition <- position
  normalizedPosition[normalizedPosition < 0] <- 0
  normalizedPosition <- normalizedPosition/sum(normalizedPosition)
  
  r <- as.character(as.hexmode(as.integer(sum(r*normalizedPosition))))
  r <- ifelse(nchar(r) == 1, paste("0", r, sep = ""), r)
  g <- as.character(as.hexmode(as.integer(sum(g*normalizedPosition))))
  g <- ifelse(nchar(g) == 1, paste("0", g, sep = ""), g)
  b <- as.character(as.hexmode(as.integer(sum(b*normalizedPosition))))
  b <- ifelse(nchar(b) == 1, paste("0", b, sep = ""), b)
  
  paste("#", r, g, b, sep = "")
}

#' Plot Bubbles
#' @param bubblePositions A n*2 matrix containing the 2D positions of the bubbles
#' @param bubbleSize A numeric vector containing the size of bubbles
#' @param bubble3DPositions A n*3 matrix containing the 3D positions of the bubbles
#' @param axisColors A length 3 character vector containing colors of the axes. Use these colors to add up the color of the bubble.
#' @param bubbleLabels A character vector containing the labels of each bubble
#' @import plotrix
plotBubbles <- function(bubblePositions, bubbleSize, bubble3DPositions, axisColors, bubbleLabels){
  # scale the largest size to radius 0.1
  AdjustedBubbleSize <- bubbleSize/(max(bubbleSize)/0.05)
  for(i in 1:nrow(bubblePositions)){
    currentColor <- colorMixer(axisColors, bubble3DPositions[i, ])
    draw.circle(bubblePositions[i, 1], bubblePositions[i, 2], AdjustedBubbleSize[i], col = currentColor)
    # calculate bubble label position
    randomAngle <- runif(1, min = 0, max = pi)
    labelX <- bubblePositions[i, 1] + AdjustedBubbleSize[i] + 0.004*nchar(bubbleLabels[i])
    labelY <- bubblePositions[i, 2] + AdjustedBubbleSize[i]*cos(randomAngle)
    text(labelX, labelY, labels = bubbleLabels[i], cex = 0.4)
  }
}

#' Draw 3D Bubble Plot
#' A function to draw 3D Bubble plot with flexible bubble size and color.
#' @param cameraCoordinates A 3*3 matrix contains directions of camera coordinates. Rows of the matrix represent x, y, z axis direction. Don't have to be unit vectors
#' @param cameraPosition A length 3 numeric vector contains the position of the camera
#' @param bubblePositions A n*3 matrix contains position of bubbles
#' @param bubbleSize A numeric vector. The size of bubbles
#' @param axisLength A number represents the length of axis to be drawn
#' @param axisColors A length 3 character vector contains colors of axes
#' @param bubbleLabel A character vector contains labels for the bubbles
#' @param axisLabel A character vector contains labels for the axes
#' @return 0 if no error
#' @export
draw3dBubblePlot <- function(cameraCoordinates, cameraPosition, bubblePositions, bubbleSize, axisLength = 1, axisColors = c("#ffffff", "#ffffff", "#ffffff"), bubbleLabel = NA, axisLabel = NA){
  # compute the transformation matrix
  cameraMatrix <- computeCameraMatrix(cameraCoordinates, cameraPosition)
  transformMatrix <- solve(cameraMatrix)
  
  # compute the transformed 2d positions
  axisPositions <- matrix(c(0, axisLength, 0, 0, 0, 0, axisLength, 0, 0, 0, 0, axisLength), 4)
  twodAxis <- compute2dCoordinates(transformMatrix, axisPositions)
  twodBubble <- compute2dCoordinates(transformMatrix, bubblePositions)
  
  # adjust the 2d positions to fit the R plot
  # center the origin
  twodBubble <- sweep(twodBubble, 2, twodAxis[1, ])
  twodAxis <- sweep(twodAxis, 2, twodAxis[1, ])
  
  # cut axis that is too long
  # balance the axis length shown in the 2D-plot
  minAxisLength <- sqrt(min(apply(twodAxis[2:4,]^2, 1, sum)))
  for(i in 2:4){
    twodAxis[i, ] <- twodAxis[i, ]*minAxisLength/sqrt(sum(twodAxis[i, ]^2))
  }
  
  # compute the largest scale
  scale <- max(max(c(twodAxis[, 1], twodBubble[, 1])/0.5), max(c(twodAxis[, 2], twodBubble[, 2])/0.5), max(c(twodAxis[, 1], twodBubble[, 1])/-0.5), max(c(twodAxis[, 2], twodBubble[, 2])/-0.5))
  # scale the positions and move the positions to the center of the canvas
  twodAxis <- twodAxis/scale + 0.5
  twodBubble <- twodBubble/scale + 0.5
  
  # start plotting
  plot.new()
  # plot the axis
  plotAxis(twodAxis, axisColors, axisLabel)
  # add the bubbles
  plotBubbles(twodBubble, bubbleSize, bubblePositions, axisColors, bubbleLabel)
  0
}

