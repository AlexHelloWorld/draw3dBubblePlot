% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw3dBubblePlot.R
\name{draw3dBubblePlot}
\alias{draw3dBubblePlot}
\title{Draw 3D Bubble Plot
A function to draw 3D Bubble plot with flexible bubble size and color.}
\usage{
draw3dBubblePlot(cameraCoordinates, cameraPosition, bubblePositions, bubbleSize,
  axisLength = 1, axisColors = c("#ffffff", "#ffffff", "#ffffff"),
  bubbleLabel = NA, axisLabel = NA)
}
\arguments{
\item{cameraCoordinates}{A 3*3 matrix contains directions of camera coordinates. Rows of the matrix represent x, y, z axis direction. Don't have to be unit vectors}

\item{cameraPosition}{A length 3 numeric vector contains the position of the camera}

\item{bubblePositions}{A n*3 matrix contains position of bubbles}

\item{bubbleSize}{A numeric vector. The size of bubbles}

\item{axisLength}{A number represents the length of axis to be drawn}

\item{axisColors}{A length 3 character vector contains colors of axes}

\item{bubbleLabel}{A character vector contains labels for the bubbles}

\item{axisLabel}{A character vector contains labels for the axes}
}
\value{
0 if no error
}
\description{
Draw 3D Bubble Plot
A function to draw 3D Bubble plot with flexible bubble size and color.
}
