% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotMST.R
\name{PlotMST}
\alias{PlotMST}
\title{Plots the minimum spanning tree of a set of coordinates}
\usage{
PlotMST(MSTCalc, color = "black", symbol = 16, xlimit = "NA",
  ylimit = "NA")
}
\arguments{
\item{MSTCalc}{- Output from the MSTDist function}

\item{color}{- Color of the lines connecting point}

\item{symbol}{- Symbol value for the pch graphical parameter for plotting coordinates}

\item{xlimit}{- Array of values denoting the x-axis limits}

\item{ylimit}{- Array of values denoting the y-axis limits}
}
\value{
Plots a minimum spanning tree
}
\description{
Plots the minimum spanning tree of a set of coordinates
}
\note{
If the xlimit and ylimit parameters are left to their default values the axis ranges are based on the minimum and maximum values of the coordinates
This function does not account for the possibility of points crossing the prime meridian and in cases where this occurs lines will cut across the entire plot
}
\examples{
w<-MSTDist(longs=c(23,78,-23,56),lats=c(21,4,55,-3))
PlotMST(MSTCalc=w)
}
