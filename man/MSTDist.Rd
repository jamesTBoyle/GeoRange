% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MSTDist.R
\name{MSTDist}
\alias{MSTDist}
\title{Calculates the minimum spanning tree distance, in kilometers, using Prim's Algorithm [1]}
\usage{
MSTDist(longs, lats)
}
\arguments{
\item{longs}{- Longitudinal occurrences in decimal degrees}

\item{lats}{- Latitudinal occurrences in decimal degrees}
}
\value{
Returns the minimum spanning tree distance in kilometers, the pairwise distance matrix of occurrences, the order points were connected in, and a 2-column array of coordinates
}
\description{
Calculates the minimum spanning tree distance, in kilometers, using Prim's Algorithm [1]
}
\details{
Uses Prim's algorithm for finding the minimum spanning tree, time-consuming calculation as the number of locations increases past 1000
}
\examples{
longs<-c(12,34,-55)
lats<-c(-41,3,56)
MSTDist(longs,lats)
}
\references{
[1] Prim, R.C. 1957. Shortest Connection Networks and Some Generalizations. The Bell System Technical Journal 36:1389-1401.
}
