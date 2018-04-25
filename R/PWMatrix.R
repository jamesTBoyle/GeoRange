#' Creates a sparse pairwise distance matrix of a coordinate set
#' @param Coords - Two-dimensional array of longitudinal and latitudinal coordinates output from CoordCollapse() function
#' @return Returns a sparse pairwise distance matrix of great circle distances between pairs of points
#' @examples
#' longs<-runif(10,-22,33)
#' lats<-runif(10,-22,33)
#' Coords<-CoordCollapse(longs,lats)
#' PWMatrix(Coords)
#' @note Uses the distm function from the geosphere package to compute pairwise distances using vincenty ellipsoid approximation of Earth
#' @export
PWMatrix<-function(Coords){
	distmat<-distm(Coords,fun=distGeo)/1000
	n<-nrow(distmat)
	for(i in 1:n){
		for(j in 1:n){
			if(i>=j){
				distmat[i,j]<-NA
			}
		}
	}
	return(distmat)
}
