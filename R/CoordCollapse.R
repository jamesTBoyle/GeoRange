#' Removes duplicate geographic locations and binds coordinates into a single element
#' @param longs - Longitudinal coordinates of occurrences in decimal degrees
#' @param lats - Latitudinal coordinates of occurrences in decimal degrees
#' @return Returns a 2-column array of coordinates without any duplicate locations
#' @examples
#' longs<-c(34,133,-45)
#' lats<-c(-12,44,76)
#' CoordCollapse(longs,lats)
#' @note Points are truncated to the hundredths place before checking for duplicates
#' @export
CoordCollapse<-function(longs,lats){
	Coords<-cbind(longs,lats)
	Coords<-as.data.frame(Coords)
	Coords<-unique(Coords[,c("longs","lats")])
	names(Coords)<-c("Longitude","Latitude")
	Coords<-Coords[order(Coords$Longitude),]
	return(Coords)
}
