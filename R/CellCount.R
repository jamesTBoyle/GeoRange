#' Calculates equal area cell counts of a specified area
#' @param longs - Array of longitudinal occurrence values in decimal degrees
#' @param lats - Array of latitudinal occurrence values in decimal degrees
#' @param project - Projection of Earth, see dggridR documentation
#' @param aperture - See dggridR documentation
#' @param topology - Shape of the equal area cells, set to hexagon by default. See ddgridR documentation.
#' @param res - Resolution of equal area cells, must range from 0-30 with larger values being higher resolution. Default of 5 is approximately equal to 5x5 degree equitorial cells
#' @return Returns the number of cells occupied, specified cell size, coordinate list, and occupancy matrix if requested
#' @note This method uses grids cells constructed by equal degrees not area. So high latitude cells will have smaller areas than those at low latitude
#' @examples
#' longs<-c(22,55,-144)
#' lats<-c(-12,22,-12)
#' CellCountEA(longs,lats,res=5)
#' @export

CellCount_EA<-function(longs,lats,project="ISEA",aperture=3,topology="HEXAGON",res=5){
	dggs<-dgconstruct(project=project,aperture=aperture,topology=topology,res=res)
	cellID<-dgGEO_to_SEQNUM(dggs,longs,lats)$seqnum
	res<-length(unique(cellID))
	return(res)
}
