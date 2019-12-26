#' Display total rain observation over China.
#'
#' @param totalRain, data.frame, {lon, lat, totalRain}
#' @param nx, interpolated grid nx
#' @param ny, interpolated grid ny
#' @param myat, contour levels
#' @param xlim, map longitude limit
#' @param ylim, map latitude limit
#' @param colors, color pallete
#' @param main, plot titile
#' @param interpKrige, interpolate totalRain by krige method (slowly)
#' @param dgrid, grid interval
#' @param imageFile, output image file
#'
#' @return NULL
#' @export
#'
#' @examples
#'   display_total_rain_china(totalRain)
#'
draw_rain_china <- function(totalRain, nx=128, ny=76,
                            myat=seq(0, 2400, by=300),
                            xlim=c(72,136), ylim=c(16,54),
                            colors=NULL, main=NULL,
                            interpKrige=FALSE, dgrid=0.5,
                            imageFile=NULL){
  # change column names
  colnames(totalRain) <- c("x", "y", "z")
  
  # interpolation to grid
  if (interpKrige) {
    pred <-expand.grid(seq(xlim[1],xlim[2],by=dgrid),seq(ylim[1],ylim[2],by=dgrid))    # create predicted grid
    pred <-cbind(pred[,1], pred[,2])
    pred <-sp::SpatialPoints(pred)
    totalRainFLD <- sp::SpatialPointsDataFrame(totalRain[,1:2],totalRain[,3])          # interpolate to grid
    totalRainFLD <- automap::autoKrige(z~1, totalRainFLD, new_data=pred)
    totalRainFLD <- as.data.frame(totalRainFLD$krige_output)
    totalRainFLD <- raster::rasterFromXYZ(totalRainFLD[,1:3],                          # convert to raster
                                          crs='+proj=longlat +ellps=WGS84')
  } else {
    totalRainFLD <- akima::interp(x=totalRain[[1]], y=totalRain[[2]],
                                  z=totalRain[[3]], linear=TRUE, nx=nx,ny=ny)
    totalRainFLD <- raster::raster(totalRainFLD, crs='+proj=longlat +ellps=WGS84')
  }
  
  # mask out china region
  chinashp <- rgdal::readOGR(system.file("extdata",package="nmcMetResources"), "bou1_4p")
  totalRainFLD <- raster::mask(totalRainFLD, chinashp, inverse = FALSE)
  
  # set color key
  myColorkey <- list(at=myat, labels=list(at=myat,cex=1.5))
  if (is.null(colors)) colors <- rev(RColorBrewer::brewer.pal(length(myat), 'RdBu'))
  
  # draw raster
  provincesBound <- nmcMetResources::provinceDF
  riversBound <- nmcMetResources::river1DF
  p1 <- rasterVis::levelplot(totalRainFLD, layers=1, margin=FALSE, contour=FALSE,
                             at=myat, colorkey=myColorkey, xlim=xlim, ylim=ylim,
                             scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                             xlab=list("Longitude", cex=1.5), ylab=list("Latitude", cex=1.5),
                             main=list(label=main, cex=1.5), lwd=2,
                             par.settings=rasterVis::rasterTheme(region=colors,axis.line=list(lwd = 2))) +
    latticeExtra::layer(panel.lines(lon,lat,col="black"),data=provincesBound) +
    latticeExtra::layer(panel.lines(lon,lat,col="blue"),data=riversBound)
  p2 <- lattice::xyplot(lat ~ lon, provincesBound, type='l', col='black',
                        scales=list(x=list(at=NULL),y=list(at=NULL)), lwd=1.5,
                        aspect="iso",xlab="",ylab="",xlim=c(106,124),ylim=c(3,23))
  
  # show the graphics
  if (is.null(imageFile)) {
    dev.new(width=14, height=10, noRStudioGD=TRUE)
    print(p1, position=c(0,0,1,1), more=TRUE)
    print(p2, position=c(0.1,0.07,0.32,0.4))
  } else {
    cairo_pdf(filename=imageFile,pointsize=12,width=9,height=6.4,family="simhei")
    print(p1, position=c(0,0,1,1), more=TRUE)
    print(p2, position=c(0.1,0.07,0.37,0.45))
    dev.off()
  }
  return(NULL)
}
