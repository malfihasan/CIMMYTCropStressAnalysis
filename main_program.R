# The script is a development-script to develop two functions: "probRaster"
# Input data



# --------------------------- #
#   Initialization section    #
# --------------------------- #

# Required library
library(raster)
library(rgdal)
library(maptools)
library(sp)
library(automap)
library(colorspace)
library(RColorBrewer)

# Required functions 
source('Functions/splitFun.R')
source('Functions/subMonth.R')
source('Functions/probThreshold4Temp.R')
source('Functions/probRasterWhole.R')

# ---------------------------------------------  Main program  -------------------------------------------------------- #

# -------------------- Intialization section --------------------- # 
# Loading the processed data frame
###############################################################
load( file='Save_file_BMD_data_Tmax_Tmin_1984_2016.Rsave' ) ### User input of the BMD data ( for more help look in the function directory ) ###
###############################################################

# Calling function to split the data in half
list.tmax <- splitFun(DF.Tmax.BMD.final[732:length(DF.Tmax.BMD.final[,1]),], 50) ### User input of begain position of the desired dataframe

list.tmin <- splitFun(DF.Tmin.BMD.final[732:length(DF.Tmin.BMD.final[,1]),], 50) ### User input of begain position of the desired dataframe


dfTmax.Whole <- DF.Tmax.BMD.final[732:length(DF.Tmax.BMD.final[,1]),]   ### User input of begain position of the desired dataframe
dfTmin.Whole <- DF.Tmin.BMD.final[732:length(DF.Tmin.BMD.final[,1]),]   ### User input of begain position of the desired dataframe

# Sample month values
begin.month = 2
begin.day = 28
end.month = 3 
end.day = 15

# Threshold values 
# # For maximum temperature 
varThreshold.max <- 32

# # For minimum temperature 
varThreshold.min <- 22

# Loading the station information 
varlocation <- read.csv('Location_station_CIMMYT.csv')

# Grid infromation
###############################################################
dummy.raster = 'C:/Users/Ali/Google Drive/Research_URI/Fundamental Shapefiles/Bangladesh/Grid_shapefile/Grid_BD_one_hund_DEG.tif'  ### User input of grid data ( path is hard coded, user can give their own grid. ) ###
###############################################################

dummy.ps <- raster(dummy.raster)

list.var <- probRasterWhole(dfTmax.Whole, dfTmin.Whole, list.tmax, list.tmin, begin.month, begin.day, end.month, end.day, varThreshold.max, varThreshold.min, 
                       raster_grid = dummy.ps, location_df = varlocation )
list.sel <- cbind(list.var[[3]]$values, list.var[[5]]$values )

# A list of various color palettes
list.pal <- list()
list.pal[[7]] <-colorRampPalette(c('blue','gray','red'), space="rgb") # Prescribed by Carlo
list.pal[[8]] <-colorRampPalette(c('gray','yellow','red'), space="rgb") # Prescribed by Carlo
list.pal[[9]] <-colorRampPalette(c('blue','green','gray'), space="rgb") # Prescribed by Carlo

# For a automated break.. :) 
roundUpto10 <- function(x, upvar=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * upvar[[which(x <= 10^floor(log10(x)) * upvar)[[1]]]]
}

range.val <- roundUpto10(range(list.sel, na.rm = T)[2] - range(list.sel, na.rm = T)[1])

min.switch=F
min.val <- min(list.sel, na.rm = T)-range.val/10
if(min.val<=0){
  min.val=abs(min.val)
  min.switch=T
}
min.round <- roundUpto10(min.val)
if (min.switch==T){
  min.round = - min.round
  min.switch=F
}

breaks <- seq(min.round,
              roundUpto10(max(list.sel, na.rm = T)),
              by=range.val/10)*100


# Selecting color pallette 
funcx <- list.pal[[8]] 

# read in the shape poly
###############################################################
bd_border <- readShapePoly("../../../../../Fundamental Shapefiles/Bangladesh/District_dissolved_BD/dis_disolved")  ### User input of shape files ( for Bangladesh boundary ) ###
river_shp <- readShapePoly("../../../../../Fundamental Shapefiles/Bangladesh/Major_rivers_py/bgd_hyd_major_rivers_py") ### User input of grid data ( for Bangladesh rivers ) ###
###############################################################

# file.output.name <- #  Need to be provided by the user. 
cairo_pdf(file.output.name, width = 6.3, height = 8., family = "serif")

values(dummy.ps) <- list.var[[3]]$values * 100

# Updated breaks 
breaks <- breaks[breaks>=0]

plot(dummy.ps,  col=funcx(12), 
     breaks=breaks,
     xaxt='n', yaxt='n' , xlim =c(88.1,92.6), ylim=c(20.5,26.7))
plot(bd_border,  border="black", col="transparent", lwd=1, add=T, xlim =c(88.1,92.6), ylim=c(20.5,26.7)) 
plot(river_shp,  border="transparent", col="blue", lwd=1, add=T, xlim =c(88.1,92.6), ylim=c(20.5,26.7)) 

#creating X-axis
axis(side=1, 
     tcl=0.3, 
     cex.axis=1, 
     at =c(seq(88,94,2)), 
     mgp=c(0.25, 0.25, 0), 
     labels=c(seq(88,94,2))
)

#Creating Y-axis
axis(side=2,
     tcl=0.3,
     mgp=c(0.25,0.25, 0), 
     at =c(seq(21,26,2)),
     cex.axis=1, 
     labels=c(seq(21,26,2)), 
     las=1 
)


dev.off()

