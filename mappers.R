# mappers.R
# Some useful functions that make working with spatial data in R much easier
# Most functions are only useful for New Orleans data, though some can be adapted/generalized for use in other areas


### Init
setInternet2(TRUE)
library(grid)
library(gridExtra)
library(gtable)
library(ggplot2)
library(maps)
library(maptools)
library(sp)
library(rgeos)
library(RColorBrewer)
library(classInt)
library(rgdal)
library(ggmap)
library(leafletR)
NOLA.proj <- CRS("+init=epsg:3452")
latlon.proj <- CRS("+init=epsg:4326")

### Functions for getting and processing Spatial data

zipToSpatial <- function(file.source){
  # Given a dl link, downloads a shapefile and converts to Spatial object
  # This is a general function and can be used for most zipped spatial data regardless of type or location.
  # It has some limitations in the way data is organized. The zipped files must be either:
  # A. In the first level of the zipped file (this is how data is most commonly structured).
  # B. In a folder within the zipped file. The folder should be the only item in the first level of the zipped file.
  # See help(readOGR) for more details on reading spatial data into R.

wdMethod <- function(){		zip.dir <- "JunkDataForZips"
  dir.create(zip.dir)
  file.land <- paste0(getwd(),"/", zip.dir, "/zipped_data.zip")

  # download and unzip file to temp directory (check if this works on work comp...if not set up junk folder within working directory)
  download.file(url=file.source, destfile=file.land, mode="wb")
  unzip(file.land, exdir=zip.dir)

  # read shp as R Spatial object. Either downloads files directly, or if the files are within another directory of the zip file, looks within those
  files <- list.files(zip.dir)
  if(length(files)>=3){
    file.trunk <- sub("^([^.]*).*", "\\1", files)[1]
    shp <- readOGR(dsn=zip.dir, layer=file.trunk)
  } else{
    files.2 <- paste0(zip.dir, "\\", files)
    files.2 <- files.2[!grepl("zipped_data.zip", files.2)]
		files.in <- list.files(files.2)
    files.in.trunk <- sub("^([^.]*).*", "\\1", files.in)[1]
    shp <- readOGR(dsn=files.2, layer=files.in.trunk)
  }

  unlink(paste0(getwd(),"/", zip.dir), recursive = TRUE)
}

	tempMethod <- function(){
 ### Method 2: sets up a temporary directory and downloads there
                zip.dir <- tempfile()
                dir.create(zip.dir)
                file.land <- "zipped_data.zip"

                #download and unzip file to temp directory (check if this works on work comp...if not set up junk folder within working directory)
                download.file(url=file.source, destfile=file.land, mode="wb")
                unzip(file.land, exdir=zip.dir)

                #read shp as R Spatial object. Either downloads files directly, or if the files are within another directory of the zip file, looks within those
                files <- list.files(zip.dir)
                if(length(files)>=3){
                                file.trunk <- sub("^([^.]*).*", "\\1", files)[1]
                                shp <- readOGR(dsn=zip.dir, layer=file.trunk)
                } else{
                                files.2 <- paste0(zip.dir, "\\", files)
                                files.in <- list.files(files.2)
                                files.in.trunk <- sub("^([^.]*).*", "\\1", files.in)[1]
                                shp <- readOGR(dsn=paste0(zip.dir,"\\", files), layer=files.in.trunk)
                }

                file.remove(file.land)
                unlink(zip.dir)
                return(shp)
}
	
wdMethod()
return(shp)
}


zipToSpatial <- function(file.source){
                #given a dl link, downloads a shapefile and converts to spatial object
                #(this is a general function and can be used for most zipped spatial data regardless of type or location.
                #It has some limitations in the way data is organized. The zipped files must be:
                                #A. In the first level of the zipped file (this is how data is most commonly structured).
                                #B. In a folder within the zipped file. The folder can be the only item in the first level of the zipped file.
                #See help(readOGR) for more details on reading spatial data into R.

                #set up temporary directory for unzipping
                zip.dir <- tempfile()
                dir.create(zip.dir)
                file.land <- "zipped_data.zip"

                #download and unzip file to temp directory (check if this works on work comp...if not set up junk folder within working directory)
                download.file(url=file.source, destfile=file.land, mode="wb")
                unzip(file.land, exdir=zip.dir)

                #read shp as R Spatial object. Either downloads files directly, or if the files are within another directory of the zip file, looks within those
                files <- list.files(zip.dir)
                if(length(files)>=3){
                                file.trunk <- sub("^([^.]*).*", "\\1", files)[1]
                                shp <- readOGR(dsn=zip.dir, layer=file.trunk)
                } else{
                                files.2 <- paste0(zip.dir, "\\", files)
                                files.in <- list.files(files.2)
                                files.in.trunk <- sub("^([^.]*).*", "\\1", files.in)[1]
                                shp <- readOGR(dsn=paste0(zip.dir,"\\", files), layer=files.in.trunk)
                }

                file.remove(file.land)
                unlink(zip.dir)
                return(shp)
}



# Some common wrappers for zipToSpatial (can take a while to run, especially for smaller geographies.
# If the data has been pulled once in a session, an archived copy is used)
getNOLA <- function(){
	if(exists("nola.abc.abc")==FALSE){
		nola <- zipToSpatial("http://data.nola.gov/api/geospatial/2b2j-u6kh?method=export&format=Shapefile")
		nola.abc.abc <- nola
		} else{nola <- nola.abc.abc}
	return(nola)
}
getCouncil <- function(){
	if(exists("council.abc.abc")==FALSE){
		council <- zipToSpatial("http://data.nola.gov/api/geospatial/24es-ghzz?method=export&format=Shapefile")
		council.abc.abc <- council
	} else{council <- council.abc.abc}
	return(council)
}
getNbhds <- function(){
	if(exists("nbhds.abc.abc")==FALSE){
		nbhds <- zipToSpatial("https://data.nola.gov/api/geospatial/ukvx-5dku?method=export&format=Shapefile")
		nbhds.abc.abc <- nbhds
	} else{nbhds <- nbhds.abc.abc}
	return(nbhds)
}
getParcels <- function(){
	if(exists("parcels.abc.abc")==FALSE){
		parcels <- zipToSpatial("http://data.nola.gov/api/geospatial/e962-egyh?method=export&format=Shapefile")
		parcels.abc.abc <- parcels
		} else{parcels <- parcels.abc.abc}
	return(parcels)
}
getBlocks <- function(){
	if(exists("blocks.abc.abc")==FALSE){
		blocks <- zipToSpatial("http://www.norpc.org/assets/pdf-documents/zip/orl_census2010_block_pl.zip")
		blocks.abc.abc <- blocks
	} else{blocks <- blocks.abc.abc}
	return(blocks)
}
getBGs <- function(){
	if(exists("BGs.abc.abc")==FALSE){
		BGs <- zipToSpatial("http://www.norpc.org/assets/pdf-documents/zip/orl_census2010_blockgrp_pl.zip")
		BGs.abc.abc <- BGs
	} else{BGs <- BGs.abc.abc}
	return(BGs)
}
getTracts <- function(){
	if(exists("tracts.abc.abc")==FALSE){
		tracts <- zipToSpatial("http://www.norpc.org/assets/pdf-documents/zip/orl_census2010_tract_pl.zip")
		tracts.abc.abc <- tracts
	} else{tracts <- tracts.abc.abc}
	return(tracts)
}

csvFromWeb <- function(file.source){
	#given a dl link, reads in a csv. This is also a generic function with uses outside of spatial analysis.
	target.dir <- getwd()
	file.loc <- paste0(target.dir, "/file.csv")
	download.file(file.source, file.loc)
	csv <- read.csv(file.loc)
	file.remove(file.loc)
	return(csv)
}

toSpatialPoints <- function(df, X, Y, remove.outliers=FALSE, nola.only=TRUE, ..., p4string){
	# Converts a data.frame into a SpatialPointsDataFrame
	# If no p4string is given, should only be used for data around New Orleans (though it could be adapted for use in other areas)
	# df: a data.frame with coordinates
	# X: column name of x coordinates/longitude
	# Y: column name of y coordinates/latitude
	# remove.outliers: remove points with >50% difference than mean x or y?
	#   good for removing bad data with coordinates outside the CRS
	# nola.only: remove points that lie outside of Orleans Parish boundaries?
	#   faster data cleaning option than remove.outliers
	# also takes the optional argument p4string, the proj4string...use if data is not LA South Stateplane or WGS84
	#

	dots <- list(...)
	#get coordinates in df and remove missing points
	coords.df <- cbind(as.numeric(unlist(df[X])),as.numeric(unlist(df[Y])))
	has.coords <- complete.cases(coords.df)
	df <- df[has.coords,]
	coords.df <- coords.df[has.coords,]

	# remove outlier points (defined as having >50% difference from mean)
	if(remove.outliers==TRUE){
		valid.coords <- c()
		for(i in 1:nrow(coords.df)){
			if(abs((coords.df[i,1]-mean(coords.df[,1]))/mean(coords.df[,1]))>.5){X.off <- 1}
			if(abs((coords.df[i,2]-mean(coords.df[,2]))/mean(coords.df[,2]))>.5){Y.off <- 1}
			if(exists("X.off")| exists("Y.off")){valid.coords[i] <- FALSE}
			else{valid.coords[i] <- TRUE}
			rm(X.off,Y.off)
		}
		df <- df[valid.coords,]
		coords.df <- coords.df[valid.coords,]
	}

	# makes Spatial object
	# if no p4string is given, assigns either LA South Stateplane or WGS84 coordinate system
	if(!is.null(dots$p4string)){ spatial.points <- SpatialPointsDataFrame(coords = coords.df, df, proj4string = CRS(p4string))
	} else if(mean(coords.df[,1]>1000, na.rm=TRUE)>=.8){
		spatial.points <- SpatialPointsDataFrame(coords=coords.df, df, proj4string=NOLA.proj)
		} else{spatial.points <- SpatialPointsDataFrame(coords=coords.df, df, proj4string=latlon.proj)}
	if(nola.only==TRUE){
		nola <- getNOLA()
		nola <- spTransform(nola, spatial.points@proj4string)
		spatial.points <- spatial.points[nola,]
	}
	return(spatial.points)
}

webToSpatialPoints <- function(file.source, X, Y, remove.outliers=FALSE, nola.only=TRUE){
	# A combination of csvFromWeb and toSpatialPoints. Goes directly from a dl link to a SpatialPoints object.
	# Unless you know the structure of the data, it's probably better to get the data as a csv, process/explore, then use toSpatialPoints()
	csv <- csvFromWeb(file.source)
	spatial.points <- toSpatialPoints(df=csv, X=X, Y=Y, remove.outliers = remove.outliers, nola.only=nola.only)
	return(spatial.points)
}

geopinsToPoints <- function(df, geopin.col="GEOPIN"){
  # Many New Orleans datasets don't have coordinates, but do have geopins, a unique id used to identify parcels.
  # This function converts a data.frame with geopins into a SpatialPoints object
  # df: a dataframe with geopins
  # geopin.col: name of the column in df with geopins
	parcels <- getParcels()

	names(df)[which(names(df)==geopin.col)] <- "GEOPIN"
	df$ID.for.match <- 10:(nrow(df)+9)
	geo.cols <- c("ID.for.match", "GEOPIN")
	valid  <- df$GEOPIN %in% parcels$GEOPIN
	df <- df[valid,]

	parcels.sub <- merge(x = parcels, y = df, all.x = FALSE, duplicateGeoms = FALSE)
	parcels.sub <- parcels.sub[order(parcels.sub$ID.for.match), ]
	df.coords <- data.frame(coordinates(parcels.sub))
	colnames(df.coords) <- c("X","Y")
	df <- cbind(df.coords, df)
	df.coords <- subset(df, select = c(X, Y))
	df.sp <- SpatialPointsDataFrame(coords=df.coords, data=df, proj4string=NOLA.proj)
	df.sp <- df.sp[, -which(names(df.sp) == "ID.for.match")]
	return(df.sp)
}


prettyLabs <- function(brks){
	labs <- c()
	lab.1 <- paste(brks[1], " - ",brks[2])
	labs <- append(labs, lab.1)
	for(i in 1:(length(brks)-3)){

	i <- i+1
	
	sep.num <- unlist(strsplit(as.character(brks[3]), "\\."))
	if(length(sep.num) == 1){
		digits <- 0
	} else { digits <- nchar(sep.num[2])}
	increment <-(1/10)^(digits) 

	lab <- paste(brks[i]+increment, "-", brks[i+1])
	labs <- append(labs, lab)
	}
		
	lab.n <- paste(">", brks[length(brks)-1] )
	labs <- append(labs, lab.n)

	return(labs)
}

### Mapping functions
mapOPAPoints <- function(pts, X, Y, size = 1, title = "Map!", location = c(-90.031742, 29.996680), zoom = 12, style = "single", fill = "black", old.map = "new", map_source = "stamen", map_type = "toner-lite", ...){
  # Given a data.frame or SpatialPointsDataFrame, creates a points map.
  # pts: df or SpatialPoints/SpatialPointsDataFrame object
  # X: field name with X values
  # Y: field name with Y values
  # size: how large should the points be?
  # title: name of the map
  # location/zoom:  parameters for the basemap...see help(get_map) for more details...defaults give a citywide view of New Orleans
	#   location can a lon/lat pair, address, or bounding box
  # style: either "single" for uniform symbols or the column name of pts that should control symbology 
  # fill: point color(s)...if style != "single" this should be a vector equal to the number of classes for factors or high/low values for continuous
  # old.map: either "new" to plot a new map or the name of an existing ggmap to add points
  # map_source: declare "source" argument in "get_map" fn
  # map_type: declare "maptype" argument in "get_map" fn

	# optional arguments:
	# breaks: if style refers to a numeric vector, defines the breakpoints. Can either be a numeric vector the same length as fill or the name of a style in classIntervals()
	#   if no breaks are given, symvology will be on a continuous scale
  # alpha: 0-1, how opaque should shapes be?
  # labs: label names (for continuous data with breaks). If not supplied, the function attempts to make them with prettyLabels()
	
	dots <- list(...)

	if(is.data.frame(pts)) {
    pts <- toSpatialPoints(df = pts, X = X, Y = Y)
  }

	pts <- spTransform(pts, latlon.proj)
	pts <- as.data.frame(pts)
	# these lines may cause bugs down the line as.data.frame() should append the X and Y columns into the last 2 columns of the data.frame, but that may not always be the case
  pts[X] <- pts[, ncol(pts)-1]
	pts[Y] <- pts[, ncol(pts)]


	# Setting plot aesthetics
	if(old.map == "new") {
		basemap <- get_map(source = map_source, location = location, maptype = map_type, zoom = zoom)
		map <- ggmap(basemap)
	} else {
    map <- old.map
  }

	blank.theme <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
            		 axis.text.y=element_blank(),axis.ticks=element_blank(),
            	 	 axis.title.x=element_blank(),
            	 	 axis.title.y=element_blank(),
            	   panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            	   panel.grid.minor=element_blank(),plot.background=element_blank())


	# Finding style to use and setting class breaks if relevant:
	if(style != "single") {
		style.col <- pts[,which(names(pts) == style)]
		has.breaks <- !is.null(dots$breaks) | is.factor(style.col) | is.character(style.col)

		if(!is.null(dots$breaks)) {
			breaks <- dots$breaks

      if(is.numeric(breaks)) {
				brks <- append(breaks, max(style.col))
			} else {
				brks <- classIntervals(style.col, n = length(fill), style = breaks)$brks
			}
		}
	}


	if (style == "single") { # single style

		map <- map +
		       geom_point(aes_string(x = X, y = Y), data = pts, fill = fill, size = size, color = "black", shape = 21) +
		       blank.theme +
		       labs(title = title, x = NULL, y = NULL)

	} else if (has.breaks) { # factor/character or numeric with breaks

		if (is.numeric(style.col)) {
	
			if(is.null(dots$labs)) {
				labs <- prettyLabs(brks)
				cats <- cut(x = style.col, breaks = brks, labels = labs, include.lowest = TRUE)
			} else {
        cats <- cut(x = style.col, breaks = brks, labels = dots$labs, include.lowest = TRUE)
      }

			pts[style] <- cats
		}

		map <- map +
		       geom_point(aes_string(x = X, y = Y, fill = style), data = pts, size = size, color="black", shape=21) +
		       blank.theme +
		       labs(title = title, x=NULL, y=NULL) +
		       scale_fill_manual(values = fill)

	} else { # continuous

		min <- min(pts[style], na.rm = TRUE)
		max <- max(pts[style], na.rm = TRUE)
		map <- map +
		       geom_point(aes_string(x = X, y = Y, fill = style), data = pts, size = size, color="black", shape=21) +
		       blank.theme +
		       labs(title = title, x = NULL, y = NULL) +
		       scale_fill_continuous(low = fill[1], high = fill[2], limits = c(min, max))
	}
	return(map)
}

mapOPAPoly <- function(geom, poly.dat="", id.var="", title="Map!", location=c(-90.031742, 29.996680), zoom=12,
	style="single", fill="black", alpha=.5, piping=FALSE, old.map="new", plot.all=FALSE, map_source = "stamen", map_type = "toner-lite", ...){
 # maps polygons, given either a SpatialPolygons object OR a common geography type and a table of characteristics
 # geom: either a SpatialPolygons object or one of c("parcels", "blocks", "BGs", "tracts", "nbhds", "council")
 # poly.dat: if geom is a geography type, data frame containing information about the geography
 # id.var: if poly.dat is present, the name of the column with common ID vars as geom (eg GeoPINs for parcels or GEOID for Census geogs.)
 # style: either "single" to display all points in the same color or a column of pts to control symbology
 # map_source: declare "source" argument in "get_map" fn
 # map_type: declare "maptype" argument in "get_map" fn

 # optional arguments:
	# breaks: if style refers to a numeric vector, defines the breakpoints. Can either be a numeric vector the same length as fill or the name of a style in classIntervals()
	#   if no breaks are given, symvology will be on a continuous scale
  # alpha: 0-1, how opaque should shapes be?
  # labs: label names (for continuous data with breaks). If not supplied, the function attempts to make them with prettyLabels()
	#if relevant, get geometric data
	dots <- list(...)

	if(is.character(geom)){
		if(geom == "council"){
			geom <- getCouncil()
		} else if(geom == "parcels"){
			geom <- getParcels()
		} else if(geom == "blocks"){
			geom <- getBlocks()
		} else if(geom == "BGs"){
			geom <- getBGs()
		} else if(geom == "tracts"){
			geom <- getTracts()
		} else if(geom == "nbhds"){
			geom <- getNbhds()
		}

		#not very elegant way to figure out which columns should be used as the ids in the match, then combining data frames
		id.ind <- which(names(poly.dat)==id.var)
		compare.cols <- sapply(as.data.frame(geom), function(u){sum(as.character(u) %in% as.character(poly.dat[,id.ind]))})
		match.inds <- which(compare.cols == max(compare.cols))
		names(geom)[match.inds] <- "id.var"
		names(poly.dat)[id.ind] <- "id.var"
		geom$id.var<- as.character(geom$id.var)
		poly.dat$id.var <- as.character(poly.dat$id.var)
		geom <- merge(x=geom, y=poly.dat, all.x=plot.all, duplicateGeoms=FALSE) #The duplicateGeoms calls could cause some unexpected behavior
	}
	geom <- spTransform(geom, CRS("+init=epsg:4326"))

	# convert the SpatialPolygons into a data frame that ggplot can map
	geom@data$id <- as.numeric(rownames(geom@data))
	poly.f <- fortify(geom, region="id")
	names(poly.f)[which(names(poly.f) == "long")] <- "long.xyz.xyz" ##### new 
	names(poly.f)[which(names(poly.f) == "lat")] <- "lat.xyz.xyz" ##### new 
	names(poly.f)[which(names(poly.f) == "group")] <- "group.xyz.xyz" ##### new 
	poly.final <- merge(poly.f, geom@data, by="id")
	
	
	# if making a new map, loads the basemap, otherwise gets the previously made map to add layers
	if(old.map=="new"){
		basemap <- get_map(source = map_source, location = location, maptype = map_type, zoom = zoom)
		map <- ggmap(basemap)
	} else{map <- old.map}

	blank.theme <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
		axis.text.y=element_blank(),axis.ticks=element_blank(),
		axis.title.x=element_blank(),
		axis.title.y=element_blank(),
		panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
		panel.grid.minor=element_blank(),plot.background=element_blank())


	#finding which style to use and setting breaks if relevant
	if(style != "single"){
		style.col <- geom@data[,which(names(geom@data) == style)]
		has.breaks <- !is.null(dots$breaks) | is.factor(style.col) | is.character(style.col)
		if(!is.null(dots$breaks)){
			breaks <- dots$breaks
			if(is.numeric(breaks)){
				brks <- append(breaks, max(style.col, na.rm = TRUE))
			} else{
				brks <- classIntervals(style.col, n = length(fill), style = breaks)$brks
			}
		}
	}

	color=fill
	## Making the map
	# Single style
	if(style=="single"){
		map <- map+
		geom_polygon(aes_string(x="long.xyz.xyz",y="lat.xyz.xyz",group="group.xyz.xyz"), fill=fill, alpha=alpha, color=color, data=poly.final) +
		blank.theme +
		labs(title=title, x=NULL, y=NULL)

	# categorical breaks (includes numeric with breaks)
	} else if(has.breaks){

		if(is.numeric(style.col)){
		
			if(is.null(dots$labs)){
				labs <- prettyLabs(brks)
				cats <- cut(x = unlist(poly.final[style]), breaks = brks, labels = labs, include.lowest=TRUE)
			
			} else{cats <- cut(x = unlist(poly.final[style]), breaks = brks, labels = dots$labs, include.lowest=TRUE)}
			poly.final[style] <- cats
		}

		map <- map+
		geom_polygon(aes_string(x="long.xyz.xyz",y="lat.xyz.xyz",group="group.xyz.xyz", fill=style, color=style), data=poly.final, alpha=alpha) +
		blank.theme +
		labs(title=title, x=NULL, y=NULL) +
		scale_fill_manual(values=fill) +
		scale_color_manual(values=color)

	# continuous numeric
	} else{
		min <- min(poly.final[style], na.rm=TRUE)
		max <- max(poly.final[style], na.rm=TRUE)
		map <- map +
		geom_polygon(aes_string(x="long.xyz.xyz",y="lat.xyz.xyz",group="group.xyz.xyz", fill=style, color=style), data=poly.final, alpha=alpha) +
		blank.theme +
		labs(title=title, x=NULL, y=NULL) +
		scale_fill_continuous(low = fill[1], high = fill[2], limits=c(min, max)) +
		scale_color_continuous(low = color[1], high = color[2], limits=c(min, max)) +
		theme(legend.key.width = unit(1.25, "cm"))
	}
	#add option for black border
	if(piping==TRUE){
	map <- map + geom_path(aes_string(x="long.xyz.xyz", y="lat.xyz.xyz", group="group.xyz.xyz"),color="black", data=poly.final)
	}
	return(map)
}

cleanLeaflet <- function(spatial, fields, simplify.poly = FALSE){
	# Cleans a spatial object for use in a leaflet map.
	# This is useful because the mapOPALeaflet function is not very robust.
	# If you want to do something that isn't supported,
	# you can use this function to put your spatial data in a useable form
	# and then define styles and write geojsons ad hoc
	# spatial: a spatial object (points, lines, polygons) to be mapped
	# fields: the fields you want to keep for the leaflet popup
	# simplify.poly: simplify the polygon geometry?

	# Separate geometry from data and process each, then recombine
	spatial.geom <- geometry(spatial)
	spatial.geom <- spTransform(spatial.geom, latlon.proj)

	if(is(spatial.geom, "SpatialPolygons") & simplify.poly == TRUE){
		spatial.geom <- gSimplify(spatial.geom, tol=0.01, topologyPreserve=TRUE)
	}

	spatial.dat <- spatial@data
	spatial.dat <- spatial.dat[,fields]
	#leaflet doesn't like some special characters including "\". This removes those.
	#If you get an error about a different invalid character, add the character to remove
	remove <- c("\\\\", "\"")
	spatial.dat <- as.data.frame(lapply(spatial.dat, function(x) gsub(paste(remove,collapse="|"), "", x)))

	if(is(spatial.geom, "SpatialPoints")){
		spatial <- SpatialPointsDataFrame(spatial.geom, data = spatial.dat)
		rownames(spatial@coords) <- rownames(spatial@data)
	} else if(is(spatial.geom, "SpatialPolygons")){
		rownames(spatial.dat) <- sapply(slot(spatial.geom, "polygons"), function(x) slot(x, "ID"))
		spatial <- SpatialPolygonsDataFrame(spatial.geom, data = spatial.dat)
	} else if(is(spatial.geom, "SpatialLines")){
		spatial <- SpatialLinesDataFrame(spatial.geom, data = spatial.dat) #may have to change rownames of spatial.dat as with polygons
	} else(stop(paste0("Geometry type: ", class(spatial.geom)[1], "not currently supported")))

	return(spatial)
}

mapOPALeaflet <- function(spatial, fields, style = "", title, base.map = "osm", fill="black", geojson.exists = FALSE, simplify.poly = FALSE, ...){
	# maps a Spatial object on an interactive leaflet map
	# note that even for moderately sized objects, writing the geojson can take a while
	# for now this can only map one object at a time
	# spatial: a spatial object (points, lines, polygons) to be mapped
	# fields: the fields you want to display in the popup
	# title: name of the map (will also be name of the geojson/html file)
	# geojson.exists: is there a geojson file already made? Only set TRUE if you just want to play with the styling
	# simplify.poly: simplify the polygon geometry? (makes the function run faster)
	
	# optional arguments:
	# breaks: if style refers to a numeric vector, defines the breakpoints. Can either be a numeric vector the same length as fill or the name of a style in classIntervals()
	#   if no breaks are given, symvology will be on a continuous scale	
	# see help(styleSingle) for all aesthetic controls
	
	dots <- list(...)
	
	#Defining aesthetics
	if(is.null(dots$col)){
		col <- "black"
	} else{col <- dots$col}
	if(is.null(dots$rad)){
		rad <- 4
	} else{rad <- dots$rad}
	if(is.null(dots$lwd)){
		lwd <- 1
	} else{lwd <- dots$lwd}
	if(is.null(dots$alpha)){
		alpha <- 1
	} else{alpha <- dots$alpha}
	if(is.null(dots$fill.alpha)){
		fill.alpha <- .5
	} else{fill.alpha <- dots$fill.alpha}
	if(is.null(dots$legend.text)){
		legend.text <- title
	} else{legend.text <- dots$legend.text}	
	
	spatial <- cleanLeaflet(spatial = spatial, fields = fields, simplify.poly = simplify.poly)
	
	#Define style
	if(style == "single"){
	
		sty <- styleSingle(col = col, lwd = lwd, alpha = alpha, fill = fill, fill.alpha = fill.alpha, rad = rad)

	} else if(!is.null(dots$breaks)){
		breaks <- dots$breaks
		prop.vec <- unlist(spatial@data[style])
		prop.num <- as.numeric(as.character(prop.vec))
			
		if(is.numeric(breaks)){
			brks <- breaks
		} else{
			brks <- classIntervals(prop.num, n = length(fill), style = breaks)$brks
			brks <- brks[-length(brks)] #this could cause problems with some break styles (but doesn't seem to be so far)
			}
		sty <- styleGrad(prop = style, breaks = brks, style.par = "col", style.val = fill, out=1,
			col = col, lwd = lwd, alpha = alpha, fill.alpha = fill.alpha, rad = rad, leg = legend.text)
	
	} else {  #cat/grad radius not currently supported, though its relatively easy to adapt code
		
		prop.vec <- unlist(spatial@data[style])
		prop.vec <- as.character(prop.vec)	
		
		if(is.null(dots$val)){
			val <- levels(as.factor(prop.vec))
			} else{val <- dots$val}
			
		sty <- styleCat(prop = style, val = val, style.par = "col", style.val = fill, col = col, lwd = lwd, alpha = alpha, fill.alpha = fill.alpha, rad = rad, leg = legend.text)
	
	}
	
	#Make and pull up leaflet map (creates folder with html map in your wd)
	map.name <- title
	leafdat <- paste0(getwd(), "/", title, ".geojson") 

	if(geojson.exists == FALSE){toGeoJSON(data = spatial, name = title)}
	final.map <- leaflet(data = leafdat, style = sty, title=title, base.map=base.map, incl.data=TRUE,  popup=names(spatial))
	browseURL(final.map)
}

mapOPAMultiLeaflet <- function(spatial.list, fields, spatial.names, title, base.map = "osm", fill="black", geojson.exists = FALSE, ...){
	# maps multiple Spatial objects on an interactive leaflet map
	# note that even for moderately sized objects, writing the geojsons can take a while
	# the leafletR package only supports single styles when using multiple
	# spatial.list: a list of spatial objects (points, lines, polygons) to be mapped
	# fields: a list of fields you want to display in the popup
	# spatial.names: vector of the name of each spatial object as it will appear on a legend
	# title: name of the map (will also be name of the geojson/html file)
	# geojson.exists: is there a geojson file already made? Only set TRUE if you just want to play with the styling
	# see help(styleSingle) for all optional aesthetic controls

	# put all aesthetic settings in vectors of length spatial.list
	dots <- list(...)
	num.objects <- length(spatial.list)

	#Defining aesthetics
	if(is.null(dots$col)){
		col <- rep("black", num.objects)
	} else{col <- dots$col}
	if(is.null(dots$rad)){
		rad <- rep(4, num.objects)
	} else{rad <- dots$rad}
	if(is.null(dots$lwd)){
		lwd <- rep(1, num.objects)
	} else{lwd <- dots$lwd}
	if(is.null(dots$alpha)){
		alpha <- rep(1, num.objects)
	} else{alpha <- dots$alpha}
	if(is.null(dots$fill.alpha)){
		fill.alpha <- rep(.5, num.objects)
	} else{fill.alpha <- dots$fill.alpha}

	#Defining styles and making geojsons
	clean.spatial.list <- list()
	sty.list <- list()
	leafdat.list <- list()
	for (i in 1:length(spatial.list)){
		spatial <- cleanLeaflet(spatial = spatial.list[[i]], fields = fields[[i]], simplify.poly = FALSE)
		sty <- styleSingle(col = col[i], lwd = lwd[i], alpha = alpha[i], fill = fill[i], fill.alpha = fill.alpha[i], rad = rad[i])
		layer.name <- spatial.names[i]
		leafdat <- paste0(getwd(), "/", layer.name, ".geojson")
		clean.spatial.list[[i]] <- spatial
		sty.list[[i]] <- sty
		leafdat.list[[i]] <- leafdat
		if(geojson.exists == FALSE){toGeoJSON(data = spatial, name = layer.name)}
	}

	#Make and pull up leaflet map (creates folder with html map in your wd)
	final.map <- leaflet(data = leafdat.list, style = sty.list, title = title, base.map = base.map, incl.data = TRUE,  popup = fields)
	browseURL(final.map)
}


### Analysis
countWithin <- function(within, around, col.name = as.character(bquote(within)), return.type="vec", ..., range){
  # Answers a common spatial analysis question: How many of one object are within another?
  # To adapt this function to answer similar questions, see help(over)
  # within: points, the object to be counted
  # around: polygons, boundaries in which to evaluate, or points to search near
  # return.type: one of c("vec", "poly", "points"). If vec, returns a vector of the counts of within in around.
	# Otherwise returns around with this vector appended to it, either as a SpatialPolygons or SpatialPoints object.
	# Can only use "points" if return.type is a SpatialPoints object
  # optional - range: if around is a SpatialPoints object, the area (in feet) to search around around
	dots <- eval(substitute(alist(...)))
	if(!is.null(dots$range)){
		range <- dots$range
	}

	within.crs <- within@proj4string
	around.crs <- around@proj4string
	within <- spTransform(within, NOLA.proj)
	around <- spTransform(around, NOLA.proj)

	if(is(around, "SpatialPoints") | is(around,"SpatialPointsDataFrame")){
		around.poly <- gBuffer(around, width=range, byid=TRUE)
		count.within <- sapply(over(around.poly,geometry(within), returnList=TRUE),length)
	} else{
		count.within <- sapply(over(around,geometry(within), returnList=TRUE),length)
		around.poly <- around
	}

	#getName <- function(v1){deparse(substitute(v1))}
	#getName <- function(v1){as.character(bquote(v1))}
	#final.name <- paste0("count.of.", col.name)

	if(return.type=="points"){
		around$poly.name <- count.within
		names(around)[which(names(around) == "poly.name")] <- col.name
		around <- spTransform(around, around.crs)
		return(around)
	} else if(return.type=="poly"){
		around.poly$poly.name <- count.within
		names(around.poly)[which(names(around.poly) == "poly.name")] <- col.name
		around.poly <- spTransform(around.poly, around.crs)
		return(around.poly)
	} else if(return.type=="vec"){
		return(count.within)
	}
}

inWhich <- function(within, around, char){
	# For a set of points, finds the characteristics of polygons that each point lies within 
	# this function also works for polygons in other polgyons (and other geometry comparisons), but has not been well tested
	# useful for finding which council district, census tract, etc. a set of points is in
	# within: points for which to find characteristics
	# around: a set of polygons
	# char: column name of around to search for
	within <- spTransform(within, NOLA.proj)
	around <- spTransform(around, NOLA.proj)
	char.df <- over(x = within, y = around)
	char <- unlist(char.df[char])
	char <- unname(char)
	return(char)
}
