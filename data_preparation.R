##################################
# CoViD-19 UK * Data Preparation #
##################################

# load libraries
pkg <- c('popiFun', 'data.table', 'maptools', 'rgdal', 'rmapshaper')
invisible(lapply(pkg, require, char = TRUE))

# set constants
in_path <- file.path(pub_path, 'ext_data', 'uk', 'covid')
out_path <- file.path(pub_path, 'datasets', 'shiny_apps', 'uk_covid')
lcns <- c('CCG', 'STP', 'NHSO', 'NHSR', 'UTLA', 'RGN')

# build boundaries for all location types
lsoa <- fread(file.path(in_path, 'LSOA.csv'))
build.parent.boundaries <- function(parent, child = 'LSOA'){
    
    # helper function
    merge_subpoly <- function(shp, subarea){
        
        # select all child polygons contained in specified parent polygon
        shp.tmp <- subset(shp, shp[['parent']] == subarea)
        
        # delete interiors
        shp.tmp <- ms_dissolve(shp.tmp)
        
        # define new polygon id
        shp.tmp$id <- subarea
        shp.tmp <- spChFIDs(shp.tmp, as.character(shp.tmp$id))
        
        return(shp.tmp)
        
    }
    
    # load child > parent lookups
    lkp <- unique(lsoa[, .(child = get(child), parent = get(parent))])

    # load child boundaries
    bnd <- readRDS(file.path(in_path, child))

    # join shapefile data slot and lookup table on the child code 
    bnd <- merge(bnd, lkp, by.x = 'id', by.y = 'child')
    
    # Build the list of subareas 
    subareas <- sort(unique(bnd[['parent']]))
    
    # Define first parent polygon
    message('Processing ', parent, ' subarea ', subareas[1], ' - number 1 out of ', length(subareas))
    shp_area <- merge_subpoly(bnd, subareas[1])
    
    # proceed for all other parent polygons, attaching every results to previous object
    for(idx in 2:length(subareas)){
        message('Processing ', parent, ' subarea ', subareas[idx], ' - number ', idx, ' out of ', length(subareas))
        shp_area <- spRbind(shp_area, merge_subpoly(bnd, subareas[idx]))
    }
    
    # delete the rmapshaperid from Polygons
    shp_area <- shp_area[, 'id']
    
    # add data
    dts <- fread(file.path(in_path, paste0(parent, '.csv')))
    shp_area <- merge(shp_area, dts, by.x = 'id', by.y = parent)
    
    # save Polygons
    saveRDS(shp_area, file.path(in_path, parent))

}
build.parent.boundaries('CCG')
build.parent.boundaries('STP')
build.parent.boundaries('NHSO')
build.parent.boundaries('NHSR')
build.parent.boundaries('UTLA')
build.parent.boundaries('RGN')

# condense all boundaries in one list only and save in RDS format
bnd <- list()
for(x in lcns) bnd[[x]] <- readRDS(file.path(in_path, x))
saveRDS(bnd, file.path(out_path, 'boundaries'))

# condense all locations csvs in one single list
y <- list()
for(x in c('trusts', lcns)) y[[x]] <- fread(file.path(in_path, paste0(x, '.csv')))
saveRDS(y, file.path(out_path, 'locations'))

# add coordinates and LSOA to trusts based on postcode
y <- fread(file.path(in_path, 'trusts.csv'))
yn <- names(y)
pc <- read_fst(file.path(geouk_path, 'postcodes'), columns = c('postcode', 'x_lon', 'y_lat'), as.data.table = TRUE)
y <- pc[y, on = 'postcode']
setcolorder(y, yn)
fwrite(y, file.path(in_path, 'trusts.csv'))
write_fst(y, file.path(out_path, 'trusts'))

# add STP to Trusts
bnd <- readRDS(file.path(out_path, 'boundaries'))
bnd <- bnd[['STP']][, 'id']
bnd <- spTransform(bnd, crs.wgs)
yt <- read_fst(file.path(out_path, 'trusts'), as.data.table = TRUE)
yt1 <- yt[, .(x_lon, y_lat) ]
coordinates(yt1)<-~x_lon+y_lat
proj4string(yt1) <- crs.wgs
yt <- cbind(yt, yt1 %over% bnd)
fwrite(yt, 'trusts.csv')
