##################################
# CoViD-19 UK * Data Preparation #
##################################

pkg <- c('popiFun', 'data.table', 'maptools', 'rgdal', 'rmapshaper')
invisible(lapply(pkg, require, char = TRUE))

in_path <- file.path(pub_path, 'ext_data', 'uk', 'covid')
out_path <- file.path(pub_path, 'datasets', 'shiny_apps', 'uk_covid')

lsoa <- fread(file.path(in_path, 'lsoa.csv'))

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
    saveRDS(shp_area, file.path(out_path, parent))

}

build.parent.boundaries('CCG')
build.parent.boundaries('STP')
build.parent.boundaries('NHSO')
build.parent.boundaries('NHSR')

bnd <- list()
for(x in c('CCG', 'STP', 'NHSO', 'NHSR')) bnd[[x]] <- readRDS(file.path(out_path, x))
    saveRDS(bnd, file.path(out_path, 'boundaries'))
