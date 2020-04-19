####################################
# CoViD-19 UK * Single leaflet map #
####################################

# load libraries
pkg <- c('popiFun', 'data.table', 'fst', 'leaflet', 'leaflet.extras')
invisible(lapply(pkg, require, char = TRUE))

# set constant
app_path <- file.path(pub_path, 'datasets', 'shiny_apps', 'uk_covid')

# define functions


# read boundaries
bnd <- readRDS(file.path(app_path, 'boundaries'))

# read trust deaths data
yt <- readRDS(file.path(app_path, 'locations'))
yt <- yt[['trusts']]
dts.t <- read_fst(file.path(app_path, 'trust_data'), as.data.table = TRUE)
yt <- dts.t[is.na(date_reported), .(deaths = sum(N)), .(code = trust)][yt, on = 'code'][is.na(deaths), deaths := 0]
q75 <- as.numeric(quantile(yt$deaths, 0.75))

# read UTLA cases data
dts.u <- readRDS(file.path(app_path, 'cases'))
dts.u <- dts.u[['UTLA']]

# augment UTLA boundaries
bnd.utla <- bnd[['UTLA']]
bnd.utla <- merge(bnd.utla, dts.u[, .(cases = sum(cases)), UTLA], by.x = 'id', by.y = 'UTLA')
bnd.utla$pcases <- round(bnd.utla$cases * 1000000 / bnd.utla$popT)

# calculate palettes over cases for UTLA polygons 
pal.cases <- colorQuantile('YlOrRd', bnd.utla$cases, 9)
pal.pcases <- colorQuantile('YlOrRd', bnd.utla$pcases, 9)

# augment STP boundaries
bnd.stp <- bnd[['STP']]
ys <- dts.t[is.na(date_reported)][yt[, .(code, STP)], on = c(trust = 'code')][, .(deaths = sum(N, na.rm = TRUE)), STP]
bnd.stp <- merge(bnd.stp, ys, by.x = 'id', by.y = 'STP')
bnd.stp$pdeaths <- round(bnd.stp$deaths * 1000000 / bnd.stp$popT)

# calculate palettes over deaths for STP polygons 
pal.deaths <- colorQuantile('YlOrRd', bnd.stp$deaths, 9)
pal.pdeaths <- colorQuantile('YlOrRd', bnd.stp$pdeaths, 9)


leaflet(options = leafletOptions(minZoom = 6)) %>% 
    # addProviderTiles(providers$Esri.WorldTopoMap) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addResetMapButton() %>% 
    # round choices (UTLA): 1) clear map
    addPolygons(
        data = bnd.utla,
        group = 'UTLA Clear',
        fillOpacity = 0,
        stroke = TRUE,
        color = 'red',
        weight = 0.4,
        label = ~paste(UTLA_name)
    ) %>% 
    # round choices (UTLA): 2) cases map
    addPolygons(
        data = bnd.utla,
        group = 'UTLA Cases',
        fillColor = ~pal.cases(cases),
        fillOpacity = 0.7,
        stroke = FALSE,
        color = 'gray',
        weight = 0.4,
        label = ~paste(UTLA_name)
    ) %>%
    # round choices (UTLA): 3) 1 mln pop cases map
    addPolygons(
        data = bnd.utla,
        group = 'UTLA Cases vs 1mln Population',
        fillColor = ~pal.pcases(pcases),
        fillOpacity = 0.7,
        stroke = FALSE,
        color = 'gray',
        weight = 0.4,
        label = ~paste(UTLA_name)
    ) %>%
    # round choices (STP): 1) clear map
    addPolygons(
        data = bnd.stp,
        group = 'STP Clear',
        fillOpacity = 0,
        stroke = TRUE,
        color = 'blue',
        weight = 0.4,
        label = ~paste(STP_name)
    ) %>%
    # round choices (STP): 2) death map
    addPolygons(
        data = bnd.stp,
        group = 'STP Deaths',
        fillColor = ~pal.deaths(deaths),
        fillOpacity = 0.7,
        stroke = FALSE,
        color = 'gray',
        weight = 0.4,
        label = ~paste(STP_name)
    ) %>%
    # round choices (STP): 3) 1 mln pop deaths map
    addPolygons(
        data = bnd.stp,
        group = 'STP Deaths vs 1mln Population',
        fillColor = ~pal.pdeaths(pdeaths),
        fillOpacity = 0.7,
        stroke = FALSE,
        color = 'gray',
        weight = 0.4,
        label = ~paste(STP_name)
    ) %>%
    # check choices (TRUSTS): 1) no deaths (green, all same size)
    addCircleMarkers(
        data = yt[deaths == 0],
        group = 'NHS Trusts No Deaths',
        lng = ~x_lon, lat = ~y_lat,
        color = 'green', weight = 1,
        label = ~name
    ) %>%
    # check choices (TRUSTS): 2) death < threshold (?)
    addCircleMarkers(
        data = yt[deaths < q75],
        group = paste('Hospitals Deaths Less Than', q75),
        lng = ~x_lon, lat = ~y_lat,
        color = 'red', weight = 1,
        label = ~name
    ) %>%
    # check choices (TRUSTS): 3) death > threshold (?)
    addCircleMarkers(
        data = yt[deaths >= q75],
        group = paste('Hospitals Deaths More Than', q75),
        lng = ~x_lon, lat = ~y_lat,
        color = 'red', weight = 1,
        label = ~name
    ) %>%
    addLayersControl(
    	baseGroups = c(
    	    'UTLA Clear', 'UTLA Cases', 'UTLA Cases vs 1mln Population',
    	    'STP Clear', 'STP Deaths', 'STP Deaths vs 1mln Population'
    	),
        overlayGroups = c(
            'Legend UTLA', 'Legend STP', 'Legend Hospitals',
            'NHS Trusts No Deaths', paste('Hospitals Deaths Less Than', q75), paste('Hospitals Deaths More Than', q75)
        )
    ) 
