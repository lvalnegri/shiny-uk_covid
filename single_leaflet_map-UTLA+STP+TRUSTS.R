####################################
# CoViD-19 UK * Single leaflet map #
####################################

# load libraries
pkg <- c('popiFun', 'data.table', 'fst', 'htmltools', 'htmlwidgets', 'leaflet', 'leaflet.extras')
invisible(lapply(pkg, require, char = TRUE))

# set constant
app_path <- file.path(pub_path, 'datasets', 'shiny_apps', 'uk_covid')

# options for map hover labels
lbl.options <- labelOptions(
    textsize = '12px', 
    direction = 'right', 
    sticky = FALSE, 
    opacity = 0.8,
    offset = c(60, -40), 
    style = list('font-weight' = 'normal', 'padding' = '2px 6px')
)

# options for map hover highlight
hlt.options <- highlightOptions(
          weight = 6,
          color = 'white',
          opacity = 1,
          bringToFront = TRUE
)

# define functions
get_num_sfx <- function(x){
    x <- as.character(x)
    switch(substring(x, nchar(x)),
        '1' = paste0(x, 'st'),
        '2' = paste0(x, 'nd'),
        '3' = paste0(x, 'rd'),
        paste0(x, 'th')
    )
}
add_label_poly <- function(y, type){
    lapply(
        1:nrow(y),
        function(x)
            HTML(
                switch(type,
                    'u' = paste0(
                            '<hr>',
                                '<b>UTLA</b>: ', y$UTLA_name[x], '<br>',
                                '<b>Region</b>: ', y$RGN_name[x], '<br>',
                            '<hr>',
                                '<b>Total Cases</b>: ', format(y$cases[x], big.mark = ','), '<br>',
                                '<b>Cases over 1mln population</b>: ', format(y$pcases[x], big.mark = ','), 
                                    ' (', get_num_sfx(round(sum(y$pcases <= y$pcases[x]) / length(y$pcases) * 100)), ' Q)<br>',
                                # '<b>Weekly % change</b>: ', format(y$wchange[x], big.mark = ','), '%<br>',
                            '<hr>'
                    ),
                    's' = paste0(
                            '<hr>',
                                '<b>STP</b>: ', y$STP_name[x], '<br>',
                                '<b>NHS Region</b>: ', y$NHSR_name[x], '<br>',
                            '<hr>',
                                '<b>Total Deaths</b>: ', format(y$deaths[x], big.mark = ','), '<br>',
                                '<b>Deaths over 1mln population</b>: ', format(y$pdeaths[x], big.mark = ','), 
                                    ' (', get_num_sfx(round(sum(y$pdeaths <= y$pdeaths[x]) / length(y$pdeaths) * 100)), ' Q)<br>',
                                # '<b>Weekly % change</b>: ', format(y$wchange[x], big.mark = ','), '%<br>',
                            '<hr>'
                    ),
                    't' = paste0(
                            '<hr>',
                                '<b>Trust</b>: ', y$name[x], '<br>',
                                '<b>CCG</b>: ', y$CCG_name[x], '<br>',
                                '<b>STP</b>: ', y$STP_name[x], '<br>',
                                '<b>NHS Region</b>: ', y$NHSR_name[x], '<br>',
                            '<hr>',
                                '<b>Total Deaths</b>: ', format(y$deaths[x], big.mark = ','), '<br>',
                                # '<b>Weekly % change</b>: ', format(y$wchange[x], big.mark = ','), '%<br>',
                            '<hr>'
                    )
                )
            )
    )
}

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
yu1 <- dts.u[, .(cases = sum(cases)), UTLA]
yu2 <- dts.t[is.na(date_reported)][yt[, .(code, UTLA)], on = c(trust = 'code')][, .(deaths = sum(N, na.rm = TRUE)), UTLA]
yu <- yu2[yu1, on = 'UTLA'][is.na(deaths), deaths := 0]
yu[, fatality_rate := round(100 * deaths / cases, 2)]

# augment UTLA boundaries
bnd.utla <- bnd[['UTLA']]
bnd.utla <- merge(bnd.utla, yu, by.x = 'id', by.y = 'UTLA')
bnd.utla$pcases <- round(bnd.utla$cases * 1000000 / bnd.utla$popT)
bnd.utla$pdeaths <- round(bnd.utla$deaths * 1000000 / bnd.utla$popT)

# calculate palettes over cases for UTLA polygons 
pal.cases <- colorQuantile('BuPu', bnd.utla$cases, 9)
pal.pcases <- colorQuantile('BuPu', bnd.utla$pcases, 9)

# augment STP boundaries
bnd.stp <- bnd[['STP']]
ys <- dts.t[is.na(date_reported)][yt[, .(code, STP)], on = c(trust = 'code')][, .(deaths = sum(N, na.rm = TRUE)), STP]
bnd.stp <- merge(bnd.stp, ys, by.x = 'id', by.y = 'STP')
bnd.stp$pdeaths <- round(bnd.stp$deaths * 1000000 / bnd.stp$popT)

# calculate palettes over deaths for STP polygons 
pal.deaths <- colorQuantile('YlOrRd', bnd.stp$deaths, 9)
pal.pdeaths <- colorQuantile('YlOrRd', bnd.stp$pdeaths, 9)

# base layer
mp <- leaflet(options = leafletOptions(minZoom = 6)) %>% 
        # addProviderTiles(providers$Esri.WorldTopoMap) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addResetMapButton() 

# round choices (UTLA): 1) clear map
mp <- mp %>% 
    addPolygons(
        data = bnd.utla,
        group = 'UTLA Clear',
        fillOpacity = 0,
        color = 'blue',
        weight = 0.8,
        smoothFactor = 0.2,
#        highlightOptions = hlt.options,
        label = add_label_poly(bnd.utla, 'u'),
        labelOptions = lbl.options
    )

# round choices (UTLA): 2) cases map
mp <- mp %>% 
    addPolygons(
        data = bnd.utla,
        group = 'UTLA Cases',
        fillColor = ~pal.cases(cases),
        fillOpacity = 0.7,
        color = 'gray',
        weight = 0.4,
        smoothFactor = 0.2,
        highlightOptions = hlt.options,
        label = add_label_poly(bnd.utla, 'u'),
        labelOptions = lbl.options
    ) 
    # ) %>% 
    # addLegend( 
    #     data = bnd.utla,
    #     group = 'UTLA Cases',
    #     pal = pal.cases, 
    #     values = ~cases, 
    #     opacity = 0.9, 
    #     title = 'CoViD19 Cases by UTLA', 
    #     position = 'bottomright'
    # )

# round choices (UTLA): 3) 1 mln pop cases map
mp <- mp %>% 
    addPolygons(
        data = bnd.utla,
        group = 'UTLA Cases vs 1mln Population',
        fillColor = ~pal.pcases(pcases),
        fillOpacity = 0.7,
        color = 'gray',
        weight = 0.4,
        smoothFactor = 0.2,
        highlightOptions = hlt.options,
        label = add_label_poly(bnd.utla, 'u'),
        labelOptions = lbl.options
    )

# round choices (STP): 1) clear map
mp <- mp %>% 
    addPolygons(
        data = bnd.stp,
        group = 'STP Clear',
        fillOpacity = 0,
        color = 'red',
        weight = 0.8,
        smoothFactor = 0.2,
#        highlightOptions = hlt.options,
        label = add_label_poly(bnd.stp, 's'),
        labelOptions = lbl.options
    )

# round choices (STP): 2) death map
mp <- mp %>% 
    addPolygons(
        data = bnd.stp,
        group = 'STP Deaths',
        fillColor = ~pal.deaths(deaths),
        fillOpacity = 0.7,
        color = 'gray',
        weight = 0.4,
        smoothFactor = 0.2,
        highlightOptions = hlt.options,
        label = add_label_poly(bnd.stp, 's'),
        labelOptions = lbl.options
    )

# round choices (STP): 3) 1 mln pop deaths map
mp <- mp %>% 
    addPolygons(
        data = bnd.stp,
        group = 'STP Deaths vs 1mln Population',
        fillColor = ~pal.pdeaths(pdeaths),
        fillOpacity = 0.7,
        color = 'gray',
        weight = 0.4,
        smoothFactor = 0.2,
        highlightOptions = hlt.options,
        label = add_label_poly(bnd.stp, 's'),
        labelOptions = lbl.options
    )

# check choices (TRUSTS): 1) no deaths (green, all same size)
yt0 <- yt[deaths == 0]
grp.y0 <- paste0('NHS Trusts No Deaths (', nrow(yt0), ')')
mp <- mp %>% 
    addCircleMarkers(
        data = yt0,
        group = grp.y0,
        lng = ~x_lon, lat = ~y_lat,
        radius = 6,
        color = 'black', 
        weight = 2,
        fillColor = 'green', 
        fillOpacity = 0.7,
        label = add_label_poly(yt0, 't'),
        labelOptions = lbl.options
    )

# check choices (TRUSTS): 2) death < threshold 
yt1 <- yt[deaths > 0 & deaths < q75]
grp.y1 <- paste0('Hospitals Deaths Less Than ', q75, ' (', nrow(yt1), ')')
mp <- mp %>% 
    addCircleMarkers(
        data = yt1,
        group = grp.y1,
        lng = ~x_lon, lat = ~y_lat,
        color = 'black', 
        weight = 1,
        fillColor = 'red', 
        fillOpacity = 0.7,
        label = add_label_poly(yt1, 't'),
        labelOptions = lbl.options
    )

# check choices (TRUSTS): 3) death >= threshold 
yt2 <- yt[deaths >= q75]
grp.y2 <- paste0('Hospitals Deaths More Than ', q75, ' (', nrow(yt1), ')')
mp <- mp %>% 
    addCircleMarkers(
        data = yt2,
        group = grp.y2,
        lng = ~x_lon, lat = ~y_lat,
        color = 'black', 
        weight = 1,
        fillColor = 'red', 
        fillOpacity = 0.7,
        label = add_label_poly(yt2, 't'),
        labelOptions = lbl.options
    )

# Add upper right menu for controls
mp <- mp %>% 
    addLayersControl(
    	baseGroups = c(
    	    'UTLA Clear', 'UTLA Cases', 'UTLA Cases vs 1mln Population',
    	    'STP Clear', 'STP Deaths', 'STP Deaths vs 1mln Population'
    	),
        overlayGroups = c(grp.y0, grp.y1, grp.y2),
    	options = layersControlOptions(collapsed = FALSE)
    ) 

# Add title
mp # <- mp %>% 



# saveWidget(mp, paste0('CoViD19_UK-', Sys.Date(), '.html'))
