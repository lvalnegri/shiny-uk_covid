####################################
# CoViD-19 UK * Pillar 1 & 2 cases #
####################################

# load libraries
pkg <- c('popiFun', 'data.table', 'htmltools', 'htmlwidgets', 'leaflet', 'leaflet.extras', 'sp')
invisible(lapply(pkg, require, char = TRUE))

# set constants
data_path <- file.path(pub_path, 'datasets', 'shiny_apps', 'uk_covid')
app_path <- '/srv/shiny-server/uk_covid_rates_utla/'
grps <- c('Weekly Rate', 'Weekly Rate WoW', 'Total Rate', 'Total Rate WoW')
lcn <- 'UTLA' # c('CTRY', 'RGN', 'UTLA', 'LTLA')

# define functions
add_label_poly <- function(y, type){
    lapply(
        1:nrow(y),
        function(x)
            HTML(
                '<hr>',
                    '<b>', type, '</b>: ', y[UTLA_name[x], '<br>',
                    '<b>Region</b>: ', y$RGN_name[x], '<br>',
                '<hr>',
                    '<b>Weekly Cases</b>: ', y$wc[x], '<br>',
                    '<b>Weekly Rate</b>: ', y$wr[x], '<br>',
                    '<b>Rate Change WoW</b>: ', y$wd[x], '<br><br>',
                    '<b>Cumulative Cases</b>: ', y$cc[x], '<br>',
                    '<b>Cumulative Rate</b>: ', y$cr[x], '<br>',
                    '<b>C. Rate Change</b>: ', y$cd[x], '<br>',
                '<hr>',
                    '<b>Population</b>: ', format(y$popT[x], big.mark = ','), '<br>',
                    '<b>Density</b>: ', format(round(y$density[x]), big.mark = ','), '<br>',
                    '<b>Median Income</b>: ', format(round(y$income[x]), big.mark = ','), '<br>',
                '<hr>'
            )
    )
}

# read boundaries
boundaries <- readRDS(file.path(data_path, 'boundaries'))

# read data
dts <- fread(
        'https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv',
        select = c('Area code', 'Area type', 'Specimen date', 'Daily lab-confirmed cases')
)
yl <- yu[`Area type` == 'Lower tier local authority'][, `Area type` := NULL] 

wn <- as.numeric(gsub('\\D', '', names(dts)[2]))
names(dts) <- gsub('\\d', '', names(dts))
# dts[, wr := as.numeric(gsub('-', '', wr))]

# augment UTLA boundaries
bnd <- boundaries[['UTLA']]
bnd <- merge(bnd, dts, by.x = 'id', by.y = 'UTLA')
bnd$tc <- sum(round(bnd$cr * bnd$popT / 100000))
bnd$wc <- sum(round(bnd$wr * bnd$popT / 100000))

# calculate palettes  
pal.wr <- colorQuantile('BuPu', bnd$wr, 10)
pal.wd <- colorNumeric('RdBu', bnd$wd, 11, reverse = TRUE)
pal.cr <- colorQuantile('BuPu', bnd$cr, 10)
pal.cd <- colorBin('YlOrRd', bnd$cd, 9)

# base layer
mp <- leaflet(options = leafletOptions(minZoom = 6)) %>% 
        # addProviderTiles(providers$Esri.WorldTopoMap) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addResetMapButton() 


# weekly map
mp <- mp %>% 
    addPolygons(
        data = bnd,
        group = grps[1],
        fillColor = ~pal.wr(wr),
        fillOpacity = 0.7,
        color = 'gray',
        weight = 0.4,
        smoothFactor = 0.2,
        highlightOptions = hlt.options,
        label = add_label_poly(bnd),
        labelOptions = lbl.options
    ) %>%
    addLegend(
        data = bnd,
        group = grps[1],
        pal = pal.wr,
        values = ~wr,
        opacity = 0.7,
        title = grps[1],
        position = 'bottomright'
    )

# Weekly WoW Change
mp <- mp %>% 
    addPolygons(
        data = bnd,
        group = grps[2],
        fillColor = ~pal.wd(wd),
        fillOpacity = 0.7,
        color = 'gray',
        weight = 0.4,
        smoothFactor = 0.2,
        highlightOptions = hlt.options,
        label = add_label_poly(bnd),
        labelOptions = lbl.options
    ) %>%
    addLegend(
        data = bnd,
        group = grps[2],
        pal = pal.wd,
        values = ~wd,
        opacity = 0.7,
        title = grps[2],
        position = 'bottomright'
    )

# Cumulative
mp <- mp %>% 
    addPolygons(
        data = bnd,
        group = grps[3],
        fillColor = ~pal.cr(cr),
        fillOpacity = 0.7,
        color = 'gray',
        weight = 0.4,
        smoothFactor = 0.2,
        highlightOptions = hlt.options,
        label = add_label_poly(bnd),
        labelOptions = lbl.options
    ) %>%
    addLegend(
        data = bnd,
        group = grps[3],
        pal = pal.cr,
        values = ~cr,
        opacity = 0.7,
        title = grps[3],
        position = 'bottomright'
    )

# Cumulative WoW Change
mp <- mp %>% 
    addPolygons(
        data = bnd,
        group = grps[4],
        fillColor = ~pal.cd(cd),
        fillOpacity = 0.7,
        color = 'gray',
        weight = 0.4,
        smoothFactor = 0.2,
        highlightOptions = hlt.options,
        label = add_label_poly(bnd),
        labelOptions = lbl.options
    ) %>%
    addLegend(
        data = bnd,
        group = grps[4],
        pal = pal.cd,
        values = ~cd,
        opacity = 0.7,
        title = grps[4],
        position = 'bottomright'
    )

# Add upper right menu for controls
mp <- mp %>% 
    addLayersControl( baseGroups = grps, options = layersControlOptions(collapsed = FALSE) ) 

# Add title
mp <- mp %>% 
        addControl(
            tags$div(HTML(paste0(
                '<p style="font-size:20px;padding:10px 5px 10px 10px;margin:0px;background-color:#FFD5C6;">',
                    'England CoViD-19 Cases Rates by UTLA <br><br>',
                    'Last Report: Week ', (wn + 1), ', data up to ', format(as.Date(paste0(2020, wn, 1), '%Y%U%u') + 1, '%d %B'), ' <br><br>',
                    'Source: <a href="https://www.gov.uk/government/publications/national-covid-19-surveillance-reports">Public Health England</a>',
                '</p>'
            ))),
            position = 'bottomleft'
        )

# save map as html in shiny server app folder
saveWidget(mp, file.path(app_path, 'index.html'))

# clean
rm(list = ls())
gc()
