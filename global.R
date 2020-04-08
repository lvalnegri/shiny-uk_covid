##########################
# CoViD-19 UK * global.R #
##########################

# load packages
pkg <- c('popiFun',
    'Cairo', 'classInt', 'data.table', 'DT', 'fontawesome', 'fst', 
    'ggplot2', 'ggthemes', 'leaflet', 'leaflet.extras', 'leafpop', 'RColorBrewer',
    'shiny', 'shinycssloaders', 'shinyjs', 'shinythemes', 'shinyWidgets'
)
invisible(lapply(pkg, require, char = TRUE))

# set options 
options(spinner.color = '#333399', spinner.size = 1, spinner.type = 4)
options(bitmapType = 'cairo', shiny.usecairo = TRUE)
options(warn = -1)

# set constants
app_path <- file.path(pub_path, 'datasets', 'shiny_apps', 'uk_covid')

msr.lst <- c('Totale Casi' = 'N', 'Variazione' = 'V1', 'Attack Rate' = 'R')

# load data
bnd <- readRDS(file.path(app_path, 'boundaries'))
dts <- read_fst(file.path(app_path, 'dataset'), as.data.table = TRUE)

last_updated <- dts[, max(datefield)]
dates <- unique(dts[, datefield])

# add text updated date on the upper right menu
navbarPageWithText <- function(..., text) {
    navbar <- navbarPage(...)
    textEl <- tags$p(class = "navbar-text", text)
    navbar[[3]][[1]]$children[[1]] <- tagAppendChild( navbar[[3]][[1]]$children[[1]], textEl)
    navbar
}

# basemap
mp <- leaflet(options = leafletOptions(minZoom = 6)) %>% 
        addProviderTiles(providers$Wikimedia, group = 'Wikimedia') %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik, group = 'OSM Mapnik') %>%
        addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = 'OSM B&W') %>%
        addProviderTiles(providers$Stamen.Toner, group = 'Stamen Toner') %>%
        addProviderTiles(providers$Stamen.TonerLite, group = 'Stamen Toner Lite') %>%
        addProviderTiles(providers$Hydda.Full, group = 'Hydda Full') %>%
        addProviderTiles(providers$Hydda.Base, group = 'Hydda Base') %>% 
        addResetMapButton()

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

# build map hover labels
add_label_prv <- function(y, x, add_v1){
    z <- paste0(
            '<hr>',
                '<b>Provincia</b>: ', y$descrizione[x], '<br>',
            '<hr>',
                '<b>Totale Casi</b>: ', format(y$[x]N, big.mark = ','), '<br>',
                '<b>Variazione giornaliera</b>: ', format(y$[x]V0, big.mark = ','), '%<br>',
                '<b>Attack Rate</b>: ', format(y$[x]R, big.mark = ','), '%<br>'
    )
    if(!is.null(add_v1))
        if(add_v1 > 1) z <- paste0(z, '<br><b>Variazione a ', V1, ' giorni</b>: ', format(y$V1[x], big.mark = ','), '%<br>')
    z <- paste0(z, '<hr>')
    HTML(z)
}

# helper for "get_map_legend"
get_fxb_labels <- function(y, dec.fig = 1){
    y <- gsub('^ *|(?<= ) | *$', '', gsub('(?!\\+|-|\\.)[[:alpha:][:punct:]]', ' ', y, perl = TRUE), perl = TRUE)
    y <- paste(y, collapse = ' ')
    y <- gsub('*\\+', Inf, y)
    y <- gsub('*\\-', -Inf, y)
    y <- unique(sort(as.numeric(unlist(strsplit(y, split = ' ')))))
    lbl_brks <- format(round(y, 3), nsmall = dec.fig)
    lbl_brks <- stringr::str_pad(lbl_brks, max(nchar(lbl_brks)), 'left')
    data.table( 
        'lim_inf' = lbl_brks[1:(length(lbl_brks) - 1)],
        'lim_sup' = lbl_brks[2:length(lbl_brks)],
        'label' = sapply(1:(length(lbl_brks) - 1), function(x) paste0(lbl_brks[x], ' ─ ', lbl_brks[x + 1]) )
    )
}
# build labels colour + text for the legend
get_map_legend <- function(mtc, brks, dec.fig = 1) {
    lbl <- get_fxb_labels(brks, dec.fig)
    brks <- sort(as.numeric(unique(c(lbl$lim_inf, lbl$lim_sup))))
    mtc <- data.table('value' = mtc)
    mtc <- mtc[, .N, value][!is.na(value)]
    mtc[, label := cut(value, brks, lbl$label, ordered = TRUE)]
    mtc <- mtc[, .(N = sum(N)), label][order(label)][!is.na(label)]
    mtc <- mtc[lbl[, .(label)], on = 'label'][is.na(N), N := 0]
    mtc[, N := format(N, big.mark = ',')]
    ypad <- max(nchar(as.character(mtc$N))) + 3
    mtc[, label := paste0(label, stringr::str_pad(paste0(' (', N, ')'), ypad, 'left'))]
    mtc$label
}

# build plots for popups
get_plot <- function(x, z = 'PRV'){
    y <- dts[[z]][N > 0 & get(z) == x, .(data, T = N)][, N := T - shift(T)][is.na(N), N := T]
    ggplot(y) + 
        geom_line(aes(data, T, group = 1)) +
        geom_col(aes(data, N)) +
        geom_text(aes(label = T, x = data, y = T), colour = 'black', size = 2, hjust = 1, vjust = -2, show.legend = FALSE) +
        labs(title = paste('Provincia di', lcn[[z]][get(z) == x, provincia]), x = '', y = '') +
        theme_minimal()
}

# constants to be possibly substituted by controls in app
lvlStrokeWeight = 5
lvlStrokeColor = 'black'
areaFillOpacity = 0.5
areaStrokeWeight = 2
areaStrokeOpacity = 0.7
areaStrokeColor = 'slateblue'
areaStrokeDash = '3'
n_breaks <- 7
fxd_brks <- c(0, 0.10, 0.25, 0.5, 1.00, 2.5, 5.0, 10.00, 25.00, 50.00, 75.00, 100.00)
cls_mth <- 'equal' # choose from: fixed, equal, quantile, pretty, hclust, kmeans
n_brks <- ifelse(cls_mth == 'fixed', length(fxd_brks) - 1, n_breaks )
use_palette <- FALSE
br_pal = 'OrRd' # see ColorBrewer website for sequential Palettes: http://colorbrewer2.org/#type=sequential&n=9
rev_cols <- FALSE
fxd_cols <- c('#ffeda0', '#feb24c', '#f03b20')

