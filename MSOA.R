# MSOA

# load packages
pkgs <- c('popiFun', 'data.table', 'ggplot2', 'leaflet', 'rvest')
lapply(pkgs, require, char = TRUE)

# check site has been updated
y <- fread('https://c19pub.azureedge.net/assets/dispatch/website_timestamp')
if(Sys.Date() == as.Date(names(y))){

# prepare boundaries
bnd <- readRDS(file.path(bnduk_path, 'rds', 's05', 'MSOA'))
# add location name
y <- fread(
        'https://coronavirus.data.gov.uk/downloads/msoa_data/MSOAs_latest.csv',
        select = 7:8,
        col.names = c('id', 'name')
)
bnd <- merge(bnd, y, by = 'id')

# add population, total + age + ethnic
tmp <- tempfile()
download.file('https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fmiddlesuperoutputareamidyearpopulationestimates%2fmid2019sape22dt4/sape22dt4mid2019msoasyoaestimatesunformatted.zip', destfile = tmp)
fname <- unzip(tmp, list = TRUE)
pop <- fread(fname$Name)


# add income


# add deprivation


# add council housing ?


# save in dedicated shiny app folder

# build calendar
cal <- data.table(
        'week' = paste0('wk_', 1:53),
        'day' = as.Date('2019-12-30') + 7 * 0:52
)



####

dts <- fread(
        'https://coronavirus.data.gov.uk/downloads/msoa_data/MSOAs_latest.csv',
        drop = c(1:6, 8),
        na.strings = '-99'
)
dts <- melt(dts, id = 1, na.rm = TRUE)
setnames(dts, c('MSOA', 'week', 'cases'))
dts <-dts[!grepl('x', MSOA)]
dts[, MSOA := factor(MSOA)]
lw <- levels(dts$week)[length(levels(dts$week))]
mw <- cal[week == levels(dts$week)[1], day]

y <- sp::merge(bnd, dts[week == lw, .(MSOA, cases)], by.x = 'id', by.y = 'MSOA')
y@data[is.na(y@data$cases), 'cases'] <- '0-2'

### map
leaflet() %>% 
    addTiles() %>% 
    addPolygons(
        data = y,
        color = ~ifelse(cases == '0-2', 'blue', 'red'),
        weight = ~ifelse(cases == '0-2', 1, sqrt(as.numeric(cases))),
            
        label = ~cases
    )

### time series chart for single MSOA
lid <- 'E02001119'
yg <- dts[MSOA == lid, .(week, cases)]
yg <- yg[cal, on = 'week'][, nacases := cases][is.na(cases), cases := 0]
yg[, ccases := cumsum(cases)]
yg <- yg[day >= mw & week <= lw]

# daily cases


# cumulative cases
ggplot(yg, aes(day, ccases, group = 1)) + 
    geom_line()
