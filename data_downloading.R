##################################
# CoViD-19 UK * Data Downloading #
##################################

pkg <- c('popiFun', 'data.table', 'fst')
invisible(lapply(pkg, require, char = TRUE))

lkp <- fread(file.path(pub_path, 'ext_data', 'uk', 'covid', 'lookups.csv'), select = c('CCGCode', 'CCG'))

# file names prefix
fns <- c(
    'https://files.digital.nhs.uk/DB/31E103/NHS%20Pathways%20Covid-19%20data%20',
    'https://files.digital.nhs.uk/99/7529F2/111%20Online%20Covid-19%20data_'
)

# Calls
y1 <- fread(
        paste0(fns[1], (Sys.Date() - 2), '.csv'), 
        select = c('SiteType', 'Call Date', 'Gender', 'AgeBand', 'CCGCode', 'TriageCount'),  
        col.names = c('type', 'datefield', 'gender', 'age', 'CCGCode', 'N') 
)
y1 <- y1[gender != 'Unknown' & age != '' & substr(CCGCode, 1, 1) == 'E']
y1 <- lkp[y1, on = 'CCGCode'][, CCGCode := NULL]
y1[type == 111, type := 1]
y1[type == 999, type := 9]

# Online
y2 <- fread(
        paste0(fns[2], (Sys.Date() - 2), '.csv'), 
        select = c('journeydate', 'gender', 'ageband', 'CCGCode', 'Total'),  
        col.names = c('datefield', 'gender', 'age', 'CCGCode', 'N') 
)
y2 <- y2[gender != 'Unknown' & age != '' & substr(CCGCode, 1, 1) == 'E']
y2 <- lkp[y2, on = 'CCGCode'][, CCGCode := NULL]
y2 <- y2[, .(N = sum(N)) , .(datefield, gender, age, CCG)]
y2[, type := 0]

# Union
y <- rbindlist(list( y1, y2 ), use.names = TRUE)
y <- y[, .(N = sum(N)) , .(type, datefield, gender, age, CCG)]
y[, `:=`( datefield = as.Date(datefield), gender = factor(gender), age = factor(age), CCG = factor(CCG) )]

# save
write_fst(y, file.path(pub_path, 'datasets', 'shiny_apps', 'uk_covid', 'dataset'))

# exit
rm(list = ls())
gc()
