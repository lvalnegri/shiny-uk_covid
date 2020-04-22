###########################################################################################################
# CoViD-19 UK * NHS Cases by UTLA and reporting date + Deaths by TRUSTS by both reporting and death dates #
###########################################################################################################
# https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/
# https://coronavirus.data.gov.uk/

pkg <- c('popiFun', 'data.table', 'fst', 'readxl')
invisible(lapply(pkg, require, char = TRUE))

in_path <- file.path(pub_path, 'ext_data', 'uk', 'covid')
out_path <- file.path(pub_path, 'datasets', 'shiny_apps', 'uk_covid')

# Deaths by Age Class
fn <- file.path(in_path, 'age_data.xlsx')
sns <- excel_sheets(fn)
y <- list()
for(sn in sns){
    y1 <- setDT(read_xlsx(fn, sn))
    setnames(y1, 1, 'age')
    y1 <- melt(y1, id.vars = 1, variable.name = 'date_happened', value.name = 'N', variable.factor = FALSE)
    y1 <- y1[!grepl('\\W', date_happened)]
    y1[, date_happened := as.Date(as.numeric(date_happened), origin = '1899-12-30')]
    if(sn != 'T') y1[, date_reported := as.Date(sn)]
    y <- rbindlist(list( y, y1 ), fill = TRUE)
}
y[, age := factor(gsub('[ [:alpha:]]', '', age))]
setcolorder(y, c('date_reported', 'date_happened'))
write_fst(y, file.path(out_path, 'age_data'))

# Deaths by Trusts
fn <- file.path(in_path, 'trust_data.xlsx')
sns <- excel_sheets(fn)
y <- list()
for(sn in sns){
    y1 <- setDT(read_xlsx(fn, sn))
    setnames(y1, 1, 'trust')
    y1 <- melt(y1, id.vars = 1, variable.name = 'date_happened', value.name = 'N', variable.factor = FALSE)
    y1 <- y1[!grepl('\\W', date_happened)]
    y1[, date_happened := as.Date(as.numeric(date_happened), origin = '1899-12-30')]
    if(sn != 'T') y1[, date_reported := as.Date(sn)]
    y <- rbindlist(list( y, y1 ), fill = TRUE)
}
y[, trust := factor(trust)]
y[N > 0, delay := as.numeric(date_reported - date_happened) ]
write_fst(y, file.path(out_path, 'trust_data'))

# Cases by UTLA England
yu <- fread('https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv')
yu[, c('Area name', 'Area type', 'Cumulative lab-confirmed cases') := NULL]
names(yu) <- c('UTLA', 'date_reported', 'cases')
lcns <- readRDS(file.path(out_path, 'locations'))
yu <- yu[lcns[['UTLA']][, .(UTLA, RGN)], on = 'UTLA']
yu[UTLA == 'E09000001' & date_reported == '2020-03-20', cases := 5]
yu[UTLA == 'E09000012' & date_reported == '2020-03-20', cases := 9]
yr <- yu[, .(cases = sum(cases)), .(date_reported, RGN)]
yu[, RGN := NULL]
y <- list('UTLA' = yu, 'RGN' = yr)
saveRDS(y, file.path(out_path, 'cases'))

# exit
rm(list = ls())
gc()
