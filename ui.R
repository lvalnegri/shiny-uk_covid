######################
# CoViD-19 UK * ui.R #
######################

shinyUI(
    
    fluidPage(
    
        includeCSS(file.path(app_path, 'styles.css')),
        includeScript(file.path(pub_path, 'scripts', 'scripts.js')),
        # includeScript(file.path(pub_path, 'scripts', 'google-analytics.js')),
        tags$head(
            tags$link(rel="shortcut icon", href="favicon.ico"),
            tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.13.0/css/all.css"), # 5.8.1 is working
            HTML("<base target='_blank'>") # makes links open in a new tab
        ),
        
        navbarPageWithText(
            header = '',
            title = HTML('<div><img src="logo.png" class="logo"><span class = "title">UK CoViD-19</span></div>'),
            windowTitle = 'UK CoViD-19', 
            id = 'mainNav',
            theme = shinytheme('united'), inverse = TRUE,
            
            # TRUSTS (trs)
            source(file.path("ui", "ui_trs.R"),  local = TRUE)$value,
            
            # STP (stp)
#            source(file.path("ui", "ui_stp.R"),  local = TRUE)$value,
            
            # UTLA (utl)
#            source(file.path("ui", "ui_ctr.R"),  local = TRUE)$value,
            
            # REGIONS (rgn)
#            source(file.path("ui", "ui_rgn.R"),  local = TRUE)$value,
            
            # COUNTRIES (ctr)
#            source(file.path("ui", "ui_ctr.R"),  local = TRUE)$value,
            
            # ABOUT (abt)
#            source(file.path("ui", "ui_abt.R"),  local = TRUE)$value,
            
            # LAST UPDATED AT
            text = paste('Last updated:', format(last_updated, '%d %B') )
        
        ),
    
        introjsUI(),
        useShinyjs()

    )
    
)
