##########################
# CoViD-19 UK * server.R #
##########################

shinyServer(function(input, output, session) {
    
    # TRUSTS (trs)
    source(file.path("server", "srv_trs.R"),  local = TRUE)$value
    
    # STP (stp)
#    source(file.path("server", "srv_stp.R"),  local = TRUE)$value
    
    # UTLA (utl)
#    source(file.path("server", "srv_ctr.R"),  local = TRUE)$value
    
    # REGIONS (rgn)
#    source(file.path("server", "srv_rgn.R"),  local = TRUE)$value
    
    # COUNTRIES (ctr)
#    source(file.path("server", "srv_ctr.R"),  local = TRUE)$value
    
})
