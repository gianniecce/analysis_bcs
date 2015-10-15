
# dta$id = dta[, id]  
# dta$av = dta[, av] 
# dta$time = dta[, time] 
# dta = dta[, c('id', 'av', 'time')] 

# Long to Wide 
TimeLongToWide = function(dta = weekend, id = 'id', av = 'av', time = 'time'){
  
  # Sequence 
  seqDay = dta[rep(1:nrow(dta), dta[,time] ), -3] %>%
    group_by(id) %>% 
    mutate( Time = 1:n() ) %>%
    spread(Time, av)
  
  seqDay = as.data.frame(seqDay)
  
  row.names(seqDay) <- seqDay[,'id']
  seqDay = seqDay [,-1]
  
  return(seqDay)
}





source_https <- function(url, ...) {
  # load package
  require(RCurl)
  
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}

# Example
source_https("https://raw.github.com/tonybreyal/Blog-Reference-Functions/master/R/bingSearchXScraper/bingSearchXScraper.R",
             "https://raw.github.com/tonybreyal/Blog-Reference-Functions/master/R/htmlToText/htmlToText.R")
