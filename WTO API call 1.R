
library(httr)
library(dplyr)
library(jsonlite)

years <- '2010-2018'

#user must insert subscription key

headers = c(
  # Request headers
  'Ocp-Apim-Subscription-Key' = 'insert_your_key'
)

params = list()
# Request parameters
params['i'] = 'ITS_CS_AM6'
params['r'] = 'all'
#840 = US
params['p'] = '000'
params['ps'] = years
params['pc'] = 'SOX'
params['spc'] = 'true'
params['fmt'] = 'json'
params['mode'] = 'full'
params['dec'] = 'default'
params['off'] = '0'
params['max'] = '1000000'
params['head'] = ''
params['lang'] = '1'
params['meta'] = 'false'


resp <- GET(paste0("https://api.wto.org/timeseries/v1/data?"
                   , paste0(names(params),'=',params,collapse = "&")),
            add_headers(headers))

if(!http_error(resp)){
  jsonRespText<-fromJSON(rawToChar(content(resp,encoding = 'UTF-8')))$Dataset
  jsonRespText
}else{
  stop('Error in Response')
}

#doesn't get all sub-categories
#need to loop through all cateogories not just SOX
data1 <- jsonRespText

