library("jsonlite")
library("dplyr")
library("knitr")
library("httr")

source('key script.R') 
query.params <- list(access_token = apikeys.R)
apikeys.R

base.uri <- "http://api.data.gov/ed/collegescorecard/"
resource.uri <- paste0("/v1/schools")
endpoint.data <- paste0(base.uri, resource.uri)
response.data <- GET(endpoint.data, add_headers('X-API-Key' = apikeys.R))
