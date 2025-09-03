#' get id and pwd from CDSE OAuth client
#' https://shapps.dataspace.copernicus.eu/dashboard/#/account/settings
#'
#' @return OAuth_client list of id and pwd for CDSE OAuth client
#' @export

get_OAuth_client  <- function(){
  # get id
  id <- Sys.getenv("OAUTH_CDSE_ID")
  if (nchar(id)==0)
    id <- Sys.getenv("PREPROCS2_CDSE_ID")

  # get pwd
  pwd <- Sys.getenv("OAUTH_CDSE_PWD")
  if (nchar(pwd)==0)
    pwd <- Sys.getenv("PREPROCS2_CDSE_SECRET")
  OAuth_client <- list('id' = id, 'pwd' = pwd)
  return(OAuth_client)
}


