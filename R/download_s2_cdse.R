get_cdse_credentials <- function() {
  creds <- list(
    username = Sys.getenv("SEN2PROC_CDSE_USERNAME"),
    password = Sys.getenv("SEN2PROC_CDSE_PASSWORD")
  )
  return(creds)
}

#' get token for CDSE
#'
#' @return list of collections per plot
#' @importFrom httr2 oauth_client oauth_flow_password
#' @export
#'
get_cdse_token <- function() {
  creds <- get_cdse_credentials()
  if (any(sapply(creds, function(x) x == ""))) {
    stop("CDSE credentials not set")
  }
  cop_user <- creds$username
  cop_pass <- creds$password
  cop_token_url <- "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token"

  client <- httr2::oauth_client(id = "cdse-public", token_url = cop_token_url)
  token <- httr2::oauth_flow_password(client, username = cop_user, password = cop_pass)
  return(list(client = client, token = token))
}

#' Download Copernicus Dataspace products listed from STAC search.
#'
#' Downloads products from Copernicus Dataspace.
#' Credentials are read from environment variables, so the easiest is to
#' set them in .Renviron or .Rprofile files in your home directory.
#'
#' In ~/.Renviron
#' ```shell
#' SEN2PROC_CDSE_USERNAME="username"
#' SEN2PROC_CDSE_PASSWORD="password"
#' ```
#'
#' In ~/.Rprofile
#' ```r
#' Sys.setenv(
#'  SEN2PROC_CDSE_USERNAME="username",
#'  SEN2PROC_CDSE_PASSWORD="password")
#' ```
#' @param x rstac::doc_items
#' @param outdir character. output dir
#' @param unzip logical. If TRUE (default), the downloaded zip files are unzipped.
#' @param progress logical. If TRUE (default), a progress bar is displayed.
#' @param dry_run logical. If TRUE, the authetication is tried but the download is not performed.
#' @param overwrite logical. If TRUE, existing files are overwritten.
#'
#' @return list of downloaded files
#' @importFrom httr2 request req_auth_bearer_token req_options req_perform req_progress
#' @importFrom utils unzip
#'
#' @examples
#' \dontrun{
#' # search for L2A products from 2016-01-01 to 2018-01-31
#' products <- S2_search(
#'   start = "2016-01-01", end = "2016-01-31",
#'   tile = "34UCF", level = "L2A"
#' )
#'
#' # download the products
#' cdse_download(products, outdir = "./data/download")
#' }
#'
#' @export
cdse_download <- function(
    x, outdir,
    unzip = TRUE,
    progress = TRUE,
    dry_run = FALSE,
    overwrite = FALSE) {
  if (!inherits(x, "doc_items")) {
    stop("x must be of class 'rstac::doc_items'")
  }

  if (!dir.exists(outdir)) {
    stop("Directory not found: ", outdir)
  }

  res <- get_cdse_token()
  client <- res$client
  token <- res$token

  if (dry_run) {
    print("Running as dry run: printing only the destination paths, without downloading files.")
  }

  outfiles <- c()
  for (f in x$features) {
    unzipfile <- file.path(outdir, f$id)
    zipfile <- paste0(unzipfile, ".zip")
    outfile <- ifelse(unzip, unzipfile, zipfile)
    safedir <- paste0(outfile,'.SAFE')
    if ((file.exists(outfile) | dir.exists(outfile)) |
        dir.exists(safedir)  && !overwrite) {
      message(sprintf("Already exists, skipping: %s", outfile))
      if (dir.exists(safedir)){
        outfiles <- c(outfiles, safedir)
      } else {
        outfiles <- c(outfiles, outfile)
      }
      next
    }
    print(sprintf("Downloading: %s", f$id))

    product_url <- f$assets[["Product"]]$href

    if (Sys.time() >= token$expires_at) {
      token <- httr2::oauth_flow_refresh(client, token$refresh_token)
    }

    req <- httr2::request(product_url) |>
      httr2::req_auth_bearer_token(token = token$access_token) |>
      # With r-curl < 5.2.1, unrestricted_auth=1 was the default.
      # Changed since r-curl 5.2.1, see https://github.com/jeroen/curl/blob/master/NEWS
      #
      # It seems necessary to add unrestricted_auth
      # as Copernicus Dataspace would systematically redirect from
      # https://catalogue.copernicus.dataspace.eu/.... to
      # https://download.copernicus.dataspace.eu/....
      # set verbosity=1 at req_perform to see that.
      # See https://github.com/r-lib/httr2/issues/475 for security details.
      # See also https://superuser.com/questions/936042/how-can-i-instruct-curl-to-reuse-credentials-after-it-followed-a-redirect
      httr2::req_options(unrestricted_auth = 1)

    if (!dry_run) {
      if (progress) {
        req <- req |>
          httr2::req_progress()
      }

      # download
      httr2::req_perform(req, zipfile) # add verbosity=1 to see the details
      # file.rename(tmpfile, zipfile)
      if (unzip) {
        print("extracting zip file...")
        utils::unzip(zipfile, exdir = outdir)
        file.remove(zipfile)
      }
    }
    outfiles <- c(outfiles, outfile)
  }
  return(outfiles)
}


#' Search helper for Copernicus DataSpace
#'
#' This helper is wrapper of rstac with
#' the most used filters, to make it easier to search.
#' For more options, use directly rstac.
#' and [rstac::stac()].
#' @param start start date "YYYY-MM-DD" or Date class.
#' @param end end date "YYYY-MM-DD" or Date class. If NULL, end = start + 1 day
#' @param tile MGRS tile, e.g. "34UCF"
#' @param level L1C or L2A
#' @param id_pat pattern to look for in product id, see details
#' @return rstac::doc_items
#' @details
#' The `id_pat` argument is a CQL2 pattern, sued with `id %like% {{id_pat}}`.
#' Character `%` is a wild card for string
#' Character `_` is a wild card for single character
#' Character `\\` is the escape
#' Thus, the argument "%\\_N0500\\_%" is lokking for any id containing string "_N0500_"
#' @examples
#' \dontrun{
#' # search for L2A products from 2016-01-01 to 2018-01-31 with
#' # - tile: 34UCF
#' # - processing baseline: N0500
#'
#' products <- s2_search(
#'   start = "2016-01-01",
#'   end = "2018-01-31",
#'   tile = "34UCF",
#'   level = "L2A",
#'   id_pat = "%\\_N0500\\_%"
#' )
#'
#' # an example with rstac
#' stac_endpoint <- "https://catalogue.dataspace.copernicus.eu/stac"
#' catalog <- stac(stac_endpoint)
#' products <- stac_search(
#'   q = catalog,
#'   collections = "SENTINEL-2",
#'   datetime = "2016-01-01/2018-01-31"
#' ) |>
#'   ext_filter(
#'     tileId == "34UCF" &&
#'       productType == "S2MSI2A" &&
#'       id %like% "%\\_N0500\\_%"
#'   ) |>
#'   post_request() |>
#'   items_fetch()
#' }
#' @export

s2_search <- function(
    start, end = NULL, tile = NULL,
    level = "L2A", id_pat = NULL) {
  # TODO: add filter on orbit number

  stac_endpoint <- "https://stac.dataspace.copernicus.eu/v1"

  start <- as.Date(start)
  if (is.null(end)) {
    end <- start + 1
  } else {
    end <- as.Date(end)
  }

  if (level == "L1C") {
    collections <- "sentinel-2-l1c"
  } else if (level == "L2A") {
    collections <- "sentinel-2-l2a"
  } else {
    stop("Level must be L1C or L2A")
  }

  catalog <- rstac::stac(stac_endpoint)

  start <- format(as.POSIXct(start), format = "%Y-%m-%dT%H:%M:%SZ")
  end <- format(as.POSIXct(end), format = "%Y-%m-%dT%H:%M:%SZ")
  request <- rstac::stac_search(
    q = catalog,
    collections = collections,
    datetime = paste0(start, "/", end)
  )

  # add tile filter
  ef <- c()
  if (!is.null(tile)) {
    tile <- gsub("^T", "", tile, perl = TRUE)
    mgrs_tile = glue::glue("MGRS-{tile}")
    ef <- c(ef, '`grid:code` == {{mgrs_tile}}')
  }

  # add id filter
  if (!is.null(id_pat)) {
    ef <- c(ef, "id %like% {{id_pat}}")
  }

  if (length(ef) > 0) {
    ef <- paste0(ef, collapse = " && ")
    request <- eval(parse(text = sprintf("request |> rstac::ext_filter(%s)", ef)))
  }

  items <- request |>
    rstac::post_request() |>
    rstac::items_fetch()

  return(items)
}




# s2_search <- function(
#     start, end = NULL, tile = NULL,
#     level = "L2A", id_pat = NULL) {
#   # TODO: add filter on orbit number
#   collections <- "SENTINEL-2"
#   start <- as.Date(start)
#   if (is.null(end)) {
#     end <- start + 1
#   } else {
#     end <- as.Date(end)
#   }
#   stac_endpoint <- "https://catalogue.dataspace.copernicus.eu/stac"
#   catalog <- rstac::stac(stac_endpoint)
#   request <- rstac::stac_search(
#     q = catalog,
#     collections = collections,
#     datetime = paste0(start, "/", end)
#   )
#
#   # add level filter
#   if (level == "L1C") {
#     product_type <- "S2MSI1C"
#   } else if (level == "L2A") {
#     product_type <- "S2MSI2A"
#   } else {
#     stop("Level must be L1C or L2A")
#   }
#   ef <- "productType == {{product_type}}"
#
#   # add tile filter
#   if (!is.null(tile)) {
#     tile <- gsub("^T", "", tile, perl = TRUE)
#     ef <- c(ef, "tileId == {{tile}}")
#   }
#
#   # add id filter
#   if (!is.null(id_pat)) {
#     ef <- c(ef, "id %like% {{id_pat}}")
#   }
#
#   ef <- paste0(ef, collapse = " && ")
#   request <- eval(parse(text = sprintf("request |> rstac::ext_filter(%s)", ef)))
#
#   items <- request |>
#     rstac::post_request() |>
#     rstac::items_fetch()
#
#   return(items)
# }


#' search items from Copernicus DataSpace
#'
#' @param collection character. 'SENTINEL-2' as default
#' @param asset_names character.
#' @param productType character.
#' @param tileId character.
#' @param bbox numeric.
#' @param SpatVect SpatVector.
#' @param cloudcover numeric.
#' @param datetime character.
#'
#' @return list of items corresponding to input parameters
#' @importFrom rstac stac stac_search ext_filter post_request items_fetch assets_select items_filter
#' @importFrom terra ext
#' @export

search_items <- function(collection = 'SENTINEL-2', asset_names = 'PRODUCT',
                         productType = "S2MSI2A",
                         tileId = NULL, bbox = NULL, SpatVect = NULL,
                         cloudcover = 100, datetime){

  s_obj <- rstac::stac("https://catalogue.dataspace.copernicus.eu/stac")
  # if SpatVect provided: define bounding box
  if (!is.null(SpatVect)){
    bbox <- terra::ext(SpatVect)
    bbox <- c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
  }

  if (!is.null(tileId) & is.null(bbox)){
    products <- s_obj |> stac_search(
      collections = collection,
      datetime = datetime) |>
      ext_filter(tileId == {{tileId}} &&
                   productType == {{productType}}) |>
      post_request() |>
      items_fetch() |>
      assets_select(asset_names=asset_names) |>
      items_filter(properties$cloudCover < cloudcover)
  } else if (!is.null(bbox) & !is.null(tileId)){
    products <- s_obj |> stac_search(
      collections = collection,
      datetime = datetime, bbox = bbox) |>
      ext_filter(tileId == {{tileId}} &&
                   productType == {{productType}}) |>
      post_request() |>
      items_fetch() |>
      assets_select(asset_names=asset_names) |>
      items_filter(properties$cloudCover < cloudcover)

  } else if (!is.null(bbox) & is.null(tileId)){
    products <- s_obj |> stac_search(
      collections = collection,
      datetime = datetime, bbox = bbox) |>
      ext_filter(productType == {{productType}}) |>
      post_request() |>
      items_fetch() |>
      assets_select(asset_names=asset_names) |>
      items_filter(properties$cloudCover < cloudcover)
  } else if (is.null(bbox) & is.null(tileId)){
    message('define spatial extent as tileID or bbox')
  }


  # check processing baseline and select latest for each acquisition
  processing_baseline_items <- unlist(lapply(lapply(products$features,
                                                    '[[', 'properties'),
                                             '[[', 'processorVersion'))
  processing_baseline_items <- gsub(x = processing_baseline_items, pattern = '99.99', replacement = '00.00')
  datetime_items <- as.Date(unlist(lapply(lapply(products$features,
                                                 '[[', 'properties'),
                                          '[[', 'datetime')))
  name_items <- unlist(lapply(products$features,'[[', 'id'))
  uniqueDate <- unique(datetime_items)
  sel_pb <- c()
  for (acq in uniqueDate){
    sel1 <- which(datetime_items==acq)
    pbl <- processing_baseline_items[sel1]
    if (max(as.numeric(pbl))>0) {
      selpn <- which(as.numeric(pbl)==max(as.numeric(pbl)))
      sel_pb <- c(sel_pb, sel1[selpn])
    }
  }
  if (length(sel_pb)==0) message('no items correspond to processing_baseline')
  products$features <- products$features[sel_pb]
  # get items in chronological order
  if (length(products$features)>0){
    datetime_items <- as.Date(unlist(lapply(lapply(products$features,
                                                   '[[', 'properties'),
                                            '[[', 'datetime')))
    sdate <- sort(as.numeric(datetime_items), index.return = T)
    products$features <- products$features[sdate$ix]
  }
  return(products)
}
