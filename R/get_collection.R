#' gets collections for plot network using microsoft planetary
#'
#' @param aoi simple feature including a unique polygon
#' @param S2tiles list. list of S2 tiles including each sf from list_aoi
#' @param datetime list. define period 'from' / 'to' day of acquisition
#' @param FileName character. filename for collection
#' @param overwrite boolean. should collection and S2 data be overwritten?
#' @param cloudcover numeric. maximum cloud cover over tile
#' @param collection character. collection targeted with CDSE
#' @param stac_url character. URL for STAC endpoint

#' @return list of collections per plot
#' @importFrom rstac stac ext_filter post_request
#' @importFrom sf st_transform
#' @importFrom stringr str_split
#' @importFrom magrittr %>%
#' @export
#'
get_collection <- function(aoi, S2tiles = NULL, datetime, FileName, overwrite = T,
                           cloudcover = 100, collection = "sentinel-2-l2a",
                           stac_url = NULL ){

  # get collection for plot, datetime and cloud cover
  if (! file.exists(FileName) | overwrite==T){
    if (is.null(stac_url))
      stac_source <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")
    if (!is.null(stac_url))
      stac_source <- rstac::stac(stac_url)
    aoistac <- aoi |>
      sf::st_transform(4326)
    # define start and end days
    startday <- as.character(datetime$from)
    endday <- as.character(datetime$to)
	# avoid notes when building package
	`eo:cloud_cover` <- s_intersects <- geometry <- anyinteracts <- interval <- s_intersects <- NULL
    # download collection
    collection_plot <- stac_source %>%
      ext_filter(collection == {{collection}} && `eo:cloud_cover` <= {{cloudcover}}
                 && s_intersects(geometry, {{aoistac}})
                 && anyinteracts(datetime, interval({{startday}}, {{endday}}))
      ) %>%
      post_request()

    if (collection=='sentinel2-l2a-sen2lasrc')
      collection_plot <- collection_plot |>
      rstactheia::items_sign_theia()

    # stop process if no image available
    if (length(collection_plot$features)==0){
      message('No acquisition available for area of interest at the specified dates of acquisition')
      print(paste0('from ', datetime$from, ' to ', datetime$to))
      message('Please check image availability on collection')
      print(collection)
      message('from provider')
      print(stac_source$base_url)
      stop_quietly()
    }
    # select tiles which are identified as wanted (== fully contains plot)
    if (!is.null(S2tiles) & unique(!is.na(S2tiles)))
        collection_plot <- eliminate_incompleteTiles(collection_plot, S2tiles = S2tiles)
    if (length(collection_plot$features)>0){
      # eliminates doublons in terms of date of acquisition
      s2id <- unlist(lapply(collection_plot$features,'[[','id'))
      tileID <- get_tile(s2id)
      if  (!is.null(S2tiles) & unique(!is.na(S2tiles)))
        collection_plot <- eliminate_doublons_dateAcq(collection_plot)
    }
  } else if (file.exists(FileName) & overwrite==F){
    # if file already exists, download it
    collection_plot <- readRDS(file = FileName)
  }
  # add acquisition date to collection
  dateacq <- unlist(lapply(lapply(collection_plot$features,'[[',"properties"), '[[', "datetime"))
  ID <- unlist(lapply(collection_plot$features,'[[',"id"))
  dateacq <- as.Date(unlist(lapply(stringr::str_split(string = dateacq, pattern = 'T'),'[[',1)))
  collection_plot$acquisitionDate <- dateacq
  # eliminate doublons in acquisition date
  uniqueDate <- unique(collection_plot$acquisitionDate)
  selunique <- match(x = uniqueDate, table = collection_plot$acquisitionDate)
  collection_plot$features <- collection_plot$features[selunique]
  collection_plot$acquisitionDate <- collection_plot$acquisitionDate[selunique]
  saveRDS(object = collection_plot, file = FileName)
  rm(collection_plot)
  return(invisible())
}



