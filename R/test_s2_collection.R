#' test S2 collection for a 10 km x 10 km tile of a given aoi
#'
#' @param aoi_path character. path for vector file defining aoi
#' @param datetime character or date or list defining date of acquisition or time range
#' @param output_dir character. path for output directory
#' @param site_name character. name of the study site
#' @param stac_info list. stac provider, name of collection url for stac
#' @param mask_path character.
#'
#' @return plots list of plots
#' @importFrom rstac stac ext_filter post_request
#' @importFrom sf st_transform
#' @importFrom stringr str_split
#' @importFrom magrittr %>%
#' @export

test_s2_collection <- function(aoi_path, datetime, output_dir, site_name = NULL,
                               stac_info, mask_path = NULL){

  message('creating a output_dir/test subdirectory')
  output_dir <- file.path(output_dir, 'test')
  options <- set_options_preprocS2(fun = 'test_s2_collection', options = NULL)

  # create proper datetime if only one date provided
  if (inherits(x = datetime, what = c('character', 'Date')))
    datetime <- list('from' = as.Date(datetime), 'to' = as.Date(datetime)+1)

  # define grid of tiles
  message('tiling area of interest')
  path_grid <- get_grid_aoi(aoi_path = aoi_path, cellsize = cellsize,
                            output_dir = output_dir, crs_target = crs_target)
  crs_target <- path_grid$crs_target
  plots <- path_grid$plots[1]

  # create raster directory
  s2_raster_dir <- file.path(output_dir, 'raster_samples')
  dir.create(path = s2_raster_dir, showWarnings = FALSE, recursive = TRUE)
  # create output collection directory
  collection_dir <- file.path(output_dir,'collections')
  dir.create(path = collection_dir, showWarnings = FALSE, recursive = TRUE)

  # define s2 tiles corresponding to aoi
  message('get S2 tiles corresponding to aoi')
  path_S2_tiling_grid <- check_s2_tiling_grid(path_S2_tiling_grid = options$path_S2_tiling_grid)
  S2_grid <- get_s2_tiles(plots = plots, dsn_bbox = aoi_path,
                          output_dir = output_dir, site_name = site_name,
                          path_S2_tiling_grid = path_S2_tiling_grid,
                          overwrite = options$overwrite)

  # get token for authentication on CDSE
  OAuth_client <- get_OAuth_client()
  id <- OAuth_client$id
  pwd <- OAuth_client$pwd

  # download S2 data
  message('download S2 collection')
  stac_info <- get_stac_info(stac_info)
  s2_tiles <- S2_grid$s2_tiles[[1]]

  # define file names for collection
  FileName_fullcoll <- file.path(collection_dir,paste0('plot_',names(plots),'.rds'))
  names(FileName_fullcoll) <- names(plots)
  FileName <- FileName_fullcoll[[1]]

  # get collection for plot, datetime and cloud cover
  stac_source <- rstac::stac(stac_info$stac_url)
  aoistac <- plots[[1]] |>
    sf::st_transform(4326)
  # define start and end days
  startday <- as.character(datetime$from)
  endday <- as.character(datetime$to)
  # avoid notes when building package
  `eo:cloud_cover` <- s_intersects <- geometry <- anyinteracts <- interval <- s_intersects <- NULL
  # download collection
  collection_plot <- stac_source %>%
    ext_filter(collection == {{stac_info$collection}} && `eo:cloud_cover` <= {{options$cloudcover}}
               && s_intersects(geometry, {{aoistac}})
               && anyinteracts(datetime, interval({{startday}}, {{endday}}))
    ) %>%
    post_request()

  # select tiles which are identified as wanted (== fully contains plot)
  if (!is.null(s2_tiles) & unique(!is.na(s2_tiles)))
    collection_plot <- eliminate_incompleteTiles(collection_plot, s2_tiles = s2_tiles)

  if (length(collection_plot$features)>0){
    item_collection = collection_plot
    # eliminates doublons in terms of date of acquisition
    s2id <- unlist(lapply(collection_plot$features,'[[','id'))
    tileID <- get_tile(s2id)
    if  (!is.null(s2_tiles) & unique(!is.na(s2_tiles)))
      s2id <- unlist(lapply(item_collection$features,'[[','id'))

    tileID <- get_tile(s2id)
    if (length(unique(tileID))>1){
      tileDist <- table(tileID)
      tileSelect <- names(tileDist[which(tileDist== max(tileDist))[1]])
      sel <- which(tileID==tileSelect)
      item_collection$features <- item_collection$features[sel]
    }
    # case when several acquisitions over same region
    # --> crossing orbits for S2A and S2B or S2C for the same acquisition date
    dateacq <- get_dateAcq(item_collection = item_collection, stac_info = stac_info)
    dist_dateacq <- table(dateacq)
    if (max(dist_dateacq)>1){
      doublons <- which(dist_dateacq>1)
      for (doublon in names(doublons)){
        selacqs <- which(dateacq==doublon)
        tileID_acqs <- tileID[selacqs]
        if (length(unique(tileID_acqs))==1 & length(tileID_acqs)>1 & !is.null(aoistac)){
          asset_names <- get_cloud_asset(stac_info = stac_info)
          item_collection_acq <- item_collection
          item_collection_acq$features <- item_collection_acq$features[selacqs]
          features_dl <- get_asset_terra(item = item_collection_acq,
                                         asset_names = asset_names,
                                         collection = stac_info$collection,
                                         aoi = aoistac)
          # identify acquisition with most valid pixels
          nbpix <- lapply(X = features_dl, FUN = function(x) length(na.omit(terra::values(x))))
          elimfeature <- which(unlist(nbpix)==min(unlist(nbpix)))[1]
          item_collection$features <- item_collection$features[-selacqs[elimfeature]]
          dateacq <- dateacq[-selacqs[elimfeature]]
          tileID <- tileID[-selacqs[elimfeature]]
        }
      }
    }
  }
  return(item_collection)
s}
