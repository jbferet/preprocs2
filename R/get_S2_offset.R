#' This function gets offset from S2 images produced from sen2cor or LaSRC
#'
#' @param MTD_MSI character. path for MTD_MSI file
#' @param MTD_LaSRC character. path for MTD_LaSRC file (specific to LaSRC)
#'
#' @return S2_offset list. info required to correct offset
#' @importFrom XML xmlToList
#' @export

get_s2_offset <- function(MTD_MSI = NULL, MTD_LaSRC = NULL){
  
  # apply offset when necessary
  listBands_bis <-     c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12")
  if (!is.null(MTD_MSI) & is.null(MTD_LaSRC)){
    # read XML file containing info about geometry of acquisition
    # s2xml <- XML::xml(MTD)
    s2xml <- XML::xmlToList(MTD_MSI)
    XML_Offset <- s2xml$General_Info$Product_Image_Characteristics$BOA_ADD_OFFSET_VALUES_LIST
    Bands <- lapply(s2xml$General_Info$Product_Image_Characteristics$Spectral_Information_List,'[[',4)
    if (!is.null(XML_Offset) && !is.null(Bands)){
      BandID  <- lapply(Bands,'[[',1)
      BandName  <- lapply(Bands,'[[',2)
      Offset <- data.frame('BandName' = unlist(BandName),
                           'BandID' = unlist(BandID),
                           'Offset' =unlist(lapply(XML_Offset,'[[',1)))
      selBands <- match(listBands_bis,Offset$BandName)
      Offset <- Offset[selBands,]
      BOA_QuantVal <- as.numeric(s2xml$General_Info$Product_Image_Characteristics$QUANTIFICATION_VALUES_LIST$BOA_QUANTIFICATION_VALUE[1])
    } else {
      Offset <- data.frame('BandName' = listBands_bis,
                           'BandID' = c(1,2,3,4,5,6,7,8,11,12),
                           'Offset' =0)
      BOA_QuantVal <- 10000
    }
  } else if (!is.null(MTD_LaSRC)){
    # read XML file containing info about geometry of acquisition
    # s2xml <- XML::xml(MTD)
    s2xml <- XML::xmlToList(MTD_LaSRC)
    attributes_LaSRC <- s2xml$bands[[14]]$.attrs
    attributes_LaSRC_df <- data.frame(attributes_LaSRC)
    if (!is.na(match('add_offset',rownames(attributes_LaSRC_df))) & !is.na(match('scale_factor',rownames(attributes_LaSRC_df)))){
      XML_Offset <- as.numeric(attributes_LaSRC[['add_offset']])
      BOA_QuantVal <- 1/as.numeric(attributes_LaSRC[['scale_factor']])
      Offset <- data.frame('BandName' = listBands_bis,
                           'BandID' = c(1,2,3,4,5,6,7,8,11,12),
                           'Offset' = XML_Offset)
    } else {
      Offset <- data.frame('BandName' = listBands_bis,
                           'BandID' = c(1,2,3,4,5,6,7,8,11,12),
                           'Offset' =0)
      BOA_QuantVal <- 10000
    }
  } else {
    Offset <- data.frame('BandName' = listBands_bis,
                         'BandID' = c(1,2,3,4,5,6,7,8,11,12),
                         'Offset' =0)
    BOA_QuantVal <- 10000
  }
  S2_offset <- list('BOA_QuantVal' = BOA_QuantVal, 'Offset' = Offset)
  return(S2_offset)
}
