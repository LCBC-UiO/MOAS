#'

ggaparc = function(data){
  ### Ids corresponding to parcelations in this image
  GeoAparc = cbind(
    id = c(0, 1, 4, 7, 8,9,15,17,20,21,22,23,28,32,34,39,41,49),
    area = c("parstriangularis","smn","motor","ipl","stg","mtg","loc","rmf",
             "lvmpfc","bankssts","insula","spc","itg","visual","cmf","parsorbitalis",
             "supfrontal","transverse")
  ) %>% as.data.frame(stringsAsFactors=F)


  if(!missing(data)){
    if(!"area" %in% names(data)){
      str = paste(GeoAparc$area, collapse=", ")
      stop(paste0("Supplied data must have an 'area' column, and must have one of following areas: ",str))
    }

    unknown=!(data$area %in% GeoAparc$area)
    if(any(unknown)){
      warning(paste0("Data contains unknown area ", data$area[unknown]))
    }
  }

  ##### main routine #####
  #Open geo object (shape file)
  fileLoc = system.file("data","geobrain", package = "MOAS")
  geobrainOGR <- rgdal::readOGR(dsn = fileLoc, layer = "geobrain", verbose = F)

  # transform to data.frame
  geobrain <- ggplot2::fortify(geobrainOGR, region="GEO_ID")

  # Do some cleanup of the data
  geobrain2 <- geobrain %>%
    dplyr::left_join(GeoAparc, by="id") %>%
    dplyr::filter(!is.na(area)) %>%
    rename(Region=id)

  ggplot2::ggplot(data = geobrain2) +
    ggplot2::geom_polygon(
      ggplot2::aes(x = long,
                   y = lat,
                   group = area),
      color = "white", size = .3) +
    coord_fixed() +
    ggplot2::theme_void()
}





