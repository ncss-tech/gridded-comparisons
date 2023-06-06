

compareAOI <- function(aoi, figTitle = '') {
  
  ## get data
  tx.clay <- getData(aoi, .variable = 'claytotal_r', .SOLUS_variable = 'Clay', .rescale_value = 1)
  tx.sand <- getData(aoi, .variable = 'sandtotal_r', .SOLUS_variable = 'Sand', .rescale_value = 1)
  
  g.texture <- tx.sand$gNATSGO
  values(g.texture) <- ssc_to_texcl(sand = values(tx.sand$gNATSGO), clay = values(tx.clay$gNATSGO), droplevels = TRUE)
  
  s.texture <- tx.sand$gNATSGO
  values(s.texture) <- ssc_to_texcl(sand = values(tx.sand$SOLUS), clay = values(tx.clay$SOLUS), droplevels = TRUE)
  
  ## compare classes
  z <- compare(g.texture, s.texture, oper = '==')
  
  ## match colors
  g.cols <- txt.lut$hex[match(
    levels(g.texture)[[1]]$label,
    txt.lut$class
  )]
  
  s.cols <- txt.lut$hex[match(
    levels(s.texture)[[1]]$label,
    txt.lut$class
  )]
  
  
  
  ## add ISSR-800
  
  
  ## approximate AOI width
  .km <- abs(diff(ext(g.texture)[1:2])) / 1000
  .mi <- 0.621371 * .km
  .ac <- bbArea(aoi)
  .subtxt <- sprintf("AOI: %0.f acres\nAOI width: %0.f miles (%0.f km)", .ac, .mi, .km)
  
  ## make figure
  par(mar = c(0, 0, 0, 0), mfrow = c(1, 2))
  plot(g.texture, axes = FALSE, col = g.cols, main = 'gNATSGO\nTexture Class <2mm\n15cm')
  mtext(.subtxt, side = 1, adj = 0, line = -1)
  
  plot(s.texture, axes = FALSE, col = s.cols, main = 'SOLUSv2\nTexture Class <2mm\n15cm')
  mtext(figTitle, side = 1, adj = 0, line = -1)
  
  # optional pixel-wise `==` operation
  # plot(z, axes = FALSE, main = 'Match')
  
  ## return list of data
  invisible(
    list(
      sand = tx.sand,
      clay = tx.clay,
      gNATSGO.texture = g.texture,
      SOLUS.texture = s.texture
    )
  ) 
}


bbArea <- function(bb, .text = FALSE) {
  
  if(.text) {
    bb <- sprintf("POLYGON((%s))", bb)
    bb <- vect(bb, crs = "OGC:CRS84")
  }
  
  a <- expanse(bb, unit = 'm') * 0.000247105
  return(round(a))
}




#' Get Data
#'
#' @param aoi 
#' @param .variable 
#' @param .SOLUS_variable 
#' @param .rescale_value 
#' @param .depth 
#'
#' @return
#' @export
#'
#' @examples
getData <- function(aoi, .variable, .SOLUS_variable, .rescale_value, .depth = 15) {
  
  # transform to WGS84
  aoi <- project(aoi, "OGC:CRS84")
  
  # coordinate SOLUS asset names / bands (depth)
  .band_name <- switch(.SOLUS_variable, 
                       'Clay' = {
                         sprintf("r_%s_cm_claytotal_pred", .depth)
                       }, 
                       'Sand' = {
                         sprintf("r_%s_cm_sandtotal_pred", .depth)
                       }
  )
  
  # download GEE asset to temp file
  # version 2
  x <- sprintf("projects/ncss-30m-covariates/assets/SOLUS100mV2/%s", .SOLUS_variable) |> 
    gd_image_from_id() |> 
    gd_download(
      region = gd_bbox(ext(aoi)),
      dtype = "uint16",
      bands = list(.band_name)
    ) |> 
    rast()
  
  # get gNATSGO WCS raster for extent
  m <- mukey.wcs(aoi, db = 'gNATSGO', quiet = TRUE)
  
  # extract RAT
  rat <- cats(m)[[1]]
  
  # thematic soil data via SDA
  p <-  get_SDA_property(property = .variable,
                         method = "Weighted Average", 
                         mukeys = as.integer(rat$mukey),
                         miscellaneous_areas = FALSE,
                         include_minors = TRUE,
                         top_depth = 14,
                         bottom_depth = 16)
  
  # join aggregate soil data + RAT
  rat <- merge(rat, p, by = 'mukey', sort = FALSE, all.x = TRUE)
  levels(m) <- rat
  
  # convert mukey + RAT -> numeric raster
  activeCat(m) <- .variable
  
  # must specify the variable index
  # faster than catalyze
  m2 <- as.numeric(m, index = .variable)
  
  # check correct variable was transferred
  stopifnot(names(m2) == .variable)
  
  # crop gNATSGO to SOLUS extent
  m2 <- crop(m2, x)
  
  # re-sample to SOLUS grid
  # m4 <- resample(m3, x)
  
  # resample SOLUS to gNATSGO grid
  solus <- resample(x, m2)
  
  # rescale SOLUS to range used by gNATSGO as-needed
  solus <- solus * .rescale_value
  
  return(c(gNATSGO = m2, SOLUS = solus))
}

