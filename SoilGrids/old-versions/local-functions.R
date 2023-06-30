
## TODO: this can now be done via, even though the following is probably more efficient

# p <- SpatialPoints(cbind(x=-119.72330, y = 36.92204), proj4string = CRS('+proj=longlat +datum=WGS84'))
# res <- SDA_spatialQuery(p, what = 'mukey')
# mu.is <- format_SQL_in_statement(res$mukey)
# sql <- sprintf("mukey IN %s", mu.is)
# x <- fetchSDA(sql, duplicates = TRUE)

SPCviaSDA <- function(x, y) {
  # basic query, with parameters x,y
  qq <- sprintf(
    "
    SELECT 
    mu.mukey,
    co.cokey AS id, comppct_r, compkind, compname,
    hzname, hzdept_r, hzdepb_r, 
    sandtotal_r AS sand, silttotal_r AS silt, claytotal_r AS clay,
    ph1to1h2o_r AS phh2o, cec7_r AS cec
    FROM mapunit AS mu
    INNER JOIN component AS co ON mu.mukey = co.mukey
    INNER JOIN chorizon as ch ON co.cokey = ch.cokey
    WHERE majcompflag = 'Yes'
    AND mu.mukey IN 
    (
      SELECT * from SDA_Get_Mukey_from_intersection_with_WktWgs84('point(%s %s)')
    )
    order by comppct_r DESC, hzdept_r ASC
    ", 
    x, y
  )
  
  # submit query
  res <- SDA_query(qq)
  
  # init SPC
  depths(res) <- id ~ hzdept_r + hzdepb_r
  hzdesgnname(res) <- 'hzname'
  site(res) <- ~ mukey + comppct_r + compkind + compname
  
  res$label <- sprintf("%s (%s)", res$compname, res$comppct_r)
  site(res)$origin <- 'SSURGO'
  
  site(res)$unique.id <- digest(c(x, y), algo = 'murmur32')
  
  return(res)
}


getSG <- function(x, y) {
  u <- sprintf("http://rest.isric.org/soilgrids/v2.0/properties/query?lon=%s&lat=%s&property=bdod&property=cec&property=cfvo&property=clay&property=phh2o&property=sand&property=silt&property=soc&depth=0-5cm&depth=0-30cm&depth=5-15cm&depth=15-30cm&depth=30-60cm&depth=60-100cm&depth=100-200cm&value=Q0.05&value=Q0.5&value=Q0.95", x, y)
  
  z <- fromJSON(u)
  
  p <- z$properties$layers$name
  lyr <- z$properties$layers$depths
  
  names(lyr) <- p
  
  lyr.prop <- lapply(lyr, function(i) {
    i[, c('values')]
  })
  
  lyr.depths <- lapply(lyr, function(i) {
    i[, c('range')]
  })
  
  lyr.prop <- do.call('rbind', lyr.prop)
  lyr.depths <- do.call('rbind', lyr.depths)
  
  
  var.names <- strsplit(row.names(lyr.prop), '.', fixed = TRUE)
  lyr.prop$var <- sapply(var.names, '[', 1)
  
  names(lyr.depths) <- c('top', 'bottom', 'units')
  
  d <- cbind(lyr.depths, lyr.prop)
  
  d$unique.id <- digest(c(x, y), algo = 'murmur32')
  d$id <- sprintf('SG:%s', d$unique.id)
  
  lwr <- dcast(d, unique.id + id + top + bottom ~ var, value.var = 'Q0.05')
  med <- dcast(d, unique.id + id + top + bottom ~ var, value.var = 'Q0.5')
  upr <- dcast(d, unique.id + id + top + bottom ~ var, value.var = 'Q0.95')
  
  lwr <- transform(lwr,
                   id = sprintf("%s-lwr", id),
                   bdod = bdod / 100,
                   cec = cec / 10,
                   cfvo = cfvo / 10,
                   clay = clay / 10,
                   phh2o = phh2o / 10,
                   sand = sand / 10,
                   silt = silt / 10,
                   soc = soc / 10
  )
  
  med <- transform(med,
                   id = sprintf("%s-med", id),
                   bdod = bdod / 100,
                   cec = cec / 10,
                   cfvo = cfvo / 10,
                   clay = clay / 10,
                   phh2o = phh2o / 10,
                   sand = sand / 10,
                   silt = silt / 10,
                   soc = soc / 10
  )
  
  upr <- transform(upr,
                   id = sprintf("%s-upr", id),
                   bdod = bdod / 100,
                   cec = cec / 10,
                   cfvo = cfvo / 10,
                   clay = clay / 10,
                   phh2o = phh2o / 10,
                   sand = sand / 10,
                   silt = silt / 10,
                   soc = soc / 10
  )
  
  depths(lwr) <- id ~ top + bottom 
  depths(med) <- id ~ top + bottom 
  depths(upr) <- id ~ top + bottom 
  
  site(lwr) <- ~ unique.id
  site(med) <- ~ unique.id
  site(upr) <- ~ unique.id
  
  site(lwr)$label <- sprintf('SG-lwr')
  site(med)$label <- sprintf('SG-med')
  site(upr)$label <- sprintf('SG-upr')
  
  res <- combine(lwr, med, upr)
  
  site(res)$origin <- 'SG'
  
  return(res)
}


prepData <- function(x, y, gl) {
  s <- SPCviaSDA(x, y)
  sg <- getSG(x, y)
  
  g <- combine(s, sg)
  
  g$texture <- ssc_to_texcl(g$sand, g$clay)
  
  site(g)$group.label <- gl
  
  return(g)
}

