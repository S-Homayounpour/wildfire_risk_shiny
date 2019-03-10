
library(WildfireRisk)
library(RSQLite)

do_treat_blocks_line <- function(block.risk, line.risk) {
  

  # load point data from database
  con <- dbConnect(SQLite(), dbname = attr(line.risk, "dbname"))
  pointdata <- dbReadTable(con, "pointdata")
  dbDisconnect(con)
  
  # get indices of intersecting scan lines for each block
  intersections <- lines_in_blocks(block.risk, line.risk, by = "block")
  # the inition data
  initials <-line.risk
  
  # reduce line.risk to a plain data frame with IDs and the variables
  # needed for risk calculation
  line.risk <- line.risk %>%
    as.data.frame() %>%
    select(locationid, lineid, forest_p, distance, is_west) %>%
    mutate(locationid = as.character(locationid))
  
  
  blocks.with.lines <- which(sapply(intersections, length) > 0)
  
  if (length(blocks.with.lines) == 0) {
    warning("No intersecting scan lines found for any block.\n")
    block.risk$ptreat_mean <- block.risk$pobs_mean
    return(line.risk)
  }
  
  # For each block intersected by scan lines, identify sample points
  # within the block, set time since fire of those points to zero,
  # re-calculate line risk values, and summarize for the block.
  
  block.risk$ptreat_mean <- block.risk$pobs_mean
  
  k <- 0
  pb <- txtProgressBar(0, length(blocks.with.lines), style = 3)
  treated_lines <- data.frame()
  
  
  
  for (iblock in blocks.with.lines) {
    ii <- intersections[[iblock]]
    
    pdat <- line.risk[ii, ] %>%
      select(locationid, lineid) %>%
      left_join(pointdata, by = c("locationid", "lineid"))
    
    # Set time since fire of points within the block
    # to zero
    pts <- lapply(1:nrow(pdat), function(i) st_point(c(pdat$x[i], pdat$y[i])) )
    pts <- st_sfc(pts, crs = st_crs(block.risk))
    
    ii <- st_intersects(block.risk[iblock, ], pts)[[1]]
    
    # It is possible to have a scan line that intersects the block
    # has no sample points in the block (so nothing to do).
    #
    if (length(ii) > 0) {
      pdat$tsf[ii] <- 0

      # Calculate updated line risk values
      ldat <- pdat %>%
        group_by(locationid, lineid) %>%
        summarize(tsf_mean_treated = mean(tsf, na.rm = TRUE)) %>%
        ungroup() %>%
        
        left_join(line.risk, by = c("locationid", "lineid")) %>%
        
        mutate(pobs = calculate_line_risk(tsf_mean = tsf_mean_treated,
                                            forest_p = forest_p,
                                            distance = distance,
                                            is_west = is_west)) %>% 
        mutate(pmax = calculate_line_risk(tsf_mean = 50,
                                          forest_p = forest_p,
                                          distance = distance,
                                          is_west = is_west) )
    print(ldat)  
    treated_lines <- rbind(treated_lines,ldat)
    }
    
    k <- k + 1
    setTxtProgressBar(pb, k)
  }
  close(pb)
  ## dropping duplicated lines with greater pobs value as
  ## seeting tsf to zero will reduce the pobs or wont change at all 
  ## if points probability does not contribute to the 
  ## overall probability of line significantly
  names(treated_lines)[3] <- "tsf_mean"
  print(treated_lines)
  trimmed <-  initials %>%  select(locationid,lineid)
  result <- rbind(as.data.frame(initials)[,-9],treated_lines)%>%
    inner_join(trimmed,by = c("locationid","lineid")) %>% 
    group_by(locationid,lineid) %>% 
    filter(pobs == min(pobs)) %>% 
    ungroup() %>% 
    distinct(locationid,lineid,.keep_all = TRUE)
  result
  }

### noDB is modified version of summarize_location_risk
## that ignore risk class attrubutes of line
summarize_location_risk_noDB <- function(line.risk, quantiles = c(0.25, 0.75)) {
  
  
  # Helper function to retrieve central point from a
  # set of scan lines
  firstpoint <- function(lines) {
    m <- st_coordinates(lines)
    data.frame(x = m[1,1], y = m[1,2])
  }
  
  has.quantiles <- !is.null(quantiles) & length(quantiles) > 0
  
  if (has.quantiles) {
    qnames <- names(quantile(1, quantiles)) %>% stringr::str_replace("\\%", "")
  }
  
  # Get point locations
  loc <- line.risk %>%
    group_by(locationid) %>%
    do(firstpoint(.$geometry))
  
  
  # Helper function to calculate mean and quantiles and
  # return them as a data frame
  fn <- function(x, varname) {
    d <- data.frame(mu = mean(x, na.rm = TRUE))
    colnames(d) <- paste(varname, "mean", sep = "_")
    
    if (has.quantiles) {
      q <- quantile(x, probs = quantiles, na.rm = TRUE)
      q <- t(q)
      colnames(q) <- paste(varname, qnames, sep = "_")
      
      d <- cbind(d, q)
    }
    
    d
  }
  
  
  # Summary statistics for each location
  pstats <- line.risk %>%
    # drop scan lines
    as.data.frame() %>%
    
    # calculate mean probabilities
    group_by(locationid) %>%
    
    do({
      dobs <- fn(.$pobs, "pobs")
      dmax <- fn(.$pmax, "pmax")
      cbind(dobs, dmax)
    }) %>%
    
    ungroup() %>%
    
    # join location data
    left_join(loc, by = "locationid") %>%
    
    # convert to a spatial (sf) object with point geometry
    st_as_sf(coords = c("x", "y"))
  
  
  # Set coordinate reference system
  st_crs(pstats) <- st_crs(line.risk)
  
  pstats
}














