library(sf)
library(tidyverse)

target_plot <- function(a, b, c, d, e, f, pointsize = 1, bordersize = .005, title = NA, legend = T, side_labels = c("DS", "SS")) {
  # browser()
  N <- a + b + c + d + e + f
  center_cir <- (a + d)/N
  incon_cir <- (a + d + b + e)/N
  
  center_rad2 <- center_cir/pi
  incon_rad2 <- incon_cir/pi
  
  # get positive coordinates, then flip to get other half of the circle
  xcenter <- seq(-sqrt(center_rad2), sqrt(center_rad2), .001)
  ycenter <- sqrt(round(center_rad2 - xcenter^2, 8))
  xincon <- seq(-sqrt(incon_rad2), sqrt(incon_rad2), .001)
  yincon <- sqrt(round(incon_rad2 - xincon^2, 8))
  
  inner_circle <- cbind(x = c(xcenter, rev(xcenter)), 
                        y = c(ycenter, -rev(ycenter)))
  mid_circle <- cbind(x = c(xincon, rev(xincon)), 
                      y = c(yincon, -rev(yincon)))
  outer_rect <- cbind(x = c(-.5, .5, .5, -.5, -.5), 
                      y = c(.5, .5, -.5, -.5, .5))
  
  ident_sf <- st_polygon(list(inner_circle))
  inconcl_sf <- list(inner_circle, mid_circle) %>% st_multilinestring() %>% st_cast("MULTIPOLYGON") %>% st_make_valid()
  elim_sf <- list(mid_circle, outer_rect) %>% st_multilinestring() %>% st_cast("MULTIPOLYGON") %>% st_make_valid()
  conclusions <- st_sf(
    concl = c("Identification", "Inconclusive", "Elimination"), 
    geometry_concl = list(ident_sf, inconcl_sf, elim_sf))
  
  prop_ss <- (a + b + c)/N
  xint <- .5-(.5 + prop_ss)/2
  ground_truth <- st_sf(gt = c("DS", "SS"), 
                        geometry_gt = list(st_polygon(list(cbind(c(-.5, xint, xint, -.5, -.5),
                                                              c(-.5, -.5, .5, .5, -.5)))),
                                        st_polygon(list(cbind(c(xint, .5, .5, xint, xint),
                                                              c(-.5, -.5, .5, .5, -.5))))))
  ideal_props <- tibble(label = c("a", "b", "c", "d", "e", "f"),
                        value = c(a, b, c, d, e, f),
                        prop = value/N,
                        gt = rep(c("SS", "DS"), each = 3),
                        concl = rep(c("Identification", "Inconclusive", "Elimination"), times = 2))
  sample_geo <- function(geo, points, bordersize) {
    if (is_empty(geo) | points == 0) {
      return(NULL)
    }
    
    st_sample(st_buffer(geo, -bordersize), points, type = "hexagonal")
  }
  
  frame <- crossing(st_drop_geometry(conclusions), st_drop_geometry(ground_truth)) %>% # workaround to deal with sf strangeness
    left_join(conclusions) %>%
    left_join(ground_truth) %>%
    mutate(geometry = map2(geometry_concl, geometry_gt, st_intersection)) %>%
    select(-geometry_gt, -geometry_concl) %>%
    mutate(type = paste(gt, concl)) %>%
    left_join(ideal_props) %>%
    # st_sf() %>%
    mutate(value2 = ifelse(value == 0, 2, value)) %>%
    mutate(points = map2(geometry, value2, sample_geo, bordersize = bordersize)) %>%
    st_sf() %>%
    mutate(concl = factor(concl, levels = c("Identification", "Inconclusive", "Elimination"))) %>%
    mutate(border = factor(concl, labels = c("white", "black", "white")) %>% as.character()) %>%
    mutate(gt = factor(gt, levels = c("DS", "SS"), labels = c("Different Source", "Same Source")))
  
  points <- select(frame, gt, concl, type, border, value, points) %>% 
    filter(value > 0) %>% 
    unnest(points) %>%
    st_set_geometry("points")

  tmp <- ggplot(frame, aes(geometry = geometry, fill = concl))
  if (is.na(title)) {
    tt <- list(theme(plot.title = element_blank()))
  } else {
    tt <- list(theme(plot.title = element_text(hjust = .5)))
  }
    
  
  tmp <- tmp  + 
    geom_sf(alpha = .6, color = "black") + 
    # geom_sf(data = filter(points, concl != "Inconclusive"), aes(geometry = points), color = "white", alpha = .5, size = 1.5*pointsize, legend = F) +
    geom_sf(data = points, aes(geometry = points, color = gt), size = pointsize, shape = 19) +
    annotate(geom = "text", x=-.5, y=.55, label = side_labels[1], hjust = 0, size = 5, color = "darkorange4") + 
    annotate(geom = "text", x=.5, y=.55, label = side_labels[2], hjust = 1, size = 5, color = "steelblue4") + 
    geom_sf(aes(geometry = geometry), fill = "transparent", size = .75, color = "black") +     
    geom_vline(xintercept = xint, size = .75) + 
    ggtitle(title) +
    theme(axis.text = element_blank(), axis.ticks = element_blank(), 
          axis.title = element_blank(), axis.line = element_blank(),
          panel.background = element_rect(fill = "white", color = "white"),
          legend.key = element_rect(fill = "white", color = "white")) + 
    tt
          
  if (!legend) {
    tmp <- tmp + 
      scale_fill_manual("Conclusion", values = c("Identification" = "steelblue", "Inconclusive" = "white", "Elimination" = "darkorange"), guide = "none") + 
      scale_color_manual("Ground Truth", values = c("Same Source" = "steelblue4", "Different Source" = "darkorange4"), guide = "none")
  } else {
    tmp <- tmp + 
      scale_fill_manual("Conclusion", values = c("Identification" = "steelblue", "Inconclusive" = "white", "Elimination" = "darkorange")) + 
      scale_color_manual("Ground Truth", values = c("Same Source" = "steelblue4", "Different Source" = "darkorange4")) +
      guides(color = guide_legend(override.aes = list(size = 3)), fill = guide_legend(override.aes = list(shape = NA))) 
  }
  
  tmp
}

#' target_plot(100, 40, 15, 15, 40, 100)
