library(sf)
library(tidyverse)

target_plot <- function(a, b, c, d, e, f) {
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
  xint <- 0#-.5+prop_ss
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
  
  geo_rm_buffer <- function(reg, dist) {
    bf <- st_buffer(reg, dist = -dist)
    st_difference(bf, reg)
  }
  
  frame <- crossing(conclusions, ground_truth) %>%
    mutate(geometry = map2(geometry_concl, geometry_gt, st_intersection)) %>%
    select(-geometry_gt, -geometry_concl) %>%
    mutate(type = paste(gt, concl)) %>%
    left_join(ideal_props) %>%
    st_sf() %>%
    mutate(points = map2(geometry, value, ~st_sample(st_buffer(.x, -.005), .y, type = "hexagonal"))) %>%
    st_sf() %>%
    mutate(concl = factor(concl, levels = c("Identification", "Inconclusive", "Elimination"))) %>%
    mutate(border = factor(concl, labels = c("white", "black", "white")) %>% as.character()) %>%
    mutate(gt = factor(gt, levels = c("DS", "SS"), labels = c("Different Source", "Same Source")))
  
  points <- select(frame, gt, concl, type, border, points) %>% unnest(points) %>%
    st_set_geometry("points")

  ggplot(frame, aes(geometry = geometry, fill = concl)) + 
    geom_sf(alpha = .6, color = "black") + 
    geom_sf(data = filter(points, concl != "Inconclusive"), aes(geometry = points), color = "white", alpha = .5, size = 1.5, legend = F) +
    geom_sf(data = points, aes(geometry = points, color = gt), size = 1) +
    scale_fill_manual("Conclusion", values = c("Identification" = "steelblue", "Inconclusive" = "white", "Elimination" = "darkorange")) + 
    scale_color_manual("Ground Truth", values = c("Same Source" = "steelblue4", "Different Source" = "darkorange4")) +
    geom_vline(xintercept = 0) + 
    annotate(geom = "text", x=-.05, y=.55, label = "Different Source", hjust = 1, size = 5, color = "darkorange4") + 
    annotate(geom = "text", x=.05, y=.55, label = "Same Source", hjust = 0, size = 5, color = "steelblue4") + 
    guides(color = guide_legend(override.aes = list(size = 3)), fill = guide_legend(override.aes = list(shape = NA))) + 
    theme(axis.text = element_blank(), axis.ticks = element_blank(), 
          axis.title = element_blank(), axis.line = element_blank(),
          panel.background = element_rect(fill = "white", color = "white"),
          legend.key = element_rect(fill = "white", color = "white")
          )
}

target_plot(100, 40, 15, 15, 40, 100)
