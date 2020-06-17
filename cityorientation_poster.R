##### Map-Histogram Poster of City Streets for a Selected City #####

# Load libraries ----------------------------------------------------------
require(osmdata)
require(sf)
require(geosphere)
require(tidyverse)
require(cowplot)
require(extrafont)
require(Cairo)

# Load/Create data ---------------------------------------------------------------
city_oi <- ""
cityOI_prov <- ""
cityOI_selected <- str_replace(as.character(city_oi), " Canada", "")

# Create a data frame that contains the limits for the city
get_limits <- function(city) {
        
        df <- getbb(city) %>%
                as_tibble(rownames = NA) %>%
                rownames_to_column(var = "dimension") %>%
                mutate(City = cityOI_selected) %>%
                pivot_longer(cols = c(min, max), 
                             names_to = "Range", 
                             values_to = "Value") %>%
                select(City, everything())
        
        return(df)
}

cityOI_limits <- get_limits(city_oi)

# Create a data frame that contains the street-level data for the city
get_streets <- function(city) {
        
        city_name <- cityOI_selected
        
        df <- getbb(city) %>%
                opq() %>%
                add_osm_feature(key = "highway",
                                value = c("motorway", "trunk", "primary", "secondary", "tertiary",
                                          "residential", "living_street", "unclassified", "service")) %>%
                osmdata_sf() %>%
                .$osm_lines %>%
                mutate(City = city_name,
                       Road_type = map_chr(highway, ~ ifelse(.x %in% c("motorway", "trunk", "primary", "secondary", "tertiary"),
                                                             "major",
                                                             "minor"))) %>%
                select(City, osm_id, name, Road_type, geometry) %>%
                filter(!is.na(name))
        
        return(df)
}

cityOI_streets <- get_streets(city_oi)

# Create a data frame that contains the labels data for the city
cityOI_labels_df <- data.frame("bb_city" = city_oi) %>%
                        mutate(City = cityOI_selected,
                               Province = cityOI_prov,
                               Country = "Canada",
                               latitude = map_chr(City, ~ sprintf("%.2f", round(mean(cityOI_limits %>% filter(City == .x, dimension == "y") %>% pull(Value)), 2))),
                               longitude = map_chr(City, ~ sprintf("%.2f", round(mean(cityOI_limits %>% filter(City == .x, dimension == "x") %>% pull(Value)), 2))),
                               lat_txt = map_chr(latitude, ~ paste0(str_extract(.x, pattern = "[:digit:]{2,}(?=\\.)"), "\u00B0",
                                                                    str_extract(.x, pattern = "(?<=\\.)[:digit:]{2,}"), "' N")),
                               lon_txt = map_chr(longitude, ~ paste0(str_extract(.x, pattern = "[:digit:]{2,}(?=\\.)"), "\u00B0",
                                                                     str_extract(.x, pattern = "(?<=\\.)[:digit:]{2,}"), "' W")),
                               coord_txt = paste0(lat_txt, " - ", lon_txt))

# Organize bearing data -----------------------------------------------------------
bearing_and_length <- function(geo_mat) {
        
        df <- data.frame(geo_mat)
        
        segments <- nrow(df)
        
        p1 <- c(df$lon[1], df$lat[1])
        p2 <- c(df$lon[segments], df$lat[segments])
        
        bear <- bearing(p1, p2)
        len <- distHaversine(p1, p2)
        
        return(list(bearing_raw = bear, Length = len))
}

bearing_adj <- function(bearing, side) {
        
        b_pos <- ifelse(bearing < 0, bearing + 360, bearing)
        
        # Top side bearing [0-180]
        b_top <- ifelse(b_pos > 180, b_pos - 180, b_pos)
        
        # Bottom side bearing [180 - 360]
        b_bot <- ifelse(b_pos < 180, b_pos + 180, b_pos)
        
        if(side == "top") {
                return(b_top) 
        } else if(side == "bottom") {
                return(b_bot)
        } else { return(NA) }
}

cityOI_bearings <- cityOI_streets %>%
                        mutate(val_list = map(geometry, ~ bearing_and_length(as.matrix(.x))),
                               val_list = map(val_list, as_tibble)) %>%
                        unnest(cols = val_list) %>%
                        mutate(bearing_top = map_dbl(bearing_raw, ~ round(bearing_adj(.x, side = "top"), 0)),
                               bearing_bot = map_dbl(bearing_raw, ~ round(bearing_adj(.x, side = "bottom"), 0)))

cityOI_bearing_summary <- cityOI_bearings %>%
                                st_set_geometry(NULL) %>%
                                select(-bearing_raw) %>%
                                mutate(bearing_bot = map_dbl(bearing_bot, ~ ifelse(.x >= 355, 0, .x))) %>%
                                pivot_longer(cols = c(bearing_top, bearing_bot),  
                                             names_to = "Side",
                                             values_to = "Bearing")

# Determine y-max for Proportion Histogram by City
prop_max <- function(city) {
        
        tmp_ordinal <- ggplot(cityOI_bearing_summary %>%
                                      filter(City == city),
                              aes(x = Bearing, weight = Length)) +
                geom_histogram(aes(y = stat(density * width)), binwidth = 10, center = 0, closed = "left")
        
        ordinal_hline_max <- layer_scales(tmp_ordinal)$y$range$range[2]
        
        return(ordinal_hline_max)
}

cityOI_HistProp_df <- data.frame("bb_city" = city_oi) %>%
                        mutate(City = cityOI_selected,
                               maxProportion_raw = map_dbl(City, ~ prop_max(.x)),
                               maxProportion_rnd = plyr::round_any(maxProportion_raw, .02, ceiling),
                               maxProportion_seq = map(maxProportion_rnd, ~ seq(0, .x, by = 0.02)),
                               maxProportion_seqSMALL = map(maxProportion_seq, ~ .x[2:(length(.x)-1)]))
                        
# Figures -----------------------------------------------------------------
color1 <- "steelblue4"
color2 <- "steelblue3"

# Polar histogram of bearings ---------------------------------------------
ordinal_plot <- ggplot(cityOI_bearing_summary %>%
                         filter(City == cityOI_selected),
                    aes(x = Bearing, weight = Length)) +
                        geom_hline(data = cityOI_HistProp_df %>%
                                                filter(City == cityOI_selected) %>%
                                                select(City, maxProportion_seq) %>%
                                                unnest(maxProportion_seq),
                                   aes(yintercept = maxProportion_seq),
                                   color = "grey88", size = 0.25, alpha = 0.85) +
                        geom_hline(data = cityOI_HistProp_df %>%
                                                filter(City == cityOI_selected) %>%
                                                select(City, maxProportion_rnd),
                                   aes(yintercept = maxProportion_rnd), 
                                   color = color1, size = 0.75) +
                        geom_vline(xintercept = seq(0, 360-1, by = 45), colour = "grey88", size = 0.25) +
                        geom_histogram(aes(y = stat(density * width)), binwidth = 10, center = 0, closed = "left", 
                                       size = 0.75, color = color1, fill = color2) +
                        scale_x_continuous(breaks = seq(0, 270, 90), labels = c("N", "E", "S", "W")) +
                        coord_polar(theta = "x", start = -pi/36) +
                        theme_teeter(baseSize = 14) + # this is custom_ggplot theme. theme_bw() would probably do fine.
                        theme(plot.margin = unit(rep(0, 4), "cm"),
                              panel.border = element_blank(),
                              panel.background = element_rect(fill = NA, colour = NA),
                              plot.background = element_rect(fill = NA, colour = NA),
                              panel.grid = element_blank(),
                              axis.title.x = element_blank(),
                              axis.title.y = element_blank(),
                              axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.y = element_blank())
ordinal_plot

# City streets plot ---------------------------------------------------------------
xlimits <- c(cityOI_limits %>% filter(City == cityOI_selected, dimension == "x", Range == "min") %>% pull(Value),
             cityOI_limits %>% filter(City == cityOI_selected, dimension == "x", Range == "max") %>% pull(Value))

ylimits <- c(cityOI_limits %>% filter(City == cityOI_selected, dimension == "y", Range == "min") %>% pull(Value),
             cityOI_limits %>% filter(City == cityOI_selected, dimension == "y", Range == "max") %>% pull(Value))

cols <- c("major" = "black", "minor" = "grey55")

city_plot <- ggplot(data = cityOI_streets %>% 
                      filter(City == cityOI_selected), 
                    aes(color = Road_type)) +
                geom_sf(aes(geometry = geometry),
                        size = .4,
                        alpha = .8) +
                scale_color_manual(values = cols, guide = F) +
                coord_sf(xlim = xlimits,
                         ylim = ylimits,
                         expand = T) +
                theme_void() +
                theme(plot.margin = margin(t = 0.25, r = 0.25, b = 3.5, l = 0.25, "cm"))
city_plot

# Combined plot -----------------------------------------------------------
lab_city <- str_to_upper(cityOI_labels_df %>% filter(City == cityOI_selected) %>% pull(City))
lab_prov <- cityOI_labels_df %>% filter(City == cityOI_selected) %>% pull(Province)
lab_coord <- cityOI_labels_df %>% filter(City == cityOI_selected) %>% pull(coord_txt)

lab_city_xy <- c(0.335, 0.13)
lab_prov_xy <- c(0.335, 0.085)
lab_coord_xy <- c(0.335, 0.05)

fig0 <- ggdraw(city_plot) +
                draw_grob(grid::circleGrob(gp = grid::gpar(fill = "white", col = color1, lwd = 4)), 
                          x = 0.3075, y = 0.010, height = 0.29) +
                draw_plot(ordinal_plot, x = 0.580, y = 0, width = 0.45, height = 0.30) +
                draw_label(lab_city, x = lab_city_xy[1], y = lab_city_xy[2], hjust = 0.5, 
                           size = 60, fontfamily = "Arial Narrow", fontface = "bold", color = color1) +
                draw_label(lab_prov, x = lab_prov_xy[1], y = lab_prov_xy[2], hjust = 0.5, 
                           fontfamily = "Arial Narrow", size = 18) +
                draw_label(lab_coord, x = lab_coord_xy[1], y = lab_coord_xy[2], hjust = 0.5, 
                           fontfamily = "Arial Narrow", size = 20) +
                draw_label(label = "the streets of", x = lab_city_xy[1], y = lab_city_xy[2] + 0.0425, hjust = 0.5,
                           size = 14, fontfamily = "Trattatello", fontface = "italic", color = "gray22") +
                draw_line(x = c(0.175, 0.25), y = c(rep(lab_prov_xy[2], 2)), color = "gray11", size = 1.25) +
                draw_line(x = c(0.42, 0.495), y = c(rep(lab_prov_xy[2], 2)), color = "gray11", size = 1.25)
fig0

ggsave2(paste0(cityOI_selected, "_poster.pdf"), plot = fig0, device = cairo_pdf,
        height = 14.0, width = 11.0, units = "in", scale = 1, dpi = 300)