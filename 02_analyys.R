
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(scales)
library(ggthemes)
library(ggalt)
library(sp)
library(viridis)

setwd("C:/Users/toomase/Dropbox/DataScience/R/kohanimed")

load("output/algandmed.RData")



# sõnad, mida otsida:
# -vere
# -pera
# -na
# -te
# ranna

eesti_hexabin <- function(sona){
    sonaga_kohanimed <- kohanimed %>%
        mutate(tunnus = ifelse(str_detect(str_to_lower(kohanimi), str_to_lower(sona)), 
                               sona, "NA")) %>%
        filter(tunnus == sona)
    
    sonaga_kohanimed$hex_id <- sprintf("ID%s",
                                       over(SpatialPoints(coordinates(data.frame(sonaga_kohanimed[,c(11, 10)])),
                                                          CRS(proj4string(est_hex_polys))),
                                            est_hex_polys))
    
    est_heat <- count(sonaga_kohanimed, tunnus, hex_id)
    est_heat$log <- log(est_heat$n)
    
    # assign colors to the mapped, scaled values
    bin_ct <- 20
    no_fill <- "#fde725"
    vir <- rev(viridis_pal()(bin_ct+1))
    vir_col <- col_bin(vir[2:length(vir)],
                       range(est_heat$log),
                       bins = bin_ct,
                       na.color = no_fill)
    
    est_heat$fill <- vir_col(est_heat$log)
    
    ggplot() +
        geom_map(data = est_hex_map, map = est_hex_map,
                 aes(x=long, y=lat, map_id=id),
                 size=0.6, color="#ffffff", fill=no_fill) +
        geom_map(data=est_heat, map=est_hex_map,
                 aes(fill=fill, map_id=hex_id),
                 color="#ffffff", size=0.6) +
        scale_fill_identity(na.value=no_fill) +
        theme_map() +
        theme(plot.title = element_text(size = 16, colour = "#614949")) +
        ggtitle(str_c("-", sona, "-")) +
                    annotate("text", x = 25.2, y = 57.5, label = str_c(nrow(sonaga_kohanimed), 
                                                                       " kohta"), 
                             colour = "#c5b1b1") +
                    coord_proj("+init=epsg:3301")
}

vaike <- eesti_hexabin("väike")
suur <- eesti_hexabin("suur")

library(gridExtra)
grid.arrange(vaike, suur, ncol=2)


# interaktiivse kaardi versioon
library(ggiraph)
proov <- ggplot() +
    geom_map(data = est_hex_map, map = est_hex_map,
                         aes(x=long, y=lat, map_id=id),
                         size=0.6, color="#ffffff", fill=no_fill) +
    geom_map_interactive(data=est_heat, map=est_hex_map,
                         aes(fill=fill, map_id=hex_id, tooltip = hex_id),
                         color="#ffffff", size=0.6) +
    scale_fill_identity(na.value=no_fill) +
    theme_map() +
    theme(plot.title = element_text(size = 22, colour = "#614949")) +
    ggtitle(str_c("-", "sona", "-")) +
    annotate("text", x = 25.2, y = 57.5, label = str_c(nrow(sonaga_kohanimed), 
                                                       " kohta"), 
             colour = "#c5b1b1", size = 6) +
    coord_proj("+init=epsg:3301")

ggiraph(code = {print(proov)}, width = 7, height = 5)
