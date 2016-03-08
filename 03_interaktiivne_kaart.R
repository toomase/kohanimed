# Näidis kuidas teha interaktiivne kaart
# Ei ole kasutatav kune ggiraph package ei kuva täpitähti
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