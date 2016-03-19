# Funktsioon joonistab valitud sõna sisaldavatest kohanimedest hexabin kaardi
eesti_hexabin <- function(sona){
    # filtreeri ainult sõna sisaldavad kohanimed välja
    sonaga_kohanimed <- kohanimed %>%
        mutate(tunnus = ifelse(str_detect(str_to_lower(kohanimi), str_to_lower(sona)), 
                               sona, "NA")) %>%
        filter(tunnus == sona)
    
    # Määra Eesti hexabin aluskaardi järgi igale asukohale tema ruudu id
    sonaga_kohanimed$hex_id <- sprintf("ID%s",
                                       over(SpatialPoints(coordinates(data.frame(sonaga_kohanimed[,c(11, 10)])),
                                                          CRS(proj4string(est_hex_polys))),
                                            est_hex_polys))
    
    # loenda kohtade arv igas hexabin ruudus
    est_heat <- count(sonaga_kohanimed, tunnus, hex_id)
    
    # kasuta log skaalat kohtade arvust (vajalik värvimiseks)
    est_heat$log <- log(est_heat$n)
    
    # määra värvid
    bin_ct <- 20  # palju erinevaid toone kasutatakse
    no_fill <- "#fde725"  # ilma kohtadeta ruutude värv
    vir <- rev(viridis_pal()(bin_ct+1))
    vir_col <- col_bin(vir[2:length(vir)],
                       range(est_heat$log),
                       bins = bin_ct,
                       na.color = no_fill)
    
    # kasuta värvimiseks log skaalal kohtade arvu
    est_heat$fill <- vir_col(est_heat$log)
    
    # joonista teemakaart
    ggplot() +
        # aluskaart
        geom_map(data = est_hex_map, map = est_hex_map,
                 aes(x=long, y=lat, map_id=id),
                 size=0.6, color="#ffffff", fill=no_fill) +
        # sõna sisaldavate kohtade arv kaardile
        geom_map(data=est_heat, map=est_hex_map,
                 aes(fill=fill, map_id=hex_id),
                 color="#ffffff", size=0.6) +
        scale_fill_identity(na.value=no_fill) +
        theme_map(base_family = "Lucida Sans") +
        theme(plot.title = element_text(size = 20, colour = "#614949")) +
        ggtitle(str_c("-", sona, "-")) +
        annotate("text", x = 25.2, y = 57.5, label = str_c(nrow(sonaga_kohanimed), 
                                                           " kohta"), 
                 colour = "#c5b1b1", size = 5) +
        coord_proj("+init=epsg:3301")  # sobiv projektsioon
}