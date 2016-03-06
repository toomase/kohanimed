# Hexabin kaardil Eesti kohanimede esinemissageduse kuvamine
# Inspiratsiooni saadud: http://truth-and-beauty.net/experiments/ach-ingen-zell/
# R-s tehtud sama projekt: http://rud.is/b/2016/01/03/zellingenach-a-visual-exploration-of-the-spatial-patterns-in-the-endings-of-german-town-and-village-names-in-r/
# R kood, mida kasutasin andmete ettevalmistamiseks: https://gist.github.com/hrbrmstr/f3d2568ad0f27b2384d3

library(raster)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

setwd("C:/Users/toomase/Dropbox/DataScience/R/kohanimed")

# Aadressilt http://xgis.maaamet.ee/knravalik/  kõik Eesti ametlikud kohanimed
# Kokku ca 156 000
# Kõigi kohanimede pärimisek tuleb teha otsin ilma märksõnata ja tulemus salvestada csv-na
kohanimed_raw <- read_csv2("data/kohanimed.csv", col_types = "nccccc_ccc_")
names(kohanimed_raw) <- str_replace_all(make.names(str_to_lower(names(kohanimed_raw))),
                                        "\\.", "_")

# Ainult eestikeelsed kohanimed
kohanimed_coordinates <- kohanimed_raw %>%
    filter(keel == "eesti") %>%
    # x ja y koordinaadid numbriteks
    mutate_each(funs(as.numeric(.)), x, y)

# Kuna kohanime andmed on L-EST97 koordinaatsüsteemis (x ja y)
# siis tuleb need ümber konvertida geodeetilisteks koordinaatideks
# Selleks kasutasin veebirakendust: http://www.maaamet.ee/rr/geo-lest/
# Vajalik on lihtsalt x ja y veerust koosnev tabel, mida saab konvertida
lest_coordinates <- kohanimed_coordinates %>%
    select(y, x) %>%
    na.omit()

# Koordinaatide konvertimise lõpptulemuse laadimine (lat ja long)
geo_coordinates_raw <- read_csv("data/geo_coordinates.csv")
names(geo_coordinates_raw) <- c("lat", "long")

# x/y koordinaadid ja lat/long koordinaadid ühes tabelis koos
coordinates <- lest_coordinates %>%
    bind_cols(geo_coordinates_raw)

# lat/long koordinaadid kohanimede tabeli külge
kohanimed <- kohanimed_coordinates %>%
    left_join(coordinates, by = c("x" = "x", "y" = "y")) %>%
    distinct()

# Eesti kaart hexabin aluskaardi tegemiseks
# Kasutan maakondade põhist kaarti, et Peipsi järv jääks välja
est_shp = subset(getData('GADM', country = 'EST', level = 1),
                 NAME_1 %in% c("Ida-Viru", "Tartu", "Jõgeva", "Hiiu", "Lääne",
                               "Valga", "Pärnu", "Järva", "Saare", "Viljandi",
                               "Põlva", "Võru", "Rapla", "Harju", "Lääne-Viru"))

# Aluskaardist hexabin kaardi tegemine
# cellsize argumendiga saab määrata kuusnurga suuruse (mida väiksem, seda rohkem kuusnurki) 
est_hex_pts <- spsample(est_shp, type = "hexagonal", cellsize = 0.12,
                        offset = c(0.5, 0.5), pretty = TRUE)

# Hexabin kaardi konvertimine polügonideks, mida saab ggplot2 kaardil kasutada
est_hex_polys <- HexPoints2SpatialPolygons(est_hex_pts)

# Vajalik transformatsioon ggplot2 jaoks. Lõpptulemuseks on valmis aluskaart.
est_hex_map <- fortify(est_hex_polys)

# Lõpptulemuse salvestamine, et seda edasises analüüsis kasutada
save(est_hex_map, est_hex_polys, kohanimed, file = "output/algandmed.RData")