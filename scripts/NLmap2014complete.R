#Gemeentekaart inladen
NLD <- readOGR(dsn = "maps/NL_Gemeenten2014", layer = "NL_Gemeenten2014")
NLD@data$Gemeentenaam <- as.character(NLD@data$Gemeentenaam)
NLD@data$Gemeentenaam[NLD@data$Gemeentenaam == "'s-Hertogenbosch"] <- "S HERTOGENBOSCH"

NLD_FORT <- fortify(NLD,region="Gemeentenaam")
NLD_FORT$id <- toupper(NLD_FORT$id)

plot(NLD_FORT, col = "darkgreen", border = "lightgray")
#!!!!!!!!!!!!!!!!!!!!zorg dat je alle regels vanuit dataprepartion.R hebt uitgevoerd!!!!!!!!!!!!!!!!!!!!!!!
#Hieronder een voorbeeld van hoe je de data relatief zet aan het aantal inwoners.
#compleet$aantal <- (compleet$HogerBeroepsonderwijs_106 / compleet$GemiddeldAantalInwoners_81)
compleet$aantal <- ((compleet$Geiten_178 / compleet$InwonersOp31December_78) * 100 )
test <- compleet %>% 
  select(gemeente = gemeente, number = zorgkosten)%>% filter(!is.na(number))

names_and_numbers <- data_frame(id=rownames(NLD@data), gemeente=NLD@data$Gemeentenaam)  %>% 
  mutate(gemeente = toupper(gemeente)) %>% 
  left_join(test, by = "gemeente")


final_map <- left_join(NLD_FORT, names_and_numbers, by = c("id" = "gemeente"))

map <- ggplot() +
  geom_polygon(data = final_map, 
               aes(x = long, y = lat, group = group, fill = number),
               color = 'gray')+
  theme_minimal()+
  scale_fill_distiller(name = "Zorgkosten was ", # change titel legend
                       palette = "Spectral")+ # change the color scheme
  labs(title="geiten Per Gemeente",  caption="Bron: cbs")

print(map) 

