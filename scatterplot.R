library(ggplot2)
compleet$weduwss
ggplot(compleet, aes(x=bol,y=zorgkosten))+
  theme_minimal()+ 
  geom_point(color="cyan")+
  geom_smooth()+
  coord_cartesian(xlim=c(1, 3.3))+
  labs(title="zorgkosten vergeleken met bol leerlingen", y="Zorgkosten per persoon", x="Aantal BeroepsOpleidendeleerweg leerlingen percentage per gemeente", caption="Source: cbs / Vektis")
