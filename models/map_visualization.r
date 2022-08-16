# libraries

library(tidyverse)
install.packages("maps")
library(maps)
install.packages("mapproj")
library(mapproj)



world_tbl <- map_data("world") %>%
  as_tibble()

world_tbl

# world base
map_data("world") %>%
  ggplot() + 
  geom_polygon(
    aes(long, lat, group = group))


map_data("world") %>%
  ggplot() +
  borders(fill = "gray")



# Ortho Projection

world_base +
  coord_map("ortho", orientation = c(-14, -52, 0))


