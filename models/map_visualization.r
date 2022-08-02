# libraries

library(tidyverse)
library(maps)
library(mapproj)



world_tbl <- map_data("world") %>%
  as_tibble()

world_tbl

# world base
world_base <- world_tbl %>%
  ggplot() + 
  geo_map(
    aes(long, lat, map_id = region),
    map = world_tbl
    color = "gray80", fill = "gray30", size = 0.3
  )


world_base

# Ortho Projection

world_base +
  coord_map("ortho", orientation = c(-14, -52))


