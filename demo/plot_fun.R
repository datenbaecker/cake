
library(cake)
library(dplyr)

all_plz <- get_plz_entites()

all_plz %>%
  mutate(kunden = rnorm(nrow(.), 10, 5)) %>%
  sample_n(200) %>%
  ggplot(aes(id = id, label = id)) +
  # Ganze Schweiz ohne sichtbare Kantonsgrenzen
  geom_canton(aes(id = label), color = "white", fill = "white", data = get_cantonal_entites()) +
  # Kantonsgrenzen
  geom_canton(aes(id = label), color = "black", fill = scales::alpha("orange", alpha = 0), data = get_cantonal_entites()) +
  geom_plz(aes(size = kunden, color = kunden), geometry_type = "point", alpha = 0.8) +
  geom_plz(geometry_type = "point", size = 0.4)
