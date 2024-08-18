
```r
example_frame <- data.frame(
  kantone = c("LU", "ZH", "BE", "VD"),
  kunden = c(1494, 3085, 3416, 2911)
)  
```

```r
library(cake)

ggplot(example_frame, aes(id = kantone, fill = kunden)) +
    geom_canton(data_provider = datenbaecker())

```
