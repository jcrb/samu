Analyse SAMU
========================================================


```r
library("xts")
```

```
## Loading required package: zoo
## 
## Attaching package: 'zoo'
## 
## Les objets suivants sont masqués from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
library("zoo")

load("samu_archive.Rda")

s67 <- d[d$service == "SAMU 67", ]
xts_s67 <- as.xts(s67, order.by = s67$date)

a <- zoo(s67$affaires, s67$date)
```

```
## Warning: some methods for "zoo" objects do not work if the index entries
## in 'order.by' are not unique
```

```r
plot(a, col = "palegreen")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 


