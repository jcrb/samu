Préparation des donnnées
========================================================

Les données des SAMU sont tockées dans un fichier *.csv* de 2006 à 2013 inclus (1 fichier par année). La première ligne est le header. L'ensembles des données sont extraites pour former un fichier *.Rda*.


```r
path = "./samu_csv/"
out.file <- ""
file.names <- dir(path, pattern = ".csv")
for (i in 1:length(file.names)) {
    file <- read.table(paste(path, file.names[i], sep = ""), header = TRUE, 
        sep = ",", stringsAsFactors = FALSE)
    out.file <- rbind(out.file, file)
}

write.table(out.file, file = "samu_archive.csv", sep = ",", row.names = FALSE, 
    qmethod = "double", fileEncoding = "utf-8")
d <- out.file
# supprime la ligne 1 qui est vide
d <- d[-1, ]
# transforme la colonne 1 en dates
d$date <- as.Date(d$date, "%d/%m/%Y")
d$service <- as.factor(d$service)
d[, 3:11] <- lapply(d[, 3:11], as.numeric)

save(d, file = "samu_archive.Rda")

names(d)
```

```
##  [1] "date"        "service"     "affaires"    "primaires"   "secondaires"
##  [6] "néonat"      "TIIH"        "ASSU"        "VSAV"        "conseils"   
## [11] "Medecins"
```

```r

str(d)
```

```
## 'data.frame':	5576 obs. of  11 variables:
##  $ date       : Date, format: "2006-12-31" "2006-12-30" ...
##  $ service    : Factor w/ 2 levels "SAMU 67","SAMU 68": 1 1 1 1 1 1 1 1 1 1 ...
##  $ affaires   : num  1450 1202 753 744 851 ...
##  $ primaires  : num  34 36 20 37 41 36 28 40 48 48 ...
##  $ secondaires: num  5 8 12 8 10 2 6 3 7 11 ...
##  $ néonat     : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ TIIH       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ ASSU       : num  124 110 80 75 123 108 115 96 100 101 ...
##  $ VSAV       : num  18 6 13 13 12 11 12 12 18 15 ...
##  $ conseils   : num  150 92 50 84 64 145 185 145 117 47 ...
##  $ Medecins   : num  329 209 76 62 61 371 337 354 151 51 ...
```

          

