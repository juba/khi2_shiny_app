#library("questionr")
#data(hdv2003)
#d <- hdv2003
load("www/data/hdv2003.Rdata")
d.vars <- names(d)
d$clso <- as.character(d$clso)
d$clso[d$clso == "1"] <- "Oui"
d$clso <- factor(d$clso, levels = c("Oui", "Non", "Ne sait pas"))
d$trav.satisf <- factor(d$trav.satisf, levels = c("Satisfaction", "Equilibre", "Insatisfaction"))
