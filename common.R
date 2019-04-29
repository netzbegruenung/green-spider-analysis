library(jsonlite)
library(dplyr)
library(ggplot2)
library(scales)

# Frische Daten gibt es per
# curl https://green-spider.netzbegruenung.de/api/v1/spider-results/table/ > data/table.json
sites <- fromJSON("data/table.json")

# Daten aufbereiten
sites$generator[sites$generator == ""] <- "UNBEKANNT"
sites_kv <- filter(sites, meta.level == "DE:KREISVERBAND")
sites_ov <- filter(sites, meta.level == "DE:ORTSVERBAND")

sites_generator_gcms <- filter(sites, generator == "typo3-gcms")
sites_generator_urwahl <- filter(sites, generator == "wordpress-urwahl")

sites_reachable <- filter(sites, rating.SITE_REACHABLE.value == "TRUE")

# die sites mit den CMS, die mind. 5 mal vorkommen
# TODO: nicht filtern, sondern nicht-Top-CMSe in "SONSTIGE" umbenennen
sites_top_cms <- sites %>% group_by(generator) %>% filter(n() >= 5) %>% filter(generator != "UNBEKANNT")
