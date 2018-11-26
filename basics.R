library(jsonlite)
library(dplyr)
library(ggplot2)

# Frische Daten gibt es unter
# https://green-spider.netzbegruenung.de/api/v1/spider-results/table/
sites <- fromJSON("data/table.json")

# Daten aufbereiten
sites$generator[sites$generator == ""] <- "UNBEKANNT"
sites_kv <- filter(sites, meta.level == "DE:KREISVERBAND")
sites_ov <- filter(sites, meta.level == "DE:ORTSVERBAND")

# die sites mit den CMS, die mind. 5 mal vorkommen
sites_top_cms <- sites %>% group_by(generator) %>% filter(n() >= 5) %>% filter(generator != "UNBEKANNT")

# Punkte

## Verteilung der Punkte
ggplot(sites, aes(score)) +
  geom_histogram(bins = 15) +
  labs(title="Verteilung der Punktzahlen", x="Punktzahl", y="Anzahl Sites")
ggsave("plots/punkte_verteilung.png")

## Verteilung der Punktzahlen (nur KV)
ggplot(sites_kv, aes(score)) +
  geom_histogram(bins = 15) +
  labs(title="Verteilung der Punktzahlen (nur KV)", x="Punktzahl", y="Anzahl Sites")
ggsave("plots/punkte_verteilung_kv.png")

## Verteilung der Punktzahlen (nur OV)
ggplot(sites_ov, aes(score)) +
  geom_histogram(bins = 15) +
  labs(title="Verteilung der Punktzahlen (nur OV)", x="Punktzahl", y="Anzahl Sites")
ggsave("plots/punkte_verteilung_ov.png")

## Punkte nach Gliederungsebene
ggplot(sites, aes(x = reorder(meta.level, score, FUN=median), y = score)) +
  coord_flip() +
  geom_boxplot() +
  labs(title="Punktzahl nach Gliederungsebene", x="Gliederung", y="Punkte")
ggsave("plots/punkte_nach_gliederungsebene.png")

## Punkte KV nach Bundesland
ggplot(sites_kv, aes(x=reorder(meta.state, -score, FUN=median), y=score)) +
  coord_flip() +
  geom_boxplot() +
  labs(title="Punktzahl nach Bundesland", x="Bundesland", y="Punkte")
ggsave("plots/punkte_kv_nach_bundesland.png")

## Punkte nach CMS
ggplot(sites_top_cms, aes(x=reorder(generator, -score, FUN=median), y=score)) +
  coord_flip() +
  geom_boxplot() +
  labs(title="Punktzahl nach CMS", y="Punkte", x="CMS")
ggsave("plots/punkte_nach_cms.png")

# Antwortzeiten

## Verteilung der Antwortzeiten (lineare X-Achse)
ggplot(sites, aes(rating.HTTP_RESPONSE_DURATION.value)) +
  geom_histogram(binwidth=50) +
  xlim(0, 2000) +
  labs(title="Verteilung der Antwortzeiten (begrenzt auf 2 Sek.)", x="Antwortzeit in Millisekunden", y="Anzahl Sites")
ggsave("plots/antwortzeiten_verteilung.png")

## Verteilung der Antwortzeiten als Kurve (lineare X-Achse)
ggplot(sites, aes(rating.HTTP_RESPONSE_DURATION.value)) +
  geom_density(adjust = 3) +
  geom_vline(xintercept = median(sites$rating.HTTP_RESPONSE_DURATION.value), colour="blue") +
  xlim(0, 1500) +
  labs(title="Verteilung der Antwortzeiten", x="Antwortzeit in Millisekunden (Median in blau)", y="Häufigkeit")
ggsave("plots/antwortzeiten_verteilung_kurve.png")

## Verteilung der Punkte (logarithmische X-Achse)
ggplot(sites, aes(rating.HTTP_RESPONSE_DURATION.value)) +
  geom_histogram() +
  scale_x_log10() +
  labs(title="Verteilung der Antwortzeiten (logarithmische X-Achse)", x="Antwortzeit in Millisekunden (log.)", y="Anzahl Sites")
ggsave("plots/antwortzeiten_verteilung_log.png")

## Antwortzeiten nach Gliederungsebene
ggplot(sites, aes(x = reorder(meta.level, -rating.HTTP_RESPONSE_DURATION.value, FUN=median), y = rating.HTTP_RESPONSE_DURATION.value)) +
  scale_y_log10() +
  coord_flip() +
  geom_boxplot() +
  labs(title="Antwortzeiten nach Gliederungsebene (logarithmische Achse)", x="Gliederung", y="Antwortzeit in Millisekunden (log.)")
ggsave("plots/antwortzeiten_nach_gliederungsebene_log.png")

## Antwortzeiten nach Bundesland
ggplot(sites, aes(x = reorder(meta.state, -rating.HTTP_RESPONSE_DURATION.value, FUN=median), y = rating.HTTP_RESPONSE_DURATION.value)) +
  scale_y_log10() +
  geom_boxplot() +
  coord_flip() +
  labs(title="Antwortzeiten nach Bundesland", x="Bundesland", y="Antwortzeit in Millisekunden (log.)")
ggsave("plots/antwortzeiten_nach_bundesland_log.png")

## Antwortzeiten nach CMS
ggplot(sites_top_cms, aes(x = reorder(generator, -rating.HTTP_RESPONSE_DURATION.value, FUN=median), y = rating.HTTP_RESPONSE_DURATION.value)) +
  geom_boxplot() +
  geom_hline(yintercept = median(sites$rating.HTTP_RESPONSE_DURATION.value), colour="blue") +
  scale_y_log10() +
  coord_flip() +
  labs(title="Antwortzeiten nach CMS", x="CMS", y="Antwortzeit in Millisekunden (logarithmisch, Median in blau)")
ggsave("plots/antwortzeiten_nach_cms_log.png")
