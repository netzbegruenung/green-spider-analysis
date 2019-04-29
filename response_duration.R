source("common.R")

# Antwortzeiten

## Beschreibung der Antwortzeiten
summary(sites$rating.HTTP_RESPONSE_DURATION.value)

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


## Verhältnis Punkte - Antwortzeit
responsetime_score <- data.frame(score = (sites_reachable$score - sites_reachable$rating.HTTP_RESPONSE_DURATION.score),
                                 duration = sites_reachable$rating.HTTP_RESPONSE_DURATION.value)
max_score = max(responsetime_score$score)
score_breaks <- function(x, n = max_score) pretty(x, n)[pretty(x, n) %% 1 == 0]

### Als Punkt-Plot
ggplot(responsetime_score, aes(floor(score), duration)) +
  geom_point() +
  geom_jitter(width = .5, height = .2) +
  geom_smooth(method = "loess")+
  scale_y_log10(limits = c(30, 10001)) +
  scale_x_continuous(breaks = score_breaks) +
  labs(title="Verhältnis Punktzahl und Antwortzeit",
       subtitle="Punktzahl auf ganze Zahlen gerundet, ohne Punkte für Antwortzeit",
       x="Punktzahl (ohne Punkte für Antwortzeit)",
       y="Antwortzeit (ms, logarithmisch)")
ggsave("plots/antwortzeiten_zu_punkte_point_log.png")

### Als Box-Plot
ggplot(responsetime_score, aes(floor(score), duration)) +
  geom_boxplot(aes(group = cut_interval(floor(score), (max_score - 1)))) +
  scale_y_log10(limits = c(30, 10001)) +
  scale_x_continuous(breaks = score_breaks) +
  labs(title="Verhältnis Punktzahl und Antwortzeit",
       x="Punktzahl (ohne Punkte für Antwortzeit)",
       y="Antwortzeit (ms, logarithmisch)")
ggsave("plots/antwortzeiten_zu_punkte_box_log.png")

# Urwahl3000

## Beschreibung der Antwortzeiten
summary(sites_generator_urwahl$rating.HTTP_RESPONSE_DURATION.value)

## Verteilung der Antwortzeiten (lineare X-Achse)
ggplot(sites_generator_urwahl, aes(rating.HTTP_RESPONSE_DURATION.value)) +
  geom_histogram(binwidth=100) +
  geom_vline(xintercept = median(sites_generator_urwahl$rating.HTTP_RESPONSE_DURATION.value), colour="blue") +
  xlim(0, 3500) +
  labs(title="Sites mit Urwahl3000: Verteilung der Antwortzeiten (begrenzt auf 3,5 Sek)",
       x="Antwortzeit in Millisekunden",
       y="Anzahl Sites")
ggsave("plots/cms_urwahl_antwortzeiten_verteilung.png")

## Verteilung der Antwortzeiten als Kurve (lineare X-Achse)
ggplot(sites_generator_urwahl, aes(rating.HTTP_RESPONSE_DURATION.value)) +
  geom_density(adjust = 3) +
  geom_vline(xintercept = median(sites_generator_urwahl$rating.HTTP_RESPONSE_DURATION.value), colour="blue") +
  xlim(0, 1500) +
  labs(title="Verteilung der Antwortzeiten", x="Antwortzeit in Millisekunden (Median in blau)", y="Häufigkeit")
ggsave("plots/cms_urwahl_antwortzeiten_verteilung_kurve.png")

## Verteilung der Punkte (logarithmische X-Achse)
ggplot(sites_generator_urwahl, aes(rating.HTTP_RESPONSE_DURATION.value)) +
  geom_histogram() +
  scale_x_log10() +
  labs(title="Verteilung der Antwortzeiten (logarithmische X-Achse)", x="Antwortzeit in Millisekunden (log.)", y="Anzahl Sites")
ggsave("plots/cms_urwahl_antwortzeiten_verteilung_log.png")
