source("common.R")

# Beschreibung der Punktzahl
summary(sites$score)

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


## Einzelne Site-Kriterien

# Feeds nach CMS
ggplot(sites_top_cms, aes(rating.FEEDS.value)) +
  geom_bar(aes(fill = generator)) +
  scale_x_discrete(labels=c("Ohne Feed","Mit Feed")) +
  scale_fill_discrete(name = "CMS") +
  labs(title="Sites mit und ohne Feed nach CMS",
       x="",
       y="Anzahl Sites")
ggsave("plots/feeds_nach_cms_a.png")

ggplot(sites_top_cms, aes(generator)) +
  geom_bar(aes(fill = rating.FEEDS.value)) +
  coord_flip() +
  scale_fill_discrete(name = "Feed vorhanden", labels = c("Nein", "Ja")) +
  labs(title="Sites mit und ohne Feed nach CMS",
       x="",
       y="Anzahl Sites")
ggsave("plots/feeds_nach_cms_b.png")

# Feeds - Nur GCMS
ggplot(sites_generator_gcms, aes(rating.FEEDS.value)) +
  geom_bar()
