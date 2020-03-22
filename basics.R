source("common.R")

# Beschreibung der Punktzahl
summary(sites)
summary(sites$score)

# Punkte

## Verteilung der Punkte
ggplot(sites, aes(score)) +
  geom_histogram(bins = 15) +
  labs(title="Verteilung der Punktzahlen", x="Punktzahl", y="Anzahl Sites")
ggsave("plots/punkte_verteilung.png")


## Verteilung der Punktzahlen (nur KV)
summary(sites_kv$score)

ggplot(sites_kv, aes(score)) +
  geom_histogram(bins = 15) +
  labs(title="Verteilung der Punktzahlen (nur KV)", x="Punktzahl", y="Anzahl Sites")
ggsave("plots/punkte_verteilung_kv.png")

## Verteilung der Punktzahlen (nur OV)
summary(sites_ov$score)

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


## Alle Kriterien

sites_rating_criteria <- select(sites,
                                rating.DNS_RESOLVABLE_IPV4.value,
                                rating.SITE_REACHABLE.value,
                                rating.CANONICAL_URL.value,
                                rating.WWW_OPTIONAL.value,
                                rating.HTTPS.value,
                                rating.RESPONSIVE.value,
                                rating.CONTACT_LINK.value,
                                rating.SOCIAL_MEDIA_LINKS.value,
                                rating.NO_NETWORK_ERRORS.value,
                                rating.NO_SCRIPT_ERRORS.value,
                                rating.NO_THIRD_PARTY_COOKIES.value,
                                rating.FAVICON.value,
                                rating.USE_SPECIFIC_FONTS.value,
                                rating.FEEDS.value,
)
sites_rating_criteria_long <- tidyr::gather(sites_rating_criteria, key = type_col, value = categories)

sapply(sites_rating_criteria_long,table)

# requires library(qdapTools)
mtabulate(sites_rating_criteria_long)

(sum(sites_rating_criteria$rating.FEEDS.value) / count(sites)) * 100
(sum(sites_rating_criteria$rating.SOCIAL_MEDIA_LINKS.value) / count(sites)) * 100
(sum(sites_rating_criteria$rating.RESPONSIVE.value) / count(sites)) * 100
(sum(sites_rating_criteria$rating.NO_THIRD_PARTY_COOKIES.value) / count(sites)) * 100


ggplot(sites_rating_criteria_long, aes(x = categories, fill = categories)) +
  geom_bar() + 
  coord_flip() +
  facet_wrap(~ type_col, scales = "free_x")

ggplot(sites_rating_criteria_long, aes(x = categories, fill = categories)) +
  geom_bar()


# Network Requests
summary(sites$rating.NETWORK_REQUESTS.value)

ggplot(sites, aes(rating.NETWORK_REQUESTS.value)) +
  geom_histogram(bins = 30) +
  xlim(0, 100) +
  labs(title="HTTP-Anfragen: Verteilung", x="Anfragen", y="Anzahl Sites")
ggsave("plots/network_requests_verteilung.png")

# Network Payload
summary(sites$rating.NETWORK_PAYLOAD.value / 1000)

ggplot(sites, aes(rating.NETWORK_PAYLOAD.value / 1000000.0)) +
  geom_histogram(bins = 30) +
  xlim(0, 8) +
  labs(title="Datentransfer: Verteilung", x="Datentransfer (MB)", y="Anzahl Sites")
ggsave("plots/network_payload_verteilung.png")


# Network request vs. payload
ggplot(sites, aes(rating.NETWORK_REQUESTS.value, rating.NETWORK_PAYLOAD.value / 1000000.0)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  xlim(0, 100) +
  ylim(0, 8) +
  labs(title="HTTP-Anfragen vs. Datentransfer", x="Requests", y="Payload (MB)")
ggsave("plots/network_payload_vs_requests.png")

mylm <- lm(sites$rating.NETWORK_REQUESTS.value ~ sites$rating.NETWORK_PAYLOAD.value)
summary(mylm)

# Vergleich Score und Network parameter
ggplot(sites, aes(rating.NETWORK_REQUESTS.value, score)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess") +
  xlim(0, 200) +
  ylim(5, 16) +
  labs(title="HTTP-Anfragen vs. Score", x="Requests", y="Score")
ggsave("plots/network_requests_vs_score.png")

lm_requests_score <- lm(sites$rating.NETWORK_REQUESTS.value ~ score)
summary(lm_requests_score)

ggplot(sites, aes(rating.NETWORK_PAYLOAD.value / 1000000.0, score)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess") +
  xlim(0, 8) +
  ylim(5, 16) +
  labs(title="Datentransfer vs. Score", x="Datentransfer (MB)", y="Score")
ggsave("plots/network_payload_vs_score.png")

lm_requests_score <- lm(sites$rating.NETWORK_REQUESTS.value ~ score)
summary(lm_requests_score)

# Datentransfer nach CMS
ggplot(sites_top_cms, aes(x=reorder(generator, rating.NETWORK_PAYLOAD.value, FUN=median), y=rating.NETWORK_PAYLOAD.value / 1000000.0)) +
  coord_flip() +
  geom_boxplot() +
  labs(title="Datentransfer nach CMS", y="Datentransfer (MB)", x="CMS")
ggsave("plots/network_payload_nach_cms.png")

# HTTP-Anfragen nach CMS
ggplot(sites_top_cms, aes(x=reorder(generator, rating.NETWORK_REQUESTS.value, FUN=median), y=rating.NETWORK_REQUESTS.value)) +
  coord_flip() +
  geom_boxplot() +
  ylim(0, 260) +
  labs(title="HTTP-Anfragen nach CMS", y="Anfragen", x="CMS")
ggsave("plots/network_requests_nach_cms.png")


