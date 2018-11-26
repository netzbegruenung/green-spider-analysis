# Datenanalyse zum Green Spider

Green Spider: https://github.com/netzbegruenung/green-spider

## Vorschau

![antwortzeiten_nach_bundesland_log](https://user-images.githubusercontent.com/273727/49047442-bca51c00-f1d7-11e8-908c-d5c9bb1ba2a8.png)

![antwortzeiten_nach_cms_log](https://user-images.githubusercontent.com/273727/49047450-c2026680-f1d7-11e8-8e87-e74c630de5f6.png)

![punkte_nach_gliederungsebene](https://user-images.githubusercontent.com/273727/49047482-d9d9ea80-f1d7-11e8-893a-ba9e67e36623.png)


## Kurzanleitung

1. Installiere [RStudio](https://www.rstudio.com/products/rstudio/download/)

2. Clone dieses Repository

3. Lade die aktuellen Green Spider Daten:

  ```nohighlight
  mkdir -p data
  curl https://green-spider.netzbegruenung.de/api/v1/spider-results/table/ > data/table.json
  ```

4. Starte RStudio

5. Installiere die `ggplot2` library mit `library.packages("ggplot2")`

6. Öffne das Script `basics.R` und führe es aus

