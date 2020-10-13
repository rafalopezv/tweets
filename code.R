
# sobre: análisis de twitter 

# authenticate via access token
library(rtweet)
library(lubridate)
library(tidyverse)
library(viridisLite)
library(magrittr)
library(highcharter)
library(graphTweets)
library(sigmajs)

# token <- create_token(
#   app = "elecciones_bolivia_2019",
#   consumer_key = "GNmZjOP7FYY2iRcBue6zGVdJE ",
#   consumer_secret = "N4sJYQotWRiA0BYerKyl4cxfrCONQVkLxBkEYT2iXiaAOuL0uZ")


#tw <- search_tweets("#VamosASalirAdelante", n = 18000)


tw <- rio::import("tw.Rdata")

#correccion de zona horaria
#tw$created_at <-  tw$created_at - dhours(4)


# ¿son contenidos propios
lang <- getOption("highcharter.lang")
lang$months <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

lang$shortMonths <- c("En", "Feb", "Mar", "Abr", "Mayo", "Jun", 
                      "Jul", "Ag", "Sep", "Oct", "Nov", "Dic")

lang$weekdays <- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", 
                   "Sábado", "Domingo")

options(highcharter.lang = lang)

tw %>% 
  mutate(
    fecha = as.Date(created_at)
  ) %>% 
  group_by(fecha) %>% 
  summarise(
    rt = sum(is_retweet),
    n = n(),
    prop_rt = rt/n*100, 
    prop_propio = 100 - prop_rt
  ) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  gather(
    key, valor, -fecha, -rt, -n
  ) %>% 
  mutate(
    key = str_replace(key, "prop_rt", "Tweet de un tercero retweteado"),
    key = str_replace(key, "prop_propio", "Tweet propio"),
  ) %>% 
  hchart(type = "area", hcaes(x = fecha, y = valor, group = key)) %>% 
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_plotOptions(area = list(
    stacking = "percent",
    lineColor = "#ffffff",
    lineWidth = 1,
    marker = list(
      lineWidth = 0.3,
      lineColor = "#ffffff"
    ))
  ) %>% 
  hc_yAxis(title = list(text = "Porcentaje de tweets<br>propios o retweets")) %>% 
  hc_tooltip(shared = T) %>% 
  hc_chart(style = list(fontFamily = "Source Code Pro")) -> g1

## si todos retuitean quien es el que produce los contenidos
net <- tw %>% 
  slice(1:1000) %>% 
  gt_edges(screen_name, retweet_screen_name) %>% 
  gt_nodes() %>% 
  gt_collect()

c(edges, nodes) %<-% net

edges$id <- 1:nrow(edges)
edges$size <- edges$n

nodes$id <- nodes$nodes
nodes$label <- nodes$nodes
nodes$size <- nodes$n

sigmajs() %>% 
  sg_nodes(nodes, id, size, label) %>% 
  sg_edges(edges, id, source, target) %>% 
  sg_layout() %>% 
  sg_cluster(colors = c("#0C46A0FF", "#41A5F4FF")) %>% 
  sg_settings(
    edgeColor = "default",
    defaultEdgeColor = "#d3d3d3"
  ) %>% 
  sg_neighbours -> g2
  


temp <- tw %>% 
  mutate(
    fecha = as.Date(created_at)
  ) %>% 
  group_by(fecha) %>% 
  summarise(
    rt = sum(is_retweet),
    n = n(),
    prop_rt = rt/n*100, 
    prop_propio = 100 - prop_rt
  ) %>% 
  mutate(twt = n- rt) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(fecha, total = n, twt, rt)

a_1 <- as.data.frame(temp)

categories_column <- "fecha"
measure_columns <- c(colnames(a_1[3:length(a_1)]))

hbr_sn <- highchart() %>%
  hc_xAxis(categories = a_1[, categories_column],
           title = categories_column)

invisible(lapply(measure_columns, function(column) {
  hbr_sn <<-
    hc_add_series(hc = hbr_sn, name = column,
                  data = a_1[, column])
}))

bar_tw <- hbr_sn %>%
  hc_chart(type = "column") %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_legend(reversed = TRUE) %>% 
  hc_add_theme(hc_theme_smpl())  %>%
  hc_yAxis(title = list(text = "Total tweets y retweets por día")) %>% 
  hc_tooltip(shared = T) %>% 
  hc_chart(style = list(fontFamily = "Source Code Pro"))

  





