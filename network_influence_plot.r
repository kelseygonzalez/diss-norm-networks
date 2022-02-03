library(GGally)

vacc_raw <-  read_csv(here("data", "vacc_main.csv"), show_col_types = FALSE)  %>% 
  select(FIPS, date, vacc_rate)  %>% 
  mutate(week_num = as.numeric((date - lubridate::ymd("2020-12-28")) / 7)) %>% 
  filter(date == lubridate::ymd("2021-04-26") - 7)

net2  <- readr::read_tsv(here('data', 'social-connectedness', 'county_county.tsv') ) %>% 
  mutate(scaled_sci = scaled_sci/1000000000) %>%
  dplyr::filter(fr_loc == '53033') %>% 
  select(user_loc, fr_loc, weights = scaled_sci) %>% 
  mutate(group = cut_interval(weights, n = 5)) %>% 
  group_by(group) %>% 
  slice_sample(n = 2) %>%
  ungroup() %>% 
  select(-group) %>% 
  filter(user_loc != fr_loc) 
  

net2_node <- vacc_raw  %>% 
  semi_join(net2, by = c("FIPS" = "user_loc"))   %>% 
  bind_rows(filter(vacc_raw, FIPS == '53033')) %>% 
  mutate(vacc_rate = round(vacc_rate, 3),
         type = ifelse(FIPS == '53033', 'ego', 'alter'))

net2 <-  net2 %>% 
  semi_join(net2_node, by = c("user_loc" = "FIPS")) %>% 
  mutate(weights = round(weights, 7))

  
network2 = network(net2, matrix.type = "edgelist",
              directed = TRUE,
              vertices = net2_node)
network2

options(scipen=999)

ggnet2(network2, 
       label = TRUE, 
       arrow.size = 8, 
       arrow.gap = 0.05,
       edge.label = 'weights',
       edge.label.color = 'black',
       edge.label.size = 3,
       # edge.size = "weights",
       node.label = 'vacc_rate',
       node.color = 'type',
       label.color = 'black',
       label.size = 3, 
       palette = c(ego = "#ee8577", alter = "#62929a"))

weighted.sd <- function(x, wt, na.rm = TRUE) {
  if (na.rm) {
    ind <- is.na(x) | is.na(wt)
    x <- x[!ind]
    wt <- wt[!ind]
  }
  wm <- weighted.mean(x, wt)
  sqrt(sum(wt * (x - wm)^2) / sum(wt))
}
weight_avg_vacc <- function(df) weighted.mean(df$vacc_rate, w = df$weights , na.rm = TRUE)
weight_sd_vacc <- function(df) weighted.sd(df$vacc_rate, wt = df$weights , na.rm = TRUE)

net <- net2 %>% left_join(select(net2_node, FIPS, vacc_rate), by = c('user_loc' = "FIPS"))
weight_avg_vacc(net)
weight_sd_vacc(net)
