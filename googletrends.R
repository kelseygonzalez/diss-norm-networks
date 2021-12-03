pacman::p_load(tidyverse, gtrendsR, lubridate, here, glue, zoo)

find_mondays <- function(year) {
  mondays <- seq(ymd(glue("{year}-01-01")),ymd(glue("{year}-12-31")),by="1 day")
  mondays <- mondays[wday(mondays,label = TRUE) == "Mon" & mondays < today() - 7]
  return(mondays)
}
find_initial_trends <- function(keyword, period_list){
  period_list <- as.character(period_list)
  
  trends <- tibble(location = character(),hits = numeric(),keyword = character(),date = character())
  
  for (period in period_list){ 
    ppl7<- as.character(ymd(period) + 7)
    trends <- gtrends(
      keyword = keyword,
      geo = "US",
      time = paste(period, ppl7),
      gprop = "web",
      category = 0,
      hl = "en-US")$interest_by_dma %>%
      tibble() %>%
      mutate(date = period) %>% 
      select(-c(geo, gprop)) %>%
      bind_rows(trends)
  }
  
  return(trends)
}
find_scale_point <- function(keyword, year){
  if (year(today()) == year) {
    span <- glue("{year}-01-01 {as.character(today() - 7)}")
    } else {
      span <- glue("{year}-01-01 {year}-12-31")
    }
  
  if (nchar(span) != 21) stop('incorrect span')
  
  print(glue("keyword = {keyword}"))
  print(glue("span = {span}"))
  
  rescale <- gtrends(
    keyword = keyword,
    geo = "US-WA-819",
    time = span,
    gprop = "web",
    category = 0,
    hl = "en-US", 
    onlyInterest = TRUE, 
    low_search_volume = FALSE)$interest_over_time %>% 
    tibble()  %>% 
    mutate(date = ymd(date(date))+1,
           hits = ifelse(hits < 1, NA, hits)) %>%
    fill(hits)%>% 
    select(-c(gprop, keyword, geo, time, category)) %>% 
    rename(hits_rescale = hits)
  
  if (rescale %>% drop_na() %>% nrow() == 0) stop('no rows of data')
  return(rescale)
}
rescale_trends <- function(trends, rescale){
  rescale_ratio <- rescale %>%
    mutate(location = 'Seattle-Tacoma WA') %>% 
    left_join( trends %>% 
                 mutate(date = ymd(date)), by = c('date', 'location'))  %>% 
    mutate(rescale_ratio = hits_rescale/hits)  %>% 
    select(date, rescale_ratio)
  
  trends <- trends %>% 
    mutate(date = ymd(date)) %>% 
    group_by(location) %>% 
    fill(hits) %>%
    ungroup() %>% 
    left_join(rescale_ratio, by = "date") %>% 
    mutate(hits_transformed_a = hits * rescale_ratio)
  return(trends)
}
find_trends <- function(keyword, year){
  trends <- find_initial_trends(keyword, find_mondays(year))
  rescaled<- find_scale_point(keyword, year)
  rescaled_trends <- rescale_trends(trends, rescaled)
  return(rescaled_trends)
}


fox_2020 <- find_trends('Fox News', 2020)


fox_2021 <- find_trends('Fox News', 2021)
write_csv(fox_2021, here('data','trends','fox_dma_2021.csv'))


sahr_2020 %>% 
  filter(location %in% c('Billings, MT', 'Seattle-Tacoma WA', 'Alpena MI', 'Fairbanks AK')) %>% 
  pivot_longer(cols = c("hits", "hits_transformed_a")) %>% 
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
    facet_wrap(~ location)



sahr_2020 <- find_trends('Social distancing', 2020)
sahr_2020 %>%
  mutate(location = str_to_upper(location),
         location = str_replace_all(location,"-", " - "),
         location = str_replace_all(location,",", ""),
         location = str_replace_all(location,"\\s(\\w{2})\\b", " \\(\\1\\)")) %>% 
  select(DMA_google = location, social_dist_trend = hits_transformed_a, date) %>% 
write_csv(here('data','trends','social_distancing_2020.csv'))



vaccine_trend_2021 <- find_trends('COVID-19 vaccine', 2021) %>%
  mutate(location = str_to_upper(location),
         location = str_replace_all(location,"-", " - "),
         location = str_replace_all(location,",", ""),
         location = str_replace_all(location,"\\s(\\w{2})\\b", " \\(\\1\\)")) %>% 
  select(DMA_google = location, vaccine_google_trend = hits_transformed_a, date)


vaccine_trend_2021 %>% 
mutate(vaccine_google_trend = replace_na(vaccine_google_trend, 0)) %>% 
filter(DMA_google %in% c('BILLINGS (MT)', 'SEATTLE - TACOMA (WA)', 'ALPENA (MI)', 'JUNEAU (AK)', 'PHOENIX (AZ)', 'DETROIT (MI)')) %>% 
  ggplot(aes(x = date, y = vaccine_google_trend, color = DMA_google)) +
  geom_line() 

write_csv(vaccine_trend_2021, here('data','trends','vaccine_trend_2021_2021.csv'))
