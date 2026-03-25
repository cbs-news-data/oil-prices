library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)
library(readr)
library(DatawRappr)

library(httr2)
library(jsonlite)

# Load environment variables
tryCatch(load_dot_env(), error = function(e) {}) 
dw_api_key <- Sys.getenv("DW_API_KEY")

# Authenticate with Datawrapper
datawrapper_auth(api_key = dw_api_key)


#WTI
WTI_url <- "https://advancedmedia.websol.barchart.com/proxies/timeseries/historical/queryeod.ashx?symbol=CLY00&data=dailynearest&maxrecords=1500&volume=total&order=asc&dividends=false&backadjust=false&daystoexpiration=1&contractroll=expiration"

WTI_resp <- request(WTI_url) |>
  req_headers(
    "User-Agent" = "Mozilla/5.0",
    "Referer" = "https://oilprice.com/commodity-price-charts?page=chart&sym=CLY00"
  ) |>
  req_perform()

WTI_txt <- resp_body_string(WTI_resp)

WTI_df <- read_csv(WTI_txt, col_names = FALSE) %>% 
  select(X1, X2, X3) %>%
  rename(type = X1,
         date = X2,
         price = X3) %>% 
  mutate(type = "WTI")




#Brent
Brent_url <- "https://advancedmedia.websol.barchart.com/proxies/timeseries/historical/queryeod.ashx?symbol=CBY00&data=dailynearest&maxrecords=1500&volume=total&order=asc&dividends=false&backadjust=false&daystoexpiration=1&contractroll=expiration"

Brent_resp <- request(Brent_url) |>
  req_headers(
    "User-Agent" = "Mozilla/5.0",
    "Referer" = "https://oilprice.com/commodity-price-charts?page=chart&sym=CBY00"
  ) |>
  req_perform()

Brent_txt <- resp_body_string(Brent_resp)

Brent_df <- read_csv(Brent_txt, col_names = FALSE) %>% 
  select(X1, X2, X3) %>%
  rename(type = X1,
         date = X2,
         price = X3) %>% 
  mutate(type = "Brent")


WTI_Brent_df <- bind_rows(WTI_df, Brent_df) %>% 
  pivot_wider(names_from = type, values_from = price) %>% 
  mutate(date = as.Date(date)) %>%
  arrange(date) %>% 
  filter(date >= "2021-01-01")

max_date <- max(WTI_Brent_df$date)
max_date_pretty <- format(max_date, "%b %d, %Y")


WTI_Brent_5years <- WTI_Brent_df %>%
  filter(date >= (max_date - years(5)))

WTI_Brent_1year <- WTI_Brent_df %>%
  filter(date >= (max_date - years(1)))

WTI_Brent_30days <- WTI_Brent_df %>%
  filter(date >= (max_date - 30))

write.csv(WTI_Brent_5years, "data/wti_brent_5years.csv", row.names = FALSE)
write.csv(WTI_Brent_1year, "data/wti_brent_1year.csv", row.names = FALSE)
write.csv(WTI_Brent_30days, "data/wti_brent_30days.csv", row.names = FALSE)

note <- paste0("Data through ", max_date_pretty, ". Represents daily nearest values.")


# Upload data and publish 5 year chart
dw_data_to_chart(WTI_Brent_5years, chart_id = "LAgII", api_key = dw_api_key)
dw_edit_chart(chart_id = "LAgII", annotate = note, api_key = dw_api_key)
dw_publish_chart(chart_id = "LAgII", api_key = dw_api_key)

#1 year chart
# Upload data and publish 1 year chart
dw_data_to_chart(WTI_Brent_1year, chart_id = "reh4w", api_key = dw_api_key)
dw_edit_chart(chart_id = "reh4w", annotate = note, api_key = dw_api_key)
dw_publish_chart(chart_id = "reh4w", api_key = dw_api_key)

#30 days chart
# Upload data and publish 30 days chart
dw_data_to_chart(WTI_Brent_30days, chart_id = "FrwMp", api_key = dw_api_key)
dw_edit_chart(chart_id = "FrwMp", annotate = note, api_key = dw_api_key)
dw_publish_chart(chart_id = "FrwMp", api_key = dw_api_key)

