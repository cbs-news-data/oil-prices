library(tidyverse)
library(rvest)
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

  
  
national_prices_regular <- prices %>%
  select(x, regular) %>%
  rename(period = x,
         price = regular) %>%
  mutate(price = as.numeric(str_replace(price, "\\$", ""))) %>% 
  mutate(period = str_replace(period, "Current Avg.", "Today"),
         period = str_replace(period, "Yesterday Avg.", "Yesterday"),
         period = str_replace(period, "Week Ago Avg.", "Last week"),
         period = str_replace(period, "Month Ago Avg.", "Last month"),
         period = str_replace(period, "Year Ago Avg.", "Last year")) %>% 
  mutate(index = case_when(
    period == "Today" ~ 1,
    period == "Yesterday" ~ 2,
    period == "Last week" ~ 3,
    period == "Last month" ~ 4,
    period == "Last year" ~ 5
  )) %>% 
  arrange(desc(index)) %>% 
  select(-index)

today_price <- national_prices_regular %>% filter(period == "Today") %>% pull(price)
yesterday_price <- national_prices_regular %>% filter(period == "Yesterday") %>% pull(price)
last_month_price <- national_prices_regular %>% filter(period == "Last month") %>% pull(price)
last_year_price <- national_prices_regular %>% filter(period == "Last year") %>% pull(price)

today_vs_yesterday <- round(today_price - yesterday_price, 2)
today_vs_last_month <- round(today_price - last_month_price, 2)
today_vs_last_year <- round(today_price - last_year_price, 2)


description <- paste0("On ", prices_updated_date_pretty, ", the average cost of gas nationwide was <b>$", round(today_price, 2), " per gallon</b>. That's <b>$",round(today_vs_yesterday, 2), " ", ifelse(today_vs_yesterday > 0, "higher", "lower"), "</b> than the day before, <b>$",round(today_vs_last_month, 2), " ", ifelse(today_vs_last_month > 0, "higher", "lower"), "</b> than a month ago and <b>$",round(today_vs_last_year, 2), " ", ifelse(today_vs_last_year > 0, "higher", "lower"), "</b> than a year ago.")

# Upload data and publish chart
dw_data_to_chart(national_prices_regular, chart_id = "rT08j", api_key = dw_api_key)
dw_edit_chart(chart_id = "rT08j", intro = description, api_key = dw_api_key)
dw_publish_chart(chart_id = "rT08j", api_key = dw_api_key)


# get state abbreviations, including DC
state_abbs <- c(state.abb, "DC")

# create lookup vector for full state names
state_lookup <- c(setNames(state.name, state.abb),
                  "DC" = "District of Columbia")

# set URL base for state prices
url_base <- "https://gasprices.aaa.com/?state="

get_state_prices <- function(state_abb) {
  
  message("Scraping: ", state_abb)
  
  # get full state name
  state_full <- state_lookup[[state_abb]]
  
  url <- paste0(url_base, state_abb)
  page <- read_html(url)
  
  tbl <- page %>%
    html_element(xpath = "//h1[contains(., 'average gas prices')]/following::table[1]") %>%
    html_table(fill = TRUE) %>% 
    clean_names() %>%
    mutate(
      state = state_abb,
      state_name = state_full
    ) %>%
    select(state, state_name, x, regular) %>% 
    setNames(c("state", "state_name", "period", "price")) %>% 
    mutate(
      price = as.numeric(str_replace(price, "\\$", "")),
      period = str_replace(period, "Current Avg.", "Today"),
      period = str_replace(period, "Yesterday Avg.", "Yesterday"),
      period = str_replace(period, "Week Ago Avg.", "Last week"),
      period = str_replace(period, "Month Ago Avg.", "Last month"),
      period = str_replace(period, "Year Ago Avg.", "Last year")
    )
  
  print(head(tbl))
  
  return(tbl)
}

safe_get_state_prices <- possibly(get_state_prices, otherwise = NULL)

all_state_prices <- map_dfr(state_abbs, safe_get_state_prices)


state_prices_clean <- all_state_prices %>% 
  pivot_wider(names_from = period, values_from = price) %>% 
  mutate(today_vs_yesterday = round(Today - Yesterday, 2),
         today_vs_last_month = round(Today - `Last month`, 2),
         today_vs_last_year = round(Today - `Last year`, 2))


map_note <- paste0("As of ", prices_updated_date_pretty, ".")

# Upload data and publish chart
dw_data_to_chart(state_prices_clean, chart_id = "812II", api_key = dw_api_key)
dw_edit_chart(chart_id = "812II", annotate = map_note, api_key = dw_api_key)
dw_publish_chart(chart_id = "812II", api_key = dw_api_key)

write.csv(national_prices, "data/national_gas_prices_aaa.csv", row.names = FALSE)
write.csv(state_prices_clean, "data/state_gas_prices_aaa.csv", row.names = FALSE)


#table 

state_prices_for_table <- state_prices_clean %>% 
  select(state_name, `Last year`, `Last month`, `Last week`, `Yesterday`, `Today`) %>% 
  rename(State = state_name) %>%
  arrange(desc(`Today`))

# Upload data and publish chart
dw_data_to_chart(state_prices_for_table, chart_id = "TIphM", api_key = dw_api_key)
dw_edit_chart(chart_id = "TIphM", annotate = map_note, api_key = dw_api_key)
dw_publish_chart(chart_id = "TIphM", api_key = dw_api_key)







