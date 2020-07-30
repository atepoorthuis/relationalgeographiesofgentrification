Prepare Twitter Data
================

## Prepare Twitter data

First we read in the raw csv with all tweets.

``` r
df_sf <- read_csv(here("analysis/data/raw_data/lexington-2012-2018.csv")) %>% 
  mutate(year = year(created_at)) %>%
  filter(year <= 2017) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  select(-c(type))
```

    ## Parsed with column specification:
    ## cols(
    ##   id = col_double(),
    ##   u_id = col_double(),
    ##   latitude = col_double(),
    ##   longitude = col_double(),
    ##   type = col_character(),
    ##   created_at = col_datetime(format = "")
    ## )

We then join this to the census tract for KY. This takes a bit of time
so we save the result to .rds to use later.

``` r
options(tigris_use_cache = TRUE, tigris_class="sf") 
## first time you use tidycensus, install your api key to RENVIRON with the following function
# census_api_key("YOURKEYHERE")

acs_ky <- get_acs(state = "KY", geography = "tract", variables = "B19013_001", geometry = T)  %>% 
    st_transform(., "+init=epsg:4326") %>% 
    mutate(county = substr(GEOID, start = 1, stop = 5))
```

    ## Getting data from the 2014-2018 5-year ACS

``` r
df_tract <- st_join(df_sf, acs_ky)
```

    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar

    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar

``` r
write_rds(df_tract, here("analysis/data/derived_data/lexington-2012-2018-tract.rds"))
write_rds(acs_ky, here("analysis/data/derived_data/acs_ky.rds"))
```

## Determining ‘home’ locations

The first step is filtering out users that do not meet the filter
criteria.

``` r
home_filter <- function(data) {
  data %>% 
    group_by(GEOID) %>% 
    mutate(count = n(),
            study_period = suppressWarnings(as.numeric(max(as.Date(created_at)) - min(as.Date(created_at)))),
            unique_days = n_distinct(as.Date(created_at))) %>% 
    filter(count > 5 & study_period > 10 & unique_days > 5) %>%
    ungroup() %>%
    select(-c(count, study_period, unique_days))
}

df_user <- df_tract %>% # 95k users
  st_set_geometry(NULL) %>% 
  select(c(id, u_id, created_at, year, GEOID)) %>% 
  group_by(u_id) %>% 
  nest_legacy() # nest is slow in tidyr 1.0.0

plan(multiprocess)
```

    ## Warning: [ONE-TIME WARNING] Forked processing ('multicore') is disabled
    ## in future (>= 1.13.0) when running R from RStudio, because it is
    ## considered unstable. Because of this, plan("multicore") will fall
    ## back to plan("sequential"), and plan("multiprocess") will fall back to
    ## plan("multisession") - not plan("multicore") as in the past. For more details,
    ## how to control forked processing or not, and how to silence this warning in
    ## future R sessions, see ?future::supportsMulticore

``` r
df_filtered <- df_user %>% 
  mutate(total_count = map_int(data, nrow)) %>% 
  filter(total_count > 10) %>% 
  mutate(data = future_map(data, home_filter))

df_filtered <- df_filtered %>% 
  unnest_legacy()

users_filtered <- unique(df_filtered$u_id)

df_filtered <- df_filtered %>% 
  mutate(period = case_when(
    year < 2014 ~ 1,
    year >= 2014 ~ 2
  ))
```

For the remaining users, we ‘score’ locations and select the one most
likely to be the home location. We will save the result to .rds again so
we can use it in our subsequent analysis.

``` r
home_extract <- function(data){
    data %>% 
        group_by(GEOID) %>% 
        transmute(count = n()) %>%
        ungroup() %>% 
        top_n(n=1, wt = count) %>% 
        slice(1) %>%
        pull(GEOID)
}

df_user_home <- df_filtered %>% 
  group_by(u_id, period) %>% 
  nest_legacy() %>% 
  mutate(homeloc = future_map_chr(data, home_extract)) %>% 
  select(-c(data))

df_user_home_total <- df_filtered %>% 
  group_by(u_id) %>% 
  nest_legacy() %>% 
  mutate(homeloc = future_map_chr(data, home_extract)) %>% 
  select(-c(data))

df_homeloc <- left_join(df_filtered, df_user_home, by = c("u_id", "period")) %>% 
  left_join(., df_user_home_total, by = c("u_id")) %>% 
  rename(homeloc_period = "homeloc.x",
         homeloc_total = "homeloc.y")

write_rds(df_homeloc, here("analysis/data/derived_data/lexington-2012-2018-homeloc.rds"))
write_rds(df_user_home, here("analysis/data/derived_data/user_home.rds"))
write_rds(df_user_home_total, here("analysis/data/derived_data/user_home_total.rds"))
```
