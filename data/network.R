library(dplyr)
library(graphTweets)
#library(rtweet)

# consumer_key = "tNWHtt2VWcA8YAtZxdS97SiQN"
# consumer_secret = "asKKcTFHo5tVuiKUnLUwauc1ggJ94Il6P18pM9vjYfKAnp0jzR "
# access_token = "46075662-qywNw1SCzDowcLBScDAvahl4chDKyQya97FG8hh0Q"
# access_secret = "HDyWIjH1dvim1hRDKnIfpSTC3XJgDFltK2HnVbnPABmCD"
# tweets_downloader <- function(tag, n, lang='en', retryonratelimit = T){
# 
#   twitter_token <- create_token(
#     app = 'Ergo - Extracting Tweets',
#     consumer_key <- consumer_key,
#     consumer_secret <- consumer_secret,
#     access_token <- access_token,
#     access_secret <- access_secret,
#     set_renv = F
#   )
# 
#   tweet.df <- search_tweets(tag, n = n, include_rts = FALSE, lang = lang, token = twitter_token, retryonratelimit = retryonratelimit, type ='mixed',
#                             geocode = "-22.6,17.01,400mi")
#   print(paste0("Total Tweets downloaded for - ",tag,": ",length(tweet.df$text)))
#   print(paste0("Total Unique Texts downloaded for - ",tag,": ",length(unique(tweet.df$text))))
# 
#   tweet.df$hashtags <- as.character(tweet.df$hashtags)
#   tweet.df$symbols <- as.character(tweet.df$symbols)
#   tweet.df$urls_url <- as.character(tweet.df$urls_url)
#   tweet.df$urls_t.co <- as.character(tweet.df$urls_t.co)
#   tweet.df$urls_expanded_url <- as.character(tweet.df$urls_expanded_url)
#   tweet.df$media_url <- as.character(tweet.df$media_url)
#   tweet.df$media_t.co <- as.character(tweet.df$media_t.co)
#   tweet.df$media_expanded_url <- as.character(tweet.df$media_expanded_url)
#   tweet.df$media_type <- as.character(tweet.df$media_type)
#   tweet.df$ext_media_url <- as.character(tweet.df$ext_media_url)
#   tweet.df$ext_media_t.co <- as.character(tweet.df$ext_media_t.co)
#   tweet.df$ext_media_expanded_url <- as.character(tweet.df$ext_media_expanded_url)
#   tweet.df$mentions_user_id <- as.character(tweet.df$mentions_user_id)
#   tweet.df$mentions_screen_name <- as.character(tweet.df$mentions_screen_name)
#   tweet.df$geo_coords <- as.character(tweet.df$geo_coords)
#   tweet.df$coords_coords <- as.character(tweet.df$coords_coords)
#   tweet.df$bbox_coords <- as.character(tweet.df$bbox_coords)
# 
#   tweet.df
# }
# tt <- tweets_downloader(tag="coronavirus OR covid OR covid19 OR corona OR wuhan OR covid_19", n=50000, lang='en')
# saveRDS(tt, file = "C:/Users/mwandingik/Box/Github/CD19/data/cd19twts2.rds")


ttc2 <- readRDS("./data/cd19twts2.rds")

# convert dates to incremental m/y as integers
posix2int <- function(created_at) {
  #m <- format(created_at, "%m")
  d <- format(created_at, "%d")
  #m <- as.integer(m)
  d <- as.integer(d)

  # mn <- min(m) + min(d) - 1
  # 
  # (m + d) - mn
}

# convert date
ttc <- ttc2 %>% 
  mutate(
    created_at = posix2int(created_at)
  )

# build network
net <- ttc %>% 
  gt_edges(screen_name, mentions_screen_name, created_at) %>% 
  gt_nodes() %>% 
  gt_dyn() %>% 
  gt_collect()

c(edges, nodes) %<-% net #lift edges and notes from list of net
#c("#fb7b63", "#f95c43","#900C3F", "#782c20")
# prep for sigmajs
nodes <- nodes %>% 
  mutate(
    id = nodes,
    label = nodes,
    size = n,
    color = scales::col_numeric(c("#FFC300", "#FF5733","#C70039", "#900C3F", "#782c20"), domain = NULL)(n)
  ) %>% 
  dplyr::select(id, label, size, color, appear = start)

edges <- edges %>% 
  mutate(
    id = 1:dplyr::n()
  ) %>% 
  dplyr::select(id, source, target, appear = created_at, weight = n)

n_tweets <- ttc %>% 
  group_by(created_at) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(cs = cumsum(n))


save(n_tweets, edges, nodes, file = "./data/net.RData")