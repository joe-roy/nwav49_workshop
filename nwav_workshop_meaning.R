library(tensorflow) # Universal Sentence Encoder
library(reticulate) # python in R.

library(rvest)
library(here)
library(uwot) # UMAP (Dimensionality Reduction Algorithm)
library(dbscan) # HDBSCAN (Clustering Algorithm)
library(plotly) # Interactive Plots
library(readxl)

## Text Data Wrangling
library(lubridate) #work with dates
library(tidytext) #tokenizer, tidy text creator
library(SnowballC) #stemming
library(ldatuning) #find topics number
library(topicmodels) #topic modeling



institutions <- regex("university|college|academy|school|center|centre|department|institution|association|committee|commission|\\.edu|\\.org", ignore_case = TRUE)
twitter_handles2020 <- read_csv("data/twitter_handles2020.csv")

#Read in data
tdf <- readRDS("data/twitter_nwav_df.rds") %>%
  #unite user name and description to identify if they are institution or not
  unite("united_names", name, screen_name, description, profile_expanded_url, sep = " ", remove = FALSE) %>%
  mutate(screen_name = as.factor(screen_name),
         institution = str_detect(united_names, institutions)) 


tdf1 = tdf %>%
  mutate(influencetype = case_when(
    screen_name%in% twitter_handles2020$handle ~ "Institution (Profiles)",
    institution & verified ~ "Institution (Verified)",
    institution & !verified ~ "Institution (Affiliated)",
    !institution & verified ~ "Non-Instituion (Verified)",
    TRUE ~ "Non-Institution (Unverified)"
  )) 




## TOPIC MODELING

#Create tibble containing the search words and other irrelevant terms 
sw <- c("engineer", "engineers", "engineering", "educate", "education", "educations",
        "university", "universities", "academia", "academic", "academics",
        "study", "studies", "program", "programs", "faculty", "faculties", "student",
        "students", "professor",  "professors", "alum", "alums", "alumni", "scholar",
        "scholars", "research", "researches", "researcher", "researchers",
        "http","https","rt","t.co","tt","cc","bit.ly", "&amp;", "amp","&lt;","&gt;") %>%
  as_tibble() %>%
  rename(word = 'value')

#Prepare data frame for analysis
tdf2 <- tdf1 %>%
  #select only relevant variables
  select(created_at, status_id, text) %>%
  
  mutate(created_at = ymd_hms(created_at),
         text2 = text,
         text = str_remove_all(text, "@[:graph:]+"),
         text = str_remove_all(text, "http[:graph:]+"),
         text = str_remove_all(text, "[0-9]"),
         text = str_replace_all(text, "[Pp]ay([:space:][:graph:]+){1,2}", ""),
         text = str_replace_all(text, "[:punct:]", " ")) %>%
  unnest_tokens(word, text) %>% #slice up text
  anti_join(sw) %>% #delete search words and stop words
  anti_join(stop_words) %>% #delete English stop words
  mutate(word = wordStem(word)) %>% #stem words
  select(created_at, status_id, word)

tdtm <- tdf2 %>%
  count(status_id, word, sort = TRUE) %>%
  cast_dtm(status_id, word, n)

#Delete rows with zeros
unique_i <- unique(tdtm$i)
tdtm2 <- tdtm[unique_i,]



#Find the appropriate number of topics. (Parameter Tuning)

# tftn <- FindTopicsNumber(
#   dtm = tdtm2,
#   topics = c(2:15), #check between 2 and 15 topics
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
#   method = "Gibbs",
#   control = list(seed = 12345),
#   mc.cores = 4L,
#   verbose = TRUE
# )
# 
# 
# FindTopicsNumber_plot(tftn)


# Actual 


tlda <- LDA(tdtm2, k =11 , control = list(seed = 1234))



tttp <- tidy(tlda, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", nrow = 2) +
  coord_flip() +
  scale_x_reordered()




### Universal Sentence Encoder (USE)

## Setting up Tensorflow and loading USE 
use_condaenv("r-tensorflow", required = TRUE)
#This is needed to load the libary and create the environment for use below. 
hub <- import("tensorflow_hub")

embed <- hub$load("https://tfhub.dev/google/universal-sentence-encoder-large/5")

use_input = tdf1 %>%
  #select only relevant variables
  select(created_at, screen_name, text) %>%
  mutate(created_at = ymd_hms(created_at),
         text2 = text,
         text = str_remove_all(text, "@[:graph:]+"),
         text = str_remove_all(text, "http[:graph:]+"),
         text = str_remove_all(text, "[0-9]"),
         text = str_replace_all(text, "[Pp]ay([:space:][:graph:]+){1,2}", ""),
         text = str_replace_all(text, "[:punct:]", " "))



twitterEmbed = embed(use_input$text)
#Extracting the sentence embeddings matrix.
# Each row represents one tweet
twitumap = as.array(twitterEmbed)

titles = use_input$text



## Explore Relationship
examples = tdf$text[1:10]
exampleProd = twitumap[1:10,]
ip = exampleProd %*% t(exampleProd) # The relationship between tweet i and tweet j is contained in cell(i,j)

 ip %>% as_tibble() %>%
  rowid_to_column(var="X") %>%
  gather(key="Y", value="Z", -1) %>%
  
  # Change Y to numeric
  mutate(Y=as.numeric(gsub("V","",Y))) %>%
  
  # Viz
  ggplot(aes(X, Y, fill= Z)) + 
  geom_tile() +
   scale_y_continuous(breaks=seq(1,10,by=1))+
   scale_x_continuous(breaks=seq(1,10,by=1))+
   scale_fill_continuous(low = "violetred", high = "aquamarine")


 
 
 ## MORE CLOSELY RELATED
 
 examples = tdf$text[201:210]
 exampleProd = twitumap[201:210,]
 ip = exampleProd %*% t(exampleProd) # The relationship between tweet i and tweet j is contained in cell(i,j)
 
 ip %>% as_tibble() %>%
   rowid_to_column(var="X") %>%
   gather(key="Y", value="Z", -1) %>%
   
   # Change Y to numeric
   mutate(Y=as.numeric(gsub("V","",Y))) %>%
   
   # Viz
   ggplot(aes(X, Y, fill= Z)) + 
   geom_tile() +
   scale_y_continuous(breaks=seq(1,10,by=1))+
   scale_x_continuous(breaks=seq(1,10,by=1))+
   scale_fill_continuous(low = "violetred", high = "aquamarine")
 


set.seed(2000) 

# n_neighbors	
# The size of local neighborhood (in terms of number of neighboring sample points) used for manifold approximation. Larger values result in more global views of the manifold, while smaller values result in more local data being preserved. In general values should be in the range 2 to 100.

# n_components	
# The dimension of the space to embed into. This defaults to 2 to provide easy visualization, but can reasonably be set to any integer value in the range 2 to 100.

# min_dist	
# The effective minimum distance between embedded points. Smaller values will result in a more clustered/clumped embedding where nearby points on the manifold are drawn closer together, while larger values will result on a more even dispersal of points. The value should be set relative to the spread value, which determines the scale at which embedded points will be spread out.

august_umap <- umap(twitumap,
                 n_neighbors = 30,
                 n_components = 30,
                 min_dist = 0,
                 metric = "euclidean")
# This function reduces the 512 feature vector to the 
# "most informative" projection onto 30 features


umap_plot <- as_tibble(august_umap) %>% 
  bind_cols(titles) %>% 
  ggplot(aes(x = V15, y = V12, label=titles)) +
  geom_point()

ggplotly(umap_plot)

# Taking the reduced matrix
# minPts is the minimum size of each cluster
clusts <- hdbscan(august_umap, minPts = 25)

length(unique(clusts$cluster))



## Seeing output of each cluster to series of files--clusters

#Append each cluster to the original data set
tdf$cluster = clusts$cluster

#Remove all lists (e.g. hashtags) that cause problems with the next 
#line -- there were 17 columns removed.  
tdfall = tdf %>% select_if(~!is.list(.))


#This generates a csv with each clusters data (for examination)
#in it. 
tdfall %>% group_by(cluster) %>%
  group_walk(~write.csv(.x, paste0("clusters/",.y$cluster,"_cluster.csv")))

#I used the results from this to calculate the % institutional 
table(tdfall$cluster,tdfall$institution) 


