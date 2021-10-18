#Load libraries
library(tidyverse)
library(stringr)
library(readxl)

#Load data
df <- readRDS("data/twitter_nwav_df.rds")

#Define the string of institutions as a regular expression
institutions <- regex("university|college|academy|school|center|centre|department|institution|association|committee|commission|\\.edu|\\.org", ignore_case = TRUE)

#Define new data frame with new variable about 'institution'
# unite() to unite columns into one character column
# mutate() to create new variable
df2 <- df %>%
  unite("united_names", name, screen_name, description, profile_expanded_url,
        sep = " ", remove = FALSE) %>%
  mutate(institution = str_detect(united_names, institutions)) %>%
  select(-united_names) %>%
  relocate(c(name, screen_name, description, profile_expanded_url, institution), .before = text) %>%
  arrange(created_at)

#Load into the contraction data
#Create new columns and transform strings into regex-compatible strings
contractions <- read_excel("~/data/contractions.xlsx") %>%
  mutate(Word = str_to_lower(Word),
         Contraction = str_to_lower(Contraction),
         cont = str_remove_all(Contraction, "[:space:]"),
         cont = str_replace_all(cont, "'|’|ʼ", "['’ʼ]"),
         notcont = str_replace_all(Word, "[:space:]", "[:space:]"))

#Create character strings based on the contraction variables
#cont_words: contraction words
#notcont_words: not contraction (i.e. full form) words
#cstr: combine both
cont_words <- str_flatten(contractions$cont, collapse = "|")
notcont_words <- str_flatten(contractions$notcont, collapse = "|")
cstr <- str_c(cont_words, notcont_words)

#New data frame and extract all contractions using the cstr string
df3 <- df2 %>%
  select(name, description, institution, created_at, text) %>%
  mutate(contraction = str_extract(text, regex(cstr, ignore_case = TRUE)),
         contraction = str_replace_all(contraction, "'|’|ʼ", "'"),
         contraction = as.factor(str_to_lower(contraction)))

#New data frame and create new variable with contraction
#case_when as a vectorized if_else statement
df4 <- df3 %>%
  mutate(contr = case_when(
    str_detect(contraction, regex(cont_words)) ~ "contraction",
    str_detect(contraction, regex(notcont_words)) ~ "not contraction"),
    contr = as.factor(contr))

#Visualize the use of contraction
#count(): count the frequency of contraction by institution/community
#filter(): filter out all NA (not available, null) observations
#group_by() and mutate(): group by institution/community and count the percentage
#ggplot(): plot the percentages by institution 
df4 %>% count(contr, institution) %>%
  filter(!is.na(contr)) %>%
  group_by(institution) %>%
  mutate(total = sum(n),
         percent = n/total) %>%
  ggplot(aes(x = institution, y = percent, fill = contr)) +
  geom_col(position = "dodge")

#Example codes for stringr functions
#Escape \\ character ? to match the actual question mark
q <- "What can you do with a materials engineering degree?"
str_extract(q, "degree?")
str_extract(q, "degree\\?")

#Replace mispelled parts of a string
str_replace("Unviersity", "unvi", "univ")
#The code above did not replace the typo because of the uppercase
#use regex() with the argument ignore_case = TRUE to ignore case
str_replace("Unviersity", regex("unvi", ignore_case = TRUE), "univ")


