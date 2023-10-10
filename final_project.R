# RedditExtractoR version 3.0.5 was used. Functions changed so it won't work with earlier version
library(RedditExtractoR)
library(dplyr)
library(tidytext)
library(dabestr)
# Data processing for Season 11 and 10

# Creates a dataframe of threads from search results for "Patch 11 notes" 
# over the past year within the League of Legends subreddit
PatchNotes11Threads <- find_thread_urls(keywords="Patch 11. notes", subreddit="leagueoflegends", sort_by="relevance", period="year")
PatchNotes10Threads <- find_thread_urls(keywords="Patch 10. notes", subreddit="leagueoflegends", sort_by="relevance", period="year")


# Filters threads to make sure titles contain the term 'Patch" in title and don't contain "bug"
FilteredPatch11Notes <- PatchNotes11Threads %>% filter(grepl('Patch', title)) %>% filter(!grepl('bug', title))
FilteredPatch10Notes <- PatchNotes10Threads %>% filter(grepl('Patch', title)) %>% filter(!grepl('bug', title))


# Obtains thread information from search results
Patch11NotesContent <- get_thread_content(FilteredPatch11Notes$url)
Patch10NotesContent <- get_thread_content(FilteredPatch10Notes$url)


# Displays comments in a 2d matrix along with author, thread title, etc
Patch11NotesComments <- Patch11NotesContent[["comments"]]
Patch10NotesComments <- Patch10NotesContent[["comments"]]


# Tokenizes the comemnts for both data frames
Patch11Tokens <- Patch11NotesComments %>%
  mutate(index = row_number()) %>%
  group_by(index) %>%
  ungroup() %>%
  unnest_tokens(word, comment)

Patch10Tokens <- Patch10NotesComments %>%
  mutate(index = row_number()) %>%
  group_by(index) %>%
  ungroup() %>%
  unnest_tokens(word, comment)


# Gets afinn sentiment scores for every comment for each season
# As well as adding labels for mean difference testing later
Patch11CommentSentiments <- Patch11Tokens %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(Patch = "eleven")

Patch10CommentSentiments <- Patch10Tokens %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(Patch = "ten")


# Combines both datasets for mean difference testing
RedditComments <- rbind(Patch10CommentSentiments, Patch11CommentSentiments)

# OPTIONAL FOR FASTER RUN TIMES IF YOU WANT TO USE
# THE WHOLE DATA FRAME USE THE COMMENTED CODE BELOW
# Samples 1500 comments from each group
SampledReddit <- RedditComments %>% group_by(Patch) %>% sample_n(1500)

# Creates a dabestr data object for mean difference testing
Redditdabestr <- SampledReddit %>% select(Patch, sentiment) %>% 
  filter(!is.na(sentiment)) %>% 
  dabest(x = Patch,
         y= sentiment,
         idx= c("ten","eleven"),
         paired = FALSE)


# Creates a dabestr object
# SlowRedditDabestr <- RedditComments %>% select(Patch, sentiment) %>%
#   filter(!is.na(sentiment)) %>%
#   dabest(x = Patch,
#          y= sentiment,
#          idx= c("ten","eleven"),
#          paired = FALSE)

# MUCH SLOWER. TAKES AROUND 20-30 OR MORE DEPENDING ON PC MINUTES TO DO MEAN DIFFERENT TESTING
# mean_diff needs the number of reps to be the number of rows within
# the data frame at a minimum
#SlowRedditResults <- mean_diff(SlowRedditDabestr,reps = length(RedditComments$Patch))

# Displays the mean difference and confidence interval
# for mean difference testing
#SlowRedditResults

# Plots the results of the mean difference testing
#plot(SlowRedditResults)

# Obtain the results of 
Redditdabestr %>% mean_diff(reps = 4000) %>% plot()

# Obtains mean difference estimation for the sampled reddit comments
RedditResults <- mean_diff(Redditdabestr,reps = length(SampledReddit$Patch))

# Displays the mean difference and confidence interval
# for mean difference testing
RedditResults

# Plots the results of the mean difference testing
plot(RedditResults)
