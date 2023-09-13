- 👋 Hi
- , I’m @Nisola5
- 👀 I’m interested in ...
- 🌱 I’m currently learning ...
- 💞️ I’m looking to collaborate on ...
- 📫 How to reach me ...

<!---
Nisola5/Nisola5 is a ✨ special ✨ repository because its `README.md` (this file) appears on your GitHub profile.
You can click the Preview link to take a look at your changes.
--->
# Load necessary libraries
library(tidyverse)
library(rwhatsapp)
library(WhatsR)
library(tidytext)
library(dplyr)
library(ggplot2)
library(lubridate)
library(wordcloud)
library(plotly)
library(ggimage)
library(stringr)
library(stringi)
library(emoji)
library(emojifont)

### Read Whatsapp exported message
uR <- rwa_read("C:/Users/PC-039/Downloads/WhatsApp Chat with Teachers Lounge Ikorodu - Level 2 Teachers.txt")
view(uR)
fom <- rwa_read("C:/Users/PC-039/Downloads/WhatsApp Chat with SAIL _ Data Science Fellowship.txt")
view(fom)
pha <- rwa_read("C:/Users/PC-039/Downloads/WhatsApp Chat with LAPs & Appeal For BOSYOPS.txt")
view(pha)

#### combined two datas with "rbind"
binddata <- rbind(uR,fom,pha) 
view(binddata)

#### 
wa <- c("C:/Users/PC-039/Downloads/WhatsApp Chat with Teachers Lounge Ikorodu - Level 2 Teachers.txt"="teachers",
 "C:/Users/PC-039/Downloads/WhatsApp Chat with SAIL _ Data Science Fellowship.txt"="sail",           
 "C:/Users/PC-039/Downloads/WhatsApp Chat with LAPs & Appeal For BOSYOPS.txt"="appeals") 

binddata$group <- str_replace_all(binddata$source,wa)


#### To check all the phone numbers
unique(binddata$author)

####created data frame to convert author's data to rewe
rewe <- data.frame( author=unique(binddata$author))

#### anonimization of phone numbers i.e allocating unique ID to each phone number
code <- character(nrow(rewe))
for(i in 1:nrow(rewe)){code[i] <- paste0("ccid",i)}

##### Match the phone number with a unique ID
binddata$ID <- code[match(binddata$author,rewe$author)]


#### To remove words from the data_set
words_to_exclude <- c("joined","using", "this", "group's","invite","link","You","were","added","removed","ikorodu","lounge",
                      "<Media omitted>"  ,"media","omitted","removed" )
pattern <- paste(words_to_exclude, collapse = "|")
prepdata <- gsub( pattern, "", binddata$text)
view( prepdata)

#### To remove number from the text
content <- gsub("\\d+", "", prepdata)
view(content)

##### Combine the cleaned data with the real data
bbinddata <- cbind(binddata,content)
view(bbinddata)

### Dropped the empty row and columns
bidata <- bbinddata%>%
  drop_na()
view(bidata)

###### Clean column 'source',"author" and "text"
B3 <- bidata%>%
  select(-source,-author,-text)
view(B3)

library(janeaustenr)
library(stringr)
library(tidytext)
B4 <- B3%>%
  select(content,group,ID)

analysis <-B4%>%
  group_by(group) %>%
  mutate(chat = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, content)


#counting the positive words only in group = appeals
positive_senti <- get_sentiments("bing") %>%
  filter(sentiment == "positive")
positiveAppeals <- analysis %>%
  filter(group == "appeals")%>%
  semi_join(positive_senti) %>%
  count(word, sort = TRUE)
  
Appeals <- analysis%>%
  filter(group=="appeals")

#we specify based on  lines whi   ch is positive, which is negative
library(tidyr)
bing <- get_sentiments("bing")
Appeals_sentiment <- Appeals %>%
  inner_join(bing) %>%
  count(group = "appeals",ID , index = chat, sentiment, sort = TRUE) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)%>%
  arrange(index)

#visualizations
library(ggplot2)
ggplot(Appeals_sentiment, aes(index, sentiment, fill = group)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~group, ncol = 2, scales = "free_x")
counting_words <- Appeals%>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)
head(counting_words)

#visualizations
library(ggplot2)
ggplot(Emma_sentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")
#word count
counting_words <- tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)
head(counting_words)

counting_words %>%
  filter(n > 5) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment))+
  geom_col() +
  coord_flip() +
  labs(y = "Sentiment Score")

#comparisoncloud
library(reshape2)
library(wordcloud)
Appeals %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "darkgreen"),
                   max.words = 100)


