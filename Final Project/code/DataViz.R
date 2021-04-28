require(ggplot2)
require(tidyverse)
require(data.table)
require(gridExtra)

train = fread('./data/Corona_NLP_train.csv')
count = train %>% group_by(TweetAt) %>% count
count = count[order(as.Date(count$TweetAt, format="%d-%m-%Y")),]
count = count %>% mutate(count = n)
q = ggplot(count, aes(x = sort(as.Date(TweetAt, format="%d-%m-%Y")), y = count)) + 
  geom_bar(stat = 'identity') + theme_classic() + xlab('TweetAt') + ylab('n')
q= q + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# sentiment
sentiment = train %>% group_by(Sentiment) %>% count
r = ggplot(sentiment, aes(x = Sentiment, y = n)) + geom_bar(stat = 'identity') + theme_classic() 
r =r + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# sentiment - combined
sentiment = sentiment %>% mutate(Sentiment = case_when(Sentiment == 'Extremely Negative' ~ 'Negative',
                                                       Sentiment == 'Negative' ~ 'Negative',
                                                       Sentiment == 'Neutral' ~ 'Neutral',
                                                       Sentiment == 'Positive' ~ 'Positive',
                                                       Sentiment == 'Extremely Positive' ~ 'Positive'))
sentiment = sentiment %>% group_by(Sentiment) %>% count
s = ggplot(sentiment, aes(x = Sentiment, y = n)) + geom_bar(stat = 'identity') + theme_classic() 
s =s + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# location
train = train %>% filter(Location != '')
location = train %>% group_by(Location) %>% count %>% filter(n >100) %>%
  filter(Location !='Worldwide')
location = location %>% mutate(new_location = Location) %>%
  mutate(Location = case_when(Location == 'Toronto, Ontario'~'Canada',
                              Location == 'Canada' ~ 'Canada',
                              Location == 'United States' ~ 'USA',
                              Location == 'United States of America' ~ 'USA',
                              Location == 'USA' ~ 'USA',
                              Location == 'New York, NY'~ 'USA',
                              Location == 'Washington, DC'~ 'USA',
                              Location == 'Los Angeles, CA'~ 'USA',
                              Location == 'India'~ 'India',
                              Location == 'UK'~ 'UK',
                              Location == 'United Kingdom'~ 'UK',
                              Location == 'London, England'~ 'UK',
                              Location == 'Australia'~ 'Australia',
                              Location == 'England, United Kingdom'~ 'UK',
                              Location == 'Chicago, IL'~ 'USA',
                              Location == 'California, USA'~ 'USA',
                              Location == 'San Francisco, CA'~ 'USA',
                              Location == 'Boston, MA'~ 'USA',
                              Location == 'New York, USA'~ 'USA',
                              Location == 'New Delhi, India'~ 'India',
                              Location == 'New York'~ 'USA',
                              Location == 'Atlanta, GA'~ 'USA',
                              Location == 'Mumbai, India'~ 'India')) %>%
  filter(Location !='Global')
location=location %>% group_by(Location) %>% summarise(sum(n))

p = ggplot(location, aes(x = Location, y = `sum(count)`, fill = Location)) +
  geom_col(alpha = 0.6,position = "dodge") +
  coord_polar() +scale_fill_viridis_d()+ylim(-2820,2820)+
  theme_minimal()  + xlab('') +ylab('') +theme(legend.position = "none")
ggsave(p, file = "location.pdf", height = 2.5, width = 3.5)
