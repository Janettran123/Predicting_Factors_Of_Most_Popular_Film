setwd("")
#install necessary libraries
#install.packages('dplyr')
#install.packages('fpp3')
#install.packages("igraph")
#install.packages('stringr')
library(dplyr)
library(fpp3)
library(igraph)
library(stringr)
library(ggplot2)


#Bring in the dataset
animation<- read.csv('Animation_Movies.csv')

str(animation)

head(animation)

#Lets break down the genres, we dont want to have something that is just animation as the main genre

anims<- animation %>% separate(genres, into= c('genre1','genre2'), sep = ',')
anims<- filter(anims, genre1 != 'Animation')
str(anims)
colSums(is.na(anims))
#No NA


#Split the Production companies

#Since most of this will be animations, lets make sure to take out 
anims <- anims %>% separate(production_companies, into = c('Main_production','co_production' ),sep = ',')
str(anims)

anims<-anims[, -c(5,10,12, 13,15,16,18,19)]
str(anims)
colSums(is.na(anims))

ggplot(anims, aes(x = genre1, fill = genre1)) +
  geom_bar() +
  scale_fill_manual(values = rainbow(length(unique(anims$genre1)))) +
  labs(title = "Bar Graph of Category Counts",
       x = "Category",
       y = "Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#create graphs based on which genres show up the most, as the main and the sub-genre

#No NA values found!
anims$vote_average

rating <- anims[,c(2,3)]
rating

cind <- rating %>%
  filter(str_detect(title, "cinderella"))

str(cind)



#What if we wanted to only sort it off of adventure movies?
adventure<- anims %>%
  filter(str_detect(genre1, "Adventure"))
names(adventure)
str(adventure)
#We are narrowed down to  704 entries for adventure
#Let's gather all the variables that we can calculate
adventures<- adventure[,c(1:4,7:8,11,17,20:23)]
str(adventures)
names(adventures)



#logistic regression
logis<- glm(popularity~ vote_average + vote_count +runtime + budget, data = adventures, 
            family = 'poisson')
summary(logis)
exp(coef(logis))
