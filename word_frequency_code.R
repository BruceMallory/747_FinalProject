#wdfrq_over_time <- function(type, target_word)
#wdfrq_over_time(c("News"),"manchin")

type <- c("News", "Op-Ed", "Editorial")
types <- NULL
#BRUCE, we're working on getting this to say "News, Op-Ed, and Editorial"
for (i in 1:length(type)){
  types <- type[i]
}


target_word <-"manchin"
  
df <- by_type("wdfrq", type)
the_word <- filter(df, word == target_word)
#Since the target_word may not be in a given article, need to finesse the code so that word="target_word" and n=0 shows up in word_in_articles, so that I get instances of 0 of that word in an article.  Therefore I've used "people" to get all of the articles and used the no_word data.frame to build in "target_word, n=0, proportion=0" into the word_in_articles data.frame.
no_word <- filter(df, word == 'people') %>%
  mutate(word = target_word,
         n = 0,
         proportion = 0) %>%
  anti_join(the_word, by = "Article")
word_in_articles <- rbind(the_word, no_word) %>%
  arrange(Article)



ggplot(word_in_articles, aes(x = Date, y = proportion)) +
  geom_point() +
  geom_smooth(se=FALSE, method="loess", formula='y~x') +
  theme_tufte() +
  ggtitle(paste("Frequency of the word", target_word,
                "\nwithin each article")) +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1), 
        axis.title.x=element_blank(),
        plot.title=element_text(family="", face="bold", color="blue")) +
  ggtitle(paste("Frequency of the word", target_word)) +
  labs(subtitle=(paste("within each of", nrow(word_in_articles),
                       as.vector(type)],
                      "\narticles from the NYTimes"))) +
  scale_x_datetime(limits=as_datetime(c("2020-11-01", "2021-08-31")),
                        labels=date_format("%b"),
                        breaks=date_breaks("months")) +
  scale_y_continuous(labels=scales::percent_format(accuracy=1))
                       
                      
                        

  geom_vline(xintercept=as_datetime("2021-01-06")) +
  geom_vline(xintercept=as_datetime("2021-04-08")) +
  geom_vline(xintercept=as_datetime("2021-06-16")) +
  geom_vline(xintercept=as_datetime("2021-07-01")) +
  geom_vline(xintercept=as_datetime("2021-07-12")) +
 
