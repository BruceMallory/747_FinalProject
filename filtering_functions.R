words_used <- Article_wdfrq %>% group_by(word) %>%
  summarise(total=sum(n)) %>% 
  arrange(desc(total))

wordcloud(words = words_used$word, freq = words_used$total, min.freq = 50,           
          max.words=200, random.order=FALSE, rot.per=0.35, 
          scale=c(3.5,0.25), colors=brewer.pal(8, "Dark2"))

pos_scores <- left_join(Article_wdfrq, Article_sntmts, by=c("Article", "Type", "Date")) %>% 
  select(word, Article, Type, Date, n, affin_score) %>% 
  filter(affin_score>quantile(affin_score)[4]) %>% 
  group_by(word) %>%
  summarise(total=sum(n)) %>% 
  arrange(desc(total))

wordcloud(words = pos_scores$word, freq = pos_scores$total, min.freq = 50,           
          max.words=200, random.order=FALSE, rot.per=0.35, 
          scale=c(3.5,0.25), colors=brewer.pal(8, "Dark2"))

ps <- pos_scores %>% 
  top_n(20) %>% 
  mutate(word=reorder(word, total)) %>% 
  ggplot(aes(word, total)) +
  geom_col(fill="chartreuse3") +
  xlab(NULL) +
  coord_flip() +
  theme_bw() +
  labs(title="Frequent words in pos. sentiment articles",
       y="Total useages within pos. articles")

neg_scores <- left_join(Article_wdfrq, Article_sntmts, by=c("Article", "Type", "Date")) %>% 
  select(word, Article, Type, Date, n, affin_score) %>% 
  filter(affin_score<quantile(affin_score)[2]) %>% 
  group_by(word) %>%
  summarise(total=sum(n)) %>% 
  arrange(desc(total))

wordcloud(words = neg_scores$word, freq = neg_scores$total, min.freq = 50,           
          max.words=200, random.order=FALSE, rot.per=0.35, 
          scale=c(3.5,0.25), colors=brewer.pal(8, "Dark2"))

ns <- neg_scores %>% 
  top_n(20) %>% 
  mutate(word=reorder(word, total)) %>% 
  ggplot(aes(word, total)) +
  geom_col(fill="red") +
  xlab(NULL) +
  coord_flip() +
  theme_bw() +
  labs(title="Frequent words in neg. sentiment articles",
       y="Total useages within neg. articles")

grid.arrange(ps, ns, ncol=2)
