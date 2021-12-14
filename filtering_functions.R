#'I've started writing this as a function, but have not been able get the
#'variable sent_dict to be succussfully passed into the function.  With the
#'visualization function I used .data[[sent_dict]].  But here that is not
#'working.  It has something to do with the different contexts that sent_dict
#'is being used.  SO: this is a work in progress.  In it's current state, 
#'sent_dict is a dummy variable, and if I want to look at different sentiment
#'dictionaries I need to find/replace "bing_score."

freq_wrds <- function(sent_dict) {
  pos_scores <-
    left_join(Article_wdfrq,
              Article_sntmts,
              by = c("Article", "Type", "Date")) %>%
    select(word, Article, Type, Date, n, bing_score) %>%
    filter(bing_score > quantile(bing_score)[4]) %>%
    group_by(word) %>%
    summarise(total = sum(n)) %>%
    arrange(desc(total))
  
  ps <- pos_scores %>%
    top_n(30) %>%
    mutate(word = reorder(word, total)) %>%
    ggplot(aes(word, total)) +
    geom_col(fill = "chartreuse3") +
    xlab(NULL) +
    coord_flip() +
    theme_bw() +
    labs(title = "Frequent words in pos. sentiment articles",
         subtitle=paste(" Using the bing lexicon"),
         y = "Total useages within pos. articles")

  neg_scores <-
    left_join(Article_wdfrq,
              Article_sntmts,
              by = c("Article", "Type", "Date")) %>%
    select(word, Article, Type, Date, n, bing_score) %>%
    filter(bing_score < quantile(bing_score)[2]) %>%
    group_by(word) %>%
    summarise(total = sum(n)) %>%
    arrange(desc(total))
  
  ns <- neg_scores %>%
    top_n(30) %>%
    mutate(word = reorder(word, total)) %>%
    ggplot(aes(word, total)) +
    geom_col(fill = "red") +
    xlab(NULL) +
    coord_flip() +
    theme_bw() +
    labs(title = "Frequent words in neg. sentiment articles",
         subtitle=paste(" Using the bing lexicon"),
         y = "Total useages within neg. articles")
  
  grid.arrange(ps, ns, ncol = 2)
}
