
#'There are four article "Types": Op-Ed, Letter, Editorial, News.  
#'I've built my display functions so that it can select a subset of the articles 
#'based on an input that is a character string - e.g. c("Op-Ed", "Editorial").


#'NOTE for by_type: which is either "wdfrq" or "sntmts".  type is a character 
#'string including the four article types (Op-Ed, Letter, Editorial, News).
by_type <- function(frame, type) {
  if (frame == "wdfrq") {
    df <- initialize_wdfrq_df()
    for (i in 1:length(type)) {
      new <- filter(Article_wdfrq, Type == type[i])
      df <- rbind(df, new)
    }
  }
  else{
    df <- initialize_sntmts_df()
    for (i in 1:length(type)) {
      new <- filter(Article_sntmts, Type == type[i])
      df <- rbind(df, new)
    }
  }
  return(df)
}

wdfrq_over_time <- function(type, target_word) {
  df <- by_type("wdfrq", type)
  the_word <- filter(df, word == target_word)
  #'Since the target_word may not be in a given article, need to finesse the code
  #'so that word="target_word" and n=0 shows up in word_in_articles, so that I
  #'get instances of 0 of that word in an article.  Therefore I've used "people"
  #'to get all of the articles and used the no_word data.frame to build in
  #'"target_word, n=0, proportion=0" into the word_in_articles data.frame.
  no_word <- filter(df, word == 'people') %>%
    mutate(word = target_word,
           n = 0,
           proportion = 0) %>%
    anti_join(the_word, by = "Article")
  word_in_articles <- rbind(the_word, no_word) %>%
    arrange(Article)
  line.data <- data.frame(xintercept = c(as_datetime("2021-01-06"), 
                                         as_datetime("2021-04-08"),
                                         as_datetime("2021-06-16"),
                                         as_datetime("2021-07-01"),
                                         as_datetime("2021-07-12")), 
                            Key_Dates = c("01/06 : Manchin publishes op-ed saying he will not alter filibuster",
                            "04/08 : Manchin offers amendments and expresses willingness to alter filibuster",
                            "06/16 : Senate blocks debate on HR1: For The People Act",
                            "07/01 : Supreme Court hands down major voting rights decision",
                            "07/12 : Texas Democrats flee state to protest voting restrictions"),
                          stringsAsFactors = FALSE)
  
  ggplot(word_in_articles, aes(x = Date, y = proportion)) +
    geom_jitter(aes(fill=cut(n, c(-Inf,0,Inf))), shape=21, size=3)  +
    scale_fill_manual(guide="none", values = c("white", "darkslateblue")) +
    geom_smooth(color="grey", se=FALSE) +
    geom_vline(aes(xintercept = xintercept, color = Key_Dates), line.data, lty=2) +
    theme_bw() +
    xlab("") +
    ylab(paste("Proportion of \"", target_word, "\" within each article")) +
    theme(text = element_text(family = "Times New Roman"),
          axis.text.x=element_text(angle=45, hjust=1, vjust=1, face="bold"),
          axis.text.y=element_text(face="bold"),
          plot.title = element_text(face="bold", size=20),
          plot.subtitle = element_text(size=14),
          legend.position = "bottom",
          legend.justification = "right",
          legend.title = element_text(size=16, color = "blue"),
          legend.text = element_text(size=12, color = "blue"),
          legend.background = element_rect(colour = "blue"),
          panel.grid.minor = element_blank()) +
    guides(color=guide_legend("Key Dates", nrow=5, byrow=FALSE)) +
    scale_color_manual(values=c("blue", "blue", "blue", "blue", "blue")) +
    scale_x_datetime(breaks=date_breaks("months"),
                     labels=date_format("%B"),
                     limits=as_datetime(c("2020-11-01", "2021-08-31"))) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
    labs(title=paste("Frequency that \"", target_word,
                     "\" is mentioned within a NYTimes article about H.R.1"), 
         subtitle=paste(" Total of",nrow(word_in_articles), "articles in", 
                        paste(type, collapse=", "), "\n", 
                        paste("\"", target_word, "\" is mentioned in", 
                              nrow(filter(word_in_articles, n >0)), "of them")))
       
}

#Code below was used for building the wdfq_over_time function
#wdfrq_over_time(c("Op-Ed", "Editorial", "Letter", "News"),"supreme")

#type <- c("Op-Ed", "Editorial", "Letter", "News")
#target_word <- "manchin"

sntmts_over_time <- function(type, sent_dict, target_word) {
  sntmnt_in_articles <- by_type("sntmnt", type)
  df <- by_type("wdfrq", type)
  the_word <- filter(df, word == target_word)
  #'Since the target_word may not be in a given article, need to finesse the code
  #'so that word="target_word" and n=0 shows up in word_in_articles, so that I
  #'get instances of 0 of that word in an article.  Therefore I've used "people"
  #'to get all of the articles and used the no_word data.frame to build in
  #'"target_word, n=0, proportion=0" into the word_in_articles data.frame.
  no_word <- filter(df, word == 'people') %>%
    mutate(word = target_word,
           n = 0,
           proportion = 0) %>%
    anti_join(the_word, by = "Article")
  word_in_articles <- rbind(the_word, no_word) %>%
    arrange(Article)
  sntmnt_in_articles <- left_join(sntmnt_in_articles, word_in_articles)
  
  line.data <- data.frame(xintercept = c(as_datetime("2021-01-06"), 
                                         as_datetime("2021-04-08"),
                                         as_datetime("2021-06-16"),
                                         as_datetime("2021-07-01"),
                                         as_datetime("2021-07-12")), 
                          Key_Dates = c("01/06 : Manchin publishes op-ed saying he will not alter filibuster",
                                        "04/08 : Manchin offers amendments and expresses willingness to alter filibuster",
                                        "06/16 : Senate blocks debate on HR1: For The People Act",
                                        "07/01 : Supreme Court hands down major voting rights decision",
                                        "07/12 : Texas Democrats flee state to protest voting restrictions"),
                          stringsAsFactors = FALSE)
  
  if (target_word == "none") {
    point_size <- 3
  }  else {
    point_size <- 200 * sntmnt_in_articles$proportion + 3
  }
  
  ggplot(sntmnt_in_articles, aes(x = Date, y = .data[[sent_dict]])) +
    geom_smooth(color="grey", se=FALSE) +
    geom_jitter(shape=21, size=point_size, aes(fill=.data[[sent_dict]])) +
    scale_fill_gradient2(guide="none", low="red", high ="chartreuse3", mid="yellow") +
    # scale_fill_viridis(guide = "none", option = "turbo",
    #                    direction = -1, begin = .1, end = .9) +
    geom_vline(aes(xintercept = xintercept, color = Key_Dates), line.data, lty=2) +
    theme_bw() +
    xlab("") +
    ylab(paste(sent_dict, "score for each article")) +
    theme(text = element_text(family = "Times New Roman"),
          axis.text.x=element_text(angle=45, hjust=1, vjust=1, face="bold"),
          axis.text.y=element_text(face="bold"),
          plot.title = element_text(face="bold", size=20),
          plot.subtitle = element_text(size=14),
          legend.position = "bottom",
          legend.justification = "right",
          legend.title = element_text(size=16, color = "blue"),
          legend.text = element_text(size=12, color = "blue"),
          legend.background = element_rect(colour = "blue"),
          panel.grid.minor = element_blank()) +
    guides(color=guide_legend("Key Dates", nrow=5, byrow=FALSE)) +
    scale_color_manual(values=c("blue", "blue", "blue", "blue", "blue")) +
    scale_x_datetime(breaks=date_breaks("months"),
                     labels=date_format("%B"),
                     limits=as_datetime(c("2020-11-01", "2021-08-31"))) +
    labs(title=paste("Sentiment of NYTimes articles about H.R.1"), 
         subtitle=paste(" Total of",nrow(sntmnt_in_articles), "articles in", 
                        paste(type, collapse=", "), "\n", 
                        "Sentiment measured using the", sent_dict, "for each article (green=positive, red=negative)\n",
                        "Point size based on the proportion of \"", target_word,
                        "\" in each article"))
}

sntmts_over_time(c("News", "Editorial", "Letter", "News"), "afinn_score", "filibuster")
wdfrq_over_time(c("News"),"justice")
type <- c("Op-Ed", "Editorial", "Letter", "News")
