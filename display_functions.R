
#There are four article "Types": Op-Ed, Letter, Editorial, News.  
#I've built my display functions so that it can select a subset of the articles based on an input 
#that is a character string - e.g. c("Op-Ed", "Editorial").


#NOTE for by_type: which is either "wdfrq" or "sntmts".  type is a character string including 
#the four article types (Op-Ed, Letter, Editorial, News).
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

sntmts_over_time <- function(type, target_word) {
  df <- by_type("sntmts", type)
  the_word <- filter(df, word == target_word)
  #Since the target_word may not be in a given article, need to finesse the code
  #so that word="target_word" and n=0 shows up in word_in_articles, so that I
  #get instances of 0 of that word in an article.  Therefore I've used "people"
  #to get all of the articles and used the no_word data.frame to build in
  #"target_word, n=0, proportion=0" into the word_in_articles data.frame.
  no_word <- filter(df, word == 'people') %>%
    mutate(word = target_word,
           n = 0,
           proportion = 0) %>%
    anti_join(the_word, by = "Article")
  word_in_articles <- rbind(the_word, no_word) %>%
    arrange(Article)
  ggplot(word_in_articles, aes(x = Date, y = proportion)) +
    geom_point() +
    geom_smooth(se=FALSE) +
    geom_vline(xintercept=as_datetime("2021-01-06")) +
    geom_vline(xintercept=as_datetime("2021-04-08")) +
    geom_vline(xintercept=as_datetime("2021-06-16")) +
    geom_vline(xintercept=as_datetime("2021-06-22")) +
    geom_vline(xintercept=as_datetime("2021-07-01")) +
    geom_vline(xintercept=as_datetime("2021-07-12"))
  return(word_in_articles)
}

wdfrq_over_time <- function(type, target_word) {
  df <- by_type("wdfrq", type)
  the_word <- filter(df, word == target_word)
  #Since the target_word may not be in a given article, need to finesse the code
  #so that word="target_word" and n=0 shows up in word_in_articles, so that I
  #get instances of 0 of that word in an article.  Therefore I've used "people"
  #to get all of the articles and used the no_word data.frame to build in
  #"target_word, n=0, proportion=0" into the word_in_articles data.frame.
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
                          Key_Dates_labels = c("a", "b", "c", "d", "e"),
                          Key_Dates = c("(a) Manchin publishes op-ed saying he will not alter filibuster",
                            "(b) Manchin offers amendments and expresses willingness to alter filibuster",
                            "(c) Senate blocks debate on bill",
                            "(d) Supreme Court hands down voting right decision",
                            "(e) Texas Democrats flee state"),
                          stringsAsFactors = FALSE)
  
  ggplot(word_in_articles, aes(x = Date, y = proportion)) +
    geom_point() +
    geom_smooth(color="red", se=FALSE) +
    geom_vline(aes(xintercept = xintercept, color = Key_Dates), line.data, lty=2) +
    annotate("text", line.data$xintercept, max(word_in_articles$proportion), 
             hjust=-.5, vjust = 0, color="blue", label = line.data$Key_Dates_labels) +
    theme_bw() +
    theme(text = element_text(family = "Times New Roman"),
          axis.text.x=element_text(angle=45, hjust=1, vjust=1),
          plot.title = element_text(face="bold", size=20, 
                                    hjust=.10, vjust=-18),
          plot.subtitle = element_text(size=14, 
                                       hjust=.10, vjust=-28),
          legend.position = "top",
          legend.justification = "right",
          legend.title = element_text(size=16, color = "blue"),
          legend.text = element_text(size=12, color = "blue"),
          legend.background = element_rect(colour = "blue"),
          panel.grid.minor = element_blank()) +
    guides(color=guide_legend("Key Dates", nrow=5, byrow=FALSE)) +
    scale_fill_discrete(name="Experimental\nCondition") +
    scale_color_manual(values=c("blue", "blue", "blue", "blue", "blue")) +
    scale_x_datetime(breaks=date_breaks("months"),
                     labels=date_format("%B"),
                     limits=as_datetime(c("2020-10-01", "2021-08-31"))) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.01),
                       limits = c(0, (max(word_in_articles$proportion))+.0025)) +
    labs(title=paste("Frequency that \"", target_word, "\" is mentioned"), 
         subtitle=paste("Total of 30 articles mention \"", target_word, "\""),
         x ="", y="proportion within each article")
  }
wdfrq_over_time(c("Op-Ed", "Editorial", "Letter"),"justice")

type <- c("Op-Ed", "Editorial", "Letter")
target_word <- "justice"

