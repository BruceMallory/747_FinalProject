
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
                          Key_Dates = c("a) Manchin publishes op-ed saying he will not alter filibuster",
                            "b) Manchin offers amendments and expresses willingness to alter filibuster",
                            "c) Senate blocks debate on bill",
                            "d) Supreme Court hands down voting right decision",
                            "e) Texas Democrats flee state"),
                          stringsAsFactors = FALSE)
  
  ggplot(word_in_articles, aes(x = Date, y = proportion)) +
    geom_point() +
    geom_smooth(color="red", se=FALSE) +
    geom_vline(aes(xintercept = xintercept, color = Key_Dates), line.data, lty=3) +
    annotate("text", line.data$xintercept, min(word_in_articles$proportion), hjust=-.25, vjust = 3, 
             color="blue", label = line.data$Key_Dates_labels) +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
          legend.position = "bottom",
          legend.text = element_text(color = "blue")) +
    guides(color=guide_legend(nrow=5,byrow=FALSE)) +
    scale_color_manual(values=c("blue", "blue", "blue", "blue", "blue")) +
    scale_x_datetime(breaks=date_breaks("months"), 
                     labels=date_format("%B"),
                     limits=as_datetime(c("2020-10-01", "2021-08-31"))) +
    xlab("")
}


ggplot(word_in_articles, aes(x = Date, y = proportion)) + 
  geom_point() + 
  geom_vline(aes(xintercept = xintercept, color = Lines), line.data, lty=3) +
  scale_colour_manual(values = line.data$color)

wdfrq_over_time(c("Op-Ed", "Editorial", "Letter"),"manchin")
type <- c("Op-Ed", "Editorial", "Letter")
target_word <- "manchin"

c("Manchin publishes op-ed  saying he will not alter filibuster",
  "Manchin offers amendments and expresses willingness to alter filibuster",
  "Senate blocks debate on bill",
  "Supreme Court hands down voting right decision",
  "Texas Democrats flee state")

# theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1), axis.title.x=element("")) +
#     scale_x_datetime(limits=as_datetime(c("2020-10-01", "2021-08-31")),
#                      labels=date_format("%B"),
#                      breaks=date_breaks("months"))
