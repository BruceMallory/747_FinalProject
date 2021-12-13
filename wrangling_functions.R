#---------------------
#Function to load a chunk of articles, tokenize by word, remove stop words, and collect word frequencies.
load_articles <- function(file_name) {
  Files <- read.delim(file_name, header=FALSE)
  Articles <- as.data.frame(Files)
  colnames(Articles) <- c("line")
  #Initializing the data.frame for collecting metrics
  #These variables strings are collecting the lines within each article
  #that will be relevant for my collecting the article words to analyze.
  #I've marked where the article title is, where the body starts and ends and where the date is.
  #Additionally I've marked where the type of article is specified,
  #because as I load articles I want to ignore the opinion articles.
  These_articles <- data.frame(
    Article = integer(),
    Type = character(),
    Date = as_datetime(character()),
    title = character(),
    word = character(),
    n = integer(),
    proportion = integer()
  )
  
  #x is a counter to help me number each of the articles that I'm collecting,
  #it's a local variable.  y is the global variable that is seeded each time this function is called
  x <- y
  Find_Bodys <- str_locate(Articles$line, "Body")
  StartHere <- which(Find_Bodys[, 1] != "NA") + 1
  Find_Classifications <- str_locate(Articles$line, "Classification")
  EndHere <- which(Find_Classifications[, 1] != "NA") - 1
  #Titles are always one line after an "End of Document"
  Find_EndDocs <- str_locate(Articles$line, "End of Document")
  EndDocs <- which(Find_EndDocs[, 1] != "NA") + 1
  Titles <- c(1, EndDocs)
  Titles <- Titles[1:length(Titles) - 1]
  Find_Sections <- str_locate(Articles$line, "Document-Type:")
  Type <- which(Find_Sections[, 1] != "NA")
  Find_Dates <- str_locate(Articles$line, "Load-Date:")
  DateLine <- which(Find_Dates[, 1] != "NA")
  
  for (i in 1:length(Titles)) {
    Date  <-
      str_split_fixed((Articles[DateLine[i], "line"]), ":", n = 2)[, 2]  %>%
      str_trim() %>%
      parse_date_time(orders = "mdy")
    the_type <- 
      str_split_fixed((Articles[Type[i], "line"]), ":", n = 2)[, 2]  %>%
      str_trim()
    the_title <- slice(Articles, Titles[i])
    the_body <- slice(Articles, StartHere[i]:EndHere[i])
    
    This_article <- rbind(the_title, the_body) %>%
      unnest_tokens(word, line) %>%
      anti_join(stop_words, by = "word") %>%
      count(word, sort = TRUE) %>%
      mutate(Article = x, 
             Type = the_type, 
             Date, 
             title = as.character(the_title$line),
             proportion = n / sum(n)) %>%
      select(Article, Type, Date, title, everything())
    These_articles <- rbind(These_articles, This_article)
    x <- x + 1
  }
  return(These_articles)
}

#---------------------
# Function to collect sentiment scores from a single article
measure_sentiment <- function(article_num) {
  matched_words_afinn <-
    filter(Article_wdfrq, Article == article_num) %>%
    inner_join(get_sentiments("afinn"), by="word") %>%
    mutate(sentiment = n * value)
  
  matched_words_bing <-
    filter(Article_wdfrq, Article == article_num) %>%
    inner_join(get_sentiments("bing"), by="word") %>%
    spread(sentiment, n , fill = 0) %>%
    mutate(sentiment = positive - negative) %>%
    filter(word != "trump")
  
  #NOTE: because "trump" is a positive word in the "bing" and "nrc" lexicons, I've removed it from the "bing" and "nrc" sentiment scoring.
  
  matched_words_nrc <-
    filter(Article_wdfrq, Article == article_num) %>%
    inner_join(get_sentiments("nrc"), by="word") %>%
    spread(sentiment, n , fill = 0) %>%
    filter(word != "trump")
  
  Article_titles <-
    Article_wdfrq[!duplicated(Article_wdfrq$Article), ]
  
  new_article <- data.frame(
    Article = article_num,
    Type = Article_titles$Type[article_num],
    Date = Article_titles$Date[article_num],
    prop_matched_affin = nrow(matched_words_afinn) /
      nrow(filter(Article_wdfrq, Article == article_num)),
    prop_matched_bing = nrow(matched_words_bing) /
      nrow(filter(Article_wdfrq, Article == article_num)),
    prop_matched_nrc = nrow(matched_words_nrc) /
      nrow(filter(Article_wdfrq, Article == article_num)),
    
    #NOTE: I've divided the total sentiment scores by the number of words in the 
    #article so that I get a per word sentiment and I can compare articles without 
    #having to worry about the length of the article.
    
    affin_score = sum(matched_words_afinn$sentiment) /
      nrow(filter(Article_wdfrq, Article == article_num)),
    bing_score = sum(matched_words_bing$sentiment) /
      nrow(filter(Article_wdfrq, Article == article_num)),
    nrc_score = (
      sum(matched_words_nrc$positive) - sum(matched_words_nrc$negative)
    ) /
      nrow(filter(Article_wdfrq, Article == article_num)),
    nrc_positive_score = sum(matched_words_nrc$positive) /
      nrow(filter(Article_wdfrq, Article == article_num)),
    nrc_negative_score = sum(matched_words_nrc$negative) /
      nrow(filter(Article_wdfrq, Article == article_num)),
    nrc_fear_score = sum(matched_words_nrc$fear) /
      nrow(filter(Article_wdfrq, Article == article_num)),
    nrc_anger_score = sum(matched_words_nrc$anger) /
      nrow(filter(Article_wdfrq, Article == article_num)),
    nrc_joy_score = sum(matched_words_nrc$joy) /
      nrow(filter(Article_wdfrq, Article == article_num))
  )
  return(new_article)
}

#---------------------
#initializing functions for the data frames I'm creating
initialize_wdfrq_df <- function() {
  df <- data.frame(
    Article = integer(),
    Type = character(),
    Date = as_datetime(character()),
    title = character(),
    word = character(),
    n = integer(),
    proportion = integer()
  )
  return(df)
}

initialize_sntmts_df <- function() {
  df <- data.frame(
    Article = integer(),
    Type = character(),
    Date = as_datetime(character()),
    prop_matched_affin = integer(),
    prop_matched_bing = integer(),
    prop_matched_nrc = integer(),
    affin_score = integer(),
    bing_score = integer(),
    nrc_score = integer(),
    nrc_positive_score = integer(),
    nrc_negative_score = integer(),
    nrc_fear_score = integer(),
    nrc_anger_score = integer()
  )
  return(df)
}