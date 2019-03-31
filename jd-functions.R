# Compute default R color scheme  
#
#' This function returns the default R color palette consisting
#' of n unique values.
#' @param n Number of default colors (integer)
#' @return A vector of n characters representing colors
getColors <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Preprocesses a corpus to output a document-term matrix 
#
#' This function applies the necessary transformations to
#' each document in the corpus and then removes certain terms
#' based on sparsity.
#' @param text Corpus of interest (character vector)
#' @return DocumentTermMatrix 
preprocessCorpus <- function(text){
  library(tm)
  # Load text as corpus
  # Create the toSpace content transformer
  toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ", x))})
  # Perform necessary operations to corpus
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, toSpace, "/")
  corpus <- tm_map(corpus, toSpace, "-")
  corpus <- tm_map(corpus, content_transformer(tolower)) # Convert to lowercase
  corpus <- tm_map(corpus, removePunctuation) # Remove punctuation
  corpus <- tm_map(corpus, stripWhitespace) # Strip whitespace
  corpus <- tm_map(corpus, removeWords, stopwords('english')) # Remove stopwords
  corpus <- tm_map(corpus, removeNumbers) # Remove numbers
  corpus <- tm_map(corpus, stemDocument,"english") # Stem document
  dtm <- DocumentTermMatrix(corpus) # Create document term matrix
  # Removes terms with sparsity greater than or equal to 99%
  dtm <- removeSparseTerms(dtm, 0.99)
}


# Plot years of experience extracted from corpus
#
#' This function plots a histogram representing the number of years suggested 
#' for a particular position. 
#' @param text Corpus of interest (character vector)
#' @param subtitle Subtitle string (character vector)
#' @return Histogram of years of experience
plotExperience <- function(text,subtitle){
  library(stringr)
  # Function to extract numbers from text
  numextract <- function(string){ 
    str_extract(string, "\\-*\\d+\\.*\\d*")
  } 
  years = numextract(text) # Extract numbers
  years <- as.numeric(years[!is.na(years)]) # Remove NAs
  # Remove numeric values greater than 10 (since > 10 years of experience is highly unlikely) and
  # non-integer values (since resumes usually list singular integer or integer ranges for suggested experience)
  years <- years[(years < 10) & (years %% 1 == 0) & (years >= 0)]
  colors = getColors(1)
  # Plot histogram using ggplot
  # library(ggplot2)
  # library(ggthemes)
  # ggplot(data.frame(years),aes(years))+geom_histogram(binwidth=1,fill=colors[1],color="white")+
  #   ggtitle("Years of Experience Suggested")+
  #   scale_x_continuous(name = "Years of Experience",
  #                      breaks = min(years):max(years),
  #                      labels = as.character(min(years):max(years)))+
  #   scale_y_continuous(name = "Frequency") +
  #   theme_bw()+
  #   theme(axis.line = element_line(size=1, colour = "black"),
  #         panel.grid.major = element_line(colour = "#d3d3d3"),
  #         panel.grid.minor = element_blank(),
  #         panel.border = element_blank(), panel.background = element_blank(),
  #         plot.title = element_text(family="Roboto", size = 18, face = "bold", hjust=0.5),
  #         axis.text.x=element_text(colour="black", size = 12),
  #         axis.text.y=element_text(colour="black", size = 12),
  #         text=element_text(family="Roboto", size = 14))
  library(ggpubr)
  p <- gghistogram(data.frame(years), x = "years",
              fill = colors[1], color = colors[1],  
              alpha=0.75, binwidth = 1, size=1.5)
    ggpar(p, xlab = "Years of Experience", ylab = "Frequency",
          title = "Years of Experience Suggested by Job Listings", 
          subtitle = subtitle, 
          font.x = c(14,"bold"), font.y = c(14,"bold"),
          font.title = c(18,"bold"), font.subtitle = 16, font.family = "Roboto",
          font.tickslab = 12, xticks.by = 1,
          orientation = c("vertical", "horizontal", "reverse"),
          ggtheme = theme_pubr())+ font("title",hjust=0.5) + font("subtitle",hjust=0.5)
}

# Plots a word cloud 
#
#' This function plots a word cloud containing the most frequently 
#' found terms in a document-term matrix. 
#' @param dtm Document-term matrix (list)
#' @return Word cloud
plotWordcloud <- function(dtm){
  m <- as.matrix(t(dtm))
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  # Wordcloud of 200 most frequently used words
  library(wordcloud)
  set.seed(123)
  wordcloud(words = d$word, freq = d$freq, min.freq = 8, scale=c(4,0.5),
            max.words=200, random.order=FALSE, rot.per=0.25, 
            colors=brewer.pal(8, "Dark2"), family= "Roboto")
}

# Plots a dendrogram
#
#' This function plots a dendrogram depicting the most frequently used 
#' terms in a document-term matrix and groups terms by a distance metric
#' derived from term frequencies. 
#' @param dtm Document-term matrix (list)
#' @return Dendrogram colored by hierarchical clusters
plotDendrogram <- function(dtm){
  library(tm)
  # Hierarchical clustering using dendrogram 
  # Since most words in this document-term matrix are 
  # at least 90% sparse, that is the percentage we use for clustering
  dtms <- removeSparseTerms(dtm, sparse=0.9) #remove superfluous words
  m <- as.matrix(dtms) #create a matrix of word frequencies
  # Perform hierarchical clustering
  library(cluster)
  distance <- dist(scale(t(m)))
  # use ward.D for hierarchial clustering
  hc <- hclust(distance, method = "ward.D")
  # Produce coloured dendrogram
  library(dendextend) 
  dend <- hc
  #represent the different clusters with different colours
  dend %>% as.dendrogram %>%
    set("labels_col", k = 7) %>%
    set("branches_k_color", k = 7) %>%
    set("branches_lwd", 3) %>% 
    set("labels_cex", 1.5) %>% 
    plot(main = 'Cluster Dendrogram', ylab = 'Height', 
         family = "Roboto", cex.main = 1.5, cex.lab = 1.3)
}


# Plot importance metrics from random forest classifier
#
#' This function plots a bar graph depicting the mean decrease in 
#' accuracy and the mean decrease in Gini impurity. 
#' @param varImp Feature importance metrics for terms (data frame)
#' @return Bar graph of feature importances 
plotImportance <- function(varImp){
  colors = getColors(2)
  library(reshape2)
  varImp <- melt(varImp[,3:4])
  library(dplyr)
  library(ggpubr)
  varImp <- varImp %>%
    group_by(Var2) %>%
    arrange(Var2,desc(value)) %>%
    top_n(20,value) %>%
    ungroup %>%
    mutate(Group=factor(rep(c(1:2),20))) 
    # mutate(Var1 = factor(paste(Var1, Var2, sep = "__"), levels = rev(paste(Var1, Var2, sep = "__")))) %>%
    # ggplot(aes(Var1,value,fill=Group,color=Group))+
    # geom_col(show.legend = FALSE) +
    # facet_wrap(~Var2, scales = "free") +
    # scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
    # theme_bw()+
    # theme(strip.text=element_text(hjust=0, family="Roboto", size=16, face = "bold"),
    #       plot.title = element_text(family="Roboto", size = 18, face = "bold", hjust=0.5),
    #       plot.subtitle = element_text(family="Roboto", size = 16, hjust=0.5),
    #       axis.text.x=element_text(colour="black", size = 11),
    #       axis.text.y=element_text(colour=colors, size = 13, face = "bold"),
    #       text=element_text(family="Roboto", size = 14)) +
    # coord_flip()+
    # labs(x = NULL, y = NULL, title = paste("Which terms are most important to random forest classification?"),
    #      subtitle = "Based on sample \"responsibilities\" and \"qualifications\" corpora.")
  p1 <- ggdotchart(varImp[1:20,], x = "Var1", y = "value", 
                   shape = 18, dot.size = 8, color = colors[1],
                   add = "segments", add.params = list(size=2),
                   rotate = TRUE, sorting = "descending")
  pp1 <- ggpar(p1, title = "Mean Decrease in Accuracy",
        legend = "none", font.x = c(14,"bold"), font.y = c(14,"bold"),
        font.title = 16, font.family = "Roboto",
        font.xtickslab = c(12), font.ytickslab = c(14),
        ggtheme = theme_bw())+ font("title",hjust=0.5) + rremove("xylab")
  p2 <- ggdotchart(varImp[21:40,], x = "Var1", y = "value",
                    shape = 18, dot.size = 8, color = colors[2], 
                    add = "segments", add.params = list(size=2),
                    rotate = TRUE, sorting = "descending")
  pp2 <- ggpar(p2, title = "Mean Decrease in Gini Impurity",
          legend = "none", font.x = c(14,"bold"), font.y = c(14,"bold"),
          font.title = 16, font.family = "Roboto",
          font.xtickslab = c(12), font.ytickslab = c(14),
          ggtheme = theme_bw())+ font("title",hjust=0.5)  + rremove("xylab")
  figure <- ggarrange(pp1, pp2, ncol = 2, labels = c("(A)","(B)"), label.x = 0.1)
  annotate_figure(figure, top = text_grob("Which terms are most important for random forest classification?", hjust = 0.5,
                                         family = "Roboto", face = "bold", size = 18),
                          bottom = text_grob("*Based on sample \"responsibilities\" and \"qualifications\" corpora.", 
                                           hjust = 0.5, family = "Roboto", face = "bold"))
}


# Scrapes target web page for information
#
#' This function scrapes job summary data from Indeed,  
#' producing a data frame containing this information
#' @return Job summary data (data frame)
scrapeSummary <- function(){
  library(rvest)
  # Load HTML page (indeed.com)
  html_page <- read_html("https://www.indeed.com/jobs?q=data+scientist&l=")
  # Find number of total jobs found in search
  total_jobs <- html_page %>%
    html_node('#searchCount') %>%
    html_text()
  total_jobs <- max(as.numeric(unlist(str_extract_all(gsub(",","",total_jobs), "\\-*\\d+\\.*\\d*"))))
  # Crawl through search results from Indeed website to obtain job summaries
  library(stringi)
  no_of_pages = 500 #ceiling( total_jobs / 10 )
  df <- data.frame(stringsAsFactors = FALSE)
  for( i in 1:no_of_pages){
    page = read_html(paste("https://www.indeed.com/jobs?q=data+scientist&start=",(10 * i-1)+1,sep=""))
    # get job summary from each listing
    job_summary <- page %>%
      html_nodes("span") %>%
      html_nodes(xpath = '//*[@class="summary"]') %>%
      html_text() %>%
      stri_trim_both()
    tmp <- data.frame(job_summary,stringsAsFactors = FALSE)
    df <- rbind(df,tmp)
  }
  df = unique(df)
  colnames(df) = "Description"
}

# Plots two terms along with top 10 similar terms side-by-side in facet grid
#
#' This function plots a comparative figure illustrating relevant terms
#' sorted by descending similarity computed using the cosine similarity. 
#' @param word1 First word of interest (character vector)
#' @param word2 Second word of interest (character vector)
#' @param wv Matrix of word vectors (matrix)
plotSimilarTerms <- function(word1, word2, wv){
  colors = getColors(2)
  # Finds most similar terms given a target vector among all word vectors
  findSimilarWords <- function(word,word_vectors){
    library(text2vec)
    target = word_vectors[,word,drop=FALSE]
    cos_sim = sim2(t(word_vectors),t(target),method='cosine',norm='l2')
    similar = head(sort(cos_sim[,1], decreasing = TRUE), 11)
    similar = similar[-1]
  }
  library(dplyr)
  library(tibble)
  query1 = data.frame(Similarity = findSimilarWords(word1,wv))
  query2 = data.frame(Similarity = findSimilarWords(word2,wv))
  query <- query1 %>%
    rownames_to_column("Term") %>%
    mutate(Selected = word1) %>%
    bind_rows(query2 %>% rownames_to_column("Term") %>% 
                mutate(Selected = word2)) %>%
    #mutate(Term = reorder(Term,Similarity)) %>%
    group_by(Selected) %>%
    arrange(Selected,desc(Similarity)) %>%
    ungroup() #%>%
  head(query,10)
      # mutate(Term = factor(paste(Term, Selected, sep = "__"), levels = rev(paste(Term, Selected, sep = "__")))) %>%
    # ggplot(aes(Term, Similarity, fill = Selected)) +
    # geom_col(show.legend = FALSE) +
    # facet_wrap(~Selected, scales = "free") +
    # scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
    # theme_bw()+
    # theme(strip.text=element_text(hjust=0, family="Roboto", size=16, face = "bold"),
    #       plot.title = element_text(family="Roboto", size = 18, face = "bold", hjust=0.5),
    #       plot.subtitle = element_text(family="Roboto", size = 16, hjust=0.5),
    #       axis.text.x=element_text(colour="black", size = 11),
    #       axis.text.y=element_text(colour="black", size = 13),
    #       text=element_text(family="Roboto", size = 14)) +
    # coord_flip() +
    # labs(x = NULL, y = NULL, title = paste("Which word vectors are most (cosine) similar to \"", word1, "\" or \"", word2,"\"?", sep = ""),
    #      subtitle = "Based on the Indeed job summary corpus, calculated from GloVe representations.")
  p1 <- ggdotchart(query[1:10,], x = "Term", y = "Similarity", 
                   shape = 18, dot.size = 8, color = colors[1],
                   add = "segments", add.params = list(size=2),
                   rotate = TRUE, sorting = "descending")
  pp1 <- ggpar(p1, title = paste("\"",word1,"\"",sep = ""),
               legend = "none", font.x = c(14,"bold"), font.y = c(14,"bold"),
               font.title = 16, font.family = "Roboto",
               font.xtickslab = c(12), font.ytickslab = c(14),
               ggtheme = theme_bw())+ font("title",hjust=0.5) + rremove("xylab")
  p2 <- ggdotchart(query[11:20,], x = "Term", y = "Similarity",
                   shape = 18, dot.size = 8, color = colors[2], 
                   add = "segments", add.params = list(size=2),
                   rotate = TRUE, sorting = "descending")
  pp2 <- ggpar(p2, title = paste("\"",word2,"\"",sep = ""),
               legend = "none", font.x = c(14,"bold"), font.y = c(14,"bold"),
               font.title = 16, font.family = "Roboto",
               font.xtickslab = c(12), font.ytickslab = c(14),
               ggtheme = theme_bw())+ font("title",hjust=0.5)  + rremove("xylab")
  figure <- ggarrange(pp1, pp2, ncol = 2, labels = c("(A)","(B)"), label.x = 0.1)
  annotate_figure(figure, top = text_grob(paste("Which word vectors are most (cosine) similar to \"", word1, "\" or \"", word2,"\"?", sep = ""), hjust = 0.5,
                                          family = "Roboto", face = "bold", size = 18),
                  bottom = text_grob("*Based on the Indeed job summary corpus, calculated from GloVe representations.", 
                                     hjust = 0.5, family = "Roboto", face = "bold"))
}

# Plots Glove word vectors projected onto 2D using MDS
#
#' This function plots the 2D projections of Glove embeddings obtained
#' through multidimensional scaling, labeled with terms and colored by term frequency. 
#' @param mdsout Projected word vectors in 2D (data frame)
#' @param words Terms in corpus of interest (character vector)
#' @param LogFrequency Logarithm of term frequency for each term (numeric vector)
#' @param metric Distance/dissimilairy metric used to compare word vectors (character vector)
plotGloveMDS <- function(mdsout,words,LogFrequency,metric){
  colors = getColors(2)
  library(ggplot2)
  ggplot(mdsout, aes(x = X1, y = X2,size=14)) + 
    geom_label(aes(label = words, fill = LogFrequency, fontface = "bold"))+
    theme_bw()+
    theme(axis.line = element_line(size=1, colour = "black"),
          panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank(),
          plot.title = element_text(family="Roboto", size = 18, face = "bold", hjust=0.5),
          plot.caption = element_text(family="Roboto", size = 12, face ="bold", hjust=0.5),
          axis.text.x=element_text(colour="black", size = 12),
          axis.text.y=element_text(colour="black", size = 12),
          text=element_text(family="Roboto", size = 14))+
    guides(size=FALSE)+
    scale_fill_gradient(low=colors[2],high=colors[1])+
    labs(title=paste("MDS of Word Vectors (", metric, ")",sep=""),
         caption="*Based on the Indeed job summary corpus, calculated from GloVe representations.",
         fill="Frequency")
}