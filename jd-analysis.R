# (1) Load sample dataset and assemble corpus -------------------------------------------
# Load relevant libraries
library(extrafont)
# Import relevant functions from associated functions file
source("jd-functions.R")
# Obtain working color scheme
colors = getColors(2)
# Determine relevant file to be loaded
filePaths <- c("responsibilities.txt","qualifications.txt")
text.resp <- readLines(filePaths[1])
text.qual <- readLines(filePaths[2])
# resp = data.frame(Description=text1)
# qual = data.frame(Description=text2)
subtitle.qual <- "Based on sample \"qualifications\" corpus."
plotExperience(text.qual, subtitle.qual)
# Create document-term matrices for responsibilities, 
# qualifications
dtm.resp <- preprocessCorpus(text.resp)  
dtm.qual <- preprocessCorpus(text.qual)
# Combine respective DTMs into collective matrix
dtm <- c(dtm.resp,dtm.qual)
# Create entire dataset of documents and terms as predictors
dataset <- as.data.frame(as.matrix(dtm))
# Create a label of responsibilities and qualifications
dataset$label = matrix(0,nrow(dataset),1)
# Label all responsibilities as 1, qualifications as 0
dataset$label[1:dtm.resp$nrow] = 1
# Convert output variable to categorical factor
dataset$label = as.factor(dataset$label)

# Construct visualizations based on term frequencies 
# (A) Wordclouds for each corpus
plotWordcloud(dtm.resp)
plotWordcloud(dtm.qual)

# (B) Dendrogram for entire corpora
plotDendrogram(dtm)

# (2) Train a (bag-of-words) random forest classifier on sample dataset ----------------------
# Then use trained classifier to classify all future test "documents"
# for further analysis
# Split overall dataset into training and test sets
library(caTools)
set.seed(123)
# Split ratio is 80-20% (Train-Test)
split = sample.split(dataset$label, SplitRatio = 0.8)
train = subset(dataset, split == TRUE)
test = subset(dataset, split == FALSE)

# Fitting Random Forest Classifier to the Training set
# Retain predictor importances
library(randomForest)
classifier = randomForest(x = train[-ncol(train)],
                          y = train$label,
                          importance = TRUE,
                          ntree = 500)

# Predict classifier performance on test set results
y_pred = predict(classifier, newdata = test[-ncol(train)])

# Construct the Confusion Matrix
library(caret)
cm = confusionMatrix(y_pred, test$label)
cm

# Applying k-Fold Cross Validation to calculate 
# and confirm mean predictive accuracy over k=10 folds
folds = createFolds(train$label, k = 10)
cv = lapply(folds, function(x) {
  train_fold = train[-x, ]
  test_fold = train[x, ]
  classifier = randomForest(x = train_fold[-ncol(train_fold)],
                            y = train_fold$label,
                            ntree = 500)
  y_pred = predict(classifier, newdata = test_fold[-ncol(test_fold)])
  cm = table(test_fold$label, y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
# Mean cross-validation accuracy
mean(as.numeric(cv))
# Standard deviation of cross-validation accuracy
sd(as.numeric(cv))

# Produce variable importance plot
# Identifies important words that affects the accuracy of the model.
varImp = importance(classifier)
plotImportance(varImp)

# Find most frequent terms in responsibilities and qualifications text files
# Most important terms in responsibilities
findMostFreqTerms(dtm.resp, n = 10L, INDEX = rep(1, dtm.resp$nrow))
# Most important terms in qualifications
findMostFreqTerms(dtm.qual, n = 10L, INDEX = rep(1, dtm.qual$nrow))


# (3) Web scraping from Indeed to obtain job summaries------------------------------------------------
# Give option to load pre-existing dataset or scrape new data
have.data = TRUE
if(have.data){
  # Read in prescraped dataset
  df <- read.csv("summaries.csv")
}else{
  # Obtain dataset from web scraping
  df <- scrapeSummary()
  # Write scraped dataframe to .csv file (in case)
  write.csv(df, file = "summaries.csv")
}
# Visualize years of experience gleaned from job summaries
subtitle.summary <- "Based on the Indeed job summary corpus"
plotExperience(unlist(df), subtitle.summary)
# Perform necessary operations to preprocess job summaries
library(tm)
df$Description = tolower(df$Description)
df$Description = removeWords(df$Description, stopwords("english"))
df$Description = removePunctuation(df$Description)
df$Description = removeNumbers(df$Description)


# (4) Word Vectorization of Job Summaries using Global Vector (GloVe) algorithm-------------------------------------
library(text2vec)
# Collect all unique terms from document to form vocabulary
iter = itoken(df$Description, preprocessor = tolower,
            tokenizer = word_tokenizer)
# Look at uni- and bigrams
vocab <- create_vocabulary(iter, ngram = c(1L, 2L))
# Prune vocabulary to retain words whose frequencies greater than 10
vocab <- prune_vocabulary(vocab, term_count_min = 10,
                          doc_proportion_min = 0.001)
# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab)
# Use window size of 10 for context words (captures most document lengths)
tcm <- create_tcm(iter, vectorizer, skip_grams_window = 10L)
set.seed(123)
# Initialize and train GloVe embeddings model
glove <- GlobalVectors$new(word_vectors_size = 100, vocabulary = vocab, x_max = 50)
wv_main <- glove$fit_transform(tcm, n_iter = 100)
wv_context <- glove$components
# Combine both main and context vectors
wv <- t(wv_main) + wv_context

# Compare prominent/interesting terms in corpus
plotSimilarTerms("data_science","machine_learning",wv)
plotSimilarTerms("degree","experience",wv)
plotSimilarTerms("ai","quantitative",wv)
plotSimilarTerms("clients","customers",wv)
plotSimilarTerms("python","r",wv)

# (5) Apply multidimensional scaling (MDS) to visualize vectors in 2 dimensions--------------------------
# (A) Euclidean distance
set.seed(123)
vectordata = dist(scale(t(wv)))
mds.euc <- data.frame(cmdscale(vectordata))
head(mds.euc)
# Label each point with corresponding word and color according to frequency
words <- colnames(wv)
LogFrequency <- log10(vocab$term_count)

plotGloveMDS(mds.euc,words,LogFrequency,"Euclidean Distance")

# (B) Cosine Distance
set.seed(123)
cosdata = 1-sim2(t(wv),t(wv),method='cosine',norm='l2')
mds.cos <- data.frame(cmdscale(cosdata))
head(mds.cos)

plotGloveMDS(mds.cos,words,LogFrequency,"Cosine Distance")


