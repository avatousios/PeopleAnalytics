# PeopleAnalytics
Talent Profiling 
Interactive Storytelling with R. 
How the story grows as the size of data changes.. 

library(reshape)
library(tm)
library(wordcloud)
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -- STEP 1 : GET THE DATA
# A dataset with 5485 lines, each line has several words.
optsFile <- choose.files()
# The labels of each line of the dataset file
subsLen <- length(optsFile)
labelsFile <- choose.files()
for(j in 1:subsLen){
  dataset <- read.delim(optsFile[j], header=FALSE)
  dataset_labels <- read.delim(labelsFile[j], header=FALSE)
  dataset_labels <- dataset_labels[,1]
  dataset_labels_p <- paste("",dataset_labels,sep="")
  unique_labels <- unique(dataset_labels_p)
  # merge documents that match certain class into a list object
  dataset_s <- sapply(unique_labels,function(label) c( dataset[dataset_labels_p %in% label,1] ) )
  # -------------------------------------------------------------------------------------------------------------------------   ------------------------------------------------------
  # -- STEP2 : COMPUTE DOCUMENT CORPUS TO MAKE TEXT MINING
  # convert each list content into a corpus
  dataset_corpus <- lapply(dataset_s, function(x) VCorpus(VectorSource( toString(x) ))) 
  # merge all documents into one single corpus
  dataset_corpus_all <- dataset_corpus[[1]]
  for (i in 2:length(unique_labels)) { dataset_corpus_all <- c(dataset_corpus_all,dataset_corpus[[i]]) }
  # remove punctuation, numbers and stopwords
  dataset_corpus_all <- tm_map(dataset_corpus_all, removePunctuation)
  dataset_corpus_all <- tm_map(dataset_corpus_all, removeNumbers)
  dataset_corpus_all <- tm_map(dataset_corpus_all, function(x) removeWords(x,stopwords("english")))
  #remove some unintersting words
  words_to_remove <- c("can", "get", "want", "also", "will", "said", "find", "end", "early", "nope", "good", "quite", "synonym", "match", "long", "way", "best", "right", "big", "long", "one", "kind", "important", "business", "real", "depends", "making", "even", "always", "finally", "may", "going", "part", "just", "make", "well", "far", "within", "ways", "first", "never", "often")
  dataset_corpus_all <- tm_map(dataset_corpus_all, removeWords, words_to_remove)
  # compute term matrix & convert to matrix class --> you get a table summarizing the occurence of each word in each class.
  document_tm <- TermDocumentMatrix(dataset_corpus_all)
  document_tm_mat <- as.matrix(document_tm)
  colnames(document_tm_mat) <- unique_labels
  #document_tm_clean <- removeSparseTerms(document_tm, 0.8)
  #document_tm_clean_mat <- as.matrix(document_tm_clean)
  #colnames(document_tm_clean_mat) <- unique_labels
  # remove words in term matrix with length < 4
  index <- as.logical(sapply(rownames(document_tm_mat), function(x) (nchar(x)>2) ))
  document_tm_clean_mat_s <- document_tm_mat[index,]
  # Have a look to the matrix you are going to use for wordcloud !
  head(document_tm_clean_mat_s)
  # --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # -- STEP 3 : make the graphics !
  # Graph 2 : first top 2000 discriminant words
  fName <- paste("subs", j, sep = "_")
  fName <- paste(fName, "png", sep = ".")
  png(fName, width = 1350, height = 1350, bg ="#f5efe0")
  comparison.cloud(document_tm_clean_mat_s,max.words=2000,random.order=FALSE,c(4,0.5), title.size=1.2, colors=brewer.pal(8,"Dark2"), title.bg.colors = brewer.pal(8, "Dark2"), title.colors = "white")
  dev.off()
  print(j)
}




<p align="center">
  <a href="http://185.4.135.24:3838/peopleanalytics/" target="_blank">
  <img src="https://github.com/avatousios/PeopleAnalytics/blob/master/stud_7.png" width=100% title="People Analytics"></a> 
</p>

