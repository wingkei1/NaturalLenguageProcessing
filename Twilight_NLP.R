library(utf8)

twilight <- "https://archive.org/stream/StephenieMeyer1.Twilight/Stephenie%20Meyer%201.%20Twilight_djvu.txt"
lines <- readLines(twilight, encoding = "UTF-8", warn=FALSE)

# 1675 y 1721
inicio <- grep(pattern = "1. FIRST SIGHT", lines, fixed = TRUE)
fin <- grep(pattern = "Acknowledgments", lines, fixed = TRUE)
book <- lines[inicio[2]:fin[1]]

# Basic checks
book[!utf8_valid(book)]
book_NFC <- utf8_normalize(book)
sum(book_NFC != book)

# Basic structuration
chapterOneLine <- paste(lines[1675:1721], collapse = "\n")
chapters <- unlist(strsplit(chapterOneLine, "\\n\\n")) 
for (cap in chapters) {
  book <- gsub(cap, paste("Chapter", cap, sep=" "), book)
}

stringQ <- paste(book, collapse = "\n")
paragraphs <- unlist(strsplit(stringQ, "Chapter")) #separamos el texto por capitulos
paragraphs <- paragraphs[-which(paragraphs == "")] #limpiamos los parrafos vacios
substring(paragraphs[1:24], 1, 70)

# Some cleaning
paragraphswoNL <- gsub("[\n]{1,}", " ", paragraphs) #quitamos \n
paragraphs <- gsub("[ ]{2,}", " ", paragraphswoNL) #quitamos multiple espaciado
substring(paragraphs[1:24], 1, 70)

# Some numbers (chars, words, sentences)
library(spacyr)
spacy_initialize() 
phrases <- spacy_tokenize(paragraphs, what="sentence")
v_phrases <- unlist(phrases)
numphrases <- length(v_phrases)
sum(v_phrases=="")
hist(nchar(v_phrases), main = "Histogram of sentence size", xlab = "Sentece size (number of characters)", ylab = "Ocurrences")

tokens <- spacy_tokenize(paragraphs,remove_punct = TRUE,remove_separators = TRUE)
v_tokens <- unlist(tokens)
v_tokens[1:10]
length(v_tokens) 
length(unique(v_tokens))
head(sort(table(v_tokens), decreasing = TRUE), n = 25)
plot(head(sort(table(v_tokens), decreasing = TRUE), n = 10), xlab = "Token", ylab = "Ocurrences")

# Sentences analysis
library("dplyr")
tic <- Sys.time()
phrasesLength <- length(v_phrases)
res <- lapply(v_phrases[2:phrasesLength], # esta ejecucion tardara un rato
              spacy_parse, #This is the function to apply to every element in v_phrases
              dependency = TRUE, nounphrase = TRUE #These are the arguments of the function
)
df <- res[[1]] #A data frame with the first resuls
for (i in 2:length(res)){
  df <- bind_rows(df, res[[i]])
}
Sys.time()-tic
saveRDS(df, file="spacy_parse_Twilight.rds")
library(kableExtra)
kable_styling(kable(df[1:20, c(3:ncol(df))]), font_size = 12)
spacy_finalize()
sessionInfo()

#Filter verbs and adjectives
verbDf <- df[df $pos%in% c("VERB"),]
kable_styling(kable(verbDf[1:200, c(3:ncol(verbDf))]), font_size = 12)
verbG <- aggregate(verbDf$token, by=list(verbDf$token), FUN=length)
verbG <- verbG[order(verbG$x,decreasing=T),]
verbG

adjDf <- df[df $pos%in% c("ADJ"),]
kable_styling(kable(adjDf[1:200, c(3:ncol(adjDf))]), font_size = 12)
adjG <- aggregate(adjDf$token, by=list(adjDf$token), FUN=length)
adjG <- adjG[order(adjG$x,decreasing=T),]
adjG


#Subword tokenization
library(tokenizers.bpe)
model <- bpe(v_phrases[2:800]) # modelo BPE con datos para el entrenamiento del modelo
text_part2 <- paste(v_phrases[601:1000], collapse="\n") # datos para las pruebas
subtoks2 <- bpe_encode(model, x = text_part2, type = "subwords")

niceSubwords <- function(strings){ gsub("\U2581", "_", strings) }
niceSubwords(head(unlist(subtoks3), n=30))

text_part3 <- paste(gsub("\"|,|\n|[c(]|[)]", "", verbG['Group.1']), collapse="\n")
subtoks3 <- bpe_encode(model, x = text_part3, type = "subwords")
niceSubwords(unlist(subtoks3)) # verbos normalizados

text_part4 <- paste(gsub("\"|,|\n|[c(]|[)]", "", adjG['Group.1']), collapse="\n")
subtoks4 <- bpe_encode(model, x = text_part4, type = "subwords")
niceSubwords(unlist(subtoks4)) # adjetivos normalizados

#  Distance between texts
library(quanteda)
texts_caps <- paragraphs
names(texts_caps) <- paste("Chap.", 1:length(texts_caps)) #assigns a name to each string
corpus_capsQ <- corpus(texts_caps)
docvars(corpus_capsQ, field="Chapter") <- 1:length(texts_caps) #docvar with chapter number
corpus_capsQ
dfm_capsQ <- dfm(tokens(corpus_capsQ))

distMatrix <-dist(as.matrix(dfm_capsQ), method="euclidean") # distancia entre capitulos
groups <-hclust(distMatrix , method="ward.D")
plot(groups, cex =0.8, hang= -1, xlab = "", ylab = "", main = "") # dendograma
rect.hclust(groups, k=10)

# The most and less frequent features
dfm_capsQ_1 <- dfm(tokens(corpus_capsQ,remove_punct = TRUE))
dfm_capsQ_2 <- dfm_remove(dfm_capsQ_1, stopwords("english")) # eliminamos las palabras menos relevantes
topfeatures(dfm_capsQ_2) # carateristicas mas frecuentes
topfeatures(dfm_capsQ_2,decreasing = FALSE) # caracteristicas menos frecuentes 
