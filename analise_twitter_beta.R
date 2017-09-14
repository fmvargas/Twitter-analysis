require("twitteR")

key <- "wBwpEOiURdCpQRWF5LyrQkTsN"
key_secret <- "V7udSb7damL1vrTyiN0zZWHiM1rCplZpoW23JXesGOGjVmuNP0"
token <- "844970339004289024-dQVKpJzYr53xHnlVUTTIFxiyk6UbPAL"
token_secret <- "LpXqE4FHFAYks4iEuG7YxZRTPwRBTUs0GE7h27oyCfVig"
setup_twitter_oauth(key, key_secret, token, token_secret)

imp <- searchTwitter('', n = 1000, since='2017-06-25', until='2017-06-26', geocode='-30.0,-52.9,20km', locale = "Brasil") #twittes por local em determinado período (até uma semana atrás)

imp_text <- sapply(imp, function(x) x$getText())

df <- do.call("rbind", lapply(imp, as.data.frame))

#usuários ativos
act.users <- as.data.frame(unique(df$screenName))
#act.users$twittes <- table(df$screenName) #não funcionaou como deveria, não está batento o resultado

df$text <- sapply(df$text, function(row) iconv(row, "latin1", "ASCII", sub = ""))
df$text <- sub("(f|ht)tp(s?)://(.*)[.][a-z]+","",df$text) #limpar links

df$text[c(41,43,44)]
sample <- df$text

pos.words <- scan('C:/OneDrive/___dev_R/analise_twitter/palavras_positivas.txt', what='character', sep=",")
neg.words <- scan('C:/OneDrive/___dev_R/analise_twitter/palavras_negativas.txt', what='character', sep=",")

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
        require(plyr)
        require(stringr)
        list=lapply(sentences, function(sentence, pos.words, neg.words)
        {
                sentence = gsub('[[:punct:]]',' ',sentence)
                sentence = gsub('[[:cntrl:]]','',sentence)
                sentence = gsub('\\d+','',sentence)  #removes decimal number
                sentence = gsub('\n','',sentence)    #removes new lines
                sentence = tolower(sentence)
                word.list = str_split(sentence, '\\s+')
                words = unlist(word.list)  #changes a list to character vector
                pos.matches = match(words, pos.words)
                neg.matches = match(words, neg.words)
                pos.matches = !is.na(pos.matches)
                neg.matches = !is.na(neg.matches)
                pp = sum(pos.matches)
                nn = sum(neg.matches)
                score = sum(pos.matches) - sum(neg.matches)
                list1 = c(score, pp, nn)
                return (list1)
        }, pos.words, neg.words)
        score_new = lapply(list, `[[`, 1)
        pp1 = lapply(list, `[[`, 2)
        nn1 = lapply(list, `[[`, 3)
        
        scores.df = data.frame(score = score_new, text=sentences)
        positive.df = data.frame(Positive = pp1, text=sentences)
        negative.df = data.frame(Negative = nn1, text=sentences)
        
        list_df = list(scores.df, positive.df, negative.df)
        return(list_df)
} 

#Cleans the twits and return merged data frame
result = score.sentiment(sample, pos.words, neg.words)

library(reshape2)
#create a copy of result data frame
test1 = result[[1]]
test2 = result[[2]]
test3 = result[[3]]
head(result)

test1$text = NULL
test2$text = NULL
test3$text = NULL

q1 = test1[1,]
q2 = test2[1,]
q3 = test3[1,]

qq1 = melt(q1, , var='Score')
qq2 = melt(q2, , var='Positive')
qq3 = melt(q3, , var='Negative')

qq1['Score'] = NULL
qq2['Positive'] = NULL
qq3['Negative'] = NULL

table1 = data.frame(Text=result[[1]]$text, Score=qq1)
table2 = data.frame(Text=result[[1]]$text, Score=qq2)
table3 = data.frame(Text=result[[1]]$text, Score=qq3)

#merge the tables
table_final = data.frame(Text = table1$Text, Score = table1$value, Positive = table2$value, Negative=table3$value)

#4 Informatio analysis - Graph, Worlcloud, Top Trends, Top hastags

#4.1 POsitive Percentage

        #4.1.1 remanaing
posSc = table_final$Positive
negSc = table_final$Negative

        #adding new column
table_final$PosPercent = posSc/(posSc+negSc)

        #replacing NaN with zero
pp = table_final$PosPercent
pp[is.nan(pp)] <- 0
table_final$PosPercent = pp

#4.2 Negative percentage

        #adding new column
table_final$NegPercent = negSc/(posSc+negSc)

#replacing NaN with zero
nn = table_final$NegPercent
nn[is.nan(nn)] <- 0
table_final$NegPercent = nn

#4.2 Graphs
#4.2.1 Histogram
hist(table_final$Positive, col=rainbow(10))
hist(table_final$Negative, col=rainbow(10))        
hist(table_final$Score, col=rainbow(10))
#4.2.2 Pie
slices <- c(sum(table_final$Positive), sum(table_final$Negative))
labels <- c('POsitive', 'Negative')

if (!require(plotrix)) {
        install.packages("plotrix")
        library(plotrix)
}

pie(slices, labels = labels, col = rainbow(length(labels)), main = "Sentiment Analysis")
#pie3D(slices, labels = labels, col = rainbow(length(labels)), explode = 0,00, main = "Sentiment Analysis")

#4.2.3 Wordcloud
if (!require(tm)) { #tm is a collection of text docuemnts
        install.packages("tm")
        library(tm)
}

imp_corpus <- Corpus(VectorSource(imp_text))
imp_corpus
inspect(imp_corpus[1])

if (!require(wordcloud)) { 
        install.packages("wordcloud")
        library(wordcloud)
}

imp_clean <- tm_map(imp_corpus, removePunctuation)
imp_clean <- tm_map(imp_clean, removeWords, stopwords("portuguese"))
imp_cleann <- tm_map(imp_clean, removeNumbers)
imp_clean <- tm_map(imp_clean, stripWhitespace)
wordcloud(imp_clean, random.order=F,max.words=80, col=rainbow(50), scale=c(3.5,1))

#4.2.4 Top hashtags from an user




