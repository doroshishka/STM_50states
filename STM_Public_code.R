install.packages("stm")
library(stm)
install.packages("tm")
library(tm)

stm_file = read.csv("~your/path.csv", header = TRUE, sep = ",")

#excluding irrelevant columns
allstates_apr_short <- subset(allstates_apr, select = c(media_name, publish_date, story_text, title, media_type, pub_state))

#preprocessing file: stemming, remocing punctuation and stop words
allprocessed_apr <- textProcessor(allstates_apr_short$story_text, metadata = allstates_apr_short)

#removing infrequent terms (those that appear in fewer than 20 documents) and associating remaining text with metadata
plotRemoved(allprocessed_apr$documents, lower.thresh = seq(1, 200, by = 100))
allout_apr <- prepDocuments(allprocessed_apr$documents, allprocessed_apr$vocab,allprocessed_apr$meta, lower.thresh = 20)
docs <- allout_apr$documents
vocab <- allout_apr$vocab
meta <- allout_apr$meta

#model search across number of topics
#using algorithm to determine the number of topics (k set at 0)
storage <- searchK(allout_apr$documents, allout_apr$vocab, K = 0,
                   prevalence =~ pub_state, data = allout_apr$meta, init.type = "Spectral")

#running stm models for 40, 50, and 60 topics to compare parameters
storageK <- searchK(docs, vocab, K = c(40, 50, 60),
                    prevalence =~ pub_state, data = meta)

#stm for 40 topics and publication state as a covariate
allFit_apr <- stm(documents = allout_apr$documents, vocab = allout_apr$vocab,
              K =40, prevalence =~ pub_state, max.em.its =  200,
              data = allout_apr$meta, init.type = "Spectral")

#labeling topics with Highest Prob, FREX, Lift, and Score
labelTopics(allFit_apr, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40))

#estimating covariate/topic relationship
allout_apr$meta$pub_state <- as.factor(allout_apr$meta$pub_state)
prep_apr <- estimateEffect(1:40 ~ pub_state, allFit_apr, meta = allout_apr$meta, uncertainty = "Global")
summary(prep_apr, topics = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40))

