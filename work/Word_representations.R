library("fastTextR")

setwd("~/data")

ft <- fasttext()
ft$train('fil9', method = "cbow", control = ft_control(), nthreads=3L, verbose=2L)
write.fasttext(model, "fil9.bin", what = "model")



q("no")
R

library("fastTextR")

ft <- fasttext()
ft$load("cc.en.300.bin")
words <- "enviroment"
ft$word_vectors("enviroment")
words <- c("asparagus", "pidgey", "yellow")
ft$word_vectors(c("asparagus", "pidgey", "yellow"))

ft$nearest_neighbors('asparagus', k = 5L)


ft$analogies(c("berlin", "germany", "france"))


model <- ft_load("cc.en.300.bin")
model




attach(getNamespace("fastTextR"))

all_words <- Rft_all_words(model$pointer)
length(all_words)
head(all_words)


ft_word_vectors(model, "enviroment")
ft_word_vectors(model, c("asparagus", "pidgey", "yellow"))

ft_nearest_neighbors(model, 'asparagus', k = 5L)

ft_analogies(model, c("berlin", "germany", "france"))

