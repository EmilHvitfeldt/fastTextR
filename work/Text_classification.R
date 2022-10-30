library("fastTextR")

setwd("~/data")

## Train
cntrl <- ft_control(verbose = 1L, learning_rate=1, loss="ns", epoch=25, max_len_ngram=2L)
model <- ft_train("cooking.train", "supervised", control = cntrl)

# or 
ft <- fasttext()
ft$train("cooking.train", "supervised", control = cntrl)
ft

## Predict
# __label__baking __label__equipment __label__bread __label__bananas
ft$predict("Which baking dish is best to bake a banana bread ?", k = 3)

txt <- c("Which baking dish is best to bake a banana bread ?", "Why not put knives in the dishwasher?")
ft$predict(txt, k = 3)

# __label__equipment __label__cleaning __label__knives
ft$predict("Why not put knives in the dishwasher?", k = 3)


## Validate
ft$test("cooking.valid", k = 2L, 0.0)
