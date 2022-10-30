q("no")
R

library(slam)
library(fastTextR)

wd <- "/home/f/work/Text_Mining/packages/fastText_text"
setwd(wd)

dir(all.files = TRUE)

cntrl <- ft.control(save_output = TRUE)

model <- fasttext("dbpedia.train", "supervised", output = "ft_model", control = cntrl)
model

model <- ft.load_model("dbpedia_model.bin.bin")
model

fastTextR:::Rft_fasttest_version()









predict_supervised_model <- function(object, newdata = character(), k = 1L, threshold = 0, 
                                     rval = c("sparse", "dense", "slam")) {
    stopifnot( inherits(object, "fasttext") )
    rval <- match.arg(rval)
    pred <- fastTextR:::Rft_predict_vec(object$pointer, newdata, as.integer(k), threshold = threshold)

    if (rval == "sparse") return(data.frame(pred, stringsAsFactors = FALSE))

    labels <- unique(pred$label)
    labels <- labels[order(as.integer(gsub("\\D", "", labels)))]
    j <- match(pred$label, labels)
    probs <- simple_triplet_matrix(i = pred$id, j = j, v = pred$prob) 
    colnames(probs) <- labels
    if (rval == "dense") as.data.frame(as.matrix(probs), stringsAsFactors = FALSE) else probs
}



ls(getNamespace("fastTextR"))

testdata <- readLines("dbpedia.test", n = 100L)
test <- gsub("__label__\\d+\\s*,", "", testdata)
test <- normalize(test)
labels <- gsub("\\D", "", gsub(",.*", "", testdata))

test.pred <- predict_supervised_model(model, test, k = 1L)
head(test.pred)
dim(test.pred)

test.pred <- predict_supervised_model(model, test, k = 1L, 0.99)
dim(test.pred)


test.pred <- predict_supervised_model(model, test, k = 3L)
head(test.pred)

test.pred <- predict_supervised_model(model, test, k = 3L, rval="slam")
head(as.matrix(test.pred))

test.pred <- predict_supervised_model(model, test, k = 3L, rval="dense")
head(test.pred)



head(as.matrix(M))


confusion_matrix <- table(labels, gsub("\\D", "", test.pred$label))
confusion_matrix

sum(diag(confusion_matrix)) / sum(confusion_matrix)



predict.supervised_model <- function(object, newdata = character(), newdata_file = "", 
                                     result_file = "", k = 1L, prob = FALSE, ...) {
    stopifnot( inherits(object, "fasttext") )
    if ( missing(newdata) ) {
        stopifnot(is.character(newdata_file), is.character(result_file), 
                  file.exists(newdata))
        if ( missing(result_file) ) {
            pred <- Rft_predict(object$pointer, newdata_file, as.integer(k), 
                                as.logical(prob))
        } else {
            pred <- Rft_predict_to_file(object$pointer, newdata_file, result_file, 
                                        as.integer(k), as.logical(prob))
            return( NULL )
        }
    } else {
        pred <- Rft_vec_predict(object$pointer, newdata, as.integer(k), as.logical(prob))
    }
    if ( !prob ) {
        if ( k > 1 ) {
            pred <- matrix(pred[[1]], ncol=k, byrow=TRUE)
            colnames(pred) <- sprintf("best_%s", seq_len(k))
            return( pred )
        }
        return( pred[[1]] )
    }
    if ( k > 1 ) {
        pred[[1]] <- matrix(pred[[1]], ncol=k, byrow=TRUE)
        pred[[2]] <- matrix(pred[[2]], ncol=k, byrow=TRUE)
        cn <- sprintf("best_%s", seq_len(k))
        colnames(pred[[1]]) <- cn
        colnames(pred[[2]]) <- cn
    }
    return( pred )
}