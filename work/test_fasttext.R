library(fastTextR)

wd <- "/home/f/work/Text_Mining/packages/fastText_text"
setwd(wd)

dir()

model <- fasttext("dbpedia_sample.train", "supervised")
model

model <- ft.load_model("dbpedia_model.bin.bin")
model

##save(model, "dbpedia_model.bin")




str(ft.control())
x <- predict(model, "dbpedia.test")
x
trueLabel <- gsub("\\s*,.*", "", readLines("dbpedia.test"))

tab <- table(ground_truth = trueLabel, predicted = x[,1])
tab[1:4, 1:4]
precision(tab)
recall(tab)
accuracy_score(tab)


tmp <- readLines("dbpedia.test")
writeLines(tmp[1:10], "dbpedia_small_with_label.test")

tmp <- gsub("__label__\\d+\\s*,\\s*", "", tmp)
writeLines(tmp, "dbpedia_mod.test")

writeLines(tmp[1:10], "dbpedia_small.test")

x <- predict(model, "dbpedia_mod.test")
tab <- table(ground_truth = trueLabel, predicted = x[,1])
tab[1:4, 1:4]
precision(tab)
recall(tab)
accuracy_score(tab)

x <- predict(model, "dbpedia_small_with_label.test")
x <- predict(model, "dbpedia_small.test")


confusion_matrix <- tab 

## confusion_matrix: ground_truth x predicted
precision <- function(confusion_matrix) {
    diag(confusion_matrix) / colSums(confusion_matrix)
}

recall <- function(confusion_matrix) {
    diag(confusion_matrix) / rowSums(confusion_matrix)
}

accuracy_score <- function(confusion_matrix) {
    sum(diag(tab)) / sum(tab)
}

cbind(precision(tab), recall(tab), accuracy_score(tab))


mean(precision(tab))
mean(recall(tab))

par(mfrow=c(2, 1))


for (i in 1:20) 
    points(i/2, 0.95, col=i)

plot_conf <- function(confusion_matrix) {
    par_save <- par(mar = c(7, 4, 4, 2) + 0.1)
    prec <- precision(confusion_matrix)
    rec <- recall(confusion_matrix)
    prec_se <- sqrt(prec * (1-prec) / colSums(confusion_matrix))
    rec_se <- sqrt(rec * (1-rec) / rowSums(confusion_matrix))
    x_prec <- seq_along(prec) - 0.1
    x_rec <- seq_along(prec) + 0.1
    plot(x = x_prec, y = prec, 
         ylim = c(0.97*min(prec), min(1.03*max(prec), 1)),
         pch = 16, col = 4, xaxt = 'n', xlab = '', ylab = 'Precision')
    arrows(x0=x_prec, y0=prec-1.96*se, x1=x_prec, y1=prec+1.96*se,
           code = 3, length = 0.2, angle = 90, col = 2)
    points(x_rec, rec, pch=16, col=3)
    arrows(x0=x_rec, y0=rec-1.96*se, x1=x_rec, y1=rec+1.96*se,
           code = 3, length = 0.2, angle = 90, col = 5)
    axis(1, at=seq_along(prec), labels=FALSE)
    text(seq_along(prec), par("usr")[3] - 0.0025, srt = 90, adj = 1,
         labels = names(prec), xpd = TRUE)
    par(par_save)
}

plot_conf(tab)
dev.off()

plot_precision <- function(confusion_matrix) {
    par_save <- par(mar = c(7, 4, 4, 2) + 0.1)
    f <- diag(confusion_matrix) / colSums(confusion_matrix)
    se <- sqrt(f * (1-f) / colSums(confusion_matrix))
    plot(f, ylim = c(0.97*min(f), min(1.03*max(f), 1)),
         pch = 16, col = 'blue', xaxt = 'n', xlab = '', ylab = 'Precision')
    arrows(seq_along(f), f-1.96*se, seq_along(f), f+1.96*se,
           code = 3, length = 0.2, angle = 90, col = 'red')
    axis(1, at=seq_along(f), labels=FALSE)
    text(seq_along(f), par("usr")[3] - 0.0025, srt = 90, adj = 1,
         labels = names(f), xpd = TRUE)
    par(par_save)
}

plot_recall <- function(confusion_matrix) {
    par_save <- par(mar = c(7, 4, 4, 2) + 0.1)
    f <- diag(confusion_matrix) / rowSums(confusion_matrix)
    se <- sqrt(f * (1-f) / rowSums(confusion_matrix))
    plot(f, ylim = c(0.97*min(f), min(1.03*max(f), 1)),
         pch = 16, col = 'blue', xaxt = 'n', xlab = '', ylab = 'Recall')
    arrows(seq_along(f), f-1.96*se, seq_along(f), f+1.96*se,
           code = 3, length = 0.2, angle = 90, col = 'red')
    axis(1, at=seq_along(f), labels=FALSE)
    text(seq_along(f), par("usr")[3] - 0.0025, srt = 90, adj = 1,
         labels = names(f), xpd = TRUE)
    par(par_save)
}

plot_precision(tab)
plot_recall(tab)

?image
args(image.default)
image(tab, col=heat.colors(6), useRaster=TRUE)

dev.off()


## Increase bottom margin to make room for rotated labels

## Create plot with no x axis and no x axis label
plot(1 : 8, xaxt = "n",  xlab = "")
## Set up x axis with tick marks alone
axis(1, labels = FALSE)
## Create some text labels
labels <- paste("Label", 1:8, sep = " ")
## Plot x axis labels at default tick marks
## Plot x axis label at line 6 (of 7)
mtext(1, text = "X Axis Label", line = 6)


plot_recall <- function(confusion_matrix) {

}


x = 1:13*2-1
plot(data$mean~x, cex=1.5,xaxt='n',ylim=c(0.3,0.40), 
    xlab='',ylab='lalala!', main='blahblahblah',col='blue',pch=16)
axis(1, at=x, labels=names)
arrows(x,CI.dn,x,CI.up,code=3,length=0.2,angle=90,col='red')
legend("bottomleft",paste(names,": S.E=",data$se),ncol=6,text.width=1)
