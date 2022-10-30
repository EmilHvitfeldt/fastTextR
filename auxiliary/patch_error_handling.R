# This file is used to modify the C++ source such that the C++ error handling
# is replaced by R error handling.

files <- dir("../src", pattern = ".cc", full.names = TRUE)
file <- "../src/fasttext.cc"

identify_throw_functions <- function(files) {
    x <- unlist(lapply(files, readLines))
    x <- grep("throw\\s+std", x, value = TRUE)
    table(trimws(gsub("(\\(.*|.*throw.*std::)", "", x)))
}

identify_cerr_functions <- function(files) {
    x <- unlist(lapply(files, readLines))
    x <- grep("cerr", x, value = TRUE)
    table(trimws(gsub("cerr.*", "cerr", x)))
}

identify_cout_functions <- function(files) {
    x <- unlist(lapply(files, readLines))
    x <- grep("cout", x, value = TRUE)
    table(trimws(gsub("cout.*", "cout", x)))
}

identify_throw_functions(files)
identify_cerr_functions(files)
identify_cout_functions(files)

for (file in files) {
    x <- readLines(file)
    
    if (any(grepl("throw\\s+std", x))) {
        x <- gsub("throw\\s+std.*?\\(", "Rcpp::stop(", x)
        writeLines(x, file)
    }

    if (any(grepl("std::cerr", x))) {
        x <- gsub("std::cerr", "Rcpp::Rcerr", x)
        writeLines(x, file)
    }

    if (any(grepl("std::cerr", x))) {
        x <- gsub("std::cout", "Rcpp::Rcout", x)
        writeLines(x, file)
    }

    if (any(grepl("exit(EXIT_FAILURE)", x, fixed = TRUE))) {
        x <- gsub("exit(EXIT_FAILURE)", 'Rcpp::stop("EXIT_FAILURE")', x, fixed = TRUE)
        writeLines(x, file)
    }
    
}

