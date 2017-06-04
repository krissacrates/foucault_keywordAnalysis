heatTable <- function(bookAcronym, keywords, clusters = 20L) {
    # 'bookAcronym' is the acronym of the book which is stored in ./output-files folder
    # 'keywords' are expecter to be character vector, eg. 'keywords = c("experience", "madness")'; single lenght vector allowed
    # clusters is a number of book parts (symetric) - chapter division is not currently supported
    
    # Read the file
    filePath <- paste0("./output-files/", bookAcronym, "_lemmas_full.csv")
    full.df <- read.csv(filePath)
    
    # Subset nouns only
    a <- which(full.df$wclass == "noun")    # vector of row numbers with nouns
    nouns.vector <- full.df[a, "lemma"] # subset only nouns based on 'a' vector
    remove(full.df) # cleanup memory from a big file
        
        # Create logical vector of occurences 
        logical.vector <- grepl(keywords, nouns.vector, ignore.case = TRUE)    # to be put into loop for more keywords

    volume.clusters <- ceiling( length(logical.vector) / clusters )  # number of keyword items in a cluster
    chunks <- split(logical.vector, ceiling(seq_along(logical.vector)/volume.clusters)) # create a list of chunks (= clusters)
    sum.clusters <- sapply(chunks, sum) # calculate sum of TRUE values each cluster - creates a numeric vector of sums
    
    heat.matrix <<- matrix(sum.clusters, nrow = 1, ncol = length(sum.clusters) )  
}