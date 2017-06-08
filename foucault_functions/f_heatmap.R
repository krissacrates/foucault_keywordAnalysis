f_heatmap <- function() {
    
    ###############################
    ## SECTION ONE: Subfunctions ##
    ###############################
    
    # Install required packages
    # requiredPacks() version 2.0 (2017-06-07)
    requiredPacks <- function() {
        packs.needed <- c("plotly", "lattice") # name all packages to use
        packs <- installed.packages()
        packs.match <- match(packs.needed, packs)   # if packaged needed are not among installed, there is NA value
        packs.nonavailable <- which(is.na(packs.match)) # which positions in packs.needed are not installed
        nonavailable.count <- length(packs.nonavailable)
        if (nonavailable.count >= 1) {
            for (i in 1:nonavailable.count) install.packages(packs.needed[i], 
                                                             quiet = TRUE, dependencies = TRUE)
        }
        packs.load <- lapply(packs.needed, require, character.only = TRUE) # coerce packages into list and load them
        x <- prod(as.logical(packs.load))   # product of logical values. If one is 'FALSE' then the product is FALSE
        return(x)
    }
    
    # Choose book to analyze
    chooseTheBook <- function() {
        books <- c("History of Madness", "The Birth of the Clinic", "The Order of Things", 
                   "The Archaeology of Knowledge", "Other - I will provide the acronym")
        acronyms <- c("HF", "NC", "MC", "AS")
        myBook <- switch (menu(books, title = "Choose the book:") + 1,
                          NA, case1 = acronyms[1],
                          case2 = acronyms[2],
                          case3 = acronyms[3],
                          case4 = acronyms[4],
                          case5 = "other")    # interactive items cannot be knitted into HTML
        if (myBook == "other") myBook <- readline(prompt = "What is your book's acronym? (eg. HF): ")
        if (is.na(myBook)) warning("No book, no fun!")
        return(myBook)
    }
    
    # Construct the table for heatmap
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
        
        heat.matrix <- matrix(sum.clusters, nrow = 1, ncol = length(sum.clusters) )  
        return(heat.matrix)
    }
    
    plotHeatmap <- function(keyword, heatMatrix) {
        #Optimize heatmap visuals
        vals <- unique(scales::rescale(c(heatMatrix)))
        o <- order(vals, decreasing = FALSE)
        cols <- scales::col_numeric("Blues", domain = NULL)(vals)
        colz <- setNames(data.frame(vals[o], cols[o]), NULL)
        
        # Create a heatmap
        p <- plot_ly(y = keyword, z = heatMatrix, colorscale = colz, type = "heatmap", height = 200)
    }
    
    ###########################    
    ## SECTION TWO: Code run ##
    ###########################
    
    # requiredPacks()
    if (!requiredPacks()) stop("Function failed: requiredPacks() error.")  # load packages needed; break if missing
    
    #chooseTheBook()
    bookAcronym <- chooseTheBook()  # choose the right book with the function
    if (is.na(bookAcronym) || bookAcronym == "other") stop("Function failed: 
            chooseTheBook() error.")  # check if data input exists out of the chooseTheBook()
    csvPath <- paste0("./output-files/", bookAcronym, "_lemmas_full.csv")  # construct the path
    if (!file.exists(csvPath)) stop(paste("File does not exists. 
                                Check the /output-files folder!"))     # check if the file exists
    
    #heatTable()
    # Get first input data to the function
    cat("Cool! You're about to create a heatmap. \n")
    kws <- readline(prompt = "Input a keyword you want to analyze: ")
    ch <- readline(prompt = "How many parts you want your book to split into? ")
    ch <- as.integer(ch)    # coerce to integer
    if (is.na(ch)) stop("Sorry! It obviously needs to be integer. Function broke...")
    heatMatrix <- heatTable(bookAcronym, kws, ch)   # Perform the function to ge the matrix table

    #plotHeatmap()
    p <- plotHeatmap(kws, heatMatrix)  # plot the matrix into heatmap
    p   # output
}