# This function unifies different types of graphics of the Foucaultize project

foucault_graphics <- function(type, version = NA) {
    
    # version 1.0 (2017-06-10)
    # supported types - "wordcloud", "barchart", "heatmap"; mandatory variable
    # version refers to the type version; optional variable (no use in the 1.0 version)
    
    ###############################
    ## SECTION ONE: Subfunctions ##
    ###############################
    
    # Install required packages
    # requiredPacks() version 2.1 (2017-06-10) - packs.needed updated
    requiredPacks <- function() {
        packs.needed <- c("wordcloud2", "plotly", "lattice") # name all packages to use
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
    # Version 3.0 (2017-06-10) - enhanced with csvFile vector
    chooseTheBook <- function() {
        # Definitions
        books <- c("History of Madness", "The Birth of the Clinic", "The Order of Things", 
                   "The Archaeology of Knowledge", "Other - I will provide the acronym")
        acronyms <- c("HF", "NC", "MC", "AS")
        
        # User choice of the book - store the acronym value
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
    
    
    # Find the right CSV file according to graph type
    chooseTheFile <- function(x, y) {
        # x = output from chooseTheBook(), ie. the book Acronym
        # y = graph type
        
        if (y == "wordcloud" || y == "barchart") {
            csvFile <- "_lemmas_nouns.csv"
        } else if (y == "heatmap") {
            csvFile <- "_lemmas_full.csv"
        } 
        
        csvPath <- paste0("./output-files/", x, csvFile)   # construct path to the right csv
        if (!file.exists(csvPath)) return(NA) else return(csvPath)
    }
    
    # Create a wordcloud
    f_wordcloud <- function(x) {
        # x = input data frame
        x <- x[, c("lemma", "n")]    # subset dataframe to 2 columns as expected
        wordcloud2(x, color = "random-light", backgroundColor="black")  
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
    
    # Check function inputs
    typeOptions <- c("wordcloud", "barchart", "heatmap")    # supported types in current version
    versionOptions <- NULL  # not supported in version 1.0 of foucault_graphics()
    t <- match(type, typeOptions)
    if (is.na(t)) stop("Function failed: You haven't provided supported graph 'type' argument.")
    
    # requiredPacks()
    if (!requiredPacks()) stop("Function failed: requiredPacks() error.")  # load packages needed; break if missing
    
    #chooseTheBook()
    bookAcronym <- chooseTheBook()  # choose the right book with the function
    if (is.na(bookAcronym) || bookAcronym == "other") stop("Function failed: 
                                                           chooseTheBook() error.")  # check if data input exists out of the chooseTheBook()
    
    # chooseTheFile()
    filePath <- chooseTheFile(x = bookAcronym, y = type)
    if (!file.exists(filePath)) stop("CSV file does not exists! Any idea...? - Function filePaht() failed.")
    sourceData <- read.csv(filePath)
    
    # Run the graph
    if (type == "wordcloud") {  # wordcloud part
        
        f_w <- f_wordcloud(x = sourceData)
        f_w
        
    } else if (type == "barchart") {    # barchart part
        
        f_b <- barchart(lemma ~ n, sourceData[1:15, ], col = "grey", xlab = "Term frequency", 
                        main = paste("Frequency of top keyowrds in ", bookAcronym))
        f_b
        
    } else if (type == "heatmap") {     # heatmap part
        
        kws <- readline(prompt = "Input a keyword you want to analyze: ")
        ch <- readline(prompt = "How many parts you want your book to split into? ")
        ch <- as.integer(ch)    # coerce to integer
        if (is.na(ch)) stop("Sorry! It obviously needs to be integer. Function failed...")
        heatMatrix <- heatTable(bookAcronym, kws, ch)   # Perform the function to ge the matrix table

        #plotHeatmap()
        p <- plotHeatmap(kws, heatMatrix)  # plot the matrix into heatmap
        p   # output
        
    }
    
}