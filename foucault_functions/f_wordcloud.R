f_wordcloud <- function() {
    
    ###############################
    ## SECTION ONE: Subfunctions ##
    ###############################
    
    # Install required packages
    # requiredPacks() version 2.0 (2017-06-07)
    requiredPacks <- function() {
        packs.needed <- c("wordcloud2") # name all packages to use
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
    # Version 2.0
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
    
    ###########################    
    ## SECTION TWO: Code run ##
    ###########################
    
    # requiredPacks()
    if (!requiredPacks()) stop("Function failed: requiredPacks() error.")  # load packages needed; break if missing
    
    #chooseTheBook()
    bookAcronym <- chooseTheBook()  # choose the right book with the function
    if (is.na(bookAcronym) || bookAcronym == "other") stop("Function failed: 
                                                           chooseTheBook() error.")  # check if data input exists out of the chooseTheBook()
    csvPath <- paste0("./output-files/", bookAcronym, "_lemmas_nouns.csv")  # construct the path
    if (!file.exists(csvPath)) stop(paste("File does not exists. 
                                          Check the /output-files folder!"))     # check if the file exists
    
    
    ## Build wordcloud
    cloud2.df <- read.csv(csvPath)
    cloud2.df <- cloud2.df[, c("lemma", "n")]    # subset dataframe to 2 columns as expected
    set.seed(8732)
    wordcloud2(cloud2.df, color = "random-light", backgroundColor="black")  
    
}