# Prerequisit: Treetagger implemented

foucaultize <- function() {
    
    ###############################
    ## SECTION ONE: Subfunctions ##
    ###############################
    
    # Find out if packages are installed
    requiredPacks <- function() {
        packs.needed <- c("koRpus", "dplyr", "wordcloud") # name all packages to use
        packs <- installed.packages()
        packs.match <- match(packs.needed, packs)   # if packaged needed are not among installed, there is NA value
        if (is.na(sum(packs.match))) { # sum of matched positions is NA in case only 1 of them is NA
            cat("Some packages are missing. Check, if you have all of them:", packs.needed, sep = "\n")
            return(FALSE)
        } else {
            packs.load <- lapply(packs.needed, require, character.only = TRUE) # coerce packages into list and load them
            x <- prod(as.logical(packs.load))   # product of logical values. If one is 'FALSE' then the product is FALSE
            return(x)
        }
    }
    
    # Ask user to choose the source text for the analysis
    bookToRead <- function() {
        # Prepare variables
        bookchoice <- c("History of Madness", "The Birth of the Clinic", "Order of Things", 
                        "Archaeology of Knowledge")
        booksource <- c("1961_History-of-madness.txt", "1963_Birth-of-the-clinic.txt", 
                        "1966_Order-of-Things.txt", "1969_Archaeology-of-Knowledge.txt")
        bookshort.choice <- c("HF", "NC", "MC", "AS")
        thebook <- menu(c(bookchoice, "Other - must be raw TXT file!"), 
                        title = "Choose the book you want to analyze:") # Get file
        cat("\n Processing...")
        if(!dir.exists("./corpus-download")){dir.create("./corpus-download")}
        
        # Download the right book
        if ( thebook <= length(bookchoice) ) {  # checks if user choice is one of book numbers
            myBook <- booksource[thebook]
            
                #Chech is the choice is not 0
                if (length(myBook) < 1) {
                    warning("You haven't chosen any book. How shall I proceed?")
                    x <- list(NA)   # main code reads output as list in positive cases. 
                                    # The 'NA' case needs to be list to prevent warnings.
                    return(x)
                }
            
            bookshort <- bookshort.choice[thebook]
            fileSource <- paste0("https://raw.githubusercontent.com/krissacrates/foucault_keywordAnalysis/master/corpus/", 
                                 myBook)
            destination <- paste0("./corpus-download/", myBook)
            if(!file.exists(destination)) {
                download.file(fileSource, destfile = destination, method = "curl")
            } else warning("File already exists: download skipped!")
            x <- list(fileName = myBook, Acronym = bookshort)
            return(x)
            
        } else if ( thebook == length(bookchoice) + 1) {    # do if user choice of the book is 'Other'
            thebook <- file.choose()
                if (tools::file_ext(thebook) != "txt") {
                    warning("This is not the TXT file.")
                    x <- list(NA)
                    return(x)
                }
            myBook <- readline(prompt = "How do you want to call the file (incl. TXT)? ")
            bookshort <- readline(prompt = "Provde 2-letter short name for the book (eg. HF): ")
            destination <- paste0("./corpus-download/", myBook)
            if(!file.exists(destination)) {
                file.copy(thebook, destination)
                cat("Message: File copied to the corpus.")
            } else warning("File already exists: step skipped!")
            x <- list(fileName = myBook, Acronym = bookshort)
            return(x)
        }   
            else {   
                warning("Unknown action...")
                x <- list(NA)   
                return(x)
        }
    }    

    
    lemmatize <- function(x, y) {
        ## 'x' is the source text
        ## 'y' is the acronym of the book
        
        # Load the file
        destination <- paste0("./corpus-download/", x)
            if(!file.exists(destination)) stop("File not found!")
        bookload <- readLines(destination)
        
        # Text cleaning
        cleanbook <- tolower(bookload) # lowercase everything
        cleanbook <- stringr::str_replace_all(cleanbook,"[^a-zA-Z\\s]", " ")    # remove everything that is not a letter
        cleanbook <- stringr::str_replace_all(cleanbook,"[\\s]+", " ")  # shrink to just one space
        
        # Tokenize
        if(!dir.exists("./corpus-clean")){dir.create("./corpus-clean")}
        cleanbookPath <- paste0("./corpus-clean/clean_", x)
        writeLines(cleanbook, con = cleanbookPath) # create output TXT file first
        tagged.book <- treetag(cleanbookPath, lang = "en") # creates kRp.tagged class
        book.df <- taggedText(tagged.book) # change kRp.tagged class into readable data frame
        
        # Use <token> if <lemma> is unknown
        lemma.unknown <- which(book.df[, "lemma"] == "<unknown>") # identify rows of unknown lemmas
        tokens.toreplace <- book.df[lemma.unknown, 1]   # create a vector of tokens in rows of unknown lemmas
        book.df[lemma.unknown, "lemma"]  <- tokens.toreplace  # replace '<unknowns>' for tokens
        
        # Save the output of full lemmas table
        book.df <- book.df[, -7:-8] # remove unnecessary columns
        if(!dir.exists("./output-files")){dir.create("./output-files")}
        outputPathFull <- paste0("./output-files/", y, "_lemmas_full.csv")
        if(file.exists(outputPathFull)) {   # remove old file if it already exists
            file.remove(outputPathFull)
            warning("The file ", outputPathFull, " already exists. It will be overwritten.")
            }
        write.csv(book.df, file = outputPathFull)
        
        # Create unque lemmas table and save the output
        # 'dplyr' package needed for this operation
        uniqueLemmas.df <- book.df %>%
            group_by(lemma, wclass) %>%
            summarise(n = n()) %>%
            ungroup() %>%
            mutate(freq = n /sum(n))
        outputPathUnique <- paste0("./output-files/", y, "_lemmas_unique.csv")
        if(file.exists(outputPathUnique)) {   # remove old file if it already exists
            file.remove(outputPathUnique)
            warning("The file ", outputPathUnique, " already exists. It will be overwritten.")
        }
        write.csv(uniqueLemmas.df, file = outputPathUnique)
        
        # Create a subset with nouns
        a <- which(uniqueLemmas.df$wclass == "noun")    # vector of row numbers with nouns
        nounsLemmas.df <- uniqueLemmas.df[a, ]  # subset only nouns based on 'a' vector
        
        # Additional metrics: absolute rank, relative rank
        nouns.rank <- rank(nounsLemmas.df$freq, ties.method = "first") # vector of absolute rank
        nouns.relativeRank <- nouns.rank / length(nounsLemmas.df$freq) # vector of relative rank
        nounsLemmas.df <- cbind(nounsLemmas.df, "rank" = nouns.rank, "relativeRank" = nouns.relativeRank) # add mew metrics to the data frame
        nounsLemmas.df <- nounsLemmas.df[order(nounsLemmas.df[, "relativeRank"], decreasing = TRUE), ] # order data frame decreasing according to the rank metric
        
        ## Additional minor manual cleanup
        # Lemma 'S' is at high rank position which is obviously only inconsistency of tokenizing. Will be removed.
        nounsLemmas.df <- nounsLemmas.df[-match("S", nounsLemmas.df$lemma), ]
        row.names(nounsLemmas.df) <- 1:nrow(nounsLemmas.df) # change row names accordingly
        
        # nouns output
        outputPathNouns <- paste0("./output-files/", y, "_lemmas_nouns.csv")
        if(file.exists(outputPathNouns)) {   # remove old file if it already exists
            file.remove(outputPathNouns)
            warning("The file ", outputPathNouns, " already exists. It will be overwritten.")
        }
        write.csv(nounsLemmas.df, file = outputPathNouns)
        cat("Success! \nFind your CSV tables in the '/output-files' folder of your working directory.")
        return(TRUE)
    }
    
    
    ###########################    
    ## SECTION TWO: Code run ##
    ###########################
    
    if (!requiredPacks()) stop("Function failed: requiredPacks() error.")  # load packages needed; break if missing
    
    book.list <- bookToRead()   # source the file for the analysis
        if (is.na(book.list[[1]])) { # check is the file is ready
            stop("Function failed: bookToRead() error.")
        }
    
        bookFile <- book.list$fileName
        bookAcro <- book.list$Acronym
    
    lem <- lemmatize(x = bookFile, y = bookAcro)   # perform lemmatization on source file and provide output files
    if (!lem) stop("Function failed: lemmatization() error.") 
 
}
