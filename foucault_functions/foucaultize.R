# Prerequisit: Treetagger implemented

foucaultize <- function() {
    
    ###########################
    ## SECTION: Subfunctions ##
    ###########################
    
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
        if(!dir.exists("./corpus-download")){dir.create("./corpus-download")}
        
        # Download the right book
        if ( thebook <= length(bookchoice) ) {  # checks if user choice is one of book numbers
            myBook <- booksource[thebook]
            
                #Chech is the choice is not 0
                if (length(myBook) < 1) {
                    warning("You haven't chosen any book. How shall I proceed?")
                    x <- NA
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
                    x <- NA
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
                x <- NA
                return(x)
        }
    }    

    
    lemmatize <- function(x, y) {
        # write the rest
    }
    
    
    #######################    
    ## SECTION: Code run ##
    #######################
    
    if (!requiredPacks()) stop("Function failed: requiredPacks() error.")  # load packages needed; break if missing
    
    book.list <- bookToRead()   # source the file for the analysis
        if (is.na(book.list)) { # check is the file is ready
            stop("Function failed: bookToRead() error.")
        }
    
        bookFile <- book.list$fileName
        bookAcro <- book.list$Acronym
    
    lemmatize(bookFile, bookAcro)   # perform lemmatization on source file and provide output files
        
    

}