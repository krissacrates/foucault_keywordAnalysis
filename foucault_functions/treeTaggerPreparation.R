# This preparation function downloads all the file necessary to install TreeTagger.
# TreeTagger needs to be installed in the shell.
# The source of the guide is: http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/
# This function assumes Linus environment. 
# Should you have any issues, please consult original guide and process accordingly.

treeTaggerPreparation <- function() {
    if(!file.exists("~/tree-tagger")){dir.create(path = "~/tree-tagger")}
    
    # Step 1: Download the tagger package for your system
    taggerPackLinuxPath <- "http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/tree-tagger-linux-3.2.1.tar.gz"
    download.file(url = taggerPackLinuxPath, destfile = "~/tree-tagger/tree-tagger-linux-3.2.1.tar.gz", method = "auto")
    
    # Step 2: Download the tagging scripts into the same directory
    taggingScripts <- "http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/tagger-scripts.tar.gz"
    download.file(url = taggingScripts, destfile = "~/tree-tagger/tagger-scripts.tar.gz", method = "auto")
    
    # Step 3: Download the installation script install-tagger.sh
    installationScript <- "http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/install-tagger.sh"
    download.file(url = installationScript, destfile = "~/tree-tagger/install-tagger.sh", method = "auto")
    
    # Step 4: Download the parameter files for English and French
    tagfile.english <-  "http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/english-par-linux-3.2-utf8.bin.gz"
        # Tagset documentation: http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/Penn-Treebank-Tagset.pdf
    tagfile.french <- "http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/french-par-linux-3.2-utf8.bin.gz"
        # Tagset documentation: http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/french-tagset.html
    download.file(url = tagfile.english, destfile = "~/tree-tagger/english-par-linux-3.2-utf8.bin.gz", method = "auto")
    download.file(url = tagfile.french, destfile = "~/tree-tagger/french-par-linux-3.2-utf8.bin.gz", method = "auto")
    
    # Step 5: Open a terminal window and run the installation script in the directory where you have downloaded the files:
    # sh install-tagger.sh
    
    # Step 6: Make a test, e.g.
    # echo 'Hello world!' | cmd/tree-tagger-english 
    # and 
    # echo 'Je suis Michel.' | cmd/tagger-chunker-french
    print("Now you are ready to install TreeTagger. Open a terminal window and run the installation script in the directory where you have downloaded the files: sh install-tagger.sh") 
}