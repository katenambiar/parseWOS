parseFiles <- function (filename, path, type = "text"){
  # Extractor version 2.2.4 (8 Dec 2011)
  # Require parameters:
  # filename = WoS filename to be extracted in the format "savedrecs-[order]_[family]_[genus]_[species][any number or null].txt"
  # path = path to the directory containing the file refered to in filename
  # debug: path <- input.dir; filename <- wos.files[1]
  #
  # Changelog: 
  # 17Nov2011 - Bug fixed in keywords plus extraction - was using keywordsend to define record end instead of keywordsplusend
  # 21Nov2011 - Updated author and journal extraction for database searching - author to upper case surname only, journal to upper case
  # 8 Dec 2011 - Added ISI unique identifer
  
  require(gdata) #Need this for the trim function
  summary <- NULL
  
  #Import text file
  extract <- readLines(paste(path,filename,sep=""), warn=FALSE, encoding = "UTF-8")
  
  # Get order, family, genus and species from the file name 
  classification <- strsplit(filename, "savedrecs-")
  classification <- unlist(lapply(classification, function(x) x[2]))
  classification <- strsplit(classification, "_")
  if (sapply(classification, length) == 4){
    order <- unlist(lapply(classification, function(x) x[1]))
    family <- unlist(lapply(classification, function(x) x[2]))
    genus <- unlist(lapply(classification, function(x) x[3]))
    species <- unlist(lapply(classification, function(x) x[4]))
    species <- strsplit(species, "\\.")
    species <- unlist(lapply(species, function(x) x[1]))
    species <- strsplit(species, "[1234567890]")
    species <- unlist(lapply(species, function(x) x[1]))
  } else {
    stop("Please check your WoS file name format - should be: savedrecs-[order]_[family]_[genus]_[species][any number or null].txt")
  }
   
  # start and end vectors for all records
  start <- grep("^AU ", extract) 
  end <- grep("^ER", extract)
  
  for (i in 1: length(start)){
    
    # Get record from full extract
    record <- extract[start[i]:end[i]]
  
    # 1st author
    record.firstauthor <- record[grep("^AU ", record)]
    record.firstauthor <-  gsub("^AU ", "", record.firstauthor)
    record.firstauthor <- unlist(lapply(strsplit(record.firstauthor, "\\,"), function(x) x[1]))
    record.firstauthor <- toupper(record.firstauthor)
  
    # Publication year
    record.year <- as.vector(record[grep("^PY ",as.character(record))])
    record.year <- as.numeric(gsub("^PY ", "", record.year))
  
    # Title extraction startpoint and endpoint
    titlestart <- grep("^TI ", record)
    titleend <- grep("^SO ", record)
    # Number of lines in each title (nb. assumes the next field after title is always Journal (^SO))
    titlelines <- titleend - titlestart
    # Extract 1st line of title
    record.title <- record[titlestart]
  
    # j loop - loops over the number of lines in each title
    for (j in 1:(titlelines-1)){
      # if the title is more than one line add on the other lines in the title
      if (titlelines > 1) {
        record.title <- paste (record.title, trim(record[j+titlestart]))
      }
    }
    record.title <- gsub("^TI ", "", record.title)
  
    #Keywords - returns a NA if no keywords are present. Requires that the following term is either ^ID or ^AB
    record.keywords <- record[grep("^DE ", record)]
    if (length(record.keywords) > 0){
          
          if (length(record[grep("^ID ", record)]) > 0){
              keywordsstart <- grep("^DE ", record)
              keywordsend <- grep("^ID ", record)
              keywordslines <- keywordsend - keywordsstart
              record.keywords <- record[keywordsstart]
              for (k in 1:(keywordslines-1)){
                if (keywordslines > 1) {
                  record.keywords <- paste (record.keywords, trim(record[k+keywordsstart]))
                }
              }
              record.keywords <- gsub("^DE ", "", record.keywords)
              
          } else if (length(record[grep("^AB ", record)]) > 0){
      
              keywordsstart <- grep("^DE ", record)
              keywordsend <- grep("^AB ", record)
              keywordslines <- keywordsend - keywordsstart
              record.keywords <- record[keywordsstart]
              for (k in 1:(keywordslines-1)){
                if (keywordslines > 1) {
                  record.keywords <- paste (record.keywords, trim(record[k+keywordsstart]))
                }
              }
              record.keywords <- gsub("^DE ", "", record.keywords)
          }
          
    } else {
      record.keywords <- NA
    } 
    
    #Keywords Plus - returns a NA if no keywords are present. Requires that the following term is either ^AB or ^C1
    record.keywordsplus <- record[grep("^ID ", record)]
    if (length(record.keywordsplus) > 0){
          
          if (length(record[grep("^AB ", record)]) > 0){
              keywordsplusstart <- grep("^ID ", record)
              keywordsplusend <- grep("^AB ", record)
              keywordspluslines <- keywordsplusend - keywordsplusstart
              record.keywordsplus <- record[keywordsplusstart]
              for (k in 1:(keywordspluslines-1)){
                if (keywordspluslines > 1) {
                  record.keywordsplus <- paste (record.keywordsplus, trim(record[k+keywordsplusstart]))
                }
              }
              record.keywordsplus <- gsub("^ID ", "", record.keywordsplus)
              
          } else if (length(record[grep("^C1 ", record)]) > 0){
      
              keywordsplusstart <- grep("^ID ", record)
              keywordsplusend <- grep("^C1 ", record)
              keywordspluslines <- keywordsplusend - keywordsplusstart
              record.keywordsplus <- record[keywordsplusstart]
              for (k in 1:(keywordspluslines-1)){
                if (keywordspluslines > 1) {
                  record.keywordsplus <- paste (record.keywordsplus, trim(record[k+keywordsplusstart]))
                }
              }
              record.keywordsplus <- gsub("^ID ", "", record.keywordsplus)
          }
          
    } else {
      record.keywordsplus <- NA
    }
    
    
    # Journal
	  record.journal <- record[grep("^SO ", record)]
	  record.journal <- gsub("^SO ", "", record.journal)
    record.journal <- toupper(record.journal)
  
    # Citations
	  record.citations <- as.vector(record[grep("^TC ",as.character(record))])
	  record.citations <- gsub("^TC ", "", record.citations)
    
    # ISI unique ID
    record.ID <- as.vector(record[grep("^UT ",as.character(record))])
    record.ID <- gsub("^UT ", "", record.ID)
  
    # Make the record row
    record.row <- as.data.frame(cbind(order, family, genus, species, record.ID, record.firstauthor, record.year, record.title, record.journal, record.keywords, record.keywordsplus, record.citations))

    #Build full table of all records 
	  summary <- rbind(summary, record.row)
}
  # Table headers for the output
  names(summary) <- c("Order","Family", "Genus", "Species", "ReferenceID", "Author", "Year", "Title", "Journal", "Keywords", "KeywordsPlus", "Citations")
  return (summary)
  
} # END EXTRACTOR FUNCTION


########################################################################################################


# Define directory to input the extracted files [CHANGE THIS IF NEEDED]
input.dir <- path.expand("~/Dropbox/IoZ/Carnivores/Species searches/WoS files/")

# Define directory to output the extracted files [CHANGE THIS IF NEEDED]
output.dir <- path.expand("~/Dropbox/IoZ/R stuff to share with K8/Brooke-Nambiar's monster/Extractor_Mark2/Output_Refs/")

# Get files to be extracted
wos.files <- list.files(input.dir)

# Loop to extract sequentially each file in the WoS_Refs folder and create the output table

outputtable <- NULL
extractsummary <- NULL
extraction.time <- system.time(
for (i in 1:length(wos.files)){
  
  cat("Extracting", wos.files[i], "\n")
  
  currentextract <- extractor(wos.files[i], input.dir)
  outputtable <- rbind(outputtable, currentextract)
  extractsummary <- rbind(extractsummary, as.data.frame(cbind(File=wos.files[i], Records=nrow(currentextract))))
}
)

write.table(outputtable, paste(output.dir, "Monster_table.txt", sep=""))
write.table(extractsummary, paste(output.dir, "Monster_table_summary.txt", sep=""))

cat(paste(length(wos.files), " files (", nrow(outputtable)," records) extracted in ", extraction.time[3], " seconds", sep = ""))
