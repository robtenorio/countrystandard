#' countrystandard
#'
#' @param x A character vector of country names, either spelled correctly or with misspellings, that you wish to standardize
#' @param code A character vector of country code. Defaults to "ISOA3", options: "ISOA3", "ISOA2", "ISON", "IMFcode", "FIPS", "STANAG", "Internet"
#' @param name A character vector of country name. Defaults to "ISOname", options: "ISOname", "IMFname", "FIPSname"
#'
#' @return a character vector of supplied country names, a vector of corrected country names, and and optional vector of country codes
#' @export
#'
#' @examples countrystandard(c("congo", "congo dr", "Democratic People's Republic of Congo"), code="IMFcode", name="IMFname")

countrystandard <- function(x = NULL, code="ISOA3", name="ISOname", spellcheck=FALSE) {
  ########### Create a Function for Later
  # function to find the edit minimum edit distance
  distance_function <- function(x=NULL){
    if(length(x) > 1){
      correct_x <- lapply(1:nrow(x), function(z) correct_substrings[which(x[z,] == min(x[z,], na.rm=TRUE))])
    }
    if(length(x) == 1){
      correct_x <- list(correct_substrings[which(x == min(x, na.rm=TRUE))])
    }
    return(unlist(correct_x))
  }
  
  ####### Find Matches Using REGEXs
  country <- x
  
  input <- tolower(country)
  temp.env <- new.env()
  data(names.regex, package="countrystandard", envir=temp.env)
  master_names <- get("master_names", envir = temp.env)
  regex <- master_names$regex
  
  regex <- gsub("\\\\", "\\", regex, fixed=TRUE)
  
  results <- data.frame(sapply(regex, function(x) grepl(x, input, perl=TRUE), USE.NAMES=TRUE))
  
  index_1 <- data.frame(which(results==TRUE,arr.ind=T))
  
  if(length(country) == 1){
    x <- index_1$col
    y <- index_1$row
  } else{
    x <- index_1$row
    y <- index_1$col
  }
  
  if(nrow(index_1) > 0) {
    standard_df_1 <- data.frame("code" = master_names[[code]][y], "standard.name" = master_names[[name]][y], 
                                "supplied.name" = country[x], "matched" = "matched",
                                stringsAsFactors=FALSE)
    
    no_match_index <- which(!country %in% standard_df_1$supplied.name)
    country_no_match <- country[no_match_index]
  } 
  if(nrow(index_1) == 0){
    country_no_match <- country
  }
  
  if(spellcheck==FALSE){
    if(length(country_no_match)==0) {
      final_df <- standard_df_1
    }
    if(length(country_no_match) > 0) {
      country_no_match_df <- data.frame("code" = NA, "standard.name" = NA, "supplied.name" = country_no_match, 
                                        "matched"="no match", stringsAsFactors=FALSE)
      if(nrow(index_1) > 0) {
        final_df <- rbind(standard_df_1, country_no_match_df)
      }
      if(nrow(index_1) == 0) {
        final_df <- country_no_match_df
      }
      
    }
    
  }
  
  ########## Spell Check Unmatched Names
  if(spellcheck==TRUE & length(country_no_match) > 0) {
    no_match_names <- strsplit(tolower(country_no_match), " ")
    
    names_to_split <- gsub(",", "", master_names$master_name)
    correct_names_split <- lapply(names_to_split, function(x) strsplit(x, " "))
    correct_substrings <- unique(tolower(unlist(correct_names_split)))
    
    edit_distance <- lapply(no_match_names, function(x) adist(x, correct_substrings, costs = list("insertions"=1,
                                                                                                  "deletions"=2,
                                                                                                  "substitutions"=2)))
    
    output <- lapply(edit_distance, function(x) distance_function(x))
    output2 <- lapply(output, function(x) paste(x, sep="", collapse=" "))
    
    ######## Run it through the REGEX again
    input <- tolower(output2)
    
    results <- data.frame(sapply(regex, function(x) grepl(x, input, perl=TRUE), USE.NAMES=TRUE))
    
    index_2 <- data.frame(which(results==TRUE,arr.ind=T))
    
    if(length(country_no_match) == 1){
      x <- index_2$col
      y <- index_2$row
    } else{
      x <- index_2$row
      y <- index_2$col
    }
    
    standard_df_2 <- data.frame("code" = master_names[[code]][y], "standard name" = master_names[[name]][y], 
                                "supplied name" = country_no_match[x], "matched"="spell checked", 
                                stringsAsFactors=FALSE)
    
    if(nrow(index_1) > 0){
      ### Now combine the two data frames
      final_df <- rbind(standard_df_1, standard_df_2)
    }
    if(nrow(index_1) == 0){
      final_df <- standard_df_2
    }
    
  }
  
  if(spellcheck==TRUE & length(country_no_match)==0){
    final_df <- standard_df_1
  }
  
  return(final_df[order(final_df$supplied.name),])
  
}
