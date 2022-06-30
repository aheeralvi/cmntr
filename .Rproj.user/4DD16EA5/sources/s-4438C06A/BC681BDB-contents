library(stringi)

cmnt_banner <- function(content) {

  line1 <- paste(sep="", "#", strrep("*", 75))
  line2 <- ""
  content <- stringi::stri_pad_both(content, width = 69, pad = " ")
  line2 <- paste(sep="", "#***", content, "***")
  line3 <- paste(sep="", "#", strrep("*", 75))
  return(c(line1, line2, line3))
}

cmnt_header <- function(name, version, description, author, date, assump,
                        params, inputs, outputs, dependencies) {
  nextLine <- paste(sep="", "#", strrep("*",75))
  comment <- vector()
  comment <- c(comment, nextLine)

  nextLine <- paste(sep="", "#* NAME: ", name)
  comment <- c(comment, nextLine)
  nextLine <- paste(sep="", "#", strrep("*",75))
  comment <- c(comment, nextLine)


  nextLine <- paste(sep="", "#* VERSION: ", version)
  comment <- c(comment, nextLine)


  nextLine <- paste(sep="", "#* AUTHOR: ", author)
  comment <- c(comment, nextLine)

  nextLine <- paste(sep="", "#* DATE: ", date)
  comment <- c(comment, nextLine)

  nextLine <- paste(sep="", "#", strrep("*",75))
  comment <- c(comment, nextLine)

  nextLine <- "#*"
  comment <- c(comment, nextLine)

  nextLine <- "#* PARAMETERS:"
  comment <- c(comment, nextLine)
  for(i in 1:length(params)) {
    nextLine <- paste(sep="", "#* - ", params[i], ":")
    comment <- c(comment, nextLine)
  }

  nextLine <- "#*"
  comment <- c(comment, nextLine)

  nextLine <- "#* INPUTS:"
  comment <- c(comment, nextLine)

  # stringHolder <- ""
  # inputCounter <- 1
  # while(inputCounter <= length(inputs)) {
  #   if(inputCounter == 1) {
  #     stringHolder <- inputs[inputCounter]
  #   }
  #   else {
  #     stringHolder <- paste(sep=", ", stringHolder, inputs[inputCounter])
  #   }
  #   inputCounter <- inputCounter + 1
  # }


  return(comment)
}











wrap <- function(str, lineLength, firstLine) {
  words <- stri_split_boundaries(str = str)

  # First line
  wordCounter <- 1
  full <- vector()
  holder <- ""
  # print(nchar(holder))
  if(!is.null(firstLine)) {
    while( (nchar(words[[1]][wordCounter]) + nchar(holder)) <= firstLine) {
      holder <- paste(sep = "", holder, words[[1]][wordCounter])
      wordCounter <- wordCounter + 1
    }
    full <- c(full, holder)
    holder <-""
  }
  # print(length(words[[1]]))

  # runaway loop why
  while(wordCounter <= length(words[[1]])) {
    # print(wordCounter)
    while(nchar(words[[1]][wordCounter]) + nchar(holder) <= lineLength && wordCounter <= length(words[[1]])) {
      holder <- paste(sep="", holder, words[[1]][wordCounter])
      wordCounter <- wordCounter + 1
    }
    full <- c(full, holder)
    holder <- ""
  }

  return(full)

}







