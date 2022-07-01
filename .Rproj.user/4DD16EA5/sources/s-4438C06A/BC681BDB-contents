library(stringi)






# Comment class constructor

new_comment <- function(name, version, description, author, date, assump, params, inputs, outputs, dependencies) {
  x<-list()
  x$name <- name
  x$version <- version
  x$description <- description
  x$author <- author
  x$date <- date
  x$assump <- assump
  x$params <- params
  x$inputs <- inputs
  x$outputs <- outputs
  x$dependencies <- dependencies
  return(structure(x, class = "comment"))
}

# Validator/Helper

comment <- function(name, version, description, author, date, assump, params, inputs, outputs, dependencies) {
  stopifnot(is.character(name))
  stopifnot(is.character(version))
  stopifnot(is.character(description))
  stopifnot(is.character(author))
  stopifnot(is.character(date))
  stopifnot(is.character(assump))
  stopifnot(is.list(params))
  stopifnot(is.vector(inputs))
  stopifnot(is.vector(outputs))
  stopifnot(is.character(dependencies))
  return(new_comment(name, version, description, author, date, assump, params, inputs, outputs, dependencies))
}






cmnt_banner <- function(content) {

  line1 <- paste(sep="", "#", strrep("*", commentWidth-1))
  line2 <- ""
  content <- stringi::stri_pad_both(content, width = 69, pad = " ")
  line2 <- paste(sep="", "#***", content, "***")
  line3 <- paste(sep="", "#", strrep("*", commentWidth-1))
  return(c(line1, line2, line3))
}

cmnt_header <- function(name, version, description, author, date, assump,
                        params, inputs, outputs, dependencies, commentWidth = 76) {
  nextLine <- paste(sep="", "#", strrep("*",commentWidth-1))
  commentVec <- vector()
  commentVec <- c(commentVec, nextLine)

  nextLine <- paste(sep="", "#* NAME: ", name)
  commentVec <- c(commentVec, nextLine)
  nextLine <- paste(sep="", "#", strrep("*",commentWidth-1))
  commentVec <- c(commentVec, nextLine)


  nextLine <- paste(sep="", "#* VERSION: ", version)
  commentVec <- c(commentVec, nextLine)

  nextLine <- "#* DESCRIPTION: "
  firstLine <- commentWidth - nchar(nextLine)
  wrappedText <- wrap(description, commentWidth, firstLine, "   ")
  nextLine <- paste(sep="", nextLine, wrappedText[1])
  commentVec <- c(commentVec, nextLine)

  if(length(wrappedText) >= 2) {
    for(i in 2:length(wrappedText)) {
      # Note hard-coding of indent
      nextLine <- paste(sep="", "#*", wrappedText[i])
      commentVec <- c(commentVec, nextLine)
    }
  }

  nextLine <- paste(sep="", "#* AUTHOR: ", author)
  commentVec <- c(commentVec, nextLine)

  nextLine <- paste(sep="", "#* DATE: ", date)
  commentVec <- c(commentVec, nextLine)

  nextLine <- paste(sep="", "#", strrep("*",commentWidth-1))
  commentVec <- c(commentVec, nextLine)

  nextLine <- "#* ASSUMPTIONS: "
  firstLine <- commentWidth - nchar(nextLine)
  wrappedText <- wrap(assump, commentWidth, firstLine, "   ")
  nextLine <- paste(sep="", nextLine, wrappedText[1])
  commentVec <- c(commentVec, nextLine)

  if(length(wrappedText) >= 2) {
    for(i in 2:length(wrappedText)) {
      nextLine <- paste(sep="", "#*", wrappedText[i])
      commentVec <- c(commentVec, nextLine)
    }
  }

  nextLine <- "#*"
  commentVec <- c(commentVec, nextLine)

  nextLine <- "#* PARAMETERS:"
  commentVec <- c(commentVec, nextLine)
  paramNames <- names(params)
  # print(params)
  # print(length(paramNames))
  for(i in 1:length(paramNames)) {

    wrappedText <- wrap(params[i], commentWidth, commentWidth - nchar(paramNames[i]) - 5,"   ")

  }

  nextLine <- "#*"
  commentVec <- c(commentVec, nextLine)

  nextLine <- "#* INPUTS:"
  commentVec <- c(commentVec, nextLine)

  wrappedText <- ""
  for(i in 1:length(inputs)) {
    if(nchar(wrappedText) == 0) {
      wrappedText <- inputs[i]
    }
    else {
      wrappedText <- paste(sep=", ", wrappedText, inputs[i])
    }
  }
  nextLine <- "#* - Datasets: "
  wrappedText <- wrap(wrappedText, commentWidth, commentWidth-nchar(nextLine), "")
  nextLine <- paste(sep="", nextLine, wrappedText[1])
  commentVec <- c(commentVec, nextLine)
  nextLine <- "#*"
  commentVec <- c(commentVec, nextLine)


  nextLine <- "#* OUTPUTS:"
  commentVec <- c(commentVec, nextLine)

  wrappedText <- ""
  for(i in 1:length(outputs)) {
    if(nchar(wrappedText) == 0) {
      wrappedText <- outputs[i]
    }
    else {
      wrappedText <- paste(sep=", ", wrappedText, outputs[i])
    }
  }
  nextLine <- "#* - Datasets: "
  wrappedText <- wrap(wrappedText, commentWidth, commentWidth-nchar(nextLine), "")
  nextLine <- paste(sep="", nextLine, wrappedText[1])
  commentVec <- c(commentVec, nextLine)
  nextLine <- "#*"
  commentVec <- c(commentVec, nextLine)
  return(commentVec)
}




wrap <- function(str, lineLength, firstLine, indentStr) {
  words <- stri_split_boundaries(str)

  # First line
  wordCounter <- 1
  full <- vector()
  holder <- ""
  # print(nchar(holder))
  if(!is.null(firstLine)) {
    while(nchar(words[[1]][wordCounter])+nchar(holder)<=firstLine
          && wordCounter <= length(words[[1]])) {
      holder <- paste(sep = "", holder, words[[1]][wordCounter])
      wordCounter <- wordCounter + 1
    }
    full <- c(full, holder)
    holder <-""
  }
  # print(length(words[[1]]))

  while(wordCounter <= length(words[[1]])) {
    # print(wordCounter)
    holder <- indentStr
    while(nchar(words[[1]][wordCounter]) + nchar(holder) <= (lineLength - nchar(indentStr) - 2)
          && wordCounter <= length(words[[1]])) {
      holder <- paste(sep="", holder, words[[1]][wordCounter])
      wordCounter <- wordCounter + 1
    }
    full <- c(full, holder)
  }

  return(full)

}







