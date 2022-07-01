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
                        params, inputs, outputs, dependencies, commentWidth = 76) {
  nextLine <- paste(sep="", "#", strrep("*",75))
  comment <- vector()
  comment <- c(comment, nextLine)

  nextLine <- paste(sep="", "#* NAME: ", name)
  comment <- c(comment, nextLine)
  nextLine <- paste(sep="", "#", strrep("*",75))
  comment <- c(comment, nextLine)


  nextLine <- paste(sep="", "#* VERSION: ", version)
  comment <- c(comment, nextLine)

  nextLine <- "#* DESCRIPTION: "
  # Note hard-coding of bounds below (and elsewhere with 75)
  firstLine <- commentWidth - nchar(nextLine)
  wrappedText <- wrap(description, commentWidth, firstLine, "   ")
  nextLine <- paste(sep="", nextLine, wrappedText[1])
  comment <- c(comment, nextLine)

  if(length(wrappedText) >= 2) {
    for(i in 2:length(wrappedText)) {
      # Note hard-coding of indent
      nextLine <- paste(sep="", "#*", wrappedText[i])
      comment <- c(comment, nextLine)
    }
  }

  nextLine <- paste(sep="", "#* AUTHOR: ", author)
  comment <- c(comment, nextLine)

  nextLine <- paste(sep="", "#* DATE: ", date)
  comment <- c(comment, nextLine)

  nextLine <- paste(sep="", "#", strrep("*",75))
  comment <- c(comment, nextLine)

  nextLine <- "#* ASSUMPTIONS: "
  firstLine <- commentWidth - nchar(nextLine)
  wrappedText <- wrap(assump, commentWidth, firstLine, "   ")
  nextLine <- paste(sep="", nextLine, wrappedText[1])
  comment <- c(comment, nextLine)

  if(length(wrappedText) >= 2) {
    for(i in 2:length(wrappedText)) {
      nextLine <- paste(sep="", "#*", wrappedText[i])
      comment <- c(comment, nextLine)
    }
  }

  nextLine <- "#*"
  comment <- c(comment, nextLine)

  nextLine <- "#* PARAMETERS:"
  comment <- c(comment, nextLine)
  paramNames <- names(params)
  # print(params)
  # print(length(paramNames))
  for(i in 1:length(paramNames)) {
    nextLine <- paste(sep="", "#* - ", paramNames[i], ": ", params[[i]])
    comment <- c(comment, nextLine)
  }

  nextLine <- "#*"
  comment <- c(comment, nextLine)

  nextLine <- "#* INPUTS:"
  comment <- c(comment, nextLine)

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
  comment <- c(comment, nextLine)
  nextLine <- "#*"
  comment <- c(comment, nextLine)


  nextLine <- "#* OUTPUTS:"
  comment <- c(comment, nextLine)

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
  comment <- c(comment, nextLine)
  nextLine <- "#*"
  comment <- c(comment, nextLine)
  return(comment)
}




wrap <- function(str, lineLength, firstLine, indentStr) {
  words <- stri_split_boundaries(str)

  # First line
  wordCounter <- 1
  full <- vector()
  holder <- ""
  # print(nchar(holder))
  if(!is.null(firstLine)) {
    # print(firstLine)
    # print(nchar(holder))
    # print(nchar(words[[1]][wordCounter]) + nchar(holder) <= firstLine)
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







