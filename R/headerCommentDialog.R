library(shiny)
library(miniUI)
library(rstudioapi)
library(stringi)

headerCommentDialog <- function() {
  ui <- miniPage(
    includeHighlightJs(),
    miniContentPanel(
      textInput("nameInput", "Name:"),
      textInput("versInput", "Version:"),
      textAreaInput("descInput", "Description:"),
      textInput("authInput", "Author:"),
      dateInput("dateInput", "Date:"),
      textAreaInput("assumpInput", "Assumptions:"),
      uiOutput("document", container = rCodeContainer),
      HTML("<button id = 'okay'
           type ='button'
           class = 'btn-overload btn btn-default action-button shiny-bound-input'>
           Okay</button>"),

      HTML("<button id = 'cancel'
           type = 'button'
           class = 'btn-overload btn btn-default action-button shiny-bound-input'>
           Cancel</button>")
    )
  ) # End UI


  server <- function(input, output, session) {
    com <- getSourceEditorContext()
    header <- com$contents


    # random onload event just because it'll probably be used
    observeEvent(input$okay, ignoreNULL = FALSE, ignoreInit = FALSE, once = TRUE, {
      name <- getField("NAME:", header)
      version <- getField("VERSION:", header)
      auth <- getField("AUTHOR:", header)
      date <- getField("DATE:", header)
      desc <- getWrapField("DESCRIPTION:", header, "   ")
      assump <- getWrapField("ASSUMPTIONS:", header, "   ")
      print(desc)
      print(assump)
      paramList <- list()
      dependList <- list()
      tempNames <- getListNames("PARAMETERS:", header)
      for(i in 1:length(tempNames)) {
        listContents <- getWrapField(paste0(tempNames[i], ":"), header, "   ")
        paramList[[tempNames[i]]] <- listContents
      }
      print(paramList)

      tempNames <- getListNames("DEPENDENCIES:", header)
      for(i in 1:length(tempNames)) {
        listContents <- getWrapField(paste0(tempNames[i],":"), header, "   ")
        dependList[[tempNames[i]]] <- listContents
      }
      print(dependList)
    })

    output$document <- renderCode({
      paste(header, collapse = "\n")
    })




  } # End Server
  viewer <- dialogViewer("Insert/Edit Header Comment:", width = 1000)
  runGadget(ui, server, viewer = viewer)
} # End Gadget



getField <- function(strField, headerComment) {
    for(eachLine in headerComment) {
      # print(eachLine)
      check <- grepl(paste0(strField, " "), eachLine)
      if(check) {
        location <- as.list(stri_locate_first_fixed(eachLine, strField))
        retVal <- stri_sub(eachLine, from = location[[2]][1] + 2, to = nchar(eachLine))
        print(retVal)
        return(retVal)
      }
    }
    return("")
}

getWrapField <- function(strField, headerComment, indentStr) {
  for(i in 1:length(headerComment)) {
    eachLine <- headerComment[i]
    check <- grepl(paste0(strField, " "), eachLine)
    if(check) {
      # print("hey")
      location <- as.list(stri_locate_first_fixed(eachLine, strField))
      retVal <- stri_sub(eachLine, from = location[[2]][1] + 2, to = nchar(eachLine))
      counter <- i + 1
      while(counter <= length(headerComment) && grepl(indentStr, headerComment[counter])) {
        # print("hi")
        holder <- stri_sub(headerComment[counter], from = nchar(indentStr) + 2)
        # print(holder)
        retVal <- paste0(retVal, holder)
        counter <- counter + 1
      }
      return(retVal)
    }
  }
}

getListNames <- function(strField, headerComment) {
  for(i in 1:length(headerComment)) {
    retVal <- vector()
    eachLine <- headerComment[i]
    check <- grepl(strField, eachLine)
    if(check) {
      counter <- i + 1
      while(counter <= length(headerComment)) {
        # print(counter)
        ifNam <- grepl(" - ", headerComment[counter])
        # print(ifNam)
        ifInd <- grepl("   ", headerComment[counter])
        if(ifNam) {
          # print(headerComment[counter])
          # PROBLEM RIGHT HERE!?
          startFrom <- as.list(stri_locate_first_regex(headerComment[counter], " - "))
          endWith <- as.list(stri_locate_first_regex( headerComment[counter], ":"))
          # print(startFrom)
          # print(endWith)
          holder <- stri_sub(headerComment[counter], from = startFrom[[2]][1] + 1, to = endWith[[2]][1]-1)
          # print(holder)
          retVal <- c(retVal, holder)
          counter <- counter + 1
        }
        else if(ifInd) {
          counter <- counter + 1
        }
        else {
          return(retVal)
        }
      }
    }
  }
}
