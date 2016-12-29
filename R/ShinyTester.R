

#' ShinyDummyCheck
#'
#' This function takes Shiny files themselves as inputs and tries to combine the different
#' assets presented in the ui and server files to see whether they match up.
#'
#' For now, it only works where the server and ui files are seperate (ie, it doesn't work for `app.R` yet)
#'
#' @param directory the directory or website containing the files for the Shiny App. Defaults to current working directory
#' @param ui a character vector size 1 containing the name of the UI files. defaults to "ui.R"
#' @param server a character vector size 1 containing the names of the SERVER file. defaults to "server.R"
#'
#' @return Returns a dataframe with the matchings b/w ui and server files. Also spawns them in VIEW mode.
#'
#' @examples
#' ShinyDummyCheck(directory = "https://raw.githubusercontent.com/mexindian/ShinyServer/master/LineSelector")
#'
#' ## Or, to test with your own app, go to your shiny app, make that your working directory, and then type `ShinyDummyCheck()`


ShinyDummyCheck <- function(directory=getwd(),ui="ui.R",server="server.R"){
  library(stringr)
  library(readr)
  library(tidyverse)

  ## Change Working Directory
  # source("ui.R", chdir = T)
  # setwd(getSrcDirectory(function(x) {x}))

  ## Get server
  read_file(paste(directory,"/",server,sep="")) %>% str_split("\r?\n") -> Srv

  Srv[[1]] %>% as.data.frame() %>%
    filter(grepl("\\(\\{",.)) %>%
    filter(!grepl("#.+",.)) %>%
    arrange(.) %>% .$. %>%
    str_split(" *<- *") %>% as.data.frame %>% t %>% as.data.frame -> SrvDF
  rownames(SrvDF) <- NULL
  colnames(SrvDF) <- c("Item","SrvCall")

  SrvDF$isOutput[grepl("output\\$",SrvDF$Item)] <- "Yes"
  SrvDF$Item <- gsub("output\\$","",SrvDF$Item)
  SrvDF$Item <- gsub(" +","",SrvDF$Item)
  SrvDF$Item <- gsub("\\(outputId *\\= *","",SrvDF$Item)
  SrvDF$SrvCall <- gsub("\\(\\{","",SrvDF$SrvCall)

  ## Get ui
  read_file(paste(directory,"/",ui,sep="")) %>% str_split("\r?\n") -> Ui

  ## But remove 'outputId' and other garbage
  Ui[[1]] <-    gsub("outputId *= *|h\\d\\(|[a-zA-Z]+Panel\\(","",Ui[[1]])

  ## Now extract just the outputs
  Ui[[1]] %>% as.data.frame() %>%
    filter(grepl("Output",.)) %>%
    filter(!grepl("#.+",.)) %>%
    arrange(.) %>% .$. %>% as.character %>%
    str_split('\\("') %>% as.data.frame %>% t %>% as.data.frame -> UiDF

  rownames(UiDF ) <- NULL
  colnames(UiDF) <- c("VisualCall","Item")
  UiDF$Item <- gsub('".+','',UiDF$Item)
  UiDF$Item <- gsub(' +','',UiDF$Item)
  UiDF$VisualCall <- gsub('"|\\)','',UiDF$VisualCall)
  UiDF$VisualCall <- gsub('.+\\(','',UiDF$VisualCall)
  UiDF$VisualCall <- gsub(' +','',UiDF$VisualCall)

  Bof <- full_join(SrvDF,UiDF)

  ## Conditions Good
  Bof$Status[tolower(gsub("render","",Bof$SrvCall)) ==  tolower(gsub("Output","",Bof$VisualCall))] <- "OK"
  Bof$Status[Bof$SrvCall == "reactive" & is.na(Bof$VisualCall)] <- "OK"
  Bof$Status[Bof$SrvCall == "renderPrint" | Bof$SrvCall == "renderText" &
               (Bof$VisualCall=="textOutput"|Bof$VisualCall=="verbatimTextOutput")] <- "OK"

  ## Conditions Bad
  Bof$Status[Bof$SrvCall != "reactive" & is.na(Bof$VisualCall)] <- "Need call in UI"
  Bof$Status[Bof$SrvCall == "reactive" & !is.na(Bof$VisualCall)] <- "Reactives don't go in UI... use 'render'somethin"
  Bof$Status[grepl("render",Bof$SrvCall) & is.na(Bof$isOutput)] <- "Put 'Output' before name in server.R definition"

  Bof %>% View
  Bof
}


#' ShinyHierarchy
#'
#' Create a hierarchical network chart that  shows the _ad hoc_ structure of your shiny Server.
#'
#'
#' @param directory the directory or website containing the files for the Shiny App. Defaults to current working directory
#' @param ui a character vector size 1 containing the name of the UI files. defaults to "ui.R"
#' @param server a character vector size 1 containing the names of the SERVER file. defaults to "server.R"
#'
#'
#' @return It returns a very very nice network chart
#' @examples
#' ShinyHierarchy(directory = "https://raw.githubusercontent.com/mexindian/ShinyServer/master/LineSelector")
#'
#' ## Or, to test with your own app, go to your shiny app, make that your working directory, and then type `ShinyHierarchy()`

ShinyHierarchy <- function(directory=getwd(),ui="ui.R",server="server.R"){
  library(stringr)
  library(readr)
  library(tidyverse)
  library(visNetwork)

  ## Get input again
  a <- read_file(paste(directory,"/",server,sep=""))
  b <- gsub("\r?\n","",a)
  ## Identify code chunks, basically each little shiny minifunction
  Chunks <- str_extract_all(b, "[a-zA-Z0-9]+ *\\<\\- *[a-zA-Z0-9]+?\\(\\{.+?\\}\\)",
                            simplify = F) %>% .[[1]]

  ## Define function that looks for some text into the Chunks
  StringFinder <- function(stringToFind){
    df <- map(Chunks,str_extract_all,stringToFind)
    df <- map(df,as.character)
    df <- df %>% as.character()
    df <-  df %>% as.matrix %>% as.data.frame(stringsAsFactors=F)
    df$V1 <- gsub('c\\(|character\\(0|\\"|\\)','',df$V1)
    df
  }

  InputsinChunks <- StringFinder("input.[a-zA-Z]+")

  ## Get inputs read into each chunk
  # InputsinChunks <- map(Chunks,str_extract_all,"input.[a-zA-Z]+")
  # InputsinChunks <- map(InputsinChunks,as.character)
  # InputsinChunks <- InputsinChunks %>% as.character()
  # InputsinChunks <-  InputsinChunks %>% as.matrix %>% as.data.frame(stringsAsFactors=F)
  # InputsinChunks$V1 <- gsub('c\\(|character\\(0|\\"|\\)','',InputsinChunks$V1)

  ## Now add in the chunk name
  InputsinChunks$name <- gsub("\\(.+","",Chunks)

  ## And make into a tidy df
  InputsDF <- InputsinChunks %>%
    mutate(V1 = strsplit(V1,",")) %>%
    unnest(V1)

  InputsDF <- InputsDF %>%
    separate(name,c("ChunkName","ChunkType"),sep = "  *\\<\\- *")

  ## Now remove input from the "Input" from input type, but add it as a seperate column
  InputsDF$V1 <- gsub(".+\\$","",InputsDF$V1)
  names(InputsDF)[3] <- "Input"
  InputsDF$InputType <- "uiInput"

  ## OK! Now grab reactives that are inputs to other Chunks ####

  ReactiveInputs <- InputsDF %>% filter(ChunkType=="reactive") %>% select(ChunkName) %>% unique %>% .$ChunkName
  ReactiveInputs <- paste(ReactiveInputs,"\\(\\)",sep="")

  ReactivesInChunks <- map_df(ReactiveInputs,StringFinder)

  ## Now add in the chunk name
  ReactivesInChunks$name <- gsub("\\(.+","",Chunks)

  ## And remove blanks
  ReactivesDF <- ReactivesInChunks %>%
    filter(V1 != "")

  ## And split up name
  ReactivesDF <- ReactivesDF %>%
    separate(name,c("ChunkName","ChunkType"),sep = "  *\\<\\- *")

  ## Now clean up df
  ReactivesDF$V1 <- gsub("\\(","",ReactivesDF$V1)
  names(ReactivesDF)[1] <- "Input"
  ReactivesDF$InputType <- "reactive"

  ## And create final df
  BofDF <- bind_rows(InputsDF,ReactivesDF)

  ## Now start processing data for a network chart ####

  ## First, create nodes:
  nodes <- bind_rows(setNames(BofDF[,1:2],c("Thingie","group")),
                     setNames(BofDF[,3:4],c("Thingie","group"))) %>% unique

  ## And assign node properties:
  nodes$id <- 1:nrow(nodes)
  nodes$label <- nodes$Thingie

  ## ... including setting the level. Set 1 for inputs, 2 for mid and 3 for outputs... but
  ## since the reactives feed into reactives, it doesn't look good to have all reactives be lvl 2
  ## therefore add small small noise to layer 2 to bettter see dependencies

  nodes$level=1
  nodes$level[nodes$group=="reactive"] <- 2 + runif(sum(nodes$group=="reactive"),min = -.5,max = .5)
  nodes$level[grep("render",nodes$group)] <- 3

  ## Now edges
  edges <- data.frame(to=nodes$id[match(x = BofDF$ChunkName,table = nodes$Thingie)],
                      from=nodes$id[match(x = BofDF$Input,table = nodes$Thingie)])

  visNetwork(nodes,edges) %>% visEdges(arrows = 'to') %>%
    visLegend() %>% visHierarchicalLayout()

}

