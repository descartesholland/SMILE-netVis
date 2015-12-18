library(networkD3)
library(magrittr)
library(shiny)

network_vis <- function(dataset) {
  shinyApp(
    ui = fluidPage(
      fluidRow(style = "padding-bottom: 20px;",
          column(4, textInput('startLine', 'Line Numbers (Comma Separated)')),
          column(4, radioButtons("indexIdentifier", label = "",
                                 choices = list("Line Index (from 1)" = 1, "Drug IDs" = 2), selected = 1)),
          column(4, numericInput('numHits', 'Max number of similarities', 15, min=1)),
          column(4, radioButtons('dupesEnabled', label = 'Render Duplicate Links', 
                                 choices = list("Yes" = 1, "No" = 2), selected = 2))
      ),
      forceNetworkOutput('network'),
      tableOutput('links')
    ),
    
    server = function(input, output, session) {
      
      dec <- function(i) {
        return(as.numeric(i)-1)
      }
      
      inputtedNumbers <- reactive({ unlist(strsplit(input$startLine, ',', fixed=TRUE)) })
      maxSimilarities <- reactive({ as.numeric(input$numHits) })
      generator <- reactive({
        group <- 1
        nodes <<- matrix(nrow=0, ncol = 2)
        nodes <<- data.frame(nodes)
        colnames(nodes) <- list("name", "group")
        nodes <<- nodes[-c(1), ]
        
        masterLinks <<- matrix(data=NA, ncol = 3)
        colnames(masterLinks) <- c("id1", "id2", "Score")
        # links <- data.frame(links)
        masterLinks <- masterLinks[-c(1), ]
        
        
        counter <- 0
        
        drugIDs <- inputtedNumbers()
        print(drugIDs)
        
        for(i in 1:length(drugIDs)) {
          file <- paste("E:\\results\\", drugIDs[i], ".txt", sep="")
            data <- read.table(file, header=TRUE, sep="|", comment.char="")
            data$SMILE1 <- NULL
            data$SMILE2 <- NULL
            data <- data[order(-data[ , "Score"] ), ] #for most similar nodes
            
            nodes[nrow(nodes)+1, ] <- c(strsplit(drugIDs[i], ".txt")[[1]], group)
             
            nodes <- rbind(nodes, setNames(cbind(data.frame(data[1:maxSimilarities(), ]$id2, rep.int(1, maxSimilarities()))), c("name", "group")))
            nodes <- nodes[match(unique(nodes$name), nodes$name),]
            rownames(nodes) <- NULL
            print(nodes)                 
                  
                generateLinks <- function(dataRow) {
                  return(setNames(c(which(nodes$name == dataRow[1])[1]-1, which(nodes$name == dataRow[2])[1]-1, dataRow[3]), c("id1", "id2", "Score")))
                }
                
            links <- data.frame(data=t(apply(data[1:maxSimilarities(),], 1, generateLinks)))
            colnames(links) <- list("id1", "id2", "Score")
            rownames(links) <- NULL
            
            if(input$dupesEnabled == 2) {
              links <- links[!duplicated(links[, c('id1', 'id2')]),]
            }
            masterLinks <- rbind(masterLinks, links)
            print("masterLinks")
            print(masterLinks)
            # print (links)
            counter <- counter + 1
            print(counter)
        }
        
        
        
        write.table(masterLinks, file="links.txt", append=FALSE, row.names=TRUE, col.names=TRUE)
        write.table(nodes, file="nodes.txt", append=FALSE, row.names=TRUE, col.names=TRUE)
        
        print('final')
        print(nodes)
        print(masterLinks)
        forceNetwork(Links = data.frame(masterLinks), Nodes = nodes, Source = "id1", Target="id2", Value = "Score", NodeID = "name", Group = "group", opacity=0.8, zoom=TRUE) #%>%
      })
      
      
#       selectedNodes <- reactive({
#         nodesDF <- data.frame(read.table("nodes.txt", col.names = c("name", "group")))
#         
#         if(input$indexIdentifier == 1) # line indices inputted
#           intermediaryNodes <- nodesDF[row.names(nodesDF) %in% unlist(inputtedNumbers()), ] 
#         else #drug IDs inputted
#           intermediaryNodes <- nodesDF[nodesDF$name %in% unlist(inputtedNumbers()), ]
#         
#         # row.names(intermediaryNodes) <- NULL
#         intermediaryNodes
#       })
#       
#       #Converts a 0-based (javascript) node index to it's current index
#       #  by looking up [the old index + 1] in the names of the selected rows
#       oldLineIndexToNew <- reactive({
#         function(lineIndex) {
#           selNodes <- selectedNodes()
#           return(match(row.names(selNodes), as.numeric(lineIndex) + 1))
#         }
#       })
#       
#       selectedLinks <- reactive({ 
#         # if(input$indexIdentifier == 1) #line indicies inputted
#         # linksDF[linksDF$id1 %in% unlist(lapply( rownames(selectedNodes()),  dec)), ]
#         linksDF <- data.frame(read.table("links.txt", col.names=c("id1", "id2", "Score")))
#         
#         })
      
      # output$links <- renderTable({linksDF})
      # output$nodes <- renderTable({nodesDF})
      
      output$network <- renderForceNetwork({
        generator()
      })
    }
  )
}