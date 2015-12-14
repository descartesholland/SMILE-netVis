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
          column(4, numericInput('numHits', 'Max number of similarities', 50, min=1))
      ),
      forceNetworkOutput('network'),
      tableOutput('links')
#       fluidRow(
#         # h3(textOutput("nodes")),
#         
       # )
    ),
    
    server = function(input, output, session) {
      
      dec <- function(i) {
        return(as.numeric(i)-1)
      }
      
      inputtedNumbers <- reactive({ unlist(strsplit(input$startLine, ',', fixed=TRUE)) })
      
      generator <- reactive({
        group <- 1
        nodes <<- matrix(nrow=0, ncol = 2)
        nodes <<- data.frame(nodes)
        colnames(nodes) <- list("name", "group")
        nodes <<- nodes[-c(1), ]
        
        links <<- matrix(data=NA, ncol = 3)
        colnames(links) <- list("id1", "id2", "Score")
        
        
        counter <- 0
        
        drugIDs <- inputtedNumbers()
        print(drugIDs)
        
        for(i in 1:length(drugIDs)) {
          file <- paste("E:\\results\\", drugIDs[i], ".txt", sep="")
          # if(counter < 35) {
            data <- read.table(file, header=TRUE, sep="|", comment.char="")
            data$SMILE1 <- NULL
            data$SMILE2 <- NULL
            
            nodes[nrow(nodes)+1, ] <- c(strsplit(drugIDs[i], ".txt")[[1]], group)
            # nodes <<- rbind(nodes, c(strsplit(file, ".txt")[[1]], group))
            
            # group <- group+1
            
            for(ii in 1:dim(data)[1]) {
              if(ii < 300) {
                
                nodes <- rbind(nodes, c(data[ii, 2], group))
                nodes <- nodes[match(unique(nodes$name), nodes$name),]
                # print(c(which(nodes$name == data[ii, 1])[1]-1, which(nodes$name == data[ii, 2])[1]-1, data[ii, 3]))
                links <- rbind(links, c(which(nodes$name == data[ii, 1])[1]-1, which(nodes$name == data[ii, 2])[1]-1, data[ii, 3]))
                
                #group<- group+1
              }
            }
            
            counter <- counter + 1
            print(counter)
          # }
        }
        
        links <- data.frame(links)
        links <- links[-c(1), ]
        
        write.table(links, file="links.txt", append=FALSE, row.names=TRUE, col.names=TRUE)
        write.table(nodes, file="nodes.txt", append=FALSE, row.names=TRUE, col.names=TRUE)
        
        forceNetwork(Links = links, Nodes = nodes, Source = "id1", Target="id2", Value = "Score", NodeID = "name", Group = "group", opacity=0.8, zoom=TRUE) #%>%
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