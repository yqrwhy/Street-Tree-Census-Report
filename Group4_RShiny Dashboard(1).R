library(shiny)

# import dataset
tree <- read.csv(file.choose())

problem <- tree[16:24]

len <- nrow(problem)
root <- rep("Have not", len)
trunk <- rep("Have not", len)
branch <- rep("Have not", len)

for (i in 1:9){
  prob <- problem[,i]
  for (n in 1:len){
    if (prob[n] == "Yes"){
      if (i <= 3){
        root[n] <- "Have"
      }else if (i <= 6){
        trunk[n] <- "Have"
      }else{
        branch[n] <- "Have"
      }
    }
  }
}

tree1 <- data.frame(tree, root, trunk, branch)

# Define UI
ui <- fluidPage(
  
  titlePanel("2015 Street Tree Census in NYC"),
  
  sidebarLayout(  
   
     sidebarPanel(
       selectInput(inputId = "dataset",
                   label = "Choose a borough:",
                   choices = c("Citywide","Bronx","Brooklyn",
                               "Manhattan","Queens","Staten Island")),
       
       numericInput(inputId = "obs",
                    label = "Number of observations to view:",
                    value = 10),
       
       radioButtons('opts', '', choices = c('Plot', 'Table'), 
                    inline = T, selected='Plot')
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Top N Species", 
                 conditionalPanel(
                   condition = 'input.opts =="Plot"',
                   plotOutput('plot1')
                 ),
                 conditionalPanel(
                   condition = 'input.opts =="Table"',
                   tableOutput("table1")
                   )
                 ),
        tabPanel("Top N Damaged Streets", 
                 conditionalPanel(
                   condition = 'input.opts =="Plot"',
                   plotOutput('plot2')
                 ),
                 conditionalPanel(
                   condition = 'input.opts =="Table"',
                   tableOutput("table2")
                   )
                 ),
        tabPanel("Tree Condition of Top N Species", 
                 conditionalPanel(
                   condition = 'input.opts =="Plot"',
                   plotOutput('plot3')
                 ),
                 conditionalPanel(
                   condition = 'input.opts =="Table"',
                   tableOutput("table3")
                 )
        ),
        tabPanel("Tree Problems", plotOutput("plot4"))
      )
    )
  )
)

#Define server
server <- function(input, output){
  datasetInput <- reactive({
    switch(input$dataset,
           "Citywide" = tree1,
           "Bronx" = tree1[which(tree1$borough == "Bronx"),],
           "Brooklyn" = tree1[which(tree1$borough == "Brooklyn"),],
           "Manhattan"= tree1[which(tree1$borough == "Manhattan"),],
           "Queens"= tree1[which(tree1$borough == "Queens"),],
           "Staten Island"= tree1[which(tree1$borough == "Staten Island"),])
  })
  
  output$plot1 <- renderPlot({
    dataset <- datasetInput()
    
    # remove unclassified trees
    dataset <- dataset[-which(dataset$spc_common == ""),]
    
    # get borough
    if (nrow(table(dataset$borough)) == 1){
      borough <- dataset$borough[1]
    }else{
      borough <- "Citywide"
    }
    
    # order the levels of spc_common with the frequency of each level
    spc_freq <- data.frame(table(dataset$spc_common))
    spc_freq <- spc_freq[order(-spc_freq$Freq),]
    dataset$spc_common <- factor(dataset$spc_common, levels = spc_freq$Var1)
    
    data1 <- table(dataset$spc_common)

    # bar plot
    opar <- par(no.readonly=TRUE)
    par(las = 2, mar = c(8,14,6,4), cex = 1.2)
    barplot(data1[input$obs:1], horiz = TRUE, 
            xaxt = "n", border = NA, space = 0.6,
            main = paste0(borough," Top ", input$obs, " Species"),
            col= "#90C463")
    par(opar)
    
    },
    
    width = 700,
    height = 800
    )
  
  output$table1 <- renderTable({ 
    dataset <- datasetInput()
    
    # remove unclassified trees
    dataset <- dataset[-which(dataset$spc_common == ""),]
    
    # order the levels of spc_common with the frequency of each level
    spc_freq <- data.frame(table(dataset$spc_common))
    spc_freq <- spc_freq[order(-spc_freq$Freq),]
    dataset$spc_common <- factor(dataset$spc_common, levels = spc_freq$Var1)
    
    data1 <- data.frame(table(dataset$spc_common))
    colnames(data1) <- c("Species", "Counts")
    
    # Table
    data1[1:input$obs,]
    })

  output$plot2 <- renderPlot({
    dataset <- datasetInput()
    
    # get borough
    if (nrow(table(dataset$borough)) == 1){
      borough <- dataset$borough[1]
    }else{
      borough <- "Citywide"
    }
    
    # select all the observations with damaged sidewalk
    damage <- dataset[which(dataset$sidewalk == "Damage"),]
    
    # order the levels of address with the frequency of each level
    sidewalk_freq <- data.frame(table(damage$address))
    sidewalk_freq <- sidewalk_freq[order(-sidewalk_freq$Freq),]
    damage$address <- factor(damage$address, levels = sidewalk_freq$Var1)
    
    data3 <- table(damage$address)
    
    # set color
    if (input$obs > 6){
      color <- append(c("#761ED6", "#8743D1", "#935ECD", "#A07CC9", "#AB93C6",
                        "#B9B2C1"), rep("#BFBFC0", input$obs - 6))
    }else{
      color <- c("#761ED6", "#8743D1", "#935ECD", 
                 "#A07CC9", "#AB93C6","#B9B2C1")[1:input$obs]
    }
    
    # bar plot
    opar <- par(no.readonly=TRUE)
    par(mar = c(8,16,6,4), cex = 1.2)
    barplot(data3[input$obs:1], horiz = TRUE, las = 2,
            xaxt = "n", border = NA, space = 0.6,
            main = paste0(borough," Top ", input$obs, 
                          " Damaged Streets that Need to be Repaired!"),
            col = rev(color))
    mtext("Number of sidewalk damages immediately adjacent to tree.  ",
          side = 3, line = -0.4)
    par(opar)

  },
  
    width = 700,
    height = 800
  )
  
  output$table2 <- renderTable({ 
    dataset <- datasetInput()
    
    # select all the observations with damaged sidewalk
    damage <- dataset[which(dataset$sidewalk == "Damage"),]
    
    # order the levels of address with the frequency of each level
    sidewalk_freq <- data.frame(table(damage$address))
    sidewalk_freq <- sidewalk_freq[order(-sidewalk_freq$Freq),]
    damage$address <- factor(damage$address, levels = sidewalk_freq$Var1)
    
    data3 <- data.frame(table(damage$address))
    colnames(data3) <- c("Damaged Streets", "Counts")
    
    # Table
    data3[1:input$obs,]
  })
  
  output$plot3 <- renderPlot({
    dataset <- datasetInput()
    
    # remove unclassified trees
    dataset <- dataset[-which(dataset$spc_common == ""),]
    
    # get borough
    if (nrow(table(dataset$borough)) == 1){
      borough <- dataset$borough[1]
    }else{
      borough <- "Citywide"
    }
    
    # order the levels of spc_common with the frequency of each level
    spc_freq <- data.frame(table(dataset$spc_common))
    spc_freq <- spc_freq[order(-spc_freq$Freq),]
    dataset$spc_common <- factor(dataset$spc_common, levels = spc_freq$Var1)
    
    # set the levels of health
    dataset$health <- factor(dataset$health, levels = c("Good", "Fair", "Poor"))
    
    # Set % labels
    spc_freq$percentage <- paste0(spc_freq$Var1, " | ", 
                                  round(spc_freq$Freq * 100 / nrow(dataset),2),"%")
    
    data2 <- table(dataset$health, dataset$spc_common)
    
    # Transform this data in %
    data2_percentage <- apply(data2, 2, function(x){x*100/sum(x,na.rm=T)})
    
    # top 10 species
    opar <- par(no.readonly=TRUE)
    par(las = 2, mar = c(8,14,6,6.5), cex = 1.2)
    barplot(data2_percentage[1:3,input$obs:1], horiz = TRUE,
            xaxt = "n", border = NA, space = 0.6,
            main = paste0(borough," Tree Condition of Top ", input$obs, " Species (in proportion)"),
            legend = TRUE, 
            args.legend = list(x = "top", inset = -0.03, bty = "n", ncol = 3),
            names.arg = c(spc_freq$percentage[input$obs:1]),
            col = c("#8CC45B", "#CBE4B3", "#E7AB21"))
    par(opar)
    },
    width = 800,
    height = 800
  )
  
  output$table3 <- renderTable({
    dataset <- datasetInput()
    
    # remove unclassified trees
    dataset <- dataset[-which(dataset$spc_common == ""),]
    
    # order the levels of spc_common with the frequency of each level
    spc_freq <- data.frame(table(dataset$spc_common))
    spc_freq <- spc_freq[order(-spc_freq$Freq),]
    dataset$spc_common <- factor(dataset$spc_common, levels = spc_freq$Var1)
    
    # set the levels of health
    dataset$health <- factor(dataset$health, levels = c("Good", "Fair", "Poor"))
    
    # Set % labels
    spc_freq$percentage <- paste0(spc_freq$Var1, " | ", 
                                  round(spc_freq$Freq * 100 / nrow(dataset),2),"%")
    
    data2 <- table(dataset$health, dataset$spc_common)
    
    # Transform this data in %
    data_percentage <- apply(data2, 2, function(x){x*100/sum(x,na.rm=T)})
    
    data_percentage <- round(t(data_percentage),2)
    
    for (i in 1:3){
      for (n in 1:nrow(data_percentage)){
        data_percentage[n,i] <- paste0(as.character(data_percentage[,i][n]), "%")
      }
    }
    
    data_percentage <- data.frame(Species = spc_freq$percentage, data_percentage)

    # Table
    data_percentage[1:input$obs,]
  })
  
  output$plot4 <- renderPlot({
    dataset <- datasetInput()
    
    # get borough
    if (nrow(table(dataset$borough)) == 1){
      borough <- dataset$borough[1]
    }else{
      borough <- "Citywide"
    }

    data4 <- cbind(table(dataset$root), table(dataset$trunk), table(dataset$branch))
    colnames(data4) <- c("Root problem", "Trunk problem", "Branch problem")
    
    opar <- par(no.readonly=TRUE)
    options(scipen=200)
    par(las = 1, mar = c(4,8,6,4), cex = 1.2)
    
    barplot(data4[1:2, 1:3],
            border = NA, space = 0.6,
            main = paste0(borough, ": How many trees have root, trunk, and branch problems?"),
            legend = TRUE, 
            args.legend = list(x = "topright", inset = c(-0.153, -0.055), bty = "n"),
            col = c("#D52B1E", "#BFBFC0"))
    
    # text(data4[2:1, 1:3], labels = as.character(data4[2:1, 1:3]), col = "#D52B1E")
    par(opar)
    },
    
    width = 700,
    height = 400
  
  )
}

shinyApp(ui, server)

