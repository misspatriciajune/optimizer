library(shiny)



shinyServer( function(input, output, session) {
 
  
  data <- reactiveValues(isset=FALSE,full=FALSE,checker=FALSE,counter=1,mincounter=1,emptymatrix=matrix(0),emptymin=matrix(0), minmatrix=matrix(0))
  
  
  output$vars <- renderUI({
    data$nVars <- as.numeric(input$assets)
    data$nCons <- as.numeric(input$constraints)
      lapply(1:(data$nVars), function(i) {
        list(tags$p(tags$u(h4(paste0("x ", i )))),
             textInput(paste0("vars", i), label = " ")
        )
      }) 
  }) # end of renderUI
  
  output$answer <- renderUI({
    numAssets <- as.integer(input$assets)
    for (j in 1:(numAssets)){
      if(j==1){x=100}
      x = x - input[[paste0("answer",j)]]
    }
    tagList(
      tags$p(tags$u(h4(paste0("Answer")))),
      textInput(paste0("variantFinal"), label = " ", value = paste0(" ")), 
      helpText(paste0(x))
    ) #end of tagList
  }) #end of renderUI

  
   
    observeEvent(input$OkButton, {  #save values
      if(!data$isset){
        data$isset = TRUE

        columnnames <- c()
        rownames <- c()
        for(i in 1:input$assets){
          columnnames <- c(columnnames,paste("x",i))          
        }
        for(i in 1:input$constraints){
          columnnames <- c(columnnames,paste("S",i))          
          rownames <- c(rownames,paste("S",i))
        }
        columnnames <- c(columnnames,"Z","Solution","TR")
        rownames <- c(rownames,"Z")        
        
        #maximization matrix
        data$emptymatrix <- matrix(0, nrow=input$constraints + 1, ncol=(as.numeric(input$assets)+input$constraints)+3, byrow=TRUE)  

        dimnames(data$emptymatrix) <- list(rownames,columnnames)
        #minimization matrix
        data$emptymin <- matrix(0, nrow=input$assets+1, ncol=(as.numeric(input$assets)+input$constraints)+3, byrow=TRUE)
        data$minmatrix <- matrix(0, nrow=input$constraints + 1, ncol= input$assets + 1, byrow=TRUE)
        
       }
      
      
        for(i in 1:as.integer(input$assets)){
          
          if(input$opt == "Minimization"){ #transpose values

            #insertion of coefficients
            data$minmatrix[i,data$counter] = as.numeric(input[[paste0("vars", i)]])
            #insertion of answer
            data$minmatrix[nrow(data$emptymin),data$counter] = as.numeric(input$variantFinal) #answer
            
          }
          
         
          if(input$opt == "Maximization"){  #insertion of coefficients
            data$emptymatrix[data$counter, i] = as.numeric(input[[paste0("vars", i)]])
            data$emptymatrix[data$counter, data$counter+as.integer(input$assets)] = 1 #slack
          }
          
        }
      
        if(input$opt == "Minimization"){ #big loop
          print(data$minmatrix)
         # print(data$emptymin)
          data$counter = data$counter + 1
        
          for(i in 1:(ncol(data$minmatrix))){ #add to empty matrix
            data$emptymin[,i] = data$minmatrix[,i]
            #data$emptymin[data$counter-1, data$counter+as.integer(input$assets)] = 1 
            
          }
          print(data$emptymin)   
          
          for(i in 1:(ncol(data$emptymin))-3){  #multiply last row to -1 
              data$emptymin[nrow(data$emptymin),i] = data$emptymin[nrow(data$emptymin),i] *-1
            } 
          print(data$emptymin)
          
          
          
        }#end big loop min
      
      
        if(input$opt == "Maximization"){#big loop
         # if(input$opt == "Maximization"){
            data$emptymatrix[data$counter, ncol(data$emptymatrix)-1] = as.numeric(input$variantFinal) #answer
         # }
          print(data$counter)
          print(input$constraints)
          data$counter = data$counter + 1
          
          
        #  if(input$opt == "Maximization") { #Negate last row
            for(i in 1:(ncol(data$emptymatrix)-3)){  #multiply last row to -1 
              data$emptymatrix[nrow(data$emptymatrix),i] = data$emptymatrix[nrow(data$emptymatrix),i] *-1
            } 
        #  } 
          
          
          #Check if matrix is already full
          if(data$counter == (input$constraints+2)){ 
            data$full = TRUE
          }
          
          
          print(data$emptymatrix)
          mat <- data$emptymatrix
          
          if(data$full == TRUE){
            write.csv(mat,file="Initial_Tableau.csv") #print values in csv
            while(min((mat[nrow(mat),1:(ncol(mat)-2)])) < 0){
              #Look for the negative number with the largest magnitude
              #returns the index of the largest negative
              #this will get the pivot column
              pivotcol = which.min(mat[nrow(mat),1:(ncol(mat)-2)])
              
              cat("pcol is ",pivotcol,"\n")
              
              #Solve for the Test Ratio
              for(i in 1:(nrow(mat))){
                if(mat[i,pivotcol] == 0){
                  mat[i,ncol(mat)] = 0
                }else{
                  mat[i,ncol(mat)] = mat[i,ncol(mat)-1]/mat[i,pivotcol]
                }
              }
              
              print(mat)
              
              #Look for the smallest positive number in the Test Ratio
              #returns the index of the smallest positive
              #this will get the pivot row
              x = mat[,ncol(mat)]
              pivotrow = which(x %in% min(x[x>0])) 
              cat("prow is ",pivotrow,"\n")
              
              #normalize pivot row
              mat[pivotrow,1:(ncol(mat)-1)] = mat[pivotrow,1:(ncol(mat)-1)]/mat[pivotrow,pivotcol]
              print("After normalizing pivot row")
              print(mat)
              #Pivot Element
              pivotelement = mat[pivotrow,pivotcol]
              #GJ
              for(i in 1:(nrow(mat))){
                if(i!=pivotrow){
                  mat[i,1:(ncol(mat)-1)] = mat[i,1:(ncol(mat)-1)] - (mat[i,pivotcol]/pivotelement)*mat[pivotrow,1:(ncol(mat)-1)]
                }
              }
              
              print(mat)
              write.csv(mat,file="Final_Tableau.csv") #print values in csv
              
              
            }
            #Printing final basic solution
            for(i in 1:nrow(mat)){ 
              for(j in 1:(ncol(mat)-1)){
                if(mat[i,j]==1) {
                  cat(colnames(mat)[j]," = ",mat[i,(ncol(mat)-1)],"\n")
                
                }
              }
            }
            
            
          }
        }#end big loop max
       
       
    })#End of OK Button

      
  
}) #end of shinyServer