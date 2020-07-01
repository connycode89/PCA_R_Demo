library(shiny)
library(stats)
library(calibrate)
library(reporttools)
library(MASS)
library(matlib)


shinyServer(function(input, output) {
  
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath, header=input$header, sep=input$sep, 
                    quote=input$quote)
  })
  
  meancent <- reactive({
    df <- filedata()
    apply(df, 2, scale, scale=FALSE, center=TRUE) 
    })
  
  stand <- reactive({
    df <- meancent()
    apply(df, 2, scale, center = FALSE, scale =  TRUE) # uses sample standard dev. rather than population
  })
  
  covar <- reactive({
    df <- stand()
    cov(df)
  })
  
  eig <- reactive({
    df <- covar()
    eigen(df)
  })
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    #inFile <- input$file1
    
    #if (is.null(inFile))
     # return(NULL)
    
    #df <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
      #       quote=input$quote)
    df <- filedata()

  })
  
  output$meancentered <- renderTable({
    df <- meancent()
  })
  
  output$stand <- renderTable({
   df <- stand()
  })
  
  output$plot1 <- renderPlot({
    df <- filedata()
    x <- df[,1]
    y <- df[,2]
    plot(x, y, xlab=colnames(df)[1], ylab=colnames(df)[2],cex=1.5,pch=19)#,labels=1:5)
    textxy(x, y, labs=seq(1:length(x)), m = c(0, 0), cex = 0.8, offset = 0.8, col="dark green")
  })
  
  output$plot2 <- renderPlot({
    df <- stand()
    x <- df[,1]
    y <- df[,2]
    plot(x, y, xlab=colnames(df)[1], ylab=colnames(df)[2],cex=1.5,pch=19)
    textxy(x, y, labs=seq(1:length(x)), m = c(0, 0), cex = 0.8, offset = 0.8, col="dark green")
  })
  
  output$covari <- renderTable({
    df <- covar()
    df
  },caption="Covariance Matrix",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  #output$eigenv1 <- renderPrint({
   # df <- eigen(covar())
    #df$values
  #})
  
  output$eigenv1 <- renderTable({
    df <- eigen(covar())
    #print(diag(df$values))
    #rbind(df$values,df$vector)
    #v1_1 <- as.character(df$vector[,1][1])
    #v1_2 <- as.character(df$vector[,1][2])
    #v2_1 <- as.character(df$vector[,2][1])
    #v2_2 <- as.character(df$vector[,2][2])
    #v1 <- paste("(",v1_1,",",v1_2,")",sep="")
    #v2 <- paste("(",v2_1,",",v2_2,")",sep="")
    lambdaValues <- c()
    lambdaLabels <- c()
    eigvecLabs <- c()
    for (i in seq(1:length(df$values))){
      lambdaLabels <- c(lambdaLabels, paste("Lambda",i, sep = ""))
      lambdaValues <- c(lambdaValues, df$values[i])
      eigvecLabs <- c(eigvecLabs, paste("Eigenvector for Lambda",i, sep = ""))
    }
    eigDF <- cbind(lambdaLabels,lambdaValues)
    colnames(eigDF) <- c("Lambdas","Eigenvalues")
    #lambda1 <- df$values[1]
    #lambda2 <- df$values[2]
    output$printeigs1 <- renderTable({
      eigDF
    })
    output$printeigs2 <- renderText({
      paste("Lambda2 =",disp(lambda2, d1=4))
    })
    #lambdas <- rbind(lambda1,lambda2)
    #eigenvectors <- rbind(v1,v2)
    #cbind(lambdas,eigenvectors)
    print(df$vector)
    colnames(df$vector) <- eigvecLabs
    df$vector
  },caption="Matrix of Eigenvectors",
  caption.placement = getOption("xtable.caption.placement", "top"))


  output$plot3 <- renderPlot({
    df <- stand()
    x <- df[,1]
    y <- df[,2]
    eig <- eigen(covar())
    v1 <- eig$vector[,1]
    v2 <- eig$vector[,2]
    plot(x, y, xlab=colnames(df)[1], ylab=colnames(df)[2], main="Standardized Data With Eigenvectors of Covariance Matrix", cex=1.5,pch=19)
    textxy(x, y, labs=seq(1:length(x)), m = c(0, 0), cex = 0.8, offset = 0.8, col="dark green")
    #points(x, cex = .5, col = "dark red")
    arrows(x0=0, y0=0, x1 = v1[1], y1 = v1[2],col="red")
    text(x=1.1*v1[1],y=1.1*v1[2],labels="V1")
    arrows(x0=0, y0=0, x1 = v2[1], y1 = v2[2],col="blue")
    text(x=1.1*v2[1],y=1.1*v2[2],labels="V2")
    })
  
  output$eigenvMat1 <- renderTable({
    eig <- eigen(covar())
    diag(eig$values)
    #print(diag(diag(eig$values)))
    #print(tr(diag(eig$values)))
  },caption="Diagonal Matrix of Eigenvalues on the Main Diagonal and Zeroes Elsewhere",
  caption.placement = getOption("xtable.caption.placement", "top"))

###Eigenvectors and eigenvalues are automatically sorted in descending order
  output$matofeigenvec <- renderTable({
    eig <- eigen(covar())
    eig$vectors
  },caption="Matrix of Eigenvectors (Loading Matrix)",
  caption.placement = getOption("xtable.caption.placement", "top"))

  output$inveigmat <- renderTable({
    eig <- eigen(covar())
    ginv(eig$vectors)
  },caption="Inverse Matrix of Matrix of Eigenvectors",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  output$screeplot <- renderPlot({
    eig <- eigen(covar())
    eigenvalues <- eig$values
    if (is.null(eigenvalues)) { 
      return(NULL)
    } else {
      
      # make.scree.plot(x)
      comp <- c("Component1","Component2")
      ScreePlot <- as.data.frame(sqrt(eigenvalues))
      colnames(ScreePlot) <- "sdev"
      ScreePlot <- cbind(ScreePlot,comp)
      #print(ScreePlot)
      splot <- screeplot(ScreePlot,type="lines") #,xlab="Components") #type="lines",
    }
  })	
  
  output$cumulprop <- renderTable({
    eig <- eigen(covar())
    CumulativeProportion <- c()
    for (i in seq(1:length(eig$values))){
      value <- sum(eig$values[1:i])/sum(eig$values)
      CumulativeProportion <- c(CumulativeProportion,value)
    }
    CumulativeProportion <- as.data.frame(CumulativeProportion)
    CumulativeProportion
  })
  
  output$inputcomp <- renderUI({
    eig <- eigen(covar())
    num <- length(eig$values)
    selectInput(inputId="numcomps",label<-"Choose a number of components to retain",choices=as.list(seq(1:num)),multiple=FALSE)
  })
  
  output$changedloadings <- renderTable({
    eig <- eigen(covar())
    loadings <- eig$vectors
    scaledData <- stand() # the original data scaled
    numberComps <-input$numcomps
    output$printText <- renderText({
      paste("The number of components retained are",disp(numberComps, d1=0),"and the new loading matrix for this is this",numberComps,"column matrix")
    })
    numberComps <- as.integer(numberComps)
    if(numberComps==1){
     df<-as.data.frame(loadings[,1:numberComps])
     colnames(df)<-c("PC1")
     df}
    else{
      df<-loadings[,1:numberComps]
      listy <- c()
      for (n in 1:numberComps){
        listy <- c(listy,paste("PC",n,sep=""))
      }
      colnames(df)<-listy
      df
    }
  })
  
  output$changedscores <- renderTable({
    eig <- eigen(covar())
    loadings <- eig$vectors
    scaledData <- stand() # the original data scaled
    numberComps <- input$numcomps
    output$printText2 <- renderText({
      paste("The score matrix for",disp(numberComps, d1=0)," retained components is:")
    })
    numberComps <- as.integer(numberComps)
    if(numberComps==1){
      loadings <- as.data.frame(loadings[,1:numberComps])}
    else{
      loadings <- loadings[,1:numberComps]
    }
    #scaledData%*%loadings
    if(numberComps==1){
      df <- scaledData%*%as.matrix(loadings)
      colnames(df) <- c("PC1")
      df
    }
    else{
      df <- scaledData%*%loadings
      listy <- c()
      for (n in 1:numberComps){
        listy <- c(listy,paste("PC",n,sep=""))
      }
      colnames(df) <- listy
      df
    }
    
  })
  
  output$plot4 <- renderPlot({           #scores plot
    
    eig <- eigen(covar())
    loadings <- eig$vectors
    scaledData <- stand() # the original data scaled
    numberComps <- input$numcomps
    output$printText2 <- renderText({
      paste("The score matrix for",disp(numberComps, d1=0)," retained components is:")
    })
    numberComps <- as.integer(numberComps)
    if(numberComps==1){
      loadings <- as.data.frame(loadings[,1:numberComps])}
    else{
      loadings <- loadings[,1:numberComps]
    }
    #scaledData%*%loadings
    if(numberComps==1){
      df <- scaledData%*%as.matrix(loadings)
    }
    else{
      df <- scaledData%*%loadings
    }
    
    if (dim(df)[2]==1){
      stripchart(df[,1], cex=1.5, pch=19)
      text(df[,1], 1.04, labels=seq(1:dim(df)[1]), m = c(0, 0), cex = 0.8, offset = 0.8, col="dark green")
      #textxy(X=df[,1], Y=NULL, labs=seq(1:5), m = c(0, 0), cex = 0.8, offset = 0.8, col="dark green")
    #  labels(seq(1:5))
    }
    else{
      plot(df,cex=1.5,pch=19)
      textxy(df[,1],df[,2], labs=seq(1:dim(df)[1]), m = c(0, 0), cex = 0.8, offset = 0.8, col="dark green")
    }
  })
  
  output$plot5 <- renderPlot({  # Loading plot
    eig <- eigen(covar())
    loadings <- eig$vectors
    scaledData <- stand() # the original data scaled
    numberComps <- input$numcomps
    output$printText2 <- renderText({
      paste("The score matrix for",disp(numberComps, d1=0)," retained components is:")
    })
    numberComps <- as.integer(numberComps)
    if(numberComps==1){
      loadings <- as.data.frame(loadings[,1:numberComps])}
    else{
      loadings <- loadings[,1:numberComps]
    }
    
    #print(loadings[1,])
    if (dim(loadings)[2]==1){
      print("1")
      #stripchart(loadings[1,], cex=1.5, pch=19)
      #stripchart(loadings[2,], cex=1.5, pch=19)
      #arrows(x0=0, x1 = loadings[1,], y0=1, y1=1, col="red")
      #arrows(x0=0, x1 = loadings[1,], y0=1, y1=1, col="blue")
      #text(loadings[,1], 1.05, labels=c("X1"), m = c(0, 0), cex = 0.8, offset = 0.8, col="dark green")
      #textxy(X=df[,1], Y=NULL, labs=seq(1:5), m = c(0, 0), cex = 0.8, offset = 0.8, col="dark green")
       # labels(seq(1:5))
    }
    else{
      plot(loadings,cex=1.5,pch=19, xlim = c(0,1), ylim=c(-1,1))
      arrows(x0=0, y0=0, x1 = loadings[2,1], y1 = loadings[1,1],col="red")
      arrows(x0=0, y0=0, x1 = loadings[2,2], y1 = loadings[1,2],col="blue")
      vars <- colnames(filedata())
      textxy(loadings[1,],loadings[2,], labs=vars, m = c(0, 0), cex = 0.8, offset = 0.8, col="dark green")
    }
    
  })
  
  output$biplot <- renderPlot({
    eig <- eigen(covar())
    loadings <- eig$vectors
    scaledData <- stand() # the original data scaled
    numberComps <- input$numcomps
    output$printText55 <- renderText({
      paste("The below is the biplot showing scores and loadings together for the specified number of retained PCs:")
    })
    #scaledData%*%loadings
    numberComps <- as.integer(numberComps)
    if(numberComps==1){
      loadings <- as.data.frame(loadings[,1:numberComps])
      rownames(loadings) <- colnames(filedata())
      }
    else{
      loadings <- loadings[,1:numberComps]
      rownames(loadings) <- colnames(filedata())
    }
    if(numberComps==1){
      df <- scaledData%*%as.matrix(loadings)
      colnames(df) <- c("PC1")
      print(df)}
    
    else{
      df <- scaledData%*%loadings
      listy <- c()
      for (n in 1:numberComps){
        listy <- c(listy,paste("PC",n,sep=""))
      }
      colnames(df) <- listy
    }
    
    print(dim(loadings)[2])  
    #print(dim(loadings)[2])
    #print(filedata())
    if(dim(loadings)[2]>=2){
    biplot(df,loadings)}
    if(dim(loadings)[2]==1){
      stripchart(df[,1], pch=19,frame=TRUE,xlab="PC1")
      text(df[,1], 1.1, labels=seq(1:length(df[,1])),cex=0.8,col="dark green")
      arrows(x0=0, y0=1.005, x1 = loadings[1,], y1 = 1.005,col="red")
      text(x=1.1*loadings[1,],y=1.03,labels=colnames(filedata())[1])
      arrows(x0=0, y0=1, x1 = loadings[2,], y1 = 1,col="blue")
      text(x=1.1*loadings[2,],y=0.97,labels=colnames(filedata())[2])
      }
    
  })
  
})


