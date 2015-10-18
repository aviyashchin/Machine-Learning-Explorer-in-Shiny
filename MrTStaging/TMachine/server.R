require(shiny);
require(caret);
require(e1071);
require(randomForest);
require(nnet);
require(glmnet);
library(mice);
library(VIM);
require(fastICA);
library("PASWR");
source("helpers.R")

#code for plotting nnets, taken from: http://beckmw.wordpress.com/2013/03/04/visualizing-neural-networks-from-the-nnet-package/
# the formula of the output of train is off and doesn't work correctly though.
#Function for plotting nnets, not working right now
# require(RCurl);
# require(scales);
# 
# root.url<-'https://gist.github.com/fawda123'
# raw.fun<-paste(
#   root.url,
#   '5086859/raw/17fd6d2adec4dbcf5ce750cbd1f3e0f4be9d8b19/nnet_plot_fun.r',
#   sep='/'
# )
# script<-getURL(raw.fun, ssl.verifypeer = FALSE);
# eval(parse(text = script));
# rm('script','raw.fun');

shinyServer(function(input,output,session)
{
  
  #reactive object, responsible for loading the main data
  rawInputData = reactive({
    rawData = input$rawInputFile
    headerTag = input$headerUI;
    sepTag = input$sepUI;
    quoteTag = input$quoteUI;
    
    
    if(!is.null(rawData)) {
      data = read.csv(rawData$datapath,header=headerTag,sep=sepTag,quote=quoteTag);
    } else {
      return(NULL);
    }
    
  });
  
  #responsible for building the model, responds to the button
  #REQUIRED, as the panel that holds the result is hidden and trainResults will not react to it, this one will  
  output$dummyTagUI = renderUI({
    dataInput = trainResults()
    if(is.null(dataInput))
      return();
    activeTab = updateTabsetPanel(session,"mainTabUI",selected="Model Results View");
    return();
  })
  
########### Joe Added Functions ##############
  output$textmissing <- renderText({ 
    
    data = rawInputData()
    
    missing = sapply(data, function(x) sum(is.na(x)))
    df.missing = data.frame(missing)
    total = sum(df.missing$missing)
    total
    
    paste("Number of Missing Values: ",total)
    
  })
  
  
  output$colmissing <- renderDataTable({ 
    data = rawInputData()
    missing = sapply(data, function(x) sum(is.na(x)))
    frame.missing = data.frame(missing)
    observation = rownames(frame.missing)
    df.missing = cbind(observation, frame.missing)
    df.missing
    
  })
  
  output$pre.data <- renderDataTable({ 
    data = rawInputData()
    df.data = data.frame(data)
    df.data
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
###############################################
  
  
  
  
  
  
  
  
  #this is the function that responds to the clicking of the button
  trainResults = eventReactive(input$runAnalysisUI,{
#     #respond to the button
#     input$runAnalysisUI;
    
    #the model we are interested in
    modelTag = isolate(input$modelSelectionUI);
    
    #make sure the data are loaded
    newData = isolate(rawInputData());
    if(is.null(newData))
      return();
    
    #grab the column
    column = isolate(input$modelLabelUI);
    
    columnElement = which(colnames(newData) == column);
    
    foldsType = isolate(input$crossFoldTypeUI);
    
    folds = isolate(input$foldsUI);
    
    control = trainControl(method=foldsType,number=folds)
    
    if(foldsType == "repeatedcv")
    {
      numberOfRepeats = isolate(input$repeatUI);
      control = trainControl(method=foldsType,number=folds,repeats=numberOfRepeats);
    }
    
    preprocessType = isolate(input$preprocessingUI);
    
    
    
    #build the equation
    form = as.formula(paste(column," ~ .",sep=""));
    
    kFolds = isolate(input$foldsUI);
    
    foldType = isolate(input$crossFoldTypeUI);
    
    if(preprocessType == "")
      preprocessType = NULL;
    
    results = NULL;
    
    results = withProgress(session, min=1, max=2, {
      setProgress(message = 'Calculation in progress',
                  detail = 'This may take a while...')
    
      setProgress(value = 1)
      
      
      
      #choose the view based on the model
      if(modelTag == "en")
      {
        
        alphaStart = isolate(input$enAlphaStartUI);
        alphaEnd = isolate(input$enAlphaEndUI);      
        alphaRange = isolate(input$enAlphaRangeUI);
        lambdaStart = isolate(input$enLambdaStartUI);
        lambdaEnd = isolate(input$enLambdaEndUI);      
        lambdaRange = isolate(input$enLambdaRangeUI);
        familyData = isolate(input$enModelTypeUI);
        gridding = expand.grid(.alpha=seq(alphaStart,alphaEnd,length.out=alphaRange),.lambda=seq(lambdaStart,lambdaEnd,length.out=lambdaRange));
        
        #create the equation
        
        results = train(form,data=newData,tuneGrid=gridding,method="glmnet",family=familyData,trControl=control,preProcess=preprocessType);
        
        return(results);
        
      } else if(modelTag == "rf") {
        
        mTryStartEnd = isolate(input$mTryRangeUI)
        
        nMtry = isolate(input$rmTryUI);
        
        
        # auto decides family
        if(length(unique(newData[column])) == 2){
          familyData <- "binomial" # For classification
        }else {
          familyData <- "gaussian" # For classification
        } #else {
        
        # familyData = isolate(input$rfModelTypeUI);
        
        gridding = expand.grid(.mtry=seq(mTryStartEnd[1],mTryStartEnd[2],by=nMtry));
        
        
        
        
        
        if(familyData != "Gaussian") {
          newData[,columnElement] = as.factor(newData[,columnElement]);
        } else {
          newData[,columnElement] = as.numeric(newData[,columnElement]);
        }
        
        
        results = train(form,data=newData,tuneGrid=gridding,method="rf",trControl=control,preProcess=preprocessType);
        return(results);
        
        
      } else if (modelTag == "nn") {
        
        familyData = isolate(input$nnModelTypeUI);
        nnRange = isolate(input$nnSizeUI);
        numNN = isolate(input$nnSizeRangeUI);
        nnDecayRange = isolate(input$nnDecayUI);
        numnnDecayRange = isolate(input$nnDecayRangeUI);
        
        gridding = expand.grid(.size=seq(nnRange[1],nnRange[2],length.out=numNN),.decay=seq(nnDecayRange[1],nnDecayRange[2],length.out=numnnDecayRange));
        
        
        if(familyData != "Gaussian") {
          newData[,columnElement] = as.factor(newData[,columnElement]);
        } else {
          newData[,columnElement] = as.numeric(newData[,columnElement]);
        }
        
        results = train(form,data=newData,tuneGrid=gridding,method="nnet",trControl=control,preProcess=preprocessType);
        return(results);
        
        
        
      }
      setProgress(value = 2);
    });
    
    return(results);
    
    
    
  })
  
  #responsible for displaying the full results
  output$trainResultsUI = renderTable({
    data = trainResults();
    if(is.null(data))
      return();
    data$results
  })
  
  #the one that matches the best
  output$bestResultsUI = renderTable({
    data = trainResults();
    if(is.null(data))
      return();
    data$results[as.numeric(rownames(data$bestTune)[1]),];
  })
  
  #a feature plot using the caret package
  output$caretPlotUI = renderPlot({
    data = rawInputData();
    column = input$modelLabelUI;
    
    
    #check if the data is loaded first
    if(is.null(data)){
      return()
    } else {
      columnElement = which(colnames(data) == column);  
      
      p = featurePlot(x=data[,-columnElement],y=data[,columnElement],plot="pairs",auto.key=T);
      print(p);
    }
  })
  
  #the results graph of the caret output
  output$finalPlotUI = renderPlot({
    data = trainResults();
    if(is.null(data)){
      return();
    } else {
      
      #the model we are interested in
      modelTag = isolate(input$modelSelectionUI);
      
      
      #grab the column
      column = isolate(input$modelLabelUI);
      
      #build the equation
      form = as.formula(paste(column," ~ .",sep=""));
      par(mfrow=c(2,1));
      p = plot(data);
      print(p);
      
      #       if(modelTag == "nn")
      #       {
      #       data$finalModel$call$formula = form;
      #       
      #       
      #       plot(data$finalModel);
      #       
      #       } else if(modelTag == "rf")
      #       {
      #         plot(data$finalModel);  
      #       }
      
    }
  })
  
  
  #simple datatable of the data
  output$rawDataView = renderDataTable({
    newData = rawInputData();
    if(is.null(newData))
      return();
    newData;
  });
  
  #responsible for selecting the label you want to regress on
  output$labelSelectUI = renderUI({
    
    data = rawInputData();
    #check if the data is loaded first
    if(is.null(data)){
      return(helpText("Choose a file to load"))
    } else {
      return(selectInput("modelLabelUI","Select Target Feature",colnames(data),colnames(data)[1]));
    }
  });
  
  #a dynamic table responsible for building the input types to the model
  output$modelParametersUI = renderUI({
    
    modelTag = input$modelSelectionUI;
    
    if(modelTag == "en")
    {
      tagList(selectInput("enModelTypeUI","Model Type",c('Binomial'="binomial",'Gaussian'="gaussian",'Multinomial'="multinomial"),"Binomial"),
              numericInput("enAlphaStartUI","Alpha Start",0.1),
              numericInput("enAlphaEndUI","Alpha End",1.0),
              numericInput("enAlphaRangeUI","# Alpha",5),
              numericInput("enLambdaStartUI","Lambda Start",0.1),
              numericInput("enLambdaEndUI","Lambda End",1),
              numericInput("enLambdaRangeUI","# Lambda",5))
    } else if(modelTag == "rf") {
      data = rawInputData();
      if(is.null(data)){
        dataRange = 2;
      } else {
        dataRange = ncol(data)-1;
      }
      tagList(sliderInput("mTryRangeUI","mTry Range",min=1,max=dataRange,value=c(1,dataRange),step=1),
              numericInput("rmTryUI","mTry Skip",1)
      )
    } else if (modelTag == "nn") {
      tagList(selectInput("nnModelTypeUI","Model Type",c('Binomial'="binomial",'Gaussian'="gaussian",'Multinomial'="multinomial"),"Binomial"),
              sliderInput("nnSizeUI","NN Size",min=1,max=25,value=c(1,5)),
              numericInput("nnSizeRangeUI","NN Size Range",5),
              sliderInput("nnDecayUI","NN Decay",min=0.0,max=1.0,value=c(0,0.1),step=0.001),
              numericInput("nnDecayRangeUI","NN Decay Range",5))      
    }
    
  })
  
  
})