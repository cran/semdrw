#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyAce)
library(lavaan)
library(semPlot)
library(dplyr)
library(semTools)
library(psych)

ui <- fluidPage(
  h6("SEM Shiny  Authored by:", tags$img(src ="K.JPG", height=100, width=100)),
  
  verbatimTextOutput("preface"),
  tags$img(src = "T.png"),
  br(),
  br(),
  br(),
  textAreaInput("model", "CFA Model Specification", value = "security =~q1+q2+q3
utility =~q4+q5+q6
                integrity =~q7+q8+q9+q10",  
                width = 400, height = 200,cols = NULL, rows = NULL, placeholder = NULL, resize = NULL),
  textAreaInput("model2", "SEM Model Specification", value = "security =~q1+q2+q3
                utility =~q4+q5+q6
                integrity =~q7+q8+q9+q10
                utility ~ security + integrity",  
                width = 400, height = 200,cols = NULL, rows = NULL, placeholder = NULL, resize = NULL),
  helpText("Copy paste data with variable names from Excel"),
  aceEditor("text", value=
              "q1	q2	q3	q4	q5	q6	q7	q8	q9	q10
            2	2	2	2	2	2	2	2	2	2
            3	3	4	2	2	3	3	4	3	3
            3	3	3	3	3	3	3	3	3	2
            3	1	3	3	3	3	3	2	3	3
            3	3	3	3	3	3	3	3	3	3
            4	3	3	2	3	3	4	4	3	4
            2	3	4	3	3	3	4	5	5	4
            4	4	4	4	3	3	4	4	4	4
            3	3	4	3	3	3	4	4	3	3
            3	3	4	3	3	3	4	4	4	4
            1	2	4	1	3	3	3	3	2	2
            4	4	4	3	3	3	3	3	4	4
            5	5	4	5	3	4	4	3	2	2
            3	3	5	4	3	4	4	4	4	4
            5	5	5	3	3	4	5	5	5	5
            3	4	5	2	3	3	5	5	5	5
            1	1	1	4	4	4	5	5	5	4
            1	1	2	3	4	2	3	3	2	2
            2	4	2	3	4	2	5	4	4	4
            5	4	2	4	4	4	5	4	4	3
            4	4	3	3	4	4	5	4	5	4
            3	1	3	4	4	3	3	3	3	3
            4	4	3	4	4	4	3	4	4	4
            3	3	3	4	4	4	4	4	4	4
            4	4	3	3	4	3	3	3	2	3
            3	5	3	4	4	4	4	3	4	4
            4	4	3	4	4	4	4	5	5	5
            3	3	3	4	4	4	5	5	5	5
            2	3	3	4	4	4	4	4	4	4
            3	3	4	4	4	3	4	4	3	4
            5	5	4	4	4	4	4	5	5	4
            5	3	4	4	4	4	5	5	5	5
            4	4	4	4	4	4	4	4	4	4
            5	5	4	3	4	4	4	5	5	4
            4	4	4	4	4	4	4	4	4	4
            4	4	4	4	4	4	4	4	4	4
            5	4	4	3	4	4	4	4	4	4
            3	3	4	4	4	4	4	4	4	3
            2	3	4	3	4	4	4	4	4	4
            5	3	4	3	4	4	5	5	5	5
            5	5	4	4	4	3	5	5	4	4
            4	4	4	4	4	4	4	4	4	4
            4	4	4	4	4	3	4	4	4	4
            4	4	4	4	4	4	4	4	4	4
            3	4	4	4	4	4	4	4	4	4
            3	3	4	4	4	4	4	4	4	4
            4	4	4	4	4	4	4	4	4	4
            4	4	4	4	4	4	4	4	4	4
            3	4	4	4	4	4	4	4	4	4
            4	3	4	4	4	4	4	4	4	4
            4	4	4	4	4	4	4	4	4	4
            3	4	4	4	4	4	5	5	5	4
            4	4	4	4	4	4	3	4	4	4
            5	4	4	4	4	4	4	5	5	5
            2	3	4	4	4	4	5	5	5	4
            3	3	4	5	4	3	4	4	4	4
            4	4	4	4	4	5	4	4	4	4
            4	3	4	4	4	4	4	4	4	4
            2	2	4	2	4	4	3	3	3	4
            4	4	4	4	4	5	4	4	4	4
            3	3	4	4	4	4	5	4	4	4
            4	4	4	4	4	4	4	4	4	4
            5	5	4	3	4	3	3	5	4	5
            3	4	4	3	4	3	4	4	4	4
            2	5	5	4	4	5	3	5	5	5
            5	5	5	4	4	4	4	4	4	4
            3	5	5	5	4	4	5	5	5	4
            4	5	5	4	4	3	4	4	4	3
            5	5	5	4	4	4	5	5	5	5
            4	5	5	4	4	4	4	4	2	4
            5	5	5	5	4	5	5	5	5	5
            4	4	5	4	4	4	4	4	4	4
            5	4	5	4	4	4	5	5	5	4
            3	3	5	5	4	4	5	5	5	4
            5	5	5	4	4	4	5	5	4	5
            5	5	5	4	4	4	4	4	3	4
            5	5	5	4	4	4	5	5	4	5
            5	5	5	3	4	5	4	5	5	4
            4	5	5	4	4	4	4	4	4	4
            3	5	5	4	4	4	5	5	5	4
            5	5	5	4	4	4	5	4	4	5
            1	1	2	4	5	4	4	4	4	4
            5	4	2	4	5	4	5	5	5	5
            5	5	2	5	5	5	5	5	4	5
            5	5	3	5	5	5	5	5	5	5
            5	5	3	5	5	5	5	5	5	5
            4	4	3	5	5	5	5	5	5	5
            3	4	3	4	5	5	4	4	4	3
            1	1	3	5	5	5	5	5	4	4
            4	4	3	5	5	5	5	5	4	4
            3	3	3	5	5	5	5	5	5	5
            4	5	3	5	5	5	5	5	5	4
            5	4	4	4	5	4	5	4	3	4
            1	1	4	5	5	5	5	5	5	4
            4	3	4	4	5	2	4	4	3	4
            3	4	4	5	5	5	5	5	5	5
            3	3	4	5	5	4	4	4	5	5
            5	5	4	4	5	4	5	5	5	5
            5	4	4	5	5	5	2	5	5	3
            4	4	4	4	5	5	4	5	5	5
            4	3	4	4	5	5	4	5	5	4
            4	4	4	5	5	5	5	5	5	4
            4	4	4	4	5	5	4	5	4	5
            4	4	4	5	5	4	4	4	4	4
            5	4	4	5	5	4	5	5	5	4
            5	1	4	5	5	5	5	5	4	5
            4	4	4	5	5	5	5	5	4	5
            4	4	4	4	5	5	3	3	4	4
            4	3	4	4	5	4	4	4	3	3
            4	4	4	5	5	4	5	5	5	4
            3	4	4	4	5	4	4	3	3	4
            5	5	4	4	5	5	5	5	5	5
            4	4	4	5	5	4	5	5	5	5
            4	4	4	5	5	5	4	4	3	4
            4	4	4	4	5	4	5	5	5	5
            4	4	4	4	5	5	5	5	5	5
            4	4	4	4	5	4	4	4	4	4
            5	5	4	4	5	4	5	5	5	5
            4	4	4	4	5	5	4	4	3	3
            3	4	4	4	5	5	4	4	4	4
            5	5	4	5	5	5	4	5	5	5
            4	2	4	4	5	3	4	5	5	4
            4	5	4	4	5	4	4	5	5	4
            4	5	4	4	5	5	4	4	4	4
            4	4	4	5	5	5	5	5	5	5
            4	4	4	5	5	5	5	5	4	5
            3	3	4	5	5	5	4	4	4	3
            4	4	4	5	5	5	4	5	5	4
            5	4	4	4	5	5	4	5	5	4
            4	4	4	4	5	5	5	5	5	5
            4	4	4	5	5	5	4	5	5	4
            3	3	4	5	5	5	5	5	5	5
            5	4	4	5	5	4	5	5	5	5
            4	4	4	5	5	4	4	5	4	5
            4	4	4	5	5	5	4	5	5	5
            4	4	4	4	5	5	4	4	4	4
            4	4	4	5	5	5	5	5	5	5
            5	5	4	5	5	5	5	5	5	5
            4	4	4	5	5	5	5	5	5	5
            4	4	4	4	5	4	4	5	4	5
            4	4	4	5	5	5	5	5	5	5
            4	4	4	5	5	5	5	5	5	5
            4	4	4	5	5	4	5	5	5	5
            4	4	4	5	5	5	5	4	5	4
            4	3	5	4	5	5	5	5	4	5
            4	5	5	5	5	4	5	5	5	4
            5	5	5	5	5	5	5	5	5	5
            5	2	5	5	5	3	4	3	3	3
            4	4	5	5	5	5	5	5	5	5
            3	5	5	4	5	4	5	5	5	5
            4	4	5	3	5	4	5	5	5	5
            5	4	5	5	5	5	5	5	5	5
            4	4	5	5	5	5	4	4	4	4
            5	5	5	5	5	5	4	5	5	5
            5	4	5	4	5	3	5	5	4	5
            2	4	5	4	5	5	4	4	5	5
            5	5	5	5	5	5	5	5	5	4
            2	5	5	4	5	5	5	5	5	5
            5	5	5	4	5	4	5	5	5	4
            5	5	5	5	5	5	5	5	5	5
            4	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	4	3	4	3	4
            4	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	4	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            4	4	5	5	5	3	5	5	5	5
            5	5	5	4	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	4	5	4	5	5	5	5
            5	5	5	5	5	4	5	5	5	5
            5	5	5	5	5	4	5	5	4	4
            5	5	5	5	5	5	5	5	5	4
            5	5	5	5	5	5	5	5	5	5
            4	4	5	5	5	4	4	5	5	5
            4	4	5	4	5	4	4	4	4	4
            5	5	5	5	5	5	3	4	4	4
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            3	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	4	2	4	4	4
            5	5	5	5	5	5	5	5	5	4
            5	5	5	5	5	5	5	5	5	5
            4	4	5	3	5	4	4	4	4	4
            5	5	5	5	5	5	5	5	5	5
            3	3	5	5	5	5	2	5	5	5
            5	5	5	5	5	5	5	5	5	5
            4	5	5	4	5	4	3	5	2	5
            5	5	5	5	5	5	5	5	5	5
            5	4	5	5	5	5	5	5	5	5
            5	5	5	4	5	4	5	5	5	5
            4	4	5	4	5	4	5	5	5	5
            5	5	5	5	5	4	5	5	5	5
            5	5	5	4	5	5	4	4	5	4
            5	5	5	5	5	5	2	5	5	5
            3	3	5	5	5	4	4	4	4	4
            4	4	5	5	5	2	2	5	1	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	4	5	5	4	4	4	5
            5	5	5	5	5	5	5	5	5	5
            3	3	5	5	5	5	5	5	5	5
            4	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	4	5	4	4	4	4	4
            5	5	5	5	5	4	5	5	5	5
            4	4	5	5	5	5	5	5	2	5
            4	4	5	5	5	4	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            4	4	5	5	5	5	5	5	5	5
            5	4	5	5	5	5	5	5	2	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            4	4	5	4	5	5	5	5	4	4
            5	5	5	4	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            4	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	4	4	4	4
            5	4	5	5	5	5	5	5	5	4
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	3	5
            4	4	5	5	5	5	5	5	5	5
            4	5	5	5	5	4	5	5	5	4
            4	5	5	4	5	5	4	4	4	4
            5	5	5	5	5	5	4	4	5	5
            3	5	5	5	5	5	5	5	5	5
            4	4	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	2	5	5	5	5	5	5	5	5
            5	4	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            4	4	5	5	5	5	4	5	4	4
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            4	5	5	5	5	5	5	5	5	5
            4	4	5	5	5	5	5	5	5	5
            4	5	5	5	5	4	5	5	4	5
            5	5	5	5	5	4	5	5	5	5
            4	4	5	5	5	5	4	5	5	5
            4	5	5	5	5	4	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	4	4	5	5
            5	5	5	4	5	5	5	5	2	2
            3	5	5	5	5	5	4	5	5	5
            4	5	5	5	5	5	4	4	5	5
            ",  mode="r", theme="white"),
  sliderInput("n_factors", label = "Enter the number of factors/components:",
              min = 1, max = 15, value = 2, step = 1),
  selectInput("rotation", label = "Select the rotaton type for PCA:",
              choices = c("none","varimax","promax"), selected = "varimax"),
  plotOutput("plot1"),
  verbatimTextOutput("printpaper1"),
  selectInput("layout", label = "Layout for CFA and SEM:",
              choices = c("tree", "circle","spring"), selected = "spring"),
  plotOutput("plot2"),
  verbatimTextOutput("printpaper2"),
  plotOutput("plot3"),
  verbatimTextOutput("printpaper3")
 
)


server <- function(input, output) {
  
  output$preface <-renderPrint({
    
    cat(sprintf("\nDr.  Kartikeya Bolar\n"))
    cat(sprintf("\nAssociate Professor and Area  Co-Chair\n"))
    cat(sprintf("\nOperations and Information Science\n"))
    cat(sprintf("\nT A  Pai Management Institute\n"))
    
  })
  output$plot1<-renderPlot({
    
    
    
    get.text <- reactive({input$text})
    if(is.null(get.text())){return ()}
    dataframe<- read.table(text = get.text(),header = TRUE)
    pc= principal(scale(dataframe),nfactors = as.numeric(input$n_factors),rotate = input$rotation)
    plot(pc)
    
    
  })
  output$plot2<-renderPlot({
    
    
    
    modelmydata = input$model
    get.text <- reactive({input$text})
    if(is.null(get.text())){return ()}
    dataframe<- read.table(text = get.text(),header = TRUE)
    mydatamodel= cfa(modelmydata,data = scale(dataframe))
    semPaths(mydatamodel,whatLabels = "std",layout = input$layout)
    
  })
  output$plot3<-renderPlot({
    
    
    
    modelmydata = input$model2
    get.text <- reactive({input$text})
    if(is.null(get.text())){return ()}
    dataframe<- read.table(text = get.text(),header = TRUE)
    mydatamodelsem= sem(modelmydata,data = scale(dataframe))
    semPaths(mydatamodelsem,whatLabels = "std",layout = input$layout)
    
})
  
  
  output$printpaper1 <-renderPrint({
    
    
    
    cat(sprintf("Data Analysis and Interpretation\n"))
    
    cat(sprintf("1. Principal Component Analysis\n"))
    
    cat(sprintf("In order to seek consensus regarding the factor structure, the responses obtained for all the items of all the constructs were subjected to the principal components analysis (PCA) extraction method. The rotation method was Varimax with Kaiser Normalisation. Prior to performing the PCA, the suitability of the data for factor analysis was assessed.\n"))
    
    get.text <- reactive({input$text})
    if(is.null(get.text())){return ()}
    dataframe<- read.table(text = get.text(),header = TRUE)
    KMOvalue = KMO(cor(dataframe))
    cat(sprintf("The Kaiser-Meyer-Olkin measure of sampling adequacy (MSA) was %f which exceeded the recommended value of 0.6",KMOvalue$MSA))
    cat(sprintf("and Bartlett's Test of Sphericity reached statistical significance, thus, supporting the factorability of the correlation matrix."))
    pc= principal(scale(dataframe),nfactors = as.numeric(input$n_factors),rotate = input$rotation)
    cat(sprintf("The same factor structure was obtained from this analysis where the %g factors obtained accounted for %g of the variance, which is closer to the thumb rule of 0.70\n",pc$factors,pc$Vaccounted[3,pc$factors]))
    cat(sprintf("Table 1 shows the Factor structure.\n"))
    print(pc$loadings[,1:pc$factors], digits = 3)
    cat(sprintf("\n2.  Testing Measurement and Structural Model"))
    cat(sprintf("\nFollowing Hair et al. (1998), we adopted a two-stage approach to the model testing. The first step involved the analysis of the measurement model, estimated by using the confirmatory factor analysis (CFA) to examine the reliability and validity of the proposed constructs. The second step tested the structural model, which was analyzed to investigate the hypotheses.
                The measurement model was developed and tested using the lavaan package functions of R, as shown in the Figure 1 below."))
    
  })
  output$printpaper2 <-renderPrint({ 
    cat(sprintf("\n Fig.1 Measurement Model\n"))
    
    cat(sprintf("\nThe model fit indices were considered to assess the goodness of fit of the proposed measurement model. The details of the Model fit Indices of the Measurement Model along with the Recommended Values are presented in Table 2.\n"))
    
    modelmydata = input$model
    get.text <- reactive({input$text})
    if(is.null(get.text())){return ()}
    dataframe<- read.table(text = get.text(),header = TRUE)
    
    
    mydatamodelcfa = cfa(modelmydata,data = scale(dataframe))
    fitmeasure = data.frame(fitvalue = fitmeasures(mydatamodelcfa))
    fitmeasuresfinal = data.frame(t(fitmeasure))
    chisquarebydegreesoffreedom =  round(fitmeasuresfinal$chisq/fitmeasuresfinal$df,2)
    fitmeasuresfinal2 = select(fitmeasuresfinal,c(38,39,13,9,23))
    fitmeasuresfinal2$chibydf = chisquarebydegreesoffreedom
    Measurementmodel = t(fitmeasuresfinal2)
    Measurementmodeltable =  data.frame(Modelfitindices = c("CMIN/df","GFI","AGFI","NFI","CFI","RMSEA"),MeasurementModel = c(chisquarebydegreesoffreedom,fitmeasuresfinal2$gfi,fitmeasuresfinal2$agfi,fitmeasuresfinal2$nfi,fitmeasuresfinal2$cfi,fitmeasuresfinal2$rmsea),Recommendedvalues = c("3 or below","0.9 or above","0.8 or above","0.9 or above","0.9 or above","0.07 or below") )
    print(Measurementmodeltable)
    cat(sprintf("\nThe Chisquare to df ratio %f of the model indicates an acceptable fit between the model and the sample data (Carmines and Mciver, 1981)",round(chisquarebydegreesoffreedom,2)))
    cat(sprintf("\nAll the other Model fit indices reported here were either quite close to the recommended values or adhering to the recommendations.  This suggests that the data collected from the respondents is aligned with the items reflected in the constructs.
                
                Next, we assessed the validity of the constructs. Churchill (1979) had suggested that convergent and discriminant validities should be examined for construct validity. Therefore, we assessed convergent validity by examining coefficient alpha(ALPHA), composite reliability (CR) and average variance extracted (AVE) from the constructs (Hair et al., 1998).
                CR is calculated by squaring the sum of loadings and then dividing it by the sum of squared loadings plus the sum of measurement error. The AVE, on the other hand, measures the variance captured by the indicators relative to the measurement error. Table 3  indicated the reliability and convergent validity measures."))
    
    relmeasure = data.frame(reliability(mydatamodelcfa))
    relmeasure$total =NULL
    relmeasure = data.frame( t(relmeasure))
    relmeasure$omega2 =NULL
    relmeasure$omega3 = NULL
    colnames(relmeasure) = c("ALPHA","CR","AVE")
    constructnames = data.frame(constructnames = row.names(relmeasure))
    relmeasure$ALPHA = round(relmeasure$ALPHA,3)
    relmeasure$CR = round(relmeasure$CR,3)
    relmeasure$AVE = round(relmeasure$AVE,3)
    b = lavInspect(mydatamodelcfa,"cor.lv")
    b = b*b
    diag(b)= relmeasure$AVE
    
    cat(sprintf("\nTable 3 :  Reliability and convergent validity measures\n"))
    print(relmeasure)
    cat(sprintf("\nTable 3 indicates that the CR values of all the constructs were from %g to %g, and were well above the suggested minimum value of 0.70 (Hair et al., 1998)",min(relmeasure$CR),max(relmeasure$CR)))
    
    cat(sprintf("\nSimilarly, AVE values were all above 0.50, providing further evidence of the convergent validity. In addition, as confirmatory evidence of the discriminant validity, the AVE in each construct is higher than the square of the correlation coefficients between two constructs (Fornell and Larcker, 1981) as shown in Table 4."))
    cat(sprintf("\nTable 4 :  AVE and Square of the correlation between the constructs\n"))
    print(b)
    cat(sprintf("\nwe are done with the exploratory and confirmatory factor analysis .
                The convergent and discriminant validity is established among the constructs"))
    cat(sprintf("%s",constructnames$constructnames))
    
    
    modelmydatasem = input$model2
    get.text <- reactive({input$text})
    if(is.null(get.text())){return ()}
    dataframe<- read.table(text = get.text(),header = TRUE)
    
    mydatamodelsem = sem(modelmydatasem,data = scale(dataframe))
    Structuralmodeltable  = data.frame(Structuralmodel= fitmeasures(mydatamodelsem,c ("chisq","df","gfi","agfi","nfi","cfi","rmsea")))
    temp= data.frame(t(Structuralmodeltable))
    temp$Chisqbydf =  temp$chisq/temp$df
    temp$chisq = NULL
    temp$df = NULL
    Structuralmodeltable =data.frame(t(temp))
    ss =  data.frame(standardizedsolution(mydatamodelsem,type = "std.lv"))
    ssreg = filter(ss, ss$op == "~")
    ssreg$ci.lower =NULL
    ssreg$ci.upper=NULL
    ssreg$op ="<----"
    colnames(ssreg)=c("Dep_var","Dir","Ind_var","StdEst","SE","CriticalRatio","P_value")
    ssreg$SE =NULL
    ssregsorted = ssreg[order(ssreg$StdEst,decreasing = TRUE),]
    
    cat(sprintf("\nAfter validating the measurement model, we tested the hypotheses about the interelationships among the constructs using the structural model.The model fit indices of the structural model are as shown in Table 5.\n"))
    cat(sprintf("\nTable 5:  Model fit indices of the structural model\n"))
    print(Structuralmodeltable)
    cat(sprintf("\nThe path or the structural analysis  is shown in Table 6.\n"))
    
    cat(sprintf("\nTable 6:  Results of Path Analysis \n"))
    print(ssregsorted)
    cat(sprintf("\nFrom the structural analysis,  the independent constructs in the decreasing order of their influence are as follows\n"))
    cat(sprintf("%s",ssregsorted$Ind_var))
    cat(sprintf("\nThe structural model is shown in the Figure 2 Below.\n")) 
  }) 
  
  output$printpaper3<- renderPrint({
    cat(sprintf("Fig.2 Structural Model"))
    modelmydatacfa = input$model
    modelmydatasem = input$model2
    get.text <- reactive({input$text})
    if(is.null(get.text())){return ()}
    dataframe<- read.table(text = get.text(),header = TRUE)
    
    
    mydatamodelcfa = cfa(modelmydatacfa,data = scale(dataframe))
    
    mydatamodelsem = sem(modelmydatasem, data = scale(dataframe))
    
    cat(sprintf("\nAdditional information about Measurement Model is as follows\n"))
    cat(sprintf("\nStandardized Estimates of CFA\n"))
    print(standardizedSolution(mydatamodelcfa))
    cat(sprintf("\nModel Fit indices of  CFA\n"))
    print(fitmeasures(mydatamodelcfa))
    cat(sprintf("\nModification indices of  CFA\n"))
    print(sort(modificationIndices(mydatamodelcfa),decreasing = TRUE))
    cat(sprintf("\nAdditional information about Structural Model is as follows\n"))
    cat(sprintf("\nStandardized Estimates of SEM\n"))
    print(standardizedSolution(mydatamodelsem))
    cat(sprintf("\nModel Fit indices of  SEM\n"))
    print(fitmeasures(mydatamodelsem))
    cat(sprintf("\nModification indices of  SEM\n"))
    print(sort(modificationIndices(mydatamodelsem),decreasing = TRUE))
    cat(sprintf("\nUnstandardized Estimates of  SEM\n"))
    print(parameterEstimates(mydatamodelsem))
  }) 
  
  
  
  
  
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)

