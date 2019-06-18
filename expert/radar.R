#devtools::install_github('neuhausi/canvasXpress')

drawradar <- function(data,strTitle="radar plot"){ 
  library(canvasXpress)
  c <- canvasXpress(
    data=data,
    
    circularArc=360,
    circularRotate=0,
    circularType="radar",
    colorScheme="Bootstrap",
    graphType="Circular",
    legendPosition="top",
    ringGraphType=list("area"),
    showTransition=TRUE,
    smpLabelScaleFontFactor =1.5,
    title=strTitle,
    transitionStep=50,
    transitionTime=1500
  )
  c
}

