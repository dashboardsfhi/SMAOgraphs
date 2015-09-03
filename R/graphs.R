SMAOColourPalette <- c(
  "01totalorange"="#ff7f00",
  "02totalorange"="#fdbf6f",
  "03norwegianred"="#e31a1c",
  "04norwegianred"="#fb9a99",
  "05blue"="#1f78b4",
  "06blue"="#a6cee3",
  "07green"="#33a02c",
  "08green"="#b2df8a",
  "09pruple"="#6a3d9a",
  "10purple"="#cab2d6",
  "11brown"="#b15928",
  "12yellow"="#ffff99"
)

SMAOLabelsTotal <- "Totalt"
SMAOLabelsNorway <- c("Norge","Norsk","Norske","Norskfødte")

SMAOFormatGGPlot <- function (q, sizeMultiplier = 3, legendKey = 3, xAngle = 0, stripes = TRUE, legendPos="bottom", ncol=3, legendBorder=FALSE) 
{
  q <- q + theme(axis.ticks = element_line(colour = "black"))
  q <- q + theme(panel.background = element_rect(colour = "white", 
                                                 fill = "white"))
  q <- q + theme(axis.line = element_line(colour = "black", 
                                          size = 0.5 * sizeMultiplier))
  q <- q + theme(strip.background = element_blank())
  q <- q + theme(panel.grid.major = element_blank())
  q <- q + theme(panel.grid.minor = element_blank())
  if (stripes) {
    q <- q + theme(panel.grid.major = element_line(colour = "black", 
                                                   size = 0.25 * sizeMultiplier, linetype = 3))
    q <- q + theme(panel.grid.minor = element_line(colour = "black", 
                                                   size = 0.25 * sizeMultiplier, linetype = 3))
  }
  q <- q + theme(legend.key.size = unit(legendKey, "lines"))
  q <- q + theme(legend.key = element_blank())
  if(legendBorder) q <- q + theme(legend.key = element_rect(colour = "black"))
  q <- q + theme(axis.title.x = element_text(size = 10 * sizeMultiplier, 
                                             vjust = 0, colour = "black"))
  q <- q + theme(axis.title.y = element_text(size = 10 * sizeMultiplier, 
                                             angle = 90, vjust = 0.25, colour = "black"))
  q <- q + theme(axis.text.y = element_text(size = 10 * sizeMultiplier, 
                                            hjust = 1, vjust = 0.4, colour = "black"))
  q <- q + theme(axis.text.x = element_text(size = 10 * sizeMultiplier, 
                                            hjust = 0.5, vjust = 1, colour = "black", angle = xAngle))
  if (xAngle != 0) {
    q <- q + theme(axis.text.x = element_text(size = 10 * 
                                                sizeMultiplier, hjust = 0, vjust = 0.5, colour = "black", 
                                              angle = xAngle))
  }
  q <- q + theme(strip.text.y = element_text(size = 10 * sizeMultiplier, 
                                             hjust = 0.5, colour = "black"))
  q <- q + theme(strip.text.x = element_text(size = 10 * sizeMultiplier, 
                                             hjust = 0.5, colour = "black"))
  q <- q + theme(legend.text = element_text(size = 10 * sizeMultiplier, 
                                            hjust = 0.5, colour = "black"))
  q <- q + theme(legend.title = element_text(size = 10 * sizeMultiplier, 
                                             hjust = 0.5, colour = "black"))
  q <- q + theme(legend.position = "right")
  q <- q + theme(plot.margin = unit(c(0.5, 0.5, 1, 0.5), "lines"))
  q <- q + theme(plot.title = element_text(size = 14 * sizeMultiplier, 
                                           hjust = 0.5, vjust = 1))
  q <- q + theme(legend.position=legendPos)
  q <- q + guides(colour = guide_legend(ncol = ncol, byrow=TRUE, title.position="top"))
  q <- q + guides(fill = guide_legend(ncol = ncol, byrow=TRUE, title.position="top"))
  
  return(q)
}

SMAOpng <- function (file = "Figure.png", w = 1, h = 1, landscape = TRUE) 
{
  width <- 2480/2
  height <- 3508/2
  if (landscape) {
    width <- 3508/2
    height <- 2480/2
  }
  width %<>% times(w)
  height %<>% times(h)
  png(file, width = width, height = height)
}

SMAOPriorityLevels <- function(useLabels){
  if(!is.factor(useLabels)){
    useLabels <- as.factor(useLabels)
  }
  useLabels <- levels(useLabels)
  useValues <- SMAOColourPalette
  usedLabels <- rep(FALSE,length(useLabels))
  usedValues <- rep(FALSE,length(useValues))
  
  if(sum(useLabels %in% SMAOLabelsTotal)>0){
    loc <- min(which(useLabels %in% SMAOLabelsTotal))
    names(useValues)[1] <- useLabels[loc]
    usedLabels[loc] <- TRUE
    usedValues[1] <- TRUE
  }
  if(sum(useLabels %in% SMAOLabelsNorway)>0){
    loc <- min(which(useLabels %in% SMAOLabelsNorway))
    names(useValues)[2] <- useLabels[loc]
    usedLabels[loc] <- TRUE
    usedValues[2] <- TRUE
  }
  j <- 5
  for(i in 1:length(usedLabels)){
    if(usedLabels[i]) next
    names(useValues)[j] <- useLabels[i]
    usedLabels[i] <- TRUE
    usedValues[j] <- TRUE
    j <- j + 1
  }
  values=useValues[usedValues]
  labels=names(useValues)[usedValues]
  return(labels)
}

SMAOColourSpecify <- function(useLabels, total=NULL, total2=NULL, norway=NULL, norway2=NULL, type="colour", lab=""){
  if(!is.factor(useLabels)) stop("THIS IS NOT A FACTOR, REORGANISING WILL DISPLAY WRONG COLOURS")
  
  useLabels <- levels(useLabels)
  useValues <- SMAOColourPalette
  usedLabels <- rep(FALSE,length(useLabels))
  usedValues <- rep(FALSE,length(useValues))
  
  values <- rep("black", length(useLabels))
  if(!is.null(total)){
    values[total] <- useValues[1]
    usedLabels[total] <- TRUE
  }
  if(!is.null(total2)){
    values[total2] <- useValues[2]
    usedLabels[total2] <- TRUE
  }
  if(!is.null(norway)){
    values[norway] <- useValues[3]
    usedLabels[norway] <- TRUE
  }
  if(!is.null(norway2)){
    values[norway2] <- useValues[4]
    usedLabels[norway2] <- TRUE
  }
  j <- 5
  if(sum(!usedLabels)>0){
    for(i in 1:length(useLabels)){
      if(usedLabels[i]) next
      values[i] <- useValues[j]
      j <- j + 1
    }
  }
  
  labels=useLabels
  
  palette <- list("values"=values,"labels"=labels)
  
  fn <- scale_colour_manual
  if(type=="fill"){
    fn <- scale_fill_manual
  }
  return(fn(lab,labels=palette$labels,values=palette$values))
}

SMAOColourYears <- function(useLabels, type="colour", lab=""){
  if(!is.factor(useLabels)){
    useLabels <- as.factor(useLabels)
  }
  useLabels <- levels(useLabels)
  
  fn <- scale_colour_brewer
  if(type=="fill"){
    fn <- scale_fill_brewer
  }
  
  return(fn(lab,palette="YlOrRd"))
}

