library(dartRverse)
library(ggplot2)
source("gl.PCA.classes.R")

gl.partial.pca.FBM <- function(genObj, 
                               k = 10, 
                               scaling = NULL, 
                               ind.row = F, 
                               ind.col = F, 
                               block.size = NULL
){
  
  if (!inherits(genObj, "genlight")) {
    stop(error("Input `x` must be a genlight object\n"))
  }
  
  if (!is.logical(ind.row) || !is.logical(ind.col)) {
    stop(error("ind.row and ind.col must be logical (T/F)\n"))
  }
  
  # Colnames <- loci ID 
  # Rownames <- individual ID 
  
  fbm <- bigstatsr::as_FBM(as.matrix(genObj)) 
  
  
  row_col <- genObj$ind.names
  col_col <- genObj$loc.names
  
  svdRun <- bigstatsr::big_SVD(X=fbm)
  svdRun$type <- "partialSVD"
  rownames(svdRun$u) <- genObj$ind.names
  rownames(svdRun$v) <- genObj$loc.names
  
  newSVD <- new("gl.SVD", 
                mainObj=svdRun)
  
  return(newSVD)
  
}

#example : gl.partial.pca.FBM(possums.gl)











