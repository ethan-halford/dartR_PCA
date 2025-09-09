library(dartRverse)
library(ggplot2)
source("gl.PCA.classes.R")

gl.partial.pca.FBM <- function(genObj, 
                               k = 10, 
                               scaling = bigsnpr::snp_scaleBinom(), 
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
  
  col_col <- rep(1:length(genObj$loc.names))
  row_col <- as.numeric(genObj$ind.names)
  
  svdRun <- bigstatsr::big_SVD(X=fbm, 
                               k=k, 
                               ind.row = row_col, 
                               ind.col = col_col, 
                               block.size = bigstatsr::block_size(nrow(fbm))
  )
  
  svdRun$type <- "partialSVD"
  rownames(svdRun$u) <- genObj$ind.names
  rownames(svdRun$v) <- genObj$loc.names
  
  newSVD <- new("gl.SVD", 
                mainObj=svdRun)
  
  return(newSVD)
  
}


#example : gl.partial.pca.FBM(possums.gl)











