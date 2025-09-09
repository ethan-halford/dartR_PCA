setOldClass("big_SVD")
setClass("gl.SVD", 
         slots = c(
           mainObj = "big_SVD", 
           d = "ANY", 
           u = "ANY", 
           v = "ANY", 
           type = "ANY"
         ))

setMethod("initialize", "gl.SVD",
          function(.Object, mainObj) {
            .Object <- callNextMethod(.Object, mainObj=mainObj)
            for(i in slotNames(.Object)[-1]){
              slot(.Object, i) <- .Object@mainObj[[i]]
            }
            return(.Object)
          }
)

setMethod("show", signature =  "gl.SVD", 
          definition = function(object){
            nameSlots <- slotNames(object)[-1]
            cat("### gl.SVD out ###\n")
            for(i in nameSlots){
              cat("  @",i,"\n", sep = "")
            }
          })
