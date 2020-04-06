`print.draw.latent` <-
function(x, rootname = NULL, ...) {
   cat("Diagram has patterns with", x$which.npos, "items positive\n")
   cat("Original order was", x$new.order, "\n")
   if(!is.null(rootname)) {
      filename <- paste(rootname, ".gv", sep = "")
      cat("Commands output to ", filename, "\n", sep = "")
      cat(file = filename, x$code)
   }   
   invisible(x)
} # end print method for draw.latent

