`plot.draw.latent` <-
function(x, rootname = NULL, graphtype = "pdf", ...) {
   if(is.null(rootname)) {
      warning("Cannot plot if rootname NULL")
      res <- list(retval = 256, cmnd = NULL)
   } else {
      cmnd <- paste("dot -T", graphtype, " ", rootname, ".gv -o ",
         rootname, ".", graphtype, sep = "")
      retval <- system(cmnd)
      res <- list(retval = retval, cmnd = cmnd)
   }
   invisible(res)
} # end plot method for draw.latent

