`draw.latent` <-
function(mat, # matrix of binary data
   threshold = 0, # patterns only printed if more frequent than threshold
   which.npos = NULL, # which values of npos to print, NULL means all
   labels = NULL, # labels for subgraphs, NULL means none, a character vector supplies
                  # the lables, otherwise lablled as n positive
   reorder = TRUE # put the items in ascending order of prevalence
) {
   X <- check.mat(mat)
   nitem <- ncol(X)
   if(reorder) {
      new.order <- order(apply(X, 2, sum))
   } else {
      new.order <- 1:nitem
   }
# new.order[i] is the position in the original vector from which the ith
# in increasing prevalence came
   do.labels <- !is.null(labels)
   make.labels <- do.labels & length(labels) != (nitem + 1)
   mesa <- data.frame(table(data.frame(X[,new.order])))
   rownames(mesa) <- apply(mesa[,1:nitem], 1, paste, sep = "",
      collapse = "")
   if(!is.null(colnames(X))) names(mesa) <- c(colnames(X), "Freq")
   mesa <- mesa[order(rownames(mesa)),]
   if(is.null(which.npos)) {
      which.sub <- 0:nitem
   } else {
      which.sub <- unique(which.npos)
      if(any(which.sub < 0 | which.sub > nitem))
         stop("'which.npos' must be non-negative and not greater than the number of items.\n")
   }
   code <- "digraph G {\n"
   code <- c(code, "node [shape = plaintext, fontsize = 14]\n")
   return.sub <- which.sub
   for (i in which.sub) {
      mesita <- subset(mesa, subset = rowSums(as.matrix(mesa[,1:nitem]) == "1") == i)
      mesita <- subset(mesita, mesita[,nitem+1] > threshold)
      if (nrow(mesita) > 0 ) {
         code <- c(code, paste("subgraph cluster", i, sep = "", collapse = ""), " {\n")
         code <- c(code, "color = white\n")
         code <- c(code, draw.subgraph(mesita))
         if (do.labels) {
            if (make.labels) {
                code <- c(code, paste('label = "', i, ' positive"', sep = "", collapse = ""), "\n")
             } else {
                code <- c(code, "label = ", labels[i], "\n")
            }
         } # end of labelling subgraphs
         code <- c(code, "}\n")
      } else {   # no patterns for this number positive
         return.sub <- return.sub[return.sub != i] # remove from vector to return
      }
   }
   code <- c(code, "}\n")
   res <- list(which.npos = return.sub,
      new.order = new.order, code = code)
   class(res) <- "draw.latent"
   res
} # end of draw.latent   

