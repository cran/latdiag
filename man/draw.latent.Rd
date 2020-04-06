\name{draw.latent}
\alias{draw.latent}
\alias{print.draw.latent}
\alias{plot.draw.latent}
\Rdversion{1.1}
\title{Draw latent scale diagram}
\description{
Writes a file of commands for the dot program to draw a graph
proposed by \insertCite{rosenbaum87}{latdiag}
and useful for checking for non-intersecting
item characteristic curves which are a property of various
sorts of latent scale including Guttman, Rasch and the Mokken
double monotone scale.
}
\usage{
draw.latent(mat, threshold = 0, which.npos = NULL,
   labels = NULL, reorder = TRUE)
\method{print}{draw.latent}(x, rootname = NULL, ...)
\method{plot}{draw.latent}(x, rootname = NULL, graphtype = "pdf", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{A matrix or data.frame of binary item responses}
  \item{threshold}{Patterns are only printed if more frequent than threshold,
defaults to 0 meaning all those which actually occur are printed}
  \item{which.npos}{Which values of number of items positive to print, 
NULL means all and is the default. Duplicates are removed}
  \item{labels}{Labels for subgraphs, NULL means none, a character 
vector supplies the labels, otherwise labelled as n positive}
  \item{reorder}{logical, put the items in ascending order of prevalence,
defaults to TRUE}
  \item{x}{An object of class \code{draw.latent}}
  \item{rootname}{Character: the rootname of the file of dot commands}
  \item{graphtype}{Character: one of the graph types supported by \code{dot}}
  \item{...}{Other arguments}
}
\details{
The \code{draw.latent} function produces the dot commands
which will draw the graph.

The \code{print} method prints some details.
If \code{rootname} is supplied it will be used as the rootname
of the file of dot commands which will have file type \code{.gv}.
If \code{rootname} is NULL (which is the default) just some details
about the result are printed.

The \code{plot} method actually does the plotting and invisibly
returns the result of the \code{system} command which executes \code{dot}.
The output file will be named with the rootname followed
by the graph type (after a dot).
The routine does not draw the graph itself but leaves that
to the \code{dot} program from \code{graphviz} which you need
to install and have on your path.

More extensive documentation is provided in the
vignette.
}
\value{
Returns a list containing:
  \item{which.npos}{which values of items positive were printed. Differs
   from input parameter if for some there were no valid
   patterns to print or duplicates have been
   removed}
  \item{new.order}{order of original items from left to right in displayed
   diagram. 
   If new.order==TRUE new.order[i] is the index in the original dataset
   of the \eqn{i}th item in increasing prevalence}
   \item{code}{a character vector of the dot commands}
}
\author{Michael Dewey}
\note{The parameter rootname is now supplied to the plot and
print functions and not to \code{draw.latent}}
\references{
\insertRef{rosenbaum87}{latdiag}
}
\examples{
set.seed(20150114)
mat <- cbind(
   sample(0:1, 100, prob = c(0.2, 0.8), replace = TRUE),
   sample(0:1, 100, prob = c(0.4, 0.6), replace = TRUE),
   sample(0:1, 100, prob = c(0.5, 0.5), replace = TRUE),
   sample(0:1, 100, prob = c(0.6, 0.4), replace = TRUE),
   sample(0:1, 100, prob = c(0.8, 0.2), replace = TRUE)
)
res <- draw.latent(mat)
#
# now need to plot(res, rootname = "mat", graphtype = "pdf")
#
}
\keyword{hplot}
