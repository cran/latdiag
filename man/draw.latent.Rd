\name{draw.latent}
\alias{draw.latent}
\alias{print.draw.latent}
\alias{plot.draw.latent}
\Rdversion{1.1}
\title{Draw latent scale diagram}
\description{
Writes a file of commands for the dot program to draw a graph
proposed by Rosenbaum (1987)
and useful for checking for non-intersecting
item characteristic curves which are a property of various
sorts of latent scale including Guttman, Rasch and the Mokken
double monotone scale.
}
\usage{
draw.latent(mat, rootname = NULL, threshold = 0, which.npos = NULL,
   labels = NULL, reorder = TRUE)
\method{print}{draw.latent}(x, ...)
\method{plot}{draw.latent}(x, graphtype = "png", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{A matrix or data.frame of binary item responses}
  \item{rootname}{The commands will be written to \code{rootname.dt}.
If NULL they will be written
to the standard output}
  \item{threshold}{Patterns are only printed if more frequent than threshold,
defaults to 0 meaning all those which actually occur are printed}
  \item{which.npos}{Which values of number of items positive to print, 
NULL means all and is the default. Duplicates are removed}
  \item{labels}{Labels for subgraphs, NULL means none, a character 
vector supplies the labels, otherwise labelled as n positive}
  \item{reorder}{logical, put the items in ascending order of prevalence,
defaults to TRUE}
  \item{x}{An object of class \code{draw.latent}}
  \item{graphtype}{Character: one of the graph types supported by \code{dot}}
  \item{...}{Other arguments}
}
\details{
The \code{plot} method actually does the plotting and invisibly
returns the result of the \code{system} command which executes \code{dot}.
The output file will be named with the rootname followed
by the graph type (after a dot).
The \code{print} method prints some details.
The routine does not draw the graph itself but leaves that
to the \code{dot} program from \code{graphviz} which you need
to install.

More extensive documentation is provided in the
documentation directory.
}
\value{
Outputs the commands to
draw the patterns and in addition returns:
   \item{rootname}{the rootname for the command file}
  \item{which.npos}{which values of items positive were printed. Differs
from input parameter if for some there were no valid
patterns to print or duplicates have been
removed}
  \item{new.order}{order of original items from left to right in displayed
diagram. 
If new.order==TRUE new.order[i] is the index in the original dataset
of the $i$th item in increasing prevalence}
}
\references{
P R Rosenbaum.
Probability inqualities for latent scales.
\emph{British Journal of Mathematical and Statistical Psychology},
  \bold{40}: 157--168, 1987
}
\author{Michael Dewey}
\examples{
set.seed(20150114)
mat <- cbind(
   sample(0:1, 100, prob = c(0.2, 0.8), replace = TRUE),
   sample(0:1, 100, prob = c(0.4, 0.6), replace = TRUE),
   sample(0:1, 100, prob = c(0.5, 0.5), replace = TRUE),
   sample(0:1, 100, prob = c(0.6, 0.4), replace = TRUE),
   sample(0:1, 100, prob = c(0.8, 0.2), replace = TRUE)
)
res <- draw.latent(mat, rootname = "mat")
#
# now need to plot(res, graphtype = "png")
#
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
