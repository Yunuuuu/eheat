% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gganno.R
\docType{class}
\name{gganno}
\alias{gganno}
\alias{ggAnno-class}
\title{Build ggAnno Class}
\usage{
gganno(ggfn, ..., matrix = NULL, which = NULL, width = NULL, height = NULL)
}
\arguments{
\item{ggfn}{A function or formula, accept a initial \link[ggplot2:ggplot]{ggplot}
data as the input and must return a \link[ggplot2:ggplot]{ggplot} object. For the
passed data details, please see section \code{ggfn}.

If a \strong{function}, it is used as is.

If a \strong{formula}, e.g. \code{~ .x + 2}, it is converted to a function with up to
two arguments: \code{.x} (single argument) or \code{.x} and \code{.y} (two arguments). The
\code{.} placeholder can be used instead of \code{.x}.  This allows you to create
very compact anonymous functions (lambdas) with up to two inputs.}

\item{...}{Additional arguments passed to \code{ggfn}.}

\item{matrix}{A matrix, if it is a simple vector, it will be converted to a
one-column matrix. Data.frame will also be coerced into matrix. If \code{NULL},
the matrix from heatmap will be used. You can also provide a function to
transform the matrix.}

\item{which}{Whether it is drawn as a column annotation or a row annotation?}

\item{width}{The width of the plotting region (the viewport) that the annotation is drawn. If it is a row annotation, the width must be an absolute unit. Since the \code{AnnotationFunction} object is always contained by the \code{\link[ComplexHeatmap]{SingleAnnotation-class}}object, you can only set the width of row annotations or height of column annotations, while e.g. the height of the row annotation is always \code{unit(1, "npc")} which means it always fully filled in the parent \code{SingleAnnotation} and only in \code{\link[ComplexHeatmap]{SingleAnnotation}} or even \code{\link[ComplexHeatmap]{HeatmapAnnotation}} can adjust the height of the row annotations.}

\item{height}{The height of the plotting region (the viewport) that the annotation is drawn. If it is a column annotation, the width must be an absolute unit.}
}
\value{
A \code{ggAnno} object.
}
\description{
Plot heatmaps layer by layer
}
\details{
\code{gganno} can be seamlessly combined with both
\link[ComplexHeatmap:Heatmap]{Heatmap} and \link{ggheat}, although legends will not be
extracted in the later case.
}
\note{
Maintaining the internal limits along the heatmap to align well with
\code{ComplexHeatmap} is important.
}
\section{ggfn}{


\code{ggfn} accept a ggplot2 object with a default data and mapping created by
\code{ggplot(data, aes(.data$x))} / \code{ggplot(data, ggplot2::aes(y = .data$y))}.
The original matrix will be converted into a long-data.frame (\code{gganno} always
regard row as the observations) with following columns:
\itemize{
\item \code{.slice}: the slice row (which = \code{"row"}) or column (which = \code{"column"})
number.
\item \code{.row_names} and \code{.column_names}: the row and column names of the original
matrix (only applicable when names exist).
\item \code{.row_index} and \code{.column_index}: the row and column index of the original
matrix.
\item \code{x} / \code{y}: indicating the x-axis (or y-axis) coordinates. Don't use
\link[ggplot2:coord_flip]{coord_flip} to flip coordinates as it may disrupt
internal operations.
\item \code{value}: the actual matrix value of the annotation matrix.
}
}

\examples{
draw(gganno(function(p) {
    p + geom_point(aes(y = value))
}, matrix = rnorm(10L), height = unit(10, "cm"), width = unit(0.7, "npc")))
}
\seealso{
\link{eanno}
}
