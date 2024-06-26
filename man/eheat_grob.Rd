% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-complexheatmap.R
\name{eheat_grob}
\alias{eheat_grob}
\title{Creates a gTree object from the Heatmap}
\usage{
eheat_grob(x, ...)
}
\arguments{
\item{x}{A \link[ComplexHeatmap:HeatmapList-class]{HeatmapList-class} object.}

\item{...}{
  Arguments passed on to \code{\link[grid:grid.grab]{grid::grid.grabExpr}}
  \describe{
    \item{\code{warn}}{ An integer specifying the amount of warnings
    to emit.  0 means no warnings, 1 means warn when it is
    certain that the grab will not faithfully represent the
    original scene. 2 means warn if there's any possibility
    that the grab will not faithfully represent the
    original scene.
  }
    \item{\code{wrap}}{ A logical indicating how the output should
    be captured. If \code{TRUE}, each non-grob element on the
    display list is captured by wrapping it in a grob.
  }
    \item{\code{wrap.grobs}}{ A logical indicating whether, if we are wrapping
    elements (\code{wrap=TRUE}), we should wrap grobs (or just
    wrap viewports).
  }
    \item{\code{width,height}}{ Size of the device used for temporary
    rendering. }
    \item{\code{device}}{ A function that opens a graphics device for temporary
    rendering. By default this is an off-screen, in-memory device
    based on the \code{pdf} device, but this default device may not be
    satisfactory when using custom fonts.
  }
  }}
}
\value{
A \link[grid:grid.grob]{gTree} object.
}
\description{
Creates a gTree object from the Heatmap
}
\examples{
h <- ggheat(matrix(rnorm(100), 10))
eheat_grob(h)
}
