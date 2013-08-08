\name{map.ellipse.array}
\alias{map.ellipse.array}
\title{Ellipse array}
\usage{
  map.ellipse.array(object, variable, dim = c(1, 2),
    draw.ellipses = TRUE, ncol = 2,
    titles = levels(variable), main.title = "", ...)
}
\description{
  Create seperate maps with ellipses for each level in a
  factor arranged in an array.
}
\examples{
\dontrun{
example(soc.mca)
map.ellipse.array(result, active[,1])
}
}

