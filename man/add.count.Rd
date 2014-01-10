\name{add.count}
\alias{add.count}
\title{Add a new layer of points on top of an existing plot with output from the
min_cut function}
\usage{
add.count(x, p, label = TRUE, ...)
}
\arguments{
  \item{x}{a matrix created by the min_cut function}

  \item{p}{is a ggplot object, preferably from one of the
  mapping functions in soc.ca}

  \item{label}{if TRUE the labels of points will be shown}

  \item{...}{further arguments are passed on to geom_path,
  geom_point and geom_text}
}
\description{
Add a new layer of points on top of an existing plot with
output from the min_cut function
}

