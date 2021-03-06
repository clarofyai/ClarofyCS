% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clarofy_functions.R
\name{ClarofyRecommend}
\alias{ClarofyRecommend}
\title{Collect recommendations resulting from the completed optimisation job}
\usage{
ClarofyRecommend(url, id)
}
\arguments{
\item{url}{API url}

\item{id}{Session id created by \link{CreateSession}}
}
\description{
Collect optimal results from job
}
\section{Optimisation Workflow}{
\itemize{
\item Returns optimal results from completed job
}
}

\examples{

\dontrun{

ProcessControlRecommendations <- ClarofyRecommend(url = url, id = sessionID)

}

}
