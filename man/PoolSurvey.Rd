% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PanelSurvey.R
\name{PoolSurvey}
\alias{PoolSurvey}
\title{PoolSurvey Class}
\description{
This class represents a collection of surveys grouped by specific periods (e.g., monthly, quarterly, annual).
It provides methods to access and manipulate the grouped surveys.
}
\keyword{PoolSurvey}
\keyword{Surveymethods,}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{surveys}}{A list containing the grouped surveys.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PoolSurvey-new}{\code{PoolSurvey$new()}}
\item \href{#method-PoolSurvey-get_surveys}{\code{PoolSurvey$get_surveys()}}
\item \href{#method-PoolSurvey-print}{\code{PoolSurvey$print()}}
\item \href{#method-PoolSurvey-clone}{\code{PoolSurvey$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PoolSurvey-new"></a>}}
\if{latex}{\out{\hypertarget{method-PoolSurvey-new}{}}}
\subsection{Method \code{new()}}{
Initializes a new instance of the PoolSurvey class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PoolSurvey$new(surveys)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{surveys}}{A list containing the grouped surveys.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PoolSurvey-get_surveys"></a>}}
\if{latex}{\out{\hypertarget{method-PoolSurvey-get_surveys}{}}}
\subsection{Method \code{get_surveys()}}{
Retrieves surveys for a specific period.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PoolSurvey$get_surveys(period = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{period}}{A string specifying the period to retrieve (e.g., "monthly", "quarterly").}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A list of surveys for the specified period.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PoolSurvey-print"></a>}}
\if{latex}{\out{\hypertarget{method-PoolSurvey-print}{}}}
\subsection{Method \code{print()}}{
Prints metadata about the PoolSurvey object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PoolSurvey$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PoolSurvey-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PoolSurvey-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PoolSurvey$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
