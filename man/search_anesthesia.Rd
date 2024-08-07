% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pfs.R
\name{search_anesthesia}
\alias{search_anesthesia}
\title{Anesthesia Conversion Factors}
\usage{
search_anesthesia(mac = NULL, locality = NULL, ...)
}
\arguments{
\item{mac}{\verb{<chr>} 5-digit Medicare Administrative Contractor (MAC) ID}

\item{locality}{\verb{<chr>} vector of 2-digit Locality IDs}

\item{...}{These dots are for future extensions and must be empty.}
}
\value{
a \link[tibble:tibble-package]{tibble}
}
\description{
The Medicare approved amount for anesthesia service is calculated using the
following formula:
}
\details{
Allowed Amount = ((Anesthesia Base Units + Billed Minutes) / 15) x Conversion Factor
}
\examples{
search_anesthesia(locality = "01", mac = "10212")

}
\seealso{
Other Physician Fee Schedule Sources: 
\code{\link{search_gpcis}()},
\code{\link{search_opps}()},
\code{\link{search_payment}()},
\code{\link{search_rvus}()}
}
\concept{Physician Fee Schedule Sources}
