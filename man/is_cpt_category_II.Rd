% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{is_cpt_category_II}
\alias{is_cpt_category_II}
\title{Validate HCPCS Level I (CPT) Category II Code}
\source{
\href{https://www.ama-assn.org/practice-management/cpt/category-ii-codes}{AMA Category II Codes}
}
\usage{
is_cpt_category_II(hcpcs_code)
}
\arguments{
\item{hcpcs_code}{\verb{<chr>} string}
}
\value{
\verb{<lgl>} \code{TRUE} if valid, otherwise \code{FALSE}
}
\description{
A valid CPT Category II code is:
}
\details{
\itemize{
\item 5 characters long
\item Begins with any 4 digits, \verb{[0-9]\{4\}}, and
\item Ends with an \verb{[F]}
}

Category II codes are supplemental tracking codes that are used to track
services on claims for performance measurement and are not billing codes.

These codes are intended to facilitate data collection about quality of care
by coding certain services and/or test results that support performance
measures and that have been agreed upon as contributing to good patient care.

Some codes in this category may relate to compliance by the health care
professional with state or federal law. The use of these codes is optional.
The codes are not required for correct coding and may not be used as a
substitute for Category I codes.

Services/procedures or test results described in this category make use of
alpha characters as the 5th character in the string (i.e., 4 digits followed
by an alpha character).

These digits are not intended to reflect the placement of the code in the
regular (Category I) part of the CPT code set.

Also, these codes describe components that are typically included in an
evaluation and management service or test results that are part of the
laboratory test/procedure. Consequently, they do not have a relative value
associated with them.
}
\examples{
x <- c('T1503', 'G0478', '81301', '69641', '0583F', '0779T', NA)

is_cpt_category_II(x)

x[which(is_cpt_category_II(x))]

}
