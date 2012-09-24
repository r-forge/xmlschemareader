\name{getElementRestrictionBase}
\alias{getElementRestrictionBase}
\title{getElementRestrictionBase}
\description{Gives the base type for the restriction on the value of the given element}
\usage{getElementRestrictionBase(element, namespaces, logLevel = 2)}
\arguments{
  \item{element}{The element, the base type of whose value, needs to be found}
  \item{namespaces}{The namespaces present in the XML Schema document containing the element}
  \item{logLevel}{The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0}
}

\value{Returns the Base Type of the Restriction on the value of the element,  if one is present, else returns NULL  }

\author{Lucy Bastin and Ankita Goyal}





