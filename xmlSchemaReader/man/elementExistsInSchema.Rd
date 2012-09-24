\name{elementExistsInSchema}
\alias{elementExistsInSchema}
\title{elementExistsInSchema}
\description{Checks whether the given element is present as a complexType element in the given XML Schema document}
\usage{elementExistsInSchema(eName, theDoc, namespaces, logLevel = 2)}
\arguments{
  \item{eName}{The name of the element which to be searched in the document }
  \item{theDoc}{The XML Schema document in which to search for the element}
  \item{namespaces}{The namespaces present in the document}
  \item{logLevel}{The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0}
}

\value{Returns true if the element is present as a complexType in the schema, otherwise false}

\author{Lucy Bastin and Ankita Goyal}





