\name{getDirectTypeDefForElementName}
\alias{getDirectTypeDefForElementName}
\title{getDirectTypeDefForElementName}
\description{Gives the definition of the data type of the element given the name of the element}
\usage{getDirectTypeDefForElementName(en, theDoc, namespaces, logLevel = 1)}
\arguments{
  \item{en}{The name of the element, whose type definition is required}
  \item{theDoc}{The XML Schema document containing the element and the definition of its datatype}
  \item{namespaces}{The namespaces present in the document}
  \item{logLevel}{The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0}
}

\value{Returns the definition of the data type of the element whose name is given, if the element is present in the document, NULL otherwise}

\author{Lucy Bastin and Ankita Goyal}





