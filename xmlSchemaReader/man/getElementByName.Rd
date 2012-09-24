\name{getElementByName}
\alias{getElementByName}
\title{getElementByName}
\description{Searches for a given element in the given document}
\usage{getElementByName(en, theDoc, namespaces, logLevel = 2)}
\arguments{
  \item{en}{The name of the element which is to be searched for.
The element may occur anywhere and as a simpleType element or as a complexType element or as a child element of some other element.}
  \item{theDoc}{The XML Schema document in which to search for the element}
  \item{namespaces}{The namespaces in the document}
  \item{logLevel}{The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0}
}

\value{Return the first occurence of the element with the given name, if it is present, else return NULL}

\author{Lucy Bastin and Ankita Goyal}





