\name{getElementByName}
\alias{getElementByName}
\title{getElementByName}
\description{Searches for a given element in the given document}
\usage{getElementByName(en, document, namespaces)}
\arguments{
  \item{en}{The name of the element which is to be searched for.
The element may occur anywhere and as a simpleType element or as a complexType element or as a child element of some other element.}
  \item{document}{The XML Schema document in which to search for the element}
  \item{namespaces}{The namespaces in the document}
}

\value{Return the first occurence of the element with the given name, if it is present, else return NULL}

\author{Lucy Bastin and Ankita Goyal}





