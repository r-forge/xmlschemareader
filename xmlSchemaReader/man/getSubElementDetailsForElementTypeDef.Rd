\name{getSubElementDetailsForElementTypeDef}
\alias{getSubElementDetailsForElementTypeDef}
\title{getSubElementDetailsForElementTypeDef}
\description{Gives the type and the restrictions on value for each of the child elements of the element whose type definition is given}
\usage{getSubElementDetailsForElementTypeDef(eTypeDef, fullDocument, 
    namespaces, logLevel = 1)}
\arguments{
  \item{eTypeDef}{Type definition of the element, the details of whose child elements are required}
  \item{fullDocument}{The XML Schema document in which the element and its type definition are present}
  \item{namespaces}{The namespaces present in the document}
  \item{logLevel}{The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0}
}

\value{The list containing the details of all the child elements in the given element's Type Definition}

\author{Lucy Bastin and Ankita Goyal}





