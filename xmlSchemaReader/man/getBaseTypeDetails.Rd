\name{getBaseTypeDetails}
\alias{getBaseTypeDetails}
\title{getBaseTypeDetails}
\description{Gives the details of the base type of a given data type}
\usage{getBaseTypeDetails(inType, theDoc, namespaces, logLevel = 1)}
\arguments{
  \item{inType}{The data type, the details of whose base type are to be found, This can EITHER be a string, if the type is named, or the element itself, to check whether a type is defined inline}
  \item{theDoc}{All or part of the XML schema document containing the type definition of the given data type}
  \item{namespaces}{The namespaces present in the document}
  \item{logLevel}{The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0}
}



\author{Lucy Bastin and Ankita Goyal}





