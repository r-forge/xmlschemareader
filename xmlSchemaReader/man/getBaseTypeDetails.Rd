\name{getBaseTypeDetails}
\alias{getBaseTypeDetails}
\title{getBaseTypeDetails}
\description{Gives the details of the base type of a given data type}
\usage{getBaseTypeDetails(inType, theDoc, namespaces, logLevel = 2)}
\arguments{
  \item{inType}{The data type, the details of whose base type are to be found}
  \item{theDoc}{All or part of the XML schema document containing the type definition of the given data type}
  \item{namespaces}{The namespaces present in the document}
  \item{logLevel}{The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0}
}

\value{Returns a list containing all the appropriate details of the base type of the given type. 
The parameters which are not valid in some cases are assigned NULL values there.}

\author{Lucy Bastin and Ankita Goyal}





