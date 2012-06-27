\name{getAttributesAndElementsForElementName}
\alias{getAttributesAndElementsForElementName}
\title{getAttributesAndElementsForElementName}
\description{Gives the type and restrictions on the values of the child elements and attributes for a given element in the document}
\usage{getAttributesAndElementsForElementName(theEName, theXMLdoc, theNamespace)}
\arguments{
  \item{theEName}{The name of the element whose details are needed}
  \item{theXMLdoc}{The XML schema document in which the element is present}
  \item{theNamespace}{The namespaces present in the document}
}

\value{The list of the details of the attributes and the child elements for the given element.
The list has two elements named "attributes" and "subelements" for the respective information. 
In case the type definiton of the given element is not present in the document, both the "attributes" and the "subelements" are assigned a NULL value. }

\author{Lucy Bastin and Ankita Goyal}





