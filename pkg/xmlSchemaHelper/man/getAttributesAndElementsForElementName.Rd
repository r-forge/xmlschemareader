\name{getAttributesAndElementsForElementName}
\alias{getAttributesAndElementsForElementName}
\title{getAttributesAndElementsForElementName}
\description{Gives the type and restrictions on the values of the child elements and attributes for a given element in the document}
\usage{getAttributesAndElementsForElementName(theEName, theXMLdoc, logLevel = 1)}
\arguments{
  \item{theEName}{(string) The name of the element whose details are needed}
  \item{theXMLdoc}{(XMLInternalDocument OR string) Either: the XML schema document in which the element is present. This should have been read using xmlParseDoc or be supplied in the R workspace.
OR the path to the XML document which is to be read.}
  \item{logLevel}{The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0}
}

\value{The list of the details of the attributes and the child elements for the given element.
The list has two elements named "attributes" and "subelements" for the respective information. 
In case the type definiton of the given element is not present in the document, both the "attributes" and the "subelements" are assigned a NULL value. }

\author{Lucy Bastin and Ankita Goyal}





