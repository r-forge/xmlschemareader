library("XML")

# --------------------------------------------------- Declare functions ----------------------------------------------------------------------------- #
getAttributesAndElementsForElementName <- function
### Gives the type and restrictions on the values of the child elements and attributes for a given element in the document
(theEName,
### (string) The name of the element whose details are needed
theXMLdoc,
### (XMLInternalDocument OR string) Either: the XML schema document in which the element is present. This should have been read using xmlParseDoc or be supplied in the R workspace.
### OR the path to the XML document which is to be read.
logLevel=1
### The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0
)
{
  if (is.character(theXMLdoc)) 
  {
     docPath <- theXMLdoc
     theXMLdoc<- xmlParseDoc(docPath)
  }
  # TODO catch the problem if there is still no valid document to work with
  
  theNamespace <- getNamespaceDefinitions(theXMLdoc)
  
  #------------------------- TEMP - print out details --------------------------- #
 
  print(paste("Getting typedef for element name: ",theEName, sep=""))
  
    print("----------------------------------------------------------------")
    output <- paste("Element:  ",theEName,sep="")
    print(output)
 #--------------end TEMP --------------------------------------------------- #

	eTD <- getDirectTypeDefForElementName(theEName, theXMLdoc, theNamespace)
  # Get the element's type definition, which contains the information on child elements and attributes
  
	if (length(eTD)>0)
  # If the type definiton of the element is present in the schema, then get the details for its child elements and its attributes, otherwise assign "NULL"
	{
	 
		eList <- getSubElementDetailsForElementTypeDef(eTD, theXMLdoc, theNamespace, logLevel)
    # This gives the details for the child elements of the given element

		aList <- getAttributeDetailsForElementTypeDef(eTD, theXMLdoc, theNamespace, logLevel)
    # This gives the details for the attributes of the given element
	}
  
	else  
	{
		eList<-NULL
		aList<-NULL
	}
	
	summaryToReturn <- list("attributes"=aList, "subelements"=eList)
	summaryToReturn
### The list of the details of the attributes and the child elements for the given element.
### The list has two elements named "attributes" and "subelements" for the respective information. 
### In case the type definiton of the given element is not present in the document, both the "attributes" and the "subelements" are assigned a NULL value. 

}



getSubElementDetailsForElementTypeDef <- function
### Gives the type and the restrictions on value for each of the child elements of the element whose type definition is given
(eTypeDef,
### Type definition of the element, the details of whose child elements are required
fullDocument,
### The XML Schema document in which the element and its type definition are present
namespaces,
### The namespaces present in the document
logLevel=1
### The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0
)
{

  if (!exists("namespaces"))
  {
      namespaces<- getNamespaceDefinitions(fullDocument)
  }
	
  elToReturn=list()
	elCounter=0

	xpath.expression <- ".//xs:element"
  # An XPath Expression to help locate the child elements

  #------------------------- TEMP - print out details --------------------------- #
  if (logLevel==2)
	{
    print(paste("Getting subelements for typedef: ",xmlGetAttr(eTypeDef, 'name'), " with xpath ",xpath.expression, sep=""))
  }
  #------------------------------------------------------------------------------ #

	elementList <- getNodeSet(doc=eTypeDef ,path=xpath.expression, namespaces)
  # This gives the list of all the child elements for the given type definition of the element

	if (length(elementList ) >0) 
  # If there are child elements for the given element's type definition
	{
		for (i in 1:length(elementList )) 
    # Loop over all the child elements and get the details for each
		{
      elCounter = elCounter + 1
      # A counter for the number of child elements
      
      elDetails <- getElementDetails(elementList[[i]], fullDocument, namespaces, logLevel)
      # Get the type and restrictions on value of the child element
      
			elToReturn[[elCounter]] <- elDetails
      # Put the details of the child element in the list

      #---------------- TEMP - print out details for the child element -------------------- #
      if (logLevel==1)
      {
        output <- paste("Sub-element: ",xmlGetAttr(elementList[[i]], 'name'),sep="")
        output <- paste(output, "-----Type=", elDetails$type, sep="")
        if (!is.null(elDetails$minOccurs)) {  output <- paste(output, "    MinOccurs=", elDetails$minOccurs, sep="")  }
        if (!is.null(elDetails$maxOccurs)) {  output <- paste(output, "  MaxOccurs=", elDetails$maxOccurs, sep="")  }
        if (!is.null(elDetails$multiple)) {  output <- paste(output, "  Multiple=", elDetails$multiple, sep="")  }
        if (!is.null(elDetails$minEx)) {  output <- paste(output, "    Minimum exclusive:  ", elDetails$minEx, sep="")  }
        if (!is.null(elDetails$minIn)) {  output <- paste(output, "     Minimum inclusive:  ", elDetails$minIn, sep="")  }
        if (!is.null(elDetails$maxEx)) {  output <- paste(output, "     Maximum exclusive:  ", elDetails$maxEx, sep="")  }
        if (!is.null(elDetails$maxIn)) {  output <- paste(output, "     Maximum inclusive:  ", elDetails$maxIn, sep="")  }
        print(output)
      }
      #------------------------------------------------------------------------------------ #
      
		}
	}
	
  elToReturn
### The list containing the details of all the child elements in the given element's Type Definition
  
}



getAttributeDetailsForElementTypeDef <- function
### Gives the type and the restrictions on value of each of the attributes for the element whose type definition is given
(eTypeDef,
### Type definition of the element, the details of whose attributes are required
fullDocument,
### The XML Schema document in which the element and its type definition are present
namespaces,
### The namespaces present in the document
logLevel=1
### The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0
)  
{
  if (!exists("namespaces"))
  {
      namespaces<- getNamespaceDefinitions(fullDocument)
  }

  attToReturn=list()
  attCounter=0
  
	xpath.expression <- ".//xs:attribute"
	# An XPath Expression to help locate the attributes

	attributeList <- getNodeSet(doc=eTypeDef,path=xpath.expression, namespaces)
  # This gives the list of all the attributes for the given type definition of the element
	    
	if (length(attributeList) >0) 
	# If there are attributes for the given element's type definition
	{
    
		for (i in 1:length(attributeList))
		# Loop over all the attributes in the list and get the details for each
    {
			attCounter = attCounter + 1
			# A counter for the number of attributes
      
			attName <- xmlGetAttr(attributeList[[i]],'name')
			# Get the name of the attribute

			attReq <- xmlGetAttr(attributeList[[i]],'use')	
			# Get the 'use' of the attribute - this will tell whether an attribute is required
			
			attType <- xmlGetAttr(attributeList[[1]],'type')	
			# Get the 'type' of the attribute - this will tell how its string value should be used
			
			attMinEx <- xmlGetAttr(attributeList[[1]],'minExclusive')
			attMaxEx <- xmlGetAttr(attributeList[[1]],'maxExclusive')
			attMinIn <- xmlGetAttr(attributeList[[1]],'minInclusive')
			attMaxIn <- xmlGetAttr(attributeList[[1]],'maxInclusive')
      
      if (!is.null(attType))
        
      # If the 'type' is not NULL, then get the details of 'base type' of that type
			{
				attType <- getBaseTypeDetails (attType, fullDocument, namespaces, logLevel)
			}
      else
      {
        # If the type is not specified as an attribute, it may be defined inline as a simple element - check!
        attType <- getBaseTypeDetails (attributeList[[1]], fullDocument, namespaces, logLevel)
      }
			aT <- attType
			attType <- aT$type
			attMinEx <- aT$minEx
			attMaxEx <- aT$maxEx
			attMinIn <- aT$minIn
			attMaxIn <- aT$maxIn
      

			thisAttList <- list("name"=attName, "type"=attType, "required"=attReq, "minEx"=attMinEx, "maxEx"=attMaxEx, "minIn"=attMinIn, "maxIn"=attMaxIn)
      # Make a list of all the details of the attribute

			attToReturn[[attCounter]] <- thisAttList
			# Put the details of each attribute in the list
			
			#---------------- TEMP - print out details for the attribute ------------------ #
      if (logLevel==1)
      {
        print(paste("Attribute name: ",attName, " - type: ", attType, " - Required: ",attReq,sep=""))
        
        if (!is.null(attMinEx)) print(paste("Min. exclusive value: ",attMinEx,sep=""))
        if (!is.null(attMaxEx)) print(paste("Max. exclusive value: ",attMaxEx,sep=""))
        if (!is.null(attMinIn)) print(paste("Min. inclusive value: ",attMinIn,sep=""))
        if (!is.null(attMaxIn)) print(paste("Max. inclusive value: ",attMaxIn,sep=""))
      }
      #------------------------------------------------------------------------------ #
      
		}
	}
	
  attToReturn
  ### The list containing the details of all the attributes in the given element's Type Definition
  
}



getElementByName<- function
### Searches for a given element in the given document
(en,
### The name of the element which is to be searched for.
### The element may occur anywhere and as a simpleType element or as a complexType element or as a child element of some other element.
theDoc,
### The XML Schema document in which to search for the element
namespaces,
### The namespaces in the document
logLevel=1
### The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0
)
{

    if (!exists("namespaces"))
   {
      namespaces<- getNamespaceDefinitions(theDoc)
   }

	xpath.expression<-paste("//xs:element[(@name= '",en ,"')]",sep="")
  # An XPath Expression to find an element which has an attribute 'name' holding the name of the element which is to be searched for in the document

	#------------------------- TEMP - print out details --------------------------- #
  if (logLevel==2)
	{
    print(paste("Getting element ",en, " using xpath ", xpath.expression,sep=""))
  }
  #------------------------------------------------------------------------------ #

	eList <- getNodeSet(doc=theDoc,path=xpath.expression, namespaces)
  # Get the list of all the elements with the given name 
  
	if (length(eList)>0)
		eList[[1]]
	else
		NULL
	### Return the first occurence of the element with the given name, if it is present, else return NULL
  
}



getElementRestrictionBase <- function
### Gives the base type for the restriction on the value of the given element
(element,
### The element, the base type of whose value, needs to be found
namespaces,
### The namespaces present in the XML Schema document containing the element
logLevel=1
### The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0
)
{
	xpath.expression <- ".//xs:restriction"
  # An Xpath Expression to locate the restriction on the value of the element
  
	restrList <- getNodeSet(doc=element,path=xpath.expression, namespaces)
  # Get the list of all the restrictions on the value of the element
		
	if (length(restrList) > 0) 
  # If the restriction is present, return its base type else return NULL
	{
		xmlGetAttr(restrList[[1]],'base')
	}
  else
    NULL
### Returns the Base Type of the Restriction on the value of the element,  if one is present, else returns NULL  
  
}



getElementListBase <- function
### Gives the data type of the items in the element's list 
(element,
### The element which contains a list
namespaces,
### The namespaces present in the document containing the element
logLevel=1
### The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0

)
{
	xpath.expression <- ".//xs:list"
  # An XPath Expression to locate the list in the element
  
	restrList <- getNodeSet(doc=element,path=xpath.expression, namespaces)
  # Get a list of all the lists that the element contains
		
	if (length(restrList) > 0) 
  {
		xmlGetAttr(restrList[[1]],'itemType')
	}
  else
    NULL
### Returns the data type of the items that the list contains, if the given element contains a list, NULL otherwise
  
}



getElementExtensionBase <- function
### Gives the base type of the extension in an element
(element,
### The element for which the base type of the extension is to be found
namespaces,
### The namespaces present in the document containing the element
logLevel=1
### The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0
)
{
  xpath.expression <- ".//xs:extension"
  # An XPath Expression to locate the extension in the element
  
	extList <- getNodeSet(doc=element,path=xpath.expression, namespaces)
	# Get a list of all the extensions used in the element
  
	if (length(extList) > 0) 
	{
		xmlGetAttr(extList[[1]],'base')
	}
  else
    NULL
### Returns the base type of the extension in an element, if an extension is present, NULL otherwise
  
}



getBaseTypeDetails <- function
### Gives the details of the base type of a given data type
(inType,
### The data type, the details of whose base type are to be found, This can EITHER be a string, if the type is named, or the element itself, to check whether a type is defined inline
theDoc,
### All or part of the XML schema document containing the type definition of the given data type
namespaces,
### The namespaces present in the document
logLevel=1
### The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0
)
{

   # annoying appeasement of CMD notes generator to make it realise that this is global
  rBase <- NULL

  elMultiple <-FALSE
  minMaxDetails <- NULL
  minEx = NULL
  maxEx = NULL
  minIn = NULL
  maxIn = NULL
 
  elBaseType = NULL
  elTypeDefinition <- NULL # This gets used if the type is not straightfoward and has to be dug for
  
   # Initialize all other details to NULL. These will be populated if there's a numeric restriction.

  # Check whether a string type name was passed in, or an actual node
  if (is.character(inType))  
  {
  
  	if (substr(inType, start=1, stop=3)  != 'xs:')
  	# Check whether the given data type is a basic xs type. If it is not a basic xs type, then recursively get the base type
  	{	
      
      		elTypeDefinition<- getTypeDefinitionFromTypeDefName(inType, theDoc, namespaces, logLevel)
          # Get the type definition of the given data type
    } 
    else
    {  
          elBaseType <- classifyType(inType)
          # elTypeDefinition remains null, we will stop searching for a more complex type definition
    }
  }
  else 
  {
      # Look for a type definition inline instead

      xpath.expression <- ".//xs:simpleType"
      # An XPath Expression to help locate the attribute definition inline, if it's there
      
      elTD <- getNodeSet(doc=inType,path=xpath.expression, namespaces)
      
      elTypeDefinition <- elTD[[1]]
      # This gives the definition of the attribute type if it is inline
    
  }
  if (!is.null(elTypeDefinition))
  {
  		rt <- getBaseType(theDoc, elTypeDefinition, namespaces, logLevel) 
  		# This will give an idea of what kind of data type this is - integer, float, string. But it may take some recursive digging.
  		# First, look to see if this extends another type. Keep looking down the tree until there are no more extensions
  
  		elBaseType <- classifyType(rt[[1]])
      
  		# Check if the type that the given data type extends, is a restriction or a list
  			
  		xpath.expression <- ".//xs:list"
      # An XPath Expression to check if this is a list
      
  		listList <- getNodeSet(doc=rt[[2]],path=xpath.expression, namespaces)
      # Gives a list of all the lists present in the type
      
  		if (length(listList) > 0) 
      # If the data type has a list, get the base type of the items in the list
  		{
        
  			elMultiple=TRUE
        # This indicates that we have a space-delimited list of values
            
  		}
  		
      if (!is.null(rt))
      {
    			minMaxDetails <- getMinMaxValues(rt[[2]], namespaces)	
          minEx = minMaxDetails$minEx
    			maxEx = minMaxDetails$maxEx
    			minIn = minMaxDetails$minIn
    			maxIn = minMaxDetails$maxIn
      }
    
  }
  list('type'=elBaseType, 'multiple'=elMultiple, 'minEx'=minEx, 'maxEx'=maxEx , 'minIn'=minIn, 'maxIn'=maxIn)
  ### Returns a list containing all the appropriate details of the base type of the given type. 
  ### The parameters which are not valid in some cases are assigned NULL values there.
  
}



getDirectTypeDefForElementName <- function
### Gives the definition of the data type of the element given the name of the element
(en,
### The name of the element, whose type definition is required
theDoc,
### The XML Schema document containing the element and the definition of its datatype
namespaces,
### The namespaces present in the document
logLevel=1
### The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0
)
{
  
	el4 <- getElementByName(en, theDoc, namespaces, logLevel)
  # Get the element given its name
  
	if (length(el4)>0)
  # If the element is present in the document, then get the definition of its datatype
	{
	  getDirectTypeDefForElement(el4, theDoc, namespaces, logLevel)
	}
	else
  # If the element is not present in the document, then print and return NULL
	{
		print("No element was found")
		NULL
	}
### Returns the definition of the data type of the element whose name is given, if the element is present in the document, NULL otherwise
  
}



getDirectTypeDefForElement <- function
### Gives the definition of the data type of the element, given the element
(en,
### The element in the XML Schema document, whose type definition is required
theDoc,
### The XML Schema document containing the element
namespaces,
### The namespaces present in the document
logLevel=1
### The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0
)
{
	eTN <- getDirectTypeNameForElement(en, theDoc, namespaces, logLevel)
  # Get the data type of the given element
  
	getTypeDefinitionFromTypeDefName(eTN, theDoc, namespaces, logLevel)
  # Get the definition of the data type of the element

### Returns the definition of the data type of the given element in the given XML Schema document
  
}

getDirectTypeNameForElement<-function
### Gives the name of the data type of the given element
(en,
### The element, the name of whose data type is required
theDoc,
### The XML Schema document containing the element
namespaces,
### The namespaces present in the document
logLevel=1
### The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0
)
{
  
  eT <- xmlGetAttr(en,'type')
  # Get the type of the element
  
	if (is.null(eT)) 
	# If 'type' is null, then get the type from the restrictions, list and extensions, whichever is present
	{
    
		eT <-getElementRestrictionBase(en,namespaces, logLevel)
    # Get the data type of the restriction on the element
    
		if (is.null(eT))
    # If the restriction is not present on the element
		{
			
      eT <-getElementExtensionBase(en,namespaces, logLevel)
      # Get the data type of the extension
      
			if (is.null(eT))
      # If the extension is not present in the element
			{
			
        eT<- getElementListBase(en,namespaces, logLevel)
        # Get the data type of the list in the element
        
				if (is.null(eT))
        # If the list is present in the element
				{
          
					eT <-xmlGetAttr(en,'ref')
          # If nothing is present, there may be a reference. Get the type of the reference in the element
					
          #------------------------- TEMP - print out details --------------------------- #
          if (logLevel==2)
          {
            print(paste("Reference to a:  ",eT,sep=""))
          }
          #------------------------------------------------------------------------------ #
          
				} 
        else
        {
				  
          #------------------------- TEMP - print out details --------------------------- #
          if (logLevel==2)
          {
            print(paste("List of:  ",eT,sep=""))
          }
          #------------------------------------------------------------------------------ #
          
				}
        
			}
      else 
      {
			  
        #------------------------- TEMP - print out details --------------------------- #
        if (logLevel==2)
        {
          print(paste("Extends:  ",eT,sep=""))
        }
        #------------------------------------------------------------------------------ #
        
			}
      
		} 
    else 
    {
		
      #------------------------- TEMP - print out details --------------------------- #
      if (logLevel==2)
      {
        print(paste("Restricts:  ",eT,sep=""))
      }
      #------------------------------------------------------------------------------ #
      
		}
    
	}
  
	eT
### Returns the name of the data type of the element
  
}



getElementDetails <- function 
### Gives the details of the given element
(theEl,
### The element whose details are needed
theDoc,
### All or part of the XML Schema document containing the element and its definition
namespaces,
### The namespaces present in the document
logLevel=1
### The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0
)
{
	elType1<-getDirectTypeNameForElement(theEl, theDoc, namespaces, logLevel)
  # Get the name of the data type of the element

	elMinOccurs <- xmlGetAttr(theEl,'minOccurs')
	# Get the 'minOccurs' of the element - this will tell you whether it's required
	
	elMaxOccurs <- xmlGetAttr(theEl,'maxOccurs')	
	# Get the 'maxOccurs' of the element
	
	elMultiple=FALSE
	# This is a flag to indicate whether to expect a space-delimited list of values

	baseInfo <- getBaseTypeDetails(elType1, theDoc, namespaces, logLevel)
  # Get the details of the data type of the element

	elName<-xmlGetAttr(theEl, 'name')
  # Get the name of the element
  
	if (length(elName) == 0)
		elName<-xmlGetAttr(theEl, 'ref')
  # If the attribute 'name' is not present, then there is a reference. Get the name from the reference

	list('name'=elName, 'type'=baseInfo$type, 'minOccurs'=elMinOccurs, 'maxOccurs'=elMaxOccurs, 'multiple'=baseInfo$multiple, 'minEx'=baseInfo$minEx, 'maxEx'=baseInfo$maxEx , 'minIn'=baseInfo$minIn, 'maxIn'=baseInfo$maxIn)
### Returns a list giving all the details of the given element
  
}



getBaseType <- function
### Gives the name of the base type for the given definition of the data type
(theDoc,
### All or part of the XML Schema document containing the given type definition
typeDef,
### The given type definition
namespaces,
### The namespaces present in the document
logLevel=1
### The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0
)  
{
  tdToReturn <- typeDef

	xBase <- getDirectTypeNameForElement(typeDef, theDoc, namespaces, logLevel)
  # Get the name of the data type
  
	if (!is.null(xBase)) 
	{
    
		if (substr(xBase, start=1, stop=3)  != 'xs:')
    # If the data type is not a basic xs type
		{
		  
      #------------------------- TEMP - print out details --------------------------- #
      if (logLevel==2)
      {
        print(paste("GetBaseType: Getting typedef for ",xBase,sep=""))
      }
      #------------------------------------------------------------------------------ #
			
      tdToReturn <- getTypeDefinitionFromTypeDefName(xBase, theDoc, namespaces, logLevel)
      # Get the type definition of the data type
      
      xBase <- getDirectTypeNameForElement(tdToReturn, theDoc, namespaces, logLevel)
      # Get the name of the data type
      
      if (!is.null(xBase))
      {
        if (substr(xBase, start=1, stop=3)  == 'xs:')
        # If the data type is a basic xs type
        {
          list('type'=xBase, 'typedef'=tdToReturn)
  		  } 
        else
        {
    			theBaseType <- getBaseType(theDoc,tdToReturn, namespaces, logLevel) 
          # Get the name of the base type of this data type
          
    			theBaseType
        }
      }
    }
		else 
    # If the data type is a basic xs type, return the data type
    { 
      list('type'=xBase, 'typedef'=tdToReturn)
    }
    
	}
	else 
  # If the element does not have a type attribute, then the definition is simply the data type
	{ 
	  list('type'=tdToReturn, 'typedef'=tdToReturn)
    # TODO check this situation
    print("No type attribute: returning typedef twice")
	}
### Returns the name of the base type of the element
  
}



getTypeDefinitionFromTypeDefName <- function
### Gives the definition for the given data type
(typeDefName,
### The name of the data type
theDoc,
### The XML Schema document containing the data type and its definition
namespaces,
### The namespaces present in the document
logLevel=1
### The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0
)
{
	splittype <- strsplit(typeDefName, ":")
  # Strip off the namespace from the name of the data type if present
	
  if (length(splittype[[1]]) > 1) 
  # If the namespace is present, get the actual name of the data type
	{
		typeDefName <- splittype[[1]][[2]]
	}
	
	xpath.expression <- paste("//xs:simpleType[(@name= '",typeDefName ,"')]",sep="")
  # An XPath Expression to locate the data type as an element of type simpleType

	#------------------------- TEMP - print out details --------------------------- #
  if (logLevel==2)
  {
    print(paste("GetTypeDefinition - about to run xpath search with: ",xpath.expression, sep=""))
  }
  #------------------------------------------------------------------------------ #

	elTypeDefinition<- getNodeSet(doc=theDoc,path=xpath.expression, namespaces)
  # Get the list of the type definition if the data type is present as a simpleType element
  
	if (length(elTypeDefinition) ==0) 
  # If the type definition is not a simpleType element
	{
    
		xpath.expression <- paste("//xs:complexType[(@name= '",typeDefName ,"')]",sep="")
    # An XPath Expression to locate the data type as an element of type complexType
    
		elTypeDefinition<- getNodeSet(doc=theDoc,path=xpath.expression, namespaces)
    # Get the list of the type definition if the data type is present as a complexType element
    
	}
  
	if (length(elTypeDefinition) ==0) 
  # If the type definition is not even a complexType element, it is a reference
	{
	
    xpath.expression <- paste("//xs:element[(@name= '",typeDefName ,"')]",sep="")
    # An XPath Expression to locate the element using the 'name' attribute
    
    thisEL<- getNodeSet(doc=theDoc,path=xpath.expression, namespaces)
    # Get the list of all occurrences of the element
    
    elTypeDefinition <- getDirectTypeDefForElement(thisEL[[1]], theDoc, namespaces, logLevel)
    # Get the type definition from this element
    
    elTypeDefinition
    # Return the type definition of the data type
    
	}
  
	else 
  # Return the type definition of the data type if it is a simpleType of complexType element
	{
		elTypeDefinition[[1]]
	}	
  
### Return the type definition of the given data type
  
}



elementExistsInSchema<- function
### Checks whether the given element is present as a complexType element in the given XML Schema document
(eName,
### The name of the element which to be searched in the document 
theDoc,
### The XML Schema document in which to search for the element
namespaces,
### The namespaces present in the document
logLevel=1
### The level of logging that will be carried out: 0 (none) 1 (limited) or 2 (full). Optional - defaults to 0
)
{
  en <- getElementByName(eName, theDoc, namespaces, logLevel)
  # Get the element by its name
  
  eB <- getElementExtensionBase(en, namespaces, logLevel)
  # Get the base type of the extension of the element
  
  if(length(eB)>0)
  # If the extension is present, the element may be present as a complexType element
  {
    
    xpath.expression <- paste("//xs:complexType[(@name='", eB, "')]", sep="")
    # An XPath Expression to locate the element as a complexType
    
    eCT <- getNodeSet(doc=theDoc, path=xpath.expression, namespaces)
    # Get the list of all the occurences of the element as complexType
    
    if (length(eCT)==0)
    # If the element is not present as a complexType, there may be a namespace prefix
    {
      
      splitBase <- strsplit(eB, ":")
      # strip off the namespace
      
      if (length(splitBase[[1]])>1)
      # If the namespace is present, get the actual base type
      {
        
        eB <- splitBase[[1]][[2]]
        xpath.expression <- paste("//xs:complexType[(@name='", eB, "')]", sep="")
        # An XPath Expression to locate the element as a complexType
        
        eCT <- getNodeSet(doc=theDoc, path=xpath.expression, namespaces)
        # Get the list of all the occurences of the element as complexType
        
      }
      
    }
    if (length(eCT)>0)
    # If the element is present, return true, false otherwise
    {
      TRUE
    }
    else
      FALSE  
    
  }
  else
  {
    FALSE
  }  
### Returns true if the element is present as a complexType in the schema, otherwise false
  
}	

classifyType<-function
### Classifies attributes according to R data types
(theBaseType
 ### The xs: namespace-specific definition name of the value
)
{
  eBT <- NULL
  
  if ((theBaseType=='xs:decimal') || (theBaseType=='xs:double') || (theBaseType=='xs:float'))
  {
    eBT="real"
  }
  else if ((theBaseType=='xs:integer') || (theBaseType=='xs:int'))
  {
    eBT="integer"
  }
  else if (theBaseType=='xs:boolean') 
  {
    eBT="boolean"
  }
  else if (theBaseType=='xs:ID') 
  {
    eBT="ID"
  }
  else 
  {
    eBT="string" 
  }
  eBT
}


getMinMaxValues<-function
### Collects details of min/max value restrictions on a supplied element
(inDef,
 ### The element to be queried
 namespaces
 ### The relevant namespaces for the document
)
{
  minEx = NULL
  maxEx = NULL
  minIn = NULL
  maxIn = NULL
  
  xpath.expression <- ".//xs:minExclusive"
  # An Xpath Expression to locate the minimum Exclusive restriction
  
  minMaxList <- getNodeSet(doc=inDef,path=xpath.expression, namespaces)
  # Get a list of the minimum exclusive restrictions on the element
  
  if (length(minMaxList) > 0) 
    # If a minimum exclusive restriction is present, get the integer value of the restriction
  {
    minEx <- as.numeric(xmlGetAttr(minMaxList[[1]],'value'))
  }
  
  xpath.expression <- ".//xs:minInclusive"
  # An Xpath Expression to locate the minimum Inclusive retriction
  
  minMaxList <- getNodeSet(doc=inDef,path=xpath.expression, namespaces)
  # Get a list of the minimum inclusive restrictions on the element
  
  if (length(minMaxList) > 0) 
    # If a minimum inclusive restriction is present, get the integer value of the restriction
  {
    minIn <- as.numeric(xmlGetAttr(minMaxList[[1]],'value'))
  }
  
  xpath.expression <- ".//xs:maxExclusive"
  # An Xpath Expression to locate the maximum Exclusive retriction
  
  minMaxList <- getNodeSet(doc=inDef,path=xpath.expression, namespaces)
  # Get a list of the maximum exclusive restrictions on the element
  
  if (length(minMaxList) > 0) 
    # If a maximum exclusive restriction is present, get the integer value of the restriction
  {
    maxEx <- as.numeric(xmlGetAttr(minMaxList[[1]],'value'))
  }
  
  xpath.expression <- ".//xs:maxInclusive"
  # An Xpath Expression to locate the maximum Inclusive retriction
  
  minMaxList <- getNodeSet(doc=inDef,path=xpath.expression, namespaces)
  # Get a list of the maximum inclusive restrictions on the element
  
  if (length(minMaxList) > 0) 
    # If a maximum inclusive restriction is present, get the integer value of the restriction
  {
    maxIn <- as.numeric(xmlGetAttr(minMaxList[[1]],'value'))
  }
  list('minEx'=minEx, 'maxEx'=maxEx, 'minIn'=minIn, 'maxIn'=maxIn)
}

getNamespaceDefinitions<-function
### Gives the namespace definitions present in the given XML Schema document
(schemaDoc
### The name of the XML Schema document
)
{ 
  rootNode<-xmlRoot(schemaDoc)
  xmlNamespaceDefinitions(rootNode, simplify=TRUE)
### Returns the namespace definitions in the given XML Schema document
  
}
