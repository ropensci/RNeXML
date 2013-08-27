require(XML)
node <- newXMLNode("meta", 
                attrs = c(id="dict1", property="cdao:has_tag",
			                    content="true", 'xsi:type'="nex:LiteralMeta",
			                    datatype="xsd:boolean"), suppressNamespaceWarning=TRUE)
n2 <- newXMLNode("node", attrs = c(id = "n4", label="n4", about="#n4"), .children = node)


s4 <- as(n2, "node")
as(s4, "XMLInternalElementNode")


