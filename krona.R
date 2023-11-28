options(scipen=999)
options(stringsAsFactors=FALSE)
library(data.tree)
library(xml2)
library(magrittr)

#get your data into a hieracial format in a data.frame   
t <- structure(list(freq = c(34L, 127L, 42L, 123L, 343L, 13L, 104L, 
                             21L, 135L, 91L, 167L, 148L, 90L, 46L, 25L, 79L, 1726L), king.n = c("", 
                                                                                                "Fungi", "Fungi", "Fungi", "Fungi", "Fungi", "Fungi", "Plantae", 
                                                                                                "Plantae", "Plantae", "Plantae", "Plantae", "Plantae", "Plantae", 
                                                                                                "Plantae", "Plantae", "Plantae"), class.n = c("", "Dothideomycetes", 
                                                                                                                                              "Dothideomycetes", "Dothideomycetes", "Dothideomycetes",     "Dothideomycetes", 
                                                                                                                                              "Dothideomycetes", "Liliopsida", "Magnoliopsida", "Magnoliopsida", 
                                                                                                                                              "Magnoliopsida", "Magnoliopsida", "Magnoliopsida", "Magnoliopsida", 
                                                                                                                                              "Magnoliopsida", "Magnoliopsida", "Magnoliopsida"), fam.n = c("", 
                                                                                                                                                                                                            "", "Cladosporiaceae", "Didymellaceae", "Pleosporaceae", "Pleosporaceae", 
                                                                                                                                                                                                            "Pleosporaceae", "Poaceae", "", "Asteraceae", "Asteraceae", "Asteraceae", 
                                                                                                                                                                                                            "Asteraceae", "Fabaceae", "Hydrophyllaceae", "Hypericaceae", 
                                                                                                                                                                                                            "Malvaceae"), gen.n = c("", "", "Cladosporium", "", "Alternaria", 
                                                                                                                                                                                                                                    "Pyrenophora", "Pyrenophora", "Phleum", "", "", "Achillea", "Centaurea", 
                                                                                                                                                                                                                                    "Jurinea", "Trifolium", "Phacelia", "Hypericum", "Malva"), spec.n = c("", 
                                                                                                                                                                                                                                                                                                          "", "cladosporioides", "", "", "", "tritici-repentis", "pratense", 
                                                                                                                                                                                                                                                                                                          "", "", "", "cyanus", "", "", "", "", "")), .Names = c("freq", 
                                                                                                                                                                                                                                                                                                                                                                 "king.n", "class.n", "fam.n", "gen.n", "spec.n"), row.names = c(1L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                 3L, 4L, 5L, 8L, 9L, 11L, 15L, 16L, 17L, 18L, 19L, 21L, 22L, 25L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                 26L, 28L), class = "data.frame")

#if any sequences can not be assigned to the highest hieracial level give them a random name like "NA"
t[t[,"king.n"]=="","king.n"] <- "NA"
#define the hieracy (do not include the counting var here), the first space is the root category
t$pathString <- paste(" ",t$king.n,t$class.n,t$fam.n,t$gen.n,t$spec.n,sep = "/")
#transform to a data.tree
t <- as.Node(t)
#define the sequence frequency calculations
t$Do(function(x) x$freq <- Aggregate(x, "freq", sum), traversal = "post-order")
print(t,"freq")
#get all frequencies for each node
repval <- t$Get("freq")

#tranform into a nested list and afterwards into a xml document to get the nesting
t <- t %>% as.list(.,mode="simple",unname=T)
t <- as_xml_document(list(root=t))%>%as.character

#from here it is just a bunch of regex
#split the xml file into one line for each < 
temp <- strsplit(t,"\n")[[1]][2] %>% strsplit(.,"(?=<)",perl=T) %>% .[[1]]
temp <- paste0(temp[c(T,F)],temp[c(F,T)]) %>% gsub("[0-9]","",.)
#remove the names nodes and replace by krona annotation
#replace the closing named tags by generic closing "node" tags
temp <- gsub("</(.*)?>","</node>",temp) %>% gsub(">.*$",">",.) 
#replace the named opening tags by generic "node" tags with the "name" attribute, replace all (because they are incomplete) numeric values with a placeholder
temp <- gsub("<(?=[^/])(.*)?>",
             '<node name="\\1"><magnitude><val>__\\1</val></magnitude><score><val>1</val></score>',temp,perl=T)
temp %<>% gsub("root"," ",.)
#find the placeholders to insert the frequencies and execute
repos <- grep("__",temp)
for (i in repos)
  temp[i] <- gsub("^(.*)(__.*?)(?=<)(.*)",paste0("\\1",repval[which(repos==i)],"\\3"),temp[i],perl=T)

#add the end of the .html file
temp <- c(temp,"</krona></div>","</body>","</html>")  %>% unlist
tout <- paste(temp,collapse="\n") 

#append your data to the Krona plot
write(tout,file="Krona1.html",append=TRUE) 