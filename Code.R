library(plyr)
library(dplyr)
min_supp=readline(prompt="Enter minimum support: ")
min_supp= as.double(min_supp)
min_conf=readline(prompt="Enter mininmum confidence: ")
min_conf= as.double(min_conf)
min_supp = min_supp * 5822
#min_conf = 0.3
ticdata2000 <- read.delim("~/Downloads/ticdata2000.txt", header=FALSE) #Read the whole dataset
ticdata2000_subset = ticdata2000[,c(14,15,16,17,18,19,20,21,22,23,24,25)] #Select 12 attributes starting from index 14
names(ticdata2000_subset) <- c("MFGEKIND","MFWEKIND","MOPLHOOG","MOPLMIDD","MOPLLAAG","MBERHOOG","MBERZELF","MBERBOER","MBERMIDD","MBERARBG","MBERARBO","MSKA") #name of columns
write.csv(ticdata2000_subset, file="file_code.csv")
ticdata2000_discrete = as(data.frame(lapply(ticdata2000_subset, as.character), stringsAsFactors=T), "transactions") #convert attributes to binary attributes
list1 = list() # Holds itemset
list2 = list() # Holds attribute names
list3 = list() # Holds attribute number from 1 to 116
LeftRules = list() # Holds left attributes of the rule
RightRules = list()
Rules = list()
support = list()
confidence = list()
lift  = list()
leverage = list()
no_rules = 0
col= colnames(ticdata2000_discrete) 
v <- nrow(ticdata2000_discrete@data) # 116 number of attributes
u <- ncol(ticdata2000_discrete@data) # 5822 number of records
for (i in  1:v)
{
  # initializing itemset with each attribute alone
   list1[i] =list(ticdata2000_discrete@data[c(i),])
   list2[i] =list(col[i])
   list3[i]=list(c(i))
}
i=1
repeat
{
  
   i_supp = length(list1[[i]][list1[[i]]==TRUE]) #calculate support of this item
   if ((i_supp) >= min_supp) #continue if support > minimum support
   {
	for (j in  1:v)
	    {
	  #we want to add an attribute to this item
	  j_supp = length(list1[[j]][list1[[j]]==TRUE]) 
		if  ((!(list2[[j]] %in% list2[[i]])) && (j_supp >= min_supp) ) ##continue if support > minimum support
	    	{
		    tmp = list1[[i]] & list1[[j]] #item after adding the new attribute
		    total_combined_supp = length(tmp[tmp==TRUE]) #calculate support of this new item
		    if ((total_combined_supp) >= min_supp) #continue if support > minimum support
			{
				list1[length(list1)+1]=list(tmp) #add this new item to the end of itemset to check if we can add another attribute  
				list2[length(list2)+1]=list(c(list2[[i]],list2[[j]])) # add the names
				list3[length(list3)+1]=list(c(list3[[i]],list3[[j]])) # add the indices
				if (total_combined_supp/i_supp >= min_conf) ##print the rule if confidence > minimum confidence
				{
				  #the rule would be item i (may contain more than 1 attr.) implies item j 
				  if (!is.unsorted(list3[[i]])) #prevents repetition of rules
				  {
				  no_rules = no_rules + 1
				  Rules[[no_rules]] = append(list2[[i]],list2[[j]])
				  LeftRules[[no_rules]] = list2[[i]]
				  RightRules[[no_rules]] = list2[[j]]
				  support[[no_rules]] = total_combined_supp/u
				  confidence[[no_rules]] = total_combined_supp/i_supp
				  lift[[no_rules]]  = (total_combined_supp/u)/((i_supp/u)*(j_supp/u))
				  leverage[[no_rules]] = (total_combined_supp/u) - ((i_supp/u)*(j_supp/u))
				  # words=c( no_rules,LeftRules[[no_rules]], ">>",RightRules[[no_rules]] ," Supp", support[[no_rules]],"Conf",confidence[[no_rules]],"Lift",lift[[no_rules]],"Lev",leverage[[no_rules]])
				  opa = toString(LeftRules[[no_rules]])
				  print(sprintf('%-3d %-35s >> %-20s Supp: %f   Conf: %f   Lift: %f   Lev: %f' ,no_rules,opa,RightRules[[no_rules]],support[[no_rules]],confidence[[no_rules]],lift[[no_rules]],leverage[[no_rules]]))
				    
				  
				  }
				}
		    }
		}
	    
	    }
	   
   }
   up = length(list1)
   if (i==up)
     break
   i=i+1
}

