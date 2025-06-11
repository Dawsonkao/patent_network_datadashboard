get_firm_list=function(df){
  edges=df$Edges
  #edge_df=edges%>%strsplit(";;")%>%as.data.frame()%>%t%>%as.data.frame()
  edge_df=str_split(edges,";;")%>%as.data.frame()%>%t%>%as.data.frame()
  firm_list=c(edge_df$V1,edge_df$V2)
  firm_list=firm_list%>%unique%>%as.data.frame()
  row.names(firm_list)=NULL
  row.names(edge_df)=NULL
  colnames(firm_list)[1]="firm"
  colnames(edge_df)=c("node1","node2")
  firm_list$firm_id=1:nrow(firm_list)
  return(list(edge_df,firm_list))
}


#產生獨立Edge(排除一個edge有兩個點、self loop)
edge_tidy1=function(x){
  x_array=str_split(x,';;')%>%unlist
  l1=length(x_array)
  x_array=unique(x_array)
  l2=length(x_array)
  if(length(x_array)==2){
    if(l1==l2){return(data.frame(Edges=x))}
    if(l1!=l2){return(data.frame(Edges=paste0(x_array,collapse = ";;")))}
  }else if(length(x_array)==1){
    return(data.frame(Edges=x_array))
  }
  else{
    xc_c=combn(x_array,2)
    result=data.frame()
    for(i in 1:dim(xc_c)[2]){
      result=rbind(result,data.frame(Edges=paste0(xc_c[1:2,i],collapse = ";;")))
    }
    return(result)
  }
}



edge_tidy2=function(df){
#edges_cur=df$Edges%>%unique()
edges_cur=df$Edges
edge_tidy_list1=lapply(edges_cur,edge_tidy1)
edge_tidy_df=do.call(rbind,edge_tidy_list1)
edge_tidy_df=table(edge_tidy_df$Edges)%>%as.data.frame()
#edge_tidy1=unique(edge_tidy1)
edge_tidy_df$Edges=edge_tidy_df$Var1

p_e=edge_tidy_df
p_e2=get_firm_list(edge_tidy_df)
df3=cbind(p_e[,c("Edges","Freq")],p_e2[[1]])
return(df3)
}



################################################################################
generate_gephi_data=function(df){
  df=df%>%filter(str_detect(Edges,';;'))
  df3=edge_tidy2(df)
  
  name=unique(c(df3$node1,df3$node2))%>%sort()
  
  nodes=data.frame(ID=1:length(name),
                   #label=name
                   #value=1,
                   Label=name
                   #title=name,
                   #shape="dot"
                   #group
                   #value
  )
  
  edges=cbind(data.frame(ID=1:nrow(df3)),df3[,c("node1","node2")])
  edges$Weight=0
  for(i in 1:nrow(edges)){
    edges$node1[i]=nodes$ID[nodes$Label==edges$node1[i]]
    edges$node2[i]=nodes$ID[nodes$Label==edges$node2[i]]
    edges$Weight[i]=df3$Freq[i]
  }
  #colnames(edges)=c("from","to","width")
  colnames(edges)[2:3]=c("Source","Target")
  #edges$Weight=1
  return(list(nodes,edges))
}

sort_edge=function(var){
  var_split=var%>%str_split(";;")%>%unlist
  return(paste0(sort(var_split),collapse = ";;"))
}
remove_duplicated_edge=function(var){
  var_split=var%>%str_split(";;")%>%unlist
  var_split=unique(var_split)
  return(paste0(sort(var_split),collapse = ";;"))
}


