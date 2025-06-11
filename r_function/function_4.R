create_final_applicant=function(df,firm_adjust_table){
#清理重複、有缺失值專利
if("Final_Applicant" %in% colnames(df)){
  df=df%>%select(-Final_Applicant)
}
df=df%>%filter(!(is.na(Applicants)|is.na(`Display Key`)))
df=df[!duplicated(df[,c("Display Key","Lens ID","Application Number","Priority Numbers","Applicants")]),]
#
df_co=df[str_detect(df$Applicants,';;'),]
df_noco=df[!str_detect(df$Applicants,';;'),]
df_co$Final_Applicant_alpha=''
df_noco$Final_Applicant_alpha=''
count=0
error_df=array()
#替換有合作的Applicant
for(i in 1:nrow(df_co)){
  x_array=str_split(df_co$Applicants[i],';;')%>%unlist
  if(sum(x_array %in% firm_adjust_table$Applicants)==length(x_array)){
    x_replace_array=c()
    for(j in 1:length(x_array)){
      x_replace_e=firm_adjust_table$Final_Applicant[firm_adjust_table$Applicants==x_array[j]]
      x_replace_array[j]=x_replace_e
    }
    df_co$Final_Applicant_alpha[i]=paste0(x_replace_array,collapse = ";;")
  }else{
    count=count+1
    error_df=c(error_df,str_split(df_co$Applicants[i],';;'%>%unlist))%>%unique()
    #print(paste0(i,' Match Failed:'))
    #print(df_co$Applicants[i])
  }
}
error_df=unique(error_df%>%unlist)%>%sort
#替換無合作的Applicant
for(i in 1:nrow(df_noco)){
  if(df_noco$Applicants[i]%in%firm_adjust_table$Applicants){
    df_noco$Final_Applicant_alpha[i]=firm_adjust_table$Final_Applicant[firm_adjust_table$Applicants==df_noco$Applicants[i]]
  }
}
#合併,join回去原資料
df_noco$Final_Applicant=df_noco$Final_Applicant_alpha
df_co$Final_Applicant=lapply(df_co$Final_Applicant_alpha,remove_duplicated_edge)%>%unlist
df_co=df_co[,c("Display Key","Final_Applicant_alpha","Final_Applicant")]
df_noco=df_noco[,c("Display Key","Final_Applicant_alpha","Final_Applicant")]

df=left_join(df,rbind(df_co,df_noco),by="Display Key")
print(paste0("Ratio of Replace:",round(1-sum(df$Final_Applicant=="")/nrow(df),2)))
return(list(df,error_df))
}


create_final_applicant2=function(df,firm_adjust_table,node_list){
  #清理重複、有缺失值專利
  if("Final_Applicant" %in% colnames(df)){
    df=df%>%select(-Final_Applicant)
  }
  df=df%>%filter(!(is.na(Applicants)|is.na(`Display Key`)))
  df=df[!duplicated(df[,c("Display Key","Lens ID","Application Number","Priority Numbers","Applicants")]),]
  #
  df_co=df[str_detect(df$Applicants,';;'),]
  df_noco=df[!str_detect(df$Applicants,';;'),]
  df_co$Final_Applicant_alpha=''
  df_noco$Final_Applicant_alpha=''
  count=0
  error_df=array()
  #將重要節點node_list的合作夥伴新增至firm_adjust_table
  #Final Applicant暫時由Applicant直接補上
  node_list
  
  #找出node_list的所有Applicant name(一間公司可能有好幾個)
  sear_list=firm_adjust_table$Applicants[firm_adjust_table$Final_Applicant%in%node_list]
  
  addition_table=df_co$Applicants[str_detect(df_co$Applicants,paste0(sear_list,collapse = "|"))]
  addition_table=str_split(addition_table,";;")%>%unlist%>%unique()%>%sort
  addition_table=addition_table[!addition_table%in%sear_list]
  
  #剔除有在firm_adjust_table的Applicant
  addition_table=addition_table[!addition_table%in%firm_adjust_table$Applicants]
  
  #替換有合作的Applicant
  for(i in 1:nrow(df_co)){
    x_array=str_split(df_co$Applicants[i],';;')%>%unlist
    if(sum(x_array %in% firm_adjust_table$Applicants)==length(x_array)){
      x_replace_array=c()
      for(j in 1:length(x_array)){
        x_replace_e=firm_adjust_table$Final_Applicant[firm_adjust_table$Applicants==x_array[j]]
        x_replace_array[j]=x_replace_e
      }
      df_co$Final_Applicant_alpha[i]=paste0(x_replace_array,collapse = ";;")
    }else{
      count=count+1
      error_df=c(error_df,str_split(df_co$Applicants[i],';;'%>%unlist))%>%unique()
      #print(paste0(i,' Match Failed:'))
      #print(df_co$Applicants[i])
    }
  }
  error_df=unique(error_df%>%unlist)%>%sort
  #替換無合作的Applicant
  for(i in 1:nrow(df_noco)){
    if(df_noco$Applicants[i]%in%firm_adjust_table$Applicants){
      df_noco$Final_Applicant_alpha[i]=firm_adjust_table$Final_Applicant[firm_adjust_table$Applicants==df_noco$Applicants[i]]
    }
  }
  #合併,join回去原資料
  df_noco$Final_Applicant=df_noco$Final_Applicant_alpha
  df_co$Final_Applicant=lapply(df_co$Final_Applicant_alpha,remove_duplicated_edge)%>%unlist
  df_co=df_co[,c("Display Key","Final_Applicant_alpha","Final_Applicant")]
  df_noco=df_noco[,c("Display Key","Final_Applicant_alpha","Final_Applicant")]
  
  df=left_join(df,rbind(df_co,df_noco),by="Display Key")
  
  
  #結束
  print(paste0("Ratio of Replace:",round(1-sum(df$Final_Applicant=="")/nrow(df),2)))
  return(list(df,error_df))
}



