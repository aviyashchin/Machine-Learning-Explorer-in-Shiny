print_and_save_graph <- function(execute_function,new_data,file_name){
  data <- new_data
  print("test")

  if (execute_function == "aggr"){
    myPATH <- file.path(IMGPATH,file_name)
    print(myPATH)
    jpeg(file=myPATH)
    print("AGGR!")
    eval(call(execute_function,data,col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data)))
    dev.off()

  }else if (execute_function == "density"){
      ggplot(data)+ 
        aes_string(x=colnames(data)[1],group="type",color="type") + 
        geom_density(fill=NA)
      print("DENSITY!")
      ggsave(filename = paste(IMGPATH,"/",file_name,sep=""), plot = last_plot())

  }else if (execute_function == "density"){
      ggplot(data)+ 
        aes_string(x=colnames(data)[1],group="type",color="type") + 
        geom_density(fill=NA)
      print("DENSITY!")
      ggsave(filename = paste(IMGPATH,"/",file_name,sep=""), plot = last_plot())

  }else if (execute_function == "density"){
      ggplot(data)+ 
        aes_string(x=colnames(data)[1],group="type",color="type") + 
        geom_density(fill=NA)
      print("DENSITY!")
      ggsave(filename = paste(IMGPATH,"/",file_name,sep=""), plot = last_plot())

  }else if (execute_function == "density"){
      ggplot(data)+ 
        aes_string(x=colnames(data)[1],group="type",color="type") + 
        geom_density(fill=NA)
      print("DENSITY!")
      ggsave(filename = paste(IMGPATH,"/",file_name,sep=""), plot = last_plot())

  }else if (execute_function == "density"){
      ggplot(data)+ 
        aes_string(x=colnames(data)[1],group="type",color="type") + 
        geom_density(fill=NA)
      print("DENSITY!")
      ggsave(filename = paste(IMGPATH,"/",file_name,sep=""), plot = last_plot())

  }else if (execute_function == "density"){
      ggplot(data)+ 
        aes_string(x=colnames(data)[1],group="type",color="type") + 
        geom_density(fill=NA)
      print("DENSITY!")
      ggsave(filename = paste(IMGPATH,"/",file_name,sep=""), plot = last_plot())
  }

  return(paste(IMGPATH,"/",file_name,sep=""))
}