Email_file_to_Slack <- function(Body,File_Location){
  if(!DEBUG){
    if (File_Location==""){
        send.mail(from = "mrtdatascientist@gmail.com",
        to = c("mrt.9msiu@zapiermail.com"),
        #to = c("avi.yashchin@gmail.com"),
        subject = "Hello sucka",
        body = Body,
        html = FALSE,
        smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "mrtdatascientist@gmail.com", passwd = "ipitythefool", ssl = TRUE),
        authenticate = TRUE,
        send = TRUE)
    }else{
        send.mail(from = "mrtdatascientist@gmail.com",
        #to = c("avi.yashchin@gmail.com"),
        to = c("mrt.9msiu@zapiermail.com"),
        subject = "Hello sucka",
        body = Body,
        html = FALSE,
        smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "mrtdatascientist@gmail.com", passwd = "ipitythefool", ssl = TRUE),
        attach.files = c(File_Location),
        authenticate = TRUE,
        send = TRUE)
    }
  }
}