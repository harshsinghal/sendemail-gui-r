library(gWidgets)
options("guiToolkit"="RGtk2")

#------------ HANDLER functions
# WIP password entry text widget
#makePassword <- function(h,...){
#	tempVal <- svalue(passwordValue)
#	print(tempVal)
#	passwordVar <<- tempVal
#	svalue(passwordValue) <-  paste(rep("*",nchar(tempVal)),collapse="")
#	#passwordValue[,] <-  paste(rep("*",nchar(tempVal)),collapse="")
#	#passwordValue <- paste(rep("*",length(tempVal)),collapse="")
#}

getFile<- function(h,...)
{
	fileName<- file.choose()
	fileName2<-gsub("\\\\","/",fileName)
	svalue(selectFileValue) <- fileName2
	#print(fileName)
}

sendEmailFun <- function(h,...){
# Create the required command line parameters for sendEmail to work
	svalue(statusValue) <- "sendEmail Pending.."
	paramsList <- list()
	paramsList$fromAddress <- c("-f",svalue(fromEmailValue))
	print(nchar(paramsList$fromAddress))
	paramsList$toAddress <- c("-t",svalue(toEmailValue))
	paramsList$emailSubject <- c("-u",svalue(subjectValue))
	paramsList$listemailMessage <- c("-m",svalue(bodyValue))
	paramsList$serverAndPort <- c("-s",svalue(serverportValue))
	paramsList$fileAttachPath <- c("-a",svalue(selectFileValue))
	if(paramsList$fileAttachPath[2] == "No File Selected"){
		paramsList$fileAttachPath <- NULL
	}
	paramsList$accUsername <- c("-xu",svalue(usernameValue))
	paramsList$accPassword <- c("-xp",svalue(passwordValue))
	paramsList1 <- lapply(paramsList,function(x){x[2] <- dQuote(x[2]);paste(x,collapse = " ")})
	suffixCall <- paste(do.call("c",paramsList1),collapse = " ")
	commandCall <- paste("sendEmail",suffixCall,sep = " ")
	returnVal <- system(as.character(commandCall),intern=T,wait=T)
	svalue(statusValue) <- returnVal
#	print(returnVal)
#	print(commandCall)

}
w <- gwindow("sendEmail R GUI",visible =TRUE)
parentGroup <- ggroup(horizontal=TRUE,container=w)
# Notebook layout
nb <- gnotebook(container=parentGroup,tab.pos=3,expand=TRUE)
# Group 1 contains the credential information
group1 <- ggroup(horizontal=FALSE,container=nb,label="Credentials")
fromEmailLabel <- glabel(text="From Email",editable=FALSE,container=group1)
fromEmailValue <- gtext("xyz@gmail.com",container=group1,font.attr=c(family="monospace",weights="bold"),wrap=TRUE,expand=FALSE)
 
toEmailLabel <- glabel(text="To Email",editable=FALSE,container=group1)
toEmailValue <- gtext("xyz@gmail.com",container=group1,font.attr=c(family="monospace",weights="bold"),wrap=FALSE,expand=FALSE)

usernameLabel <- glabel(text="User Name",editable=FALSE,container=group1)
usernameValue <- gtext("xyz@gmail.com",container=group1,font.attr=c(family="monospace",weights="bold"),wrap=FALSE,expand=FALSE)

passwordLabel <- glabel(text="Password",editable=FALSE,container=group1)
passwordValue <- gtext("xyz",container=group1,height=2,font.attr=c(family="monospace",weights="bold"),wrap=FALSE,expand=FALSE)
#addHandlerChanged(passwordValue,handler=makePassword,interval=1)

serverportLabel <- glabel(text="Server:Port",editable=FALSE,container=group1)
serverportValue <- gtext("smtp.gmail.com:587",container=group1,height=2,font.attr=c(family="monospace",weights="bold"),wrap=FALSE,expand=FALSE)
# group2 contains the content and file attachment if any
group2 <- ggroup(horizontal=FALSE,container=nb,label="Content")
subjectLabel <- glabel(text="Subject",editable=FALSE,container=group2)
subjectValue <- gtext("Subject Text",container=group2,height=2,font.attr=c(family="monospace",weights="bold"),wrap=FALSE,expand=FALSE)

bodyLabel <- glabel(text="Body",editable=FALSE,container=group2)
bodyValue <- gtext("Body of the email",container=group2,height=2,font.attr=c(family="monospace",weights="bold"),wrap=FALSE,expand=FALSE)

selectFileLabel <- gbutton(text="Select File Attachment",editable=FALSE,container=group2,handler=getFile)
#addHandlerClicked(selectFileLabel,handeler=getFile)
selectFileValue <-gtext("No File Selected",container=group2,height=2,font.attr=c(family="monospace",weights="bold"),wrap=FALSE,expand=FALSE)

sendEmailButton <- gbutton(text="Send Email",border=TRUE,container=group2,handler=sendEmailFun)

statusLabel <- glabel(text="Status",editable=FALSE,container=group2)
statusValue <- gtext("sendEmail command response",container=group2,height=2,font.attr=c(family="monospace",weights="bold"),wrap=FALSE,expand=FALSE)
# group3 contaisn information about sendEmail and gWidgets - citations
group3 <- ggroup(horizontal=FALSE,container=nb,label="About")
aboutValue <- gtext(c("sendEmail created by Brandon Zehm \n",
					"http://caspian.dotconf.net/menu/Home/ \n \n",
					"GUI Widgets from the gWidgets package by John Verzani \n",
					"http://cran.r-project.org/web/packages/gWidgets/index.html \n \n", 
					"I've tested sendEmail for Gmail only \n",
					"send comments/feedback to singhalblr@gmail.com \n",
					"-- Harsh Singhal")
					,
		container=group3,height=2,font.attr=c(family="monospace",weights="bold"),wrap=TRUE,expand=TRUE,markup=TRUE)

