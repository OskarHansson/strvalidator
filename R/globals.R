# Hack to avoid NOTES in R CMD check
# Hadley does not seem to like it: 
# http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
if (base::getRversion() >= "2.15.1") {
  utils::globalVariables(c("gwindow","ggroup","glayout","glabel","gdroplist",
                           "addHandlerChanged","svalue","svalue<-","gmessage",
                           "gframe","gedit","gbutton","enabled<-","dispose",
                           "gradio","gcheckbox","visible","visible<-",
                           "gtable","gspinbutton","gtext","delete",
                           "gfilebrowse","gbasicdialog","gexpandgroup",
                           "addHandlerDoubleclick","addDropSource","addDropTarget",
                           "blockHandler","addHandlerFocus","ginput","gfile",
                           "gnotebook", "gtable_add_grob")) ## Needed to avoid notes when using gWidgets
}

if (base::getRversion() >= "2.15.1") {
  utils::globalVariables(c("ggplot","aes")) ## Needed to avoid notes when using ggplot
}

