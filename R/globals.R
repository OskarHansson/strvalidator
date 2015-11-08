# Hack to avoid NOTES in R CMD check
# Hadley does not seem to like it: 
# http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# Alternatively '::' can be used: https://stat.ethz.ch/pipermail/r-devel/2007-June/046048.html

## Needed to avoid notes when using gWidgets.
if (base::getRversion() >= "2.15.1") {
  utils::globalVariables(c("gwindow","ggroup","glayout","glabel","gdroplist",
                           "addHandlerChanged","svalue","svalue<-","gmessage",
                           "gframe","gedit","gbutton","enabled<-","dispose",
                           "gradio","gcheckbox","visible","visible<-",
                           "gtable","gspinbutton","gtext","delete",
                           "gfilebrowse","gbasicdialog","gexpandgroup",
                           "addHandlerDoubleclick","addDropSource","addDropTarget",
                           "blockHandler","addHandlerFocus","ginput","gfile",
                           "gnotebook"))
}

## Needed to avoid notes when using ggplot.
if (base::getRversion() >= "2.15.1") {
  utils::globalVariables(c("ggplot","aes","aes_string","position_jitter",
                           "geom_line","geom_point","geom_bar","geom_tile",
                           "geom_ribbon","geom_segment","geom_text","geom_rect","geom_boxplot","geom_density",
                           "stat_boxplot","stat_ecdf",
                           "theme","element_text","element_blank","theme_grey",
                           "labs","xlab","ylab","guides","guide_legend", 
                           "facet_grid","facet_wrap",
                           "scale_colour_manual","scale_y_discrete","scale_x_discrete",
                           "scale_x_continuous","scale_y_continuous","scale_fill_manual","scale_fill_brewer",
                           "scale_colour_discrete","scale_y_reverse","coord_cartesian",
                           "ggplotGrob","ggplot_build")) 
}

## Needed to avoid notes when using data.table in functions:
## calculateAllele, calculateAT, calculateAT_gui, calculateAT6,
## calculateAT6_gui, plotBalance_gui
if (base::getRversion() >= "2.15.1") {
  utils::globalVariables(c("Height","Blocked","Sample.File.Name","Dye",
                           "Hb","Marker","Lb", "H", "Amount", "Size"))
}