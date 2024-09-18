district_map <- function() {
  dcts = read_sf("dcts.shx")
  dcts$DISTRICT_N <- factor(dcts$DISTRICT_N, levels = c("1","2","3","4","5","6","7","8","9","10","11","DC"))
  return(dcts)
}

map_districts <- function(format = "png") {
  dctsmap <- ggplot(dcts) + geom_sf(color = "black", lwd = 1, aes(fill = DISTRICT_N)) + 
    geom_sf_label(aes(label = ABBR)) + 
    scale_fill_manual(values = c("#64baaa", "#fa217f", "#8fda59", "#ef66f0", "#14e54b", "#cd4c35", "#539322", "#b07ccf", "#e2c627", "#1288da", "#fba55c", "#b6c5f5"), labels = c("1st","2d","3d","4th","5th","6th","7th","8th","9th","10th","11th","D.C.")) +
    ggthemes::theme_map() + labs(title = "Federal Judicial Districts and Circuits", color = "Circuits")
  ggsave(paste0("dctsmap.",format), plot = dctsmap, width = 11, height = 8.5, units = "in", dpi = "retina")
}