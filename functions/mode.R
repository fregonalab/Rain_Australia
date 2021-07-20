Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#Note: The function was developed by @RichScriven in:
#https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode/8189441#8189441