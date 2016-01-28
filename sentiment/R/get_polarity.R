get_polarity = function (textColumns, ...) {
  polarity_analysis <- classify_polarity(textColumns, ...)
  result <- toString(polarity_analysis[[1,4]])
  return (result)
}
