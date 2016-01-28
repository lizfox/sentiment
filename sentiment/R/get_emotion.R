get_emotion = function (textColumns, ...) {
  emotion_analysis <- classify_emotion(textColumns, ...)
  result <- toString(emotion_analysis[[1,7]])
  if (result!='anger'
      && result!='fear'
      && result!='joy'
      && result!='sadness'
      && result!='surprise'
      && result!='NA') {
    result <- 'anticipation'
  }
  return (result)
}
