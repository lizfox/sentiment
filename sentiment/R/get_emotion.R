get_emotion = function (textColumns, ...) {
  emotion_analysis <- classify_emotion(textColumns, ...)
  result <- toString(emotion_analysis[[1,9]])
  return (result)
}
