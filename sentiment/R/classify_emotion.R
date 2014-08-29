classify_emotion = function (textColumns, prior = 1, ...) {
  matrix <- create_matrix(textColumns, ...)
  matrix.dt = data.table(doc = matrix$i,
                         word = matrix$dimnames$Terms[matrix$j],
                         v = matrix$v)
  lexicon <- data.table(read.csv(system.file("data/emotions.csv.gz", package = "sentiment"), 
                                 header = FALSE,
                                 stringsAsFactors = F))
  setnames(lexicon, colnames(lexicon), c("word", "emotion"))
  counts <- lexicon[,table(emotion)]
  counts.dt = lexicon[,list(count = .N), list(emotion)]
  
  matrix.dt = merge(matrix.dt, lexicon, by = 'word', all.x=T, allow.cartesian = T)
  matrix.dt = merge(matrix.dt, counts.dt, by = 'emotion', all.x=T)
  matrix.dt[,score := v * (log(prior / count))]
  
  bar = matrix.dt[,list(score = sum(score)),list(doc, emotion)]
  priors = data.table(melt((log(counts/nrow(lexicon)))))
  
  bar = data.table(merge.data.frame(priors, bar, all.x = T, by = NULL))[(as.character(emotion.x) == emotion.y) | (is.na(emotion.y))]
  bar = bar[,list(score = sum(score, na.rm=T),
                  prior = mean(value)),
            list(emotion.x, doc)]
  bar[,score2 := score + prior]
  scores = data.table(dcast(bar, doc ~ emotion.x, value.var = 'score2'))
  
  scores[,best_fit := (colnames(scores)[-1][apply(scores[,-1,with=F], 1, which.min)])]
  doc.dt = data.table(doc = 1:length(textColumns))
  return (merge(doc.dt, scores, by = 'doc', all.x=T))
}
