classify_polarity = function (textColumns,
                               pstrong = 0.5, pweak = 1, prior = 1, 
                               verbose = FALSE, ...) 
{
  matrix <- create_matrix(textColumns, ...)
  matrix.dt = data.table(doc = matrix$i,
                         word = matrix$dimnames$Terms[matrix$j],
                         v = matrix$v)
  lexicon <- data.table(read.csv(system.file("data/subjectivity.csv.gz", 
                                             package = "sentiment"),
                                 header = FALSE,
                                 stringsAsFactors = F))
  setnames(lexicon, colnames(lexicon), c("word", "strength", "sentiment"))
  counts <- lexicon[,table(sentiment)]
  counts.dt = lexicon[,list(count = .N), list(sentiment)]
  
  matrix.dt = merge(matrix.dt, lexicon, all.x=T, allow.cartesian = T, by = 'word')
  matrix.dt = merge(matrix.dt, counts.dt, all.x=T, by = 'sentiment')
  matrix.dt[,score := (log(ifelse(strength == "strongsubj", pstrong, pweak) * prior / count))]
  
  bar = matrix.dt[,list(score = sum(score)),list(doc, sentiment)]
  priors = data.table(melt((log(counts/nrow(lexicon)))))
  
  bar = data.table(merge.data.frame(priors, bar, all.x = T, by = NULL))[(as.character(sentiment.x) == sentiment.y) | (is.na(sentiment.y))]
  bar = bar[,list(score = sum(score, na.rm=T),
                  prior = mean(value)),
            list(sentiment.x, doc)]
  bar[,score2 := score + prior]
  scores = data.table(dcast(bar, doc ~ sentiment.x, value.var = 'score2'))
  
  scores[,best_fit := ifelse(abs(positive) > abs(negative), "positive", ifelse(positive == negative, "neutral", "negative"))]
  
  doc.dt = data.table(doc = 1:length(textColumns))
  return (merge(doc.dt, scores, by = 'doc', all.x=T))
}
