classify_emotion <- function(textColumns, language="english", algorithm="bayes", prior=1.0, verbose=FALSE, ...) {
	matrix <- create_matrix(textColumns,language=language,...)
	
	lexicon_filename <- paste("data/emotions_",language,".csv.gz",sep="")
	lexicon <- read.csv(system.file(lexicon_filename,package="sentiment"),header=FALSE)

	counts <- list(anger=length(which(lexicon[,2]=="anger")),distrust=length(which(lexicon[,2]=="distrust")),trust=length(which(lexicon[,2]=="trust")),fear=length(which(lexicon[,2]=="fear")),joy=length(which(lexicon[,2]=="joy")),sadness=length(which(lexicon[,2]=="sadness")),surprise=length(which(lexicon[,2]=="surprise")),anticipation=length(which(lexicon[,2]=="anticipation")),total=nrow(lexicon))
	documents <- c()

	for (i in 1:nrow(matrix)) {
		if (verbose) print(paste("DOCUMENT",i))
		scores <- list(anger=0,distrust=0,trust=0,fear=0,joy=0,sadness=0,surprise=0,anticipation=0)
		doc <- matrix[i,]
		words <- findFreqTerms(doc,lowfreq=1)
		
		for (word in words) {
            for (key in names(scores)) {
                emotions <- lexicon[which(lexicon[,2]==key),]
                index <- pmatch(word,emotions[,1],nomatch=0)
                if (index > 0) {
                    entry <- emotions[index,]
                    
                    category <- as.character(entry[[2]])
                    count <- counts[[category]]
        
                    score <- 1.0
                    if (algorithm=="bayes") score <- abs(log(score*prior/count))
            
                    if (verbose) {
                        print(paste("WORD:",word,"CAT:",category,"SCORE:",score))
                    }
                    
                    scores[[category]] <- scores[[category]]+score
                }
            }
        }
        
        if (algorithm=="bayes") {
            for (key in names(scores)) {
                count <- counts[[key]]
                total <- counts[["total"]]
                score <- abs(log(count/total))
                scores[[key]] <- scores[[key]]+score
            }
        } else {
            for (key in names(scores)) {
                scores[[key]] <- scores[[key]]+0.000001
            }
        }
		
        best_fit <- names(scores)[which.max(unlist(scores))]
        if (best_fit == (as.numeric(unlist(scores[2]))-3.09234 < .1) best_fit <- NA
		documents <- rbind(documents,c(scores$anger,scores$distrust,scores$trust,scores$fear,scores$joy,scores$sadness,scores$surprise,scores$anticipation,best_fit))
	}
	
	colnames(documents) <- c("ANGER","DISTRUST","TRUST","FEAR","JOY","SADNESS","SURPRISE","ANTICIPATION","BEST_FIT")
	return(documents)
}
