normWeights<-cmpfun(function(weights){
  s<-sum(weights)
  return(round(weights/s,2))
})
continue2discrete<-cmpfun(function(a){
  a<-as.numeric(a)
  q<-summary(a)
  a[a<=q[2]]<-1
  a[a<=q[3] & a>q[2]]<-2
  a[a<=q[5] & a>q[3]]<-3
  a[a>q[5]]<-4
  return(a)
})

batchGenerator<-cmpfun(function(fpath,headers,numeric_headers,weights,batchSize,sep){
# print(fpath)
# print(headers)
# print(numeric_headers)
# print(weights)
# print(batchSize)
weights<-normWeights(weights)
samples <- read.csv(fpath,stringsAsFactors=F,header = T,encoding = "UTF-8",check.names=F,sep=sep)
steps = nrow(samples)*100
samples_ori<-samples
if(numeric_headers!="None" & !is.null(numeric_headers)){
  for(h in numeric_headers){
  samples[h]<-continue2discrete(unlist(samples[h]))
}
}

#samples$Age <- as.numeric(samples$Age)
# colnames(samples)[1] <- "sampleId"
rownames(samples)<-samples[,1]

# samples$patientId<-1:nrow(samples)
d <- dim(samples)[2]
#samples <- samples[,c(d,1:(d-1))]
#samples$AgeLevel=sapply(samples$Age,function(k){k %/% 20}) 
L =dim(samples)[1]
#samples$idx=1:L

sql="SELECT %s,count(*) CT FROM samples group by %s"
Refs <- sapply(headers,function(c1){sqldf(sprintf(sql,c1,c1))})

for(i in 1:dim(Refs)[2]){
   v <- Refs[,i][[2]]
	v <- v/sqrt(sum(v^2))
   Refs[,i][[2]] <- v
}

batchNumber=L %/% batchSize

index=1:L
batch=list()
bestBatch=list()
bestScore=0;

incProgress(3/15,message = "Iteration beginning!")
for(stp in 1:steps){
	batch=list()
	scores=c();
	poolIndex <- 1:L
	for(i in 1:batchNumber){
	   score=0;
		bt <- sample(poolIndex,batchSize,replace=F)
		poolIndex <- setdiff(poolIndex,bt)
		tmp <- samples[bt,]
		batch[[length(batch)+1]] <- tmp

		for(j in 1: length(headers)){
         v1 <- sapply(Refs[,j][[1]],function(v){sum(tmp[,headers[j]]==v)})
			v1 <- v1 / sqrt(sum(v1^2))
			v2 <- Refs[,j][[2]] -v1
			score <- score+weights[j]*sqrt(sum(v2^2))
		}
		scores <- c(scores,score)
	}

	batchScore <- 1/((1+sum(scores))*(1+sd(scores)/mean(scores)))
	#cat(batchScore,"\n")
	if(bestScore < batchScore){
	  
		bestBatch <- batch
		bestScore <- batchScore
		#cat(sprintf("step %d:better batch generated with score %f\n",stp,bestScore))
	}
}
incProgress(13/15,message = "Iteration finishing! Post data processing now...")
for(i in 1:batchNumber){
   bestBatch[[i]]$batchId <- i 
}
rtBatch <-  bestBatch[[1]]
for(i in 2:batchNumber){
  rtBatch <- rbind(rtBatch,bestBatch[[i]])
}

remainder <- dim(samples)[1] %% batchSize
if(remainder !=0){
   tmp <- samples[setdiff(samples[,1],rtBatch[,1]),]
   tmp$batchId <- batchNumber+1
	rtBatch <- rbind(rtBatch,tmp)
}
# rtBatch <- rtBatch[,colnames(rtBatch)!="Age"]
# colnames(rtBatch)[1] <- 'patientId'
# colnames(samples)[1] <- 'patientId'
# result <- sqldf("SELECT distinct A.*,samples.Age From rtBatch A,samples WHERE A.idx=samples.idx")
# result <- result[,colnames(result)!='idx']

result <- sqldf("select * from rtBatch order by batchId ")
rownames(samples_ori)<-unlist(samples_ori[1])
for(h in numeric_headers){
  result[h]<-samples_ori[unlist(result[1]),h]
}
return(result)
})

