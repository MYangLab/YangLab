args <- commandArgs(trailingOnly = TRUE)

if(length(args) < 1) {
  args <- c("--help")
}
 
## Help section
if("--help" %in% args) {
  cat("
      Example:

      Rscript plotPcaFromSmartpca.R  /home/hongru/work/project/aDNA/tibetan/pca/modernEastAsian.pca.evec /home/hongru/work/project/aDNA/tibetan/pca/modernEastAsian.pca.megaGroup 8 modernEastAsian
    
      evec file should already have the individual name and population name. See the example.
      megaGroup.file: the populations are assigned to larger megaGroup
      seed: random integer, will change the color assignment.

      Rscript plotPcaFromSmartpca.R  smartpca.output.evec megaGroup.file seed output.name

      For adjusting, please go to plateau.ipynb.

	  Notes:
	  (1) if do not want to assign megaGroup, just one pop one megaGroup
	  (2) One megaGroup will get a color, within the megaGroup, each population will get a shape.\n\n")
    
  q(save="no")
}

##########################################################################
##
## Read in pca matrix and merge it with megaGroup.
##
##########################################################################
library(RColorBrewer)
library(scales)

pca = read.table(args[1], header=F)
megaGroup = read.table(args[2], header=F)
names(pca)[1] = c("indName")
names(pca)[12] = c("popName")
names(megaGroup) = c("popName", "megaGroupName")
pca.comb = merge (pca, megaGroup, by="popName", all.x=T)
##########################################################################
##
## Get the color and shape box ready.
##
##########################################################################
col.number = length(unique(sort(pca.comb$megaGroupName)))  ## 13 is the number of color to use
pch.number = max(table(megaGroup$megaGroupName))  ## 13 is the number of shapes to use.
col.box = c()
if (col.number>11){
    col.box=brewer.pal(11, "Spectral")
    col.box = colorRampPalette(col.box)(col.number) 
} else {
    col.box = brewer.pal(col.number, "Spectral")
}
set.seed(args[3])
col.box = sample(col.box)
pch.box = c(16,15,17,4:14,22,2,1)
##########################################################################
##
## Fix the pch index
##
##########################################################################
pca.comb$pchIndex = c("NA")
for (g in unique(sort(pca.comb$megaGroup))){
    pca.comb[pca.comb$megaGroup==g,]$pchIndex = as.numeric(as.factor(as.character(pca.comb[pca.comb$megaGroup==g,]$popName)))
    ## Under each megaGroup, make each population have factor assigned from 1.
}

##########################################################################
##
## Order the matrix
## Mel: Added write to table line
## Important for df.legend to make sense.
##
##########################################################################
pca.comb = pca.comb[with(pca.comb, order(megaGroupName, popName)), ]   ## Sort the dataframe
write.table(pca.comb,row.names=TRUE,col.names=TRUE)

##########################################################################
##
## Now do the plot
##
##########################################################################
pdf(file = paste(args[4],".pdf", sep=""), paper="us" )
#par(mar=c(5,4,4,2))

xmin = min(pca.comb$V2)*1.3
xmax = max(pca.comb$V2)*1.1
ymin = min(pca.comb$V3)*2.5
ymax = max(pca.comb$V3)*1.5   ### Adjust to leave more space for legend plotting.

plot(pca.comb$V2, pca.comb$V3, xlim=c(xmin, xmax), ylim=c(ymin, ymax), col='white')
df.legend = read.table(text = "", col.names = c("group","color","index"),colClasses = c("character","numeric","numeric"))

for(i in 1:dim(pca.comb)[1]){    
    col.index = as.numeric(pca.comb$megaGroupName[i])%%(col.number+1) 
    ch.index = as.numeric(pca.comb$pchIndex[i])%%(length(pch.box)+1) 
    #ch.index = 1
    df.legend[i,1] = as.character(pca.comb$popName[i])
    df.legend[i,2] = col.index
    df.legend[i,3] = ch.index
    points(pca.comb$V2[i], pca.comb$V3[i], cex=0.7, col = alpha(col.box[col.index],0.99), pch = pch.box[ch.index])
}


##########################################################################
##
## Add the legend
##
##########################################################################

df.legend = unique(df.legend)
names(df.legend) = c("group","color","index")

legend("bottomleft", 
       legend=df.legend$group, 
       cex=0.5,
       title="Modern East Asian",
       title.adj=0.01,
       pch = pch.box[df.legend$index], 
       col = col.box[df.legend$color], 
       ncol=5)
dev.off()



