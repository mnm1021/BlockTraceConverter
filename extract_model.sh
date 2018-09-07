#!/bin/bash

# currently we are on the mounted directory of given device,
# and the output 'models.txt' would be written on the parent of current directory.
# assumes that fio benchmark is stored on the parent of current directory.

bs=("4K" "8K" "16K" "32K" "64K" "128K" "256K")
ratios=("readonly" "writeonly" "readdominant" "writedominant")
#direct=("direct" "buffered")

bs_option=("--bs=4K" "--bs=8K" "--bs=16K" "--bs=32K" "--bs=64K" "--bs=128K" "--bs=256K")
ratios_option=("--rwmixread=100 --rwmixwrite=0" "--rwmixread=0 --rwmixwrite=100" "--rwmixread=70 --rwmixwrite=30" "--rwmixread=30 --rwmixwrite=70")
#direct_option=("--direct=1" "--direct=0")

bs_count=${#bs[@]}
ratios_count=${#ratios[@]}
#direct_count=${#direct[@]}

for ((i_bs=0; i_bs<$bs_count; i_bs++))
do
for ((i_ratio=0; i_ratio<$ratios_count; i_ratio++))
do
	
	# this may be changed depending on the variables to set on.

	# 1. get latency data by fio
	# size should be replaced into responsible amount when extracting real model for conversion.
	../fio/fio --name=test --size=50G --rw=rw --direct=1 ${bs_option[$i_bs]} ${ratios_option[$i_ratio]} --latency-log ../data.txt

	# 2. split read and write data by spliter
	../spliter ../data.txt ../read_${bs[$i_bs]}_${ratios[$i_ratio]}.txt ../write_${bs[$i_bs]}_${ratios[$i_ratio]}.txt
	rm ../data.txt

	# 3. get model from given status
	if [ $i_ratio -ne 1 ] # writeonly does not have read data
	then
		# R script: CLI R ignores other lines if line is separated... hard to read, but no other ways.
		# this extracts model from given latency data, and stores model's data into ../models.txt.
		R -e "
			library(\"mixtools\"); x<-scan(\"../read_${bs[$i_bs]}_${ratios[$i_ratio]}.txt\"); y<-tryCatch({normalmixEM(x,k=7,maxit=6000)},error=function(cond){print(\"no match for K=7, trying K=6\");return(tryCatch({normalmixEM(x,k=6,maxit=6000)},error=function(cond){print(\"no match for K=6, trying K=5\");return(tryCatch({normalmixEM(x,k=5,maxit=6000)},error=function(cond){print(\"no match for K=5, trying K=4\");return(tryCatch({normalmixEM(x,k=4,maxit=6000)},error=function(cond){print(\"no match for K=4, trying K=3\");return(tryCatch({normalmixEM(x,k=3,maxit=6000)},error=function(cond){print(\"no match for K=3, trying K=2\");return(tryCatch({normalmixEM(x,k=2,maxit=6000)},error=function(cond){print(\"no match for K=2, trying K=1\");return(tryCatch({normalmixEM(x,k=1,maxit=6000)},error=function(cond){print(\"no match for normalmixEM\")}))}))}))}))}))}))}); write(\"read ${ratios[$i_ratio]} ${bs[$i_bs]}\", file=\"../models.txt\", ncolumns=10, append=TRUE); write(y\$lambda, file=\"../models.txt\", ncolumns=10, append=TRUE, sep=\",\"); write(y\$mu, file=\"../models.txt\", ncolumns=10, append=TRUE, sep=\",\"); write(y\$sigma, file=\"../models.txt\", ncolumns=10, append=TRUE, sep=\",\");
		"
	fi

	if [ $i_ratio -ne 0 ] # readonly does not have write data
	then
		# R script: CLI R ignores other lines if line is separated... hard to read, but no other ways.
		# this extracts model from given latency data, and stores model's data into ../models.txt.
		R -e "
			library(\"mixtools\"); x<-scan(\"../write_${bs[$i_bs]}_${ratios[$i_ratio]}.txt\"); y<-tryCatch({normalmixEM(x,k=7,maxit=6000)},error=function(cond){print(\"no match for K=7, trying K=6\");return(tryCatch({normalmixEM(x,k=6,maxit=6000)},error=function(cond){print(\"no match for K=6, trying K=5\");return(tryCatch({normalmixEM(x,k=5,maxit=6000)},error=function(cond){print(\"no match for K=5, trying K=4\");return(tryCatch({normalmixEM(x,k=4,maxit=6000)},error=function(cond){print(\"no match for K=4, trying K=3\");return(tryCatch({normalmixEM(x,k=3,maxit=6000)},error=function(cond){print(\"no match for K=3, trying K=2\");return(tryCatch({normalmixEM(x,k=2,maxit=6000)},error=function(cond){print(\"no match for K=2, trying K=1\");return(tryCatch({normalmixEM(x,k=1,maxit=6000)},error=function(cond){print(\"no match for normalmixEM\")}))}))}))}))}))}))}); write(\"write ${ratios[$i_ratio]} ${bs[$i_bs]}\", file=\"../models.txt\", ncolumns=10, append=TRUE); write(y\$lambda, file=\"../models.txt\", ncolumns=10, append=TRUE, sep=\",\"); write(y\$mu, file=\"../models.txt\", ncolumns=10, append=TRUE, sep=\",\"); write(y\$sigma, file=\"../models.txt\", ncolumns=10, append=TRUE, sep=\",\");
		"
	fi

	rm ../read_${bs[$i_bs]}_${ratios[$i_ratio]}.txt ../write_${bs[$i_bs]}_${ratios[$i_ratio]}.txt

done
done
