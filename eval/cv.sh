# Cross-validation template.


# Root data directory
data=...

# NKJP tagset configuration
nkjp_tagset=.../nkjp-tagset.cfg

# Number of cores to use
N=4


for i in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10"
do
    echo $i

    # Construct training material.
    echo -n "" > $data/train/train$i.plain
    for j in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10"
    do
	if [ $i != $j ]; then
	    echo "> "$j
	    cat $data/folds/test$j.plain >> $data/train/train$i.plain
	fi
    done

    # Train tagger and tag eval file.
    concraft-pl train $nkjp_tagset $data/train/train$i.plain -i 15 -b 50 --prune 0.1 --ondisk --outmodel $data/concraft/model$i.gz +RTS -N$N -A256M
    concraft-pl tag $data/concraft/model$i.gz < $data/text/test$i.txt > $data/tagged/test$i.plain

    echo -e "\nSTATS\n"
    concraft-pl compare $nkjp_tagset $data/folds/test$i.plain $data/tagged/test$i.plain
    echo -e "\nEND STATS\n"

    # Delete the training material, we will not need it again.
    rm $data/train/train$i.plain
done
