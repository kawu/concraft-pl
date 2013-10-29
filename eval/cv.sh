# Cross-validation script.


# Root data directory
data=$1

# NKJP tagset configuration
nkjp_tagset=$2


echo ""
echo "Data directory: "$1
echo "Tagset file: "$2
shift
shift
echo "Training arguments: ""$@"
echo ""


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
    concraft-pl train --tagset=$nkjp_tagset $data/train/train$i.plain -e $data/folds/test$i.plain -o $data/concraft/model$i.gz "$@"
    concraft-pl tag $data/concraft/model$i.gz < $data/text/test$i.txt > $data/tagged/test$i.plain

    echo -e "\nSTATS\n"
    concraft-pl compare --tagset=$nkjp_tagset $data/folds/test$i.plain $data/tagged/test$i.plain
    echo -e "\nEND STATS\n"

    # Delete the training material, we will not need it again.
    rm $data/train/train$i.plain
done
