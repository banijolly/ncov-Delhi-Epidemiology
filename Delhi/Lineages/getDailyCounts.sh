
##Usage: ./getDailyCounts.sh <RAWfile> <outputfile>
##Example: ./getDailyCounts.sh CH/CH_RAW CH/CH_lineage_Daily

printf "Date\tB.1\tB.1.36\tB.1.1.7\tB.1.617.1\tB.1.617.2\tOthers\tTotal\n" > $2
for i in `cat DATES2`; do printf $i"\t";for j in `cat lineages `; do cat $1 | grep -w $i | awk -F"\t" -v r=$j '($5==r)' | wc -l; done| tr "\n" "\t"; printf "\n" ; done | awk -F"\t" '{print $1"\t"$2"\t"$3"\t"$4"\t"$5"\t"$6"\t"$7"\t"$2+$3+$4+$5+$6+$7}'>> $2

