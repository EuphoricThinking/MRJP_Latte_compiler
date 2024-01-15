for file in `ls ../lattests/good/*lat`
do
	echo "$file"
	./latc_x86_64 "$file" | grep -e "OK|ERROR"
	echo "between"
	./latc_x86_64 "$file" > ${file}_new.out
	diff ${file}.out ${file}_new.out
	echo $?
done

echo -e "\nBAD"

for file in `ls ../lattests/bad/*lat`
do
        echo "$file"
        cat "$file" | ./latc_x86_64 | grep "OK|ERROR"
	echo $?
done
