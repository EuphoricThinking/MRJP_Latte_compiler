for file in `ls ../lattests/good/*lat`
do
	echo "$file"
	./latc_x86_64 "$file" | grep -e "OK|ERROR"
	echo "between"
	./latc_x86_64 "$file"  #> "${file%.*}.new"
	"${file%.*}" > "${file%.*}.new"
	diff "${file%.*}.output" "${file%.*}.new"
	echo $?
done

echo -e "\nBAD"

for file in `ls ../lattests/bad/*lat`
do
        echo "$file"
        ./latc_x86_64 "$file" | grep "OK|ERROR"
	echo $?
done
