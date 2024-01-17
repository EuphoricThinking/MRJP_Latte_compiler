for file in `ls ../lattests/good/*lat`
do
	echo "$file"
	./latc_x86_64 "$file" | grep -e "OK|ERROR"
	./latc_x86_64 "$file"

	if [ -e "${file%.*}.input" ]; then
		cat "${file%.*}.input" | "${file%.*}" > "${file%.*}.new"
	else
		"${file%.*}" > "${file%.*}.new"
	fi

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
