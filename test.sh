ghc typechecker.hs

for file in `ls ./lattests/good/*lat`
do
	echo "$file"
	cat "$file" | ./src/typechecker | grep -e "OK|ERROR"
	echo $?
done

echo -e "\nBAD"

for file in `ls ./lattests/bad/*lat`
do
        echo "$file"
        cat "$file" | ./src/typechecker | grep "OK|ERROR"
	echo $?
done
