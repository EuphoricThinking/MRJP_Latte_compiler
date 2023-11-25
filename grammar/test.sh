ghc typechecker.hs

for file in `ls ../lattests/good/*lat`
do
	echo "$file"
	cat "$file" | ./typechecker | grep "OK"
done

echo -e "\nBAD"

for file in `ls ../lattests/bad/*lat`
do
        echo "$file"
        cat "$file" | ./typechecker | grep "OK"
done
