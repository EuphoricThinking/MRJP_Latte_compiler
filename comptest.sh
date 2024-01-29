make
./latc_x86_64 ../lattests/good/core001.lat
#./ltc ../playground/extStage.lat
./latc_x86_64 ../lattests/extensions/arrays1/array002.lat
chmod u+x ../lattests/extensions/arrays1/array002
../lattests/extensions/arrays1/array002
# cat ../lattests/good/core001.s
