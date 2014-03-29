problem=$1
input=$2
output=$3

HERE="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

dir="$HERE/$problem"
solution="$dir/solution.hs"
exe_file="$dir/solution.exe"

ghc --make -Wall -O2 -o $exe_file $solution > /dev/null 2>&1

if [ $? != "0" ]
then 
    echo "Compilation error"
    exit 1
fi  

time $exe_file < $input > $output

rm "$dir/solution.hi"
rm "$dir/solution.o"
rm "$dir/solution.exe"
