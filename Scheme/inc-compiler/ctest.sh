gcc -Wall ctest.c startup.c -o test
echo "*OUTPUT*"
./test
gcc -fomit-frame-pointer -S ctest.c
echo "*ASSEMBLY*"
cat -n ctest.s | expand -t 4 | sed 's/ *//'
rm test ctest.s
