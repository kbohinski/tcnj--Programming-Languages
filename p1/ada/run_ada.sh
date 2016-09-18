gcc -c five_weekends.adb
gnatbind five_weekends
gnatlink five_weekends
gnatmake five_weekends.adb
./five_weekends.exe
rm five_weekends.ali
rm five_weekends.o
rm five_weekends.exe
