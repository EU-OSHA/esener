cd plots/txt/
find . -name '*.htm' | xargs rm
cd ../xls
find . -name '*.xls' | xargs rm
cd ../svg
find . -name '*.svg.gz' | xargs rm
find . -name '*.svg' | xargs rm
cd ../eps
find . -name '*.eps' | xargs rm
cd ../pdf
find . -name '*.pdf' | xargs rm
cd ../js
find . -name '*.js' | xargs rm
cd ..
find . -name '*.png' | xargs rm
cd ..

