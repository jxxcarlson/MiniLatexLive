color=`tput setaf 48`
reset=`tput setaf 7`

echo
echo "${color}Compiling ...${reset}"
elm make --optimize src/Main.elm --output=Main.js


echo "${color}Minifiying ...${reset}"
uglifyjs Main.js -mc 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9"' -o Main.min.js

echo "${color}Copying to 138.197.81.6${reset}"
scp -r index-remote.html  root@138.197.81.6:/var/www/demo.minilatex.app/html/index.html
scp -r Main.min.js  root@138.197.81.6:/var/www/demo.minilatex.app/html/
scp -r assets/style.css  root@138.197.81.6:/var/www/demo.minilatex.app/html/assets/
scp -r assets/custom-element-config.css  root@138.197.81.6:/var/www/demo.minilatex.app/html/assets/
scp -r assets/math-text.css  root@138.197.81.6:/var/www/demo.minilatex.app/html/assets/

