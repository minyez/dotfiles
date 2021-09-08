#!/usr/bin/env bash
# reg from https://blog.csdn.net/zbgjhy88/article/details/85110956

fontreg='zh_font.reg'
zhfont="wqy-zenhei.ttc"
(( $# >= 1 )) && fontreg="$1"
(( $# >= 2 )) && zhfont="$2"

cat > "$fontreg" << EOF
REGEDIT4
 
[HKEY_LOCAL_MACHINE\Software\Microsoft\Windows NT\CurrentVersion\FontLink\SystemLink]
"Lucida Sans Unicode"="$zhfont"
"Microsoft Sans Serif"="$zhfont"
"Microsoft YaHei"="$zhfont"
"MS Sans Serif"="$zhfont"
"Tahoma"="$zhfont"
"Tahoma Bold"="$zhfont"
"SimSun"="$zhfont"
"Arial"="$zhfont"
"Arial Black"="$zhfont"
"微软雅黑"="$zhfont"
"宋体"="$zhfont"
"新細明體"="$zhfont"
EOF
