function noproxy() {
    env -u http_proxy -u https_proxy -u HTTP_PROXY -u HTTPS_PROXY "$@"
}

function vimify() {
    (vim - -esbnN -c "$*" -c 'w!/dev/fd/3|q!' >/dev/null) 3>&1
}

# Who needs copying and pasting from Stack Overflow when we can do it on the cmdline!
function stacksort() {
    echo $@ | ( (vim - -esbnN -c '.s/\(.*\)/\=system('"'"'a='"'"'."https:\/\/api.stackexchange.com\/2.2\/".'"'"'; q=`curl -s -G --data-urlencode "q='"'"'.submatch(1).'"'"'" --compressed "'"'"'."${a}search\/advanced?order=desc&sort=relevance&site=stackoverflow".'"'"'" | python -c "'"'"'."exec(\\\"import sys \\nimport json\\nprint(json.loads('"'"''"'"'.join(sys.stdin.readlines()))['"'"'items'"'"'][0]['"'"'question_id'"'"'])\\\")".'"'"'"`; curl -s --compressed "'"'"'."${a}questions\/$q\/answers?order=desc&sort=votes&site=stackoverflow&filter=withbody".'"'"'" | python -c "'"'"'."exec(\\\"import sys\\nimport json\\nprint(json.loads('"'"''"'"'.join(sys.stdin.readlines()))['"'"'items'"'"'][0]['"'"'body'"'"']).encode('"'"'utf8'"'"')\\\")".'"'"'"'"'"')/' -c '%s/\(\_.\+\)<code>\(\_.\+\)<\/code>\(\_.\+\)/\2/g' -c '%s/&gt;/>/g' -c 'w!/dev/stderr|q!' >/dev/null) 2>&1)
}

function stacksort-raw() {
    echo $@ | ( (vim - -esbnN -c '.s/\(.*\)/\=system('"'"'a='"'"'."https:\/\/api.stackexchange.com\/2.2\/".'"'"'; q=`curl -s -G --data-urlencode "q='"'"'.submatch(1).'"'"'" --compressed "'"'"'."${a}search\/advanced?order=desc&sort=relevance&site=stackoverflow".'"'"'" | python -c "'"'"'."exec(\\\"import sys \\nimport json\\nprint(json.loads('"'"''"'"'.join(sys.stdin.readlines()))['"'"'items'"'"'][0]['"'"'question_id'"'"'])\\\")".'"'"'"`; curl -s --compressed "'"'"'."${a}questions\/$q\/answers?order=desc&sort=votes&site=stackoverflow&filter=withbody".'"'"'" | python -c "'"'"'."exec(\\\"import sys\\nimport json\\nprint(json.loads('"'"''"'"'.join(sys.stdin.readlines()))['"'"'items'"'"'][0]['"'"'body'"'"']).encode('"'"'utf8'"'"')\\\")".'"'"'"'"'"')/' -c 'w!/dev/stderr|q!' >/dev/null) 2>&1)
}

function resize() {
    file="$1"
    dimensions="800x800"
    if [ $# -eq 2 ]; then
        dimensions="$2"
    fi

    convert "$file" -resize "$dimensions" "$1"
}

