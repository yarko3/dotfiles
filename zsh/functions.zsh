function noproxy() {
    env -u http_proxy -u https_proxy -u HTTP_PROXY -u HTTPS_PROXY "$@"
}

function vimify() {
    (vim - -esbnN -c "$*" -c 'w!/dev/fd/3|q!' >/dev/null) 3>&1
}

function resize() {
    file="$1"
    dimensions="800x800"
    if [ $# -eq 2 ]; then
        dimensions="$2"
    fi

    convert "$file" -resize "$dimensions" "$1"
}

