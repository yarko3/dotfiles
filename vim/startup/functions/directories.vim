function! Cdfile()
  if expand('%') != ''
    cd %:h
  else
    echom "Not currently in a file."
  endif
endfunction

" local_project_roots must contain full paths or regexes of paths;
" append local project root directories
let g:local_project_roots=[]

" cd into a local root that matches something
" in g:local_project_roots
"
" returns 1 if a local root was found and cd'd into;
" 0 otherwise
function! CdLocalRoot()
  let l:cur_dir = expand('%:p')
  for l:local_root_pat in g:local_project_roots
    if l:cur_dir =~ l:local_root_pat
      let l:local_root_dir = matchstr(l:cur_dir, l:local_root_pat)
      exec "cd " . l:local_root_dir
      return 1
    endif
  endfor
  return 0
endfunction

function! GetCSVRoot()
  call Cdfile()
  let l:git_root = Trim(system("git rev-parse --show-toplevel"))
  if empty(matchstr(l:git_root, '^fatal:.*'))
    return l:git_root
  endif
  return ""
endfunction

" cd to the root of the current file's project directory
function! CdRoot()
  if CdLocalRoot()
    return
  endif

  let l:csv_root = GetCSVRoot()
  if !empty(l:csv_root)
    exec "cd " . l:csv_root
  endif
endfunction
