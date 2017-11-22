function! Cdfile()
  if expand('%') != ''
    cd %:h
  else
    echom "Not currently in a file."
  endif
endfunction

" local_project_dirs must contain full paths or regexes of paths;
" append local project root directories
let g:local_project_roots=[]

" cd into a local root that matches something
" in g:local_project_roots
"
" returns 1 if a local root was found and cd'd into;
" 0 otherwise
function! Cdlocalroot()
  let cur_dir = expand('%:p')
  for local_root_pat in g:local_project_roots
    if cur_dir =~ local_root_pat
      let local_root_dir = matchstr(cur_dir, local_root_pat)
      exec "cd " . local_root_dir
      return 1
    endif
  endfor
  return 0
endfunction

" cd to the root of the current file's project directory
function! Cdroot()
  if Cdlocalroot()
    return
  endif

  call Cdfile()
  exec "cd " . Trim(system("git rev-parse --show-toplevel"))
  echom expand('.')
endfunction
