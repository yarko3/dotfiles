"" ============================================================================
""                                Environment
"" ============================================================================
function! GetPlatform()
  if has('win32') || has('win64')
    return "Windows"
  elseif has("unix")
    return substitute(system("uname"), "\n", "", "g")
  else
    return "Unknown"
  endif
endfunction

function! FileExists(filepath)
  if filereadable(expand(a:filepath))
    return 1
  else
    return 0
  endif
endfunction

function! SourceIfExists(filepath)
  if FileExists(a:filepath)
    execute "source " . a:filepath
  endif
endfunction

function! AtWork()
  if FileExists('~/.at_work')
    return 1
  else
    return 0
  endif
endfunction

" Compile spell files.
function! MakeSpell()
  silent mkspell! ~/.vim/spell/extra-words.add
endfunction
