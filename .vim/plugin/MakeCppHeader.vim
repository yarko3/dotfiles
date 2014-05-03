" Vim plugin to assist with making standard .cpp/.h file pairs from basic templates
" @author Ben Hipple
" @date 3/16/2014
"
" @usage - From any Vim window, type :call MkClass("MyPackage", "my_file_name") to make one cpp/h pair.
"          To many several, type :call BatchMkClass("MyPackage", "my_file_name1", "my_file_name2", "my_file_name3", ...) etc.
"
" @note Helper functions are prefaced with "XH_" to avoid namespace pollution, since I'm not sure how to declare a
"   function private in Vimscript :)


" Call to write a single cpp/h pair
function! MkClass(package, filename)
   "---------------------- CONFIGURABLE VARIABLES -----------------------"
   let openingComment = "Client Heat"
   let author = "Training Class"

   """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
   execute "tabe" a:filename . ".cpp"
   execute XH_MakeCPP(a:filename, a:package, openingComment, author)
   execute "silent w"

   execute "vsp " . a:filename . ".h"
   execute XH_MakeHeader(a:filename, a:package, openingComment, author)
   execute "silent w"

endfunction

" Calls MkClass for every filename given in the variable length arglist.
function! BatchMkClass(package, ...)

   for fileName in a:000
      silent call MkClass(a:package, fileName)
   endfor

endfunction


" Helper functions
function! XH_MakeHeader(filename, package, openingComment, author)
   let classname = XH_CalcClassName(a:filename)
   let str = XH_FileComment(a:openingComment, a:author)

   let str = str . "#ifndef " . XH_CalcIncludeGuard(a:filename) . "\n#define " . XH_CalcIncludeGuard(a:filename) . "\n\n"
   let str = str . "// " . a:filename . ".h\n\n"

   let str = str . XH_OpenNamespace(a:package)
   let str = str . XH_AddClassBody(classname)
   let str = str . XH_CloseNamespace(a:package)

   let str = str . "\n\n#endif // " . XH_CalcIncludeGuard(a:filename)
   put!=str
endfunction

function! XH_MakeCPP(filename, package, openingComment, author)
   let str = XH_FileComment(a:openingComment, a:author)

   let str = str . "// " . a:filename . ".cpp\n"
   let str = str . "#include <" . a:filename . ".h>\n\n"

   let str = str . XH_OpenNamespace(a:package)
   let str = str . XH_CloseNamespace(a:package)

   put!=str
endfunction

function! XH_OpenNamespace(package)
   let str = "namespace BloombergLP {\n"
   let str = str . "namespace " . a:package . " {\n"
   let str = str . "\n"
   return str
endfunction

function! XH_CloseNamespace(package)
   let str = "\n"
   let str = str . "} // close " . a:package . "\n"
   let str = str . "} // close BloombergLP"
   return str
endfunction

function! XH_FileComment(openingComment, author)
   let str = "/* " . a:openingComment . "\n * @author " . a:author . "\n *\n * @brief \n */\n\n"
   return str
endfunction

" Default classname replaces filename's first character with a capital letter,
" removes underscores, and capitalizes the subsequent letter
function! XH_CalcClassName(filename)
   let classname = substitute(a:filename, '^[a-z]', '\U\0', "g")
   let classname = substitute(classname, '_\([a-z]\)', '\U\1', "g")
   return classname
endfunction

" Prefixes INCLUDED_ before the filename, and converts the filename to upper case
function! XH_CalcIncludeGuard(filename)
   let str = toupper(a:filename)
   return "INCLUDED_" . str
endfunction

function! XH_AddClassBody(classname)
   let str = "class " . a:classname . " {\n"
   let str = str . "   public:\n"

   " Constructor
   let str = str . '      //' . a:classname . '() { }' . "\n"
   " Destructor
   let str = str . '      //~' . a:classname . '() { } ' . "\n\n"

   " Copy Constructor
   let str = str . '      //' . a:classname . '(const ' . a:classname . ' &other); ' . "\n"
   " Copy Assignment Operator
   let str = str . '      //' . a:classname . ' &operator=(const ' . a:classname . ' &rhs);' . "\n\n"

   let str = str . "   private:\n"
   let str = str . "\n\n}; // close " . a:classname . "\n"

   return str
endfunction
