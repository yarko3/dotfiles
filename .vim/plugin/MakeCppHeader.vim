
" Function for the user to call
function! MkClass(filename, package)
   "---------------------- CONFIGURABLE VARIABLES ------------------------
   let firstLineComment = "C++ Lab"
   let author = "Ben Hipple"
    
   """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
   " Make the .cpp
   execute "tabe" a:filename . ".cpp"
   execute XH_MakeCPP(a:filename, a:package, firstLineComment, author)
   execute "w"

   " Make the .h
   execute "vsp " . a:filename . ".h"
   execute XH_MakeHeader(a:filename, a:package, firstLineComment, author)
   execute "w"

endfunction


" Helper functions
function! XH_MakeHeader(filename, package, firstLineComment, author)
   let classname = XH_CalcClassName(a:filename)
   let str = XH_FileComment(a:firstLineComment, a:author)

   let str = str . "#ifndef " . XH_CalcIncludeGuard(a:filename) . "\n#define " . XH_CalcIncludeGuard(a:filename) . "\n\n"

   let str = str . XH_OpenNamespace(a:package)
   let str = str . XH_AddClassBody(classname)
   let str = str . XH_CloseNamespace(a:package)

   let str = str . "\n\n#endif // " . XH_CalcIncludeGuard(a:filename)
   put!=str
endfunction

function! XH_MakeCPP(filename, package, firstLineComment, author)
   let str = XH_FileComment(a:firstLineComment, a:author)

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
   let str = str . "} // end namespace " . a:package . "\n"
   let str = str . "} // end namespace BloombergLP"
   return str
endfunction

function! XH_FileComment(firstLineComment, author)
   let str = "/\* " . a:firstLineComment . "\n \* @author " . a:author . "\n \*/\n\n"
   return str
endfunction

" Default classname replaces filename's first character with a capital letter
function! XH_CalcClassName(filename) 
   return substitute(a:filename, "^[a-z]", "\\U\\0", "g")
endfunction

" Default Include Guard name replaces filename [A-Z] with _[A-Z], then capitalizes the entire string
function! XH_CalcIncludeGuard(filename)
   let str = substitute(a:filename, "[A-Z]", "_\\0", "g")
   return toupper(str)
endfunction

function! XH_AddClassBody(classname)
   let str = "class " . a:classname . " {\n"
   let str = str . "   public:\n"

   " Orthodox Canonical Form
   " Constructor
   let str = str . '      //' . a:classname . '() { }' . "\n"

   " Copy Constructor
   let str = str . '      //' . a:classname . '(const ' . a:classname . ' &other) { } ' . "\n"

   " Copy Assignment Operator
   let str = str . '      //' . a:classname . ' &operator=(const ' . a:classname . ' &rhs)' . "\n\n"

   " Destructor
   let str = str . '      //~' . a:classname . '() { } ' . "\n\n"

   let str = str . "   private:\n"

   let str = str . "\n\n}; // end " . a:classname . "\n" 

   return str
endfunction
