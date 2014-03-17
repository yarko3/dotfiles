" Vim plugin to assist with making standard .cpp/.h file pairs from basic templates
" @author Ben Hipple
" @date 3/16/2014
"
" @usage - From any Vim window, type :call MkClass("MyPackage", "myFileName") to make one cpp/h pair.
"          To many several, type :call BatchMkClass("MyPackage", "myFileName1", "myFileName2", "myFileName3", ...) etc.
"          
" @note Helper functions are prefaced with "XH_" to avoid namespace pollution, since I'm not sure how to declare a 
"   function private in Vimscript :)


" Calls MkClass for every filename given in the variable length arglist.
function! BatchMkClass(package, ...)

   for fileName in a:000 
      silent call MkClass(a:package, fileName)
   endfor

endfunction

" Call to write a single cpp/h pair
function! MkClass(package, filename)
   "---------------------- CONFIGURABLE VARIABLES ------------------------
   let firstLineComment = "Index Manager"
   let author = "Ben Hipple"
    
   """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
   " Make the .cpp
   execute "tabe" a:filename . ".cpp"
   execute XH_MakeCPP(a:filename, a:package, firstLineComment, author)
   execute "silent w"

   " Make the .h
   execute "vsp " . a:filename . ".h"
   execute XH_MakeHeader(a:filename, a:package, firstLineComment, author)
   execute "silent w"

endfunction


" Helper functions
function! XH_MakeHeader(filename, package, firstLineComment, author)
   let classname = XH_CalcClassName(a:filename)
   let str = XH_FileComment(a:firstLineComment, a:author)

   let str = str . "#ifndef " . XH_CalcIncludeGuard(a:filename) . "\n#define " . XH_CalcIncludeGuard(a:filename) . "\n\n"
   let str = str . "// " . a:filename . ".h\n\n"

   let str = str . XH_OpenNamespace(a:package)
   let str = str . XH_AddClassBody(classname)
   let str = str . XH_CloseNamespace(a:package)

   let str = str . "\n\n#endif // " . XH_CalcIncludeGuard(a:filename)
   put!=str
endfunction

function! XH_MakeCPP(filename, package, firstLineComment, author)
   let str = XH_FileComment(a:firstLineComment, a:author)

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

function! XH_FileComment(firstLineComment, author)
   let str = "/\* " . a:firstLineComment . "\n \* @author " . a:author . "\n * 2014-02-10-SF-NY Training\n *\n * @brief \n \*/\n\n"
   return str
endfunction

" Default classname replaces filename's first character with a capital letter
function! XH_CalcClassName(filename) 
   return substitute(a:filename, "^[a-z]", "\\U\\0", "g")
endfunction

" Default Include Guard name replaces filename [A-Z] with _[A-Z], then capitalizes the entire string
" and prefixes INCLUDED_
function! XH_CalcIncludeGuard(filename)
   let str = substitute(a:filename, "[A-Z]", "_\\0", "g")
   let str = toupper(str)
   return "INCLUDED_" . str
endfunction

function! XH_AddClassBody(classname)
   let str = "class " . a:classname . " {\n"
   let str = str . "   public:\n"

   " Constructor
   let str = str . '      //' . a:classname . '() { }' . "\n"
   " Destructor
   let str = str . '      //~' . a:classname . '() { } ' . "\n\n"

   let str = str . "   private:\n"
   
   " Copy Constructor
   let str = str . '      ' . a:classname . '(const ' . a:classname . ' &other); ' . "\n"
   " Copy Assignment Operator
   let str = str . '      ' . a:classname . ' &operator=(const ' . a:classname . ' &rhs);' . "\n"

   let str = str . "\n\n}; // close " . a:classname . "\n" 

   return str
endfunction
