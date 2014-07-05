" Vim plugin to assist with making standard .cpp/.h file pairs using
"     basic templates that are roughly compliant with BDE standards
"
" @author   Ben Hipple
" @date     6/27/2014
"
" @usage -  From any Vim window, type :call MkClass("MyPackage", "my_file_name") to make one cpp/h pair.
"
"           To many several, type :call BatchMkClass("MyPackage", "my_file_name1", "my_file_name2", "my_file_name3", ...) etc.
"
"
" @note Helper functions are prefaced with "XH_" to avoid namespace pollution, since I'm not sure how to declare a
"   function private in Vimscript :)


" Call to write a single cpp/h pair
function! MkClass(namespace, filename)
    "---------------------- CONFIGURABLE VARIABLES -----------------------"
    let openingComment = "//@BRIEF"

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    execute "tabe" a:filename . ".cpp"
    execute XH_MakeCPP(a:filename, a:namespace, openingComment)
    execute "silent w"

    execute "vsp " . a:filename . ".h"
    execute XH_MakeHeader(a:filename, a:namespace, openingComment)
    execute "silent w"

endfunction

" Calls MkClass for every filename given in the variable length arglist.
function! BatchMkClass(namespace, ...)
    for fileName in a:000
        silent call MkClass(a:namespace, fileName)
    endfor
endfunction


" Helper functions
function! XH_MakeHeader(filename, namespace, openingComment)
    let classname = XH_CalcClassName(a:filename)

    let str = XH_CalcPrologue(a:filename, ".h")
    let str = str . "#ifndef " . XH_CalcIncludeGuard(a:filename) . "\n#define " . XH_CalcIncludeGuard(a:filename) . "\n\n"

    let str = str . XH_OpenNamespace(a:namespace)
    let str = str . XH_AddClassBody(classname)
    let str = str . XH_CloseNamespace(a:namespace)

    let str = str . "#endif // " . XH_CalcIncludeGuard(a:filename) . "\n\n"
    let str = str . XH_CopyrightString()

    put!=str
    execute XH_DeleteLastLine()
endfunction

function! XH_MakeCPP(filename, namespace, openingComment)

    let str = XH_CalcPrologue(a:filename, ".cpp")
    let str = str . "#include <" . a:filename . ".h>\n\n"

    let str = str . XH_OpenNamespace(a:namespace)
    let str = str . XH_CloseNamespace(a:namespace)
    let str = str . XH_CopyrightString()

    put!=str
    execute XH_DeleteLastLine()
endfunction

function! XH_OpenNamespace(namespace)
    let str = "namespace BloombergLP {\n"
    let str = str . "namespace " . a:namespace . " {\n\n"
    return str
endfunction

function! XH_CloseNamespace(namespace)
    let str = "\n"
    let str = str . "} // close " . a:namespace . "\n"
    let str = str . "} // close BloombergLP" . "\n\n"
    return str
endfunction


" Default classname replaces filename's first character with a capital letter,
" removes underscores, and capitalizes the subsequent letter
function! XH_CalcClassName(filename)
    let classname = substitute(a:filename, '^[a-z]', '\U\0', "g")
    let classname = substitute(classname, '_\([a-z]\)', '\U\1', "g")
    return classname
endfunction

function! XH_CalcPrologue(filename, filetype)
    let str = "// " . a:filename . a:filetype

    " BDE Specified Language tag, right justified to the 79th column
    let languageTag = '-*-C++-*-'
    let spaceCt = 79 - (3 + strlen(a:filename) + strlen(a:filetype) + strlen(languageTag))

    let ct = 0
    while(ct < spaceCt)
        let str = str . " "
        let ct += 1
    endwhile

    let str = str . languageTag . "\n"
    return str
endfunction

function! XH_CalcIncludeGuard(filename)
    return "INCLUDED_" . toupper(a:filename)
endfunction

function! XH_AddClassBody(classname)
    let indentSize = '    '
    let str = "class " . a:classname . " {\n"
    let str = str . "  public:\n"

    " Constructor
    let str = str . indentSize . '//' . a:classname . '() { }' . "\n"
    " Destructor
    let str = str . indentSize . '//~' . a:classname . '() { }' . "\n\n"

    " Copy Constructor
    let str = str . indentSize . '//' . a:classname . '(const ' . a:classname . ' &other);' . "\n"
    " Copy Assignment Operator
    let str = str . indentSize . '//' . a:classname . ' &operator=(const ' . a:classname . ' &rhs);' . "\n\n"

    let str = str . "  private:\n"
    let str = str . "\n};" . "\n"

    return str
endfunction

" Bloomberg LP Copyright Message
function! XH_CopyrightString()
    let str = "// ----------------------------------------------------------------------------" . "\n"
    let str = str . "// NOTICE:" . "\n"
    let str = str . "//      Copyright (C) Bloomberg L.P., 2014" . "\n"
    let str = str . "//      All Rights Reserved." . "\n"
    let str = str . "//      Property of Bloomberg L.P. (BLP)" . "\n"
    let str = str . "//      This software is made available solely pursuant to the" . "\n"
    let str = str . "//      terms of a BLP license agreement which governs its use." . "\n"
    let str = str . "// ------------------------------- END-OF-FILE --------------------------------"
    return str
endfunction

" Get rid of the blank line at EOF
function! XH_DeleteLastLine()
    execute "normal G"
    execute "normal dd"
endfunction
