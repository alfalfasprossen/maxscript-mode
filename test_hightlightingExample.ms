fn function1 arg1 = ()

fn function2 arg1 arg2 = ()

fn function3 kwarg1:defval1 = ()

fn function4 kwarg1: kwarg2:defval2 = ()

/***
docstring face
***/

/*
non docstring multiline comment
*/

-- comment face

"string face"

@"raw string face"

"string with ' single quotes should not break string-highlighting"

"string with -- comment should not break string-highlighting" TODO: check if this is truly the case in max

"string with escaped quotes \" should not \" break string-highlighting"

"string with escaped raw string @\" should not \" break string-highlighting"

"string with /*** docstring ***/ should not break string-hightlighting"

"string with `backquotes` should not break string-hightlighting, though they may hightlight the backquoted part differently"

\"escaped quote outside string should break string-hightlighting!" TODO: check how maxscript handles this "
"str 1"\
"str2"

var1 = value1
var1 = functioncall()
var1 = functioncall without braces

functioncall( arg1 arg2 )
functioncall without braces

#nameface
#nameface = #othername
arrayShouldNotBeNameFace = #()
arrayShouldNotBeNameFace2 = #(1, 2, 3, 4)

numeralvar = 1
floatvar = 1.2

$object
$object.name
$bla*.name
