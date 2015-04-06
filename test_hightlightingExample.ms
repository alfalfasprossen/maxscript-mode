fn function0 = ()

fn function1 arg1 = ()

fn function2 arg1 arg2 = ()

fn function3 kwarg1:defval1 = ()

fn function4 kwarg1: kwarg2:defval2 = ()

fn function5 kwarg1: kwarg2 : defval2 kwarg3 : defval3 = ()

/***
docstring face
***/

/*
non docstring multiline comment
*/

/**
not docstring
*/

-- comment face

--comment face

--"commented string"

/*
"multiline commented string"
*/

/***
"docstring including string"
***/

// not a comment

"string face"

@"raw string face"

"string with ' single quotes should not break string-highlighting"

"string with -- comment should not break string-highlighting"

"string with escaped quotes \" should not \" break string-highlighting"

"string with escaped raw string @\" should not \" break string-highlighting"

"string with /*** docstring ***/ should not break string-hightlighting"

"string with `backquotes` should not break string-hightlighting, though they may hightlight the backquoted part differently"

\"escaped quote outside string should break string-hightlighting!".. "
"str 1"\
"str2"

var1 = value1
var1 = functioncall()
var1 = functioncall without braces
:var = preceded by colon should not be var highlighted

functioncall( arg1 arg2 )
functioncall without braces
(functioncall arg1 arg2)
functioncall kwarg:val kwarg: val kwarg : val
invalidarg:value

#nameface
#nameface = #othername
arrayShouldNotBeNameFace = #()
arrayShouldNotBeNameFace2 = #(1, 2, 3, 4)

numeralvar = 1
floatvar = 1.2

$object
$object.name
$bla*.name

for obj in objects where obj.parent != undefined do(
	return ok
)

UNDEFINED OK Undefined Ok
struct STRUCT objects OBJECTS
