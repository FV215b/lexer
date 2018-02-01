type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

val currentString = ref ""
val stringStartPos = ref 0
fun appendS s = currentString := !currentString ^ s
fun nextLine pos = (lineNum := !lineNum + 1; linePos := pos :: !linePos)

fun eof () = 
let 
  val pos = hd(!linePos) 
in 
  Tokens.EOF(pos,pos) 
end

%% 
%%

<INITIAL>\n	=> (nextLine yypos; continue());
<INITIAL>[" "|\t|\r]	=> (continue());

<INITIAL>type   => (Tokens.TYPE (yypos, yypos + 4));
<INITIAL>var  	=> (Tokens.VAR  (yypos, yypos + 3));
<INITIAL>function	=> (Tokens.FUNCTION (yypos, yypos + 8));
<INITIAL>break  => (Tokens.BREAK (yypos, yypos + 5));
<INITIAL>of     => (Tokens.OF   (yypos, yypos + 2));
<INITIAL>end    => (Tokens.END  (yypos, yypos + 3));
<INITIAL>in     => (Tokens.IN   (yypos, yypos + 2));
<INITIAL>nil    => (Tokens.NIL  (yypos, yypos + 3));
<INITIAL>let    => (Tokens.LET  (yypos, yypos + 3));
<INITIAL>do     => (Tokens.DO   (yypos, yypos + 2));
<INITIAL>to     => (Tokens.TO   (yypos, yypos + 2));
<INITIAL>for    => (Tokens.FOR  (yypos, yypos + 3));
<INITIAL>while  => (Tokens.WHILE (yypos, yypos + 5));
<INITIAL>else   => (Tokens.ELSE (yypos, yypos + 4));
<INITIAL>then   => (Tokens.THEN (yypos, yypos + 4));
<INITIAL>if     => (Tokens.IF   (yypos, yypos + 2));
<INITIAL>array  => (Tokens.ARRAY (yypos, yypos + 5));

<INITIAL>":="   => (Tokens.ASSIGN (yypos, yypos + 2));
<INITIAL>"|"    => (Tokens.OR (yypos, yypos + 1));
<INITIAL>"&"    => (Tokens.AND (yypos, yypos + 1));
<INITIAL>">="   => (Tokens.GE (yypos, yypos + 2));
<INITIAL>">"    => (Tokens.GT (yypos, yypos + 1));
<INITIAL>"<="   => (Tokens.LE (yypos, yypos + 2));
<INITIAL>"<"    => (Tokens.LT (yypos, yypos + 1));
<INITIAL>"<>"   => (Tokens.NEQ (yypos, yypos + 2));
<INITIAL>"="    => (Tokens.EQ (yypos, yypos + 1));
<INITIAL>"/"    => (Tokens.DIVIDE (yypos, yypos + 1));
<INITIAL>"*"    => (Tokens.TIMES (yypos, yypos + 1));
<INITIAL>"-"    => (Tokens.MINUS (yypos, yypos + 1));
<INITIAL>"+"    => (Tokens.PLUS (yypos, yypos + 1));
<INITIAL>"."    => (Tokens.DOT (yypos, yypos + 1));
<INITIAL>"("    => (Tokens.LPAREN (yypos, yypos + 1));
<INITIAL>")"    => (Tokens.RPAREN (yypos, yypos + 1));
<INITIAL>"["    => (Tokens.LBRACK (yypos, yypos + 1));
<INITIAL>"]"    => (Tokens.RBRACK (yypos, yypos + 1));
<INITIAL>"{"    => (Tokens.LBRACE (yypos, yypos + 1));
<INITIAL>"}"    => (Tokens.RBRACE (yypos, yypos + 1));
<INITIAL>";"    => (Tokens.SEMICOLON (yypos, yypos + 1));
<INITIAL>":"    => (Tokens.COLON (yypos, yypos + 1));
<INITIAL>","	=> (Tokens.COMMA (yypos, yypos + 1));

<INITIAL>[0-9]+	=> (Tokens.INT (valOf (Int.fromString yytext), yypos,yypos + size yytext));
<INITIAL>[a-zA-Z]([a-zA-Z]|[0-9]|"_")	=> (Tokens.ID (yytext, yypos, yypos + size yytext));

<INITIAL>"\""	=> (YYBEGIN STRING; currentString :=""; stringStartPos := yypos; continue());
<STRING>"\\"	=> (YYBEGIN ESCAPE; continue());
<STRING>"\""	=> (YYBEGIN INITIAL; Tokens.STRING(!currentString, !stringStartPos, yypos + 1));
<STRING>\n	=> (ErrorMsg.error yypos ("illegal newline character " ^ yytext); continue());
<STRING>. 	=> (appendS yytext; continue());
<ESCAPE>\n	=> (nextLine yypos; YYBEGIN DOUBLE_ESCAPE; continue());
<ESCAPE>[" "\t\f]	=> (YYBEGIN DOUBLE_ESCAPE; continue());
<ESCAPE>n	=> (appendS "\n"; YYBEGIN STRING; continue());
<ESCAPE>t	=> (appendS "\t"; YYBEGIN STRING; continue());
<ESCAPE>^	=> (YYBEGIN CONTROL; continue());
<ESCAPE>.	=> (ErrorMsg.error yypos ("illegal escape character " ^ yytext); continue());
<DOUBLE_ESCAPE>"\\"	=> (YYBEGIN STRING; continue());
<DOUBLE_ESCAPE>\n	=> (nextLine yypos; continue());
<DOUBLE_ESCAPE>[" "\t\f]	=> (continue());
<DOUBLE_ESCAPE>.	=> (ErrorMsg.error yypos ("illegal double escape character " ^ yytext); continue());
<CONTROL>.	=> (appendS (String.str (chr (ord (String.sub(yytext, 0)) - 64)));
                        YYBEGIN STRING; continue ());

<INITIAL>"/*"   => (YYBEGIN COMMENT; continue());
<COMMENT>"*/"   => (YYBEGIN INITIAL; continue());
<COMMENT>.      => (continue());


.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

