unit UparserErrors;

interface

resourcestring
  SErrFunDefined='Function %s is already defined';
  eofincom_ERR = 'Unexpected EOF in comment block';
  delimeter_expected = 'Operator delimeter expected';
  eofinstring_ERR ='Unexpected EOF in string constant declaration';
  need_opbr ='( expected';
  comma_expected= 'Comma expected';
  need_clbr= ') expected';
  begin_expected= 'BEGIN expected';
  unk_macrotype= 'Unknown macro type %S';
  par_notfound= 'Parameter %S not found';
  unkn_id= 'Unknown identifier: %s';
  unexp_writer= 'Unknown variable type during writing program';
  do_exp= 'DO expected';
  down_to_exp= 'TO or DOWNTO expected';
  unit_declared= 'Unit %s already defined';
  bad_unit= 'Unit declaration error';
  fun_notfound= 'Function %s not found';
  until_exp= 'UNTIL Expected';
  linker_error= 'Link Error';
  labname_exp= 'Label name expected';
  label_already= 'Label <%s> already defined';
  delim_or_coma= 'Comma or delimeter expected';
  err_declpart= 'Error in declaration block';
  lab_notdef= 'Label <%s> not defined';
  progname_exp= 'Program name expected';
  varname_exp= 'Variable name expected';
  var_already= 'Variable <%s> already defined';
  bad_varblock= 'Error in variable declaration block';
  var_NotDef= 'Variable <%s> not defined';
  else_exp= 'ELSE expected';
  then_exp= 'THEN expected';
  id_expected= 'Identifier expected';
  meth_decerr= 'Method declaration error';
  bad_methparam= 'Method parameters declaration error';
  no_props= 'Properties not implemented';
  need_par= 'Parent name expected';
  clbr_exp= ') Expected';
  only_class= 'Only class declarations allowed';
  err_decl= '%s declaration  error';
  p2_exp= 'Colon expected';
  synt_err='Syntax error in  (%s): %s.';
  bad_idName= 'Bad identifier name <%s>';
  bad_id= 'Bad identifier <%s>';
  opsq_exp= '[ expected but %s found';
  clsq_exp= '] expected but %s found';
  in_funuse= 'Invalid function usage';
  in_procuse= 'Invalid procedure usage';
  bad_hex= 'Hex constant declaration error';
  file_not_found= 'File %S not found';
  compile_before= 'Compile before run';
  bad_realconst= 'Real constant declaration error';
  bad_charconst= 'String constant declaration error';
  unsup_partype= 'Unsupported parameter type';
  no_resvar= 'Variable Result not found for %s';
  proc_notfound= 'Procedure %s not found';
  eq_exp= '= expected';
  end_expected= 'END expected';
  SErrCircularVarRef='Circular variable %S reference';
  SErrUnknReaderType='Unknown reader type';

implementation

end.
 
